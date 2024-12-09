{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Sync.WebBlockHeaderStore
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A handle that provides tools obtaining and validating block headers within
-- the Chainweb network and making them available in the local database.
--
module Chainweb.Sync.WebBlockHeaderStore
( WebBlockHeaderStore(..)
, newWebBlockHeaderStore
, getBlockHeader

-- *
, WebBlockPayloadStore(..)
, newEmptyWebPayloadStore
, newWebPayloadStore

-- * Utils
, memoInsert
, PactExecutionService(..)
) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Foldable
import Data.Hashable
import qualified Data.Text as T

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Servant.Client

import System.LogLevel

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RemoteDB
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.ChainValue
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI.Client
import Chainweb.Time
import Chainweb.TreeDB
import qualified Chainweb.TreeDB as TDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.LogMessage
import Data.PQueue
import Data.TaskMap

import P2P.Peer
import P2P.TaskQueue

import Utils.Logging.Trace

import Chainweb.Storage.Table

-- -------------------------------------------------------------------------- --
-- Response Timeout Constants

-- | For P2P queries we lower the response timeout to 500ms. This is tight
-- but guarantees that the task isn't blocked for too long and retries
-- another node quickly in case the first node isn't available. If no node
-- in the network is able to serve that quickly the local node is out of
-- luck. However, 500ms should be sufficient for a well-connected node
-- anywhere on the world. Otherwise, the node would have trouble anyways.
--
taskResponseTimeout :: HTTP.ResponseTimeout
taskResponseTimeout = HTTP.responseTimeoutMicro 500_000

pullOriginResponseTimeout :: HTTP.ResponseTimeout
pullOriginResponseTimeout = HTTP.responseTimeoutMicro 1_000_000

-- | Set the response timeout on all requests made with the ClientEnv This
-- overwrites the default response timeout of the connection manager.
--
setResponseTimeout :: HTTP.ResponseTimeout -> ClientEnv -> ClientEnv
setResponseTimeout t env =  env
    { makeClientRequest = \u r -> defaultMakeClientRequest u r <&> \req -> req
        { HTTP.responseTimeout = t
        }
    }

-- -------------------------------------------------------------------------- --
-- Append Only CAS for WebBlockHeaderDb

newtype WebBlockHeaderCas = WebBlockHeaderCas WebBlockHeaderDb

instance HasChainwebVersion WebBlockHeaderCas where
    _chainwebVersion (WebBlockHeaderCas db) = _chainwebVersion db
    {-# INLINE _chainwebVersion #-}

-- -------------------------------------------------------------------------- --
-- Obtain and Validate Block Payloads

data WebBlockPayloadStore tbl = WebBlockPayloadStore
    { _webBlockPayloadStoreCas :: !(PayloadDb tbl)
        -- ^ Cas for storing complete payload data including outputs.
    , _webBlockPayloadStoreMemo :: !(TaskMap BlockPayloadHash PayloadData)
        -- ^ Internal memo table for active tasks
    , _webBlockPayloadStoreQueue :: !(PQueue (Task ClientEnv PayloadData))
        -- ^ task queue for scheduling tasks with the task server
    , _webBlockPayloadStoreLogFunction :: !LogFunction
        -- ^ LogFunction
    , _webBlockPayloadStoreMgr :: !HTTP.Manager
        -- ^ Manager object for making HTTP requests
    , _webBlockPayloadStorePact :: !WebPactExecutionService
        -- ^ handle to the pact execution service for validating transactions
        -- and computing outputs.
    }

-- -------------------------------------------------------------------------- --
-- WebBlockHeaderStore

-- | In order to use this a processor for the queue is needed.
--
-- The module P2P.TaskQueue provides a P2P session that serves the queue.
--
-- TODO
--
-- * Find a better name
-- * Parameterize in cas
-- * This is currently based on TreeDB (for API) and BlockHeaderDB, it
--   would be possible to run this on top of any CAS and API that offers
--   a simple GET.
--
data WebBlockHeaderStore = WebBlockHeaderStore
    { _webBlockHeaderStoreCas :: !WebBlockHeaderDb
    , _webBlockHeaderStoreMemo :: !(TaskMap (ChainValue BlockHash) (ChainValue BlockHeader))
    , _webBlockHeaderStoreQueue :: !(PQueue (Task ClientEnv (ChainValue BlockHeader)))
    , _webBlockHeaderStoreLogFunction :: !LogFunction
    , _webBlockHeaderStoreMgr :: !HTTP.Manager
    }

instance HasChainwebVersion WebBlockHeaderStore where
    _chainwebVersion = _chainwebVersion . _webBlockHeaderStoreCas
    {-# INLINE _chainwebVersion #-}

-- -------------------------------------------------------------------------- --
-- Overlay CAS with asynchronous weak HashMap

memoInsert
    :: (Table t (CasKeyType v) v, IsCasValue v)
    => Hashable (CasKeyType v)
    => t
    -> TaskMap (CasKeyType v) v
    -> CasKeyType v
    -> (CasKeyType v -> IO v)
    -> IO v
memoInsert cas m k a = tableLookup cas k >>= \case
    Nothing -> memo m k $ \k' -> do
        -- there is the chance of a race here. At this time some task may just
        -- have finished updating the CAS with the key we are looking for. We
        -- could solve this by doing a another CAS lookup here. But, depending
        -- on the CAS, that could be expensive, too. For now we except a few
        -- duplicate tasks due to races instead of adding an extra CAS lookup to
        -- every task.
        !v <- a k'
        casInsert cas v
        return v
    (Just !x) -> return x

-- | Query a payload either from the local store, or the origin, or P2P network.
--
-- The payload is only queried and not inserted into the local store. We want to
-- insert it only after it got validate by pact in order to avoid accumlation of
-- garbage.
--
getBlockPayload
    :: CanReadablePayloadCas tbl
    => Cas candidateCas PayloadData
    => WebBlockPayloadStore tbl
    -> candidateCas
    -> Priority
    -> Maybe PeerInfo
        -- ^ Peer from with the BlockPayloadHash originated, if available.
    -> BlockHeader
        -- ^ The BlockHeader for which the payload is requested
    -> IO PayloadData
getBlockPayload s candidateStore priority maybeOrigin h = do
    logfun Debug $ "getBlockPayload: " <> sshow h
    tableLookup candidateStore payloadHash >>= \case
        Just !x -> return x
        Nothing -> lookupPayloadWithHeight cas (Just $ view blockHeight h) payloadHash >>= \case
            Just !x -> return $! payloadWithOutputsToPayloadData x
            Nothing -> memo memoMap payloadHash $ \k ->
                pullOrigin (view blockHeight h) k maybeOrigin >>= \case
                    Nothing -> do
                        t <- queryPayloadTask (view blockHeight h) k
                        pQueueInsert queue t
                        awaitTask t
                    (Just !x) -> return x

  where
    v = _chainwebVersion h
    payloadHash = view blockPayloadHash h
    cid = _chainId h

    mgr = _webBlockPayloadStoreMgr s
    cas = _webBlockPayloadStoreCas s
    memoMap = _webBlockPayloadStoreMemo s
    queue = _webBlockPayloadStoreQueue s

    logfun :: LogLevel -> T.Text -> IO ()
    logfun = _webBlockPayloadStoreLogFunction s

    traceLogfun :: LogMessage a => LogLevel -> a -> IO ()
    traceLogfun = _webBlockPayloadStoreLogFunction s

    taskMsg k msg = "payload task " <> sshow k <> " @ " <> sshow (view blockHash h) <> ": " <> msg

    traceLabel subfun =
        "Chainweb.Sync.WebBlockHeaderStore.getBlockPayload." <> subfun

    -- | Try to pull a block payload from the given origin peer
    --
    pullOrigin :: BlockHeight -> BlockPayloadHash -> Maybe PeerInfo -> IO (Maybe PayloadData)
    pullOrigin _ k Nothing = do
        logfun Debug $ taskMsg k "no origin"
        return Nothing
    pullOrigin bh k (Just origin) = do
        let originEnv = setResponseTimeout pullOriginResponseTimeout $ peerInfoClientEnv mgr origin
        logfun Debug $ taskMsg k "lookup origin"
        !r <- trace traceLogfun (traceLabel "pullOrigin") k 0
            $ runClientM (payloadClient v cid k (Just bh)) originEnv
        case r of
            (Right !x) -> do
                logfun Debug $ taskMsg k "received from origin"
                return $ Just x
            Left (e :: ClientError) -> do
                logfun Debug $ taskMsg k $ "failed to receive from origin: " <> sshow e
                return Nothing

    -- | Query a block payload via the task queue
    --
    queryPayloadTask :: BlockHeight -> BlockPayloadHash -> IO (Task ClientEnv PayloadData)
    queryPayloadTask bh k = newTask (sshow k) priority $ \logg env -> do
        logg @T.Text Debug $ taskMsg k "query remote block payload"
        let taskEnv = setResponseTimeout taskResponseTimeout env
        !r <- trace traceLogfun (traceLabel "queryPayloadTask") k (let Priority i = priority in i)
            $ runClientM (payloadClient v cid k (Just bh)) taskEnv
        case r of
            (Right !x) -> do
                logg @T.Text Debug $ taskMsg k "received remote block payload"
                return x
            Left (e :: ClientError) -> do
                logg @T.Text Debug $ taskMsg k $ "failed: " <> sshow e
                throwM e

-- -------------------------------------------------------------------------- --
-- Obtain, Validate, and Store BlockHeaders

newtype GetBlockHeaderFailure = GetBlockHeaderFailure T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception GetBlockHeaderFailure

-- | Run an action to obtain and validate a BlockHeader along with all of it's
-- dependencies. Dependencies are computed asynchronously. Asynchronous
-- computations are memoized and shared. The results are stored in the provided
-- CAS storage.
--
-- NOTE: This fetches all prerequesites of a block recursively. It works best
-- for relatively shallow queries. For synchronizing longer/deeper forks an
-- iterative algorithm is preferable.
--
getBlockHeaderInternal
    :: CanPayloadCas tbl
    => BlockHeaderCas candidateHeaderCas
    => PayloadDataCas candidatePayloadCas
    => WebBlockHeaderStore
    -> WebBlockPayloadStore tbl
    -> candidateHeaderCas
    -> candidatePayloadCas
    -> Maybe (BlockPayloadHash, PayloadWithOutputs)
    -> Priority
    -> Maybe PeerInfo
    -> ChainValue BlockHash
    -> IO (ChainValue BlockHeader)
getBlockHeaderInternal headerStore payloadStore candidateHeaderCas candidatePayloadCas localPayload priority maybeOrigin h = do
    logg Debug $ "getBlockHeaderInternal: " <> sshow h
    !bh <- memoInsert cas memoMap h $ \k@(ChainValue cid k') -> do

        -- query BlockHeader via
        --
        -- - header store,
        -- - candidates header cache,
        -- - local database (we may have validated this header before)
        -- - cut origin, or
        -- - task queue of P2P network
        --
        (maybeOrigin', header) <- tableLookup candidateHeaderCas k' >>= \case
            Just !x -> return (maybeOrigin, x)
            Nothing -> pullOrigin k maybeOrigin >>= \case
                Nothing -> do
                    t <- queryBlockHeaderTask k
                    pQueueInsert queue t
                    (ChainValue _ !x) <- awaitTask t
                    return (Nothing, x)
                Just !x -> return (maybeOrigin, x)

        -- Check that the chain id is correct. The candidate cas is indexed just
        -- by the block hash. So, if this fails it is most likely a bug in code
        -- that uses or populates the candidateHeaderCas.
        --
        unless (_chainId header == cid) $ throwM $ GetBlockHeaderFailure
            $ "chain id of block header doesn't match expected chain id. "
            <> "Most likely, this is a bug in Chainweb.Sync.WebBlockHeaderStore."

        -- Perform intrinsic validations on the block header. There's another
        -- complete pass of block header validations after payload validation
        -- when the header is finally added to the db.
        --
        now <- getCurrentTimeIntegral
        validateIntrinsicM now header

        -- Query Prerequesits recursively. If there is already a job for this
        -- prerequesite in the memo-table it is awaited, otherwise a new job is
        -- created.
        --
        let isGenesisParentHash p = _chainValueValue p == genesisParentBlockHash v p
            queryAdjacentParent p = Concurrently $ unless (isGenesisParentHash p) $ void $ do
                logg Debug $ taskMsg k
                    $ "getBlockHeaderInternal.getPrerequisteHeader (adjacent) for " <> sshow h
                    <> ": " <> sshow p
                getBlockHeaderInternal
                    headerStore
                    payloadStore
                    candidateHeaderCas
                    candidatePayloadCas
                    localPayload
                    priority
                    maybeOrigin'
                    p

            -- Perform inductive (involving the parent) validations on the block
            -- header. There's another complete pass of block header validations
            -- after payload validation when the header is finally added to the db.
            --
            queryParent p = Concurrently $ void $ do
                logg Debug $ taskMsg k
                    $ "getBlockHeaderInternal.getPrerequisteHeader (parent) for " <> sshow h
                    <> ": " <> sshow p
                void $ getBlockHeaderInternal
                    headerStore
                    payloadStore
                    candidateHeaderCas
                    candidatePayloadCas
                    localPayload
                    priority
                    maybeOrigin'
                    p
                chainDb <- getWebBlockHeaderDb (_webBlockHeaderStoreCas headerStore) header
                validateInductiveChainM (tableLookup chainDb) header

        !p <- runConcurrently
            -- query payload
            $ Concurrently
                (getBlockPayload payloadStore candidatePayloadCas priority maybeOrigin' header)

            -- query parent (recursively)
            --
            <* queryParent (view blockParent <$> chainValue header)

            -- query adjacent parents (recursively)
            <* mconcat (queryAdjacentParent <$> adjParents header)

            -- TODO Above recursive calls are potentially long running
            -- computations. In particular pact validation can take significant
            -- amounts of time. We may try make these calls tail recursive by
            -- providing a continuation. This would allow earlier garbage
            -- collection of some stack resources.
            --
            -- This requires to provide a CPS version of memoInsert.

        logg Debug $ taskMsg k $ "getBlockHeaderInternal got pre-requesites for " <> sshow h

        -- ------------------------------------------------------------------ --
        -- Validation

        -- 1. Validate Parents and Adjacent Parents
        --
        -- Existence and validity of parents and adjacent parents is guaranteed
        -- in the dependency resolution code above.

        -- 2. Validate BlockHeader
        --
        -- Single chain properties are currently validated when the block header
        -- is inserted into the block header db.

        -- 3. Validate Braiding
        --
        -- Currently, we allow blocks here that are not part of a valid
        -- braiding. However, those block won't make it into cuts, because the
        -- cut processor uses 'joinIntoHeavier' to combine an external cut with
        -- the local cut, which guarantees that only blocks with valid braiding
        -- are referenced by local cuts.
        --
        -- TODO: check braiding and reject blocks without valid braiding here.

        -- 4. Validate block payload
        --
        -- Pact validation is done in the context of a particular header. Just
        -- because the payload does already exist in the store doesn't mean that
        -- validation succeeds in the context of a particular block header.
        --
        -- If we reach this point in the code we are certain that the header
        -- isn't yet in the block header database and thus we still must
        -- validate the payload for this block header.
        --

        logg Debug $ taskMsg k $ "getBlockHeaderInternal validate payload for " <> sshow h <> ": " <> sshow p
        validateAndInsertPayload header p `catch` \(e :: SomeException) -> do
            logg Warn $ taskMsg k $ "getBlockHeaderInternal pact validation for " <> sshow h <> " failed with :" <> sshow e
            throwM e
        logg Debug $ taskMsg k "getBlockHeaderInternal pact validation succeeded"

        logg Debug $ taskMsg k $ "getBlockHeaderInternal return header " <> sshow h
        return $! chainValue header
    logg Debug $ "getBlockHeaderInternal: got block header for " <> sshow h
    return bh

  where

    mgr = _webBlockHeaderStoreMgr headerStore
    cas = WebBlockHeaderCas $ _webBlockHeaderStoreCas headerStore
    memoMap = _webBlockHeaderStoreMemo headerStore
    queue = _webBlockHeaderStoreQueue headerStore
    v = _chainwebVersion cas

    logfun :: LogFunction
    logfun = _webBlockHeaderStoreLogFunction headerStore

    logg :: LogFunctionText
    logg = logfun @T.Text

    taskMsg k msg = "header task " <> sshow k <> ": " <> msg

    traceLabel subfun =
        "Chainweb.Sync.WebBlockHeaderStore.getBlockHeaderInternal." <> subfun

    pact = _pactValidateBlock
        $ _webPactExecutionService
        $ _webBlockPayloadStorePact payloadStore

    validateAndInsertPayload :: BlockHeader -> PayloadData -> IO ()
    validateAndInsertPayload hdr p = do
        let payload = case localPayload of
                Just (hsh, pwo)
                    | hsh == view blockPayloadHash hdr
                        -> CheckablePayloadWithOutputs pwo
                _ -> CheckablePayload p
        outs <- trace logfun
            (traceLabel "pact")
            (view blockHash hdr)
            (length (view payloadDataTransactions p))
            $ pact hdr payload
        addNewPayload (_webBlockPayloadStoreCas payloadStore) (view blockHeight hdr) outs

    queryBlockHeaderTask ck@(ChainValue cid k)
        = newTask (sshow ck) priority $ \l env -> chainValue <$> do
            l @T.Text Debug $ taskMsg ck "query remote block header"
            let taskEnv = setResponseTimeout taskResponseTimeout env
            !r <- trace l (traceLabel "queryBlockHeaderTask") k (let Priority i = priority in i)
                $ TDB.lookupM (rDb cid taskEnv) k `catchAllSynchronous` \e -> do
                    l @T.Text Debug $ taskMsg ck $ "failed: " <> sshow e
                    throwM e
            l @T.Text Debug $ taskMsg ck "received remote block header"
            return r

    rDb :: ChainId -> ClientEnv -> RemoteDb
    rDb cid env = RemoteDb
        { _remoteEnv = env
        , _remoteLogFunction = ALogFunction logfun
        , _remoteVersion = v
        , _remoteChainId = cid
        }

    adjParents = toList . imap ChainValue . _getBlockHashRecord . view blockAdjacentHashes

    pullOrigin
        :: ChainValue BlockHash
        -> Maybe PeerInfo
        -> IO (Maybe BlockHeader)
    pullOrigin ck Nothing = do
        logg Debug $ taskMsg ck "no origin"
        return Nothing
    pullOrigin ck@(ChainValue cid k) (Just origin) = do
        let originEnv = setResponseTimeout pullOriginResponseTimeout $ peerInfoClientEnv mgr origin
        logg Debug $ taskMsg ck "lookup origin"
        !r <- trace logfun (traceLabel "pullOrigin") k 0
            $ TDB.lookup (rDb cid originEnv) k
        logg Debug $ taskMsg ck "received from origin"
        return r

    -- pullOriginDeps _ Nothing = return ()
    -- pullOriginDeps ck@(ChainValue cid k) (Just origin) = do
    --     let originEnv = peerInfoClientEnv mgr origin
    --     curRank <- liftIO $ do
    --         cdb <- give (_webBlockHeaderStoreCas headerStore) (getWebBlockHeaderDb cid)
    --         maxRank cdb
    --     (l, _) <- TDB.branchEntries (rDb cid originEnv)
    --         Nothing (Just 1000)
    --         (Just $ int curRank) Nothing
    --         mempty (HS.singleton (UpperBound k))
    --     liftIO $ logg Info $ taskMsg ck $ "pre-fetched " <> sshow l <> " block headers"
    --     return ()

newWebBlockHeaderStore
    :: HTTP.Manager
    -> WebBlockHeaderDb
    -> LogFunction
    -> IO WebBlockHeaderStore
newWebBlockHeaderStore mgr wdb logfun = do
    m <- new
    queue <- newEmptyPQueue
    return $! WebBlockHeaderStore wdb m queue logfun mgr

newEmptyWebPayloadStore
    :: CanPayloadCas tbl
    => ChainwebVersion
    -> HTTP.Manager
    -> WebPactExecutionService
    -> LogFunction
    -> PayloadDb tbl
    -> IO (WebBlockPayloadStore tbl)
newEmptyWebPayloadStore v mgr pact logfun payloadDb = do
    initializePayloadDb v payloadDb
    newWebPayloadStore mgr pact payloadDb logfun

newWebPayloadStore
    :: HTTP.Manager
    -> WebPactExecutionService
    -> PayloadDb tbl
    -> LogFunction
    -> IO (WebBlockPayloadStore tbl)
newWebPayloadStore mgr pact payloadDb logfun = do
    payloadTaskQueue <- newEmptyPQueue
    payloadMemo <- new
    return $! WebBlockPayloadStore
        payloadDb payloadMemo payloadTaskQueue logfun mgr pact

getBlockHeader
    :: CanPayloadCas tbl
    => BlockHeaderCas candidateHeaderCas
    => PayloadDataCas candidatePayloadCas
    => WebBlockHeaderStore
    -> WebBlockPayloadStore tbl
    -> candidateHeaderCas
    -> candidatePayloadCas
    -> Maybe (BlockPayloadHash, PayloadWithOutputs)
    -> ChainId
    -> Priority
    -> Maybe PeerInfo
    -> BlockHash
    -> IO BlockHeader
getBlockHeader headerStore payloadStore candidateHeaderCas candidatePayloadCas localPayload cid priority maybeOrigin h
    = ((\(ChainValue _ b) -> b) <$> go)
        `catch` \(TaskFailed _es) -> throwM $ TreeDbKeyNotFound @BlockHeaderDb h
  where
    go = getBlockHeaderInternal
        headerStore
        payloadStore
        candidateHeaderCas
        candidatePayloadCas
        localPayload
        priority
        maybeOrigin
        (ChainValue cid h)
{-# INLINE getBlockHeader #-}

instance (CasKeyType (ChainValue BlockHeader) ~ k) => ReadableTable WebBlockHeaderCas k (ChainValue BlockHeader) where
    tableLookup (WebBlockHeaderCas db) (ChainValue cid h) =
        (Just . ChainValue cid <$> lookupWebBlockHeaderDb db cid h)
            `catch` \e -> case e of
                TDB.TreeDbKeyNotFound _ -> return Nothing
                _ -> throwM @_ @(TDB.TreeDbException BlockHeaderDb) e
    {-# INLINE tableLookup #-}

instance (CasKeyType (ChainValue BlockHeader) ~ k) => Table WebBlockHeaderCas k (ChainValue BlockHeader) where
    tableInsert (WebBlockHeaderCas db) _ (ChainValue _ h)
        = insertWebBlockHeaderDb db h
    {-# INLINE tableInsert #-}

    tableDelete = error "not implemented"

    -- This is fine since the type 'WebBlockHeaderCas' is not exported. So the
    -- instance is available only locally.
    --
    -- The instance requires that memoCache doesn't delete from the cas.
