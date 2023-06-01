{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
, memoBatchInsert
, PactExecutionService(..)
) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Foldable
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import qualified Data.Text as T
import Data.Typeable

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Servant.Client

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.ChainValue
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI.Client
import Chainweb.Time
import Chainweb.TreeDB
import qualified Chainweb.TreeDB as TDB
import Chainweb.TreeDB.RemoteDB
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
    , _webBlockPayloadStoreQueue :: !(PQueue (Task ClientEnv [PayloadData]))
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

-- | This function inserts batches of items in to a CAS while sharing the task
-- for computing the items on a per item basis.
--
-- It doesn't wait for the result, but instead it only schedules insertion and
-- returns immedidately.
--
memoBatchInsert
    -- :: forall a v k
    -- . IsCas a
    -- => v ~ CasValueType a
    :: forall t v k
    . Table t (CasKeyType v) v
    => IsCasValue v
    => k ~ CasKeyType v
    => Show k
    => Typeable k
    => Hashable k
    => t
    -> TaskMap (CasKeyType v) v
    -> [k]
    -> ([k] -> IO [Maybe v])
    -> IO ()
memoBatchInsert cas m ks a = tableLookupBatch cas ks >>= \rs -> do
    let missing :: [k] = fst <$> filter (isNothing . snd) (zip ks rs)
    memoBatch m missing $ \(ks' :: [k]) -> do
        !vs <- catMaybes <$> a ks'
        casInsertBatch cas vs
        return $! HM.fromList $ (\v -> (casKey v, v)) <$> vs

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
    logfun Debug $ taskMsg ""
    tableLookup candidateStore payloadHash >>= \case
        Just !x -> do
            logfun Info $ taskMsg "got payload from candidate store"
            return x
        Nothing -> tableLookup cas payloadHash >>= \case
            (Just !x) -> return $! payloadWithOutputsToPayloadData x
            Nothing -> memo memoMap payloadHash $ \k ->
                pullOrigin k maybeOrigin >>= \case
                    Nothing -> do
                        t <- queryPayloadTask k
                        pQueueInsert queue t
                        head <$> awaitTask t -- FIXME
                    (Just !x) -> return x
  where
    v = _chainwebVersion h
    payloadHash = _blockPayloadHash h
    cid = _chainId h

    mgr = _webBlockPayloadStoreMgr s
    cas = _webBlockPayloadStoreCas s
    memoMap = _webBlockPayloadStoreMemo s
    queue = _webBlockPayloadStoreQueue s

    logfun :: LogLevel -> T.Text -> IO ()
    logfun = _webBlockPayloadStoreLogFunction s

    taskMsg msg = "payload task " <> toText cid <> ":" <> toText payloadHash <> "@" <> toText (_blockHash h) <> ": " <> msg

    -- | Try to pull a block payload from the given origin peer
    --
    pullOrigin :: BlockPayloadHash -> Maybe PeerInfo -> IO (Maybe PayloadData)
    pullOrigin _ Nothing = do
        logfun Debug $ taskMsg "no origin"
        return Nothing
    pullOrigin k (Just origin) = flip catchAllSynchronous (\_ -> return Nothing) $ do
        let originEnv = peerInfoClientEnv mgr origin
        logfun Debug $ taskMsg "lookup origin"
        runClientM (payloadClient v cid k) originEnv >>= \case
            (Right !x) -> do
                logfun Debug $ taskMsg "received from origin"
                return $ Just x
            Left (e :: ClientError) -> do
                logfun Debug $ taskMsg $ "failed to receive from origin: " <> sshow e
                return Nothing

    -- | Query a block payload via the task queue
    --
    queryPayloadTask :: BlockPayloadHash -> IO (Task ClientEnv [PayloadData])
    queryPayloadTask k = newTask (sshow k) priority $ \logg env -> do
        logg @T.Text Debug $ taskMsg "query remote block payload"
        runClientM (payloadClient v cid k) env >>= \case
            (Right x) -> do
                logg @T.Text Debug $ taskMsg "received remote block payload"
                return [x]
            Left (e :: ClientError) -> do
                logg @T.Text Debug $ taskMsg $ "failed: " <> sshow e
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
    -> Priority
    -> Maybe PeerInfo
    -> ChainValue (BlockHeight, BlockHash)
        -- ^ The target hash that is queried
    -> BlockHeader
        -- ^ Current height. This is used to speculatively query additional
        -- headers and payloads ahead of time.
    -> IO (ChainValue BlockHeader)
getBlockHeaderInternal headerStore payloadStore candidateHeaderCas candidatePayloadCas priority maybeOrigin hh curHdr = do
    logg Debug $ "getBlockHeaderInternal: " <> sshow h
    !bh <- memoInsert cas memoMap h $ \k@(ChainValue cid k') -> do

        -- header hh is not in headerStore, therfore query BlockHeader via
        --
        -- - header store,
        -- - candidates header cache,
        -- - local database (we may have validated this header before)
        -- - cut origin, or
        -- - task queue of P2P network
        --
        (maybeOrigin', header) <- tableLookup candidateHeaderCas k' >>= \case
            Just !x -> do
                logg Info $ taskMsg k
                    $ "getBlockHeaderInternal: get header from candidate store: " <> toText h
                return (maybeOrigin, x)
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
        let isGenesisParentHash (ChainValue c (_, p)) = p == genesisParentBlockHash v c
            queryAdjacentParent p = Concurrently $ unless (isGenesisParentHash p) $ void $ do
                logg Debug $ taskMsg k
                    $ "getBlockHeaderInternal.getPrerequisteHeader (adjacent) for " <> sshow h
                    <> ": " <> sshow p
                getBlockHeaderInternal
                    headerStore
                    payloadStore
                    candidateHeaderCas
                    candidatePayloadCas
                    priority
                    maybeOrigin'
                    p
                    curHdr

            -- Perform inductive (involving the parent) validations on the block
            -- header. There's another complete pass of block header validations
            -- after payload validation when the header is finally added to the db.
            --
            queryParent hdr = Concurrently $ void $ do
                let p = ChainValue (_blockChainId hdr) (max (_blockHeight hdr) 1 - 1, _blockParent hdr)
                logg Debug $ taskMsg k
                    $ "getBlockHeaderInternal.getPrerequisteHeader (parent) for " <> sshow h
                    <> ": " <> sshow p
                void $ getBlockHeaderInternal
                    headerStore
                    payloadStore
                    candidateHeaderCas
                    candidatePayloadCas
                    priority
                    maybeOrigin'
                    p
                    curHdr
                chainDb <- getWebBlockHeaderDb (_webBlockHeaderStoreCas headerStore) header
                validateInductiveChainM (tableLookup chainDb) header

        !p <- runConcurrently
            -- query payload
            $ Concurrently
                (getBlockPayload payloadStore candidatePayloadCas priority maybeOrigin' header)

            -- query parent (recursively)
            --
            <* queryParent header

            -- query adjacent parents (recursively)
            <* mconcat (queryAdjacentParent <$> adjParents header)

            -- TODO Above recursive calls are potentially long running
            -- computations. In particular pact validation can take significant
            -- amounts of time. We may try make these calls tail recursive by
            -- providing a continuation. This would allow earlier garbage
            -- collection of some stack resources.
            --
            -- This requires to provide a CPS version of memoInsert.

        logg Debug $ taskMsg k $ "getBlockHeaderInternal got pre-requesites for " <> toText h

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

    h = snd <$> hh
    trgHeight = fst $ _chainValueValue hh

    mgr = _webBlockHeaderStoreMgr headerStore
    cas = WebBlockHeaderCas $ _webBlockHeaderStoreCas headerStore
    memoMap = _webBlockHeaderStoreMemo headerStore
    queue = _webBlockHeaderStoreQueue headerStore
    v = _chainwebVersion cas

    logfun :: LogFunction
    logfun = _webBlockHeaderStoreLogFunction headerStore

    logg :: LogFunctionText
    logg = logfun @T.Text

    taskMsg k msg = "header task " <> toText k <> ": " <> msg

    pact = _pactValidateBlock
        $ _webPactExecutionService
        $ _webBlockPayloadStorePact payloadStore

    validateAndInsertPayload :: BlockHeader -> PayloadData -> IO ()
    validateAndInsertPayload hdr p = do
        outs <- trace
            logfun
            "Chainweb.Sync.WebBlockHeaderStore.getBlockHeaderInternal.pact"
            (_blockHash hdr)
            (length (_payloadDataTransactions p))
            $ pact hdr p
        casInsert (_webBlockPayloadStoreCas payloadStore) outs

    queryBlockHeaderTask ck@(ChainValue cid k)
        = newTask (sshow ck) priority $ \l env -> chainValue <$> do
            l @T.Text Debug $ taskMsg ck "query remote block header"
            !r <- TDB.lookupM (rDb v cid env) k `catchAllSynchronous` \e -> do
                l @T.Text Debug $ taskMsg ck $ "failed: " <> sshow e
                throwM e
            l @T.Text Debug $ taskMsg ck "received remote block header"
            return r

    rDb :: ChainwebVersion -> ChainId -> ClientEnv -> RemoteDb
    rDb _ cid env = RemoteDb env (ALogFunction logfun) v cid

    adjParents hdr
        = map (fmap (ph,))
        . toList
        . imap ChainValue
        . _getBlockHashRecord
        $ _blockAdjacentHashes hdr
      where
        ph = max 1 (_blockHeight hdr) - 1

    pullOrigin
        :: ChainValue BlockHash
        -> Maybe PeerInfo
        -> IO (Maybe BlockHeader)
    pullOrigin ck Nothing = do
        logg Debug $ taskMsg ck "no origin"
        return Nothing
    pullOrigin (ChainValue _ k) _ | k == _blockHash curHdr = return $ Just curHdr
    pullOrigin ck@(ChainValue cid k) (Just origin) = flip catchAllSynchronous (\_ -> return Nothing) $ do
        let originEnv = peerInfoClientEnv mgr origin
        logg Debug $ taskMsg ck "lookup origin"
        let rs = TDB.branchEntries (rDb v cid originEnv)
                Nothing -- cursor
                Nothing -- limit
                (Just . int $ min (_blockHeight curHdr) trgHeight) -- minimum rank
                    -- FIXME guarantee that at least one block is returned
                Nothing  -- maximum rank
                (HS.singleton $ LowerBound $ _blockHash curHdr)
                    -- this lower bound may not exist on the server side, in which case
                    -- it is ignored and the minimum rank bounds the result.
                (HS.singleton $ UpperBound k)
        rs $ S.next >=> \case
            Left _ -> return Nothing
            Right (x, r) -> do
                unless (_blockHash x == k) $ error "pullOrigin: assertion failed"
                logg Info $ taskMsg ck "received from origin, height: " <> sshow (_blockHeight x)

                -- inject remainder of stream into candidates cache
                candidatePayloadHashes <- r
                    & S.copy
                    & S.mapM_ (liftIO . casInsert candidateHeaderCas)
                    & S.map _blockPayloadHash
                    & S.toList_

                logg Info $ taskMsg ck $ "fetching " <> sshow (length candidatePayloadHashes) <> " candidate payloads form origin"

                -- This races against the other queries (and is probably much slower). Does it still make sense?
                unless (Data.Foldable.null candidatePayloadHashes) $ do
                    memoBatchInsert candidatePayloadCas pmemoMap candidatePayloadHashes $ \hs ->
                        queryCandidatePayloads ck (_blockHeight x) cid hs
                return $ Just x

    pqueue = _webBlockPayloadStoreQueue payloadStore
    pmemoMap = _webBlockPayloadStoreMemo payloadStore

    queryCandidatePayloads
        :: ChainValue BlockHash
        -> BlockHeight
        -> ChainId
        -> [BlockPayloadHash]
        -> IO [Maybe PayloadData]
    queryCandidatePayloads ck he cid hs
        | Data.Foldable.null hs = return mempty
        | otherwise = do
            t <- newTask (sshow hs) priority $ \taskLogg env -> do
                taskLogg @T.Text Info $ taskMsg ck $ "query " <> sshow (length hs) <> " remote block payload " <> sshow he
                runClientM (payloadBatchClient v cid hs) env >>= \case
                    (Right !xs) -> do
                        taskLogg @T.Text Info $ taskMsg ck $ "got " <> sshow (length xs) <> " candidate payloads form origin"
                        return xs
                    Left (e :: ClientError) -> do
                        taskLogg @T.Text Info $ taskMsg ck $ "failed to fetch candidate payloads from origin: " <> sshow e
                        throwM e
            pQueueInsert pqueue t
            rs <- HM.fromList . fmap (\x -> (casKey x, x)) <$> awaitTask t
            return $! fmap (`HM.lookup` rs) hs

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
    -> ChainId
    -> Priority
    -> Maybe PeerInfo
    -> (BlockHeight, BlockHash)
    -> BlockHeader
        -- ^ current Header
    -> IO BlockHeader
getBlockHeader headerStore payloadStore candidateHeaderCas candidatePayloadCas cid priority maybeOrigin h curHdr
    = ((\(ChainValue _ b) -> b) <$> go)
        `catch` \(TaskFailed _es) -> throwM $ TreeDbKeyNotFound @BlockHeaderDb (snd h)
  where
    go = getBlockHeaderInternal
        headerStore
        payloadStore
        candidateHeaderCas
        candidatePayloadCas
        priority
        maybeOrigin
        (ChainValue cid h)
        curHdr
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

