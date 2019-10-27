{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Sync.WebBlockHeaderStore
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
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
import Data.Reflection (give)
import qualified Data.Text as T

import qualified Network.HTTP.Client as HTTP

import Servant.Client

import System.LogLevel

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI.Client
import Chainweb.Sync.WebBlockHeaderStore.Types
import Chainweb.TreeDB
import qualified Chainweb.TreeDB as TDB
import Chainweb.TreeDB.RemoteDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.CAS
import Data.LogMessage
import Data.PQueue
import Data.TaskMap

import P2P.Peer
import P2P.TaskQueue

import Utils.Logging.Trace

-- -------------------------------------------------------------------------- --
-- Overlay CAS with asynchronous weak HashMap

memoInsert
    :: IsCas a
    => Hashable (CasKeyType (CasValueType a))
    => a
    -> TaskMap (CasKeyType (CasValueType a)) (CasValueType a)
    -> CasKeyType (CasValueType a)
    -> (CasKeyType (CasValueType a) -> IO (CasValueType a))
    -> IO (CasValueType a)
memoInsert cas m k a = casLookup cas k >>= \case
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

chainValue :: HasChainId v => v -> ChainValue v
chainValue v = ChainValue (_chainId v) v
{-# INLINE chainValue #-}

-- | Query a payload either from the local store, or the origin, or P2P network.
--
-- The payload is only queried and not inserted into the local store. We want to
-- insert it only after it got validate by pact in order to avoid accumlation of
-- garbage.
--
getBlockPayload
    :: PayloadCas cas
    => PayloadDataCas candidateCas
    => WebBlockPayloadStore cas
    -> candidateCas PayloadData
    -> Priority
    -> Maybe PeerInfo
        -- ^ Peer from with the BlockPayloadHash originated, if available.
    -> BlockHeader
        -- ^ The BlockHeader for which the payload is requested
    -> IO PayloadData
getBlockPayload s candidateStore priority maybeOrigin h = do
    logfun Debug $ "getBlockPayload: " <> sshow h
    casLookup candidateStore payloadHash >>= \case
        Just !x -> return x
        Nothing -> casLookup cas payloadHash >>= \case
            (Just !x) -> return $! payloadWithOutputsToPayloadData x
            Nothing -> memo memoMap payloadHash $ \k ->
                pullOrigin k maybeOrigin >>= \case
                    Nothing -> do
                        t <- queryPayloadTask k
                        pQueueInsert queue t
                        awaitTask t
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

    taskMsg k msg = "payload task " <> sshow k <> " @ " <> sshow (_blockHash h) <> ": " <> msg

    -- | Try to pull a block payload from the given origin peer
    --
    pullOrigin :: BlockPayloadHash -> Maybe PeerInfo -> IO (Maybe PayloadData)
    pullOrigin k Nothing = do
        logfun Debug $ taskMsg k "no origin"
        return Nothing
    pullOrigin k (Just origin) = do
        let originEnv = peerInfoClientEnv mgr origin
        logfun Debug $ taskMsg k "lookup origin"
        runClientM (payloadClient v cid k) originEnv >>= \case
            (Right !x) -> do
                logfun Debug $ taskMsg k "received from origin"
                return $! Just x
            Left (e :: ClientError) -> do
                logfun Debug $ taskMsg k $ "failed to receive from origin: " <> sshow e
                return Nothing

    -- | Query a block payload via the task queue
    --
    queryPayloadTask :: BlockPayloadHash -> IO (Task ClientEnv PayloadData)
    queryPayloadTask k = newTask (sshow k) priority $ \logg env -> do
        logg @T.Text Debug $ taskMsg k "query remote block payload"
        runClientM (payloadClient v cid k) env >>= \case
            (Right !x) -> do
                logg @T.Text Debug $ taskMsg k "received remote block payload"
                return x
            Left (e :: ClientError) -> do
                logg @T.Text Debug $ taskMsg k $ "failed: " <> sshow e
                throwM e

-- -------------------------------------------------------------------------- --
-- Obtain, Validate, and Store BlockHeaders

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
    :: PayloadCas payloadCas
    => PayloadDataCas candidatePayloadCas
    => BlockHeaderCas candidateHeaderCas
    => WebBlockHeaderStore
    -> WebBlockPayloadStore payloadCas
    -> candidateHeaderCas BlockHeader
    -> candidatePayloadCas PayloadData
    -> Priority
    -> Maybe PeerInfo
    -> ChainValue BlockHash
    -> IO (ChainValue BlockHeader)
getBlockHeaderInternal headerStore payloadStore candidateHeaderCas candidatePayloadCas priority maybeOrigin h = do
    logg Debug $ "getBlockHeaderInternal: " <> sshow h
    memoInsert cas memoMap h $ \k@(ChainValue _ k') -> do

        -- query BlockHeader via
        --
        -- - candidates header cache,
        -- - cut origin, or
        -- - task queue of P2P network
        --
        (maybeOrigin', header) <- casLookup candidateHeaderCas k' >>= \case
            Just !x -> return $! (maybeOrigin, x)
            Nothing -> pullOrigin k maybeOrigin >>= \case
                Nothing -> do
                    t <- queryBlockHeaderTask k
                    pQueueInsert queue t
                    (ChainValue _ !x) <- awaitTask t
                    return $! (Nothing, x)
                (Just !x) -> return $! (maybeOrigin, x)

        -- Query Prerequesits recursively. If there is already job for this
        -- prerequesite in the memo-table it is awaited, otherwise a new job is
        -- created.
        --
        let queryPrerequesiteHeader p = Concurrently $ void $ do
                logg Debug $ taskMsg k $ "getBlockHeaderInternal.getPrerequisteHeader for " <> sshow h <> ": " <> sshow p
                getBlockHeaderInternal headerStore payloadStore candidateHeaderCas candidatePayloadCas priority maybeOrigin' p

        p <- runConcurrently
            -- query payload
            $ Concurrently (getBlockPayload payloadStore candidatePayloadCas priority maybeOrigin' header)

            -- query parent (recursively)
            --
            <* queryPrerequesiteHeader (_blockParent <$> chainValue header)

            -- query adjacent parents (recursively)
            <* mconcat (queryPrerequesiteHeader <$> adjParents header)

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
        logg Debug $ taskMsg k $ "getBlockHeaderInternal pact validation succeeded"

        logg Debug $ taskMsg k $ "getBlockHeaderInternal return header " <> sshow h
        return $! chainValue header

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
            l @T.Text Debug $ taskMsg ck $ "query remote block header"
            !r <- TDB.lookupM (rDb v cid env) k `catchAllSynchronous` \e -> do
                l @T.Text Debug $ taskMsg ck $ "failed: " <> sshow e
                throwM e
            l @T.Text Debug $ taskMsg ck "received remote block header"
            return r

    rDb :: ChainwebVersion -> ChainId -> ClientEnv -> RemoteDb
    rDb _ cid env = RemoteDb env (ALogFunction logfun) v cid

    adjParents = toList . imap ChainValue . _getBlockHashRecord . _blockAdjacentHashes

    pullOrigin
        :: ChainValue BlockHash
        -> Maybe PeerInfo
        -> IO (Maybe BlockHeader)
    pullOrigin ck Nothing = do
        logg Debug $ taskMsg ck "no origin"
        return Nothing
    pullOrigin ck@(ChainValue cid k) (Just origin) = do
        let originEnv = peerInfoClientEnv mgr origin
        logg Debug $ taskMsg ck $ "lookup origin"
        !r <- TDB.lookup (rDb v cid originEnv) k
        logg Debug $ taskMsg ck "received from origin"
        return r

    -- pullOriginDeps _ Nothing = return ()
    -- pullOriginDeps ck@(ChainValue cid k) (Just origin) = do
    --     let originEnv = peerInfoClientEnv mgr origin
    --     curRank <- liftIO $ do
    --         cdb <- give (_webBlockHeaderStoreCas headerStore) (getWebBlockHeaderDb cid)
    --         maxRank cdb
    --     (l, _) <- TDB.branchEntries (rDb v cid originEnv)
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
    :: PayloadCas cas
    => ChainwebVersion
    -> HTTP.Manager
    -> WebPactExecutionService
    -> LogFunction
    -> PayloadDb cas
    -> IO (WebBlockPayloadStore cas)
newEmptyWebPayloadStore v mgr pact logfun payloadCas = do
    initializePayloadDb v payloadCas
    newWebPayloadStore mgr pact payloadCas logfun

newWebPayloadStore
    :: HTTP.Manager
    -> WebPactExecutionService
    -> PayloadDb cas
    -> LogFunction
    -> IO (WebBlockPayloadStore cas)
newWebPayloadStore mgr pact payloadCas logfun = do
    payloadTaskQueue <- newEmptyPQueue
    payloadMemo <- new
    return $! WebBlockPayloadStore
        payloadCas payloadMemo payloadTaskQueue logfun mgr pact

getBlockHeader
    :: PayloadCas cas
    => BlockHeaderCas candidateHeaderCas
    => PayloadDataCas candidatePayloadCas
    => WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> candidateHeaderCas BlockHeader
    -> candidatePayloadCas PayloadData
    -> ChainId
    -> Priority
    -> Maybe PeerInfo
    -> BlockHash
    -> IO BlockHeader
getBlockHeader headerStore payloadStore candidateHeaderCas candidatePayloadCas cid priority maybeOrigin h
    = ((\(ChainValue _ b) -> b) <$> go)
        `catch` \(TaskFailed _es) -> throwM $ TreeDbKeyNotFound @BlockHeaderDb h
  where
    go = getBlockHeaderInternal
        headerStore
        payloadStore
        candidateHeaderCas
        candidatePayloadCas
        priority
        maybeOrigin
        (ChainValue cid h)
{-# INLINE getBlockHeader #-}


instance IsCas WebBlockHeaderCas where
    type CasValueType WebBlockHeaderCas = ChainValue BlockHeader
    casLookup (WebBlockHeaderCas db) (ChainValue cid h) =
        (Just . ChainValue cid <$> give db (lookupWebBlockHeaderDb cid h))
            `catch` \e -> case e of
                TDB.TreeDbKeyNotFound _ -> return Nothing
                _ -> throwM @_ @(TDB.TreeDbException BlockHeaderDb) e
    casInsert (WebBlockHeaderCas db) (ChainValue _ h)
        = give db (insertWebBlockHeaderDb h)
    casDelete = error "not implemented"

    -- This is fine since the type 'WebBlockHeaderCas' is not exported. So the
    -- instance is available only locally.
    --
    -- The instance requires that memoCache doesn't delete from the cas.
