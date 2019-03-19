{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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

#ifndef MIN_VERSION_servant_client
#define MIN_VERSION_servant_client(a,b,c) 1
#endif

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
, memoCache
, PactExectutionService(..)
) where

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Foldable
import Data.Hashable
import Data.Reflection (give)
import qualified Data.Text as T

import GHC.Generics (Generic)

import qualified Network.HTTP.Client as HTTP

import Servant.Client

import System.LogLevel

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Graph (HasChainGraph(..))
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI.Client
import Chainweb.TreeDB
import qualified Chainweb.TreeDB as TDB
import Chainweb.TreeDB.RemoteDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.CAS
import Data.LogMessage
import Data.PQueue
import Data.TaskMap

import P2P.Peer
import P2P.TaskQueue

-- -------------------------------------------------------------------------- --
-- Servant backward compatibility

#if !MIN_VERSION_servant_client(0,16,0)
type ClientError = ServantError
#endif

-- -------------------------------------------------------------------------- --
-- Overlay CAS with asynchronous weak HashMap

memoCache
    :: IsCas a
    => Hashable (CasKeyType (CasValueType a))
    => a
    -> TaskMap (CasKeyType (CasValueType a)) (CasValueType a)
    -> CasKeyType (CasValueType a)
    -> (CasKeyType (CasValueType a) -> IO (CasValueType a))
    -> IO (CasValueType a)
memoCache cas m k a = casLookup cas k >>= \case
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
    Just x -> return x

-- -------------------------------------------------------------------------- --
-- Tag Values With a ChainId

data ChainValue a = ChainValue !ChainId !a
    deriving (Show, Eq, Ord, Generic)
    deriving (Functor, Foldable, Traversable)
    deriving anyclass (NFData, Hashable)

instance TraversableWithIndex ChainId ChainValue where
  itraverse f (ChainValue cid v) = ChainValue cid <$> f cid v
  {-# INLINE itraverse #-}

instance FoldableWithIndex ChainId ChainValue
instance FunctorWithIndex ChainId ChainValue

instance IsCasValue a => IsCasValue (ChainValue a) where
    type CasKeyType (ChainValue a) = ChainValue (CasKeyType a)
    casKey (ChainValue cid a) = ChainValue cid (casKey a)
    {-# INLINE casKey #-}

instance HasChainId (ChainValue a) where
    _chainId (ChainValue cid _) = cid
    {-# INLINE _chainId #-}

chainValue :: HasChainId v => v -> ChainValue v
chainValue v = ChainValue (_chainId v) v
{-# INLINE chainValue #-}

-- -------------------------------------------------------------------------- --
-- Append Only CAS for WebBlockHeaderDb

newtype WebBlockHeaderCas = WebBlockHeaderCas WebBlockHeaderDb

instance HasChainwebVersion WebBlockHeaderCas where
    _chainwebVersion (WebBlockHeaderCas db) = _chainwebVersion db
    {-# INLINE _chainwebVersion #-}

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
    emptyCas = error "not implemented"

    -- This is fine since the type 'WebBlockHeaderCas' is not exported. So the
    -- instance is available only locally.
    --
    -- The instance requires that memoCache doesn't delete from the cas.

-- -------------------------------------------------------------------------- --
-- Obtain and Validate Block Payloads

newtype PactExectutionService = PactExectutionService
    (BlockHeader -> PayloadData -> IO PayloadWithOutputs)

data WebBlockPayloadStore cas = WebBlockPayloadStore
    { _webBlockPayloadStoreCas :: !(PayloadDb cas)
        -- ^ Cas for storing complete payload data including outputs.
    , _webBlockPayloadStoreMemo :: !(TaskMap BlockPayloadHash PayloadWithOutputs)
        -- ^ Internal memo table for active tasks
    , _webBlockPayloadStoreQueue :: !(PQueue (Task ClientEnv PayloadWithOutputs))
        -- ^ task queue for scheduling tasks with the task server
    , _webBlockPayloadStoreLogFunction :: !LogFunction
        -- ^ LogFunction
    , _webBlockPayloadStoreMgr :: !HTTP.Manager
        -- ^ Manager object for making HTTP requests
    , _webBlockPayloadStorePact :: !PactExectutionService
        -- ^ handle to the pact execution service for validating transactions
        -- and computing outputs.
    }

getBlockPayload
    :: PayloadCas cas
    => WebBlockPayloadStore cas
    -> Priority
    -> Maybe PeerInfo
        -- ^ Peer from with the BlockPayloadHash originated, if available.
    -> BlockHeader
        -- ^ The BlockHeader for which the payload is requested
    -> IO PayloadWithOutputs
getBlockPayload s priority maybeOrigin h
    = memoCache cas memoMap payloadHash $ \k -> do
        payload <- pullOrigin k maybeOrigin >>= \case
            Nothing -> do
                t <- queryPayloadTask k
                pQueueInsert queue t
                awaitTask t
            Just x -> return x
        return payload
  where
    v = _chainwebVersion h
    payloadHash = _blockPayloadHash h
    cid = _chainId h
    validateTxs = pact h

    mgr = _webBlockPayloadStoreMgr s
    PactExectutionService pact = _webBlockPayloadStorePact s
    cas = _webBlockPayloadStoreCas s
    memoMap = _webBlockPayloadStoreMemo s
    queue = _webBlockPayloadStoreQueue s

    logfun :: LogLevel ->  T.Text -> IO ()
    logfun = _webBlockPayloadStoreLogFunction s

    -- | Try to pull a block payload from the given origin peer
    --
    pullOrigin :: BlockPayloadHash -> Maybe PeerInfo -> IO (Maybe PayloadWithOutputs)
    pullOrigin k Nothing = do
        logfun Debug $ "task " <> sshow k <> ": no origin"
        return Nothing
    pullOrigin k (Just origin) = do
        let originEnv = peerInfoClientEnv mgr origin
        logfun Debug $ "task " <> sshow k <> ": lookup origin"
        runClientM (payloadClient v cid k) originEnv >>= \case
            Right x -> do
                logfun Debug $ "task " <> sshow k <> ": received from origin"
                Just <$> validateTxs x
            Left (e :: ClientError) -> do
                logfun Debug $ "task " <> sshow k <> " failed to receive from origin: " <> sshow e
                return Nothing

    -- | Query a block payload via the tasj queue
    --
    queryPayloadTask :: BlockPayloadHash -> IO (Task ClientEnv PayloadWithOutputs)
    queryPayloadTask k = newTask (sshow k) priority $ \logg env -> do
        logg @T.Text Debug $ "task " <> sshow k <> ": query remote block payload"
        runClientM (payloadClient v cid k) env >>= \case
            Right x -> do
                logg @T.Text Debug $ "task " <> sshow k <> ": received remote block payload"
                validateTxs x
            Left (e :: ClientError) -> do
                logg @T.Text Debug $ "task " <> sshow k <> " failed: " <> sshow e
                throwM e

-- -------------------------------------------------------------------------- --
-- Obtain, Validate, and Store BlockHeaders

-- | Run an action to obtain and validate a BlockHeader along with all of it's
-- dependencies. Dependencies are computed asynchronously. Asynchronous
-- computations are memoized and shared. The results are stored in the provided
-- CAS storage.
--
getBlockHeaderInternal
    :: PayloadCas payloadCas
    => WebBlockHeaderStore
    -> WebBlockPayloadStore payloadCas
    -> Priority
    -> Maybe PeerInfo
    -> ChainValue BlockHash
    -> IO (ChainValue BlockHeader)
getBlockHeaderInternal headerStore payloadStore priority maybeOrigin h
    = memoCache cas memoMap h $ \k -> do

        -- query BlockHeader via origin or query BlockHeader via task queue of
        -- P2P network
        --
        header <- pullOrigin k maybeOrigin >>= \case
            Nothing -> do
                t <- queryBlockHeaderTask k
                pQueueInsert queue t
                ChainValue _ x <- awaitTask t
                return x
            Just x -> return x

        let queryHeader = Concurrently
                . void
                . getBlockHeaderInternal headerStore payloadStore priority maybeOrigin

        runConcurrently $ mconcat
            -- query parent (recursively)
            $ queryHeader (_blockParent <$> chainValue header)

            -- query payload
            : Concurrently
                (void (getBlockPayload payloadStore priority maybeOrigin header))

            -- query adjacent parents (recursively)
            : (queryHeader <$> adjParents header)


        -- validate block header

        return $ chainValue header

  where
    mgr = _webBlockHeaderStoreMgr headerStore
    cas = WebBlockHeaderCas $ _webBlockHeaderStoreCas headerStore
    memoMap = _webBlockHeaderStoreMemo headerStore
    queue = _webBlockHeaderStoreQueue headerStore
    v = _chainwebVersion cas

    logfun :: LogFunction
    logfun = _webBlockHeaderStoreLogFunction headerStore

    queryBlockHeaderTask ck@(ChainValue cid k)
        = newTask (sshow ck) priority $ \logg env -> chainValue <$> do
            logg @T.Text Debug $ "task " <> sshow ck <> ": query remote block header"
            r <- TDB.lookupM (rDb v cid logfun env) k `catchAllSynchronous` \e -> do
                logg @T.Text Debug $ "task " <> sshow ck <> " failed: " <> sshow e
                throwM e
            logg @T.Text Debug $ "task " <> sshow ck <> ": received remote block header"
            return r

    rDb :: ChainwebVersion -> ChainId -> LogFunction -> ClientEnv -> RemoteDb
    rDb _ cid logg env = RemoteDb env (ALogFunction logg) v cid

    adjParents = toList . imap ChainValue . _getBlockHashRecord . _blockAdjacentHashes

    pullOrigin :: ChainValue BlockHash -> Maybe PeerInfo -> IO (Maybe BlockHeader)
    pullOrigin ck Nothing = do
        logfun @T.Text Debug $ "task " <> sshow ck <> ": no origin"
        return Nothing
    pullOrigin ck@(ChainValue cid k) (Just origin) = do
        let originEnv = peerInfoClientEnv mgr origin
        logfun @T.Text Debug $ "task " <> sshow ck <> ": lookup origin"
        r <- TDB.lookup (rDb v cid logfun originEnv) k
        logfun @T.Text Debug $ "task " <> sshow ck <> ": received from origin"
        return r

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

newWebBlockHeaderStore
    :: HTTP.Manager
    -> WebBlockHeaderDb
    -> LogFunction
    -> IO WebBlockHeaderStore
newWebBlockHeaderStore mgr wdb logfun = do
    m <- new
    queue <- newEmptyPQueue
    return $ WebBlockHeaderStore wdb m queue logfun mgr

newEmptyWebPayloadStore
    :: PayloadCas cas
    => ChainwebVersion
    -> HTTP.Manager
    -> PactExectutionService
    -> LogFunction
    -> IO (WebBlockPayloadStore cas)
newEmptyWebPayloadStore v mgr pact logfun = do
    payloadCas <- emptyPayloadDb
    initializePayloadDb v payloadCas
    newWebPayloadStore mgr pact payloadCas logfun

newWebPayloadStore
    :: HTTP.Manager
    -> PactExectutionService
    -> PayloadDb cas
    -> LogFunction
    -> IO (WebBlockPayloadStore cas)
newWebPayloadStore mgr pact payloadCas logfun = do
    payloadTaskQueue <- newEmptyPQueue
    payloadMemo <- new
    return $ WebBlockPayloadStore
        payloadCas payloadMemo payloadTaskQueue logfun mgr pact

instance HasChainwebVersion WebBlockHeaderStore where
    _chainwebVersion = _chainwebVersion . _webBlockHeaderStoreCas
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph WebBlockHeaderStore where
    _chainGraph = _chainGraph . _webBlockHeaderStoreCas
    {-# INLINE _chainGraph #-}

getBlockHeader
    :: PayloadCas cas
    => WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> ChainId
    -> Priority
    -> Maybe PeerInfo
    -> BlockHash
    -> IO BlockHeader
getBlockHeader headerStore payloadStore cid priority maybeOrigin h
    = ((\(ChainValue _ b) -> b) <$> go)
        `catch` \(TaskFailed _es) -> throwM $ TreeDbKeyNotFound @BlockHeaderDb h
  where
    go = getBlockHeaderInternal
        headerStore
        payloadStore
        priority
        maybeOrigin
        (ChainValue cid h)
{-# INLINE getBlockHeader #-}

