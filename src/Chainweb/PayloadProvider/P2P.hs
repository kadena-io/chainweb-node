{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module: Chainweb.PayloadProvider.P2P
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- FIXME: rename this module into Chainweb.PayloadProvider.PayloadStore
--
module Chainweb.PayloadProvider.P2P
( PayloadStore(..)
, newPayloadStore
, getPayload
, getPayloadSimple
) where

import Chainweb.BlockPayloadHash
import Chainweb.Storage.Table
import Chainweb.Utils
import Control.Monad.Catch
import Data.Aeson (object, (.=))
import Data.Functor
import Data.LogMessage
import Data.PQueue
import Data.TaskMap
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import P2P.Peer
import P2P.Session
import P2P.TaskQueue
import Servant.Client
import System.LogLevel
import Utils.Logging.Trace
import Chainweb.Storage.Table.HashMap (emptyTable)

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
-- Payload Store

-- | P2P network service handle for payloads.
--
-- It facilities storage and synchronization of payloads accross nodes in the
-- P2P network.
--
-- Payload data is indexed by the ranked block payload hash. There are no
-- constraints on the payload data values.
--
data PayloadStore tbl a = PayloadStore
    { _payloadStoreTable :: !tbl
        -- ^ Content addressed store for storing payload data
    , _payloadStoreMemo :: !(TaskMap RankedBlockPayloadHash a)
        -- ^ Internal memo table for active P2P tasks. Due to the way how cut
        -- resolution works the same payload may be requested several times
        -- while the first request is still processed. This map helps to avoid
        -- running a P2P task redundantly.
    , _payloadStoreQueue :: !(PQueue (Task ClientEnv a))
        -- ^ task queue for scheduling tasks with the P2P task server. This
        -- Queue should be shared across chains (and possibly also across
        -- payload providers, although for that we would have to hide the
        -- payload type parameter, both here as well as in the HTTP client)
    , _payloadStoreLogFunction :: !LogFunction
        -- ^ LogFunction
    , _payloadStoreMgr :: !HTTP.Manager
        -- ^ Manager object for making HTTP requests
    , _payloadStoreGetPayloadClient :: !(RankedBlockPayloadHash -> ClientM a)
        -- ^ HTTP client for querying payloads (provided by the PayloadProvider)
        --
        -- There is a default Payload REST API in
        -- "Chainweb.PayloadProvider.P2P.RestAPI" that can be used in common
        -- cases.
        --
        -- NOTE: This used to have a ChainId parameter which allowed sharing of
        -- the store between payload providers with the same payload type. This
        -- could make it easier to limit concurrency across chains. If this is
        -- needed the ChainId parameter would have to be re-added.
    }

-- FIXME: not sure whether the following instances are a good idea...

-- | Expose the /local/ payload table.
--
-- NOTE, this does /not/ look for payloads in P2P network. Data available in
-- this table is guaranteed to be fully validated, both by the execution client
-- as well as against the evaluation context.
--
instance
    (ReadableTable pdb RankedBlockPayloadHash a)
    => ReadableTable (PayloadStore pdb a) RankedBlockPayloadHash a
  where
    tableLookup = tableLookup . _payloadStoreTable
    tableLookupBatch' s = tableLookupBatch' (_payloadStoreTable s)
    tableMember = tableMember . _payloadStoreTable

-- | Expose the /local/ payload table.
--
-- NOTE, this does /not/ look for payloads in P2P network. Data available in
-- this table is guaranteed to be fully validated, both by the execution client
-- as well as against the evaluation context.
--
instance
    (Table pdb RankedBlockPayloadHash a)
    => Table (PayloadStore pdb a) RankedBlockPayloadHash a
  where
    tableInsert = tableInsert . _payloadStoreTable
    tableInsertBatch s = tableInsertBatch (_payloadStoreTable s)
    tableDelete s = tableDelete (_payloadStoreTable s)
    tableDeleteBatch s = tableDeleteBatch (_payloadStoreTable s)

-- -------------------------------------------------------------------------- --
-- Initialization

-- | FIXME:
--
-- Why not move a generic payload DB here, so that it can be managed by the
-- payload store. (We can offer more complex payload providers to provide there
-- own version if desired.)
--
-- FIXME:
--
-- The queue needs to be hooked up to a p2p node. Where is that done? Given
-- that a single p2p node can serve many payload stores, the queue should
-- probably be passed as a parameter.
--
newPayloadStore
    :: Table tbl RankedBlockPayloadHash a
    => HTTP.Manager
        -- ^ Manager for P2P networking. This manager should be shared by all
        -- P2P networks.
    -> LogFunction
    -> tbl
        -- ^ initialized Payload DB. In particular it is expected that genesis
        -- payloads are already available.
    -> (RankedBlockPayloadHash -> ClientM a)
    -> IO (PayloadStore tbl a)
newPayloadStore mgr logfun payloadDb cli = do
    payloadTaskQueue <- newEmptyPQueue
    payloadMemo <- new
    return $! PayloadStore
        { _payloadStoreTable = payloadDb
        , _payloadStoreMemo = payloadMemo
        , _payloadStoreQueue = payloadTaskQueue
        , _payloadStoreLogFunction = logfun
        , _payloadStoreMgr = mgr
        , _payloadStoreGetPayloadClient = cli
        }

-- -------------------------------------------------------------------------- --
-- Get Payload

-- | Simple version of getPayload
--
-- Mostly useful for testing and debugging
--
getPayloadSimple
    :: forall a tbl
    . Table tbl RankedBlockPayloadHash a
    => CasKeyType a ~ RankedBlockPayloadHash
    => PayloadStore tbl a
    -> RankedBlockPayloadHash
    -> IO a
getPayloadSimple s r = do
    candidates <- emptyTable
    getPayload s candidates prio Nothing r
  where
    prio = Priority $ negate $ int $ _rankedBlockPayloadHashHeight r

-- | Query a payload either from the local store, or the origin, or P2P network.
--
-- The payload is only queried and not inserted into the local store. We want to
-- insert it only after it got validate by the payload provider.
--
getPayload
    :: forall a tbl candidateTbl
    . Table tbl RankedBlockPayloadHash a
    => Table candidateTbl RankedBlockPayloadHash a
    => PayloadStore tbl a
    -> candidateTbl
    -> Priority
        -- ^ larger values get precedence in the P2P task queue. A good
        -- heuristics is to use the negated block height.
    -> Maybe PeerInfo
        -- ^ Peer from with the BlockPayloadHash originated, if available.
    -> RankedBlockPayloadHash
    -> IO a
getPayload s candidateStore priority maybeOrigin payloadHash = do
    logfun Debug $ "getPayload: " <> sshow payloadHash
    tableLookup candidateStore payloadHash >>= \case
        Just !x -> return x
        Nothing -> tableLookup tbl payloadHash >>= \case
            Just !x -> return x
            Nothing -> memo memoMap payloadHash $ \_ ->
                pullOrigin payloadHash maybeOrigin >>= \case
                    Nothing -> do
                        t <- queryPayloadTask payloadHash
                        pQueueInsert queue t
                        awaitTask t
                    Just !x -> return x
  where
    mgr = _payloadStoreMgr s
    tbl = _payloadStoreTable s
    memoMap = _payloadStoreMemo s
    queue = _payloadStoreQueue s

    logfun :: LogLevel -> T.Text -> IO ()
    logfun = _payloadStoreLogFunction s

    traceLogfun :: forall m . LogMessage m => LogLevel -> m -> IO ()
    traceLogfun = _payloadStoreLogFunction s

    taskMsg k msg = "payload task " <> sshow k <> " @ " <> sshow payloadHash <> ": " <> msg

    traceLabel subfun =
        "Chainweb.PayloadProvider.P2P.getPayload." <> subfun

    -- Only used for logging trace telemetry
    rankedHashJson rh = object
        [ "height" .= _rankedBlockPayloadHashHeight rh
        , "hash" .= _rankedBlockPayloadHashHash rh
        ]

    -- | Try to pull a block payload from the given origin peer
    --
    pullOrigin :: RankedBlockPayloadHash -> Maybe PeerInfo -> IO (Maybe a)
    pullOrigin k Nothing = do
        logfun Debug $ taskMsg k "no origin"
        return Nothing
    pullOrigin k (Just origin) = do
        let originEnv = setResponseTimeout pullOriginResponseTimeout $ peerInfoClientEnv mgr origin
        logfun Debug $ taskMsg k "lookup origin"
        !r <- trace traceLogfun (traceLabel "pullOrigin") (rankedHashJson k) 0
            $ runClientM (_payloadStoreGetPayloadClient s k) originEnv
        case r of
            (Right !x) -> do
                logfun Debug $ taskMsg k "received from origin"
                return $ Just x
            Left (e :: ClientError) -> do
                logfun Debug $ taskMsg k $ "failed to receive from origin: " <> sshow e
                return Nothing

    -- | Query a block payload via the task queue
    --
    queryPayloadTask :: RankedBlockPayloadHash -> IO (Task ClientEnv a)
    queryPayloadTask k = newTask (sshow k) priority $ \logg env -> do
        logg @T.Text Debug $ taskMsg k "query remote block payload"
        let taskEnv = setResponseTimeout taskResponseTimeout env
        !r <- trace traceLogfun (traceLabel "queryPayloadTask") (rankedHashJson k) (let Priority i = priority in i)
            $ runClientM (_payloadStoreGetPayloadClient s k) taskEnv
        case r of
            (Right !x) -> do
                logg @T.Text Debug $ taskMsg k "received remote payload"
                return x
            Left (e :: ClientError) -> do
                logg @T.Text Debug $ taskMsg k $ "failed: " <> sshow e
                throwM e

