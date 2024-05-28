{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Pact.Service.PactQueue
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution service queue for Chainweb
--
module Chainweb.Pact.Service.PactQueue
( addRequest
, cancelSubmittedRequest
, waitForSubmittedRequest
, submitRequestAnd
, submitRequestAndWait
, getNextWriteRequest
, getNextReadRequest
, getPactQueueStats
, newPactQueue
, resetPactQueueStats
, PactQueue
, PactQueueStats(..)
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Exception.Safe
import Control.Monad

import Data.Aeson
import Data.IORef

import GHC.Generics

import Numeric.Natural

-- internal modules

import Chainweb.Pact.Service.Types
import Chainweb.Time
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Pact Queue

-- | A Pact Queue with different priorities for block validation, new block, and
-- other requests.
--
data PactQueue = PactQueue
    { _pactQueueWriteRequests :: !(TBQueue (T2 SubmittedRequestMsg (Time Micros)))
    , _pactQueueCloseRequests :: !(TBQueue (T2 SubmittedRequestMsg (Time Micros)))
    , _pactQueueNewBlockRequests :: !(TBQueue (T2 SubmittedRequestMsg (Time Micros)))
    , _pactQueueReadRequests :: !(TBQueue (T2 SubmittedRequestMsg (Time Micros)))
    , _pactQueuePactQueueValidateBlockMsgCounters :: !(IORef PactQueueCounters)
    , _pactQueuePactQueueNewBlockMsgCounters :: !(IORef PactQueueCounters)
    , _pactQueuePactQueueOtherMsgCounters :: !(IORef PactQueueCounters)
    }

initPactQueueCounters :: PactQueueCounters
initPactQueueCounters = PactQueueCounters
    { _pactQueueCountersCount = 0
    , _pactQueueCountersSum = 0
    , _pactQueueCountersMin = maxBound
    , _pactQueueCountersMax = 0
    }

newPactQueue :: Natural -> IO PactQueue
newPactQueue sz = PactQueue
    <$> newTBQueueIO sz
    <*> newTBQueueIO sz
    <*> newTBQueueIO sz
    <*> newTBQueueIO sz
    <*> newIORef initPactQueueCounters
    <*> newIORef initPactQueueCounters
    <*> newIORef initPactQueueCounters

-- | Add a request to the Pact execution queue
--
addRequest :: PactQueue -> RequestMsg r -> IO (TVar (RequestStatus r))
addRequest q msg = do
    statusRef <- newTVarIO RequestNotStarted
    let submittedReq = SubmittedRequestMsg msg statusRef
    entranceTime <- getCurrentTimeIntegral
    atomically $ writeTBQueue priority (T2 submittedReq entranceTime)
    return statusRef
  where
    priority = case msg of
        -- Write-requests
        ValidateBlockMsg {} -> _pactQueueWriteRequests q
        SyncToBlockMsg {} -> _pactQueueWriteRequests q
        CloseMsg -> _pactQueueCloseRequests q
        -- Read-requests
        NewBlockMsg {} -> _pactQueueNewBlockRequests q
        _ -> _pactQueueReadRequests q

-- | Cancel a request that's already been submitted to the Pact queue.
--
cancelSubmittedRequest :: TVar (RequestStatus r) -> IO ()
cancelSubmittedRequest statusRef = atomically $ do
    status <- readTVar statusRef
    case status of
        RequestFailed _ -> return ()
        RequestInProgress -> writeTVar statusRef (RequestFailed (toException AsyncCancelled))
        RequestDone _ -> return ()
        RequestNotStarted -> writeTVar statusRef (RequestFailed (toException AsyncCancelled))

-- | Block waiting for the result of a request that's already been submitted
-- to the Pact queue.
--
waitForSubmittedRequest :: TVar (RequestStatus r) -> IO r
waitForSubmittedRequest statusRef = atomically $ do
    status <- readTVar statusRef
    case status of
        RequestFailed e -> throwIO e
        RequestInProgress -> retry
        RequestDone r -> return r
        RequestNotStarted -> retry

-- | Submit a request and give a handle on its status to the continuation.
-- When the continuation terminates, *cancel the request*.
--
submitRequestAnd :: PactQueue -> RequestMsg r -> (TVar (RequestStatus r) -> IO a) -> IO a
submitRequestAnd q msg k = mask $ \restore -> do
    status <- addRequest q msg
    restore (k status) `finally`
        uninterruptibleMask_ (cancelSubmittedRequest status)

-- | Submit a request and wait for it to finish; if interrupted by an
-- asynchronous exception, *cancel the request*.
--
submitRequestAndWait :: PactQueue -> RequestMsg r -> IO r
submitRequestAndWait q msg = submitRequestAnd q msg waitForSubmittedRequest

-- | Get the next available Write-request from the Pact execution queue
--
getNextWriteRequest :: PactQueue -> IO SubmittedRequestMsg
getNextWriteRequest q = do
    T2 req entranceTime <- atomically $ tryReadTBQueueOrRetry (_pactQueueWriteRequests q)
        <|> tryReadTBQueueOrRetry (_pactQueueCloseRequests q)
    requestTime <- diff <$> getCurrentTimeIntegral <*> pure entranceTime
    updatePactQueueCounters (counters req q) requestTime
    return req
  where
    tryReadTBQueueOrRetry = tryReadTBQueue >=> \case
        Nothing -> retry
        Just msg -> return msg

    counters (SubmittedRequestMsg ValidateBlockMsg{} _) = _pactQueuePactQueueValidateBlockMsgCounters
    counters (SubmittedRequestMsg NewBlockMsg{} _) = error "getNextWriteRequest.counters.impossible"
    counters _ = _pactQueuePactQueueOtherMsgCounters

-- | Get the next available Read-request from the Pact execution queue
getNextReadRequest :: PactQueue -> IO SubmittedRequestMsg
getNextReadRequest q = do
    T2 req entranceTime <- atomically $ tryReadTBQueueOrRetry (_pactQueueNewBlockRequests q)
        <|> tryReadTBQueueOrRetry (_pactQueueReadRequests q)
    requestTime <- diff <$> getCurrentTimeIntegral <*> pure entranceTime
    updatePactQueueCounters (counters req q) requestTime
    return req
  where
    tryReadTBQueueOrRetry = tryReadTBQueue >=> \case
        Nothing -> retry
        Just msg -> return msg

    counters (SubmittedRequestMsg ValidateBlockMsg{} _) = error "getNextReadRequest.counters.impossible"
    counters (SubmittedRequestMsg NewBlockMsg{} _) = _pactQueuePactQueueNewBlockMsgCounters
    counters _ = _pactQueuePactQueueOtherMsgCounters

-- -------------------------------------------------------------------------- --
-- Pact Queue Telemetry

-- | Counters for one Pact queue priority
--
data PactQueueCounters = PactQueueCounters
    { _pactQueueCountersCount :: {-# UNPACK #-} !Int
    , _pactQueueCountersSum :: {-# UNPACK #-} !Micros
    , _pactQueueCountersMin :: {-# UNPACK #-} !Micros
    , _pactQueueCountersMax :: {-# UNPACK #-} !Micros
    }
    deriving (Show, Generic)
    deriving anyclass NFData

instance ToJSON PactQueueCounters where
    toJSON = object . pactQueueCountersProperties
    toEncoding = pairs . mconcat . pactQueueCountersProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

pactQueueCountersProperties :: KeyValue e kv => PactQueueCounters -> [kv]
pactQueueCountersProperties pqc =
    [ "count" .= _pactQueueCountersCount pqc
    , "sum" .= _pactQueueCountersSum pqc
    , "min" .= _pactQueueCountersMin pqc
    , "max" .= _pactQueueCountersMax pqc
    , "avg" .= avg
    ]
  where
    avg :: Maybe Double
    avg = if _pactQueueCountersCount pqc == 0
        then Nothing
        else Just $ fromIntegral (_pactQueueCountersSum pqc) / fromIntegral (_pactQueueCountersCount pqc)
{-# INLINE pactQueueCountersProperties #-}

updatePactQueueCounters :: IORef PactQueueCounters -> TimeSpan Micros -> IO ()
updatePactQueueCounters countersRef (timeSpanToMicros -> timespan) = do
    atomicModifyIORef' countersRef $ \ctrs ->
        ( PactQueueCounters
            { _pactQueueCountersCount = _pactQueueCountersCount ctrs + 1
            , _pactQueueCountersSum = _pactQueueCountersSum ctrs + timespan
            , _pactQueueCountersMin = _pactQueueCountersMin ctrs `min` timespan
            , _pactQueueCountersMax = _pactQueueCountersMax ctrs `max` timespan
            }
        , ()
        )

-- | Statistics for all Pact queue priorities
--
data PactQueueStats = PactQueueStats
    { _validateblock :: !PactQueueCounters
    , _newblock :: !PactQueueCounters
    , _othermsg :: !PactQueueCounters
    }
    deriving (Generic, NFData)

instance ToJSON PactQueueStats where
    toJSON = object . pactQueueStatsProperties
    toEncoding = pairs . mconcat . pactQueueStatsProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

pactQueueStatsProperties :: KeyValue e kv => PactQueueStats -> [kv]
pactQueueStatsProperties o =
    [ "validate" .= _validateblock o
    , "newblock" .= _newblock o
    , "other" .= _othermsg o
    ]
{-# INLINE pactQueueStatsProperties #-}

resetPactQueueStats :: PactQueue -> IO ()
resetPactQueueStats q = do
    reset (_pactQueuePactQueueValidateBlockMsgCounters q)
    reset (_pactQueuePactQueueNewBlockMsgCounters q)
    reset (_pactQueuePactQueueOtherMsgCounters q)
  where
    reset ref = atomicWriteIORef ref initPactQueueCounters

getPactQueueStats :: PactQueue -> IO PactQueueStats
getPactQueueStats q = PactQueueStats
    <$> readIORef (_pactQueuePactQueueValidateBlockMsgCounters q)
    <*> readIORef (_pactQueuePactQueueNewBlockMsgCounters q)
    <*> readIORef (_pactQueuePactQueueOtherMsgCounters q)
