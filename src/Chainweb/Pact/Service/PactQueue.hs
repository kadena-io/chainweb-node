{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
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
, getNextWriteRequest
, getNextReadRequest
, getPactQueueStats
, newPactQueue
, resetPactQueueStats
, PactQueue
, PactQueueStats(..)
) where

import Control.Applicative
import Control.Concurrent.STM.TBQueue
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Control.Monad.STM

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
    { _pactQueueWriteRequests :: !(TBQueue (T2 RequestMsg (Time Micros)))
    -- NewBlock requests are Read-requests as well but prioritize them with their own queue
    , _pactQueueNewBlockRequests :: !(TBQueue (T2 RequestMsg (Time Micros)))
    , _pactQueueReadRequests :: !(TBQueue (T2 RequestMsg (Time Micros)))
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
    <*> newIORef initPactQueueCounters
    <*> newIORef initPactQueueCounters
    <*> newIORef initPactQueueCounters

-- | Add a request to the Pact execution queue
--
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest q msg =  do
    entranceTime <- getCurrentTimeIntegral
    atomically $ writeTBQueue priority (T2 msg entranceTime)
  where
    priority = case msg of
        -- Write-requests
        ValidateBlockMsg {} -> _pactQueueWriteRequests q
        SyncToBlockMsg {} -> _pactQueueWriteRequests q
        -- Read-requests
        NewBlockMsg {} -> _pactQueueNewBlockRequests q
        _ -> _pactQueueReadRequests q

-- | Get the next available Write-request from the Pact execution queue
--
getNextWriteRequest :: PactQueue -> IO RequestMsg
getNextWriteRequest q = do
    T2 req entranceTime <- atomically $ tryReadTBQueueOrRetry (_pactQueueWriteRequests q) <|> tryReadTBQueueOrRetry (_pactQueueNewBlockRequests q)
    requestTime <- diff <$> getCurrentTimeIntegral <*> pure entranceTime
    updatePactQueueCounters (counters req q) requestTime
    return req
  where
    tryReadTBQueueOrRetry = tryReadTBQueue >=> \case
        Nothing -> retry
        Just msg -> return msg

    counters ValidateBlockMsg{} = _pactQueuePactQueueValidateBlockMsgCounters
    counters NewBlockMsg{} =  _pactQueuePactQueueNewBlockMsgCounters 
    counters _ = _pactQueuePactQueueOtherMsgCounters

-- | Get the next available Read-request from the Pact execution queue
getNextReadRequest :: PactQueue -> IO RequestMsg
getNextReadRequest q = do
    T2 req entranceTime <- atomically $ tryReadTBQueueOrRetry (_pactQueueReadRequests q)
    requestTime <- diff <$> getCurrentTimeIntegral <*> pure entranceTime
    updatePactQueueCounters (counters req q) requestTime
    return req
  where
    tryReadTBQueueOrRetry = tryReadTBQueue >=> \case
        Nothing -> retry
        Just msg -> return msg

    counters ValidateBlockMsg{} = error "getNextReadRequest.counters.impossible"
    counters NewBlockMsg{} = error "getNextReadRequest.counters.impossible"
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
