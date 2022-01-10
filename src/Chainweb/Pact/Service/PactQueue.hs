{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
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
, getNextRequest
, getPactQueueStats
, newPactQueue
, resetPactQueueStats
, PactQueue
, PactQueueStats(..)
) where

import Control.Applicative
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TVar
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Control.Monad.STM

import Data.Aeson
import Data.IORef
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Max(..), Min(..), Sum(..))

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
    { _pactQueueValidateBlock :: !(TBQueue (T2 RequestMsg (Time Micros)))
    , _pactQueueValidateBlockMaxSize :: !(TVar Natural)
    , _pactQueueNewBlock :: !(TBQueue (T2 RequestMsg (Time Micros)))
    , _pactQueueNewBlockMaxSize :: !(TVar Natural)
    , _pactQueueOtherMsg :: !(TBQueue (T2 RequestMsg (Time Micros)))
    , _pactQueueOtherMaxSize :: !(TVar Natural)
    , _pactQueueCounters :: !(IORef (Map RequestMsgType PactQueueCounters))
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
    <*> newTVarIO 0
    <*> newTBQueueIO sz
    <*> newTVarIO 0
    <*> newTBQueueIO sz
    <*> newTVarIO 0
    <*> newIORef Map.empty 

-- | Add a request to the Pact execution queue
--
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest q msg =  do
    entranceTime <- getCurrentTimeIntegral
    atomically $ writeTBQueue priority (T2 msg entranceTime)
  where
    priority = case msg of
        ValidateBlockMsg {} -> _pactQueueValidateBlock q
        NewBlockMsg {} -> _pactQueueNewBlock q
        _ -> _pactQueueOtherMsg q

-- | Get the next available request from the Pact execution queue
--
getNextRequest :: PactQueue -> IO RequestMsg
getNextRequest q = do
    validateSize <- atomically $ lengthTBQueue (_pactQueueValidateBlock q)
    newBlockSize <- atomically $ lengthTBQueue (_pactQueueNewBlock q)
    otherSize <- atomically $ lengthTBQueue (_pactQueueOtherMsg q)
    T2 req entranceTime <- atomically
            $ tryReadTBQueueOrRetry (_pactQueueValidateBlock q) 
        <|> tryReadTBQueueOrRetry (_pactQueueNewBlock q)
        <|> tryReadTBQueueOrRetry (_pactQueueOtherMsg q)
    atomically $ modifyTVar (_pactQueueValidateBlockMaxSize q) (max validateSize)
    atomically $ modifyTVar (_pactQueueNewBlockMaxSize q) (max newBlockSize)
    atomically $ modifyTVar (_pactQueueOtherMaxSize q) (max otherSize)
    requestTime <- diff <$> getCurrentTimeIntegral <*> pure entranceTime
    atomicModifyIORef' (_pactQueueCounters q) ((,()) . Map.alter (Just . maybe initPactQueueCounters (updatePactQueueCounters (timeSpanToMicros requestTime))) (requestMsgToType req))
    return req
  where
    tryReadTBQueueOrRetry = tryReadTBQueue >=> \case
        Nothing -> retry
        Just msg -> return msg

-- -------------------------------------------------------------------------- --
-- Pact Queue Telemetry

-- | Counters for one Pact queue message type
--
data PactQueueCounters = PactQueueCounters
    { _pactQueueCountersCount :: {-# UNPACK #-} !Int
    , _pactQueueCountersSum :: {-# UNPACK #-} !(Sum Micros)
    , _pactQueueCountersMin :: {-# UNPACK #-} !(Min Micros)
    , _pactQueueCountersMax :: {-# UNPACK #-} !(Max Micros)
    }
    deriving (Show, Generic)
    deriving anyclass NFData

instance ToJSON PactQueueCounters where
    toJSON = object . pactQueueCountersProperties
    toEncoding = pairs . mconcat . pactQueueCountersProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

pactQueueCountersProperties :: KeyValue kv => PactQueueCounters -> [kv]
pactQueueCountersProperties pqc =
    [ "count" .= _pactQueueCountersCount pqc
    , "sum" .= getSum (_pactQueueCountersSum pqc)
    , "min" .= _pactQueueCountersMin pqc
    , "max" .= _pactQueueCountersMax pqc
    , "avg" .= avg
    ]
  where
    avg :: Maybe Double
    avg = 
        if _pactQueueCountersCount pqc == 0
        then Nothing
        else Just $ fromIntegral (getSum $ _pactQueueCountersSum pqc) / fromIntegral (_pactQueueCountersCount pqc)
{-# INLINE pactQueueCountersProperties #-}

updatePactQueueCounters :: Micros -> PactQueueCounters -> PactQueueCounters
updatePactQueueCounters timespan counters = PactQueueCounters
    { _pactQueueCountersCount = _pactQueueCountersCount counters + 1
    , _pactQueueCountersSum = _pactQueueCountersSum counters <> Sum timespan
    , _pactQueueCountersMin = _pactQueueCountersMin counters <> Min timespan
    , _pactQueueCountersMax = _pactQueueCountersMax counters <> Max timespan
    }

-- | Statistics for all Pact queue priorities
--
data PactQueueStats = PactQueueStats
    { _counters :: !(Map RequestMsgType PactQueueCounters)
    , _validateblockMaxSize :: !Natural
    , _newblockMaxSize :: !Natural
    , _othermsgMaxSize :: !Natural
    }
    deriving (Generic, NFData)

instance ToJSON PactQueueStats where
    toJSON = object . pactQueueStatsProperties
    toEncoding = pairs . mconcat . pactQueueStatsProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

pactQueueStatsProperties :: KeyValue kv => PactQueueStats -> [kv]
pactQueueStatsProperties o =
    [ "counters" .= _counters o
    , "validateblockMaxSize" .= _validateblockMaxSize o
    , "newblockMaxSize" .= _newblockMaxSize o
    , "otherMaxSize" .= _othermsgMaxSize o
    ]
{-# INLINE pactQueueStatsProperties #-}

resetPactQueueStats :: PactQueue -> IO ()
resetPactQueueStats q = do
    atomicWriteIORef (_pactQueueCounters q) Map.empty
    atomically $ do 
        writeTVar (_pactQueueValidateBlockMaxSize q) 0
        writeTVar (_pactQueueNewBlockMaxSize q) 0
        writeTVar (_pactQueueOtherMaxSize q) 0

getPactQueueStats :: PactQueue -> IO PactQueueStats
getPactQueueStats q = PactQueueStats
    <$> readIORef (_pactQueueCounters q)
    <*> readTVarIO (_pactQueueValidateBlockMaxSize q)
    <*> readTVarIO (_pactQueueNewBlockMaxSize q)
    <*> readTVarIO (_pactQueueOtherMaxSize q)

