{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ViewPatterns              #-}
-- |
-- Module: Chainweb.Pact.Service.PactQueue
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution service queue for Chainweb

module Chainweb.Pact.Service.PactQueue
    ( addRequest
    , getNextRequest
    , getValidateBlockMsgPactQueueCounters
    , getNewBlockMsgPactQueueCounters
    , getOtherMsgPactQueueCounters
    , newPactQueue
    , PactQueue
    ) where

import Data.Aeson (ToJSON)
import Control.Applicative
import Control.Concurrent.STM.TBQueue
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Control.Monad.STM
import Data.IORef
import qualified Data.Text as T
import Data.Tuple.Strict
import GHC.Generics
import Numeric.Natural
import Chainweb.Pact.Service.Types
import Chainweb.Time

-- | The type of the Pact Queue
-- type PactQueue = TBQueue RequestMsg
data PactQueue = PactQueue
  {
    _pactQueueValidateBlock :: !(TBQueue (T2 RequestMsg (Time Micros)))
  , _pactQueueNewBlock :: !(TBQueue (T2 RequestMsg (Time Micros)))
  , _pactQueueOtherMsg :: !(TBQueue (T2 RequestMsg (Time Micros)))
  , _pactQueuePactQueueValidateBlockMsgStats :: !PactQueueStats
  , _pactQueuePactQueueNewBlockMsgStats :: !PactQueueStats
  , _pactQueuePactQueueOtherMsgStats :: !PactQueueStats
  }

newPactQueue :: Natural -> IO PactQueue
newPactQueue sz = do
  (_pactQueueValidateBlock, _pactQueueNewBlock, _pactQueueOtherMsg) <-
    atomically $ do
      v <- newTBQueue sz
      n <- newTBQueue sz
      o <- newTBQueue sz
      return (v,n,o)
  _pactQueuePactQueueValidateBlockMsgStats <- do
      counters <- newIORef $ PactQueueCounters 0 0 0 0
      return PactQueueStats
        {
          _pactQueueStatsQueueName = "ValidateBlockMsg"
        , _pactQueueStatsCounters = counters
        }
  _pactQueuePactQueueNewBlockMsgStats <- do
      counters <- newIORef $ PactQueueCounters 0 0 0 0
      return PactQueueStats
        {
          _pactQueueStatsQueueName = "NewBlockMsg"
        , _pactQueueStatsCounters = counters
        }
  _pactQueuePactQueueOtherMsgStats <- do
      counters <- newIORef $ PactQueueCounters 0 0 0 0
      return PactQueueStats
        {
          _pactQueueStatsQueueName = "OtherMsg"
        , _pactQueueStatsCounters = counters
        }
  return PactQueue {..}

-- | Add a request to the Pact execution queue
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
getNextRequest :: PactQueue -> IO RequestMsg
getNextRequest q = do
  (T2 req entranceTime) <- atomically $
    tryReadTBQueueOrRetry (_pactQueueValidateBlock q)
    <|> tryReadTBQueueOrRetry (_pactQueueNewBlock q)
    <|> tryReadTBQueueOrRetry (_pactQueueOtherMsg q)
  exitTime <- getCurrentTimeIntegral
  let requestTime = exitTime `diff` entranceTime
  case req of
    ValidateBlockMsg {} ->
      updatePactQueueStats (_pactQueuePactQueueValidateBlockMsgStats q) requestTime
    NewBlockMsg {} ->
      updatePactQueueStats (_pactQueuePactQueueNewBlockMsgStats q) requestTime
    _ ->
      updatePactQueueStats (_pactQueuePactQueueOtherMsgStats q) requestTime
  return req
  where
    tryReadTBQueueOrRetry = tryReadTBQueue >=> \case
      Nothing -> retry
      Just msg -> return msg


data PactQueueStats = PactQueueStats
  {
    _pactQueueStatsQueueName :: !T.Text
  , _pactQueueStatsCounters  :: !(IORef PactQueueCounters)
  }

data PactQueueCounters = PactQueueCounters
  {
    _pactQueueCountersCount :: {-# UNPACK #-} !Int
  , _pactQueueCountersSum   :: {-# UNPACK #-} !Micros
  , _pactQueueCountersMin   :: {-# UNPACK #-} !Micros
  , _pactQueueCountersMax   :: {-# UNPACK #-} !Micros
  } deriving (Show, Generic)
    deriving anyclass (ToJSON, NFData)

updatePactQueueStats :: PactQueueStats -> TimeSpan Micros -> IO ()
updatePactQueueStats stats (timeSpanToMicros -> timespan) = do
    atomicModifyIORef' (_pactQueueStatsCounters stats) $ \ctrs ->
      (PactQueueCounters
      {
          _pactQueueCountersCount = _pactQueueCountersCount ctrs + 1
        , _pactQueueCountersSum = _pactQueueCountersSum ctrs + timespan
        , _pactQueueCountersMin = _pactQueueCountersMin ctrs `min` timespan
        , _pactQueueCountersMax = _pactQueueCountersMax ctrs `max` timespan
      }
      , ())

_resetPactQueueStats :: PactQueueStats -> IO ()
_resetPactQueueStats stats = atomicModifyIORef' (_pactQueueStatsCounters stats) (const (PactQueueCounters 0 0 0 0, ()))

getValidateBlockMsgPactQueueCounters :: PactQueue -> IO PactQueueCounters
getValidateBlockMsgPactQueueCounters pq = readIORef (_pactQueueStatsCounters $ _pactQueuePactQueueValidateBlockMsgStats pq)

getNewBlockMsgPactQueueCounters :: PactQueue -> IO PactQueueCounters
getNewBlockMsgPactQueueCounters pq = readIORef (_pactQueueStatsCounters $ _pactQueuePactQueueNewBlockMsgStats pq)

getOtherMsgPactQueueCounters :: PactQueue -> IO PactQueueCounters
getOtherMsgPactQueueCounters pq = readIORef (_pactQueueStatsCounters $ _pactQueuePactQueueOtherMsgStats pq)
