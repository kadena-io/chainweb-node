{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
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
    , getPactQueueStats
    , newPactQueue
    , resetPactQueueStats
    , PactQueue
    , PactQueueStats(..)
    ) where

import Data.Aeson
import Control.Applicative
import Control.Concurrent.STM.TBQueue
import Control.DeepSeq (NFData)
import Control.Lens (each)
import Control.Monad ((>=>))
import Control.Monad.STM
import Data.IORef
import Data.Text (Text)
import GHC.Generics
import Numeric.Natural
import Chainweb.Pact.Service.Types
import Chainweb.Time
import Chainweb.Utils

-- | The type of the Pact Queue
-- type PactQueue = TBQueue RequestMsg
data PactQueue = PactQueue
    {
      _pactQueueValidateBlock :: !(TBQueue (T2 RequestMsg (Time Micros)))
    , _pactQueueNewBlock :: !(TBQueue (T2 RequestMsg (Time Micros)))
    , _pactQueueOtherMsg :: !(TBQueue (T2 RequestMsg (Time Micros)))
    , _pactQueuePactQueueValidateBlockMsgStats :: !PactQueueStats'
    , _pactQueuePactQueueNewBlockMsgStats :: !PactQueueStats'
    , _pactQueuePactQueueOtherMsgStats :: !PactQueueStats'
    }

initPactQueueCounters :: PactQueueCounters
initPactQueueCounters = PactQueueCounters
    {
      _pactQueueCountersCount = 0
    , _pactQueueCountersSum = 0
    , _pactQueueCountersMin = maxBound
    , _pactQueueCountersMax = 0
    }

newPactQueue :: Natural -> IO PactQueue
newPactQueue sz = do
    (pactQueueValidateBlock, pactQueueNewBlock, pactQueueOtherMsg) <- each newTBQueueIO (sz,sz,sz)
    pactQueuePactQueueValidateBlockMsgStats <- do
        counters <- newIORef initPactQueueCounters
        return PactQueueStats'
          {
            _pactQueueStatsQueueName = "ValidateBlockMsg"
          , _pactQueueStatsCounters = counters
          }
    pactQueuePactQueueNewBlockMsgStats <- do
        counters <- newIORef initPactQueueCounters
        return PactQueueStats'
          {
            _pactQueueStatsQueueName = "NewBlockMsg"
          , _pactQueueStatsCounters = counters
          }
    pactQueuePactQueueOtherMsgStats <- do
        counters <- newIORef initPactQueueCounters
        return PactQueueStats'
          {
            _pactQueueStatsQueueName = "OtherMsg"
          , _pactQueueStatsCounters = counters
          }
    return PactQueue
      {
        _pactQueueValidateBlock = pactQueueValidateBlock
      , _pactQueueNewBlock = pactQueueNewBlock
      , _pactQueueOtherMsg = pactQueueOtherMsg
      , _pactQueuePactQueueValidateBlockMsgStats = pactQueuePactQueueValidateBlockMsgStats
      , _pactQueuePactQueueNewBlockMsgStats = pactQueuePactQueueNewBlockMsgStats
      , _pactQueuePactQueueOtherMsgStats = pactQueuePactQueueOtherMsgStats
      }

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
        stats = case req of
          ValidateBlockMsg {} -> _pactQueuePactQueueValidateBlockMsgStats q
          NewBlockMsg {} -> _pactQueuePactQueueNewBlockMsgStats q
          _ -> _pactQueuePactQueueOtherMsgStats q
    updatePactQueueStats stats requestTime
    return req
      where
        tryReadTBQueueOrRetry = tryReadTBQueue >=> \case
          Nothing -> retry
          Just msg -> return msg


data PactQueueStats' = PactQueueStats'
    {
      _pactQueueStatsQueueName :: !Text
    , _pactQueueStatsCounters  :: !(IORef PactQueueCounters)
    }

data PactQueueStats = PactQueueStats
    {
      _validateblock :: !PactQueueCounters
    , _newblock :: !PactQueueCounters
    , _othermsg :: !PactQueueCounters
    } deriving (Generic, NFData)

instance ToJSON PactQueueStats where
    toJSON = object . pactQueueStatsProperties
    toEncoding = pairs . mconcat . pactQueueStatsProperties

pactQueueStatsProperties :: KeyValue kv => PactQueueStats -> [kv]
pactQueueStatsProperties o =
    [ "validate" .= _validateblock o
    , "newblock" .= _newblock o
    , "othermsg" .= _othermsg o
    ]

data PactQueueCounters = PactQueueCounters
    {
      _pactQueueCountersCount :: {-# UNPACK #-} !Int
    , _pactQueueCountersSum   :: {-# UNPACK #-} !Micros
    , _pactQueueCountersMin   :: {-# UNPACK #-} !Micros
    , _pactQueueCountersMax   :: {-# UNPACK #-} !Micros
    } deriving (Show, Generic)
      deriving anyclass NFData


instance ToJSON PactQueueCounters where
    toJSON = object . pactQueueCountersProperties
    toEncoding = pairs . mconcat . pactQueueCountersProperties

pactQueueCountersProperties :: KeyValue kv => PactQueueCounters -> [kv]
pactQueueCountersProperties pqc =
    [ "count" .= _pactQueueCountersCount pqc
    , "sum"   .= _pactQueueCountersSum pqc
    , "min"   .= _pactQueueCountersMin pqc
    , "max"   .= _pactQueueCountersMax pqc
    , "avg"   .= avg
    ]
      where
          avg :: Maybe Double
          avg = if _pactQueueCountersCount pqc == 0 then Nothing
            else Just $ fromIntegral (_pactQueueCountersSum pqc) / fromIntegral (_pactQueueCountersCount pqc)



updatePactQueueStats :: PactQueueStats' -> TimeSpan Micros -> IO ()
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

resetPactQueueStats :: PactQueue -> IO ()
resetPactQueueStats q = do
    resetPactQueueStats' (_pactQueuePactQueueValidateBlockMsgStats q)
    resetPactQueueStats' (_pactQueuePactQueueNewBlockMsgStats q)
    resetPactQueueStats' (_pactQueuePactQueueOtherMsgStats q)

resetPactQueueStats' :: PactQueueStats' -> IO ()
resetPactQueueStats' stats = atomicWriteIORef (_pactQueueStatsCounters stats) initPactQueueCounters

getPactQueueStats :: PactQueue -> IO PactQueueStats
getPactQueueStats = getPactQueueStats' >=> \(vstats,nbstats,ostats) -> return
    PactQueueStats
      {
        _validateblock = vstats
      , _newblock = nbstats
      , _othermsg = ostats
      }

getPactQueueStats' :: PactQueue -> IO (PactQueueCounters, PactQueueCounters, PactQueueCounters)
getPactQueueStats' q = (,,)
    <$> getValidateBlockMsgPactQueueCounters q
    <*> getNewBlockMsgPactQueueCounters q
    <*> getOtherMsgPactQueueCounters q

getValidateBlockMsgPactQueueCounters :: PactQueue -> IO PactQueueCounters
getValidateBlockMsgPactQueueCounters pq = readIORef (_pactQueueStatsCounters $ _pactQueuePactQueueValidateBlockMsgStats pq)

getNewBlockMsgPactQueueCounters :: PactQueue -> IO PactQueueCounters
getNewBlockMsgPactQueueCounters pq = readIORef (_pactQueueStatsCounters $ _pactQueuePactQueueNewBlockMsgStats pq)

getOtherMsgPactQueueCounters :: PactQueue -> IO PactQueueCounters
getOtherMsgPactQueueCounters pq = readIORef (_pactQueueStatsCounters $ _pactQueuePactQueueOtherMsgStats pq)
