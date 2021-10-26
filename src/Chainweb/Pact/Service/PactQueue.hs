{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
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
    , newPactQueue
    , PactQueue
    ) where

import Control.Applicative
import Control.Concurrent.STM.TBQueue
import Control.Monad ((>=>))
import Control.Monad.STM
import Data.Tuple.Strict
import Numeric.Natural
import System.LogLevel

import Data.LogMessage
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
  }

newPactQueue :: Natural -> STM PactQueue
newPactQueue sz = do
  _pactQueueValidateBlock <- newTBQueue sz
  _pactQueueNewBlock <- newTBQueue sz
  _pactQueueOtherMsg <- newTBQueue sz
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
getNextRequest :: LogFunctionText -> PactQueue -> IO RequestMsg
getNextRequest logger q = do
  (T2 req entranceTime) <- atomically $
    tryReadTBQueueOrRetry (_pactQueueValidateBlock q)
    <|> tryReadTBQueueOrRetry (_pactQueueNewBlock q)
    <|> tryReadTBQueueOrRetry (_pactQueueOtherMsg q)
  exitTime <- getCurrentTimeIntegral
  let requestTime = exitTime `diff` entranceTime
  case req of
    ValidateBlockMsg {} -> logger Warn ("PactQueue: ValidateBlockMsg took " <> sshow requestTime <> " microseconds.")
    NewBlockMsg {} -> logger Warn ("PactQueue: NewBlockMsg took " <> sshow requestTime <> " microseconds.")
    _ -> logger Warn ("PactQueue: OtherMsg took " <> sshow requestTime <> " microseconds.")
  return req
  where
    tryReadTBQueueOrRetry = tryReadTBQueue >=> \case
      Nothing -> retry
      Just msg -> return msg
