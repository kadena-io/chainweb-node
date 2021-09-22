{-# LANGUAGE RecordWildCards #-}
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
import Control.Monad.STM
import Numeric.Natural

import Chainweb.Pact.Service.Types

-- | The type of the Pact Queue
-- type PactQueue = TBQueue RequestMsg
data PactQueue = PactQueue
  {
    _pactQueueValidateBlock :: !(TBQueue RequestMsg)
  , _pactQueueNewBlock :: !(TBQueue RequestMsg)
  , _pactQueueOtherMsg :: !(TBQueue RequestMsg)
  }

newPactQueue :: Natural -> STM PactQueue
newPactQueue sz = do
  _pactQueueValidateBlock <- newTBQueue sz
  _pactQueueNewBlock <- newTBQueue sz
  _pactQueueOtherMsg <- newTBQueue sz
  return PactQueue {..}

-- | Add a request to the Pact execution queue
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest q msg = atomically $
  case msg of
    ValidateBlockMsg {} -> writeTBQueue (_pactQueueValidateBlock q) msg
    NewBlockMsg {} -> writeTBQueue (_pactQueueNewBlock q) msg
    _ -> writeTBQueue (_pactQueueOtherMsg q) msg

-- | Get the next available request from the Pact execution queue
getNextRequest :: PactQueue -> IO RequestMsg
getNextRequest q = atomically $ do
  v <- tryReadTBQueue (_pactQueueValidateBlock q)
  b <- tryReadTBQueue (_pactQueueNewBlock q)
  o <- tryReadTBQueue (_pactQueueOtherMsg q)
  case v <|> o <|> b of
    Nothing -> retry
    Just msg -> return msg
