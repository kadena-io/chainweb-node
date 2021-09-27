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
    (
      newPactQueue
    , PactQueue(..)
    , PactQueueAccess(..)
    ) where

import Control.Applicative
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import Numeric.Natural

import Chainweb.Pact.Service.Types

class PactQueueAccess queue where
  -- | Add a request to the Pact execution queue
  addRequest :: queue -> RequestMsg -> IO ()
  -- | Get the next available request from the Pact execution queue
  getNextRequest :: queue -> IO RequestMsg

data PactQueue = PactQueue
  {
    validateBlockQueue :: TBQueue RequestMsg
  , newBlockQueue :: TBQueue RequestMsg
  , otherMsgsQueue :: TBQueue RequestMsg
  }

newPactQueue :: Natural -> STM PactQueue
newPactQueue sz = do
    validateBlockQueue <- newTBQueue sz
    newBlockQueue <- newTBQueue sz
    otherMsgsQueue <- newTBQueue sz
    return PactQueue {..}

instance PactQueueAccess PactQueue where
  addRequest PactQueue{..} msg = atomically $
    case msg of
      ValidateBlockMsg {} -> writeTBQueue validateBlockQueue msg
      NewBlockMsg {} -> writeTBQueue newBlockQueue msg
      _ -> writeTBQueue otherMsgsQueue msg

  getNextRequest PactQueue{..} = atomically $ do
    v <- tryReadTBQueue validateBlockQueue
    b <- tryReadTBQueue newBlockQueue
    o <- tryReadTBQueue otherMsgsQueue
    case v <|> b <|> o of
      Nothing -> retry
      Just msg -> return msg
