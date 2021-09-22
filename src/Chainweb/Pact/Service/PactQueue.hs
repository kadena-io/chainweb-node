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
      newPactQueueAccess
    , PactQueue
    , PactQueueAccess(..)
    ) where

import Control.Applicative
import Control.Monad.STM
import Control.Concurrent.STM.TBQueue
import Numeric.Natural

import Chainweb.Pact.Service.Types

-- | The type of the Pact Queue
type PactQueue = TBQueue RequestMsg

data PactQueueAccess = PactQueueAccess
  {
    addRequest :: RequestMsg -> IO ()
  , getNextRequest :: IO RequestMsg
  }

newPactQueueAccess :: Natural -> STM PactQueueAccess
newPactQueueAccess sz = do
  vQueue <- newTBQueue sz
  nQueue <- newTBQueue sz
  oQueue <- newTBQueue sz
  return PactQueueAccess
    {
      addRequest = \reqMsg -> atomically $ case reqMsg of
        ValidateBlockMsg {} -> writeTBQueue vQueue reqMsg
        NewBlockMsg {} -> writeTBQueue nQueue reqMsg
        _ -> writeTBQueue oQueue reqMsg
    , getNextRequest = atomically $ do
        v <- tryReadTBQueue vQueue
        n <- tryReadTBQueue nQueue
        o <- tryReadTBQueue oQueue
        case v <|> n <|> o of
          Nothing -> retry
          Just msg -> return msg
    }
