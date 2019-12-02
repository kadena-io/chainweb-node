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
    , PactQueue(..)
    ) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM

import Chainweb.Pact.Service.Types

-- | The type of the Pact Queue
newtype PactQueue = PactQueue (TBQueue RequestMsg, TBQueue RequestMsg)

-- | Add a request to the Pact execution queue
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest (PactQueue (rq, vq)) msg = atomically $
    case msg of
      ValidateBlockMsg _ -> writeTBQueue vq msg
      _ -> writeTBQueue rq msg

-- | Get the next available request from the Pact execution queue
getNextRequest :: PactQueue -> IO RequestMsg
getNextRequest (PactQueue (rq, vq)) = atomically $ do
    mnext <- tryReadTBQueue vq
    case mnext of
      Nothing -> readTBQueue rq
      Just next -> return next
