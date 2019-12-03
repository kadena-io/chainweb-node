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
data PactQueue = PactQueue
  { pqPrimaryQueue :: TBQueue RequestMsg
  , pqValidationQueue :: TBQueue RequestMsg
  }

-- | Add a request to the Pact execution queue
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest p msg = atomically $
    case msg of
      ValidateBlockMsg _ -> writeTBQueue (pqValidationQueue p) msg
      _ -> writeTBQueue (pqPrimaryQueue p) msg

-- | Get the next available request from the Pact execution queue
getNextRequest :: PactQueue -> IO RequestMsg
getNextRequest p =
    atomically $ maybeM (readTBQueue (pqPrimaryQueue p)) (tryReadTBQueue (pqValidationQueue p))
  where
    maybeM a b = b >>= maybe a pure
