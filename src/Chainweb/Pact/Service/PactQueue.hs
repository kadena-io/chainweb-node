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
    , PactQueue
    ) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM

import Chainweb.Pact.Service.Types

-- | The type of the Pact Queue
type PactQueue = TBQueue RequestMsg

-- | Add a request to the Pact execution queue
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest q msg = atomically $ writeTBQueue q msg

-- | Get the next available request from the Pact execution queue
getNextRequest :: PactQueue -> IO RequestMsg
getNextRequest q = atomically $ readTBQueue q
