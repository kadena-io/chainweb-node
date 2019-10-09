-- |
-- Module: Chainweb.Pact.Service.PactQueue
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution service queue for Chainweb

module Chainweb.Pact.Service.PactQueue
( PactQueue
, newPactQueue
, addRequest
, getNextRequest
, sendCloseMsg
) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.BoundedChan (BoundedChan)
import qualified Control.Concurrent.BoundedChan as BChan

import Chainweb.Pact.Service.Types

type PactQueue = BoundedChan RequestMsg

-- | Add a request to the Pact execution queue
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest = BChan.writeChan

-- TODO: add tryAddRequest here? non-essential uses of pact queue (like mempool
-- gossip) could fail fast on a blocked queue instead of making the load
-- problem worse

-- TODO: kill pact service with exception instead of enqueue

-- | Send special 'close' message to stop the processing thread
sendCloseMsg :: PactQueue -> IO ()
sendCloseMsg = flip addRequest CloseMsg

-- | Get the next available request from the Pact execution queue
getNextRequest :: PactQueue -> IO RequestMsg
getNextRequest = BChan.readChan

-- | Make a new pact service queue.
newPactQueue :: IO PactQueue
newPactQueue = do
    caps <- getNumCapabilities
    BChan.newBoundedChan $! caps * multiplier
  where
    multiplier :: Int
    multiplier = 8
