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
    , sendCloseMsg
    ) where

import Control.Concurrent.STM.TQueue
import Control.Monad.STM

import Chainweb.Pact.Service.Types

-- | Add a request to the Pact execution queue
addRequest :: TQueue RequestMsg -> RequestMsg -> IO ()
addRequest q msg = do
    atomically $ writeTQueue q msg

-- | Send special 'close' message to stop the processing thread
sendCloseMsg :: TQueue RequestMsg -> IO ()
sendCloseMsg q = do
    atomically $ writeTQueue q CloseMsg

-- | Get the next available request from the Pact execution queue
getNextRequest :: TQueue RequestMsg -> IO RequestMsg
getNextRequest q = do
    atomically $ readTQueue q
