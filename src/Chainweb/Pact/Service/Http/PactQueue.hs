-- |
-- Module: Chainweb.Pact.Service.Http.PactQueue
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution Http service queue for Chainweb

module Chainweb.Pact.Service.Http.PactQueue
    ( addHttpRequest
    , addResponse
    , getNextHttpRequest
    , getNextResponse
    , RequestType(..)
    ) where

import Control.Concurrent.STM.TQueue
import Control.Monad.STM

import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Http.Types

-- | Add a request to the Pact execution queue
addHttpRequest :: (TQueue RequestHttpMsg) -> RequestHttpMsg -> IO ()
addHttpRequest q msg = do
    atomically $ writeTQueue q msg

getNextHttpRequest :: (TQueue RequestHttpMsg) -> IO RequestHttpMsg
getNextHttpRequest q = do
    atomically $ readTQueue q

-- | Add a response to the Pact execution response queue
addResponse :: (TQueue ResponseHttpMsg) -> ResponseHttpMsg -> IO ()
addResponse q msg = do
    atomically $ writeTQueue q msg
    return ()

-- | Get the next available response from the Pact execution response queue
getNextResponse :: (TQueue ResponseHttpMsg) -> IO ResponseHttpMsg
getNextResponse q = do
    atomically $ readTQueue q
