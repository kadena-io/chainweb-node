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
    , addResponse
    , getNextRequest
    , getNextResponse
    , RequestId(..)
    , RequestMsg(..)
    , RequestType(..)
    , ResponseMsg(..)
    ) where

import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Chainweb.Pact.Service.Types

-- | Add a request to the Pact execution queue
addRequest :: TVar (TQueue RequestMsg) -> RequestMsg -> IO ()
addRequest var msg = do
    q <- readTVarIO var
    atomically $ writeTQueue q msg
    return ()

-- | Get the next available request from the Pact execution queue
getNextRequest :: TVar (TQueue RequestMsg) -> IO RequestMsg
getNextRequest var = do
    q <- readTVarIO var
    atomically $ readTQueue q

-- | Add a response to the Pact execution response queue
addResponse :: TVar (TQueue ResponseMsg) -> ResponseMsg -> IO ()
addResponse var msg = do
    q <- readTVarIO var
    atomically $ writeTQueue q msg
    return ()

-- | Get the next available response from the Pact execution response queue
getNextResponse :: TVar (TQueue ResponseMsg) -> IO ResponseMsg
getNextResponse var = do
    q <- readTVarIO var
    atomically $ readTQueue q
