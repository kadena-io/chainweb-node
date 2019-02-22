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
    , addHttpRequest
    , addResponse
    , getNextHttpRequest
    , getNextRequest
    , getNextResponse
    , RequestId(..)
    , RequestHttpMsg(..)
    , RequestType(..)
    , ResponseHttpMsg(..)
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

--TODO: remove or combine with 'getNextRequest
-- | Add a request to the Pact execution queue
addHttpRequest :: TVar (TQueue RequestHttpMsg) -> RequestHttpMsg -> IO ()
addHttpRequest var msg = do
    q <- readTVarIO var
    atomically $ writeTQueue q msg
    return ()

-- | Get the next available request from the Pact execution queue
getNextRequest :: TVar (TQueue RequestMsg) -> IO RequestMsg
getNextRequest var = do
    q <- readTVarIO var
    atomically $ readTQueue q

--TODO: remove or combine with 'getNextRequest
getNextHttpRequest :: TVar (TQueue RequestHttpMsg) -> IO RequestHttpMsg
getNextHttpRequest var = do
    q <- readTVarIO var
    atomically $ readTQueue q

-- | Add a response to the Pact execution response queue
addResponse :: TVar (TQueue ResponseHttpMsg) -> ResponseHttpMsg -> IO ()
addResponse var msg = do
    q <- readTVarIO var
    atomically $ writeTQueue q msg
    return ()

-- | Get the next available response from the Pact execution response queue
getNextResponse :: TVar (TQueue ResponseHttpMsg) -> IO ResponseHttpMsg
getNextResponse var = do
    q <- readTVarIO var
    atomically $ readTQueue q
