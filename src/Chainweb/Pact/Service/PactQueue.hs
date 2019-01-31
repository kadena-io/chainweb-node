{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

addRequest :: STM (TQueue RequestMsg) -> RequestMsg -> IO ()
addRequest reqQStm msg = do
    q <- atomically reqQStm
    atomically $ writeTQueue q msg

getNextRequest :: STM (TQueue RequestMsg) -> IO RequestMsg
getNextRequest reqQStm = do
    q <- atomically reqQStm
    atomically $ readTQueue q

addResponse :: ResponseMsg -> IO RequestId
addResponse _msg = undefined

getNextResponse :: STM (TQueue ResponseMsg) -> IO ResponseMsg
getNextResponse respQStm = do
    q <- atomically respQStm
    atomically $ readTQueue q
