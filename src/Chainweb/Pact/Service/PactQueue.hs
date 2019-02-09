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

import System.Time.Extra

import Chainweb.Pact.Service.Types

addRequest :: IO (TVar (TQueue RequestMsg)) -> RequestMsg -> IO ()
addRequest reqQVar msg = do
    var <- reqQVar
    q <- atomically $ readTVar var
    atomically $ writeTQueue q msg
    return ()

getNextRequest :: IO (TVar (TQueue RequestMsg)) -> IO RequestMsg
getNextRequest reqQVar = do
    var <- reqQVar
    q <- atomically $ readTVar var
    mayM <- timeout 5.0 (tryRead q)
    case mayM of
        Just m -> return m
        Nothing -> error "No! (tryRead timeout)"
      where
        tryRead :: TQueue RequestMsg -> IO RequestMsg
        tryRead ku = do
            maybeMsg <- atomically $ tryReadTQueue ku
            case maybeMsg of
              Just msg -> return msg
              Nothing -> tryRead ku

addResponse :: IO (TVar (TQueue ResponseMsg)) -> ResponseMsg -> IO ()
addResponse respQVar msg = do
    var <- respQVar
    q <- atomically $ readTVar var
    atomically $ writeTQueue q msg
    return ()

getNextResponse :: IO (TVar (TQueue ResponseMsg)) -> IO ResponseMsg
getNextResponse respQVar = do
    var <- respQVar
    q <- atomically $ readTVar var
    respMsg <- atomically $ readTQueue q
    return respMsg
