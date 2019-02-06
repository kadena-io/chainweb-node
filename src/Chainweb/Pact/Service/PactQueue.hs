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
import Control.Monad.STM

import Debug.Trace

import System.Time.Extra

import Chainweb.Pact.Service.Types

addRequest :: IO (TQueue RequestMsg) -> RequestMsg -> IO ()
addRequest reqQ msg = do
    q <- reqQ
    atomically $ writeTQueue q msg
    trace ("addRequest -- added: " ++ show msg) (return ())

    -- TODO: remove this
    maybePeek <- atomically $ tryPeekTQueue q
    case maybePeek of
        Just _mp -> do
            trace "maybe peek works as expected" (return ())
        Nothing -> do
            trace "maybePeek failed??" (return ())

getNextRequest :: IO (TQueue RequestMsg) -> IO RequestMsg
getNextRequest reqQ = do
    q <- trace "top of getNextRequest" reqQ
    -- reqMsg <- atomically $ readTQueue q
    -- trace ("getNextRequest - received request msg: " ++ show reqMsg) $ return reqMsg

    -- mayM <- timeout (fromIntegral 5) (tryRead q)
    mayM <- timeout 5.0 (tryRead q)
    case mayM of
        Just m -> do
            _ <- error "Yes! (tryRead)"
            return m
        Nothing -> error "No! (tryRead timeout)"
      where
        tryRead :: TQueue RequestMsg -> IO RequestMsg
        tryRead ku = do
            maybeMsg <- atomically $ tryReadTQueue ku
            case maybeMsg of
              Just msg -> do
                  trace "Just msg" (return msg)
              Nothing -> tryRead ku

addResponse :: IO (TQueue ResponseMsg) -> ResponseMsg -> IO ()
addResponse respQ msg = do
    q <- respQ
    atomically $ writeTQueue q msg
    trace ("addResponse -- added: " ++ show msg) (return ())

getNextResponse :: IO (TQueue ResponseMsg) -> IO ResponseMsg
getNextResponse respQ = do
    q <- respQ
    respMsg <- atomically $ readTQueue q
    trace ("getNextResponse - received response msg: " ++ show respMsg) $ return respMsg
