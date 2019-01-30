{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Pact.Service.PactApi
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution HTTP API for Chainweb

module Chainweb.Pact.Service.PactApi
    (
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM

import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Int
import Data.List
import Data.Maybe
import Data.Time.Calendar

import GHC.Generics

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import System.Directory

import Chainweb.BlockHeader
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.PactQueue

type PactAPI = "new" :> ReqBody '[JSON] BlockHeader :> Post '[JSON] RequestId
      :<|> "validate" :> ReqBody '[JSON] BlockHeader :> Post '[JSON] RequestId
      :<|> "poll" :> Capture "requestId" RequestId :> Post '[JSON] BlockPayloadHash

pactServer :: Server PactAPI
pactServer = addNewBlockReq
        :<|> addValidateBlockReq
        :<|> pollForResponse

pactAPI :: Proxy PactAPI
pactAPI = Proxy

pactServiceApp :: Application
pactServiceApp = do
    let reqIdStm = (newTVar (fromIntegral 0) :: STM (TVar Int64))
    reqIdVar <- liftIO $ atomically reqIdStm
    let reqStm  = (newTQueue :: STM (TQueue RequestMsg))
    requestQ <- liftIO $ atomically reqStm
    let respStm = (newTQueue :: STM (TQueue ResponseMsg))
    responseQ <- liftIO $ atomically respStm
    liftIO $ withAsync
                (initPactService requestQ responseQ)
                (\_ -> return ())
    serve pactAPI pactServer
{-
withAsync :: IO a -> (Async a -> IO b) -> IO b
newTQueue :: STM (TQueue a)
newTQueueIO :: IO (TQueue a)

data RequestMsg = RequestMsg
    { _reqRequestType :: RequestType
    , _reqBlockHeader :: BlockHeader
    }

data ResponseMsg = ResponseMsg
    { _respRequestType :: RequestType
    , _respBlockHeader :: BlockPayloadHash
    }
-}

addNewBlockReq :: BlockHeader -> Handler RequestId
addNewBlockReq header = do
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqBlockHeader = header }
    reqId <- liftIO $ addRequest msg
    return reqId

addValidateBlockReq :: BlockHeader -> Handler RequestId
addValidateBlockReq header = do
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqBlockHeader = header }
    reqId <- liftIO $ addRequest msg
    return reqId

pollForResponse :: RequestId -> Handler BlockPayloadHash
pollForResponse = undefined
