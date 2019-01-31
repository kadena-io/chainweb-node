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
import Control.Monad.Trans.Reader
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
import qualified Data.Vault.Lazy as V

import GHC.Generics

import Network.Wai
import Network.Wai.Handler.Warp (run)

import Servant
import Servant.API

import Chainweb.BlockHeader
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types

pactServer :: ServerT PactAPI PactAppM
pactServer = newBlockReq
        :<|> newBlockAsyncReq
        :<|> validateBlockReq
        :<|> validateBlockAsyncReq
        :<|> pollForResponse

toHandler :: RequestIdEnv -> PactAppM a -> Handler a
toHandler env x = runReaderT x env

app :: RequestIdEnv -> Application
app env = serve pactAPI $ hoistServer pactAPI (toHandler env) pactServer

newBlockReq :: BlockHeader -> PactAppM (Either String BlockPayloadHash)
newBlockReq header = do
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqBlockHeader = header }
    reqId <- liftIO $ addRequest msg
    return $ waitForResponse reqId

waitForResponse :: RequestId -> Either String BlockPayloadHash
waitForResponse = undefined

newBlockAsyncReq :: BlockHeader -> PactAppM RequestId
newBlockAsyncReq header = do
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqBlockHeader = header }
    reqId <- liftIO $ addRequest msg
    return reqId

validateBlockReq :: BlockHeader -> PactAppM (Either String BlockPayloadHash)
validateBlockReq header = do
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqBlockHeader = header }
    reqId <- liftIO $ addRequest msg
    return $ waitForResponse reqId

validateBlockAsyncReq :: BlockHeader -> PactAppM RequestId
validateBlockAsyncReq header = do
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqBlockHeader = header }
    reqId <- liftIO $ addRequest msg
    return reqId

pollForResponse :: RequestId -> PactAppM (Either String BlockPayloadHash)
pollForResponse = undefined
