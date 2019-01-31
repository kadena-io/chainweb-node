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
    ( run
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM

import Network.Wai

import Servant

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

run :: Application
run request _respond = do
    let reqIdStm = (newTVar (RequestId 0) :: STM (TVar RequestId))
    let reqQStm  = (newTQueue :: STM (TQueue RequestMsg))
    let respQStm = (newTQueue :: STM (TQueue ResponseMsg))
    let env = RequestIdEnv
          { _rieReqIdStm = reqIdStm
          , _rieReqQStm = reqQStm
          , _rieRespQStm = respQStm }
    liftIO $ withAsync
                (initPactService reqQStm respQStm reqIdStm)
                (\_ -> return ())
    app env request _respond

app :: RequestIdEnv -> Application
app env = serve pactAPI $ hoistServer pactAPI (toHandler env) pactServer

newBlockReq :: BlockHeader -> PactAppM (Either String BlockPayloadHash)
newBlockReq bHeader = do
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqBlockHeader = bHeader }
    reqId <- liftIO $ addRequest msg
    return $ waitForResponse reqId

waitForResponse :: RequestId -> Either String BlockPayloadHash
waitForResponse = undefined

newBlockAsyncReq :: BlockHeader -> PactAppM RequestId
newBlockAsyncReq bHeader = do
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqBlockHeader = bHeader }
    reqId <- liftIO $ addRequest msg
    return reqId

validateBlockReq :: BlockHeader -> PactAppM (Either String BlockPayloadHash)
validateBlockReq bHeader = do
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqBlockHeader = bHeader }
    reqId <- liftIO $ addRequest msg
    return $ waitForResponse reqId

validateBlockAsyncReq :: BlockHeader -> PactAppM RequestId
validateBlockAsyncReq bHeader = do
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock

          , _reqBlockHeader = bHeader }
    reqId <- liftIO $ addRequest msg
    return reqId

pollForResponse :: RequestId -> PactAppM (Either String BlockPayloadHash)
pollForResponse = undefined
