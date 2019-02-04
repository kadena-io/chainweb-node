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
    ( newBlockReq
    , pactServer
    , pactServiceApp
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM

import qualified Data.HashTable.IO as H
import Data.HashTable.ST.Basic (HashTable)

import Network.Wai

import Servant

import System.Time.Extra

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

pactServiceApp :: Application
pactServiceApp request _respond = do
    let reqIdStm = (newTVar (RequestId 0) :: STM (TVar RequestId))
    let reqQStm  = (newTQueue :: STM (TQueue RequestMsg))
    let respQStm = (newTQueue :: STM (TQueue ResponseMsg))
    ht <- H.new :: IO (H.IOHashTable HashTable RequestId BlockPayloadHash)
    let env = RequestIdEnv
          { _rieReqIdStm = reqIdStm
          , _rieReqQStm = reqQStm
          , _rieRespQStm = respQStm
          , _rieResponseMap = ht }
    liftIO $ withAsync
                (initPactService reqQStm respQStm)
                (\_ -> return ())
    app env request _respond

app :: RequestIdEnv -> Application
app env = serve pactAPI $ hoistServer pactAPI (toHandler env) pactServer

incRequestId :: PactAppM RequestId
incRequestId = do
    reqIdStm <- view rieReqIdStm
    reqIdVar <- liftIO $ atomically reqIdStm
    liftIO $ atomically $ modifyTVar' reqIdVar succ
    newReqId <- liftIO $ atomically $ readTVar reqIdVar
    return newReqId

newBlockReq :: BlockHeader -> PactAppM (Either String BlockPayloadHash)
newBlockReq bHeader = do
    newReqId <- incRequestId
    reqQStm <- view rieReqQStm
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQStm msg
    waitForResponse newReqId

newBlockAsyncReq :: BlockHeader -> PactAppM RequestId
newBlockAsyncReq bHeader = do
    newReqId <- incRequestId
    reqQStm <- view rieReqQStm
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQStm msg
    return newReqId

validateBlockReq :: BlockHeader -> PactAppM (Either String BlockPayloadHash)
validateBlockReq bHeader = do
    newReqId <- incRequestId
    reqQStm <- view rieReqQStm
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQStm msg
    waitForResponse newReqId

validateBlockAsyncReq :: BlockHeader -> PactAppM RequestId
validateBlockAsyncReq bHeader = do
    newReqId <- incRequestId
    reqQStm <- view rieReqQStm
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQStm msg
    return newReqId

-- TODO: Get timeout value from config
timeoutSeconds :: Int
timeoutSeconds = 30

waitForResponse :: RequestId -> PactAppM (Either String BlockPayloadHash)
waitForResponse requestId = do
    respQStm <- view rieRespQStm
    respHTable <- view rieResponseMap
    t <- liftIO $ timeout (fromIntegral timeoutSeconds) (go respQStm respHTable)
    case t of
        Nothing -> return $ Left $ "Timeout occured waiting for response to: " ++ show requestId
        Just payload -> do
            return $ Right payload
      where
        go
            :: STM (TQueue ResponseMsg)
            -> H.IOHashTable HashTable RequestId BlockPayloadHash
            -> IO BlockPayloadHash
        go respQ respTable = do
            resp <- getNextResponse respQ
            H.insert respTable (_respRequestId resp) (_respPayloadHash resp)
            x <- H.lookup respTable requestId
            case x of
                Just payload -> return payload
                Nothing -> go respQ respTable

pollForResponse :: RequestId -> PactAppM (Either String BlockPayloadHash)
pollForResponse requestId = do
    respQStm <- view rieRespQStm
    resp <- liftIO $ getNextResponse respQStm
    respTable <- view rieResponseMap
    liftIO $ H.insert respTable (_respRequestId resp) (_respPayloadHash resp)
    respMap <- view rieResponseMap
    x <- liftIO $ H.lookup respMap requestId
    case x of
        Just payload -> return $ Right payload
        Nothing -> return $ Left $ "Result not yet available for: " ++ show requestId
