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
    , withPactServiceApp
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Exception hiding (Handler)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM

import qualified Data.HashTable.IO as H
import Data.HashTable.ST.Basic (HashTable)

import Debug.Trace

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp

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

withPactServiceApp :: Int -> IO () -> IO ()
withPactServiceApp port action = do
    let reqQ  = atomically $ (newTQueue :: STM (TQueue RequestMsg))
    let respQ = atomically $ (newTQueue :: STM (TQueue ResponseMsg))
    withAsync (initPactService reqQ respQ) $ \a -> do
        link a
        let threadId = asyncThreadId a
        let reqIdVar = trace ("async created with thread id: " ++ show (threadId))
                            (atomically $ (newTVar (RequestId 0) :: STM (TVar RequestId)))

        let ht =  H.new :: IO (H.IOHashTable HashTable RequestId BlockPayloadHash)
        let env = RequestIdEnv
              { _rieReqIdVar = reqIdVar
              , _rieReqQ = reqQ
              , _rieRespQ = respQ
              , _rieResponseMap = ht }
        bracket (liftIO $ forkIO $ Warp.run port (pactServiceApp env))
            killThread
            (const action)

pactServiceApp :: RequestIdEnv -> Application
pactServiceApp env = serve pactAPI $ hoistServer pactAPI (toHandler env) pactServer

incRequestId :: PactAppM RequestId
incRequestId = do
    reqIdVarIO <- view rieReqIdVar
    reqIdVar <- liftIO $ reqIdVarIO
    liftIO $ atomically $ modifyTVar' reqIdVar succ
    newReqId <- liftIO $ atomically $ readTVar reqIdVar
    return newReqId

newBlockReq :: BlockHeader -> PactAppM (Either String BlockPayloadHash)
newBlockReq bHeader = do
    newReqId <- incRequestId
    reqQ <- view rieReqQ
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQ msg
    resp <- waitForResponse newReqId
    return resp

newBlockAsyncReq :: BlockHeader -> PactAppM RequestId
newBlockAsyncReq bHeader = do
    _ <- error "PactApi - newBlockAsyncReq"
    newReqId <- incRequestId
    reqQ <- view rieReqQ
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQ msg
    return newReqId

validateBlockReq :: BlockHeader -> PactAppM (Either String BlockPayloadHash)
validateBlockReq bHeader = do
    _ <- error "PactApi - validateBlockReq"
    newReqId <- incRequestId
    reqQ <- view rieReqQ
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQ msg
    waitForResponse newReqId

validateBlockAsyncReq :: BlockHeader -> PactAppM RequestId
validateBlockAsyncReq bHeader = do
    _ <- error "PactApi - validateBlockAsyncReq"
    newReqId <- incRequestId
    reqQ <- view rieReqQ
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQ msg
    return newReqId

-- TODO: Get timeout value from config
timeoutSeconds :: Int
timeoutSeconds = 30

waitForResponse :: RequestId -> PactAppM (Either String BlockPayloadHash)
waitForResponse requestId = do
    respQ <- view rieRespQ
    respHTableIO <- view rieResponseMap
    respHTable <- liftIO $ respHTableIO
    t <- liftIO $ timeout (fromIntegral timeoutSeconds) (go respQ respHTable)
    case t of
        Nothing -> do
            _ <- error "Left"
            return $ Left $ "Timeout occured waiting for response to: " ++ show requestId
        Just payload -> do
            _ <- error "Right"
            return $ Right payload
      where
        go
            :: IO (TQueue ResponseMsg)
            -> H.IOHashTable HashTable RequestId BlockPayloadHash
            -> IO BlockPayloadHash
        go rQ respTable = do
            -- _ <- error "b4 getNextResponse"
            resp <- getNextResponse rQ
            _ <- error "aft getNextResponse"
            H.insert respTable (_respRequestId resp) (_respPayloadHash resp)
            x <- H.lookup respTable requestId
            case x of
                Just payload -> do
                  liftIO $ putStrLn "Lookup returned 'Just'"
                  return payload
                Nothing -> do
                  liftIO $ putStrLn "Lookup returned 'Nothing'"
                  go rQ respTable

pollForResponse :: RequestId -> PactAppM (Either String BlockPayloadHash)
pollForResponse requestId = do
    respQ <- view rieRespQ
    resp <- liftIO $ getNextResponse respQ
    respTableIO <- view rieResponseMap
    respTable <- liftIO respTableIO
    liftIO $ H.insert respTable (_respRequestId resp) (_respPayloadHash resp)
    x <- liftIO $ H.lookup respTable requestId
    case x of
        Just payload -> return $ Right payload
        Nothing -> return $ Left $ "Result not yet available for: " ++ show requestId
