{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp

import Servant

import System.Time.Extra

import Chainweb.BlockHeader
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types

pactServer :: ServerT PactAPI PactAppM
pactServer = newBlockReq
        :<|> newBlockAsyncReq
        :<|> validateBlockReq
        :<|> validateBlockAsyncReq
        :<|> pollForResponse

toHandler :: RequestIdEnv -> PactAppM a -> Handler a
toHandler env x = runReaderT x env

withPactServiceApp :: Int -> MemPoolAccess -> IO a -> IO a
withPactServiceApp port memPoolAccess action = do
    reqQ <- atomically (newTQueue :: STM (TQueue RequestMsg))
    let reqQVar = atomically $ newTVar reqQ
    respQ  <- atomically (newTQueue :: STM (TQueue ResponseMsg))
    let respQVar = atomically $ newTVar respQ
    withAsync (initPactService reqQVar respQVar memPoolAccess) $ \a -> do
        link a
        let reqIdVar = atomically (newTVar (RequestId 0) :: STM (TVar RequestId))
        let ht =  H.new :: IO (H.IOHashTable HashTable RequestId Transactions)
        let env = RequestIdEnv
              { _rieReqIdVar = reqIdVar
              , _rieReqQ = reqQVar
              , _rieRespQ = respQVar
              , _rieResponseMap = ht }
        bracket (liftIO $ forkIO $ Warp.run port (pactServiceApp env))
            killThread
            (const action)

pactServiceApp :: RequestIdEnv -> Application
pactServiceApp env = serve pactAPI $ hoistServer pactAPI (toHandler env) pactServer

incRequestId :: PactAppM RequestId
incRequestId = do
    reqIdVarIO <- view rieReqIdVar
    reqIdVar <- liftIO reqIdVarIO
    liftIO $ atomically $ modifyTVar' reqIdVar succ
    liftIO $ readTVarIO reqIdVar

newBlockReq :: BlockHeader -> PactAppM (Either String Transactions)
newBlockReq bHeader = do
    newReqId <- incRequestId
    reqQ <- view rieReqQ
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQ msg
    waitForResponse newReqId

newBlockAsyncReq :: BlockHeader -> PactAppM RequestId
newBlockAsyncReq bHeader = do
    newReqId <- incRequestId
    reqQ <- view rieReqQ
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqRequestId = newReqId
          , _reqBlockHeader = bHeader }
    liftIO $ addRequest reqQ msg
    return newReqId

validateBlockReq :: BlockHeader -> PactAppM (Either String Transactions)
validateBlockReq bHeader = do
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

waitForResponse :: RequestId -> PactAppM (Either String Transactions)
waitForResponse requestId = do
    respQVar <- view rieRespQ
    respHTableIO <- view rieResponseMap
    respHTable <- liftIO respHTableIO
    t <- liftIO $ timeout (fromIntegral timeoutSeconds) (go respQVar respHTable)
    case t of
        Nothing -> return $ Left $ "Timeout occured waiting for response to: " ++ show requestId
        Just payload -> return $ Right payload
    where
        go :: IO (TVar (TQueue ResponseMsg))
            -> H.IOHashTable HashTable RequestId Transactions
            -> IO Transactions
        go respQVar respTable = do
            resp <- getNextResponse respQVar
            H.insert respTable (_respRequestId resp) (_respPayload resp)
            x <- H.lookup respTable requestId
            case x of
                Just payload -> return payload
                Nothing -> go respQVar respTable

pollForResponse :: RequestId -> PactAppM (Either String Transactions)
pollForResponse requestId = do
    respQ <- view rieRespQ
    resp <- liftIO $ getNextResponse respQ
    respTableIO <- view rieResponseMap
    respTable <- liftIO respTableIO
    liftIO $ H.insert respTable (_respRequestId resp) (_respPayload resp)
    x <- liftIO $ H.lookup respTable requestId
    case x of
        Just payload -> return $ Right payload
        Nothing -> return $ Left $ "Result not yet available for: " ++ show requestId
