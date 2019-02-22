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

import Chainweb.BlockHeader
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types

-- | Servant definition for Pact Execution as a service
pactServer :: ServerT PactAPI PactAppM
pactServer = newBlockReq
        :<|> validateBlockReq
        :<|> pollForResponse

toHandler :: RequestIdEnv -> PactAppM a -> Handler a
toHandler env x = runReaderT x env

-- | Entry point for Pact Execution service
withPactServiceApp :: Int -> MemPoolAccess -> IO a -> IO a
withPactServiceApp port memPoolAccess action = do
    reqQ <- atomically (newTQueue :: STM (TQueue RequestHttpMsg))
    reqQVar <- atomically $ newTVar reqQ
    respQ  <- atomically (newTQueue :: STM (TQueue ResponseHttpMsg))
    respQVar <- atomically $ newTVar respQ
    withAsync (initPactServiceHttp reqQVar respQVar memPoolAccess) $ \a -> do
        link a
        reqIdVar <- atomically (newTVar (RequestId 0) :: STM (TVar RequestId))
        ht <-  H.new :: IO (H.IOHashTable HashTable RequestId Transactions)
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

-- TODO: request Id to be replaced with request hash
incRequestId :: PactAppM RequestId
incRequestId = do
    reqIdVar <- view rieReqIdVar
    liftIO $ atomically $ modifyTVar' reqIdVar succ
    liftIO $ readTVarIO reqIdVar

-- | Handler for new block requests (async, returning RequestId immediately for future polling)
newBlockReq :: BlockHeader -> PactAppM RequestId
newBlockReq bHeader = do
    newReqId <- incRequestId
    reqQ <- view rieReqQ
    let msg = RequestHttpMsg
          { _reqhRequestType = NewBlock
          , _reqhRequestId = newReqId
          , _reqhBlockHeader = bHeader }
    liftIO $ addHttpRequest reqQ msg
    return newReqId

-- | Handler for validate block requests (async, returning RequestId immediately for future polling)
validateBlockReq :: BlockHeader -> PactAppM RequestId
validateBlockReq bHeader = do
    newReqId <- incRequestId
    reqQ <- view rieReqQ
    let msg = RequestHttpMsg
          { _reqhRequestType = ValidateBlock
          , _reqhRequestId = newReqId
          , _reqhBlockHeader = bHeader }
    liftIO $ addHttpRequest reqQ msg
    return newReqId

-- | Handler for polling on a RequestId
pollForResponse :: RequestId -> PactAppM (Either String Transactions)
pollForResponse requestId = do
    respQ <- view rieRespQ
    resp <- liftIO $ getNextResponse respQ
    respTable <- view rieResponseMap
    liftIO $ H.insert respTable (_resphRequestId resp) (_resphPayload resp)
    x <- liftIO $ H.lookup respTable requestId
    case x of
        Just payload -> return $ Right payload
        Nothing -> return $ Left $ "Result not yet available for: " ++ show requestId
