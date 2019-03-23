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

module Chainweb.Pact.Service.Http.PactApi
    ( pactServer
    , pactServiceApp
    , withPactServiceApp
    ) where

import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Exception hiding (Handler)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import Data.Text (Text)

import Network.Socket (Socket, close)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp

import Servant

import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Service.Http.Types
import Chainweb.Pact.Types

import Pact.Types.Command

-- | Servant definition for Pact Execution as a service
pactServer :: ServerT PactAPI PactAppM
pactServer = localReq

toHandler :: LocalEnv -> PactAppM a -> Handler a
toHandler env x = runReaderT x env

-- | Entry point for Pact Execution service
withPactServiceApp :: Either Socket Int -> Warp.HostPreference -> (TQueue RequestMsg) -> IO a -> IO a
withPactServiceApp socketOrPort hostPreference reqQ action = do
    let env = LocalEnv { _rieReqQ = reqQ }
    let runWarp = case socketOrPort of
          Left socket -> flip Warp.runSettingsSocket socket
              $ Warp.setHost hostPreference
                Warp.defaultSettings
          Right port -> Warp.runSettings
              $ Warp.setPort port
              $ Warp.setHost hostPreference
                Warp.defaultSettings
    let closeSocket = case socketOrPort of
          Left socket -> close socket
          Right _ -> return ()
    bracket (liftIO $ forkIO $ runWarp (pactServiceApp env))
        (\t -> closeSocket >> killThread t)
        (const action)

pactServiceApp :: LocalEnv -> Application
pactServiceApp env = serve pactAPI $ hoistServer pactAPI (toHandler env) pactServer

-- | Handler for "local" requests
localReq :: Command Text -> PactAppM (Either PactException FullLogTxOutput)
localReq bHeader = do
    reqQ <- view rieReqQ
    respVar <- liftIO $ newEmptyMVar
    let msg = LocalMsg LocalReq
          { _localRequest = bHeader
          , _localResultVar = respVar}
    liftIO $ addRequest reqQ msg
    rsp  <- liftIO $ takeMVar respVar -- TODO: Maybe add some timeout value here
    return rsp
