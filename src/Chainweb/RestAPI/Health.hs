{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | An endpoint for toggleable health checking. Used to report
-- readiness/unreadiness for serving to a load balancer.

module Chainweb.RestAPI.Health
  ( HealthCheckApi
  , HealthStatus
  , someHealthCheckApi
  , someHealthCheckServer
  , newHealthCheckServer
  , setHealth
  ) where

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Proxy
import Data.String
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import Servant
import System.IO.Unsafe

import Web.DeepRoute

import Chainweb.RestAPI.Utils

type HealthCheckApi = "health-check" :> Get '[PlainText] Text

someHealthCheckApi :: SomeApi
someHealthCheckApi = SomeApi (Proxy @HealthCheckApi)

data HealthStatus = Healthy | Unhealthy

globalHealthStatus :: MVar HealthStatus
globalHealthStatus = unsafePerformIO $! newMVar Healthy
{-# NOINLINE globalHealthStatus #-}

healthCheckHandler :: (MonadIO m, IsString s) => m s
healthCheckHandler = liftIO $ do
    h <- readMVar globalHealthStatus
    case h of
        Healthy -> return "Health check OK.\n"
        Unhealthy -> errorWithStatus serviceUnavailable503 ""

someHealthCheckServer :: SomeServer
someHealthCheckServer = SomeServer (Proxy @HealthCheckApi) healthCheckHandler

newHealthCheckServer :: Route Application
newHealthCheckServer = seg "health-check" $
    endpoint methodGet "text/plain" $ \_ resp ->
        resp . responseLBS ok200 [] =<< healthCheckHandler

setHealth :: HealthStatus -> IO ()
setHealth !h = void $ swapMVar globalHealthStatus h
