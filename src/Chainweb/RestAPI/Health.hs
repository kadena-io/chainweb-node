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
  , setHealth
  ) where

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import Servant
import System.IO.Unsafe

import Chainweb.RestAPI.Utils

type HealthCheckApi = "health-check" :> Get '[PlainText] Text

someHealthCheckApi :: SomeApi
someHealthCheckApi = SomeApi (Proxy @HealthCheckApi)

data HealthStatus = Healthy | Unhealthy

globalHealthStatus :: MVar HealthStatus
globalHealthStatus = unsafePerformIO $! newMVar Healthy
{-# NOINLINE globalHealthStatus #-}

someHealthCheckServer :: SomeServer
someHealthCheckServer = SomeServer (Proxy @HealthCheckApi) handler
  where
    drainMsg = "Failed health check due to service drain.\n"
    handler = do
        h <- liftIO $ readMVar globalHealthStatus
        case h of
          Healthy -> return "Health check OK.\n"
          Unhealthy -> throwError $ setErrText drainMsg err503

setHealth :: HealthStatus -> IO ()
setHealth !h = void $ swapMVar globalHealthStatus h
