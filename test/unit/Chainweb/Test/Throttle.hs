{-# language OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Chainweb.Test.Throttle (tests) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar
import Data.Maybe
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

import qualified Network.HTTP.Client as Client
import Network.HTTP.Types (status200, status429)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

import Chainweb.Time
import Chainweb.Utils ((&))
import Chainweb.Utils.Throttle

import Test.Tasty
import Test.Tasty.HUnit
import qualified PropertyMatchers as P
import PropertyMatchers ((?))

tests :: TestTree
tests = testGroup "Chainweb.Test.Throttle"
    [ testCase "request cost" $ runTest
        mempty
        ThrottleConfig
            { _requestCost = 1
            , _requestBody100ByteCost = 0
            , _responseBody100ByteCost = 0
            , _maxBudget = 1
            , _freeRate = 1
            , _throttleExpiry = Seconds 20
            }
        $ \req manager -> do
            Client.httpLbs req manager
                >>= P.fun Client.responseStatus
                ? P.equals status200
            Client.httpLbs req manager
                >>= throttled
    , testCase "request body size cost" $ runTest
        mempty
        ThrottleConfig
            { _requestCost = 1
            , _requestBody100ByteCost = 1
            , _responseBody100ByteCost = 0
            , _maxBudget = 2
            , _freeRate = 1
            , _throttleExpiry = Seconds 20
            }
        $ \req manager -> do
            let req' = req { Client.requestBody = Client.RequestBodyBS $ BSC.replicate 200 'a' }
            Client.httpLbs req' manager
                >>= throttled
    , testCase "response body size penalties do not interrupt the response" $ runTest
        (BSB.byteString $ BSC.replicate 100 'a')
        ThrottleConfig
            { _requestCost = 1
            , _requestBody100ByteCost = 0
            , _responseBody100ByteCost = 1
            , _maxBudget = 2
            , _freeRate = 1
            , _throttleExpiry = Seconds 20
            }
        $ \req manager -> do
            Client.httpLbs req manager
                >>= P.fun Client.responseBody
                ? P.fun LBS.length
                ? P.equals 100
            Client.httpLbs req manager
                >>= throttled
    ]


throttled :: HasCallStack => P.Prop (Client.Response LBS.LazyByteString)
throttled = P.checkAll
    [ P.fun Client.responseBody ? P.equals "host throttled"
    , P.fun Client.responseStatus ? P.equals status429
    ]

runTest
    :: BSB.Builder
    -> ThrottleConfig
    -> (Client.Request -> Client.Manager -> IO ())
    -> IO ()
runTest respBody throttleConfig f = do
    throttleMiddleware (\_ _ -> return ()) "test" throttleConfig $ \mw -> do
        let app = mw $ \_req resp -> resp $ Wai.responseBuilder status200 [] respBody
        (sockPort, sock) <- openFreePort
        readyVar <- newEmptyMVar
        let settings = defaultSettings
                & setBeforeMainLoop (putMVar readyVar ())
                & setOnExceptionResponse (\ex -> fromMaybe (defaultOnExceptionResponse ex) (throttledResponse ex))
                & setOnException (\_ _ -> return ())
        withAsync (runSettingsSocket settings sock app) $ \_ -> do
            takeMVar readyVar
            manager <- Client.newManager Client.defaultManagerSettings
            req <- Client.parseRequest $ "http://127.0.0.1:" <> show sockPort

            f req manager
