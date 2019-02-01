{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Test.PactService
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb

module Chainweb.Test.PactService where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class

import Data.Aeson

import qualified Network.Wai.Handler.Warp as Warp

import Servant
import Servant.Client
import Servant.Server

import Test.Tasty
import Test.Tasty.HUnit

import Chainweb.BlockHeader
import Chainweb.Pact.Service.PactApi
import Chainweb.Pact.Service.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Types

tests :: TestTree
tests = testCase "Pact service tests" pactServiceTests

pactServiceTests :: IO ()
pactServiceTests = withPactServiceApp

withPactServiceApp :: IO () -> IO ()
withPactServiceApp action = do
    port <- generatePort
    bracket (liftIO $ forkIO $ Warp.run port pactServiceApp)
        killThread
        (const action)

generatePort :: IO Int
generatePort = getStdRandom (randomR (1024,65535))

testApp :: _
testApp =
    around_ withPactServiceApp $ do
        --let createUser = client (Proxy :: Proxy PactAPI)
        let pactClient = client (Proxy :: Proxy PactAPI)
        baseUrl <- runIO $ parseBaseUrl ("http://localhost:" ++ show port)
        manager <- runIO $ newManager defaultManagerSettings
        let clientEnv = mkClientEnv manager baseUrl

        -- result <- runClientM (createUser 50001) clientEnv
        result <- runClientM (newBlockReq testBlockHeader) clientEnv
        -- result `shouldBe` (Right $ User { name = "some user", user_id = 50001})
        result @?= testPayloadHash

getTestBlockHeader :: BlockHeader
getTestBlockHeader =
    let gbh0 = genesisBlockHeader Test peterson cid
    in last $ take 2 $ testBlockHeaders gbh0

testPayloadHash :: BlockPayloadHash
testPayloadHash = undefined
