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

import Data.Word (Word32)

import Network.HTTP.Client (newManager, defaultManagerSettings)

import Servant
import Servant.Client
import Servant.Client.Internal.HttpClient (ClientM(..))

import System.Random

import Test.Tasty
import Test.Tasty.HUnit

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Pact.Service.PactApi
import Chainweb.Pact.Service.Types
import Chainweb.Test.Utils
import Chainweb.Version

tests :: TestTree
tests = testCase "Pact service tests" pactTestApp

pactTestApp :: IO ()
pactTestApp = do
    port <- generatePort
    withPactServiceApp port $ do
        baseUrl <- parseBaseUrl ("http://localhost:" ++ show port)
        putStrLn $ "pactTestApp - baseUrl: " ++ show baseUrl
        manager <- newManager defaultManagerSettings
        let clientEnv = mkClientEnv manager baseUrl
        result <- runClientM (testGetNewBlock getTestBlockHeader) clientEnv
        putStrLn $ "pactTestApp - result: " ++ show result
        expected <- testPayloadHash
        result @?= expected

generatePort :: IO Int
generatePort = getStdRandom (randomR (1024,65535))

testGetNewBlock :: BlockHeader -> ClientM (Either String BlockPayloadHash)
testGetNewBlockAsync :: BlockHeader -> ClientM RequestId
testValidate :: BlockHeader -> ClientM (Either String BlockPayloadHash)
testValidateAsync :: BlockHeader -> ClientM RequestId
testPoll :: RequestId -> ClientM (Either String BlockPayloadHash)

testGetNewBlock
    :<|> testGetNewBlockAsync
    :<|> testValidate
    :<|> testValidateAsync
    :<|> testPoll
       = client (Proxy :: Proxy PactAPI)

getTestBlockHeader :: BlockHeader
getTestBlockHeader = do
    let testId = testChainId $ (1 :: Word32)
    let gbh0 = genesisBlockHeader Test peterson testId
    last $ take 2 $ testBlockHeaders gbh0

testPayloadHash :: IO (Either ServantError (Either String BlockPayloadHash))
testPayloadHash = do
    bhb <- randomBlockHashBytes
    return $ Right $ Right $ BlockPayloadHash bhb
