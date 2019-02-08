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

module Chainweb.Test.Pact.PactService where

import Data.Aeson
import Data.Word (Word32)

import Network.HTTP.Client (newManager, defaultManagerSettings)

import Servant
import Servant.Client
import Servant.Client.Internal.HttpClient (ClientM(..))

import System.IO.Extra
import System.Random

import Test.Tasty
import Test.Tasty.HUnit

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Pact.Service.PactApi
import Chainweb.Pact.Service.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Pact.Types
import Chainweb.Test.Utils
import Chainweb.Version

tests :: TestTree
tests = testCase "Pact service tests" pactTestApp

pactTestApp :: IO ()
pactTestApp = do
    port <- generatePort
    withPactServiceApp port testMemPoolAccess $ do
        baseUrl <- parseBaseUrl ("http://localhost:" ++ show port)
        putStrLn $ "pactTestApp - baseUrl: " ++ show baseUrl
        manager <- newManager defaultManagerSettings
        let clientEnv = mkClientEnv manager baseUrl
        result <- runClientM (testGetNewBlock getTestBlockHeader) clientEnv
        -- let h = result `asTypeOf` _ -- :: Either ServantError (Either String Transactions)
        case result of
          Left servantError -> do
            -- putStrLn $ show servantError
            assertFailure $ "Servant error: " ++ show servantError
          Right x -> case x of
            Left err -> do
              -- putStrLn $ show err
              assertFailure $ "Error in pact response: "  ++ show err
            Right ts -> do
                let jsonTrans = show (toJSON ts) ++ "\n"
                putStrLn $ "pactTestApi - JSON results: \n\n" ++ jsonTrans
                putStrLn "\n\n"

                expectedPayload <- readFile' $ testPactFilesDir ++ "block-results-expected.txt"

                jsonTrans @?= expectedPayload

generatePort :: IO Int
generatePort = getStdRandom (randomR (1024,65535))

testGetNewBlock :: BlockHeader -> ClientM (Either String Transactions)
testGetNewBlockAsync :: BlockHeader -> ClientM RequestId
testValidate :: BlockHeader -> ClientM (Either String Transactions)
testValidateAsync :: BlockHeader -> ClientM RequestId
testPoll :: RequestId -> ClientM (Either String Transactions)

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
