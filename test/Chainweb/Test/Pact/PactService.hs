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
import System.Time.Extra

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
        manager <- newManager defaultManagerSettings
        let clientEnv = mkClientEnv manager baseUrl

        -- testing:  /new
        response <- runClientM (testGetNewBlock getTestBlockHeader) clientEnv
        checkRespTrans "block-results-expected.txt" response

        -- testing: /newAsync
        idResponse <- runClientM (testGetNewBlockAsync getTestBlockHeader) clientEnv
        case idResponse of
            (Left servantError) -> assertFailure $
                "No requestId returned from testGetNewBlockAsync" ++ show servantError
            (Right rqid) -> do
                rspM <- pollForTestResp clientEnv rqid
                case rspM of
                    Nothing -> assertFailure "Polling timeout for testGetNewBlockAsync"
                    Just rsp -> checkRespTrans "block-results-expected.txt" rsp

pollForTestResp
    :: ClientEnv
    -> RequestId
    -> IO (Maybe (Either ServantError (Either String Transactions)))
pollForTestResp clientEnv reqId = do
    timeout (fromIntegral timeoutSeconds) $ do
        runClientM (testPoll reqId) clientEnv

timeoutSeconds :: Int
timeoutSeconds = 30 -- seconds

checkRespTrans :: FilePath -> Either ServantError (Either String Transactions) -> Assertion
checkRespTrans _ (Left servantError) = assertFailure $ "Servant error: " ++ show servantError
checkRespTrans fp (Right x) =
    case x of
        Left err -> do
            assertFailure $ "Error in pact response: "  ++ show err
        Right ts -> do
            let jsonTrans = show (toJSON ts) ++ "\n"
            -- uncomment to capture updated test results
            -- putStrLn $ "\n\npactTestApi - JSON results: \n\n" ++ jsonTrans ++ "\n\n"
            expectedPayload <- readFile' $ testPactFilesDir ++ fp
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
