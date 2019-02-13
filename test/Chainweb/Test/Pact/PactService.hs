{-# LANGUAGE OverloadedStrings #-}

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
import Data.Vector ((!))
import qualified Data.Vector as V
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
        let headers = V.fromList $ getBlockHeaders 4

        mapM_ putStrLn $ show . _blockHeight <$> headers

        base <- parseBaseUrl ("http://localhost:" ++ show port)
        mgr <- newManager defaultManagerSettings
        let clientEnv = mkClientEnv mgr base

        -- testing:  /new
        response0 <- runClientM (testGetNewBlock (headers ! 0)) clientEnv
        checkRespTrans "block-results-expected-0.txt" response0

        -- testing:  /validate
        response0b <- runClientM (testValidate (headers ! 0)) clientEnv
        checkRespTrans "block-results-expected-0.txt" response0b

        -- testing:  /validate
        validateResp1 <- runClientM (testValidate (headers ! 1)) clientEnv
        checkRespTrans "block-results-expected-1.txt" validateResp1

        -- testing: /valiAsync and /poll
        idResponse <- runClientM (testValidateAsync (headers ! 2)) clientEnv
        case idResponse of
            (Left servantError) -> assertFailure $
                "No requestId returned from testValidateAsync" ++ show servantError
            (Right rqid) -> do
                rspM <- pollForTestResp clientEnv rqid
                case rspM of
                    Nothing -> assertFailure "Polling timeout for testValidateAsync"
                    Just rsp -> checkRespTrans "block-results-expected-2.txt" rsp

pollForTestResp
    :: ClientEnv
    -> RequestId
    -> IO (Maybe (Either ServantError (Either String Transactions)))
pollForTestResp clientEnv reqId =
    timeout (fromIntegral timeoutSeconds) $
        runClientM (testPoll reqId) clientEnv

timeoutSeconds :: Int
timeoutSeconds = 30 -- seconds

checkRespTrans :: FilePath -> Either ServantError (Either String Transactions) -> Assertion
checkRespTrans _ (Left servantError) = assertFailure $ "Servant error: " ++ show servantError
checkRespTrans fp (Right x) =
    case x of
        Left err -> assertFailure $ "Error in pact response: "  ++ show err
        Right ts -> do
            let jsonTrans = show (toJSON ts) ++ "\n"
            -- uncomment to capture updated test results
            putStrLn $ "\n\npactTestApi - JSON results: \n\n" ++ jsonTrans ++ "\n\n"
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

getGenesisBlockHeader :: BlockHeader
getGenesisBlockHeader = do
    let testId = testChainId (1 :: Word32)
    genesisBlockHeader Test peterson testId

getBlockHeaders :: Int -> [BlockHeader]
getBlockHeaders n = do
    let testId = testChainId (1 :: Word32)
    let gbh0 = genesisBlockHeader Test peterson testId
    let after0s = take (n - 1) $ testBlockHeaders gbh0
    gbh0 : after0s
