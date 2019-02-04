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

import Data.Word (Word32)

import qualified Network.Wai.Handler.Warp as Warp

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

withPactServiceApp :: (Int -> IO ()) -> IO ()
withPactServiceApp action = do
    port <- generatePort
    bracket (liftIO $ forkIO $ Warp.run port pactServiceApp)
        killThread
        (const (action port))

generatePort :: IO Int
generatePort = getStdRandom (randomR (1024,65535))


testGetNewBlock :: BlockHeader -> ClientM (Either String BlockPayloadHash)
-- testGetNewBlock :: BlockHeader -> Either String BlockPayloadHash
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

pactTestApp :: IO ()
pactTestApp =
    withPactServiceApp $ (\port -> do
        baseUrl <- parseBaseUrl ("http://localhost:" ++ show port)
        manager <- newManager defaultManagerSettings
        let clientEnv = mkClientEnv manager baseUrl
        result <- runClientM (testGetNewBlock getTestBlockHeader) clientEnv
        expected <- testPayloadHash
        result @?= expected
    )

getTestBlockHeader :: BlockHeader
getTestBlockHeader = do
    let testId = testChainId $ (1 :: Word32)
    let gbh0 = genesisBlockHeader Test peterson testId
    last $ take 2 $ testBlockHeaders gbh0

testPayloadHash :: IO (Either ServantError (Either String BlockPayloadHash))
testPayloadHash = do
    bhb <- randomBlockHashBytes
    return $ Right $ Right $ BlockPayloadHash bhb

-- computeBlockHash :: BlockHeader -> BlockHash
-- newtype BlockPayloadHash = BlockPayloadHash BlockHashBytes
-- blockHashBytes :: MonadThrow m => B.ByteString -> m BlockHashBytes
-- randomBlockHashBytes :: MonadIO m => m BlockHashBytes
