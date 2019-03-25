{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module: Chainweb.Test.RemotePactTest
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via the Http Pact interface (/send, etc.)(inprocess) API  in Chainweb
module Chainweb.Test.Pact.RemotePactTest where

import qualified Data.Aeson as A
import Data.Proxy
import Data.Text (Text)

import Network.HTTP.Client hiding (Proxy)

import Servant.Client
import System.LogLevel

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.Golden

import Text.RawString.QQ(r)

import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Util

import Chainweb.Logger
import Chainweb.Pact.RestAPI
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Test.Pact.Utils

send :: SubmitBatch -> ClientM RequestKeys
send   = client (Proxy :: Proxy SendApi)

poll :: Poll -> ClientM PollResponses
poll   = client (Proxy :: Proxy PollApi)

listen :: ListenerRequest -> ClientM ApiResult
listen = client (Proxy :: Proxy ListenApi)

local :: Command Text -> ClientM (CommandSuccess A.Value)
local  = client (Proxy :: Proxy LocalApi)

tests :: IO TestTree
tests = do
    tt0 <- pactRemoteTest
    return $ testGroup "PactRemoteTest" [tt0]

pactRemoteTest :: IO TestTree
pactRemoteTest = do
    let settings = defaultManagerSettings
    mgr <- newManager settings
    -- TODO: how do I determine the port?
    let env = mkClientEnv mgr (testUrl 88)

    let j = A.toJSON escaped
    let e = (fromJSON' j :: Either String SubmitBatch)
    case e of
     Left err -> return $ testCase "tbd" (assertFailure err)
     Right sb -> do
         result <- runClientM (send sb) env
         case result of
             Left e -> assertFailure (show e)
             Right (RequestKeys rks) -> return $ testCase "TBD" (assertBool "TBD" True)

testUrl :: Int -> BaseUrl
testUrl port = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "127.0.0.1"
    , baseUrlPort = port
    , baseUrlPath = "pact" }

escaped = [r|{"cmds":[{"hash":"0e89ee947053a74ce99a0cdb42f2028427c0b387a7913194e5e0960bbcb1f48a4df1fa23fff6c87de681eff79ce746c47db68f16bad175ad8b193c7845838ebc","sigs":[],"cmd":"{\"payload\":{\"exec\":{\"data\":null,\"code\":\"(+ 1 2)\"}},\"meta\":{\"gasLimit\":1,\"chainId\":\"8\",\"gasPrice\":1,\"sender\":\"sender00\",\"fee\":0},\"nonce\":\"\\\"2019-03-25 02:16:13.831007 UTC\\\"\"}"}]}|]
