{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Concurrent hiding (readMVar, putMVar)
import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Exception
import Control.Lens
import Control.Monad

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Proxy
import Data.Streaming.Network (HostPreference)
import Data.String.Conv (toS)
import Data.Text (Text)

import Network.HTTP.Client.TLS as HTTP
import Network.Connection as HTTP

import Numeric.Natural

import Servant.Client
import System.FilePath
import System.LogLevel
import System.Time.Extra

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.Golden

import Text.RawString.QQ(r)

import Pact.Types.API
import Pact.Types.Command

-- internal modules

import Chainweb.Chainweb
import Chainweb.Chainweb.PeerResources
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Pact.RestAPI
import Chainweb.Payload.PayloadStore (emptyInMemoryPayloadDb)
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Pact.Utils
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration
import P2P.Peer

send :: SubmitBatch -> ClientM RequestKeys
send   = client (Proxy :: Proxy SendApi)

poll :: Poll -> ClientM PollResponses
poll   = client (Proxy :: Proxy PollApi)

listen :: ListenerRequest -> ClientM ApiResult
listen = client (Proxy :: Proxy ListenApi)

local :: Command Text -> ClientM (CommandSuccess A.Value)
local  = client (Proxy :: Proxy LocalApi)

nNodes :: Natural
nNodes = 1

tests :: IO TestTree
tests = do
    peerInfoVar <- newEmptyMVar
    let cwVersion = TestWithTime petersonChainGraph
    theAsync <- async $ runTestNodes Warn cwVersion nNodes Nothing peerInfoVar
    link theAsync
    newPeerInfo <- readMVar peerInfoVar
    let thePort = _hostAddressPort (_peerAddr newPeerInfo)

    putStrLn $ "Server listening on port: " ++ show thePort ++ ". Sending client request..."

    tt0 <- clientTest thePort cwVersion
    return $ testGroup "PactRemoteTest" [tt0]

clientTest :: Port -> ChainwebVersion -> IO TestTree
clientTest thePort cwVersion = do
    let mgrSettings = HTTP.mkManagerSettings (HTTP.TLSSettingsSimple True False False) Nothing
    mgr <- HTTP.newTlsManagerWith mgrSettings
    let url = testUrl thePort cwVersion "0.0" 8
    -- putStrLn $ "URL: " ++ showBaseUrl url
    let env = mkClientEnv mgr url
    let msb = A.decode $ toS escapedCmd :: Maybe SubmitBatch
    tt0 <- case msb of
          Nothing -> assertFailure "decoding command string failed"
          Just sb -> do
              result <- sendWithRetry sb env
              case result of
                  Left e -> assertFailure (show e)
                  Right rks -> checkRequestKeys "command-expected-0" rks
    return tt0

maxSendRetries :: Int
maxSendRetries = 30

-- | To allow time for node to startup, retry a number of times
sendWithRetry :: SubmitBatch -> ClientEnv -> IO (Either ServantError RequestKeys)
sendWithRetry sb env = go maxSendRetries
  where
    go retries =  do
        result <- runClientM (send sb) env
        case result of
            Left _ ->
                if retries == 0 then do
                    putStrLn $ "send failing after " ++ show maxSendRetries ++ " retries"
                    return result
                else do
                    sleep 1
                    go (retries - 1)
            Right _ -> do
                putStrLn $ "send succeeded after " ++ show (maxSendRetries - retries) ++ " retries"
                return result

checkRequestKeys :: FilePath -> RequestKeys -> IO TestTree
checkRequestKeys filePrefix rks = do
    let fp = filePrefix ++ "-rks.txt"
    let bsRks = return $ foldMap (toS . show ) (_rkRequestKeys rks)
    return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) bsRks

testUrl :: Port -> ChainwebVersion -> String -> Int -> BaseUrl
testUrl thePort v release chainNum = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "127.0.0.1"
    , baseUrlPort = fromIntegral thePort
    , baseUrlPath = "chainweb/"
                  ++ release ++ "/"
                  ++ toS (chainwebVersionToText v) ++ "/"
                  ++ "chain/"
                  ++ show chainNum ++ "/"
                  ++ "pact" }

escapedCmd :: BS.ByteString
escapedCmd = [r|{"cmds":[{"hash":"0e89ee947053a74ce99a0cdb42f2028427c0b387a7913194e5e0960bbcb1f48a4df1fa23fff6c87de681eff79ce746c47db68f16bad175ad8b193c7845838ebc","sigs":[],"cmd":"{\"payload\":{\"exec\":{\"data\":null,\"code\":\"(+ 1 2)\"}},\"meta\":{\"gasLimit\":1,\"chainId\":\"8\",\"gasPrice\":1,\"sender\":\"sender00\",\"fee\":0},\"nonce\":\"\\\"2019-03-25 02:16:13.831007 UTC\\\"\"}"}]}|]

----------------------------------------------------------------------------------------------------
-- test node(s), config, etc. for this test
----------------------------------------------------------------------------------------------------
runTestNodes
    :: LogLevel
    -> ChainwebVersion
    -> Natural
    -> (Maybe FilePath)
    -> MVar PeerInfo
    -> IO ()
runTestNodes loglevel v n chainDbDir portMVar = do
    forConcurrently_ [0 .. int n - 1] $ \i -> do
        threadDelay (500000 * int i)
        let baseConf = config v n (NodeId i) chainDbDir
        conf <- if
            | i == 0 ->
                return $ bootstrapConfig baseConf
            | otherwise ->
                setBootstrapPeerInfo <$> readMVar portMVar <*> pure baseConf
        node loglevel portMVar conf

node :: LogLevel -> MVar PeerInfo -> ChainwebConfiguration -> IO ()
node loglevel peerInfoVar conf = do
    pdb <- emptyInMemoryPayloadDb
    withChainweb conf logger pdb $ \cw -> do

        -- If this is the bootstrap node we extract the port number and publish via an MVar.
        when (nid == NodeId 0) $ do
            let bootStrapInfo = view (chainwebPeer . peerResPeer . peerInfo) cw
            putMVar peerInfoVar bootStrapInfo

        runChainweb cw `finally` do
            _ <- error "Une"
            logFunctionText logger Info "write sample data"
            logFunctionText logger Info "shutdown node"
        _ <- error "DUEY"
        return ()
  where
    nid = _configNodeId conf
    logger :: GenericLogger
    logger = addLabel ("node", toText nid) $ genericLogger loglevel (\t -> putStrLn (show t))

host :: Hostname
host = unsafeHostnameFromText "::1"

interface :: HostPreference
interface = "::1"

config
    :: ChainwebVersion
    -> Natural
    -> NodeId
    -> (Maybe FilePath)
    -> ChainwebConfiguration
config v n nid chainDbDir = defaultChainwebConfiguration v
    & set configNodeId nid
    & set (configP2p . p2pConfigPeer . peerConfigHost) host
    & set (configP2p . p2pConfigPeer . peerConfigInterface) interface
    & set (configP2p . p2pConfigKnownPeers) mempty
    & set (configP2p . p2pConfigIgnoreBootstrapNodes) True
    & set (configP2p . p2pConfigMaxPeerCount) (n * 2)
    & set (configP2p . p2pConfigMaxSessionCount) 4
    & set (configP2p . p2pConfigSessionTimeout) 60
    & set configChainDbDirPath chainDbDir
    & set (configMiner . enableConfigConfig . configTestMiners) (MinerCount n)
    & set (configTransactionIndex . enableConfigEnabled) True

bootstrapConfig
    :: ChainwebConfiguration
    -> ChainwebConfiguration
bootstrapConfig conf = conf
    & set (configP2p . p2pConfigPeer) peerConfig
    & set (configP2p . p2pConfigKnownPeers) []
  where
    peerConfig = (head $ bootstrapPeerConfig $ _configChainwebVersion conf)
        & set peerConfigPort 0
        & set peerConfigHost host

setBootstrapPeerInfo
    :: PeerInfo
    -> ChainwebConfiguration
    -> ChainwebConfiguration
setBootstrapPeerInfo =
    over (configP2p . p2pConfigKnownPeers) . (:)
