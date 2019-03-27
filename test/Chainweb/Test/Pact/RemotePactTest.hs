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

import Control.Concurrent.MVar.Strict
import Control.Exception
import Control.Lens

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Proxy
import Data.Streaming.Network (HostPreference)
import Data.String.Conv (toS)
import Data.Text (Text)

import Network.HTTP.Client.TLS

import Numeric.Natural

import Servant.Client

import System.LogLevel
import System.Time.Extra

import Test.Tasty.HUnit
import Test.Tasty
-- import Test.Tasty.Golden

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

tests :: IO TestTree
tests = do
    peerInfoVar <- newEmptyMVar
    runTestNode Warn (TestWithTime petersonChainGraph) Nothing peerInfoVar
    newPeerInfo <- readMVar peerInfoVar
    let thePort = _hostAddressPort (_peerAddr newPeerInfo)

    putStrLn $ "Server listening on port: " ++ show thePort
    putStrLn "Sleeping..."
    sleep 10
    putStrLn "Done sleeping, sending client request"

    tt0 <- clientTest thePort
    return $ testGroup "PactRemoteTest" [tt0]

clientTest :: Port -> IO TestTree
clientTest thePort = do
    mgr <- newTlsManager
    let env = mkClientEnv mgr (testUrl thePort)
    putStrLn $ "URL: " ++ show (testUrl thePort)
    let msb = A.decode $ toS escapedCmd :: Maybe SubmitBatch
    case msb of
        Nothing -> return $ testCase "tbd" (assertFailure "decoding command string failed")
        Just sb -> do
            putStrLn $ "About to call runCliemtM w/send -- sb: " ++ show sb
            result <- runClientM (send sb) env
            case result of
                Left e -> assertFailure (show e)
                Right (RequestKeys _rks) -> return $ testCase "TBD" (assertBool "TBD" True)

testUrl :: Port -> BaseUrl
testUrl thePort = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "127.0.0.1"
    , baseUrlPort = fromIntegral thePort
    , baseUrlPath = "pact" }

escapedCmd :: BS.ByteString
escapedCmd = [r|{"cmds":[{"hash":"0e89ee947053a74ce99a0cdb42f2028427c0b387a7913194e5e0960bbcb1f48a4df1fa23fff6c87de681eff79ce746c47db68f16bad175ad8b193c7845838ebc","sigs":[],"cmd":"{\"payload\":{\"exec\":{\"data\":null,\"code\":\"(+ 1 2)\"}},\"meta\":{\"gasLimit\":1,\"chainId\":\"8\",\"gasPrice\":1,\"sender\":\"sender00\",\"fee\":0},\"nonce\":\"\\\"2019-03-25 02:16:13.831007 UTC\\\"\"}"}]}|]

----------------------------------------------------------------------------------------------------
-- test node config, etc. for this test
----------------------------------------------------------------------------------------------------
runTestNode
    :: LogLevel
    -> ChainwebVersion
    -> Maybe FilePath
    -> MVar PeerInfo
    -> IO ()
runTestNode loglevel v chainDbDir portMVar = do
    let baseConf = config v 1 (NodeId 0) chainDbDir
    let conf = bootstrapConfig baseConf
    node loglevel portMVar conf

node
    :: LogLevel
    -> MVar PeerInfo
    -> ChainwebConfiguration
    -> IO ()
node loglevel peerInfoVar conf = do
    pdb <- emptyInMemoryPayloadDb
    withChainweb conf logger pdb $ \cw -> do

        -- If this is the bootstrap node we extract the port number and publish via an MVar.
        if (nid == NodeId 0)
            then do
                let bootStrapInfo = view (chainwebPeer . peerResPeer . peerInfo) cw
                putStrLn $ "bootstrap info from config: " ++ show bootStrapInfo
                putMVar peerInfoVar bootStrapInfo
            else error "RemotePactTest.node -- nodeId 0 expected"

        putStrLn "about to run cw..."
        runChainweb cw `finally` do
            logFunctionText logger Info "write sample data"
            logFunctionText logger Info "shutdown node"
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
        -- ^ number of nodes
    -> NodeId
        -- ^ NodeId
    -> (Maybe FilePath)
        -- ^ directory where the chaindbs are persisted
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
        -- Normally, the port of bootstrap nodes is hard-coded. But in test-suites that may run
        -- concurrently we want to use a port that is assigned by the OS.

        & set peerConfigHost host

setBootstrapPeerInfo
    :: PeerInfo
        -- ^ Peer info of bootstrap node
    -> ChainwebConfiguration
    -> ChainwebConfiguration
setBootstrapPeerInfo =
    over (configP2p . p2pConfigKnownPeers) . (:)
