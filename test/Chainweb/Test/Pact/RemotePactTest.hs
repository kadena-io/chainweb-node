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

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens (over, set, view)
import Control.Monad

import Data.Aeson ((.=))
import Data.Foldable
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Proxy
import Data.Streaming.Network (HostPreference)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import GHC.Generics

import Network.HTTP.Client hiding (host, Proxy)

import Numeric.Natural

import Safe

import Servant.Client

import qualified Streaming.Prelude as S

import System.Environment
import System.LogLevel
import System.Timeout

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.Golden

import Text.RawString.QQ(r)

import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Util


-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Chainweb
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Difficulty (BlockRate(..), blockRate)
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Pact.RestAPI
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload.PayloadStore (emptyInMemoryPayloadDb)
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Utils
import Chainweb.Time (Seconds)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

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
    runTestNode Warn (TestWithTime petersonChainGraph) 10 Nothing peerInfoVar
    newPeerInfo <- readMVar peerInfoVar

    let thePort = _hostAddressPort (_peerAddr newPeerInfo)

    tt0 <- pactRemoteTest thePort
    return $ testGroup "PactRemoteTest" [tt0]

pactRemoteTest :: Port -> IO TestTree
pactRemoteTest thePort = do
    let settings = defaultManagerSettings
    mgr <- newManager settings
    let env = mkClientEnv mgr (testUrl thePort)
    let msb = A.decode escapedCmd :: Maybe SubmitBatch
    case msb of
        Nothing -> return $ testCase "tbd" (assertFailure "decoding command string failed")
        Just sb -> do
            result <- runClientM (send sb) env
            case result of
                Left e -> assertFailure (show e)
                Right (RequestKeys rks) -> return $ testCase "TBD" (assertBool "TBD" True)

testUrl :: Port -> BaseUrl
testUrl thePort = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "127.0.0.1"
    , baseUrlPort = fromIntegral thePort
    , baseUrlPath = "pact" }

escapedCmd = [r|{"cmds":[{"hash":"0e89ee947053a74ce99a0cdb42f2028427c0b387a7913194e5e0960bbcb1f48a4df1fa23fff6c87de681eff79ce746c47db68f16bad175ad8b193c7845838ebc","sigs":[],"cmd":"{\"payload\":{\"exec\":{\"data\":null,\"code\":\"(+ 1 2)\"}},\"meta\":{\"gasLimit\":1,\"chainId\":\"8\",\"gasPrice\":1,\"sender\":\"sender00\",\"fee\":0},\"nonce\":\"\\\"2019-03-25 02:16:13.831007 UTC\\\"\"}"}]}|]

----------------------------------------------------------------------------------------------------
-- test node config, etc. for this test
----------------------------------------------------------------------------------------------------
runTestNode
    :: LogLevel
    -> ChainwebVersion
    -> Seconds
    -> Maybe FilePath
    -> MVar PeerInfo
    -> IO ()
runTestNode loglevel v seconds chainDbDir portMVar = do
    void $ timeout (int seconds * 1000000)
        $ return $ runNode loglevel v chainDbDir portMVar

runNode
    :: LogLevel
    -> ChainwebVersion
    -> (Maybe FilePath)
    -> MVar PeerInfo
    -> IO ()
runNode loglevel v chainDbDir bootstrapPortVar = do
    -- setEnv "CHAINWEB_DISABLE_PACT" "0"
    let baseConf = config v 1 (NodeId 0) chainDbDir
    let conf = bootstrapConfig baseConf

    node loglevel bootstrapPortVar conf


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
        -- Only listen on the loopback device. On Mac OS X this prevents the
        -- firewall dialog form poping up.

    & set (configP2p . p2pConfigKnownPeers) mempty
    & set (configP2p . p2pConfigIgnoreBootstrapNodes) True
        -- The bootstrap peer info is set later after the bootstrap nodes
        -- has started and got its port assigned.

    & set (configP2p . p2pConfigMaxPeerCount) (n * 2)
        -- We make room for all test peers in peer db.

    & set (configP2p . p2pConfigMaxSessionCount) 4
        -- We set this to a low number in order to keep the network sparse (or
        -- at last no being a clique) and to also limit the number of port allocations

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

node
    :: LogLevel
    -> MVar PeerInfo
    -> ChainwebConfiguration
    -> IO ()
node loglevel bootstrapPeerInfoVar conf = do
    pdb <- emptyInMemoryPayloadDb
    withChainweb conf logger pdb $ \cw -> do

        -- If this is the bootstrap node we extract the port number and publish via an MVar.
        when (nid == NodeId 0) $ putMVar bootstrapPeerInfoVar
            $ view (chainwebPeer . peerResPeer . peerInfo) cw

        runChainweb cw `finally` do
            logFunctionText logger Info "write sample data"
            logFunctionText logger Info "shutdown node"
  where
    nid = _configNodeId conf
    logger :: GenericLogger
    logger = addLabel ("node", toText nid) $ genericLogger loglevel (\t -> putStrLn (show t))


expectedBlockCount :: ChainwebVersion -> Seconds -> Natural
expectedBlockCount v seconds = round ebc
  where
    ebc :: Double
    ebc = int seconds * int (order $ _chainGraph v) / int br

    br :: Natural
    br = case blockRate v of
        Just (BlockRate n) -> int n
        Nothing -> error $ "expectedBlockCount: ChainwebVersion with no BlockRate given: " <> show v
