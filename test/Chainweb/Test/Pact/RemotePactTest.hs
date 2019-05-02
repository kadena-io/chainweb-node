{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Test.RemotePactTest
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via the Http Pact interface (/send, etc.)(inprocess) API  in Chainweb
module Chainweb.Test.Pact.RemotePactTest
( tests
, withNodes
, withRequestKeys
, runGhci
) where

import Control.Concurrent hiding (putMVar, readMVar)
import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Exception
import Control.Lens
import Control.Monad

import qualified Data.Aeson as A
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Streaming.Network (HostPreference)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Network.Connection as HTTP
import Network.HTTP.Client.TLS as HTTP

import Numeric.Natural

import Prelude hiding (lookup)

import Servant.API
import Servant.Client

import System.LogLevel
import System.Time.Extra

import Test.Tasty
import Test.Tasty.HUnit

import Pact.ApiReq (mkExec)
import Pact.Parse (ParsedDecimal(..), ParsedInteger(..))
import Pact.Types.API
import qualified Pact.Types.ChainMeta as CM
import Pact.Types.Command
import qualified Pact.Types.Hash as H

-- internal modules

import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.PeerResources
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI.Client
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Pact.RestAPI
#if ! MIN_VERSION_servant(0,16,0)
import Chainweb.RestAPI.Utils
#endif
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

import P2P.Node.Configuration
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Global Settings

nNodes :: Natural
nNodes = 1

version :: ChainwebVersion
version = TimedCPM petersonChainGraph

cid :: HasCallStack => ChainId
cid = head . toList $ chainIds version

testCmds :: PactTestApiCmds
testCmds = apiCmds version cid

-- -------------------------------------------------------------------------- --
-- Tests

tests :: RocksDb -> ScheduledTest
tests rdb = testGroupSch "PactRemoteTests"
    [ withNodes rdb nNodes $ \net ->
        withRequestKeys net $ \rks ->
            testGroup "PactRemoteTests"
                [ responseGolden net rks
                , mempoolValidation net rks
                ]
    ]
    -- The outer testGroupSch wrapper is just for scheduling purposes.

-- for Stuart:
runGhci :: IO ()
runGhci = withTempRocksDb "ghci.RemotePactTests" $ defaultMain . _schTest . tests

responseGolden :: IO ChainwebNetwork -> IO RequestKeys -> TestTree
responseGolden networkIO rksIO = golden "command-0-resp" $ do
    rks <- rksIO
    cwEnv <- _getClientEnv <$> networkIO
    (PollResponses theMap) <- testPoll testCmds cwEnv rks
    let mays = map (`HM.lookup` theMap) (_rkRequestKeys rks)
    let values = _arResult <$> catMaybes mays
    return $! toS $! foldMap A.encode values

mempoolValidation :: IO ChainwebNetwork -> IO RequestKeys -> TestTree
mempoolValidation networkIO rksIO = testCase "mempoolValidationCheck" $ do
    rks <- rksIO
    cwEnv <- _getClientEnv <$> networkIO
    let mPool = toMempool version cid tConfig 10000 cwEnv :: MempoolBackend ChainwebTransaction
    testMPValidated mPool rks
  where
    tConfig = mempoolTxConfig noopMempool

-- -------------------------------------------------------------------------- --
-- Utils

withRequestKeys
    :: IO ChainwebNetwork
    -> (IO RequestKeys -> TestTree)
    -> TestTree
withRequestKeys networkIO = withResource mkKeys (\_ -> return ())
  where
    mkKeys = do
        cwEnv <- _getClientEnv <$> networkIO
        testSend testCmds cwEnv

testSend :: PactTestApiCmds -> ClientEnv -> IO RequestKeys
testSend cmds env = do
    sb <- testBatch
    result <- sendWithRetry cmds env sb
    case result of
        Left e -> assertFailure (show e)
        Right rks -> return rks

testPoll :: PactTestApiCmds -> ClientEnv -> RequestKeys -> IO PollResponses
testPoll cmds env rks = do
    response <- pollWithRetry cmds env rks
    case response of
        Left e -> assertFailure (show e)
        Right rsp -> return rsp

testMPValidated
    :: MempoolBackend ChainwebTransaction
    -> RequestKeys
    -> Assertion
testMPValidated mPool rks = do
    let txHashes = V.fromList $ TransactionHash . H.unHash . unRequestKey <$> _rkRequestKeys rks
    b <- go maxMempoolRetries mPool txHashes
    assertBool "At least one transaction was not validated" b
  where
    go :: Int -> MempoolBackend ChainwebTransaction -> Vector TransactionHash ->  IO Bool
    go 0 _ _ = return False
    go retries mp hashes = do
        results <- mempoolLookup mp hashes
        if checkValidated results then return True
        else do
            sleep 1
            go (retries - 1) mp hashes

maxMempoolRetries :: Int
maxMempoolRetries = 30

checkValidated :: Vector (LookupResult ChainwebTransaction) -> Bool
checkValidated results =
    not (null results) && V.all f results
  where
    f (Validated _) = True
    f Confirmed = True
    f _ = False

getClientEnv :: BaseUrl -> IO ClientEnv
getClientEnv url = do
    let mgrSettings = HTTP.mkManagerSettings (HTTP.TLSSettingsSimple True False False) Nothing
    mgr <- HTTP.newTlsManagerWith mgrSettings
    return $ mkClientEnv mgr url

maxSendRetries :: Int
maxSendRetries = 30

-- | To allow time for node to startup, retry a number of times
sendWithRetry :: PactTestApiCmds -> ClientEnv -> SubmitBatch -> IO (Either ClientError RequestKeys)
sendWithRetry cmds env sb = go maxSendRetries
  where
    go retries =  do
        result <- runClientM (sendApiCmd cmds sb) env
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

maxPollRetries :: Int
maxPollRetries = 30

-- | To allow time for node to startup, retry a number of times
pollWithRetry :: PactTestApiCmds -> ClientEnv -> RequestKeys -> IO (Either ClientError PollResponses)
pollWithRetry cmds env rks = do
  sleep 3
  go maxPollRetries
    where
      go retries = do
          result <- runClientM (pollApiCmd cmds (Poll (_rkRequestKeys rks))) env
          case result of
              Left _ ->
                  if retries == 0 then do
                      putStrLn $ "poll failing after " ++ show maxSendRetries ++ " retries"
                      return result
                  else do
                      sleep 1
                      go (retries - 1)
              Right _ -> do
                  putStrLn $ "poll succeeded after " ++ show (maxSendRetries - retries) ++ " retries"
                  return result

testBatch :: IO SubmitBatch
testBatch = do
    kps <- testKeyPairs
    c <- mkExec "(+ 1 2)" A.Null pm kps Nothing
    pure $ SubmitBatch [c]
  where
    pm :: CM.PublicMeta
    pm = CM.PublicMeta (CM.ChainId "0") "sender00" (ParsedInteger 100) (ParsedDecimal 0.0001)

type PactClientApi
       = (SubmitBatch -> ClientM RequestKeys)
    :<|> ((Poll -> ClientM PollResponses)
    :<|> ((ListenerRequest -> ClientM ApiResult)
    :<|> (Command Text -> ClientM (CommandSuccess A.Value))))

generatePactApi :: ChainwebVersion -> ChainId -> PactClientApi
generatePactApi cwVersion chainid =
     case someChainwebVersionVal cwVersion of
        SomeChainwebVersionT (_ :: Proxy cv) ->
          case someChainIdVal chainid of
            SomeChainIdT (_ :: Proxy cid) -> client (Proxy :: Proxy (PactApi cv cid))

apiCmds :: ChainwebVersion -> ChainId -> PactTestApiCmds
apiCmds cwVersion theChainId =
    let sendCmd :<|> pollCmd :<|> _ :<|> _ = generatePactApi cwVersion theChainId
    in PactTestApiCmds sendCmd pollCmd

data PactTestApiCmds = PactTestApiCmds
    { sendApiCmd :: SubmitBatch -> ClientM RequestKeys
    , pollApiCmd :: Poll -> ClientM PollResponses }

--------------------------------------------------------------------------------
-- test node(s), config, etc. for this test
--------------------------------------------------------------------------------

newtype ChainwebNetwork = ChainwebNetwork { _getClientEnv :: ClientEnv }

withNodes
    :: RocksDb
    -> Natural
    -> (IO ChainwebNetwork -> TestTree)
    -> TestTree
withNodes rdb n f = withResource start
    (cancel . fst)
    (f . fmap (ChainwebNetwork . snd))
  where
    start = do
        peerInfoVar <- newEmptyMVar
        a <- async $ runTestNodes rdb Warn version n peerInfoVar
        i <- readMVar peerInfoVar
        cwEnv <- getClientEnv $ getCwBaseUrl $ _hostAddressPort $ _peerAddr i
        return (a, cwEnv)

    getCwBaseUrl :: Port -> BaseUrl
    getCwBaseUrl p = BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = fromIntegral p
        , baseUrlPath = ""
        }

runTestNodes
    :: RocksDb
    -> LogLevel
    -> ChainwebVersion
    -> Natural
    -> MVar PeerInfo
    -> IO ()
runTestNodes rdb loglevel v n portMVar =
    forConcurrently_ [0 .. int n - 1] $ \i -> do
        threadDelay (1000 * int i)
        let baseConf = config v n (NodeId i)
        conf <- if
            | i == 0 ->
                return $ bootstrapConfig baseConf
            | otherwise ->
                setBootstrapPeerInfo <$> readMVar portMVar <*> pure baseConf
        node rdb loglevel portMVar conf

node :: RocksDb -> LogLevel -> MVar PeerInfo -> ChainwebConfiguration -> IO ()
node rdb loglevel peerInfoVar conf = do
    rocksDb <- testRocksDb ("remotePactTest-" <> encodeUtf8 (toText nid)) rdb
    withChainweb conf logger rocksDb $ \cw -> do

        -- If this is the bootstrap node we extract the port number and publish via an MVar.
        when (nid == NodeId 0) $ do
            let bootStrapInfo = view (chainwebPeer . peerResPeer . peerInfo) cw
            putMVar peerInfoVar bootStrapInfo

        runChainweb cw `finally` do
            logFunctionText logger Info "write sample data"
            logFunctionText logger Info "shutdown node"
        return ()
  where
    nid = _configNodeId conf
    logger :: GenericLogger
    logger = addLabel ("node", toText nid) $ genericLogger loglevel print

host :: Hostname
host = unsafeHostnameFromText "::1"

interface :: HostPreference
interface = "::1"

config
    :: ChainwebVersion
    -> Natural
    -> NodeId
    -> ChainwebConfiguration
config v n nid = defaultChainwebConfiguration v
    & set configNodeId nid
    & set (configP2p . p2pConfigPeer . peerConfigHost) host
    & set (configP2p . p2pConfigPeer . peerConfigInterface) interface
    & set (configP2p . p2pConfigKnownPeers) mempty
    & set (configP2p . p2pConfigIgnoreBootstrapNodes) True
    & set (configP2p . p2pConfigMaxPeerCount) (n * 2)
    & set (configP2p . p2pConfigMaxSessionCount) 4
    & set (configP2p . p2pConfigSessionTimeout) 60
    & set (configMiner . enableConfigConfig . configTestMiners) (MinerCount n)
    & set (configTransactionIndex . enableConfigEnabled) True

bootstrapConfig :: ChainwebConfiguration -> ChainwebConfiguration
bootstrapConfig conf = conf
    & set (configP2p . p2pConfigPeer) peerConfig
    & set (configP2p . p2pConfigKnownPeers) []
  where
    peerConfig = head (bootstrapPeerConfig $ _configChainwebVersion conf)
        & set peerConfigPort 0
        & set peerConfigHost host

setBootstrapPeerInfo :: PeerInfo -> ChainwebConfiguration -> ChainwebConfiguration
setBootstrapPeerInfo =
    over (configP2p . p2pConfigKnownPeers) . (:)
