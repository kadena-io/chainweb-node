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
module Chainweb.Test.Pact.RemotePactTest where

import Control.Concurrent hiding (putMVar, readMVar)
import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Exception
import Control.Lens
import Control.Monad

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Streaming.Network (HostPreference)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Network.Connection as HTTP
import Network.HTTP.Client.TLS as HTTP

import Numeric.Natural

import Prelude hiding (lookup)

import Servant.API
import Servant.Client

import System.FilePath
import System.LogLevel
import System.Time.Extra

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Text.RawString.QQ (r)

import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Util

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
import Chainweb.Payload.PayloadStore (emptyInMemoryPayloadDb)
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Pact.Utils
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration
import P2P.Peer

nNodes :: Natural
nNodes = 1

version :: ChainwebVersion
version = TimedConsensus petersonChainGraph

cid :: ChainId
cid = either (error . sshow) id $ mkChainId version (0 :: Int)

tests :: IO TestTree
tests = do
    peerInfoVar <- newEmptyMVar
    theAsync <- async $ runTestNodes Warn version nNodes Nothing peerInfoVar
    link theAsync
    newPeerInfo <- readMVar peerInfoVar
    let thePort = _hostAddressPort (_peerAddr newPeerInfo)

    let cmds = apiCmds version cid
    let cwBaseUrl = getCwBaseUrl thePort
    cwEnv <- getClientEnv cwBaseUrl
    (tt0, rks) <- testSend cmds cwEnv

    tt1 <- testPoll cmds cwEnv rks
    let tConfig = mempoolTxConfig noopMempool
    let mPool = toMempool version cid tConfig 10000 cwEnv :: MempoolBackend ChainwebTransaction
    tt2 <- testMPValidated mPool rks

    return $ testGroup "PactRemoteTest" $ tt0 : (tt1 : [tt2])

testSend :: PactTestApiCmds -> ClientEnv -> IO (TestTree, RequestKeys)
testSend cmds env = do
    let msb = decodeStrictOrThrow $ toS escapedCmd
    case msb of
        Nothing -> assertFailure "decoding command string failed"
        Just sb -> do
            result <- sendWithRetry cmds env sb
            case result of
                Left e -> assertFailure (show e)
                Right rks -> do
                    tt0 <- checkRequestKeys "command-0" rks
                    return (tt0, rks)

testPoll :: PactTestApiCmds -> ClientEnv -> RequestKeys -> IO TestTree
testPoll cmds env rks = do
    response <- pollWithRetry cmds env rks
    case response of
        Left e -> assertFailure (show e)
        Right rsp -> checkResponse "command-0" rks rsp

testMPValidated
    :: MempoolBackend ChainwebTransaction
    -> RequestKeys
    -> IO TestTree
testMPValidated mPool rks = do
    let txHashes = V.fromList $ TransactionHash . unHash . unRequestKey <$> _rkRequestKeys rks
    responses <- mempoolLookup mPool txHashes
    checkValidated responses

checkValidated :: Vector (LookupResult ChainwebTransaction) -> IO TestTree
checkValidated results = do
    when (null results)
        $ assertFailure "No results returned from mempool's lookupTransaction"
    return $ testCase "allTransactionsValidated" $
        assertBool "At least one transaction was not validated" $ V.all f results
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
sendWithRetry :: PactTestApiCmds -> ClientEnv -> SubmitBatch -> IO (Either ServantError RequestKeys)
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
pollWithRetry :: PactTestApiCmds -> ClientEnv -> RequestKeys -> IO (Either ServantError PollResponses)
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

maxMemPoolRetries :: Int
maxMemPoolRetries = 30

checkRequestKeys :: FilePath -> RequestKeys -> IO TestTree
checkRequestKeys filePrefix rks = do
    let fp = filePrefix ++ "-expected-rks.txt"
    let bsRks = return $ foldMap (toS . show ) (_rkRequestKeys rks)
    return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) bsRks

checkResponse :: FilePath -> RequestKeys -> PollResponses -> IO TestTree
checkResponse filePrefix rks (PollResponses theMap) = do
    let fp = filePrefix ++ "-expected-resp.txt"
    let mays = map (`HM.lookup` theMap) (_rkRequestKeys rks)
    let values = _arResult <$> catMaybes mays
    let bsResponse = return $ toS $ foldMap A.encode values

    return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) bsResponse

getCwBaseUrl :: Port -> BaseUrl
getCwBaseUrl thePort = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "127.0.0.1"
    , baseUrlPort = fromIntegral thePort
    , baseUrlPath = "" }

escapedCmd :: BS.ByteString
escapedCmd = [r|{"cmds":[{"hash":"d0613e7a16bf938f45b97aa831b0cc04da485140bec11cc8954e0509ea65d823472b1e683fa2950da1766cbe7fae9de8ed416e80b0ccbf12bfa6549eab89aeb6","sigs":[{"addr":"368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca","sig":"71cdedd5b1305881b1fd3d4ac2009cb247d0ebb55d1d122a7f92586828a1ed079e6afc9e8b3f75fa25fba84398eeea6cc3b92949a315420431584ba372605d07","scheme":"ED25519","pubKey":"368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"}],"cmd":"{\"payload\":{\"exec\":{\"data\":null,\"code\":\"(+ 1 2)\"}},\"meta\":{\"gasLimit\":100,\"chainId\":\"0\",\"gasPrice\":1.0e-4,\"sender\":\"sender00\"},\"nonce\":\"2019-03-29 20:35:45.012384811 UTC\"}"}]}|]

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

----------------------------------------------------------------------------------------------------
-- test node(s), config, etc. for this test
----------------------------------------------------------------------------------------------------
runTestNodes
    :: LogLevel
    -> ChainwebVersion
    -> Natural
    -> Maybe FilePath
    -> MVar PeerInfo
    -> IO ()
runTestNodes loglevel v n chainDbDir portMVar =
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
    -> Maybe FilePath
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

-- for Stuart:
runGhci :: IO ()
runGhci = tests >>= defaultMain
