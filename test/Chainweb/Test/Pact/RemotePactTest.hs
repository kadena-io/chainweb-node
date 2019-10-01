{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module: Chainweb.Test.RemotePactTest
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via the Http Pact interface (/send,
-- etc.) (inprocess) API in Chainweb
module Chainweb.Test.Pact.RemotePactTest
( tests
, withNodes
, withRequestKeys
) where

import Control.Concurrent hiding (newMVar, putMVar, readMVar, modifyMVar)
import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import Data.Default (def)
import Data.Either
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Proxy
import Data.Streaming.Network (HostPreference)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import NeatInterpolation

import Network.Connection as HTTP
import Network.HTTP.Client.TLS as HTTP

import Numeric.Natural

import Prelude hiding (lookup)

import Servant.API
import Servant.Client

import System.IO.Extra
import System.LogLevel
import System.Time.Extra

import Test.Tasty
import Test.Tasty.HUnit

import Pact.ApiReq (mkExec)
import Pact.Types.API
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.ChainMeta as Pact
import Pact.Types.Command
import qualified Pact.Types.Hash as H
import Pact.Types.Names
import Pact.Types.Term

-- internal modules

import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.PeerResources
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Pact.RestAPI
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.Service.Types
#if ! MIN_VERSION_servant(0,16,0)
import Chainweb.RestAPI.Utils
#endif
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

import P2P.Node.Configuration
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Global Settings

#define DEBUG_TEST 0

debug :: String -> IO ()
#if DEBUG_TEST
debug = putStrLn
#else
debug = const $ return ()
#endif

nNodes :: Natural
nNodes = 1

version :: ChainwebVersion
version = FastTimedCPM petersonChainGraph

cid :: HasCallStack => ChainId
cid = head . toList $ chainIds version

testCmds :: PactTestApiCmds
testCmds = apiCmds version cid

testCmdsChainId :: ChainId -> PactTestApiCmds
testCmdsChainId cid_ = apiCmds version cid_

-- -------------------------------------------------------------------------- --
-- Tests. GHCI use `runSchedRocks tests`

-- | Note: These tests are intermittently non-deterministic due to the way
-- random chain sampling works with our test harnesses.
--
tests :: RocksDb -> ScheduledTest
tests rdb = testGroupSch "Chainweb.Test.Pact.RemotePactTest"
    [ withNodes rdb nNodes $ \net ->
        withMVarResource 0 $ \iomvar ->
          withTime $ \iot ->
            testGroup "remote pact tests" [
                withRequestKeys iot iomvar net $ responseGolden net
              , after AllSucceed "remote-golden" $
                testGroup "remote spv" [spvRequests iot net]
              , after AllSucceed "remote spv" $
                testCase "/send reports validation failure" $
                sendValidationTest iot net
              ]
    ]

responseGolden :: IO ChainwebNetwork -> IO RequestKeys -> TestTree
responseGolden networkIO rksIO = golden "remote-golden" $ do
    rks <- rksIO
    cwEnv <- _getClientEnv <$> networkIO
    (PollResponses theMap) <- pollWithRetry testCmds cwEnv rks
    let values = mapMaybe (\rk -> _crResult <$> HashMap.lookup rk theMap)
                          (NEL.toList $ _rkRequestKeys rks)
    return $! toS $! foldMap A.encode values


sendValidationTest :: IO (Time Integer) -> IO ChainwebNetwork -> IO ()
sendValidationTest iot nio = do
    cenv <- fmap _getClientEnv nio
    mv <- newMVar 0
    SubmitBatch batch1 <- testBatch' iot 10000 mv
    SubmitBatch batch2 <- testBatch' (return $ Time $ TimeSpan 0) 2 mv
    let batch = SubmitBatch $ batch1 <> batch2
    sid <- mkChainId version (0 :: Int)
    expectSendFailure $ flip runClientM cenv $ do
        sendApiCmd (testCmdsChainId sid) batch

  where
    expectSendFailure act = do
        m <- (wrap `catch` h)
        maybe (return ()) (\msg -> assertBool msg False) m
      where
        wrap = do
            let ef out = Just ("expected exception on bad tx, got: "
                               <> show out)
            act >>= return . either (const Nothing) ef

        h :: SomeException -> IO (Maybe String)
        h _ = return Nothing


spvRequests :: IO (Time Integer) -> IO ChainwebNetwork -> TestTree
spvRequests iot nio = testCaseSteps "spv client tests"$ \step -> do
    cenv <- fmap _getClientEnv nio
    batch <- mkTxBatch
    sid <- mkChainId version (0 :: Int)
    r <- flip runClientM cenv $ do

      void $ liftIO $ step "sendApiClient: submit batch"
      rks <- sendApiCmd (testCmdsChainId sid) batch

      void $ liftIO $ step "pollApiClient: poll until key is found"
      void $ liftIO $ pollWithRetry (testCmdsChainId sid) cenv rks

      liftIO $ sleep 6

      void $ liftIO $ step "spvApiClient: submit request key"
      pactSpvApiClient version sid (go rks)

    case r of
      Left e -> assertFailure $ "output proof failed: " <> sshow e
      Right _ -> return ()
  where
    tid = Pact.ChainId "1"

    go (RequestKeys t) = SpvRequest (NEL.head t) tid

    mkTxBatch = do
      ks <- liftIO testKeyPairs
      t <- toTxCreationTime <$> iot
      let ttl = 2 * 24 * 60 * 60
          pm = Pact.PublicMeta (Pact.ChainId "0") "sender00" 100000 0.01 ttl t
      cmd1 <- liftIO $ mkExec txcode txdata pm ks Nothing (Just "0")
      cmd2 <- liftIO $ mkExec txcode txdata pm ks Nothing (Just "1")
      return $ SubmitBatch (pure cmd1 <> pure cmd2)

    txcode = show $
      [text|
         (coin.cross-chain-transfer
           'sender00
           (read-msg 'target-chain-id)
           'sender00
           (read-keyset 'sender00-keyset)
           1.0)
         |]

    txdata =
      -- sender00 keyset
      let ks = KeySet
            [ "6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7" ]
            (Name $ BareName "keys-all" def)
      in A.object
        [ "sender01-keyset" A..= ks
        , "target-chain-id" A..= tid
        ]

-- -------------------------------------------------------------------------- --
-- Utils

withRequestKeys
    :: IO (Time Integer)
    -> IO (MVar Int)
    -> IO ChainwebNetwork
    -> (IO RequestKeys -> TestTree)
    -> TestTree
withRequestKeys iot ioNonce networkIO = withResource mkKeys (\_ -> return ())
  where
    mkKeys = do
        cwEnv <- _getClientEnv <$> networkIO
        mNonce <- ioNonce
        testSend iot mNonce testCmds cwEnv

testSend :: IO (Time Integer) -> MVar Int -> PactTestApiCmds -> ClientEnv -> IO RequestKeys
testSend iot mNonce cmds env = do
    sb <- testBatch iot mNonce
    result <- sendWithRetry cmds env sb
    case result of
        Left e -> assertFailure (show e)
        Right rks -> return rks

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
                    debug $ "send failing after " ++ show maxSendRetries ++ " retries"
                    return result
                else do
                    sleep 1
                    go (retries - 1)
            Right _ -> do
                debug $ "send succeeded after " ++ show (maxSendRetries - retries) ++ " retries"
                return result

maxPollRetries :: Int
maxPollRetries = 30

-- | To allow time for node to startup, retry a number of times
pollWithRetry
    :: PactTestApiCmds
    -> ClientEnv
    -> RequestKeys
    -> IO PollResponses
pollWithRetry cmds env rks = do
  sleep 3
  go maxPollRetries
    where
      go !retries = do
          let retry failure = do
                if retries == 0
                  then do
                    debug $ "poll failing after " ++ show maxSendRetries
                               ++ " retries"
                    fail failure
                  else sleep 1 >> go (retries - 1)
          let rkks = _rkRequestKeys rks
          result <- runClientM (pollApiCmd cmds (Poll rkks)) env
          case result of
              Left e -> retry (show e)
              Right (PollResponses mp) -> do
                  ok <- checkResults (toList rkks) mp
                  if ok
                    then do
                        debug $ concat
                            [ "poll succeeded after "
                            , show (maxSendRetries - retries)
                            , " retries" ]
                        return $ PollResponses mp
                    else do
                        debug "poll check failed, continuing"
                        let msg = concat [ "poll check failure. keys: "
                                         , show rks
                                         , "\nresult map was: "
                                         , show mp
                                         ]
                        retry msg

      checkCommandResult x cr =
          _crReqKey cr == x && isOkPactResult (_crResult cr)

      isOkPactResult (PactResult e) = isRight e

      checkResults [] _ = return True
      checkResults (x:xs) mp = case HashMap.lookup x mp of
                                 (Just cr) -> if checkCommandResult x cr
                                                then checkResults xs mp
                                                else return False
                                 Nothing -> return False

testBatch' :: IO (Time Integer) -> Integer -> MVar Int -> IO SubmitBatch
testBatch' iot ttl mnonce = do
    modifyMVar mnonce $ \(!nn) -> do
        let nonce = "nonce" <> sshow nn
        t <- toTxCreationTime <$> iot
        kps <- testKeyPairs
        c <- mkExec "(+ 1 2)" A.Null (pm t) kps Nothing (Just nonce)
        pure $ (succ nn, SubmitBatch (pure c))
  where
    pm :: Pact.TxCreationTime -> Pact.PublicMeta
    pm t = Pact.PublicMeta (Pact.ChainId "0") "sender00" 1000 0.1
               (fromInteger ttl) t

testBatch :: IO (Time Integer) -> MVar Int -> IO SubmitBatch
testBatch iot mnonce = testBatch' iot ttl mnonce
  where
    ttl = 2 * 24 * 60 * 60

type PactClientApi
       = (SubmitBatch -> ClientM RequestKeys)
    :<|> ((Poll -> ClientM PollResponses)
    :<|> ((ListenerRequest -> ClientM ListenResponse)
    :<|> (Command Text -> ClientM (CommandResult H.Hash))))

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
        a <- withLink $ runTestNodes rdb Warn version n peerInfoVar
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
    System.IO.Extra.withTempDir $ \dir -> withChainweb conf logger rocksDb (Just dir) False $ \cw -> do

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
    & set configReintroTxs True
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
