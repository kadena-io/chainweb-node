{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.MultiNode
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- # Test Configuration
--
-- The test runs
--
-- * for a short time,
-- * on a small chain,
-- * over a fast and reliable network,
-- * with a limited number of nodes.
--
-- The configuration defines a scaled down, accelerated chain that tries to
-- similulate a full-scale chain in a miniaturized settings.
--
module Chainweb.Test.MultiNode ( test, replayTest, compactAndResumeTest ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens (set, view, over)
import Control.Monad

import Data.Aeson
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.FilePath
import System.IO.Temp
import System.Logger.Types qualified as YAL
import System.LogLevel
import System.Timeout

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Chainweb
import Chainweb.Chainweb.Configuration
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.Types (_sConn)
import Chainweb.Pact.Backend.Utils (withSqliteDb)
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.Pact
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Utils
import Chainweb.Time (Seconds(..))
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB

import Chainweb.Storage.Table.RocksDB

import P2P.Node.Configuration
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- * Configuration
--
-- This test runs
--
-- * for a short time,
-- * on a small chain,
-- * over a fast and reliable network,
-- * with a limited number of nodes.
--
-- The configuration defines a scaled down, accelerated chain that tries to
-- similulate a full-scale chain in a miniaturized settings.
--

-- | Test Configuration for a scaled down Test chainweb.
--
multiConfig
    :: ChainwebVersion
    -> Natural
        -- ^ number of node
    -> ChainwebConfiguration
multiConfig v n = defaultChainwebConfiguration v
    & set (configP2p . p2pConfigPeer . peerConfigHost) host
    & set (configP2p . p2pConfigPeer . peerConfigPort) 0
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
        -- at last no being a clique) and to also limit the number of
        -- port allocations

    & set (configP2p . p2pConfigSessionTimeout) 20
        -- Use short sessions to cover session timeouts and setup logic in the
        -- test.

    & set (configP2p . p2pConfigBootstrapReachability) 0
        -- disable reachability test, which is unreliable during testing

    & set (configMining . miningCoordination . coordinationEnabled) True
    & set (configMining . miningInNode) miner

    & set configReintroTxs True
        -- enable transaction re-introduction

    & set configThrottling throttling
        -- throttling is effectively disabled to not slow down the test nodes

    & set (configServiceApi . serviceApiConfigPort) 0
    & set (configServiceApi . serviceApiConfigInterface) interface
  where
    miner = NodeMiningConfig
        { _nodeMiningEnabled = True
        , _nodeMiner = noMiner
        , _nodeTestMiners = MinerCount n
        }

    throttling = defaultThrottlingConfig
        { _throttlingRate = 10_000 -- per second
        , _throttlingPeerRate = 10_000 -- per second, one for each p2p network
        }

-- | Configure a bootstrap node
--
multiBootstrapConfig
    :: ChainwebConfiguration
    -> ChainwebConfiguration
multiBootstrapConfig conf = conf
    & set (configP2p . p2pConfigPeer) peerConfig
    & set (configP2p . p2pConfigKnownPeers) []
  where
    peerConfig = (head $ testBootstrapPeerConfig $ _configChainwebVersion conf)
        & set peerConfigPort 0
        -- Normally, the port of bootstrap nodes is hard-coded. But in
        -- test-suites that may run concurrently we want to use a port that is
        -- assigned by the OS.

        & set peerConfigHost host
        & set peerConfigInterface interface

-- -------------------------------------------------------------------------- --
-- Minimal Node Setup that logs conensus state to the given mvar

harvestConsensusState
    :: GenericLogger
    -> MVar ConsensusState
    -> Int
    -> StartedChainweb logger
    -> IO ()
harvestConsensusState _ _ _ (Replayed _ _) =
    error "harvestConsensusState: doesn't work when replaying, replays don't do consensus"
harvestConsensusState logger stateVar nid (StartedChainweb cw) = do
    runChainweb cw `finally` do
        logFunctionText logger Info "write sample data"
        modifyMVar_ stateVar $
            sampleConsensusState
                nid
                (view (chainwebCutResources . cutsCutDb . cutDbWebBlockHeaderDb) cw)
                (view (chainwebCutResources . cutsCutDb) cw)
        logFunctionText logger Info "shutdown node"

multiNode
    :: LogLevel
    -> (T.Text -> IO ())
    -> MVar PeerInfo
    -> ChainwebConfiguration
    -> RocksDb
    -> FilePath
    -> Int
        -- ^ Unique node id. Node id 0 is used for the bootstrap node
    -> (forall logger. Int -> StartedChainweb logger -> IO ())
    -> IO ()
multiNode loglevel write bootstrapPeerInfoVar conf rdb pactDbDir nid inner = do
    withSystemTempDirectory "multiNode-backup-dir" $ \backupTmpDir ->
            withChainweb conf logger nodeRocksDb (pactDbDir </> show nid) backupTmpDir False $ \cw -> do
                case cw of
                    StartedChainweb cw' ->
                        when (nid == 0) $ putMVar bootstrapPeerInfoVar
                            $ view (chainwebPeer . peerResPeer . peerInfo) cw'
                    Replayed _ _ -> return ()
                inner nid cw
  where
    logger :: GenericLogger
    logger = addLabel ("node", toText nid) $ genericLogger loglevel write

    nodeRocksDb = rdb { _rocksDbNamespace = T.encodeUtf8 $ toText nid }

-- -------------------------------------------------------------------------- --
-- Run Nodes

runNodes
    :: LogLevel
    -> (T.Text -> IO ())
    -> ChainwebConfiguration
    -> Natural
        -- ^ number of nodes
    -> RocksDb
    -> FilePath
    -> (forall logger. Int -> StartedChainweb logger -> IO ())
    -> IO ()
runNodes loglevel write baseConf n rdb pactDbDir inner = do
    -- NOTE: pact is enabled until we have a good way to disable it globally in
    -- "Chainweb.Chainweb".
    --
    -- TODO: disable pact for these tests
    --

    bootstrapPortVar <- newEmptyMVar
        -- this is a hack for testing: normally bootstrap node peer infos are
        -- hardcoded. To avoid conflicts in concurrent test runs we extract an
        -- OS assigned port from the bootstrap node during startup and inject it
        -- into the configuration of the remaining nodes.

    forConcurrently_ [0 .. int n - 1] $ \i -> do
        threadDelay (500_000 * int i)

        conf <- if
            | i == 0 ->
                return $ multiBootstrapConfig baseConf
            | otherwise ->
                setBootstrapPeerInfo <$> readMVar bootstrapPortVar <*> pure baseConf

        multiNode loglevel write bootstrapPortVar conf rdb pactDbDir i inner

runNodesForSeconds
    :: LogLevel
        -- ^ Loglevel
    -> (T.Text -> IO ())
        -- ^ logging backend callback
    -> ChainwebConfiguration
    -> Natural
        -- ^ Number of chainweb consensus nodes
    -> Seconds
        -- ^ test duration in seconds
    -> RocksDb
    -> FilePath
    -> (forall logger. Int -> StartedChainweb logger -> IO ())
    -> IO ()
runNodesForSeconds loglevel write baseConf n (Seconds seconds) rdb pactDbDir inner = do
    void $ timeout (int seconds * 1_000_000)
        $ runNodes loglevel write baseConf n rdb pactDbDir inner

-- | Run nodes
--   Each node creates blocks
--   We wait until they've made a sufficient amount of blocks
--   We stop the nodes
--   We open sqlite connections to some of the database dirs and compact them
--   We restart all nodes with the same database dirs
--   We observe that they can make progress
compactAndResumeTest :: ()
  => LogLevel
  -> ChainwebVersion
  -> Natural
  -> TestTree
compactAndResumeTest logLevel v n =
  let
    name = "compact-resume"
  in
  after AllFinish "ConsensusNetwork" $ testCaseSteps name $ \step ->
  withTempRocksDb "compact-resume-test-rocks" $ \rdb ->
  withSystemTempDirectory "compact-resume-test-pact" $ \pactDbDir -> do
    let logFun = step . T.unpack
    let logger = genericLogger logLevel logFun

    logFun "phase 1... creating blocks"
    -- N.B: This consensus state stuff counts the number of blocks
    -- in RocksDB, rather than the number of blocks in all chains
    -- on the current cut. This is fine because we ultimately just want
    -- to make sure that we are making progress (i.e, new blocks).
    stateVar <- newMVar (emptyConsensusState v)
    let ct :: Int -> StartedChainweb logger -> IO ()
        ct = harvestConsensusState logger stateVar
    runNodesForSeconds logLevel logFun (multiConfig v n) n 60 rdb pactDbDir ct
    Just stats1 <- consensusStateSummary <$> swapMVar stateVar (emptyConsensusState v)
    assertGe "average block count before compaction" (Actual $ _statBlockCount stats1) (Expected 50)
    logFun $ sshow stats1

    logFun "phase 2... compacting"
    let cid = unsafeChainId 0
    -- compact only half of them
    let nids = filter even [0 .. int @_ @Int n - 1]
    forM_ nids $ \nid -> do
      let dir = pactDbDir </> show nid
      withSqliteDb cid logger dir False $ \sqlEnv -> do
        C.withDefaultLogger YAL.Warn $ \cLogger -> do
          let cLogger' = over YAL.setLoggerScope (\scope -> ("nodeId",sshow nid) : ("chainId",sshow cid) : scope) cLogger
          let flags = [C.NoVacuum, C.NoGrandHash]
          let db = _sConn sqlEnv
          let bh = BlockHeight 5
          void $ C.compact (C.Target bh) cLogger' db flags

    logFun "phase 3... restarting nodes and ensuring progress"
    runNodesForSeconds logLevel logFun (multiConfig v n) n 60 rdb pactDbDir ct
    Just stats2 <- consensusStateSummary <$> swapMVar stateVar (emptyConsensusState v)
    -- We ensure that we've gotten to at least 1.5x the previous block count
    assertGe "average block count post-compaction" (Actual $ _statBlockCount stats2) (Expected (3 * _statBlockCount stats1 `div` 2))
    logFun $ sshow stats2

replayTest
    :: LogLevel
    -> ChainwebVersion
    -> Natural
    -> TestTree
replayTest loglevel v n = after AllFinish "ConsensusNetwork" $ testCaseSteps name $ \step ->
    withTempRocksDb "replay-test-rocks" $ \rdb ->
    withSystemTempDirectory "replay-test-pact" $ \pactDbDir -> do
        let tastylog = step . T.unpack
        let logFun = step . T.unpack
        tastylog "phase 1..."
        stateVar <- newMVar $ emptyConsensusState v
        let ct = harvestConsensusState (genericLogger loglevel logFun) stateVar
        runNodesForSeconds loglevel logFun (multiConfig v n) n 60 rdb pactDbDir ct
        Just stats1 <- consensusStateSummary <$> swapMVar stateVar (emptyConsensusState v)
        assertGe "maximum cut height before reset" (Actual $ _statMaxHeight stats1) (Expected $ 10)
        tastylog $ sshow stats1
        tastylog $ "phase 2... resetting"
        runNodesForSeconds loglevel logFun (multiConfig v n & set (configCuts . cutInitialBlockHeightLimit) (Just 5)) n 30 rdb pactDbDir ct
        state2 <- swapMVar stateVar (emptyConsensusState v)
        let stats2 = fromJuste $ consensusStateSummary state2
        tastylog $ sshow stats2
        assertGe "block count after reset" (Actual $ _statBlockCount stats2) (Expected $ _statBlockCount stats1)
        tastylog $ "phase 3... replaying"
        let replayInitialHeight = 5
        firstReplayCompleteRef <- newIORef False
        runNodesForSeconds loglevel logFun
            (multiConfig v n
                & set (configCuts . cutInitialBlockHeightLimit) (Just replayInitialHeight)
                & set configOnlySyncPact True)
            n (Seconds 20) rdb pactDbDir $ \nid cw -> case cw of
                Replayed l u -> do
                    writeIORef firstReplayCompleteRef True
                    _ <- flip HM.traverseWithKey (_cutMap l) $ \cid bh ->
                        assertEqual ("lower chain " <> sshow cid) replayInitialHeight (_blockHeight bh)
                    assertEqual "upper cut" (_stateCutMap state2 HM.! nid) u
                    _ <- flip HM.traverseWithKey (_cutMap u) $ \cid bh ->
                        assertGe ("upper chain " <> sshow cid) (Actual $ _blockHeight bh) (Expected replayInitialHeight)
                    return ()
                _ -> error "replayTest: not a replay"
        assertEqual "first replay completion" True =<< readIORef firstReplayCompleteRef
        let fastForwardHeight = 10
        tastylog $ "phase 4... replaying with fast-forward limit"
        secondReplayCompleteRef <- newIORef False
        runNodesForSeconds loglevel logFun
            (multiConfig v n
                & set (configCuts . cutInitialBlockHeightLimit) (Just replayInitialHeight)
                & set (configCuts . cutFastForwardBlockHeightLimit) (Just fastForwardHeight)
                & set configOnlySyncPact True)
            n (Seconds 20) rdb pactDbDir $ \_ cw -> case cw of
                Replayed l u -> do
                    writeIORef secondReplayCompleteRef True
                    _ <- flip HM.traverseWithKey (_cutMap l) $ \cid bh ->
                        assertEqual ("lower chain " <> sshow cid) replayInitialHeight (_blockHeight bh)
                    _ <- flip HM.traverseWithKey (_cutMap u) $ \cid bh ->
                        assertEqual ("upper chain " <> sshow cid) fastForwardHeight (_blockHeight bh)
                    return ()
                _ -> error "replayTest: not a replay"
        assertEqual "second replay completion" True =<< readIORef secondReplayCompleteRef
        tastylog "done."
    where
    name = "Replay network"

-- -------------------------------------------------------------------------- --
-- Test

test
    :: LogLevel
    -> ChainwebVersion
    -> Natural
    -> Seconds
    -> TestTree
test loglevel v n seconds = testCaseSteps name $ \f ->
    -- Count log messages and only print the first 60 messages
    withTempRocksDb "multinode-tests" $ \rdb ->
    withSystemTempDirectory "replay-test-pact" $ \pactDbDir -> do
        let tastylog = f . T.unpack
        let logFun = tastylog
            maxLogMsgs = 60
        var <- newMVar (0 :: Int)
        let countedLog msg = modifyMVar_ var $ \c -> force (succ c) <$
                when (c < maxLogMsgs) (logFun msg)
        stateVar <- newMVar (emptyConsensusState v)
        runNodesForSeconds loglevel countedLog (multiConfig v n) n seconds rdb pactDbDir
            (harvestConsensusState (genericLogger loglevel logFun) stateVar)
        consensusStateSummary <$> readMVar stateVar >>= \case
            Nothing -> assertFailure "chainweb didn't make any progress"
            Just stats -> do
                logsCount <- readMVar var
                tastylog $ "Number of logs: " <> sshow logsCount
                tastylog $ "Expected BlockCount: " <> sshow (expectedBlockCount v seconds) -- 80 + 19.5 * 20
                tastylog $ encodeToText stats
                tastylog $ encodeToText $ object
                    [ "maxEfficiency%" .= (realToFrac (bc $ _statMaxHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                    , "minEfficiency%" .= (realToFrac (bc $ _statMinHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                    , "medEfficiency%" .= (realToFrac (bc $ _statMedHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                    , "avgEfficiency%" .= (realToFrac (bc $ round (_statAvgHeight stats)) * (100 :: Double) / int (_statBlockCount stats))
                    ]

                (assertGe "number of blocks") (Actual $ _statBlockCount stats) (Expected $ _statBlockCount l)
                (assertGe "maximum cut height") (Actual $ _statMaxHeight stats) (Expected $ _statMaxHeight l)
                (assertGe "minimum cut height") (Actual $ _statMinHeight stats) (Expected $ _statMinHeight l)
                (assertGe "median cut height") (Actual $ _statMedHeight stats) (Expected $ _statMedHeight l)
                (assertGe "average cut height") (Actual $ _statAvgHeight stats) (Expected $ _statAvgHeight l)

                (assertLe "maximum cut height") (Actual $ _statMaxHeight stats) (Expected $ _statMaxHeight u)
                (assertLe "minimum cut height") (Actual $ _statMinHeight stats) (Expected $ _statMinHeight u)
                (assertLe "median cut height") (Actual $ _statMedHeight stats) (Expected $ _statMedHeight u)
                (assertLe "average cut height") (Actual $ _statAvgHeight stats) (Expected $ _statAvgHeight u)

  where
    l = lowerStats v seconds
    u = upperStats v seconds

    name = "ConsensusNetwork (nodes: " <> show n <> ", seconds: " <> show seconds <> ")"

    bc x = blockCountAtCutHeight v x - order (chainGraphAtCutHeight v x)

-- -------------------------------------------------------------------------- --
-- Results

data ConsensusState = ConsensusState
    { _stateBlockHashes :: !(HS.HashSet BlockHash)
        -- ^ for short tests this is fine. For larger test runs we should
        -- use HyperLogLog+

    , _stateCutMap :: !(HM.HashMap Int Cut)
        -- ^ Node Id map
    , _stateChainwebVersion :: !ChainwebVersion
    }
    deriving (Show, Generic, NFData)

instance HasChainwebVersion ConsensusState where
    _chainwebVersion = _stateChainwebVersion

emptyConsensusState :: ChainwebVersion -> ConsensusState
emptyConsensusState v = ConsensusState mempty mempty v

sampleConsensusState
    :: Int
        -- ^ node Id
    -> WebBlockHeaderDb
    -> CutDb tbl
    -> ConsensusState
    -> IO ConsensusState
sampleConsensusState nid bhdb cutdb s = do
    !hashes' <- webEntries bhdb
        $ S.fold_ (flip HS.insert) (_stateBlockHashes s) id
        . S.map _blockHash
    !c <- _cut cutdb
    return $! s
        { _stateBlockHashes = hashes'
        , _stateCutMap = HM.insert nid c (_stateCutMap s)
        }

data Stats = Stats
    { _statBlockCount :: !Natural
    , _statMaxHeight :: !CutHeight
    , _statMinHeight :: !CutHeight
    , _statMedHeight :: !CutHeight
    , _statAvgHeight :: !Double
    }
    deriving (Show, Eq, Ord, Generic, ToJSON)

consensusStateSummary :: ConsensusState -> Maybe Stats
consensusStateSummary s
    | HM.null (_stateCutMap s) = Nothing
    | otherwise = Just Stats
        { _statBlockCount = int $ hashCount
        , _statMaxHeight = maxHeight
        , _statMinHeight = minHeight
        , _statMedHeight = medHeight
        , _statAvgHeight = avgHeight
        }
  where
    cutHeights = _cutHeight <$> _stateCutMap s
    graph = chainGraphAt s
        $ maximum . concatMap chainHeights
        $ toList
        $ _stateCutMap s
    hashCount = HS.size (_stateBlockHashes s) - int (order graph)

    avg :: Foldable f => Real a => f a -> Double
    avg f = realToFrac (sum $ toList f) / realToFrac (length f)

    median :: Foldable f => Ord a => f a -> a
    median f = (!! ((length f + 1) `div` 2)) $ L.sort (toList f)

    minHeight = minimum $ HM.elems cutHeights
    maxHeight = maximum $ HM.elems cutHeights
    avgHeight = avg $ HM.elems cutHeights
    medHeight = median $ HM.elems cutHeights

expectedBlockCount :: ChainwebVersion -> Seconds -> Double
expectedBlockCount v s = expectedGlobalBlockCountAfterSeconds v s

lowerStats :: ChainwebVersion -> Seconds -> Stats
lowerStats v seconds = Stats
    { _statBlockCount = round $ ebc * 0.6 -- temporarily, was 0.8
    , _statMaxHeight = round $ ebc * 0.5 -- temporarily, was 0.7
    , _statMinHeight = round $ ebc * 0.1 -- temporarily, was 0.3
    , _statMedHeight = round $ ebc * 0.5
    , _statAvgHeight = ebc * 0.5
    }
  where
    ebc = expectedBlockCount v seconds


upperStats :: ChainwebVersion -> Seconds -> Stats
upperStats v seconds = Stats
    { _statBlockCount = round $ ebc * 1.4
    , _statMaxHeight = round $ ech * 1.4
    , _statMinHeight = round $ ech * 1.4
    , _statMedHeight = round $ ech * 1.4
    , _statAvgHeight = ech * 1.4
    }
  where
    ebc = expectedBlockCount v seconds
    ech = expectedCutHeightAfterSeconds v seconds

