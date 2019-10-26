{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.MultiNode
-- Copyright: Copyright © 2019 Kadena LLC.
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
module Chainweb.Test.MultiNode ( test ) where

#ifndef DEBUG_MULTINODE_TEST
#define DEBUG_MULTINODE_TEST 0
#endif

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens (over, set, view)
import Control.Monad

import Data.Aeson
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List
import Data.Streaming.Network (HostPreference)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#if DEBUG_MULTINODE_TEST
import qualified Data.Text.IO as T
#endif

import GHC.Generics

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.LogLevel
import System.Timeout

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Chainweb
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Utils
import Chainweb.Time (Seconds(..))
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.CAS.RocksDB

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

host :: Hostname
host = unsafeHostnameFromText "::1"

interface :: HostPreference
interface = "::1"

-- | Test Configuration for a scaled down Test chainweb.
--
config
    :: ChainwebVersion
    -> Natural
        -- ^ number of nodes
    -> NodeId
        -- ^ NodeId
    -> ChainwebConfiguration
config v n nid = defaultChainwebConfiguration v
    & set configNodeId nid
        -- Set the node id.

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
        -- at last no being a clique) and to also limit the number of
        -- port allocations

    & set (configP2p . p2pConfigSessionTimeout) 60
        -- Use short sessions to cover session timeouts and setup logic in the
        -- test.

    & set (configMiner . enableConfigConfig . configTestMiners) (MinerCount n)
        -- The number of test miners being used.

    & set configReintroTxs True
        -- enable transaction re-introduction

    & set (configTransactionIndex . enableConfigEnabled) True
        -- enable transaction index

-- | Set the bootstrap node port of a 'ChainwebConfiguration'
--
setBootstrapPeerInfo
    :: PeerInfo
        -- ^ Peer info of bootstrap node
    -> ChainwebConfiguration
    -> ChainwebConfiguration
setBootstrapPeerInfo
    = over (configP2p . p2pConfigKnownPeers) . (:)
        -- The the port of the bootstrap node. Normally this is hard-coded.
        -- But in test-suites that may run concurrently we want to use a port
        -- that is assigned by the OS.

-- | Configure a bootstrap node
--
bootstrapConfig
    :: ChainwebConfiguration
    -> ChainwebConfiguration
bootstrapConfig conf = conf
    & set (configP2p . p2pConfigPeer) peerConfig
    & set (configP2p . p2pConfigKnownPeers) []
  where
    peerConfig = (head $ bootstrapPeerConfig $ _configChainwebVersion conf)
        & set peerConfigPort 0
        -- Normally, the port of bootstrap nodes is hard-coded. But in
        -- test-suites that may run concurrently we want to use a port that is
        -- assigned by the OS.

        & set peerConfigHost host
        & set peerConfigInterface interface

-- -------------------------------------------------------------------------- --
-- Minimal Node Setup that logs conensus state to the given mvar

node
    :: LogLevel
    -> (T.Text -> IO ())
    -> MVar ConsensusState
    -> MVar PeerInfo
    -> ChainwebConfiguration
    -> RocksDb
    -> IO ()
node loglevel write stateVar bootstrapPeerInfoVar conf rdb = do
    withChainweb conf logger nodeRocksDb Nothing False $ \cw -> do

        -- If this is the bootstrap node we extract the port number and
        -- publish via an MVar.
        when (nid == NodeId 0) $ putMVar bootstrapPeerInfoVar
            $ view (chainwebPeer . peerResPeer . peerInfo) cw

        runChainweb cw `finally` do
            logFunctionText logger Info "write sample data"
            sample cw
            logFunctionText logger Info "shutdown node"
  where
    nid = _configNodeId conf

    logger :: GenericLogger
    logger = addLabel ("node", toText nid) $ genericLogger loglevel write

    sample cw = modifyMVar_ stateVar $ \state -> force <$>
        sampleConsensusState
            nid
            (view (chainwebCutResources . cutsCutDb . cutDbWebBlockHeaderDb) cw)
            (view (chainwebCutResources . cutsCutDb) cw)
            state

    nodeRocksDb = set rocksDbNamespace (T.encodeUtf8 $ toText nid) rdb

-- -------------------------------------------------------------------------- --
-- Run Nodes

runNodes
    :: LogLevel
    -> (T.Text -> IO ())
    -> MVar ConsensusState
    -> ChainwebVersion
    -> Natural
        -- ^ number of nodes
    -> IO ()
runNodes loglevel write stateVar v n =
    withTempRocksDb "multinode-tests" $ \rdb -> do

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
            threadDelay (500000 * int i)

            let baseConf = config v n (NodeId i)
            conf <- if
                | i == 0 ->
                    return $ bootstrapConfig baseConf
                | otherwise ->
                    setBootstrapPeerInfo <$> readMVar bootstrapPortVar <*> pure baseConf

            node loglevel write stateVar bootstrapPortVar conf rdb

runNodesForSeconds
    :: LogLevel
        -- ^ Loglevel
    -> ChainwebVersion
    -> Natural
        -- ^ Number of chainweb consensus nodes
    -> Seconds
        -- ^ test duration in seconds
    -> (T.Text -> IO ())
        -- ^ logging backend callback
    -> IO (Maybe Stats)
runNodesForSeconds loglevel v n (Seconds seconds) write = do
    stateVar <- newMVar $ emptyConsensusState v
    void $ timeout (int seconds * 1000000)
        $ runNodes loglevel write stateVar v n

    consensusState <- readMVar stateVar
    return (consensusStateSummary consensusState)

-- -------------------------------------------------------------------------- --
-- Test

test
    :: LogLevel
    -> ChainwebVersion
    -> Natural
    -> Seconds
    -> TestTree
test loglevel v n seconds = testCaseSteps label $ \f -> do
    let tastylog = f . T.unpack
#if DEBUG_MULTINODE_TEST
    -- useful for debugging, requires import of Data.Text.IO.
    let logFun = T.putStrLn
        maxLogMsgs = 100000
#else
    let logFun = tastylog
        maxLogMsgs = 60
#endif

    -- Count log messages and only print the first 60 messages
    var <- newMVar (0 :: Int)
    let countedLog msg = modifyMVar_ var $ \c -> force (succ c) <$
            if c < maxLogMsgs then logFun msg else return ()

    runNodesForSeconds loglevel v n seconds countedLog >>= \case
        Nothing -> assertFailure "chainweb didn't make any progress"
        Just stats -> do
            logsCount <- readMVar var
            tastylog $ "Number of logs: " <> sshow logsCount
            tastylog $ "Expected BlockCount: " <> sshow (expectedBlockCount v seconds)
            tastylog $ encodeToText stats
            tastylog $ encodeToText $ object
                [ "maxEfficiency%" .= (realToFrac (_statMaxHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                , "minEfficiency%" .= (realToFrac (_statMinHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                , "medEfficiency%" .= (realToFrac (_statMedHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                , "avgEfficiency%" .= (_statAvgHeight stats * (100 :: Double) / int (_statBlockCount stats))
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

    label = "ConsensusNetwork (nodes: " <> show n <> ", seconds: " <> show seconds <> ")"

-- -------------------------------------------------------------------------- --
-- Results

data ConsensusState = ConsensusState
    { _stateBlockHashes :: !(HS.HashSet BlockHash)
        -- ^ for short tests this is fine. For larger test runs we should
        -- use HyperLogLog+

    , _stateCutMap :: !(HM.HashMap NodeId Cut)
    , _stateChainwebVersion :: !ChainwebVersion
    }
    deriving (Show, Generic, NFData)

instance HasChainwebVersion ConsensusState where
    _chainwebVersion = _stateChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ConsensusState where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

emptyConsensusState :: ChainwebVersion -> ConsensusState
emptyConsensusState v = ConsensusState mempty mempty v

sampleConsensusState
    :: NodeId
    -> WebBlockHeaderDb
    -> CutDb cas
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
    { _statBlockCount :: !Int
    , _statMaxHeight :: !BlockHeight
    , _statMinHeight :: !BlockHeight
    , _statMedHeight :: !BlockHeight
    , _statAvgHeight :: !Double
    }
    deriving (Show, Eq, Ord, Generic, ToJSON)

consensusStateSummary :: ConsensusState -> Maybe Stats
consensusStateSummary s
    | HM.null (_stateCutMap s) = Nothing
    | otherwise = Just Stats
        { _statBlockCount = hashCount
        , _statMaxHeight = maxHeight
        , _statMinHeight = minHeight
        , _statMedHeight = medHeight
        , _statAvgHeight = avgHeight
        }
  where
    cutHeights = _cutHeight <$> _stateCutMap s
    hashCount = HS.size (_stateBlockHashes s) - int (order $ _chainGraph s)

    avg :: Foldable f => Real a => f a -> Double
    avg f = realToFrac (sum $ toList f) / realToFrac (length f)

    median :: Foldable f => Ord a => f a -> a
    median f = (!! ((length f + 1) `div` 2)) $ sort (toList f)

    minHeight = minimum $ HM.elems cutHeights
    maxHeight = maximum $ HM.elems cutHeights
    avgHeight = avg $ HM.elems cutHeights
    medHeight = median $ HM.elems cutHeights

expectedBlockCount :: ChainwebVersion -> Seconds -> Natural
expectedBlockCount v (Seconds seconds) = round ebc
  where
    ebc :: Double
    ebc = int seconds * int (order $ _chainGraph v) / int br

    br :: Natural
    br = case blockRate v of
        BlockRate (Seconds n) -> int n

lowerStats :: ChainwebVersion -> Seconds -> Stats
lowerStats v (Seconds seconds) = Stats
    { _statBlockCount = round $ ebc * 0.3 -- temporarily, was 0.8
    , _statMaxHeight = round $ ebc * 0.3 -- temporarily, was 0.7
    , _statMinHeight = round $ ebc * 0.09 -- temporarily, was 0.3
    , _statMedHeight = round $ ebc * 0.3 -- temporarily, was 0.5
    , _statAvgHeight = ebc * 0.3 -- temporarily, was 0.5
    }
  where
    ebc :: Double
    ebc = int seconds * int (order $ _chainGraph v) / int br

    br :: Natural
    br = case blockRate v of
        BlockRate (Seconds n) -> int n

upperStats :: ChainwebVersion -> Seconds -> Stats
upperStats v (Seconds seconds) = Stats
    { _statBlockCount = round $ ebc * 1.2
    , _statMaxHeight = round $ ebc * 1.2
    , _statMinHeight = round $ ebc * 1.2
    , _statMedHeight = round $ ebc * 1.2
    , _statAvgHeight = ebc * 1.2
    }
  where
    ebc :: Double
    ebc = int seconds * int (order $ _chainGraph v) / int br

    br :: Natural
    br = case blockRate v of
        BlockRate (Seconds n) -> int n
