{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

#define DEBUG_MULTINODE_TEST 0

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens (set, view, _head)
import Control.Monad

import Data.Aeson
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List
import qualified Data.Text as T
#if DEBUG_MULTINODE_TEST
import qualified Data.Text.IO as T
#endif
import Data.Time.Clock

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
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Difficulty (BlockRate(..), blockRate)
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Utils
import Chainweb.Time (Seconds)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.CAS.HashMap hiding (toList)
import Data.LogMessage

import P2P.Node.Configuration
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Generic Log Functions

-- | Simpel generic log functions for chainweb. For production purposes a proper
-- logging framework should be used.
--
chainwebLogFunctions
    :: Foldable f
    => LogLevel
    -> (T.Text -> IO ())
    -> NodeId
    -> f ChainId
    -> ChainwebLogFunctions
chainwebLogFunctions level write nid cids = ChainwebLogFunctions
    { _chainwebNodeLogFun = aLogFunction "node"
    , _chainwebMinerLogFun = aLogFunction "miner"
    , _chainwebCutLogFun = aLogFunction "cut"
    , _chainwebChainLogFuns = foldl' chainLog mempty cids
    }
  where
    -- a log function that logs only errors and writes them to stdout
    aLogFunction label = ALogFunction $ \l msg -> if
        | l <= level -> do
            now <- getCurrentTime
            write
                $ sq (sshow now)
                <> sq (toText nid)
                <> sq label
                <> sq (sshow l)
                <> " "
                <> logText msg
        | otherwise -> return ()

    chainLog m c = HM.insert c (aLogFunction (toText c)) m

    sq t = "[" <> t <> "]"

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
        -- Set the node id.

    & set (configP2p . p2pConfigPeer . peerConfigInterface) "127.0.0.1"
        -- Only listen on the loopback device. On Mac OS X this prevents the
        -- firewall dialog form poping up.

    & set (configP2p . p2pConfigMaxPeerCount) (n * 2)
        -- We make room for all test peers in peer db.

    & set (configP2p . p2pConfigMaxSessionCount) 4
        -- We set this to a low number in order to keep the network sparse (or
        -- at last no being a clique) and to also limit the number of
        -- port allocations

    & set (configP2p . p2pConfigSessionTimeout) 60
        -- Use short sessions to cover session timeouts and setup logic in the
        -- test.

    & set configChainDbDirPath chainDbDir
        -- Place where the chaindbs are persisted.

    & set (configMiner . configTestMiners) (MinerCount n)
        -- The number of test miners being used.

-- | Set the boostrap node port of a 'ChainwebConfiguration'
--
setBootstrapPort
    :: Port
        -- ^ Port of bootstrap node
    -> ChainwebConfiguration
    -> ChainwebConfiguration
setBootstrapPort
    = set (configP2p . p2pConfigKnownPeers . _head . peerAddr . hostAddressPort)
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
        & set (peerConfigAddr . hostAddressPort) 0
        -- Normally, the port of bootstrap nodes is hard-coded. But in
        -- test-suites that may run concurrently we want to use a port that is
        -- assigned by the OS.

-- -------------------------------------------------------------------------- --
-- Minimal Node Setup that logs conensus state to the given mvar

node
    :: LogLevel
    -> (T.Text -> IO ())
    -> MVar ConsensusState
    -> MVar Port
    -> ChainwebConfiguration
    -> IO ()
node loglevel write stateVar bootstrapPortVar conf =
    withChainweb @HashMapCas conf logfuns $ \cw -> do

        -- If this is the bootstrap node we extract the port number and
        -- publish via an MVar.
        when (nid == NodeId 0) $ putMVar bootstrapPortVar (cwPort cw)

        runChainweb cw `finally` sample cw
  where
    nid = _configNodeId conf

    logfuns = chainwebLogFunctions loglevel write nid (chainIds_ $ _chainGraph conf)

    sample cw = modifyMVar_ stateVar $ \state -> force <$>
        sampleConsensusState
            nid
            (view (chainwebCutResources . cutsCutDb . cutDbWebBlockHeaderDb) cw)
            (view (chainwebCutResources . cutsCutDb) cw)
            state

    cwPort = _hostAddressPort . _peerAddr . _peerInfo . _peerResPeer . _chainwebPeer

-- -------------------------------------------------------------------------- --
-- Run Nodes

runNodes
    :: LogLevel
    -> (T.Text -> IO ())
    -> MVar ConsensusState
    -> ChainwebVersion
    -> Natural
        -- ^ number of nodes
    -> (Maybe FilePath)
        -- ^ directory where the chaindbs are persisted
    -> IO ()
runNodes loglevel write stateVar v n chainDbDir = do
    bootstrapPortVar <- newEmptyMVar
        -- this is a hack for testing: normally bootstrap node peer infos are
        -- hardcoded. To avoid conflicts in concurrent test runs we extract an
        -- OS assigned port from the bootstrap node during startup and inject it
        -- into the configuration of the remaining nodes.

    forConcurrently_ [0 .. int n - 1] $ \i -> do
        threadDelay (500000 * int i)

        let baseConf = config v n (NodeId i) chainDbDir
        conf <- if
            | i == 0 ->
                return $ bootstrapConfig baseConf
            | otherwise ->
                setBootstrapPort <$> readMVar bootstrapPortVar <*> pure baseConf

        node loglevel write stateVar bootstrapPortVar conf

runNodesForSeconds
    :: LogLevel
        -- ^ Loglevel
    -> ChainwebVersion
    -> Natural
        -- ^ Number of chainweb consensus nodes
    -> Seconds
        -- ^ test duration in seconds
    -> Maybe FilePath
        -- ^ directory where the chaindbs are persisted
    -> (T.Text -> IO ())
        -- ^ logging backend callback
    -> IO (Maybe Stats)
runNodesForSeconds loglevel v n seconds chainDbDir write = do
    stateVar <- newMVar $ emptyConsensusState v
    void $ timeout (int seconds * 1000000)
        $ runNodes loglevel write stateVar v n chainDbDir

    consensusState <- readMVar stateVar
    return (consensusStateSummary consensusState)

-- -------------------------------------------------------------------------- --
-- Test

test
    :: LogLevel
    -> ChainwebVersion
    -> Natural
    -> Seconds
    -> Maybe FilePath
    -> TestTree
test loglevel v n seconds chainDbDir = testCaseSteps label $ \f -> do
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

    runNodesForSeconds loglevel v n seconds chainDbDir countedLog >>= \case
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
    !hashes' <- S.fold_ (flip HS.insert) (_stateBlockHashes s) id
        $ S.map _blockHash
        $ webEntries bhdb
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
expectedBlockCount v seconds = round ebc
  where
    ebc :: Double
    ebc = int seconds * int (order $ _chainGraph v) / int br

    br :: Natural
    br = case blockRate v of
        Just (BlockRate n) -> int n
        Nothing -> error $ "expectedBlockCount: ChainwebVersion with no BlockRate given: " <> show v

lowerStats :: ChainwebVersion -> Seconds -> Stats
lowerStats v seconds = Stats
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
        Just (BlockRate n) -> int n
        Nothing -> error $ "lowerStats: ChainwebVersion with no BlockRate given: " <> show v

upperStats :: ChainwebVersion -> Seconds -> Stats
upperStats v seconds = Stats
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
        Just (BlockRate n) -> int n
        Nothing -> error $ "upperStats: ChainwebVersion with no BlockRate given: " <> show v
