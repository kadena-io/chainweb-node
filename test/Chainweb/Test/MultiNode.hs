{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
module Chainweb.Test.MultiNode ( test ) where

#ifndef DEBUG_MULTINODE_TEST
#define DEBUG_MULTINODE_TEST 0
#endif

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens (set, view)
import Control.Monad

import Data.Aeson
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#if DEBUG_MULTINODE_TEST
import qualified Data.Text.IO as T
#endif

import GHC.Generics

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.IO.Temp
import System.LogLevel
import System.Timeout

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Chainweb
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Test.Utils
import Chainweb.Time (Seconds(..))
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
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

-- | Test Configuration for a scaled down Test chainweb.
--
multiConfig
    :: ChainwebVersion
    -> Natural
        -- ^ number of nodes
    -> ChainwebConfiguration
multiConfig v n = config v n
    & set (configP2p . p2pConfigSessionTimeout) 20
        -- Use short sessions to cover session timeouts and setup logic in the
        -- test.
    & set configThrottling throttling
        -- throttling is effectively disabled to not slow down the test nodes
  where
    throttling = defaultThrottlingConfig
        { _throttlingRate = 10_000 -- per second
        , _throttlingMiningRate = 10_000 --  per second
        , _throttlingPeerRate = 10_000 -- per second, one for each p2p network
        , _throttlingLocalRate = 10_000  -- per 10 seconds
        }

-- -------------------------------------------------------------------------- --
-- Minimal Node Setup that logs conensus state to the given mvar

multiNode
    :: LogLevel
    -> (T.Text -> IO ())
    -> MVar ConsensusState
    -> MVar PeerInfo
    -> ChainwebConfiguration
    -> RocksDb
    -> Int
        -- ^ Unique node id. Node id 0 is used for the bootstrap node
    -> IO ()
multiNode loglevel write stateVar bootstrapPeerInfoVar conf rdb nid = do
    withSystemTempDirectory "multiNode-pact-db" $ \pactDbDir ->
        withChainweb conf logger nodeRocksDb pactDbDir False $ \cw -> do

            -- If this is the bootstrap node we extract the port number and
            -- publish via an MVar.
            when (nid == 0) $ putMVar bootstrapPeerInfoVar
                $ view (chainwebPeer . peerResPeer . peerInfo) cw

            runChainweb cw `finally` do
                logFunctionText logger Info "write sample data"
                sample cw
                logFunctionText logger Info "shutdown node"
  where
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
            threadDelay (500_000 * int i)

            let baseConf = multiConfig v n
            conf <- if
                | i == 0 ->
                    return $ bootstrapConfig baseConf
                | otherwise ->
                    setBootstrapPeerInfo <$> readMVar bootstrapPortVar <*> pure baseConf

            multiNode loglevel write stateVar bootstrapPortVar conf rdb i

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
    void $ timeout (int seconds * 1_000_000)
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
        maxLogMsgs = 100_000
#else
    let logFun = tastylog
        maxLogMsgs = 60
#endif

    -- Count log messages and only print the first 60 messages
    var <- newMVar (0 :: Int)
    let countedLog msg = modifyMVar_ var $ \c -> force (succ c) <$
            when (c < maxLogMsgs) (logFun msg)

    runNodesForSeconds loglevel v n seconds countedLog >>= \case
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

    label = "ConsensusNetwork (nodes: " <> show n <> ", seconds: " <> show seconds <> ")"

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
    {-# INLINE _chainwebVersion #-}

emptyConsensusState :: ChainwebVersion -> ConsensusState
emptyConsensusState v = ConsensusState mempty mempty v

sampleConsensusState
    :: Int
        -- ^ node Id
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
    graph = chainGraphAt_ s
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
    { _statBlockCount = round $ ebc * 0.3 -- temporarily, was 0.8
    , _statMaxHeight = round $ ebc * 0.3 -- temporarily, was 0.7
    , _statMinHeight = round $ ebc * 0.09 -- temporarily, was 0.3
    , _statMedHeight = round $ ebc * 0.3 -- temporarily, was 0.5
    , _statAvgHeight = ebc * 0.3 -- temporarily, was 0.5
    }
  where
    ebc = expectedBlockCount v seconds


upperStats :: ChainwebVersion -> Seconds -> Stats
upperStats v seconds = Stats
    { _statBlockCount = round $ ebc * 1.2
    , _statMaxHeight = round $ ebc * 1.2
    , _statMinHeight = round $ ebc * 1.2
    , _statMedHeight = round $ ebc * 1.2
    , _statAvgHeight = ebc * 1.2
    }
  where
    ebc = expectedBlockCount v seconds
