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
module Chainweb.Test.MultiNode
( test
, Seconds(..)
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad

import Data.Aeson
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List
import qualified Data.Text as T
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
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Miner.Test
import Chainweb.NodeId
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Utils
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.DiGraph
import Data.LogMessage

import P2P.Node.Configuration
import P2P.Peer

newtype Seconds = Seconds Natural
    deriving newtype (Show, Eq, Ord, Num, Enum, Integral, Real)

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

-- | The graph for this test is hardcoded to be peterson graph
--
graph :: ChainGraph
graph = petersonChainGraph

-- | block time seconds is set to 4 seconds. Since this test runs over the the
-- loopback device a very short block time should be fine.
--
blockTimeSeconds :: Natural
blockTimeSeconds = 4

-- | Test Configuration for a scaled down Test chainweb.
--
config
    :: Natural
        -- ^ number of nodes
    -> NodeId
        -- ^ NodeId
    -> ChainwebConfiguration
config n nid = defaultChainwebConfiguration
    & set configNodeId nid
        -- Set the node id.

    & set (configP2p . p2pConfigPeer . peerConfigInterface) "127.0.0.1"
        -- Only listen on the loopback device. On Mac OS X this prevents the
        -- firewall dialog form poping up.

    & set (configMiner . configMeanBlockTimeSeconds) (blockTimeSeconds  * n)
        -- The block time for an indiviual miner, such that the overall block
        -- time is 'blockTimeSeconds'.

    & set (configP2p . p2pConfigMaxPeerCount) (n * 2)
        -- We make room for all test peers in peer db.

    & set (configP2p . p2pConfigMaxSessionCount) 4
        -- We set this to a low number in order to keep the network sparse (or
        -- at last no being a clique) and to also limit the number of
        -- port allocations

    & set (configP2p . p2pConfigSessionTimeout) 10
        -- Use short sessions to cover session timeouts and setup logic in the
        -- test.

bootstrapConfig
    :: Natural
        -- ^ number of nodes
    -> ChainwebConfiguration
bootstrapConfig n = config n (NodeId 0)
    & set (configP2p . p2pConfigPeer) (head $ bootstrapPeerConfig Test)

-- -------------------------------------------------------------------------- --
-- Minimal Node Setup that logs conensus state to the given mvar

node
    :: LogLevel
    -> (T.Text -> IO ())
    -> MVar ConsensusState
    -> ChainGraph
    -> ChainwebConfiguration
    -> IO ()
node loglevel write stateVar g conf = withChainweb g conf logfuns $ \cw ->
    runChainweb cw `finally` sample cw
  where
    nid = _configNodeId conf

    logfuns = chainwebLogFunctions loglevel write nid (chainIds_ graph)

    sample cw = modifyMVar_ stateVar $ \state -> force <$>
        sampleConsensusState
            nid
            (view (chainwebCuts . cutsCutDb . cutDbWebBlockHeaderDb) cw)
            (view (chainwebCuts . cutsCutDb) cw)
            state

-- -------------------------------------------------------------------------- --
-- Run Nodes

runNodes
    :: LogLevel
    -> (T.Text -> IO ())
    -> MVar ConsensusState
    -> Natural
        -- ^ number of nodes
    -> IO ()
runNodes loglevel write stateVar n
    = forConcurrently_ [0 .. int n - 1] $ \i -> do
        threadDelay (500000 * int i)
        node loglevel write stateVar graph . conf $ NodeId i
  where
    conf (NodeId 0) = bootstrapConfig n
    conf i = config n i

runNodesForSeconds
    :: LogLevel
        -- ^ Loglevel
    -> Natural
        -- ^ Number of chainweb consensus nodes
    -> Seconds
        -- ^ test duration in seconds
    -> (T.Text -> IO ())
        -- ^ logging backend callback
    -> IO (Maybe Stats)
runNodesForSeconds loglevel n seconds write = do
    stateVar <- newMVar emptyConsensusState
    void $ timeout (int seconds * 1000000) (runNodes loglevel write stateVar n)

    consensusState <- readMVar stateVar
    return (consensusStateSummary consensusState)

-- -------------------------------------------------------------------------- --
-- Test

test :: LogLevel -> Natural -> Seconds -> TestTree
test loglevel n seconds = testCaseSteps label $ \f -> do
    let tastylog = f . T.unpack
#if 1
    let logFun = tastylog
        maxLogMsgs = 60
#else
    -- useful for debugging, requires import of Data.Text.IO.
    let logFun = T.putStrLn
        maxLogMsgs = 1000
#endif

    -- Count log messages and only print the first 60 messages
    var <- newMVar (0 :: Int)
    let countedLog msg = modifyMVar_ var $ \c -> force (succ c) <$
            if c < maxLogMsgs then logFun msg else return ()

    runNodesForSeconds loglevel n seconds countedLog >>= \case
        Nothing -> assertFailure "chainweb didn't make any progress"
        Just stats -> do
            logsCount <- readMVar var
            tastylog $ "Number of logs: " <> sshow logsCount
            tastylog $ "Expected BlockCount: " <> sshow (expectedBlockCount seconds)
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
    l = lowerStats seconds
    u = upperStats seconds

    label = "Chainweb Network (nodes: " <> show n <> ", seconds: " <> show seconds <> ")"

-- -------------------------------------------------------------------------- --
-- Results

data ConsensusState = ConsensusState
    { _stateBlockHashes :: !(HS.HashSet BlockHash)
        -- ^ for short tests this is fine. For larger test runs we should
        -- use HyperLogLog+

    , _stateCutMap :: !(HM.HashMap NodeId Cut)
    }
    deriving (Show, Generic, NFData)

emptyConsensusState
    :: ConsensusState
emptyConsensusState = ConsensusState mempty mempty

sampleConsensusState :: NodeId -> WebBlockHeaderDb -> CutDb -> ConsensusState -> IO ConsensusState
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
    hashCount = HS.size (_stateBlockHashes s) - int (order graph)

    avg :: Foldable f => Real a => f a -> Double
    avg f = realToFrac (sum $ toList f) / realToFrac (length f)

    median :: Foldable f => Ord a => f a -> a
    median f = (!! ((length f + 1) `div` 2)) $ sort (toList f)

    minHeight = minimum $ HM.elems cutHeights
    maxHeight = maximum $ HM.elems cutHeights
    avgHeight = avg $ HM.elems cutHeights
    medHeight = median $ HM.elems cutHeights

expectedBlockCount :: Seconds -> Natural
expectedBlockCount seconds = round ebc
  where
    ebc :: Double
    ebc = int seconds * int (order graph) / int blockTimeSeconds

lowerStats :: Seconds -> Stats
lowerStats seconds = Stats
    { _statBlockCount = round $ ebc * 0.8
    , _statMaxHeight = round $ ebc * 0.7
    , _statMinHeight = round $ ebc * 0.3
    , _statMedHeight = round $ ebc * 0.5
    , _statAvgHeight = ebc * 0.5
    }
  where
    ebc :: Double
    ebc = int seconds * int (order graph) / int blockTimeSeconds

upperStats :: Seconds -> Stats
upperStats seconds = Stats
    { _statBlockCount = round $ ebc * 1.2
    , _statMaxHeight = round $ ebc * 1.2
    , _statMinHeight = round $ ebc * 1.2
    , _statMedHeight = round $ ebc * 1.2
    , _statAvgHeight = ebc * 1.2
    }
  where
    ebc :: Double
    ebc = int seconds * int (order graph) / int blockTimeSeconds

