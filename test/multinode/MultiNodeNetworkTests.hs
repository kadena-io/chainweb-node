{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb tests the involve running full networks with multiple nodes.
--
module Main ( main ) where

import Chainweb.Graph
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import System.IO.Temp
import System.LogLevel
import Test.Tasty
import Test.Tasty.HUnit
import Chainweb.Test.MultiNode
import Chainweb.Version (withVersion)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Numeric.Natural

main :: IO ()
main = defaultMain suite

loglevel :: LogLevel
loglevel = Warn

nodeCount :: Natural
nodeCount = fromIntegral $ unsafePerformIO getNumCapabilities
{-# NOINLINE nodeCount #-}

-- note that because these tests run in parallel they must all use distinct rocksdb and sqlite dirs.
suite :: TestTree
suite = independentSequentialTestGroup "MultiNodeNetworkTests"
    [ testCaseSteps ("ConsensusNetwork - TimedConsensus - " <> show nodeCount <> " nodes - 30 seconds") $ \step ->
        withTempRocksDb "multinode-tests-timedconsensus-petersen-twenty-rocks" $ \rdb ->
        withSystemTempDirectory "multinode-tests-timedconsensus-petersen-twenty-pact" $ \pactDbDir ->
        withVersion (timedConsensusVersion petersenChainGraph twentyChainGraph) $
            Chainweb.Test.MultiNode.test loglevel nodeCount 30 rdb pactDbDir step

    , testCaseSteps ("ConsensusNetwork - TimedConsensus - " <> show nodeCount <> " nodes - 60 seconds - d4k4 upgrade") $ \step ->
        withTempRocksDb "multinode-tests-timedconsensus-twenty-d4k4-rocks" $ \rdb ->
        withSystemTempDirectory "multinode-tests-timedconsensus-twenty-d4k4-pact" $ \pactDbDir ->
        withVersion (timedConsensusVersion twentyChainGraph d4k4ChainGraph) $
            Chainweb.Test.MultiNode.test loglevel nodeCount 60 rdb pactDbDir step

    , testCaseSteps ("ConsensusNetwork - InstantTimedCPM singleChainGraph - " <> show nodeCount <> " nodes - 30 seconds") $ \step ->
        withTempRocksDb "multinode-tests-instantcpm-single-rocks" $ \rdb ->
        withSystemTempDirectory "multinode-tests-instantcpm-single-pact" $ \pactDbDir ->
        withVersion (instantCpmTestVersion singletonChainGraph) $
            Chainweb.Test.MultiNode.test loglevel nodeCount 30 rdb pactDbDir step

    , testCaseSteps ("Replay - InstantTimedCPM - " <> show nodeCount <> " nodes") $ \step ->
        withTempRocksDb "replay-test-instantcpm-pair-rocks" $ \rdb ->
        withSystemTempDirectory "replay-test-instantcpm-pair-pact" $ \pactDbDir ->
        withVersion (instantCpmTestVersion pairChainGraph) $
            Chainweb.Test.MultiNode.replayTest loglevel nodeCount rdb pactDbDir step
    ]
