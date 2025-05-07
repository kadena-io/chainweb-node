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
import qualified Chainweb.Test.MultiNode

main :: IO ()
main = defaultMain suite

loglevel :: LogLevel
loglevel = Debug

-- note that because these tests run in parallel they must all use distinct rocksdb and sqlite dirs.
suite :: TestTree
suite = independentSequentialTestGroup "MultiNodeNetworkTests"
    [ testCaseSteps "ConsensusNetwork - TimedConsensus - 10 nodes - 30 seconds" $ \step ->
        withTempRocksDb "multinode-tests-timedconsensus-petersen-twenty-rocks" $ \rdb ->
        withSystemTempDirectory "multinode-tests-timedconsensus-petersen-twenty-pact" $ \pactDbDir ->
        Chainweb.Test.MultiNode.test loglevel (timedConsensusVersion petersenChainGraph twentyChainGraph) 10 30 rdb pactDbDir step
    -- , testCaseSteps "ConsensusNetwork - InstantTimedCPM singleChainGraph - 10 nodes - 30 seconds" $ \step ->
    --     withTempRocksDb "multinode-tests-instantcpm-single-rocks" $ \rdb ->
    --     withSystemTempDirectory "multinode-tests-instantcpm-single-pact" $ \pactDbDir ->
    --     Chainweb.Test.MultiNode.test loglevel (instantCpmTestVersion singletonChainGraph) 10 30 rdb pactDbDir step
    -- , testCaseSteps "Replay - InstantTimedCPM - 6 nodes" $ \step ->
    --     withTempRocksDb "replay-test-instantcpm-pair-rocks" $ \rdb ->
    --     withSystemTempDirectory "replay-test-instantcpm-pair-pact" $ \pactDbDir ->
    --     Chainweb.Test.MultiNode.replayTest loglevel (instantCpmTestVersion pairChainGraph) 6 rdb pactDbDir step
    ]
