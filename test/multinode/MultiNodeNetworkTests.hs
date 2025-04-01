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
loglevel = Warn

-- note that because these tests run in parallel they must all use distinct rocksdb and sqlite dirs.
suite :: TestTree
suite = independentSequentialTestGroup "MultiNodeNetworkTests"
    [ testCaseSteps "ConsensusNetwork - TimedConsensus - 10 nodes - 30 seconds" $ \step ->
        withTempRocksDb "multinode-tests-timedconsensus-petersen-twenty-rocks" $ \rdb ->
        withSystemTempDirectory "multinode-tests-timedconsensus-petersen-twenty-pact" $ \pactDbDir ->
        Chainweb.Test.MultiNode.test loglevel (timedConsensusVersion petersenChainGraph twentyChainGraph) 10 30 rdb pactDbDir step
    , testCaseSteps "ConsensusNetwork - FastTimedCPM singleChainGraph - 10 nodes - 30 seconds" $ \step ->
        withTempRocksDb "multinode-tests-fasttimedcpm-single-rocks" $ \rdb ->
        withSystemTempDirectory "multinode-tests-fasttimedcpm-single-pact" $ \pactDbDir ->
        Chainweb.Test.MultiNode.test loglevel (fastForkingCpmTestVersion singletonChainGraph) 10 30 rdb pactDbDir step
    , testCaseSteps "Replay - FastTimedCPM - 6 nodes" $ \step ->
        withTempRocksDb "replay-test-fasttimedcpm-pair-rocks" $ \rdb ->
        withSystemTempDirectory "replay-test-fasttimedcpm-pair-pact" $ \pactDbDir ->
        Chainweb.Test.MultiNode.replayTest loglevel (fastForkingCpmTestVersion pairChainGraph) 6 rdb pactDbDir step
    , testCaseSteps "Replay - TransitionTimedCPM - 6 nodes" $ \step ->
        withTempRocksDb "replay-test-transitiontimedcpm-pair-rocks" $ \rdb ->
        withSystemTempDirectory "replay-test-transitiontimedcpm-pair-pact" $ \pactDbDir ->
        Chainweb.Test.MultiNode.replayTest loglevel (instantCpmTransitionTestVersion pairChainGraph) 1 rdb pactDbDir step
    ]
