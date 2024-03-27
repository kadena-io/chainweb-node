{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb Slow Tests
--

module SlowTests ( main ) where

import Chainweb.Graph
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import System.IO.Temp
import System.LogLevel
import Test.Tasty
import Test.Tasty.HUnit
import qualified Chainweb.Test.MultiNode
import qualified Network.X509.SelfSigned.Test

main :: IO ()
main = defaultMain suite

loglevel :: LogLevel
loglevel = Warn

-- note that because these tests run in parallel they must all use distinct rocksdb and sqlite dirs.
suite :: TestTree
suite = independentSequentialTestGroup "ChainwebSlowTests"
    -- [ testCaseSteps "compact-resume" $ \step ->
    --     withTempRocksDb "compact-resume-test-rocks" $ \rdb ->
    --     withSystemTempDirectory "compact-resume-test-pact" $ \pactDbDir -> do
    --     Chainweb.Test.MultiNode.compactAndResumeTest loglevel (fastForkingCpmTestVersion pairChainGraph) 6 rdb pactDbDir step
    -- , testCaseSteps "compact-live-node" $ \step ->
    --     withTempRocksDb "pact-import-test-rocks" $ \rdb ->
    --     withSystemTempDirectory "pact-import-test-pact" $ \pactDbDir -> do
    --     Chainweb.Test.MultiNode.compactLiveNodeTest loglevel (fastForkingCpmTestVersion twentyChainGraph) 1 rdb pactDbDir step
    [ testCaseSteps "ConsensusNetwork - TimedConsensus - 10 nodes - 30 seconds" $ \step ->
        withTempRocksDb "multinode-tests-timedconsensus-peterson-twenty-rocks" $ \rdb ->
        withSystemTempDirectory "multinode-tests-timedconsensus-peterson-twenty-pact" $ \pactDbDir ->
        Chainweb.Test.MultiNode.test loglevel (timedConsensusVersion petersonChainGraph twentyChainGraph) 10 30 rdb pactDbDir step
    , testCaseSteps "ConsensusNetwork - FastTimedCPM pairChainGraph - 10 nodes - 30 seconds" $ \step ->
        withTempRocksDb "multinode-tests-fasttimedcpm-single-rocks" $ \rdb ->
        withSystemTempDirectory "multinode-tests-fasttimedcpm-single-pact" $ \pactDbDir ->
        Chainweb.Test.MultiNode.test loglevel (fastForkingCpmTestVersion singletonChainGraph) 10 30 rdb pactDbDir step
    , testCaseSteps "Replay - FastTimedCPM - 6 nodes" $ \step ->
        withTempRocksDb "replay-test-fasttimedcpm-pair-rocks" $ \rdb ->
        withSystemTempDirectory "replay-test-fasttimedcpm-pair-pact" $ \pactDbDir ->
        Chainweb.Test.MultiNode.replayTest loglevel (fastForkingCpmTestVersion pairChainGraph) 6 rdb pactDbDir step
    , testCaseSteps "pact-import" $ \step ->
        withTempRocksDb "pact-import-test-rocks" $ \rdb ->
        withSystemTempDirectory "pact-import-test-pact" $ \pactDbDir -> do
        Chainweb.Test.MultiNode.pactImportTest loglevel (fastForkingCpmTestVersion twentyChainGraph) 1 rdb pactDbDir step
    , testGroup "Network.X05.SelfSigned.Test"
        [ Network.X509.SelfSigned.Test.tests
        ]
    ]
