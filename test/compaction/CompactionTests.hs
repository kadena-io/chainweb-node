-- |
-- Module: Main
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Tests for expensive data maintenance operations, most notably compaction.
--
module Main
( main
) where

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
suite = independentSequentialTestGroup "CompactionTests"
    [ testCaseSteps "compact-resume" $ \step ->
        withTempRocksDb "compact-resume-test-rocks-src" $ \srcRocksDb ->
        withTempRocksDb "compact-resume-test-rocks-target" $ \targetRocksDb ->
        withSystemTempDirectory "compact-resume-test-pact-src" $ \srcPactDbDir ->
        withSystemTempDirectory "compact-resume-test-pact-target" $ \targetPactDbDir -> do
        Chainweb.Test.MultiNode.compactAndResumeTest loglevel (fastForkingCpmTestVersion pairChainGraph) 6 srcRocksDb targetRocksDb srcPactDbDir targetPactDbDir step
    , testCaseSteps "compact-live-node" $ \step ->
        withTempRocksDb "pact-import-test-rocks" $ \rdb ->
        withSystemTempDirectory "pact-import-test-pact-src" $ \srcPactDbDir ->
        withSystemTempDirectory "pact-import-test-pact-target" $ \targetPactDbDir -> do
        Chainweb.Test.MultiNode.compactLiveNodeTest loglevel (fastForkingCpmTestVersion twentyChainGraph) 1 rdb srcPactDbDir targetPactDbDir step
    , testCaseSteps "pact-import" $ \step ->
        withTempRocksDb "pact-import-test-rocks" $ \rdb ->
        withSystemTempDirectory "pact-import-test-pact" $ \pactDbDir -> do
        Chainweb.Test.MultiNode.pactImportTest loglevel (fastForkingCpmTestVersion twentyChainGraph) 1 rdb pactDbDir step
    ]
