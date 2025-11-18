{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import Chainweb.Test.MultiNode
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Version (withVersion)
import GHC.Conc (getNumProcessors)
import Numeric.Natural
import System.Environment
import System.IO.Temp
import System.IO.Unsafe (unsafePerformIO)
import System.LogLevel
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read (readMaybe)

main :: IO ()
main = defaultMain suite

loglevel :: LogLevel
loglevel = Warn

-- | NOTE: This is the number of /physical/ processors. This may be different
-- from the number of CPU cores that are avaialble to the process, for instance
-- in a containerized environment.
--
-- A more appropriate value can be obtained via querying cgroup settings (cf.
-- the cgroup-rts-threads package for an implementation of this). However, for
-- the purpose of these tests this should be rarly needed. Almost always one
-- should run these tests with all available processors.
--
nodeCount :: Natural
nodeCount = unsafePerformIO $ do
    lookupEnv "CHAINWEB_MULTINODE_TESTS_NODE_COUNT" >>= \case
        Just s -> case readMaybe s of
            Just i -> return i
            Nothing -> error $ "Invalid CHAINWEB_MULTINODE_TESTS_NODE_COUNT value: " <> s
        Nothing -> fromIntegral <$> getNumProcessors
{-# NOINLINE nodeCount #-}

-- |
--
-- Note that because these tests run in parallel they must all use distinct
-- rocksdb and sqlite dirs.
--
-- Note that it seems that these tsets don't mind that much running in parallel.
-- However, they do mind if the number of nodes in each of these tests execeeds
-- the number of available capabilities.
--
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
            Chainweb.Test.MultiNode.test loglevel nodeCount 30 rdb pactDbDir step

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
