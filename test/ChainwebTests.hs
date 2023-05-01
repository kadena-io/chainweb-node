{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb Test Suite
--

module Main ( main ) where

import Test.Tasty
import Test.Tasty.JsonReporter
import Test.Tasty.QuickCheck

-- internal modules
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import qualified Chainweb.Test.Difficulty (properties)
import qualified Chainweb.Test.BlockHeader.Genesis
import qualified Chainweb.Test.BlockHeader.Validation
import qualified Chainweb.Test.BlockHeaderDB
import qualified Chainweb.Test.BlockHeaderDB.PruneForks (tests)
import qualified Chainweb.Test.Cut (properties)
import qualified Chainweb.Test.CutDB
import qualified Chainweb.Test.HostAddress (properties)
import qualified Chainweb.Test.Mempool.Consensus
import qualified Chainweb.Test.Mempool.InMem
import qualified Chainweb.Test.Mempool.RestAPI
import qualified Chainweb.Test.Mempool.Sync
import qualified Chainweb.Test.Mining (tests)
import qualified Chainweb.Test.Misc
import qualified Chainweb.Test.Pact.DbCacheTest
import qualified Chainweb.Test.Pact.Compaction (tests)
import qualified Chainweb.Test.Pact.Checkpointer
import qualified Chainweb.Test.Pact.ModuleCacheOnRestart
import qualified Chainweb.Test.Pact.NoCoinbase
import qualified Chainweb.Test.Pact.PactExec
import qualified Chainweb.Test.Pact.PactMultiChainTest
import qualified Chainweb.Test.Pact.PactSingleChainTest
import qualified Chainweb.Test.Pact.PactReplay
import qualified Chainweb.Test.Pact.RemotePactTest
import qualified Chainweb.Test.Pact.RewardsTest
import qualified Chainweb.Test.Pact.SQLite
import qualified Chainweb.Test.Pact.SPV
import qualified Chainweb.Test.Pact.TransactionTests
import qualified Chainweb.Test.Pact.TTL
import qualified Chainweb.Test.RestAPI
import qualified Chainweb.Test.Rosetta
import qualified Chainweb.Test.Rosetta.RestAPI
import qualified Chainweb.Test.Roundtrips
import qualified Chainweb.Test.SPV
import qualified Chainweb.Test.SPV.EventProof
import qualified Chainweb.Test.Sync.WebBlockHeaderStore (properties)
import qualified Chainweb.Test.TreeDB (properties)
import qualified Chainweb.Test.TreeDB.RemoteDB
import Chainweb.Test.Utils
    (RunStyle(..), ScheduledTest(..), schedule, testGroupSch, toyChainId,
    withToyDB)
import qualified Chainweb.Test.Version (tests)
import qualified Chainweb.Test.Chainweb.Utils.Paging (properties)

import Chainweb.Storage.Table.RocksDB

import qualified Data.Test.PQueue (properties)
import qualified Data.Test.Word.Encoding (properties)

import qualified P2P.Test.TaskQueue (properties)
import qualified P2P.Test.Node (properties)

main :: IO ()
main =
    withTempRocksDb "chainweb-tests" $ \rdb ->
    withToyDB rdb toyChainId $ \h0 db ->
        defaultMainWithIngredients (consoleAndJsonReporter : defaultIngredients)
            $ adjustOption adj
            $ testGroup "Chainweb Tests" . schedule Sequential
            $ pactTestSuite rdb
            : mempoolTestSuite db h0
            : rosettaTestSuite rdb
            : suite rdb
  where
    adj NoTimeout = Timeout (1_000_000 * 60 * 10) "10m"
    adj x = x

mempoolTestSuite :: BlockHeaderDb -> BlockHeader -> ScheduledTest
mempoolTestSuite db genesisBlock = testGroupSch "Mempool Consensus Tests"
    $ schedule Sequential [Chainweb.Test.Mempool.Consensus.tests db genesisBlock]

pactTestSuite :: RocksDb -> ScheduledTest
pactTestSuite rdb = testGroupSch "Chainweb-Pact Tests"
    $ schedule Sequential
        [ Chainweb.Test.Pact.PactExec.tests
        , ScheduledTest "DbCacheTests" Chainweb.Test.Pact.DbCacheTest.tests
        , Chainweb.Test.Pact.Compaction.tests
        , Chainweb.Test.Pact.Checkpointer.tests
        , Chainweb.Test.Pact.PactMultiChainTest.tests
        , Chainweb.Test.Pact.PactSingleChainTest.tests rdb
        , Chainweb.Test.Pact.RemotePactTest.tests rdb
        , Chainweb.Test.Pact.PactReplay.tests rdb
        , Chainweb.Test.Pact.ModuleCacheOnRestart.tests rdb
        , Chainweb.Test.Pact.TTL.tests rdb
        , Chainweb.Test.Pact.RewardsTest.tests
        , Chainweb.Test.Pact.NoCoinbase.tests
        ]

rosettaTestSuite :: RocksDb -> ScheduledTest
rosettaTestSuite rdb = testGroupSch "Chainweb-Rosetta API Tests" $ schedule Sequential
    [ Chainweb.Test.Rosetta.RestAPI.tests rdb
    ]

suite :: RocksDb -> [ScheduledTest]
suite rdb =
    [ testGroupSch "Chainweb Unit Tests"
        [ testGroup "BlockHeaderDb"
            [ Chainweb.Test.BlockHeaderDB.tests rdb
            , Chainweb.Test.TreeDB.RemoteDB.tests
            , Chainweb.Test.BlockHeaderDB.PruneForks.tests
            , testProperties "Chainweb.Test.TreeDB" Chainweb.Test.TreeDB.properties
            ]
        , Chainweb.Test.Pact.SQLite.tests
        , Chainweb.Test.CutDB.tests rdb
        , Chainweb.Test.Pact.TransactionTests.tests
        , Chainweb.Test.Roundtrips.tests
        , Chainweb.Test.Rosetta.tests
        , Chainweb.Test.RestAPI.tests rdb
        , testGroup "SPV"
            [ Chainweb.Test.SPV.tests rdb
            , Chainweb.Test.Pact.SPV.tests
            , Chainweb.Test.SPV.EventProof.properties
            ]
        , Chainweb.Test.Mempool.InMem.tests
        , Chainweb.Test.Mempool.Sync.tests
        , Chainweb.Test.Mempool.RestAPI.tests
        , Chainweb.Test.Mining.tests rdb
        , Chainweb.Test.Misc.tests
        , Chainweb.Test.BlockHeader.Genesis.tests
        , Chainweb.Test.BlockHeader.Validation.tests
        , Chainweb.Test.Version.tests
        , testProperties "Chainweb.Test.Chainweb.Utils.Paging" Chainweb.Test.Chainweb.Utils.Paging.properties
        , testProperties "Chainweb.Test.HostAddress" Chainweb.Test.HostAddress.properties
        , testProperties "Chainweb.Test.Sync.WebBlockHeaderStore" Chainweb.Test.Sync.WebBlockHeaderStore.properties
        , testProperties "P2P.Test.TaskQueue" P2P.Test.TaskQueue.properties
        , testProperties "P2P.Test.Node" P2P.Test.Node.properties
        , testProperties "Data.Test.PQueue" Data.Test.PQueue.properties
        , testProperties "Chainweb.Test.Difficulty" Chainweb.Test.Difficulty.properties
        , testProperties "Data.Test.Word.Encoding" Data.Test.Word.Encoding.properties
        , testProperties "Chainweb.Test.Cut" (Chainweb.Test.Cut.properties rdb)
        ]
    ]
