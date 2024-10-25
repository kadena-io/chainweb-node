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

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Test.Tasty
import Test.Tasty.JsonReporter
import Test.Tasty.QuickCheck

-- chainweb modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Storage.Table.RocksDB
import Chainweb.Version.Development
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Registry

-- chainweb-test-tools modules

import Chainweb.Test.Utils
    (independentSequentialTestGroup, toyChainId, withToyDB)

-- internal modules

import qualified Chainweb.Test.BlockHeader.Genesis (tests)
import qualified Chainweb.Test.BlockHeader.Validation (tests)
import qualified Chainweb.Test.BlockHeaderDB (tests)
import qualified Chainweb.Test.BlockHeaderDB.PruneForks (tests)
import qualified Chainweb.Test.Chainweb.Utils.Paging (properties)
import qualified Chainweb.Test.Cut (properties)
import qualified Chainweb.Test.CutDB (tests)
import qualified Chainweb.Test.Difficulty (properties)
import qualified Chainweb.Test.HostAddress (properties)
import qualified Chainweb.Test.Mempool.Consensus (tests)
import qualified Chainweb.Test.Mempool.InMem (tests)
import qualified Chainweb.Test.Mempool.RestAPI (tests)
import qualified Chainweb.Test.Mempool.Sync (tests)
import qualified Chainweb.Test.Mining (tests)
import qualified Chainweb.Test.Misc (tests)
import qualified Chainweb.Test.Pact.Checkpointer (tests)
import qualified Chainweb.Test.Pact.DbCacheTest (tests)
import qualified Chainweb.Test.Pact.GrandHash (tests)
import qualified Chainweb.Test.Pact.ModuleCacheOnRestart (tests)
import qualified Chainweb.Test.Pact.NoCoinbase (tests)
import qualified Chainweb.Test.Pact.PactExec (tests)
import qualified Chainweb.Test.Pact.PactMultiChainTest (tests)
import qualified Chainweb.Test.Pact.PactReplay (tests)
import qualified Chainweb.Test.Pact.PactSingleChainTest (tests)
import qualified Chainweb.Test.Pact.RemotePactTest (tests)
import qualified Chainweb.Test.Pact.RewardsTest (tests)
import qualified Chainweb.Test.Pact.SPV (tests)
import qualified Chainweb.Test.Pact.SQLite (tests)
import qualified Chainweb.Test.Pact.TTL (tests)
import qualified Chainweb.Test.Pact.TransactionTests (tests)
import qualified Chainweb.Test.Pact.VerifierPluginTest (tests)
import qualified Chainweb.Test.RestAPI (tests)
import qualified Chainweb.Test.Rosetta (tests)
import qualified Chainweb.Test.Rosetta.RestAPI (tests)
import qualified Chainweb.Test.Roundtrips (tests)
import qualified Chainweb.Test.SPV (tests)
import qualified Chainweb.Test.SPV.EventProof (properties)
import qualified Chainweb.Test.Sync.WebBlockHeaderStore (properties)
import qualified Chainweb.Test.TreeDB (properties)
import qualified Chainweb.Test.TreeDB.RemoteDB
import qualified Chainweb.Test.Version (tests)
import qualified Data.Test.PQueue (properties)
import qualified Data.Test.Word.Encoding (properties)
import qualified P2P.Test.Node (properties)
import qualified P2P.Test.TaskQueue (properties)

main :: IO ()
main = do
    registerVersion RecapDevelopment
    registerVersion Development
    withTempRocksDb "chainweb-tests" $ \rdb ->
        runResourceT $ do
            (h0, db) <- withToyDB rdb toyChainId
            liftIO $ defaultMainWithIngredients (consoleAndJsonReporter : defaultIngredients)
                $ adjustOption adj
                $ testGroup "Chainweb Tests"
                $ pactTestSuite rdb
                : mempoolTestSuite db h0
                : nodeTestSuite rdb
                : suite rdb
  where
    adj NoTimeout = Timeout (1_000_000 * 60 * 10) "10m"
    adj x = x

mempoolTestSuite :: BlockHeaderDb -> BlockHeader -> TestTree
mempoolTestSuite db genesisBlock = testGroup "Mempool Consensus Tests"
    [Chainweb.Test.Mempool.Consensus.tests db genesisBlock]

pactTestSuite :: RocksDb -> TestTree
pactTestSuite rdb = testGroup "Chainweb-Pact Tests"
    [ Chainweb.Test.Pact.PactExec.tests
    , Chainweb.Test.Pact.DbCacheTest.tests
    , Chainweb.Test.Pact.Checkpointer.tests
    , Chainweb.Test.Pact.PactMultiChainTest.tests
    , Chainweb.Test.Pact.VerifierPluginTest.tests
    , Chainweb.Test.Pact.PactSingleChainTest.tests rdb
    , Chainweb.Test.Pact.PactReplay.tests rdb
    , Chainweb.Test.Pact.ModuleCacheOnRestart.tests rdb
    , Chainweb.Test.Pact.TTL.tests rdb
    , Chainweb.Test.Pact.RewardsTest.tests
    , Chainweb.Test.Pact.NoCoinbase.tests
    , Chainweb.Test.Pact.GrandHash.tests
    ]

nodeTestSuite :: RocksDb -> TestTree
nodeTestSuite rdb = independentSequentialTestGroup "Tests starting nodes"
    [ Chainweb.Test.Rosetta.RestAPI.tests rdb
    , Chainweb.Test.Pact.RemotePactTest.tests rdb
    ]

suite :: RocksDb -> [TestTree]
suite rdb =
    [ testGroup "Chainweb Unit Tests"
        [ testProperties "Chainweb.Test.Cut" (Chainweb.Test.Cut.properties rdb)
        , testGroup "BlockHeaderDb"
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
        ]
    ]
