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

module Main ( main, setTestLogLevel ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import System.Environment
import System.LogLevel

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
    (independentSequentialTestGroup, toyChainId, withToyDB, toyVersion)

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
-- import qualified Chainweb.Test.Mempool.Consensus (tests)
import qualified Chainweb.Test.Mempool.InMem (tests)
import qualified Chainweb.Test.Mempool.RestAPI (tests)
import qualified Chainweb.Test.Mempool.Sync (tests)
import qualified Chainweb.Test.MinerReward (tests)
import qualified Chainweb.Test.Mining (tests)
import qualified Chainweb.Test.Misc (tests)
import qualified Chainweb.Test.Pact.CheckpointerTest
import qualified Chainweb.Test.Pact.HyperlanePluginTests
import qualified Chainweb.Test.Pact.PactServiceTest
import qualified Chainweb.Test.Pact.RemotePactTest
-- import qualified Chainweb.Test.Pact.SPVTest
import qualified Chainweb.Test.Pact.TransactionExecTest
import qualified Chainweb.Test.Pact.TransactionTests
import qualified Chainweb.Test.Pact4.NoCoinbase
import qualified Chainweb.Test.Pact4.RewardsTest
import qualified Chainweb.Test.Pact4.SQLite
import qualified Chainweb.Test.Pact4.VerifierPluginTest
import qualified Chainweb.Test.Pact4.TransactionTests
import qualified Chainweb.Test.RestAPI (tests)
import qualified Chainweb.Test.Roundtrips (tests)
-- import qualified Chainweb.Test.SPV (tests)
-- import qualified Chainweb.Test.SPV.EventProof (properties)
import qualified Chainweb.Test.Sync.WebBlockHeaderStore (properties)
import qualified Chainweb.Test.TreeDB (properties)
import qualified Chainweb.Test.TreeDB.RemoteDB
import qualified Chainweb.Test.Version (tests)
import qualified Data.Test.PQueue (properties)
import qualified Data.Test.Word.Encoding (properties)
import qualified P2P.Test.Node (properties)
import qualified P2P.Test.TaskQueue (properties)
import Chainweb.Version (withVersion)
import qualified Test.Chainweb.SPV.Argument

setTestLogLevel :: LogLevel -> IO ()
setTestLogLevel l = setEnv "CHAINWEB_TEST_LOG_LEVEL" (show l)

main :: IO ()
main = do
    withTempRocksDb "chainweb-tests" $ \rdb ->
        runResourceT $ do
            -- (h0, db) <- withToyDB rdb toyChainId
            liftIO
                $ defaultMainWithIngredients (consoleAndJsonReporter : defaultIngredients)
                $ adjustOption adj
                $ testGroup "Chainweb Tests"
                $ suite rdb
                -- : mempoolTestSuite db h0

    where
    adj NoTimeout = Timeout (1_000_000 * 60 * 10) "10m"
    adj x = x

-- mempoolTestSuite :: BlockHeaderDb -> BlockHeader -> TestTree
-- mempoolTestSuite db genesisBlock = testGroup "Mempool Consensus Tests"
--     [withVersion toyVersion $ Chainweb.Test.Mempool.Consensus.tests db genesisBlock]

suite :: RocksDb -> [TestTree]
suite rdb =
    [ testGroup "Chainweb Unit Tests"
        [ testProperties "Chainweb.Test.Cut" (Chainweb.Test.Cut.properties rdb)
        , testGroup "BlockHeaderDb"
            [ Chainweb.Test.BlockHeaderDB.tests rdb
            , Chainweb.Test.TreeDB.RemoteDB.tests
            , Chainweb.Test.BlockHeaderDB.PruneForks.tests rdb
            , testProperties "Chainweb.Test.TreeDB" Chainweb.Test.TreeDB.properties
            ]
        , Chainweb.Test.CutDB.tests rdb
        , Chainweb.Test.Pact.CheckpointerTest.tests
        , Chainweb.Test.Pact.TransactionExecTest.tests rdb
        , Chainweb.Test.Pact.PactServiceTest.tests rdb
        -- TODO: PP
        -- , Chainweb.Test.Pact.SPVTest.tests rdb
        , Chainweb.Test.Pact.RemotePactTest.tests rdb
        , Chainweb.Test.Pact.HyperlanePluginTests.tests rdb
        , Chainweb.Test.Pact.TransactionTests.tests
        , Chainweb.Test.Roundtrips.tests
        , Chainweb.Test.RestAPI.tests rdb
        -- TODO: PP
        -- , testGroup "SPV"
            -- [ Chainweb.Test.SPV.tests rdb
            -- [ Chainweb.Test.Pact4.SPV.tests rdb
        --     , Chainweb.Test.SPV.EventProof.properties
            -- ]
        , testGroup "Pact 4"
            [ Chainweb.Test.Pact4.NoCoinbase.tests
            , Chainweb.Test.Pact4.RewardsTest.tests
            , Chainweb.Test.Pact4.SQLite.tests
            , Chainweb.Test.Pact4.VerifierPluginTest.tests
            , Chainweb.Test.Pact4.TransactionTests.tests
            ]
        , Chainweb.Test.Mempool.InMem.tests
        , Chainweb.Test.Mempool.Sync.tests
        , Chainweb.Test.Mempool.RestAPI.tests
        , Chainweb.Test.Mining.tests rdb
        , Chainweb.Test.MinerReward.tests
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
    , testGroup "Chainweb Payload Provider Unit Tests"
        [ Test.Chainweb.SPV.Argument.tests
        ]
    ]
