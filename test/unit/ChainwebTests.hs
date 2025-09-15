{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

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

-- import qualified Chainweb.Test.BlockHeaderDB.PruneForks (tests)
-- import qualified Chainweb.Test.Mempool.Consensus (tests)
-- import qualified Chainweb.Test.Pact.SPVTest
-- import qualified Chainweb.Test.SPV (tests)
-- import qualified Chainweb.Test.SPV.EventProof (properties)
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.BlockHeader.Genesis qualified (tests)
import Chainweb.Test.BlockHeader.Validation qualified (tests)
import Chainweb.Test.BlockHeaderDB qualified (tests)
import Chainweb.Test.Chainweb.Utils.Paging qualified (properties)
import Chainweb.Test.Cut qualified (properties)
import Chainweb.Test.CutDB qualified (tests)
import Chainweb.Test.Difficulty qualified (properties)
import Chainweb.Test.HostAddress qualified (properties)
import Chainweb.Test.Mempool.InMem qualified (tests)
import Chainweb.Test.Mempool.RestAPI qualified (tests)
import Chainweb.Test.Mempool.Sync qualified (tests)
import Chainweb.Test.MinerReward qualified (tests)
import Chainweb.Test.Mining qualified (tests)
import Chainweb.Test.Misc qualified (tests)
import Chainweb.Test.Pact.CheckpointerTest qualified
import Chainweb.Test.Pact.HyperlanePluginTests qualified
import Chainweb.Test.Pact.PactServiceTest qualified
import Chainweb.Test.Pact.RemotePactTest qualified
import Chainweb.Test.Pact.TransactionExecTest qualified
import Chainweb.Test.Pact.TransactionTests qualified
import Chainweb.Test.Pact4.NoCoinbase qualified
import Chainweb.Test.Pact4.RewardsTest qualified
import Chainweb.Test.Pact4.SQLite qualified
import Chainweb.Test.Pact4.TransactionTests qualified
import Chainweb.Test.Pact4.VerifierPluginTest qualified
import Chainweb.Test.RestAPI qualified (tests)
import Chainweb.Test.Roundtrips qualified (tests)
import Chainweb.Test.Sync.WebBlockHeaderStore qualified (properties)
import Chainweb.Test.TreeDB qualified (properties)
import Chainweb.Test.TreeDB.RemoteDB qualified
import Chainweb.Test.Version qualified (tests)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Test.PQueue qualified (properties)
import Data.Test.Word.Encoding qualified (properties)
import P2P.Test.Node qualified (properties)
import P2P.Test.TaskQueue qualified (properties)
import System.Environment
import System.LogLevel
import Test.Chainweb.SPV.Argument qualified
import Test.Tasty
import Test.Tasty.JsonReporter
import Test.Tasty.QuickCheck

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
            -- , Chainweb.Test.BlockHeaderDB.PruneForks.tests
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
