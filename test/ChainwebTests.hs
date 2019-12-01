{-# LANGUAGE DataKinds #-}
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
import Test.Tasty.QuickCheck

-- internal modules
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import qualified Chainweb.Cut.Test (properties)
import qualified Chainweb.Difficulty (properties)
import qualified Chainweb.HostAddress (properties)
import qualified Chainweb.Sync.WebBlockHeaderStore.Test (properties)
import qualified Chainweb.Test.BlockHeader.Genesis
import qualified Chainweb.Test.BlockHeaderDB
import qualified Chainweb.Test.CoinContract
import qualified Chainweb.Test.Mempool.Consensus
import qualified Chainweb.Test.Mempool.InMem
import qualified Chainweb.Test.Mempool.RestAPI
import qualified Chainweb.Test.Mempool.Sync
import qualified Chainweb.Test.Miner.Core
import qualified Chainweb.Test.Misc
import qualified Chainweb.Test.Pact.ChainData
import qualified Chainweb.Test.Pact.Checkpointer
import qualified Chainweb.Test.Pact.PactExec
import qualified Chainweb.Test.Pact.PactInProcApi
import qualified Chainweb.Test.Pact.PactReplay
import qualified Chainweb.Test.Pact.RemotePactTest
import qualified Chainweb.Test.Pact.RewardsTest
import qualified Chainweb.Test.Pact.SPV
import qualified Chainweb.Test.Pact.TTL
import qualified Chainweb.Test.RestAPI
import qualified Chainweb.Test.Roundtrips
import qualified Chainweb.Test.SPV
import qualified Chainweb.Test.Store.CAS.FS
import qualified Chainweb.Test.TreeDB.Persistence
import qualified Chainweb.Test.TreeDB.RemoteDB
import Chainweb.Test.Utils
    (RunStyle(..), ScheduledTest, schedule, testGroupSch, toyChainId,
    withToyDB)
import qualified Chainweb.TreeDB (properties)
import qualified Chainweb.Utils.Paging (properties)

import Data.CAS.RocksDB
import qualified Data.PQueue.Test (properties)
import qualified Data.Word.Encoding (properties)

import qualified P2P.Node.PeerDB (properties)
import qualified P2P.TaskQueue.Test (properties)

main :: IO ()
main =
    withTempRocksDb "chainweb-tests" $ \rdb ->
    withToyDB rdb toyChainId $ \h0 db -> do
        defaultMain
            $ adjustOption adj
            $ testGroup "Chainweb Tests" . schedule Sequential
            $ pactTestSuite rdb
            : mempoolTestSuite db h0
            : suite rdb
  where
    adj NoTimeout = Timeout (1000000 * 60 * 10) "10m"
    adj x = x

mempoolTestSuite :: BlockHeaderDb -> BlockHeader -> ScheduledTest
mempoolTestSuite db genesisBlock = testGroupSch "Mempool Consensus Tests"
    $ schedule Sequential [Chainweb.Test.Mempool.Consensus.tests db genesisBlock]

pactTestSuite :: RocksDb -> ScheduledTest
pactTestSuite rdb = testGroupSch "Chainweb-Pact Tests"
    $ schedule Sequential
        [ Chainweb.Test.Pact.PactExec.tests
        , Chainweb.Test.Pact.Checkpointer.tests
        , Chainweb.Test.Pact.PactInProcApi.tests
        , Chainweb.Test.Pact.RemotePactTest.tests rdb
        , Chainweb.Test.Pact.PactReplay.tests
        , Chainweb.Test.Pact.ChainData.tests
        , Chainweb.Test.Pact.TTL.tests
        , Chainweb.Test.Pact.RewardsTest.tests
        ]

suite :: RocksDb -> [ScheduledTest]
suite rdb =
    [ testGroupSch "Chainweb Unit Tests"
        [ testGroup "BlockHeaderDb"
            [ Chainweb.Test.BlockHeaderDB.tests rdb
            , Chainweb.Test.TreeDB.RemoteDB.tests
            , Chainweb.Test.TreeDB.Persistence.tests rdb
            , testProperties "Chainweb.TreeDB" Chainweb.TreeDB.properties
            ]
        , Chainweb.Test.CoinContract.tests
        , Chainweb.Test.Store.CAS.FS.tests
        , Chainweb.Test.Roundtrips.tests
        , Chainweb.Test.RestAPI.tests rdb
        , Chainweb.Test.SPV.tests rdb
        , Chainweb.Test.Pact.SPV.tests
        , Chainweb.Test.Mempool.InMem.tests
        , Chainweb.Test.Mempool.Sync.tests
        , Chainweb.Test.Mempool.RestAPI.tests
        , Chainweb.Test.Miner.Core.tests
        , Chainweb.Test.Misc.tests
        , Chainweb.Test.BlockHeader.Genesis.tests
        , testProperties "Chainweb.BlockHeaderDb.RestAPI.Server" Chainweb.Utils.Paging.properties
        , testProperties "Chainweb.HostAddress" Chainweb.HostAddress.properties
        , testProperties "Chainweb.Sync.WebBlockHeaderStore.Test" Chainweb.Sync.WebBlockHeaderStore.Test.properties
        , testProperties "P2P.Node.PeerDB" P2P.Node.PeerDB.properties
        , testProperties "P2P.TaskQueue.Test" P2P.TaskQueue.Test.properties
        , testProperties "Data.PQueue.Test" Data.PQueue.Test.properties
        , testProperties "Chainweb.Difficulty" Chainweb.Difficulty.properties
        , testProperties "Data.Word.Encoding" Data.Word.Encoding.properties
        , testProperties "Chainweb.Cut.Test" (Chainweb.Cut.Test.properties rdb)
        ]
    ]
