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


import System.LogLevel
import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import qualified Chainweb.Difficulty (properties)
import qualified Chainweb.HostAddress (properties)
import qualified Chainweb.Test.BlockHeaderDB
import qualified Chainweb.Test.DiGraph
import qualified Chainweb.Test.Mempool.InMem
import qualified Chainweb.Test.Mempool.Socket
import qualified Chainweb.Test.Mempool.Sync
import qualified Chainweb.Test.MultiNode
import qualified Chainweb.Test.Pact.PactExec
import qualified Chainweb.Test.Pact.PactService
import qualified Chainweb.Test.RestAPI
import qualified Chainweb.Test.Roundtrips
import qualified Chainweb.Test.Store.CAS.FS
import qualified Chainweb.Test.Store.Git
import qualified Chainweb.Test.TreeDB.Persistence
import qualified Chainweb.Test.TreeDB.RemoteDB
import qualified Chainweb.Test.TreeDB.Sync
import Chainweb.Test.Utils (RunStyle(..), schedule, testGroupSch)
import qualified Chainweb.Utils.Paging (properties)
import Chainweb.Version

import qualified Data.DiGraph (properties)
import qualified Data.Word.Encoding (properties)

import qualified Network.X509.SelfSigned.Test

import qualified P2P.Node.PeerDB (properties)

main :: IO ()
main = do
  pactSuite <- pactTestSuite -- Tasty.Golden tests nudge this towards being an IO result
  let allTests = testGroup "Chainweb Tests"
                     [ suite
                     , pactSuite
                     , Chainweb.Test.MultiNode.test Warn TestWithTime 10 120 Nothing ]
  defaultMain allTests

pactTestSuite :: IO TestTree
pactTestSuite = do
    pactTests <- Chainweb.Test.Pact.PactExec.tests
    pactServiceTests <- Chainweb.Test.Pact.PactService.tests
    return $ testGroup "Chainweb-Pact Tests"
        [ pactTests
        , pactServiceTests ]

suite :: TestTree
suite = testGroup "ChainwebTests" $ schedule Sequential
    [ testGroupSch "Chainweb Unit Tests"
        [ testGroup "BlockHeaderDb"
            [ Chainweb.Test.BlockHeaderDB.tests
            , Chainweb.Test.TreeDB.RemoteDB.tests
            , Chainweb.Test.TreeDB.Persistence.tests
            , Chainweb.Test.TreeDB.Sync.tests
            ]
        , Chainweb.Test.Store.CAS.FS.tests
        , Chainweb.Test.Store.Git.tests
        , Chainweb.Test.Roundtrips.tests
        , Chainweb.Test.RestAPI.tests
        , Chainweb.Test.DiGraph.tests
        , Chainweb.Test.Mempool.InMem.tests
        , Chainweb.Test.Mempool.Socket.tests
        , Chainweb.Test.Mempool.Sync.tests
--         , Chainweb.Test.Mempool.Websocket.tests
        , testProperties "Chainweb.BlockHeaderDb.RestAPI.Server" Chainweb.Utils.Paging.properties
        , testProperties "Chainweb.HostAddress" Chainweb.HostAddress.properties
        , testProperties "P2P.Node.PeerDB" P2P.Node.PeerDB.properties
        , testProperties "Data.DiGraph" Data.DiGraph.properties
        , testGroup "Network.X05.SelfSigned.Test"
            [ Network.X509.SelfSigned.Test.tests
            ]
        , testProperties "Chainweb.Difficulty" Chainweb.Difficulty.properties
        , testProperties "Data.Word.Encoding" Data.Word.Encoding.properties
        ]
    , testGroupSch "Pact Tests"
        [ Chainweb.Test.Pact.tests
        ]
    , testGroupSch "Slow Tests"
        [ Chainweb.Test.MultiNode.test Warn TestWithTime 10 120 Nothing
        ]
    ]
