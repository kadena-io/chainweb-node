{-# LANGUAGE CPP #-}
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

import qualified Chainweb.HostAddress (properties)
import qualified Chainweb.Test.BlockHeaderDB
import qualified Chainweb.Test.DiGraph
import qualified Chainweb.Test.RestAPI
import qualified Chainweb.Test.Roundtrips
import qualified Chainweb.Test.TreeDB.Persistence
import qualified Chainweb.Test.TreeDB.RemoteDB
import qualified Chainweb.Test.TreeDB.Sync
import qualified Chainweb.Utils.Paging (properties)
#ifdef WITH_PACT
import qualified Chainweb.Test.Pact
#endif

import qualified Data.DiGraph (properties)

--import qualified Network.X509.SelfSigned.Test

import qualified P2P.Node.PeerDB (properties)

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
    [ testGroup "BlockHeaderDb"
        [Chainweb.Test.Pact.tests]
    ]
{-
    [ testGroup "BlockHeaderDb"
        [ Chainweb.Test.BlockHeaderDB.tests
        , Chainweb.Test.TreeDB.RemoteDB.tests
        , Chainweb.Test.TreeDB.Persistence.tests
        , Chainweb.Test.TreeDB.Sync.tests
        ]
-}
#ifdef WITH_PACT
    -- , Chainweb.Test.Pact.tests
#endif
{-
    , Chainweb.Test.Roundtrips.tests
    , Chainweb.Test.RestAPI.tests
    , Chainweb.Test.DiGraph.tests
    , testProperties "Chainweb.BlockHeaderDb.RestAPI.Server" Chainweb.Utils.Paging.properties
    , testProperties "Chainweb.HostAddress" Chainweb.HostAddress.properties
    , testProperties "P2P.Node.PeerDB" P2P.Node.PeerDB.properties
    , testProperties "Data.DiGraph" Data.DiGraph.properties
    -- DB 2019-01-17 Disabling to fix CI
    -- , testGroup "Network.X05.SelfSigned.Test"
    --     [ Network.X509.SelfSigned.Test.tests
    --     ]
    ]
-}
