{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

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
import qualified Chainweb.RestAPI.Utils (properties)
import qualified Chainweb.Test.ChainDB
import qualified Chainweb.Test.ChainDB.Persistence
import qualified Chainweb.Test.ChainDB.Sync
import qualified Chainweb.Test.RestAPI
import qualified Chainweb.Test.Roundtrips

import qualified P2P.Node.PeerDB (properties)

import qualified Data.DiGraph (properties)

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
    [ testGroup "ChainDB"
        [ Chainweb.Test.ChainDB.tests
        , Chainweb.Test.ChainDB.Persistence.tests
        , Chainweb.Test.ChainDB.Sync.tests
        ]
    , Chainweb.Test.Roundtrips.tests
    , Chainweb.Test.RestAPI.tests
    , testProperties "Chainweb.ChainDB.RestAPI.Server" Chainweb.RestAPI.Utils.properties
    , testProperties "Chainweb.HostAddress" Chainweb.HostAddress.properties
    , testProperties "P2P.Node.PeerDB" P2P.Node.PeerDB.properties
    , testProperties "Data.DiGraph" Data.DiGraph.properties
    ]

