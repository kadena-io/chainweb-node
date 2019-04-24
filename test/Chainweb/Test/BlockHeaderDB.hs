{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Test the `BlockHeaderDb` API.
--
module Chainweb.Test.BlockHeaderDB
( tests
) where

import Data.Semigroup (Min(..))

import qualified Streaming.Prelude as S

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.Test.TreeDB (RunStyle(..), treeDbInvariants)
import Chainweb.Test.Utils (insertN, toyBlockHeaderDb, withToyDB, toyChainId, withTestBlockHeaderDb)
import Chainweb.TreeDB

import Data.CAS.RocksDB

tests :: RocksDb -> TestTree
tests rdb = testGroup "Unit Tests"
    [ testGroup "Basic Interaction"
      [ testCase "Initialization + Shutdown" $ toyBlockHeaderDb rdb toyChainId >>= closeBlockHeaderDb . snd
      ]
    , testGroup "Insertion"
      [ testCase "10 Insertions" $ insertItems rdb
      ]
    , testGroup "TreeDb Instance"
      [ testCase "rank filtering" $ rankFiltering rdb
      ]
    , testGroup "Misc."
      [ testCase "height" $ correctHeight rdb
      ]
    , treeDbInvariants (withTestBlockHeaderDb rdb) Parallel
    ]

insertItems :: RocksDb -> Assertion
insertItems rdb = withToyDB rdb toyChainId $ \g db -> insertN 10 g db

correctHeight :: RocksDb -> Assertion
correctHeight rdb = withToyDB rdb toyChainId $ \g db -> do
    maxRank db >>= \r -> r @?= 0
    insertN 10 g db
    maxRank db >>= \r -> r @?= 10

rankFiltering :: RocksDb -> Assertion
rankFiltering rdb = withToyDB rdb toyChainId $ \g db -> do
    insertN 100 g db
    l <- entries db Nothing Nothing (Just . MinRank $ Min 90) Nothing $ S.length_
    l @?= 11

