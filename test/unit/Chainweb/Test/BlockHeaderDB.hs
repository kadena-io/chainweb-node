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

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Foldable
import Data.Semigroup (Min(..))

import qualified Streaming.Prelude as S

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.Test.TreeDB (treeDbInvariants)
import Chainweb.Test.Utils
import Chainweb.TreeDB

import Chainweb.Storage.Table.RocksDB

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
    , treeDbInvariants
        (\x f -> runResourceT $ do
          db <- withTestBlockHeaderDb rdb x
          liftIO $ f db (traverse_ . unsafeInsertBlockHeaderDb))
        testGroup
    ]

insertItems :: RocksDb -> Assertion
insertItems rdb = runResourceT $ do
  (g, db) <- withToyDB rdb toyChainId
  liftIO $ insertN 10 g db

correctHeight :: RocksDb -> Assertion
correctHeight rdb = runResourceT $ do
    (g, db) <- withToyDB rdb toyChainId
    liftIO $ maxRank db >>= \r -> r @?= 0
    liftIO $ insertN 10 g db
    liftIO $ maxRank db >>= \r -> r @?= 10

rankFiltering :: RocksDb -> Assertion
rankFiltering rdb = runResourceT $ do
    (g, db) <- withToyDB rdb toyChainId
    liftIO $ insertN 100 g db
    l <- liftIO $ entries db Nothing Nothing (Just . MinRank $ Min 90) Nothing $ S.length_
    liftIO $ l @?= 11

