{-# LANGUAGE LambdaCase #-}
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

import Control.Exception (try)
import Control.Monad (void)

import Data.List (sortOn)
import Data.Semigroup (Min(..))
import Data.Tree (Tree(..))

import qualified Streaming.Prelude as S

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, ioProperty, testProperty)

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.Utils (SparseTree(..), insertN, toyBlockHeaderDb, withDB)
import Chainweb.TreeDB


tests :: TestTree
tests = testGroup "Unit Tests"
    [ testGroup "Basic Interaction"
      [ testCase "Initialization + Shutdown" $ toyBlockHeaderDb chainId0 >>= closeBlockHeaderDb . snd
      , testProperty "Conversion to/from Tree" treeIso_prop
      ]
    , testGroup "Insertion"
      [ testCase "10 Insertions" insertItems
      , testCase "Reinserting the Genesis Block is a no-op" reinsertGenesis
      , testCase "Reinserting the entire DB is a no-op" reinsertDb
      , testCase "Can't tweak old nodes" cantInsertTweakedNode
      ]
    , testGroup "TreeDb Instance"
      [ testCase "rank filtering" rankFiltering
      ]
    , testGroup "Misc."
      [ testCase "height" correctHeight
      , testCase "copy" copyTest
      ]
    ]

chainId0 :: ChainId
chainId0 = testChainId 0

fromFoldable :: Foldable f => BlockHeaderDb -> f BlockHeader -> IO ()
fromFoldable db = insertStream db . S.each

-- | Property: There must exist an isomorphism between any `Tree BlockHeader`
-- and a `TreeDb`.
--
treeIso_prop :: SparseTree -> Property
treeIso_prop (SparseTree t) = ioProperty $ do
    db <- initBlockHeaderDb . Configuration $ rootLabel t
    fromFoldable db t
    t' <- toTree db
    pure $ normalizeTree t == normalizeTree t'

insertItems :: Assertion
insertItems = withDB chainId0 $ \g db -> insertN 10 g db

normalizeTree :: Ord a => Tree a -> Tree a
normalizeTree n@(Node _ []) = n
normalizeTree (Node r f) = Node r . map normalizeTree $ sortOn rootLabel f

-- | This test represents a critical invariant: that reinserting the genesis block
-- has no effect on the Database. In particular, the persistence function
-- `restore` assumes this to be true, and likewise `persist` will also write
-- the genesis block to file, assuming `restore` will ignore it upon read.
--
reinsertGenesis :: Assertion
reinsertGenesis = withDB chainId0 $ \g db -> do
    insert db g
    l <- S.length_ $ entries db Nothing Nothing Nothing Nothing
    l @?= 1

reinsertDb :: Assertion
reinsertDb = withDB chainId0 $ \g db -> do
    insertN 10 g db
    insertStream db . void $ entries db Nothing Nothing Nothing Nothing

-- | A user should not be able to overwrite past nodes with arbitrary contents.
--
cantInsertTweakedNode :: Assertion
cantInsertTweakedNode = withDB chainId0 $ \g db -> do
    insertN 10 g db
    h <- maxHeader db
    let (Nonce n) = _blockNonce h
    try (insert db $ h { _blockNonce = Nonce $ n + 1 }) >>= \case
        Left (_ :: ValidationFailure) -> pure ()
        Right _ -> assertFailure "Altered the contents of a past node!"

correctHeight :: Assertion
correctHeight = withDB chainId0 $ \g db -> do
    maxRank db >>= \r -> r @?= 0
    insertN 10 g db
    maxRank db >>= \r -> r @?= 10

copyTest :: Assertion
copyTest = withDB chainId0 $ \g db -> do
    db' <- copy db
    maxRank db  >>= \r -> r @?= 0
    maxRank db' >>= \r -> r @?= 0
    insertN 10 g db'
    maxRank db  >>= \r -> r @?= 0
    maxRank db' >>= \r -> r @?= 10
    insertN 20 g db
    maxRank db  >>= \r -> r @?= 20
    maxRank db' >>= \r -> r @?= 10

rankFiltering :: Assertion
rankFiltering = withDB chainId0 $ \g db -> do
    insertN 100 g db
    l <- S.length_ $ entries db Nothing Nothing (Just . MinRank $ Min 90) Nothing
    l @?= 11
