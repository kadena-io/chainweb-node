{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.TreeDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Test the invariants of the `TreeDb` typeclass.
--
module Chainweb.Test.TreeDB ( treeDbInvariants ) where

import Control.Exception (SomeException(..), try)
import Control.Lens (to, (%~), (&), (^.))

import Data.Generics.Wrapped (_Unwrapped)
import Data.List (sort, sortOn)
import Data.Tree (Tree(..))

import qualified Streaming.Prelude as S

import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Test.Utils
import Chainweb.TreeDB

treeDbInvariants :: (TreeDb db, DbEntry db ~ BlockHeader) => (DbEntry db -> IO db) -> TestTree
treeDbInvariants f = testGroup "TreeDb Invariants"
    [ testGroup "Properties"
        [ testGroup "Shape"
            [ testProperty "Conversion to/from Tree" $ treeIso_prop f
            , testProperty "Root node is its own parent" $ rootParent_prop f
            ]
        , testGroup "Behaviour"
            [ testProperty "Reinsertion is a no-op" $ reinsertion_prop f
            , testProperty "Can't manipulate old nodes" $ handOfGod_prop f
            , testProperty "All leaves are properly fetched" $ leafFetch_prop f
            , testProperty "Leaves are streamed in ascending order" $ leafOrder_prop f
            , testProperty "maxRank reports correct height" $ maxRank_prop f
            ]
        ]
    ]

fromFoldable :: (TreeDb db, Foldable f) => db -> f (DbEntry db) -> IO ()
fromFoldable db = insertStream db . S.each

-- | Property: There must exist an isomorphism between any `Tree BlockHeader`
-- and a `TreeDb`.
--
treeIso_prop
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => (BlockHeader -> IO db) -> SparseTree -> Property
treeIso_prop f (SparseTree t) = ioProperty $ do
    db <- f $ rootLabel t
    fromFoldable db t
    t' <- toTree db
    pure $ normalizeTree t == normalizeTree t'

-- | Property: Reinserting any amount of `BlockHeader`s that already exist in
-- the `TreeDb` must have no effect (no overwrites, no exceptions, etc.)
--
-- In particular, the persistence function `restore` assumes this property to be
-- true, and likewise `persist` will also write the genesis block to file,
-- assuming `restore` will ignore it upon read.
--
reinsertion_prop
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => (BlockHeader -> IO db) -> SparseTree -> Property
reinsertion_prop f (SparseTree t) = ioProperty $ do
    db <- f $ rootLabel t
    fromFoldable db t
    fromFoldable db t
    l <- S.length_ $ entries db Nothing Nothing Nothing Nothing
    pure $ l == length t

-- | Property: It must be impossible to fetch an existing header, alter its
-- contents, and reinsert it into the Tree.
--
-- Even if no exception is thrown due to validation failure, the "rewritten"
-- block should not have actually changed.
--
handOfGod_prop
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => (BlockHeader -> IO db) -> SparseTree -> Property
handOfGod_prop f (SparseTree t) = ioProperty $ do
    db <- f $ rootLabel t
    fromFoldable db t
    h <- maxHeader db
    try (insert db $ (h & blockNonce . _Unwrapped %~ succ)) >>= \case
        Left (_ :: SomeException) -> pure True
        Right _ -> do
            h' <- maxHeader db
            pure $ h == h'

-- | Property: The root node's parent must always be itself.
--
rootParent_prop
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => (BlockHeader -> IO db) -> SparseTree -> Property
rootParent_prop f (SparseTree t) = ioProperty $ do
    db <- f $ rootLabel t
    fromFoldable db t
    r <- root db
    pure $ _blockParent r == _blockHash r

-- | Property: A `TreeDb` must be able to yield all of its leaves properly.
--
leafFetch_prop
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => (BlockHeader -> IO db) -> SparseTree -> Property
leafFetch_prop f (SparseTree t) = ioProperty $ do
    db <- f $ rootLabel t
    fromFoldable db t
    ls <- fmap sort . S.toList_ $ leafEntries db Nothing Nothing Nothing Nothing
    pure $ ls == sort (treeLeaves t)

-- | Property: `leafEntries` streams leaves in ascending order of `BlockHeight`.
--
leafOrder_prop
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => (BlockHeader -> IO db) -> SparseTree -> Property
leafOrder_prop f (SparseTree t) = ioProperty $ do
    db <- f $ rootLabel t
    fromFoldable db t
    ls <- S.toList_ $ leafEntries db Nothing Nothing Nothing Nothing
    pure $ ls == sortOn _blockHeight ls

-- | Property: `maxRank` correctly reports the `BlockHeight` of the highest node
-- in the Tree.
--
maxRank_prop
    :: (TreeDb db, DbEntry db ~ BlockHeader)
    => (BlockHeader -> IO db) -> SparseTree -> Property
maxRank_prop f (SparseTree t) = ioProperty $ do
    db <- f $ rootLabel t
    fromFoldable db t
    r <- maxRank db
    let h = (^. _Unwrapped . to fromIntegral) . maximum . map _blockHeight $ treeLeaves t
    pure $ r == h
