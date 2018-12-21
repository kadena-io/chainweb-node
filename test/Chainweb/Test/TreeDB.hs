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
        [ testProperty "Conversion to/from Tree" $ treeIso_prop f
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
