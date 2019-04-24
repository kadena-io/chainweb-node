{-# LANGUAGE FlexibleContexts #-}
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
module Chainweb.Test.TreeDB ( treeDbInvariants, RunStyle(..) ) where

import Control.Exception (SomeException(..), try)
import Control.Lens (each, from, over, to, (^.), (^..))

import Data.Bool (bool)
import Data.Foldable (foldlM)
import qualified Data.HashSet as HS
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import Data.Tree (Tree(..))

import Numeric.Natural (Natural)

import Prelude hiding (lookup)

import Streaming (Of(..), Stream)
import qualified Streaming.Prelude as P

import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.Test.Utils
import Chainweb.TreeDB
import Chainweb.Utils (len)

treeDbInvariants
    :: (TreeDb db, IsBlockHeader (DbEntry db), Ord (DbEntry db), Ord (DbKey db))
    -- | Given a generic entry, should yield a database for testing, and then
    -- safely close it after use.
    => (DbEntry db -> (db -> IO Bool) -> IO Bool)
    -> RunStyle
    -> TestTree
treeDbInvariants f rs = testGroup "TreeDb Invariants"
    [ testGroup "Properties" $ schedule rs
        [ testGroupSch "TreeDb Shape" $ schedule rs
            [ testPropertySch "Conversion to and from Tree" $ treeIso_prop f
            , testPropertySch "Root node has genesis parent hash" $ rootParent_prop f
            ]
        , testGroupSch "Basic Streaming" $ schedule rs
              [ testGroupSch "Self-reported Stream Length" $ schedule rs
                    [ testPropertySch "streaming keys"
                          $ streamCount_prop f (\db -> keys db Nothing Nothing Nothing Nothing)
                    , testPropertySch "streaming entries"
                          $ streamCount_prop f (\db -> entries db Nothing Nothing Nothing Nothing)
                    , testPropertySch "streaming branchKeys"
                          $ streamCount_prop f (\db -> branches branchKeys db)
                    , testPropertySch "streaming branchEntries"
                          $ streamCount_prop f (\db -> branches branchEntries db)
                    ]
              , testGroupSch "Miscellaneous" $ schedule rs
                    [ testPropertySch "Parent lookup of genesis fails" $ genParent_prop f
                    , testPropertySch "All entries are properly fetched" $ entriesFetch_prop f
                    ]
              ]
        , testGroupSch "TreeDb Behaviour" $ schedule rs
            [ testPropertySch "Reinsertion is a no-op" $ reinsertion_prop f
            , testPropertySch "Cannot manipulate old nodes" $ handOfGod_prop f
            , testPropertySch "Entries are streamed in ascending order" $ entryOrder_prop f
            , testPropertySch "maxRank reports correct height" $ maxRank_prop f
            ]
        ]
    ]

-- | Insert the contents of any `Foldable` into a `TreeDb` "in place".
--
fromFoldable :: (TreeDb db, Foldable f) => db -> f (DbEntry db) -> IO ()
fromFoldable db = insertStream db . P.each

-- | Sugar for producing a populated `TreeDb` from a `Tree`.
--
withTreeDb :: TreeDb db => (DbEntry db -> (db -> IO a) -> r) -> Tree (DbEntry db) -> (db -> IO a) -> r
withTreeDb f t g = f (rootLabel t) $ \db -> fromFoldable db t *> g db

-- | Property: There must exist an isomorphism between any `Tree BlockHeader`
-- and a `TreeDb`.
--
treeIso_prop
    :: forall db. TreeDb db
    => IsBlockHeader (DbEntry db)
    => Ord (DbEntry db)
    => Ord (DbKey db)
    => (DbEntry db -> (db -> IO Bool) -> IO Bool) -> SparseTree -> Property
treeIso_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db -> do
    t' <- toTree db
    pure $ normalizeTree t == normalizeTree t'
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

-- | Property: Reinserting any amount of `BlockHeader`s that already exist in
-- the `TreeDb` must have no effect (no overwrites, no exceptions, etc.)
--
-- In particular, the persistence function `restore` assumes this property to be
-- true, and likewise `persist` will also write the genesis block to file,
-- assuming `restore` will ignore it upon read.
--
reinsertion_prop
    :: forall db. (TreeDb db, IsBlockHeader (DbEntry db))
    => (DbEntry db -> (db -> IO Bool) -> IO Bool) -> SparseTree -> Property
reinsertion_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db -> do
    fromFoldable db t
    l <- entries db Nothing Nothing Nothing Nothing $ P.length_
    pure $ l == length t
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

-- | Property: It must be impossible to fetch an existing header, alter its
-- contents, and reinsert it into the Tree.
--
-- Even if no exception is thrown due to validation failure, the "rewritten"
-- block should not have actually changed.
--
handOfGod_prop
    :: forall db. (TreeDb db, IsBlockHeader (DbEntry db))
    => (DbEntry db -> (db -> IO Bool) -> IO Bool) -> SparseTree -> Property
handOfGod_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db -> do
    h <- maxEntry db
    try (insert db (over (isoBH . blockNonce) succ h)) >>= \case
        Left (_ :: SomeException) -> pure True
        Right _ -> do
            h' <- maxEntry db
            pure $ h == h'
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

-- | Property: The root node's parent must always be itself.
--
rootParent_prop
    :: forall db. (TreeDb db, IsBlockHeader (DbEntry db))
    => (DbEntry db -> (db -> IO Bool) -> IO Bool) -> SparseTree -> Property
rootParent_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db -> do
    r <- (^. isoBH) <$> root db
    pure $ prop_block_genesis_parent r
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

-- | Property: A `Stream` should properly self-report the amount of items that
-- were streamed at the end.
--
streamCount_prop
    :: forall db a. (TreeDb db, IsBlockHeader (DbEntry db))
    => (DbEntry db -> (db -> IO Bool) -> IO Bool)
    -> (db -> (Stream (Of a) IO (Natural, Eos) -> IO (Of [a] (Natural, Eos))) -> IO (Of [a] (Natural, Eos)))
    -> SparseTree
    -> Property
streamCount_prop f g (SparseTree t0) = ioProperty . withTreeDb f t $ \db -> do
    (ls :> (n, _)) <- g db $ \s -> P.toList s
    pure $ len ls == n -- && n > 0
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

entriesFetch_prop
    :: forall db. (TreeDb db, IsBlockHeader (DbEntry db))
    => (DbEntry db -> (db -> IO Bool) -> IO Bool) -> SparseTree -> Property
entriesFetch_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db -> do
    l <- entries db Nothing Nothing Nothing Nothing $ P.length_
    pure $ l == length t
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

-- | Property: `maxRank` correctly reports the `BlockHeight` of the highest node
-- in the Tree.
--
maxRank_prop
    :: forall db. (TreeDb db, IsBlockHeader (DbEntry db))
    => (DbEntry db -> (db -> IO Bool) -> IO Bool) -> SparseTree -> Property
maxRank_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db -> do
    r <- maxRank db
    let h = fromIntegral . maximum . (^.. each . isoBH . to _blockHeight) $ treeLeaves t
    pure $ r == h
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

-- | Property: No child is streamed before its parent.
--
entryOrder_prop
    :: forall db. (TreeDb db, IsBlockHeader (DbEntry db))
    => (DbEntry db -> (db -> IO Bool) -> IO Bool) -> SparseTree -> Property
entryOrder_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db -> do
    hs <- entries db Nothing Nothing Nothing Nothing $ P.toList_ . P.map (^. isoBH)
    pure . isJust $ foldlM g S.empty hs
  where
    g acc h = let acc' = S.insert (_blockHash h) acc
              in bool Nothing (Just acc') $ isGenesisBlockHeader h || S.member (_blockParent h) acc'

    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

branches
    :: TreeDb t
    => (t
        -> Maybe a
        -> Maybe a1
        -> Maybe a2
        -> Maybe a3
        -> HS.HashSet (LowerBound (Key (DbEntry t)))
        -> HS.HashSet (UpperBound (Key (DbEntry t)))
        -> (m b -> IO x) -> IO x)
    -> t
    -> (m b -> IO x)
    -> IO x
branches f db g = do
    geni <- root db
    leaf <- maxEntry db
    let lows = HS.singleton . LowerBound $ key geni
        ups  = HS.singleton . UpperBound $ key leaf
    f db Nothing Nothing Nothing Nothing lows ups g

genParent_prop
    :: forall db. (TreeDb db, IsBlockHeader (DbEntry db))
    => (DbEntry db -> (db -> IO Bool) -> IO Bool) -> SparseTree -> Property
genParent_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db ->
    isNothing . parent <$> root db
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0
