{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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
module Chainweb.Test.TreeDB
( treeDbInvariants
, RunStyle(..)
, properties
) where

import Control.Lens (each, from, over, to, (^.), (^..), view)

import Data.Bool (bool)
import Data.Foldable (foldlM, toList)
import Data.Function
import Data.Functor.Identity
import qualified Data.HashSet as HS
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import Data.Tree (Tree(..), levels)

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
import Chainweb.Test.Utils.BlockHeader
import Chainweb.TreeDB
import Chainweb.Utils (int, len, tryAllSynchronous)
import Chainweb.Utils.Paging

type Insert db = db -> [DbEntry db] -> IO ()
type WithTestDb db = forall prop . Testable prop => DbEntry db -> (db -> Insert db -> IO prop) -> IO prop

treeDbInvariants
    :: (TreeDb db, IsBlockHeader (DbEntry db), Ord (DbEntry db), Ord (DbKey db))
    => WithTestDb db
        -- ^ Given a generic entry should yield a database and insert function for
        -- testing, and then safely close it after use.
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
                          $ streamCount_prop f (branches branchKeys)
                    , testPropertySch "streaming branchEntries"
                          $ streamCount_prop f (branches branchEntries)
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
            , testPropertySch "getBranchIncreasing streams in ascending order" $ prop_getBranchIncreasing_order f
            , testPropertySch "getBranchIncreasing streams returns leaf entry last" $ prop_getBranchIncreasing_end f
            , testPropertySch "getBranchIncreasing streams ordered by parent relation" $ prop_getBranchIncreasing_parents f
            , testPropertySch "forkEntry returns correct results" $ prop_forkEntry f
            ]
        ]
    ]

-- | Sugar for producing a populated `TreeDb` from a `Tree`.
--
withTreeDb
    :: TreeDb db
    => Testable prop
    => WithTestDb db
    -> Tree (DbEntry db)
    -> (db -> Insert db -> IO prop)
    -> IO prop
withTreeDb f t g = f (rootLabel t) $ \db insert -> insert db (toList t) *> g db insert

-- | Property: There must exist an isomorphism between any `Tree BlockHeader`
-- and a `TreeDb`.
--
treeIso_prop
    :: forall db. TreeDb db
    => IsBlockHeader (DbEntry db)
    => Ord (DbEntry db)
    => Ord (DbKey db)
    => WithTestDb db
    -> SparseTree
    -> Property
treeIso_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db _ -> do
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
    => WithTestDb db
    -> SparseTree
    -> Property
reinsertion_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db insert -> do
    insert db (toList t)
    l <- entries db Nothing Nothing Nothing Nothing P.length_
    return $ l === length t
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
    => WithTestDb db
    -> SparseTree
    -> Property
handOfGod_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db insert -> do
    h <- maxEntry db
    tryAllSynchronous (insert db [over (isoBH . blockNonce) succ h]) >>= \case
        Left _ -> pure True
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
    => WithTestDb db
    -> SparseTree
    -> Property
rootParent_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db _ -> do
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
    => WithTestDb db
    -> (db -> (Stream (Of a) IO (Natural, Eos) -> IO (Of [a] (Natural, Eos))) -> IO (Of [a] (Natural, Eos)))
    -> SparseTree
    -> Property
streamCount_prop f g (SparseTree t0) = ioProperty . withTreeDb f t $ \db _ -> do
    (ls :> (n, _)) <- g db $ \s -> P.toList s
    pure $ len ls == n -- && n > 0
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

entriesFetch_prop
    :: forall db. (TreeDb db, IsBlockHeader (DbEntry db))
    => WithTestDb db
    -> SparseTree
    -> Property
entriesFetch_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db _ -> do
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
    => WithTestDb db
    -> SparseTree
    -> Property
maxRank_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db _ -> do
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
    => WithTestDb db
    -> SparseTree
    -> Property
entryOrder_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db _ -> do
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
    => WithTestDb db
    -> SparseTree
    -> Property
genParent_prop f (SparseTree t0) = ioProperty . withTreeDb f t $ \db _ ->
    isNothing . parent <$> root db
  where
    t :: Tree (DbEntry db)
    t = fmap (^. from isoBH) t0

-- -------------------------------------------------------------------------- --
-- Seek Limit Stream Properties

prop_seekLimitStream_limit :: [Int] -> Natural -> Property
prop_seekLimitStream_limit l i = i <= len l ==> actual === expected
    & cover 1 (i == len l) "limit == length of stream"
    & cover 1 (i == 0) "limit == 0"
    & cover 1 (null l) "length of stream == 0"
  where
    actual = runIdentity . P.toList $ seekLimitStream id Nothing (Just (Limit i)) (P.each l)
    expected = take (int i) l :> (i, Eos (i >= len l))

prop_seekLimitStream_id :: [Int] -> Property
prop_seekLimitStream_id l = actual === expected
    & cover 1 (null l) "len l == 0"
  where
    actual = runIdentity $ P.toList $ seekLimitStream id Nothing Nothing (P.each l)
    expected = l :> (len l, Eos True)

properties :: [(String, Property)]
properties =
    [ ("seekLimitStream_limit", property prop_seekLimitStream_limit)
    , ("seekLimitStream_id", property prop_seekLimitStream_id)
    ]

-- -------------------------------------------------------------------------- --
-- Fork Entry

prop_forkEntry
    :: forall db
    . TreeDb db
    => IsBlockHeader (DbEntry db)
    => WithTestDb db
    -> Natural
    -> Natural
    -> Property
prop_forkEntry f i j = do
    ioProperty $ withTreeDb f t $ \db insert -> do
        insert db a
        insert db b
        e <- forkEntry db (head $ reverse (g : a)) (head $ reverse $ (g : b))
        return $ e === g
  where
    g = view (from isoBH) $ toyGenesis toyChainId
    t = Node g []
    a = take (int i) $ branch (Nonce 0) g
    b = take (int j) $ branch (Nonce 1) g

    branch n x = view (from isoBH) <$> testBlockHeadersWithNonce n (ParentHeader $ view isoBH x)

-- -------------------------------------------------------------------------- --
-- forward branch entries

prop_getBranchIncreasing_order
    :: forall db
    . TreeDb db
    => IsBlockHeader (DbEntry db)
    => WithTestDb db
    -> SparseTree
    -> Property
prop_getBranchIncreasing_order f (SparseTree t0) = forAll (int <$> choose (0,m)) $ \i -> do
    label ("depth " <> show m) $ label ("width " <> show w) $
        ioProperty $ withTreeDb f t $ \db _ -> do
            e <- maxEntry db
            branch <- getBranchIncreasing db e i $ \s -> s & P.map rank & P.toList_
            return $ branch === [i .. rank e]
  where
    w = maximum $ length <$> levels t0
    m = length $ levels t0
    t = fmap (^. from isoBH) t0

prop_getBranchIncreasing_end
    :: forall db
    . TreeDb db
    => IsBlockHeader (DbEntry db)
    => WithTestDb db
    -> SparseTree
    -> Property
prop_getBranchIncreasing_end f (SparseTree t0) = forAll (int <$> choose (0,m - 1)) $ \i ->
    ioProperty $ withTreeDb f t $ \db _ -> do
        e <- maxEntry db
        l <- getBranchIncreasing db e i P.last_
        return $ l === Just e
  where
    m = length $ levels t0
    t = fmap (^. from isoBH) t0

prop_getBranchIncreasing_parents
    :: forall db
    . TreeDb db
    => IsBlockHeader (DbEntry db)
    => WithTestDb db
    -> SparseTree
    -> Property
prop_getBranchIncreasing_parents f (SparseTree t0) = forAll (int <$> choose (0,m)) $ \i ->
    ioProperty $ withTreeDb f t $ \db _ -> do
        e <- maxEntry db
        branch <- getBranchIncreasing db e i $ \s -> P.toList_ $ P.map (view isoBH) s
        return $ and $ zipWith (\a b -> _blockHash a == _blockParent b) branch (drop 1 branch)
  where
    m = length $ levels t0
    t = fmap (^. from isoBH) t0
