{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module: Chainweb.Cut
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Cut
( Cut
, _cutMap
, cutMap
, _cutHeight
, cutHeight
, _cutWeight
, cutWeight
, _cutAdjPairs
, cutAdjPairs
, cutAdjs
, lookupCutM
, forkDepth

-- * Exceptions
, CutException(..)

-- * Genesis Cut
, genesisCut
, genesisCut_

-- * Checks
, checkBraidingOfCut
, checkBraidingOfCutPairs
, checkBraidingOfCutPair
, isBraidingOfCutPair

-- * Extending Cuts
, isMonotonicCutExtension
, monotonicCutExtension
, tryMonotonicCutExtension

-- * Join
, Join(..)
, join
, join_
, applyJoin
, prioritizeHeavier
, prioritizeHeavier_
, joinIntoHeavier
, joinIntoHeavier_

-- * Testing

, testMine
, testMineCut
, randomChainId
, arbitraryChainGraphChainId

-- ** properties
, prop_cutBraiding
, prop_cutBraidingGenesis
, prop_joinBase
, prop_joinBaseMeet

, properties_lattice
, properties_lattice_passing
, properties_cut
, properties_testMining

-- ** all passing properties
, properties

) where

import Control.DeepSeq
import Control.Exception hiding (catch)
import Control.Lens hiding ((:>))
import Control.Monad hiding (join)
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Foldable
import Data.Function
import Data.Functor.Of
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Heap as H
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Reflection hiding (int)

import GHC.Generics (Generic)
import GHC.Stack

import Numeric.Natural

import Prelude hiding (lookup)

import qualified QuickCheck.GenT as TT

import qualified Streaming.Prelude as S

import System.Random

import qualified Test.QuickCheck as T
import qualified Test.QuickCheck.Monadic as T

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Difficulty (HashTarget)
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.Time
import Chainweb.TreeDB hiding (properties)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Numeric.AffineSpace

-- -------------------------------------------------------------------------- --
-- Cut

-- | A cut is a distributed state of a concurrent computation. Formally, a cut
-- satisfies the property
--
-- * prop_cutBraiding
--
-- A cut also satisfies the properties that
--
-- * the graph is valid,
-- * the graph corresponds to the 'ChainwebVersion',
-- * the set of 'ChainId's of the cut are exactly the vertices of the graph,
-- * all blockHeaders are valid (which is guaranteed by 'genesisBlockHeader')
--   with respect to the graph.
--
data Cut = Cut
    { _cutHeaders :: !(HM.HashMap ChainId BlockHeader)
    , _cutGraph :: !ChainGraph
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

makeLenses ''Cut

instance HasChainGraph Cut where
    _chainGraph = view cutGraph
    {-# INLINE _chainGraph #-}

type instance Index Cut = ChainId
type instance IxValue Cut = BlockHeader

instance IxedGet Cut where
    ixg i = cutHeaders . ix i
    {-# INLINE ixg #-}

_cutMap :: Cut -> HM.HashMap ChainId BlockHeader
_cutMap = _cutHeaders

cutMap :: Getter Cut (HM.HashMap ChainId BlockHeader)
cutMap = cutHeaders

lookupCutM
    :: MonadThrow m
    => HasChainId cid
    => cid
    -> Cut
    -> m BlockHeader
lookupCutM cid c = firstOf (ixg (_chainId cid)) c
    ??? ChainNotInChainGraphException
        (Expected $ chainIds_ $ _chainGraph c)
        (Actual (_chainId cid))

_cutWeight :: Cut -> BlockWeight
_cutWeight = sumOf $ cutHeaders . folded . blockWeight

cutWeight :: Getter Cut BlockWeight
cutWeight = to _cutWeight
{-# INLINE cutWeight #-}

_cutHeight :: Cut -> BlockHeight
_cutHeight = sumOf $ cutHeaders . folded . blockHeight

cutHeight :: Getter Cut BlockHeight
cutHeight = to _cutHeight
{-# INLINE cutHeight #-}

-- | All adjacent pairs of a cut
--
_cutAdjPairs :: Cut -> HS.HashSet (AdjPair BlockHeader)
_cutAdjPairs c = HS.map (fmap (\x -> c ^?! ixg x)) . adjs $ _cutGraph c

cutAdjPairs :: Getter Cut (HS.HashSet (AdjPair BlockHeader))
cutAdjPairs = to _cutAdjPairs
{-# INLINE cutAdjPairs #-}

-- | Assumes that the cid exists in the graph.
--
cutAdjs
    :: HasChainId cid
    => Cut
    -> cid
    -> HM.HashMap ChainId BlockHeader
cutAdjs c cid = HM.intersection
    (_cutHeaders c)
    (HS.toMap (adjacentChainIds (_cutGraph c) cid))

-- -------------------------------------------------------------------------- --
-- Genesis Cut

-- | TODO
--
-- This guarantees that
--
-- * the graph is valid,
-- * the graph corresponds to the 'ChainwebVersion',
-- * the set of 'ChainId's of the cut are the exactly the vertices of the graph,
-- * all blockHeaders are valid (which is guaranteed by 'genesisBlockHeader').
--
-- These properties are maintained as inductive invariants for all Cuts.
--
genesisCut
    :: Given ChainGraph
    => ChainwebVersion
    -> Cut
genesisCut = genesisCut_ given

genesisCut_
    :: ChainGraph
    -> ChainwebVersion
    -> Cut
genesisCut_ graph v = Cut
    { _cutHeaders = HM.mapWithKey (\cid _ -> genesisBlockHeader v graph cid)
        . HS.toMap
        $ chainIds_ graph
    , _cutGraph = graph
    }

-- -------------------------------------------------------------------------- --
-- Exceptions

data CutException
    = InvalidCutPair (AdjPair BlockHeader)
    | NonMonotonicCutExtension (Expected BlockHash) (Actual BlockHash) BlockHeader
    | InvalidCutExtension BlockHeader
    deriving (Show, Eq, Ord, Generic)

instance Exception CutException

-- -------------------------------------------------------------------------- --
-- Cut Properties

-- | Check that a cut is correctly braided.
--
-- This check is only needed for external cuts. Note, that for
-- imported cuts this must be checked recursively. This can
-- be done by doing a join that starts with a meet with a local
-- cut.
--
checkBraidingOfCut
    :: MonadThrow m
    => Cut
    -> m ()
checkBraidingOfCut = checkBraidingOfCutPairs . _cutAdjPairs

-- | Check that a set of adjacent pairs of a cut is correctly braided.
--
checkBraidingOfCutPairs
    :: MonadThrow m
    => Foldable f
    => f (AdjPair BlockHeader)
    -> m ()
checkBraidingOfCutPairs = traverse_ checkBraidingOfCutPair

checkBraidingOfCutPair
    :: MonadThrow m
    => AdjPair BlockHeader
    -> m ()
checkBraidingOfCutPair p = unlessM (isBraidingOfCutPair p)
    $ throwM (InvalidCutPair p)

-- | Returns whether an adjacent pair in a cut is correctly braided.
--
-- * throws 'ChainNotAdjacentException'
--
isBraidingOfCutPair
    :: MonadThrow m
    => AdjPair BlockHeader
    -> m Bool
isBraidingOfCutPair (Adj a b) = do
    ab <- getAdjacentHash b a
    ba <- getAdjacentHash a b
    return
        $ (_blockParent a == ba && _blockParent b == ab)
        || ba == _blockHash a
        || ab == _blockHash b

-- -------------------------------------------------------------------------- --
-- Extending Cuts

-- | Extends a Cut monotonically, i.e. the replaced block header is the parent
-- of the added block header.
--
-- Checks
--
-- * block header is from the ChainGraph of the Cut
-- * result has valid braiding
-- * result is a cut
-- * update is monotonic
--
-- This includes a check that inductively maintains 'checkBraidingOfCut'.
--
isMonotonicCutExtension
    :: MonadThrow m
    => Cut
    -> BlockHeader
    -> m Bool
isMonotonicCutExtension c h = give (_cutGraph c) $ do
    checkBlockHeaderGraph h
    return $ monotonic && validBraiding
  where
    monotonic = _blockParent h == c ^?! ixg (_chainId h) . blockHash
    validBraiding = getAll $ foldMap
        (\v -> All $ let a = c ^?! ixg (_chainId v) in _blockHash a == v || _blockParent a == v)
        (_getBlockHashRecord $ _blockAdjacentHashes h)

monotonicCutExtension
    :: MonadThrow m
    => Cut
    -> BlockHeader
    -> m Cut
monotonicCutExtension c h = do
    unlessM (isMonotonicCutExtension c h) $ throwM $ InvalidCutExtension h
    return $ c & cutHeaders . ix (_chainId h) .~ h

tryMonotonicCutExtension
    :: MonadThrow m
    => Cut
    -> BlockHeader
    -> m (Maybe Cut)
tryMonotonicCutExtension c h = extendIf <$> isMonotonicCutExtension c h
  where
    extendIf True = Just $ set (cutHeaders . ix (_chainId h)) h c
    extendIf False = Nothing

-- -------------------------------------------------------------------------- --
-- Join

type JoinQueue a = H.Heap (H.Entry (BlockHeight, a) BlockHeader)

data Join a = Join
    { _joinBase :: !Cut
    , _joinQueue :: !(JoinQueue a)
    }

join
    :: Ord a
    => Given WebBlockHeaderDb
    => (DiffItem BlockHeader -> DiffItem (Maybe a))
    -> Cut
    -> Cut
    -> IO (Join a)
join f = join_ f `on` _cutHeaders

-- | This merges two maps from ChainIds to BlockHeaders such that the result
-- is a Cut. Note, however, that the resulting cut contains only the chain ids
-- from the intersection of the input maps.
--
join_
    :: forall a
    . Ord a
    => Given WebBlockHeaderDb
    => (DiffItem BlockHeader -> DiffItem (Maybe a))
    -> HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
    -> IO (Join a)
join_ prioFun a b = give (_chainGraph (given @WebBlockHeaderDb)) $ do
    (m, h) <- foldM f (mempty, mempty) (zipChainIdMaps a b)
    return $ Join (Cut m given) h
  where
    f
        :: (HM.HashMap ChainId BlockHeader, JoinQueue a)
        -> (ChainId, BlockHeader, BlockHeader)
        -> IO (HM.HashMap ChainId BlockHeader, JoinQueue a)
    f (m, q) (cid, x, y) = do
        db <- getWebBlockHeaderDb cid
        (q' :> h) <- S.fold g q id $ branchDiff_ db x y
        return (HM.insert cid h m, q')

    g :: JoinQueue a -> DiffItem BlockHeader -> JoinQueue a
    g q x = foldl' maybeInsert q $ zip (toList x) (toList (prioFun x))

    maybeInsert
        :: H.Heap (H.Entry (BlockHeight, a) BlockHeader)
        -> (BlockHeader, Maybe a)
        -> H.Heap (H.Entry (BlockHeight, a) BlockHeader)
    maybeInsert q (_, Nothing) = q
    maybeInsert q (h, Just p) = H.insert (H.Entry (_blockHeight h, p) h) q

    -- | Only chain ids of the intersection are included in the result.
    --
    zipChainIdMaps
        :: HM.HashMap ChainId BlockHeader
        -> HM.HashMap ChainId BlockHeader
        -> [(ChainId, BlockHeader, BlockHeader)]
    zipChainIdMaps m0 m1 = catMaybes
        [ (cida, x, y) <$ guard (cida == cidb)
        | (cida, x) <- itoList m0
        | (cidb, y) <- itoList m1
        ]

-- | If the cuts are from different graphs only the chain ids of the
-- intersection are included in the result.
--
zipCuts
    :: Cut
    -> Cut
    -> [(ChainId, BlockHeader, BlockHeader)]
zipCuts a b = catMaybes
    [ (cida, x, y) <$ guard (cida == cidb)
    | (cida, x) <- itoList $ _cutHeaders a
    | (cidb, y) <- itoList $ _cutHeaders b
    ]

-- This can't fail because of missing dependencies. It can fail because
-- of conflict.
--
applyJoin :: MonadThrow m => Join a -> m Cut
applyJoin m = foldM
    (\c b -> fromMaybe c <$> tryMonotonicCutExtension c (H.payload b))
    (_joinBase m)
    (_joinQueue m)

joinIntoHeavier
    :: Given WebBlockHeaderDb
    => Cut
    -> Cut
    -> IO Cut
joinIntoHeavier = joinIntoHeavier_ `on` _cutHeaders

joinIntoHeavier_
    :: Given WebBlockHeaderDb
    => HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
    -> IO Cut
joinIntoHeavier_ a b = do
    m <- join_ (prioritizeHeavier_ a b) a b
    applyJoin m

prioritizeHeavier :: Cut -> Cut -> DiffItem BlockHeader -> DiffItem (Maybe Int)
prioritizeHeavier = prioritizeHeavier_ `on` _cutHeaders

-- | Note: consider the weight of the recursive dependencies for the
-- priority of a block header. For that we would have to annotate the
-- block headers before putting them in the queue. To traverse only once,
-- we'd have to traverse the zipped cuts by height and not by chainid, which
-- could easily be done by merging/zipping the branch-diff streams.
--
prioritizeHeavier_
    :: Foldable f
    => Ord (f BlockHeader)
    => f BlockHeader
    -> f BlockHeader
    -> DiffItem BlockHeader
    -> DiffItem (Maybe Int)
prioritizeHeavier_ a b = f
  where
    heaviest = maxBy (compare `on` weight) a b
    w c = if c == heaviest then 0 else 1

    f (LeftD _) = LeftD (Just $ w a)
    f (RightD _) = RightD (Just $ w b)
    f (BothD _ _)
        | heaviest == a = BothD (Just 0) Nothing
        | otherwise = BothD Nothing (Just 0)

    weight c =
        ( sumOf (folded . blockWeight) c
            -- first sort by weight
        , sumOf (folded . blockHeight) c
            -- for scenarios with trivial difficulty height is added as
            -- secondary metrics
        , c
            -- the block hashes of the cut are added as tie breaker in order
            -- to guarantee commutativity.
        )

-- -------------------------------------------------------------------------- --
-- Cut Meet

-- | Intersection of cuts
--
meet
    :: Given WebBlockHeaderDb
    => Cut
    -> Cut
    -> IO Cut
meet a b = do
    r <- HM.fromList <$> mapM f (zipCuts a b)
    return $ Cut r (_chainGraph (given @WebBlockHeaderDb))
  where
    f (cid, x, y) = (cid,) <$> do
        db <- getWebBlockHeaderDb cid
        forkEntry db x y

forkDepth
    :: Given WebBlockHeaderDb
    => Cut
    -> Cut
    -> IO Natural
forkDepth a b = do
    m <- meet a b
    return . int $ max (maxDepth m a) (maxDepth m b)
  where
    maxDepth l u = maximum
        $ (\(_, x, y) -> _blockHeight y - _blockHeight x)
        <$> zipCuts l u

-- -------------------------------------------------------------------------- --
-- TESTING
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Test Mining

-- Try to mine a new block header on the given chain for the given cut.
-- Returns 'Nothing' if mining isn't possible because of missing adjacent
-- dependencies.
--
testMine
    :: HasChainId cid
    => Given WebBlockHeaderDb
    => Nonce
    -> HashTarget
    -> NodeId
    -> cid
    -> Cut
    -> IO (Maybe Cut)
testMine n ht nid i c = do
    ct <- getCurrentTimeIntegral
    forM (testMineCut n ht ct nid i c) $ \(h, c') ->
        c' <$ insertWebBlockHeaderDb h

-- | Only produces a new cut but doesn't insert it into the chain database.
--
testMineCut
    :: HasCallStack
    => HasChainId cid
    => Nonce
    -> HashTarget
    -> Time Int64
    -> NodeId
    -> cid
    -> Cut
    -> Maybe (BlockHeader, Cut)
testMineCut n ht ct nid i c = do
    h <- newHeader . BlockHashRecord <$> newAdjHashes
    Just (h, c & cutHeaders . ix cid .~ h)
  where
    cid = _chainId i

    -- | The parent block to mine on.
    --
    p :: BlockHeader
    p = c ^?! ixg cid

    newHeader :: BlockHashRecord -> BlockHeader
    newHeader as = testBlockHeader' (nodeIdFromNodeId nid cid) as n ht ct p

    -- | Try to get all adjacent hashes dependencies.
    --
    newAdjHashes :: Maybe (HM.HashMap ChainId BlockHash)
    newAdjHashes = forM (_getBlockHashRecord $ _blockAdjacentHashes p) $ \x ->
        c ^?! ixg (_chainId x) . to (tryAdj (_blockHeight p))

    tryAdj :: BlockHeight -> BlockHeader -> Maybe BlockHash
    tryAdj h b
        | _blockHeight b == h = Just (_blockHash b)
        | _blockHeight b == h + 1 = Just $ _blockParent b
        | otherwise = Nothing

randomChainId :: HasChainGraph g => g -> IO ChainId
randomChainId g = (!!) (toList cs) <$> randomRIO (0, length cs - 1)
  where
    cs = give (_chainGraph g) chainIds

-- -------------------------------------------------------------------------- --
-- Arbitrary Cuts

arbitraryCut
    :: HasCallStack
    => Given ChainGraph
    => T.Gen Cut
arbitraryCut = T.sized $ \s -> do
    k <- T.choose (0,s)
    foldlM (\c _ -> genCut c) (genesisCut Test) [0..(k-1)]
  where
    genCut :: Cut -> T.Gen Cut
    genCut c = do
        cids <- T.shuffle (toList chainIds)
        S.each cids
            & S.mapMaybeM (mine c)
            & S.map snd
            & S.head_
            & fmap fromJust

    mine :: Cut -> ChainId -> T.Gen (Maybe (BlockHeader, Cut))
    mine c cid = do
        n <- Nonce <$> T.arbitrary
        nid <- T.arbitrary
        return $ testMineCut n genesisBlockTarget nid cid c

arbitraryChainGraphChainId :: Given ChainGraph => T.Gen ChainId
arbitraryChainGraphChainId = T.elements (toList chainIds)

instance Given ChainGraph => T.Arbitrary Cut where
    arbitrary = arbitraryCut

-- | Provide option to provide db with a branch/cut.
--
arbitraryWebChainCut
    :: HasCallStack
    => Given ChainGraph
    => Given WebBlockHeaderDb
    => Cut
        -- @genesisCut Test@ is always a valid cut
    -> T.PropertyM IO Cut
arbitraryWebChainCut initialCut = do
    k <- T.pick $ T.sized $ \s -> T.choose (0,s)
    foldlM (\c _ -> genCut c) initialCut [0..(k-1)]
  where
    genCut c = do
        cids <- T.pick $ T.shuffle (toList chainIds)
        S.each cids
            & S.mapMaybeM (mine c)
            & S.head_
            & fmap fromJust

    mine c cid = do
        n <- T.pick $ Nonce <$> T.arbitrary
        nid <- T.pick T.arbitrary
        liftIO $ testMine n genesisBlockTarget nid cid c

arbitraryWebChainCut_
    :: HasCallStack
    => Given ChainGraph
    => Given WebBlockHeaderDb
    => Cut
        -- @genesisCut Test@ is always a valid cut
    -> TT.GenT IO Cut
arbitraryWebChainCut_ initialCut = do
    k <- TT.sized $ \s -> TT.choose (0,s)
    foldlM (\c _ -> genCut c) initialCut [0..(k-1)]
  where
    genCut c = do
        cids <- TT.liftGen $ T.shuffle (toList chainIds)
        S.each cids
            & S.mapMaybeM (mine c)
            & S.head_
            & fmap fromJust

    mine c cid = do
        n <- Nonce <$> TT.liftGen T.arbitrary
        nid <- TT.liftGen T.arbitrary
        liftIO $ testMine n genesisBlockTarget nid cid c

-- -------------------------------------------------------------------------- --
-- Arbitrary Fork

data TestFork = TestFork
    { _testForkBase :: !Cut
    , _testForkLeft :: !Cut
    , _testForkRight :: !Cut
    }
    deriving (Show, Eq, Ord, Generic)

instance (Given ChainGraph, Given WebBlockHeaderDb) => T.Arbitrary (IO TestFork) where
    arbitrary = TT.runGenT arbitraryFork_

instance (Given ChainGraph, Given WebBlockHeaderDb) => T.Arbitrary (IO (Join Int)) where
    arbitrary = TT.runGenT $ do
        TestFork _ cl cr <- arbitraryFork_
        liftIO $ join (prioritizeHeavier cl cr) cl cr

-- | Fork point is the genesis cut
--
-- TODO: provide option to fork of elsewhere
--
arbitraryFork
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO TestFork
arbitraryFork = do
    base <- arbitraryWebChainCut (genesisCut Test)
    TestFork base
        <$> arbitraryWebChainCut base
        <*> arbitraryWebChainCut base

arbitraryFork_
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => TT.GenT IO TestFork
arbitraryFork_ = do
    base <- arbitraryWebChainCut_ (genesisCut Test)
    TestFork base
        <$> arbitraryWebChainCut_ base
        <*> arbitraryWebChainCut_ base

-- -------------------------------------------------------------------------- --
-- 'meet' and 'join' form a lattice with genesisCut as bottom
--
-- The order of the lattice is conistent with the weight order.
--
-- Note:
--
-- * The non-optimal join function 'joinIntoHeavier' doesn't satisfy the lattice
--   laws In particular associativity. However it must satisfy commutativity,
--   for reasonably fast convergence.
--
-- * 'joinIntoHeavier' is likely to be optimal on low diameter graphs even for
--   relatively small test instance size parameters, because it is optimal when
--   a fork on a chain is longer than the diameter.)
--
-- * for fork of depth 1 'joinIntoHeavier' is not a good strategy.
--
-- * TODO: properties about consistency of order
--

-- Join

prop_joinIdempotent
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinIdempotent = do
    c <- arbitraryWebChainCut (genesisCut Test)
    T.run $ (==) c <$> joinIntoHeavier c c

-- FIXME!
prop_joinCommutative
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinCommutative = do
    TestFork _ cl cr <- arbitraryFork
    T.run $ (==)
        <$> joinIntoHeavier cl cr
        <*> joinIntoHeavier cr cl

-- Fails for heuristic joins
--
prop_joinAssociative
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinAssociative = do
    TestFork _ c0 c1 <- arbitraryFork
    TestFork _ c10 c11 <- TestFork c1
        <$> arbitraryWebChainCut c1
        <*> arbitraryWebChainCut c1

    -- d0 <- T.run $ forkDepth c10 c11
    -- T.pre (diameter (given @ChainGraph) <= d0)
    -- T.monitor (T.counterexample $ "fork depth: " <> sshow d0)
    -- d1 <- T.run $ forkDepth c0 c10
    -- T.pre (diameter (given @ChainGraph) <= d1)
    -- T.monitor (T.counterexample $ "fork depth: " <> sshow d1)

    T.run $ do
        let m = joinIntoHeavier
        (==)
            <$> (m c0 =<< m c10 c11)
            <*> (m c0 c10 >>= \x -> m x c11)

prop_joinIdentity
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinIdentity = do
    c <- arbitraryWebChainCut (genesisCut Test)
    T.run $ (==) c <$> joinIntoHeavier (genesisCut Test) c

-- Meet

prop_meetIdempotent
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetIdempotent = do
    c <- arbitraryWebChainCut (genesisCut Test)
    T.run $ (==) c <$> meet c c

prop_meetCommutative
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetCommutative = do
    TestFork _ cl cr <- arbitraryFork
    T.run $ (==)
        <$> meet cl cr
        <*> meet cr cl

prop_meetAssociative
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetAssociative = do
    TestFork _ c0 c1 <- arbitraryFork
    TestFork _ c10 c11 <- TestFork c1
        <$> arbitraryWebChainCut c1
        <*> arbitraryWebChainCut c1
    T.run $ do
        let m = meet
        (==)
            <$> (m c0 =<< m c10 c11)
            <*> (m c0 c10 >>= \x -> m x c11)

-- | this a corollary of 'prop_joinIdentity' and 'prop_meetJoinAbsorption'
--
prop_meetZeroAbsorption
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetZeroAbsorption = do
    c <- arbitraryWebChainCut (genesisCut Test)
    T.run $ do
        c' <- meet (genesisCut Test) c
        return (c == c')

prop_joinMeetAbsorption
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinMeetAbsorption = do
    TestFork _ c0 c1 <- arbitraryFork
    T.run $ do
        c0' <- joinIntoHeavier c0 =<< meet c0 c1
        return (c0' == c0)

prop_meetJoinAbsorption
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetJoinAbsorption = do
    TestFork _ c0 c1 <- arbitraryFork
    T.run $ do
        c0' <- meet c0 =<< joinIntoHeavier c0 c1
        return (c0' == c0)

properties_lattice :: Given ChainGraph => [(String, T.Property)]
properties_lattice =
    [ ("joinIdemPotent", ioTest prop_joinIdempotent)
    , ("joinCommutative", ioTest prop_joinCommutative)
    , ("joinAssociative", ioTest prop_joinAssociative) -- Fails
    , ("joinIdentity", ioTest prop_joinIdentity)

    , ("meetIdemPotent", ioTest prop_meetIdempotent)
    , ("meetCommutative", ioTest prop_meetCommutative)
    , ("meetAssociative", ioTest prop_meetAssociative)
    , ("meetZeroAbsorption", ioTest prop_meetZeroAbsorption) -- Fails

    , ("joinMeetAbsorption", ioTest prop_joinMeetAbsorption)
    , ("meetJoinAbsorption", ioTest prop_meetJoinAbsorption) -- Fails
    ]

properties_lattice_passing :: Given ChainGraph => [(String, T.Property)]
properties_lattice_passing =
    [ ("joinIdemPotent", ioTest prop_joinIdempotent)
    , ("joinCommutative", ioTest prop_joinCommutative)
    , ("joinIdentity", ioTest prop_joinIdentity)

    , ("meetIdemPotent", ioTest prop_meetIdempotent)
    , ("meetCommutative", ioTest prop_meetCommutative)
    , ("meetAssociative", ioTest prop_meetAssociative)

    , ("joinMeetAbsorption", ioTest prop_joinMeetAbsorption)
    ]

-- -------------------------------------------------------------------------- --
-- Cut Properties

prop_cutBraiding :: Given ChainGraph => Cut -> Bool
prop_cutBraiding = either throw (const True) . checkBraidingOfCut

prop_cutBraidingGenesis :: Given ChainGraph => Bool
prop_cutBraidingGenesis = either throw (const True)
    $ checkBraidingOfCut (genesisCut Test)

-- TODO
--
-- * cuts are partially ordered with respect to parent and parent hashes
-- * partial order is consistent with weight and blockheight
--
-- * this order induces a lattice

properties_cut :: Given ChainGraph => [(String, T.Property)]
properties_cut =
    [ ("Cut has valid braiding", T.property prop_cutBraiding)
    , ("Genesis Cut has valid braiding", T.property prop_cutBraidingGenesis)
    ]

-- -------------------------------------------------------------------------- --
-- Meet Properties

prop_meetGenesisCut
    :: Given ChainGraph
    => Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetGenesisCut = liftIO $ (==) c <$> meet c c
  where
    c = genesisCut Test

-- -------------------------------------------------------------------------- --
-- Misc Properties

prop_arbitraryForkBraiding :: Given ChainGraph => T.Property
prop_arbitraryForkBraiding = ioTest $ do
    TestFork b cl cr <- arbitraryFork
    T.assert (prop_cutBraiding b)
    T.assert (prop_cutBraiding cl)
    T.assert (prop_cutBraiding cr)
    return True

prop_joinBase :: Given ChainGraph => T.Property
prop_joinBase = ioTest $ do
    TestFork b cl cr <- arbitraryFork
    m <- liftIO $ join (prioritizeHeavier cl cr) cl cr
    return (_joinBase m == b)

prop_joinBaseMeet
    :: Given ChainGraph
    => T.Property
prop_joinBaseMeet = ioTest $ do
    TestFork _ a b <- arbitraryFork
    liftIO $ (==)
        <$> meet a b
        <*> (_joinBase <$> join (prioritizeHeavier a b) a b)

properties_testMining :: Given ChainGraph => [(String, T.Property)]
properties_testMining =
    [ ("Cuts of arbitrary fork have valid braiding", prop_arbitraryForkBraiding)]

properties_misc :: Given ChainGraph => [(String, T.Property)]
properties_misc =
    [ ("prop_joinBase", prop_joinBase)
    , ("prop_joinBaseMeet", prop_joinBaseMeet)
    , ("prop_meetGenesisCut", ioTest prop_meetGenesisCut)
    , ("Cuts of arbitrary fork have valid braiding", prop_arbitraryForkBraiding)
    ]

-- -------------------------------------------------------------------------- --
-- "Valid" Properties

properties :: [(String, T.Property)]
properties = give pairChainGraph
    $ properties_lattice_passing
    <> properties_cut
    <> properties_testMining
    <> properties_misc

-- -------------------------------------------------------------------------- --
-- TestTools

giveNewWebChain
    :: MonadIO m
    => Given ChainGraph
    => (Given WebBlockHeaderDb => m a)
    -> m a
giveNewWebChain f = do
    db <- liftIO (initWebBlockHeaderDb Test)
    give db f

ioTest
    :: Given ChainGraph
    => (Given WebBlockHeaderDb => T.PropertyM IO Bool)
    -> T.Property
ioTest f = T.monadicIO $ giveNewWebChain $ f >>= T.assert
