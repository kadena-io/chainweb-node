{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
, limitCut

-- * Exceptions
, CutException(..)

-- * Genesis Cut
, genesisCut

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

-- * Meet
, meet

) where

import Control.DeepSeq
import Control.Exception hiding (catch)
import Control.Lens hiding ((:>))
import Control.Monad hiding (join)
import Control.Monad.Catch

import Data.Foldable
import Data.Function
import Data.Functor.Of
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Heap as H
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Ord
import Data.Reflection hiding (int)

import GHC.Generics (Generic)
import GHC.Stack

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeaders)
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.TreeDB hiding (properties)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

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
    , _cutChainwebVersion :: !ChainwebVersion
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

makeLenses ''Cut

instance HasChainGraph Cut where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion Cut where
    _chainwebVersion = view cutChainwebVersion
    {-# INLINE _chainwebVersion #-}

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
        (Expected $ chainIds c)
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
_cutAdjPairs c = HS.map (fmap (\x -> c ^?! ixg x)) . adjs $ _chainGraph c

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
    (HS.toMap (adjacentChainIds (_chainGraph c) cid))

-- -------------------------------------------------------------------------- --
-- Limit Cut Hashes By Height

limitCut
    :: HasCallStack
    => WebBlockHeaderDb
    -> BlockHeight
        -- upper bound for the cut height. This is not a tight bound.
    -> Cut
    -> IO Cut
limitCut wdb h c = c & (cutHeaders . itraverse) f
  where
    ch = h `div` int (order (_chainGraph wdb))
    f cid x = do
        db <- give wdb $ getWebBlockHeaderDb cid
        a <- S.head_ & branchEntries db
            Nothing (Just 1)
            Nothing (Just $ int ch)
            mempty (HS.singleton $ UpperBound $ _blockHash x)
        return $ fromJuste a
            -- this is safe because branchEntries returns at least
            -- the genesis block

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
    :: ChainwebVersion
    -> Cut
genesisCut v = Cut
    { _cutHeaders = genesisBlockHeaders v
    , _cutChainwebVersion = v
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
isMonotonicCutExtension c h = do
    checkBlockHeaderGraph h
    return $ monotonic && validBraiding
  where
    monotonic = _blockParent h == c ^?! ixg (_chainId h) . blockHash
    validBraiding = getAll $ ifoldMap
        (\cid v -> All $ let a = c ^?! ixg cid in _blockHash a == v || _blockParent a == v)
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
    => WebBlockHeaderDb
    -> (DiffItem BlockHeader -> DiffItem (Maybe a))
    -> Cut
    -> Cut
    -> IO (Join a)
join wdb f = join_ wdb f `on` _cutHeaders

-- | This merges two maps from ChainIds to BlockHeaders such that the result
-- is a Cut. Note, however, that the resulting cut contains only the chain ids
-- from the intersection of the input maps.
--
join_
    :: forall a
    . Ord a
    => WebBlockHeaderDb
    -> (DiffItem BlockHeader -> DiffItem (Maybe a))
    -> HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
    -> IO (Join a)
join_ wdb prioFun a b = do
    (m, h) <- foldM f (mempty, mempty) (zipChainIdMaps a b)
    return $ Join (Cut m (_chainwebVersion wdb)) h
  where
    f
        :: (HM.HashMap ChainId BlockHeader, JoinQueue a)
        -> (ChainId, BlockHeader, BlockHeader)
        -> IO (HM.HashMap ChainId BlockHeader, JoinQueue a)
    f (m, q) (cid, x, y) = do
        db <- give wdb $ getWebBlockHeaderDb cid
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
    :: WebBlockHeaderDb
    -> Cut
    -> Cut
    -> IO Cut
joinIntoHeavier wdb = joinIntoHeavier_ wdb `on` _cutHeaders

joinIntoHeavier_
    :: WebBlockHeaderDb
    -> HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
    -> IO Cut
joinIntoHeavier_ wdb a b = do
    m <- join_ wdb (prioritizeHeavier_ a b) a b
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
    :: WebBlockHeaderDb
    -> Cut
    -> Cut
    -> IO Cut
meet wdb a b = do
    r <- HM.fromList <$> mapM f (zipCuts a b)
    return $ Cut r (_chainwebVersion wdb)
  where
    f (cid, x, y) = (cid,) <$> do
        db <- give wdb $ getWebBlockHeaderDb cid
        forkEntry db x y

forkDepth
    :: WebBlockHeaderDb
    -> Cut
    -> Cut
    -> IO Natural
forkDepth wdb a b = do
    m <- meet wdb a b
    return . int $ max (maxDepth m a) (maxDepth m b)
  where
    maxDepth l u = maximum
        $ (\(_, x, y) -> _blockHeight y - _blockHeight x)
        <$> zipCuts l u

