{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
, limitCutHeaders
, unsafeMkCut

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

-- * Misc internal tools
, zipCuts

) where

import Control.DeepSeq
import Control.Exception hiding (catch)
import Control.Lens hiding ((:>))
import Control.Monad hiding (join)
import Control.Monad.Catch

import Data.Bifoldable
import Data.Foldable
import Data.Function
import Data.Functor.Of
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Heap as H
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Ord
import Data.These

import GHC.Generics (Generic)
import GHC.Stack

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeaders)
import Chainweb.BlockHeight
import Chainweb.BlockWeight
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

meanChainHeight :: Cut -> BlockHeight
meanChainHeight
    = BlockHeight . mean . fmap (_height . _blockHeight) . toList . _cutHeaders
  where
    mean l = int $ round $ sum (realToFrac <$> l) / realToFrac (length l)

instance HasChainGraph Cut where
    _chainGraph c = _chainGraph (_chainwebVersion c, meanChainHeight c)
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

unsafeMkCut :: ChainwebVersion -> HM.HashMap ChainId BlockHeader -> Cut
unsafeMkCut v hdrs = Cut
    { _cutHeaders = hdrs
    , _cutChainwebVersion = v
    }

-- -------------------------------------------------------------------------- --
-- Limit Cut Hashes By Height

-- | Find a `Cut` that is a predecessor of the given one, and that has a block
-- height that is smaller or equal the given height.
--
-- If the requested limit is larger or equal to the current height, the given
-- cut is returned.
--
-- Otherwise, the requested height limit is divided by the number of chains
-- (rounded down) and the result is used such that for each chain the
-- predecessor of the given cut at the respective height is returned.
--
limitCut
    :: HasCallStack
    => WebBlockHeaderDb
    -> BlockHeight
        -- upper bound for the cut height. This is not a tight bound.
    -> Cut
    -> IO Cut
limitCut wdb h c
    | h >= _cutHeight c = return c
    | otherwise = do
        hdrs <- itraverse go $ view cutHeaders c
        return $ set cutHeaders (HM.mapMaybe id hdrs) c
  where
    ch :: BlockHeight
    ch = chainHeightAtCutHeight wdb h

    go :: ChainId -> BlockHeader -> IO (Maybe BlockHeader)
    go cid bh = do
        !db <- getWebBlockHeaderDb wdb cid
        seekAncestor db bh (min (int $ _blockHeight bh) (int ch))
        -- this is safe because it's guaranteed that the requested rank is
        -- smaller then the block height of the argument

        -- TODO ^ this comment isn't true any more. Instead ignore chains
        -- that don't give a result for the requested hight.

-- TODO for chain expansion
--
chainHeightAtCutHeight :: HasChainwebVersion v => v -> BlockHeight -> BlockHeight
chainHeightAtCutHeight = error "Chainweb.Cut.chainHeightAtCutHeight: not yet implemented"
-- chainHeightAtCutHeight v ch = ch
--   where
--     gorder :: Natural
--     gorder = order $ _chainGraph wdb
--
--     ch :: BlockHeight
--     ch = h `div` int gorder

limitCutHeaders
    :: HasCallStack
    => WebBlockHeaderDb
    -> BlockHeight
        -- ^ upper bound for the cut height. This is not a tight bound.
    -> HM.HashMap ChainId BlockHeader
    -> IO (HM.HashMap ChainId BlockHeader)
limitCutHeaders whdb h ch = _cutHeaders <$> limitCut whdb h Cut
    { _cutHeaders = ch
    , _cutChainwebVersion = _chainwebVersion whdb
    }

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
        $! (_blockParent a == ba && _blockParent b == ab)
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
    return $! monotonic && validBraiding
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
    return $! c & cutHeaders . ix (_chainId h) .~ h

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

type DiffItem a = These a a

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
    return $! Join (Cut m (_chainwebVersion wdb)) h
  where
    f
        :: (HM.HashMap ChainId BlockHeader, JoinQueue a)
        -> (ChainId, BlockHeader, BlockHeader)
        -> IO (HM.HashMap ChainId BlockHeader, JoinQueue a)
    f (m, q) (cid, x, y) = do
        db <- getWebBlockHeaderDb wdb cid
        (q' :> h) <- S.fold g q id $ branchDiff_ db x y
        let !h' = q' `seq` h `seq` HM.insert cid h m
        return (h', q')

    g :: JoinQueue a -> DiffItem BlockHeader -> JoinQueue a
    g q x = foldl' maybeInsert q $ zip (biList x) (biList (prioFun x))

    maybeInsert
        :: H.Heap (H.Entry (BlockHeight, a) BlockHeader)
        -> (BlockHeader, Maybe a)
        -> H.Heap (H.Entry (BlockHeight, a) BlockHeader)
    maybeInsert !q (_, Nothing) = q
    maybeInsert !q (!h, (Just !p)) = H.insert (H.Entry (_blockHeight h, p) h) q

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

-- | Merge two Cuts. If at least one of the input cuts had a valid braiding the
-- result is guaranteed to have a valid braiding for all blocks included in cut
-- and their ancestors.
--
-- This is because the merge starts with the intersection of both cuts, using
-- 'branchDiff_' on each chain, and constructs the merge cut using
-- 'tryMonotonicCutExtension'. If one of the inputs is correctly braided, so is
-- the intersection. 'tryMonotonicCutExtension' is guaranteed to maintain that
-- property.
--
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

    f (This _) = This (Just $ w a)
    f (That _) = That (Just $ w b)
    f (These _ _)
        | heaviest == a = These (Just 0) Nothing
        | otherwise = These Nothing (Just 0)

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
    !r <- HM.fromList <$> mapM f (zipCuts a b)
    return $! Cut r (_chainwebVersion wdb)
  where
    f (!cid, !x, !y) = (cid,) <$!> do
        db <- getWebBlockHeaderDb wdb cid
        forkEntry db x y

forkDepth
    :: WebBlockHeaderDb
    -> Cut
    -> Cut
    -> IO Natural
forkDepth wdb a b = do
    m <- meet wdb a b
    return $! int $ max (maxDepth m a) (maxDepth m b)
  where
    maxDepth l u = maximum
        $ (\(_, x, y) -> _blockHeight y - _blockHeight x)
        <$> zipCuts l u
