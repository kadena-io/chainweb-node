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
-- A cut represents the a distributed state of a concurrent computation.
--
-- In the context of Chainweb it contains one block for each chain (possibly of
-- a subset of all chains) that any pair of two blocks in the cut either
-- directly depend on each other or are independent from each other.
--
-- Intuitively, a cut is a set of blocks from different chains that can (but
-- doesn't have to) occur at the same time as the head of their respective
-- chains.
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
, chainHeights
, meanChainHeight
, maxChainHeight
, minChainHeight
, isTransitionCut

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
import qualified Data.Text as T
import Data.These

import GHC.Generics (Generic)
import GHC.Stack

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
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
-- A cut can be created in the following ways and only in the following ways:
--
-- 1. 'genesisCut',
-- 2. 'monotonicCutExtension' and 'tryMonotonicCutExtension',
-- 3. 'applyJoin' (preceeded by 'join'),
-- 4. 'limitCut'.
--
-- On startup a cut is created by
-- 5. deserializing the cut that is persisted in the local cutDB.
--
-- There are a few more methods (like for instance 'meet') that are not relevant
-- for production scenarios.
--
-- The above methods use either the 'Cut\'' constructor or the 'unsafeCutHeaders'
-- function, both of which are not exported from this module.
--
data Cut = Cut'
    { _cutHeaders :: !(HM.HashMap ChainId BlockHeader)
    , _cutChainwebVersion :: !ChainwebVersion
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

cutChainwebVersion :: Lens' Cut ChainwebVersion
cutChainwebVersion = lens _cutChainwebVersion $ \c v -> c { _cutChainwebVersion = v }
{-# INLINE cutChainwebVersion #-}

cutHeaders :: Getter Cut (HM.HashMap ChainId BlockHeader)
cutHeaders = to _cutHeaders

unsafeCutHeaders :: Setter' Cut (HM.HashMap ChainId BlockHeader)
unsafeCutHeaders = lens _cutHeaders $ \c m -> c { _cutHeaders = m }
{-# INLINE unsafeCutHeaders #-}

-- | The chain graph is the graph at the /minimum/ height of the block headers
-- in the cut.
--
instance HasChainGraph Cut where
    _chainGraph c = chainGraphAt (_chainwebVersion c) (minChainHeight c)
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

_cutHeight :: Cut -> CutHeight
_cutHeight = sumOf $ cutHeaders . folded . blockHeight . to int

cutHeight :: Getter Cut CutHeight
cutHeight = to _cutHeight
{-# INLINE cutHeight #-}

unsafeMkCut :: ChainwebVersion -> HM.HashMap ChainId BlockHeader -> Cut
unsafeMkCut v hdrs = Cut'
    { _cutHeaders = hdrs
    , _cutChainwebVersion = v
    }

-- -------------------------------------------------------------------------- --
-- Adjacents
--
-- Note that in a cut adjacency is directed. The reason is that the headers in a
-- cut may be of different height and may thus use different graphs.

-- | Map of adjacent block headers for a chain in a cut. The function considers
-- the block height for the respective chain in the given cut.
--
-- Assumes that the cid exists in the graph.
--
cutAdjs
    :: HasChainId cid
    => Cut
    -> cid
    -> HM.HashMap ChainId BlockHeader
cutAdjs c = HM.intersection (_cutHeaders c) . HS.toMap . cutAdjChainIds c
{-# INLINE cutAdjs #-}

-- | Adjacent chain ids for a chain in a cut. The function considers the block
-- height for the respective chain in the given cut.
--
-- Assumes that the cid exists in the graph.
--
cutAdjChainIds :: HasChainId cid => Cut -> cid -> HS.HashSet ChainId
cutAdjChainIds c cid = c ^?! ixg (_chainId cid) . blockAdjacentChainIds
{-# INLINE cutAdjChainIds #-}

-- | /Directed/ adjacent pairs of a cut. Note that block headers in a cut can be
-- of different block height and can thus use different chain graphs.
--
_cutAdjPairs :: Cut -> [(BlockHeader, BlockHeader)]
_cutAdjPairs c = do
    (cid, h) <- HM.toList (_cutMap c)
    x <- toList (cutAdjs c cid)
    return (h, x)
{-# INLINE _cutAdjPairs #-}

cutAdjPairs :: Getter Cut [(BlockHeader, BlockHeader)]
cutAdjPairs = to _cutAdjPairs
{-# INLINE cutAdjPairs #-}

-- -------------------------------------------------------------------------- --
-- Chain Heights

chainHeights :: Cut -> [BlockHeight]
chainHeights = fmap (_blockHeight) . toList . _cutHeaders
{-# INLINE chainHeights #-}

meanChainHeight :: Cut -> BlockHeight
meanChainHeight = mean . chainHeights
  where
    mean l = round $ sum @_ @Double (realToFrac <$> l) / realToFrac (length l)
{-# INLINE meanChainHeight #-}

maxChainHeight :: Cut -> BlockHeight
maxChainHeight = maximum . chainHeights
{-# INLINE maxChainHeight #-}

minChainHeight :: Cut -> BlockHeight
minChainHeight = minimum . chainHeights
{-# INLINE minChainHeight #-}

-- | Returns whether a chain graph transition occurs within the cut.
--
isTransitionCut :: Cut -> Bool
isTransitionCut c = minChainHeight c < lastGraphChange c (maxChainHeight c)

-- -------------------------------------------------------------------------- --
-- Tools for Graph Transitions
--
-- These functions are used to adjust the available chains during construction
-- of new cuts:
--
-- * 'monotonicCutExtension' and 'tryMonotonicCutExtension': extend the
--   resulting cut with genesis headers of new chains.
--
-- * 'limitCut': project out chains that don't exist in the result cut.
--
-- * 'join' and 'applyJoin': add chains from both cuts to the input cuts, so
--   that all chains are available in the join base and can be restored during
--   'applyJoin'; project out non-existing chains in the result.
--
-- The graph is determined by the /minimum/ height of the blocks in the cut.
--
-- The minimum is used to ensure that cuts in a new graph only exist when /all/
-- blocks are on the new cut. This means the new chains are included only if all
-- old chains have transitioned to the minimum block height of the new graph.

cutHeadersMinHeight :: HM.HashMap ChainId BlockHeader -> BlockHeight
cutHeadersMinHeight = minimum . fmap _blockHeight
{-# INLINE cutHeadersMinHeight #-}

cutHeadersChainwebVersion :: HM.HashMap ChainId BlockHeader -> ChainwebVersion
cutHeadersChainwebVersion m = _chainwebVersion $ head $ toList m
{-# INLINE cutHeadersChainwebVersion #-}

-- | The function projects onto the chains available at the minimum block height
-- in input headers.
--
-- At a graph change chains are considered blocked until "all" chains performed
-- the transition to the new graph. Thus, a block in the new graph has all of
-- its dependencies available.
--
-- This an internal function. The result is meaningful only if the input headers
-- form a valid cut. In particular, the input must not be empty.
--
projectChains
    :: HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
projectChains m = HM.intersection m
    $ HS.toMap
    $ chainIdsAt (cutHeadersChainwebVersion m) (cutHeadersMinHeight m)
{-# INLINE projectChains #-}

-- | Extend the chains for the graph at the minimum block height of the input
-- headers. If a header for a chain is missing the genesis block header for that
-- chain is added.
--
-- This an internal function. The result is meaningful only if the input headers
-- form a valid cut. In particular, the input must not be empty.
--
extendChains
    :: HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
extendChains m = HM.union m
    $ genesisBlockHeadersAtHeight
        (cutHeadersChainwebVersion m)
        (cutHeadersMinHeight m)
{-# INLINE extendChains #-}

-- | This function adds all chains that are available in either of the input
-- headers. It is assumed that both input header maps are contain headers for
-- all chains for the graph at the respective minimum height of the headers.
--
-- This function is used when dealing with joins that internally compute an
-- intersection on the blocks on all chains, but the goal is to preserve blocks
-- from all chains.
--
-- This an internal function. The result is meaningful only if the input headers
-- form a valid cut. In particular, the input must not be empty.
--
joinChains
    :: HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
    -> (HM.HashMap ChainId BlockHeader, HM.HashMap ChainId BlockHeader)
joinChains a b = (HM.union a c, HM.union b c)
  where
    v = cutHeadersChainwebVersion a
    c = genesisBlockHeader v <$> a <> b
{-# INLINE joinChains #-}

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
    -> CutHeight
        -- upper bound for the cut height. This is not a tight bound.
    -> Cut
    -> IO Cut
limitCut wdb h c
    | h >= _cutHeight c = return c
    | otherwise = do
        hdrs <- itraverse go $ view cutHeaders c
        return $! set unsafeCutHeaders (projectChains $ HM.mapMaybe id hdrs) c
  where
    ch :: BlockHeight
    ch = floor $ avgBlockHeightAtCutHeight (_chainwebVersion wdb) h

    go :: ChainId -> BlockHeader -> IO (Maybe BlockHeader)
    go cid bh = do
        !db <- getWebBlockHeaderDb wdb cid
        seekAncestor db bh (min (int $ _blockHeight bh) (int ch))
        -- this is safe because it's guaranteed that the requested rank is
        -- smaller then the block height of the argument

-- | The resulting headers are valid cut headers only if the input headers are
-- valid cut headers, too. The inverse is not true.
--
limitCutHeaders
    :: HasCallStack
    => WebBlockHeaderDb
    -> CutHeight
        -- ^ upper bound for the cut height. This is not a tight bound.
    -> HM.HashMap ChainId BlockHeader
    -> IO (HM.HashMap ChainId BlockHeader)
limitCutHeaders whdb h ch = _cutHeaders <$> limitCut whdb h Cut'
    { _cutHeaders = ch
    , _cutChainwebVersion = _chainwebVersion whdb
    }

-- -------------------------------------------------------------------------- --
-- Genesis Cut

-- | The genesis cut for a chainweb version.
--
-- The genesis cut contains only the genesis headers at block height0 0.
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
genesisCut v = Cut'
    { _cutHeaders = genesisBlockHeadersAtHeight v 0
    , _cutChainwebVersion = v
    }

-- -------------------------------------------------------------------------- --
-- Exceptions

data CutException
    = InvalidCutPair BlockHeader BlockHeader
    | NonMonotonicCutExtension (Expected BlockHash) (Actual BlockHash) BlockHeader
    | InvalidCutExtension BlockHeader
    deriving (Show, Eq, Ord, Generic)

instance Exception CutException

-- -------------------------------------------------------------------------- --
-- Cut Properties

-- | Check that a cut is correctly braided.
--
-- This check is only needed for external cuts. Note, that for imported cuts
-- this must be checked recursively.
--
-- The implementation of this check here is expensive, namely \(O(m)\), where
-- \(m\) is the size of the chain graph, i.e. the order of the graph times the
-- degree. Usually, a more efficient way to check this property for a new cut is
-- to performm a join that starts with a meet with a local cut, where the local
-- cut is know to have this property.
--
-- Thus, the functions in this section are mostly used for assertions in tests.
-- They also serve as reference implementations and documentation.
--
checkBraidingOfCut :: MonadThrow m => Cut -> m ()
checkBraidingOfCut = checkBraidingOfCutPairs . _cutAdjPairs
{-# INLINE checkBraidingOfCut #-}

-- | Check that a set of adjacent pairs of a cut is correctly braided.
--
checkBraidingOfCutPairs
    :: MonadThrow m
    => Foldable f
    => f (BlockHeader, BlockHeader)
    -> m ()
checkBraidingOfCutPairs = traverse_ (uncurry checkBraidingOfCutPair)
{-# INLINE checkBraidingOfCutPairs #-}

-- | Checks that directed adjacent pair in a cut is correctly braided.
--
checkBraidingOfCutPair
    :: MonadThrow m
    => BlockHeader
        -- ^ Source header
    -> BlockHeader
        -- ^ target header
    -> m ()
checkBraidingOfCutPair s t = do
    unless (absBlockHeightDiff s t <= 1) $ throwM $ InvalidCutPair s t
    unlessM (isBraidingOfCutPair s t) $ throwM (InvalidCutPair s t)

-- | Returns whether a directed adjacent pair in a cut is correctly braided.
--
-- * throws 'ChainNotAdjacentException'
--
-- Assuming that all headers in the cut have the same graph, the following
-- condition for all undirected adjacent pairs in the cut would inductively
-- guarantee a valid braiding:
--
-- @
-- isBraidingOfCutPair (AdjPair a b) = do
--    ab <- getAdjacentHash b a -- adjacent of a on chain of b
--    ba <- getAdjacentHash a b -- adajcent of b on chain of a
--    return
--        $! (_blockParent a == ba && _blockParent b == ab)
--        || ab == _blockHash b
--        || ba == _blockHash a
-- @
--
-- The actual implementation is a it more complex because headers of different
-- height in a cut may use a diferent chain graph during the transition from one
-- graph to another graph: instead of undirected adjacent pairs the condition
-- consideres directed adjacent pairs.
--
-- A corresponding function is currently implemented in 'Chainweb.Cut.Create'
--
-- TODO: unify the implementation of both. In particular make sure that the
-- function here that is used during validation is not more lenient than the
-- function used for extending cuts for mining.
--
isBraidingOfCutPair
    :: MonadThrow m
    => BlockHeader
        -- ^ Source header
    -> BlockHeader
        -- ^ target header
    -> m Bool
isBraidingOfCutPair a b = do
    ab <- getAdjacentHash b a -- adjacent of a on chain of b
    ba <- getAdjacentHash a b -- adajcent of b on chain of a
    return
        $! (_blockParent a == ba && _blockParent b == ab) -- same graph
        || (_blockHeight a > _blockHeight b) && ab == _blockHash b
        || (_blockHeight a < _blockHeight b) && True {- if same graph: ba == _blockHash a -}

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
-- FIXME: this must conform with 'isBraidingOfCutPair'. Double check that we
-- have test for this or check if the implementation can be shared.
--
-- TODO: do we have to check that the correct graph is used?
--
isMonotonicCutExtension
    :: HasCallStack
    => MonadThrow m
    => Cut
    -> BlockHeader
    -> m Bool
isMonotonicCutExtension c h = do
    checkBlockHeaderGraph h
    return $! monotonic && validBraiding
  where
    monotonic = _blockParent h == case c ^? ixg (_chainId h) . blockHash of
        Nothing -> error $ T.unpack $ "isMonotonicCutExtension.monotonic: missing parent in cut. " <> encodeToText h
        Just x -> x
    validBraiding = getAll $ ifoldMap
        (\cid -> All . validBraidingCid cid)
        (_getBlockHashRecord $ _blockAdjacentHashes h)

    validBraidingCid cid a
        | Just b <- c ^? ixg cid = _blockHash b == a || _blockParent b == a
        | _blockHeight h == genesisHeight v cid = a == genesisParentBlockHash v cid
        | otherwise = error $ T.unpack $ "isMonotonicCutExtension.validBraiding: missing adjacent parent on chain " <> sshow cid <> " in cut. " <> encodeToText h

    v = _chainwebVersion c

-- | Extend a cut with a block header. Throws 'InvalidCutExtension' if the block
-- header isn't a monotonic cut extension.
--
monotonicCutExtension
    :: MonadThrow m
    => Cut
    -> BlockHeader
    -> m Cut
monotonicCutExtension c h = tryMonotonicCutExtension c h >>= \case
    Nothing -> throwM $ InvalidCutExtension h
    Just x -> return x

-- | Extend a cut with a block header. Returns 'Nothing' the block header isn't
-- a monotonic cut extension.
--
tryMonotonicCutExtension
    :: MonadThrow m
    => Cut
    -> BlockHeader
    -> m (Maybe Cut)
tryMonotonicCutExtension c h = isMonotonicCutExtension c h >>= \case
    False -> return Nothing
    True -> return $ Just
        $! over unsafeCutHeaders extendChains
        $ set (unsafeCutHeaders . ix (_chainId h)) h c

-- -------------------------------------------------------------------------- --
-- Join

type DiffItem a = These a a

type JoinQueue a = H.Heap (H.Entry (BlockHeight, a) BlockHeader)

-- | This represents the Join of two cuts in an algrithmically convenient way.
--
data Join a = Join
    { _joinBase :: !Cut
        -- ^ the base of the join, the largest cut that is contained in both
        -- cuts, or when viewed as sets, the intersection.
    , _joinQueue :: !(JoinQueue a)
        -- ^ a queue of block headers from both cuts that allows construct
        -- the join cut from the join base.
    }

-- | This computes the join for cuts accross all chains.
--
-- If you want to compute a join for cuts that include only a subset of all
-- chains.
--
join
    :: Ord a
    => WebBlockHeaderDb
    -> (DiffItem BlockHeader -> DiffItem (Maybe a))
    -> Cut
    -> Cut
    -> IO (Join a)
join wdb f = join_ wdb f `on` _cutHeaders

-- | This merges two maps from ChainIds to BlockHeaders such that the result is
-- a Cut. Note, however, that the resulting cut contains only the chain ids from
-- the intersection of the input maps.
--
-- NOTE: For this to work as expected make sure that both inputs contain all
-- chains that should be present in the output.
--
-- Adds genesis blocks for chains that are not yet active. This purpose of this
-- is to make sure that all chains of both inputs are preserved in the join, so
-- that the appliation of the join contains all chains of the original cuts.
-- Otherwise the join would contain only the intersection of all chains and any
-- information/blocks in the other chains would be lost when applying the join.
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
    (m, h) <- foldM f (mempty, mempty) (zipChainIdMaps a' b')
    return $! Join (Cut' m (_chainwebVersion wdb)) h
  where
    (a', b') = joinChains a b

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

-- This can't fail because of missing dependencies. It can't fail because
-- of conflict.
--
-- Non-existing chains are stripped from the result.
--
applyJoin :: MonadThrow m => Join a -> m Cut
applyJoin m = over unsafeCutHeaders projectChains
    <$> foldM
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
-- Chains that aren't yet initialized are included in the join and later
-- stripped from the result.
--
-- If you want to compute a join for cuts that include only a subset of all
-- chains, make sure that @genesisBlockHeaders v@ only returns genesis headers
-- for those chains that you care about.
--
joinIntoHeavier
    :: WebBlockHeaderDb
    -> Cut
    -> Cut
    -> IO Cut
joinIntoHeavier wdb = joinIntoHeavier_ wdb `on` _cutHeaders

-- | Chains that aren't yet initialized are included in the join and later
-- stripped from the result.
--
-- If you want to compute a join for cuts that include only a subset of all
-- chains, make sure that @genesisBlockHeaders v@ only returns genesis headers
-- for those chains that you care about.
--
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
    return $! Cut' r (_chainwebVersion wdb)
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
