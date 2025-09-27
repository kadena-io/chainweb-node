{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
, cutToTextShort
, cutDiffToTextShort
, _cutHeaders
, cutHeaders
, _cutMap
, cutMap
, _cutHeight
, cutHeight
, _cutWeight
, cutWeight
, _cutMinHeight
, cutMinHeight
, _cutMaxHeight
, cutMaxHeight
, _cutIsTransition
, cutIsTransition

, _cutAdjPairs
, cutAdjPairs
, cutAdjs
, lookupCutM
, unsafeMkCut
, chainHeights

-- * Exceptions
, CutException(..)

-- * Genesis Cut
, genesisCut

-- * Checks
, checkBraidingOfCut
, checkBraidingOfCutPairs
, checkBraidingOfCutPair
, isBraidingOfCutPair

) where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Parent
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Control.DeepSeq
import Control.Exception hiding (catch)
import Control.Lens hiding ((:>), (.=))
import Control.Monad hiding (join)
import Control.Monad.Catch
import Data.Foldable
import Data.Function
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prelude hiding (lookup)

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
-- 4. 'limitCut' and 'tryLimitCut'.
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
    { _cutHeaders' :: !(HM.HashMap ChainId BlockHeader)

    -- Memoize properties that have linear compute cost
    , _cutHeight' :: {- lazy -} CutHeight
    , _cutMinHeight' :: {- lazy -} BlockHeight
    , _cutMaxHeight' :: {- lazy -} BlockHeight
    , _cutWeight' :: {- lazy -} BlockWeight
    , _cutIsTransition' :: {- lazy -} Bool
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

_cutHeaders :: Cut -> HM.HashMap ChainId BlockHeader
_cutHeaders = _cutHeaders'
{-# INLINE cutHeaders #-}

cutHeaders :: Getter Cut (HM.HashMap ChainId BlockHeader)
cutHeaders = to _cutHeaders
{-# INLINE _cutHeaders #-}

_cutMap :: Cut -> HM.HashMap ChainId BlockHeader
_cutMap = _cutHeaders
{-# INLINE _cutMap #-}

cutMap :: Getter Cut (HM.HashMap ChainId BlockHeader)
cutMap = cutHeaders
{-# INLINE cutMap #-}

_cutWeight :: Cut -> BlockWeight
_cutWeight = _cutWeight'
{-# INLINE _cutWeight #-}

cutWeight :: Getter Cut BlockWeight
cutWeight = to _cutWeight
{-# INLINE cutWeight #-}

_cutHeight :: Cut -> CutHeight
_cutHeight = _cutHeight'
{-# INLINE _cutHeight #-}

cutHeight :: Getter Cut CutHeight
cutHeight = to _cutHeight
{-# INLINE cutHeight #-}

_cutMinHeight :: Cut -> BlockHeight
_cutMinHeight = _cutMinHeight'
{-# INLINE _cutMinHeight #-}

cutMinHeight :: Getter Cut BlockHeight
cutMinHeight = to _cutMinHeight
{-# INLINE cutMinHeight #-}

_cutMaxHeight :: Cut -> BlockHeight
_cutMaxHeight = _cutMaxHeight'
{-# INLINE _cutMaxHeight #-}

cutMaxHeight :: Getter Cut BlockHeight
cutMaxHeight = to _cutMaxHeight
{-# INLINE cutMaxHeight #-}

_cutIsTransition :: Cut -> Bool
_cutIsTransition = _cutIsTransition'
{-# INLINE _cutIsTransition #-}

cutIsTransition :: Getter Cut Bool
cutIsTransition = to _cutIsTransition
{-# INLINE cutIsTransition #-}

-- | The chain graph is the graph at the /minimum/ height of the block headers
-- in the cut.
--
instance HasVersion => HasChainGraph Cut where
    _chainGraph c = chainGraphAt (_cutMinHeight c)
    {-# INLINE _chainGraph #-}

type instance Index Cut = ChainId
type instance IxValue Cut = BlockHeader

instance IxedGet Cut where
    ixg i = cutHeaders . ix i
    {-# INLINE ixg #-}

lookupCutM
    :: (MonadThrow m, HasVersion)
    => HasChainId cid
    => cid
    -> Cut
    -> m BlockHeader
lookupCutM cid c = firstOf (ixg (_chainId cid)) c
    ??? ChainNotInChainGraphException
        (Expected chainIds)
        (Actual (_chainId cid))

unsafeMkCut :: HasVersion => HM.HashMap ChainId BlockHeader -> Cut
unsafeMkCut hdrs = Cut'
    { _cutHeaders' = hdrs
    , _cutHeight' = int $ sum $ view blockHeight <$> hdrs
    , _cutWeight' = sum $ view blockWeight <$> hdrs
    , _cutMinHeight' = minimum $ view blockHeight <$> hdrs
    , _cutMaxHeight' = maximum $ view blockHeight <$> hdrs
    , _cutIsTransition' =  minheight < lastGraphChange maxheight
    }
  where
    minheight = minimum $ view blockHeight <$> hdrs
    maxheight = maximum $ view blockHeight <$> hdrs

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
chainHeights = fmap (view blockHeight) . toList . _cutHeaders
{-# INLINE chainHeights #-}

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
    :: HasVersion
    => Cut
genesisCut = unsafeMkCut (genesisBlockHeadersAtHeight 0)

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
--    ba <- getAdjacentHash a b -- adjacent of b on chain of a
--    return
--        $! (view blockParent a == ba && view blockParent b == ab)
--        || ab == view blockHash b
--        || ba == view blockHash a
-- @
--
-- The actual implementation is a it more complex because headers of different
-- height in a cut may use a different chain graph during the transition from one
-- graph to another graph: instead of undirected adjacent pairs the condition
-- considers directed adjacent pairs.
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
    ba <- getAdjacentHash a b -- adjacent of b on chain of a
    return
        $! (view blockParent a == ba && view blockParent b == ab) -- same graph
        || (view blockHeight a > view blockHeight b) && ab == Parent (view blockHash b)
        || (view blockHeight a < view blockHeight b) && True {- if same graph: ba == view blockHash a -}

cutToTextShort :: Cut -> [Text]
cutToTextShort c =
    [ blockHeaderShortDescription bh
    | (_, bh) <- List.sortOn fst $ HM.toList (_cutHeaders c)
    ]

cutDiffToTextShort :: Cut -> Cut -> [Text]
cutDiffToTextShort c c' =
    [ T.unwords
        [ maybe "No block" blockHeaderShortDescription bh
        , "->"
        , maybe "No block" blockHeaderShortDescription bh'
        ]
    | cid <- List.sort $ HM.keys $ HM.union (_cutHeaders c) (_cutHeaders c')
    , let bh = HM.lookup cid (_cutHeaders c)
    , let bh' = HM.lookup cid (_cutHeaders c')
    , bh /= bh'
    ]
