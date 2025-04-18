{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module: Chainweb.Graph
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A chain graph is
--
-- * directed
-- * regular
-- * symmetric
-- * irreflexive
--
module Chainweb.Graph
(
-- * Exceptions
  ChainGraphException(..)

-- * Chain Graph

, ChainGraph
, chainGraphKnown
, chainGraphGraph
, validChainGraph
, adjacentChainIds
, toAdjacencySets
, HasChainGraph(..)

-- * Undirected Edges
, AdjPair
, pattern Adj
, _getAdjPair
, adjs
, adjsOfVertex

-- * Graph Properties
, shortestPath
, diameter
, size
, order
, degree

-- * Checks with a given chain graph

, isWebChain
, graphChainIds
, checkWebChainId
, checkAdjacentChainIds

-- * Known Graphs
, KnownGraph(..)
, knownGraph

-- * Memoize Known Chain Graphs

, knownChainGraph
, singletonChainGraph
, pairChainGraph
, triangleChainGraph
, petersenChainGraph
, twentyChainGraph
, hoffmanSingletonChainGraph

) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData(..))
import Control.Lens (Getter, to, view)
import Control.Monad (unless, void)
import Control.Monad.Catch (Exception, MonadThrow(..))

import Data.Bits (xor)
import Data.Function (on)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Kind (Type)

import GHC.Generics hiding (to)

import Numeric.Natural

-- internal imports

import Chainweb.ChainId (ChainId, HasChainId(..), unsafeChainId)
import Chainweb.Utils

import qualified Data.DiGraph as G

-- -------------------------------------------------------------------------- --
-- Exceptions

-- | This exceptions are not about the properties of the graph itself
-- but about properties of enties (BlockHeader graph) that are constrained
-- by this graph. So, maybe we should move this and the respective checks
-- to the place where those enties are defined and rename these exceptions
-- accordingly. However, keeping it here remove code duplication.
--
data ChainGraphException :: Type where
    ChainNotInChainGraphException
        :: Expected (HS.HashSet ChainId)
        -> Actual ChainId
        -> ChainGraphException
    AdjacentChainMismatch
        :: Expected (HS.HashSet ChainId)
        -> Actual (HS.HashSet ChainId)
        -> ChainGraphException
    ChainNotAdjacentException
        :: ChainId
        -> Expected ChainId
        -> Actual (HS.HashSet ChainId)
        -> ChainGraphException
    deriving (Show, Eq, Generic)

instance Exception ChainGraphException

-- -------------------------------------------------------------------------- --
-- Chainweb Graph

data ChainGraph = ChainGraph
    { _chainGraphGraph :: !(G.DiGraph ChainId)
    , _chainGraphKnown :: !KnownGraph
    , _chainGraphShortestPathCache :: {- lazy -} G.ShortestPathCache ChainId
    , _chainGraphHash :: {- lazy -} Int
    }
    deriving (Generic)
    deriving anyclass (NFData)

instance Show ChainGraph where
    show = show . _chainGraphGraph

instance Eq ChainGraph where
    (==) = (==) `on` (_chainGraphHash &&& _chainGraphGraph)

instance Ord ChainGraph where
    compare = compare `on` (_chainGraphHash &&& _chainGraphGraph)

instance Hashable ChainGraph where
    hashWithSalt s = xor s . _chainGraphHash

instance HasTextRepresentation ChainGraph where
    toText = toText . _chainGraphKnown
    fromText = fmap knownChainGraph . fromText

    {-# INLINE toText #-}
    {-# INLINE fromText #-}

chainGraphKnown :: Getter ChainGraph KnownGraph
chainGraphKnown = to _chainGraphKnown
{-# INLINE chainGraphKnown #-}

chainGraphGraph :: Getter ChainGraph (G.DiGraph ChainId)
chainGraphGraph = to _chainGraphGraph
{-# INLINE chainGraphGraph #-}

-- | A valid chain graph is symmetric, regular, and the out-degree
-- is at least 1 if the graph has at least two vertices.
--
-- These properties imply that the graph is strongly connected.
--
validChainGraph :: G.DiGraph ChainId -> Bool
validChainGraph g
    = G.isDiGraph g
    && G.isSymmetric g
    && G.isRegular g
    && (G.order g <= 1 || G.symSize g >= 1)
{-# INLINE validChainGraph #-}

-- | Returns an empty set if the chain id is not in the graph
--
adjacentChainIds
    :: HasChainId p
    => ChainGraph
    -> p
    -> HS.HashSet ChainId
adjacentChainIds graph@(ChainGraph g _ _ _) cid
    | isWebChain graph cid = G.adjacents (_chainId cid) g
    | otherwise = mempty
{-# INLINE adjacentChainIds #-}

-- | Return the adjacency set representation of the underlying graph
--
toAdjacencySets :: ChainGraph -> HM.HashMap ChainId (HS.HashSet ChainId)
toAdjacencySets g = G.adjacencySets (_chainGraphGraph g)

-- -------------------------------------------------------------------------- --
-- Undirected Edges

-- | Undirected Edge in a Chain Graph
--
newtype AdjPair a = AdjPair { _getAdjPair :: (a, a) }
    deriving stock (Show, Ord, Eq, Generic, Functor)
    deriving anyclass (Hashable)

pattern Adj :: HasChainId a => a -> a -> AdjPair a
pattern Adj a b <- AdjPair (a, b)
  where
    Adj a b
        | _chainId a < _chainId b = AdjPair (a,b)
        | otherwise = AdjPair (b,a)
{-# COMPLETE Adj #-}

adjs
    :: ChainGraph
    -> HS.HashSet (AdjPair ChainId)
adjs = HS.map (uncurry Adj) . G.edges . _chainGraphGraph
{-# INLINE adjs #-}

adjsOfVertex
    :: HasChainId p
    => ChainGraph
    -> p
    -> HS.HashSet (AdjPair ChainId)
adjsOfVertex g a = HS.map (Adj (_chainId a)) $ adjacentChainIds g a

-- -------------------------------------------------------------------------- --
-- Properties

-- | The 'size' of the graph refers to the number of /edges/ |E| of a given
-- graph.
--
size :: ChainGraph -> Natural
size = (`div` 2) . G.size . _chainGraphGraph
    -- A chaingraph is guaranteed to be symmetric. @G.symSize@ is less efficient
    -- than @(`div` 2) . G.size@, because the former computes the symmetric
    -- closure of the graph, while the latter assumes symmetry.

-- | The 'order' of the graph refers to the number of /vertices/ |V| of a given
-- graph.
--
order :: ChainGraph -> Natural
order = G.order . _chainGraphGraph

degree :: ChainGraph -> Natural
degree = G.minOutDegree . _chainGraphGraph

diameter :: ChainGraph -> Natural
diameter = fromJuste . G.diameter_ . _chainGraphShortestPathCache
    -- this is safe, because we know that the graph is strongly connected

shortestPath :: ChainId -> ChainId -> ChainGraph -> [ChainId]
shortestPath src trg = fromJuste
    . G.shortestPath_ src trg
    . _chainGraphShortestPathCache
    -- this is safe, because we know that the graph is strongly connected

-- -------------------------------------------------------------------------- --
-- HasChainGraph

class HasChainGraph a where
    _chainGraph :: a -> ChainGraph
    chainGraph :: Getter a ChainGraph

    _chainGraph = view chainGraph
    {-# INLINE _chainGraph #-}

    chainGraph = to _chainGraph
    {-# INLINE chainGraph #-}

    {-# MINIMAL _chainGraph | chainGraph #-}

instance HasChainGraph ChainGraph where
    _chainGraph = id
    {-# INLINE _chainGraph #-}

-- -------------------------------------------------------------------------- --
-- Checks with a given Graphs

graphChainIds :: ChainGraph -> HS.HashSet ChainId
graphChainIds = G.vertices . _chainGraphGraph
{-# INLINE graphChainIds #-}

-- | Given a 'ChainGraph' @g@, @checkWebChainId p@ checks that @p@ is a vertex
-- in @g@.
--
checkWebChainId :: MonadThrow m => HasChainGraph g => HasChainId p => g -> p -> m ()
checkWebChainId g p = unless (isWebChain g p)
    $ throwM $ ChainNotInChainGraphException
        (Expected (graphChainIds $ _chainGraph g))
        (Actual (_chainId p))

-- | Returns whether the given chain is a vertex in the chain graph
--
isWebChain :: HasChainGraph g => HasChainId p => g -> p -> Bool
isWebChain g p = G.isVertex (_chainId p) (_chainGraphGraph $ _chainGraph g)
{-# INLINE isWebChain #-}

-- | Given a 'ChainGraph' @g@, @checkAdjacentChainIds cid as@ checks that the
-- 'ChainId' cid is in @g@ and the set of adjacents chain ids of @cid@ is the
-- expected set @as@.
--
checkAdjacentChainIds
    :: MonadThrow m
    => HasChainGraph g
    => HasChainId cid
    => HasChainId adj
    => g
        -- ^ For all block but genesis blocks, this should be the graph of
        -- the parent block.
    -> cid
    -> Expected (HS.HashSet adj)
    -> m (HS.HashSet adj)
checkAdjacentChainIds g cid expectedAdj = do
    checkWebChainId g cid
    void $ check AdjacentChainMismatch
        (HS.map _chainId <$> expectedAdj)
        (Actual $ G.adjacents (_chainId cid) (_chainGraphGraph $ _chainGraph g))
    return $! getExpected expectedAdj

-- -------------------------------------------------------------------------- --
-- Some Graphs

-- | Graphs which have known, specific, intended meaning for Chainweb.
--
data KnownGraph
    = Singleton
        -- ^ degree 0, diameter 0, order 1
    | Pair
        -- ^ degree 1, diameter 1, order 2
    | Triangle
        -- ^ degree 2, diameter 1, order 3
    | Petersen
        -- ^ degree 3, diameter 2, order 10
    | Twenty
        --  degree 3, diameter 3, order 20
    | HoffmanSingleton
        -- ^ degree 7, diameter 2, order 50
    | D3K4
        -- ^ degree 3, diameter 4, order 38
    | D4K3
        -- ^ degree 4, diameter 3, order 41
    | D4K4
        -- ^ degree 4, diameter 4, order 98
    | D5K3
        -- ^ degree 5, diameter 3, order  72
    | D5K4
        -- ^ degree 5, diameter 4, order  212
    deriving (Generic, Enum, Bounded)
    deriving anyclass (NFData)

instance HasTextRepresentation KnownGraph where
    toText Singleton = "singleton"
    toText Pair = "pair"
    toText Triangle = "triangle"
    toText Petersen = "petersen"
    toText Twenty = "twenty"
    toText HoffmanSingleton = "hoffman"
    toText D3K4 = "d3k4"
    toText D4K3 = "d4k3"
    toText D4K4 = "d4k4"
    toText D5K3 = "d5k3"
    toText D5K4 = "d5k4"

    fromText "singleton" = return Singleton
    fromText "pair" = return Pair
    fromText "triangle" = return Triangle
    fromText "petersen" = return Petersen
    fromText "twenty" = return Twenty
    fromText "hoffman" = return HoffmanSingleton
    fromText "d3k4" = return D3K4
    fromText "d4k3" = return D4K3
    fromText "d4k4" = return D4K4
    fromText "d5k3" = return D5K3
    fromText "d5k4" = return D5K4
    fromText x = throwM $ TextFormatException $ "unknown KnownGraph: " <> x

    {-# INLINE toText #-}
    {-# INLINE fromText #-}

knownGraph :: KnownGraph -> G.DiGraph Int
knownGraph Singleton = G.singleton
knownGraph Pair = G.pair
knownGraph Triangle = G.triangle
knownGraph Petersen = G.petersenGraph
knownGraph Twenty = G.twentyChainGraph
knownGraph HoffmanSingleton = G.hoffmanSingleton
knownGraph D3K4 = G.d3k4
knownGraph D4K3 = G.d4k3
knownGraph D4K4 = G.d4k4
knownGraph D5K3 = G.d5k3
knownGraph D5K4 = G.d5k4

-- -------------------------------------------------------------------------- --
-- Memoized Known Chain Graphs

-- | This function is unsafe, it throws an error if the graph isn't a valid
-- chain graph. That's OK, since chaingraphs are hard-coded in the code and
-- won't change dynamically, except for during testing.
--
toChainGraph :: KnownGraph -> ChainGraph
toChainGraph kg
    | validChainGraph c = ChainGraph
        { _chainGraphGraph = c
        , _chainGraphKnown = kg
        , _chainGraphShortestPathCache = G.shortestPathCache c
        , _chainGraphHash = hash c
        }
    | otherwise = error "the given graph is not a valid chain graph"
  where
    c = G.mapVertices (unsafeChainId . int) $! knownGraph kg
{-# INLINE toChainGraph #-}

knownChainGraph :: KnownGraph -> ChainGraph
knownChainGraph Singleton = singletonChainGraph
knownChainGraph Pair = pairChainGraph
knownChainGraph Triangle = triangleChainGraph
knownChainGraph Petersen = petersenChainGraph
knownChainGraph Twenty = twentyChainGraph
knownChainGraph HoffmanSingleton = hoffmanSingletonChainGraph
knownChainGraph D3K4 = d3k4ChainGraph
knownChainGraph D4K3 = d4k3ChainGraph
knownChainGraph D4K4 = d4k4ChainGraph
knownChainGraph D5K3 = d5k3ChainGraph
knownChainGraph D5K4 = d5k4ChainGraph

singletonChainGraph :: ChainGraph
singletonChainGraph = toChainGraph Singleton
{-# NOINLINE singletonChainGraph #-}

pairChainGraph :: ChainGraph
pairChainGraph = toChainGraph Pair
{-# NOINLINE pairChainGraph #-}

triangleChainGraph :: ChainGraph
triangleChainGraph = toChainGraph Triangle
{-# NOINLINE triangleChainGraph #-}

petersenChainGraph :: ChainGraph
petersenChainGraph = toChainGraph Petersen
{-# NOINLINE petersenChainGraph #-}

twentyChainGraph :: ChainGraph
twentyChainGraph = toChainGraph Twenty
{-# NOINLINE twentyChainGraph #-}

hoffmanSingletonChainGraph :: ChainGraph
hoffmanSingletonChainGraph = toChainGraph HoffmanSingleton
{-# NOINLINE hoffmanSingletonChainGraph #-}

d3k4ChainGraph :: ChainGraph
d3k4ChainGraph = toChainGraph D3K4
{-# NOINLINE d3k4ChainGraph #-}

d4k3ChainGraph :: ChainGraph
d4k3ChainGraph = toChainGraph D4K3
{-# NOINLINE d4k3ChainGraph #-}

d4k4ChainGraph :: ChainGraph
d4k4ChainGraph = toChainGraph D4K4
{-# NOINLINE d4k4ChainGraph #-}

d5k3ChainGraph :: ChainGraph
d5k3ChainGraph = toChainGraph D5K3
{-# NOINLINE d5k3ChainGraph #-}

d5k4ChainGraph :: ChainGraph
d5k4ChainGraph = toChainGraph D5K4
{-# NOINLINE d5k4ChainGraph #-}

