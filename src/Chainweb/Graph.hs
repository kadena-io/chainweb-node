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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
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
, toChainGraph
, validChainGraph
, adjacentChainIds
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

-- * Specific, Known Graphs

, KnownGraph(..)
, knownGraph
, singletonChainGraph
, pairChainGraph
, triangleChainGraph
, petersonChainGraph
, twentyChainGraph
, hoffmanSingletonGraph

) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData(..))
import Control.Lens (Getter, to, view)
import Control.Monad (unless, void)
import Control.Monad.Catch (Exception, MonadThrow(..))

import Data.Bits (xor)
import Data.Function (on)
import Data.Hashable (Hashable(..))
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
        :: Expected ChainId
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

chainGraphKnown :: Getter ChainGraph KnownGraph
chainGraphKnown = to _chainGraphKnown
{-# INLINE chainGraphKnown #-}

-- | This function is unsafe, it throws an error if the graph isn't a valid
-- chain graph. That's OK, since chaingraphs are hard-coded in the code and
-- won't change dynamically, except for during testing.
--
toChainGraph :: (a -> ChainId) -> KnownGraph -> G.DiGraph a -> ChainGraph
toChainGraph f kg g
    | validChainGraph c = ChainGraph
        { _chainGraphGraph = c
        , _chainGraphKnown = kg
        , _chainGraphShortestPathCache = G.shortestPathCache c
        , _chainGraphHash = hash c
        }
    | otherwise = error "the given graph is not a valid chain graph"
  where
    c = G.mapVertices f g
{-# INLINE toChainGraph #-}

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

adjacentChainIds
    :: HasChainId p
    => ChainGraph
    -> p
    -> HS.HashSet ChainId
adjacentChainIds (ChainGraph g _ _ _) cid = G.adjacents (_chainId cid) g
{-# INLINE adjacentChainIds #-}

-- -------------------------------------------------------------------------- --
-- Undirected Edges

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
        (Expected (G.vertices $ _chainGraphGraph $ _chainGraph g))
        (Actual (_chainId p))


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
data KnownGraph = Singleton | Pair | Triangle | Peterson | Twenty | HoffmanSingle
    deriving (Generic)
    deriving anyclass (NFData)

knownGraph :: KnownGraph -> ChainGraph
knownGraph Singleton = singletonChainGraph
knownGraph Pair = pairChainGraph
knownGraph Triangle = triangleChainGraph
knownGraph Peterson = petersonChainGraph
knownGraph Twenty = twentyChainGraph
knownGraph HoffmanSingle = hoffmanSingletonGraph

singletonChainGraph :: ChainGraph
singletonChainGraph = toChainGraph (unsafeChainId . int) Singleton G.singleton

pairChainGraph :: ChainGraph
pairChainGraph = toChainGraph (unsafeChainId . int) Pair G.pair

triangleChainGraph :: ChainGraph
triangleChainGraph = toChainGraph (unsafeChainId . int) Triangle G.triangle

petersonChainGraph :: ChainGraph
petersonChainGraph = toChainGraph (unsafeChainId . int) Peterson G.petersonGraph

twentyChainGraph :: ChainGraph
twentyChainGraph = toChainGraph (unsafeChainId . int) Twenty G.twentyChainGraph

hoffmanSingletonGraph :: ChainGraph
hoffmanSingletonGraph = toChainGraph (unsafeChainId . int) HoffmanSingle G.hoffmanSingleton
