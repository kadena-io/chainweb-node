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
, chainIds
, chainIds_
, checkWebChainId
, checkAdjacentChainIds

-- * Some Graphs

, singletonChainGraph
, pairChainGraph
, petersonChainGraph
) where

import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Bits
import Data.Function
import Data.Hashable
import qualified Data.HashSet as HS
import Data.Kind
import Data.Reflection hiding (int)

import GHC.Generics hiding (to)

import Numeric.Natural

-- internal imports

import Chainweb.ChainId
import Chainweb.Utils

import Data.DiGraph hiding (diameter, order, shortestPath, size)
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
    { _chainGraphGraph :: !(DiGraph ChainId)
    , _chainGraphShortestPathCache :: {- lazy -} ShortestPathCache ChainId
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

-- | This function is unsafe, it throws an error if the graph isn't a valid
-- chain graph. That's OK, since chaingraphs are hard-coded in the code and
-- won't change dinamically, except for during testing.
--
toChainGraph :: (a -> ChainId) -> DiGraph a -> ChainGraph
toChainGraph f g
    | validChainGraph c = ChainGraph
        { _chainGraphGraph = c
        , _chainGraphShortestPathCache = shortestPathCache c
        , _chainGraphHash = hash c
        }
    | otherwise = error "the given graph is not a valid chain graph"
  where
    c = mapVertices f g
{-# INLINE toChainGraph #-}

-- | A valid chain graph is symmetric, regular, and the out-degree
-- is at least 1 if the graph has at least two vertices.
--
-- These properties imply that the graph is strongly connected.
--
validChainGraph :: DiGraph ChainId -> Bool
validChainGraph g
    = isDiGraph g
    && isSymmetric g
    && isRegular g
    && (G.order g <= 1 || G.size g >= 1)
{-# INLINE validChainGraph #-}

adjacentChainIds
    :: HasChainId p
    => ChainGraph
    -> p
    -> HS.HashSet ChainId
adjacentChainIds (ChainGraph g _ _) cid = adjacents (_chainId cid) g
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
adjs = HS.map (uncurry Adj) . edges . _chainGraphGraph
{-# INLINE adjs #-}

adjsOfVertex
    :: HasChainId p
    => ChainGraph
    -> p
    -> HS.HashSet (AdjPair ChainId)
adjsOfVertex g a = HS.map (Adj (_chainId a)) $ adjacentChainIds g a

-- -------------------------------------------------------------------------- --
-- Properties

size :: ChainGraph -> Natural
size = G.size . _chainGraphGraph

order :: ChainGraph -> Natural
order = G.order . _chainGraphGraph

degree :: ChainGraph -> Natural
degree = G.minOutDegree . _chainGraphGraph

diameter :: ChainGraph -> Natural
diameter = fromJuste . diameter_ . _chainGraphShortestPathCache
    -- this is safe, because we know that the graph is strongly connected

shortestPath :: ChainId -> ChainId -> ChainGraph -> [ChainId]
shortestPath src trg = fromJuste
    . shortestPath_ src trg
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

chainIds :: Given ChainGraph => HS.HashSet ChainId
chainIds = vertices (_chainGraphGraph given)
{-# INLINE chainIds #-}

chainIds_ :: ChainGraph -> HS.HashSet ChainId
chainIds_ = vertices . _chainGraphGraph
{-# INLINE chainIds_ #-}

-- | Given a 'ChainGraph' @g@, @checkWebChainId p@ checks that @p@ is a vertex
-- in @g@.
--
checkWebChainId :: MonadThrow m => HasChainGraph g => HasChainId p => g -> p -> m ()
checkWebChainId g p = unless (isWebChain g p)
    $ throwM $ ChainNotInChainGraphException
        (Expected (vertices $ _chainGraphGraph $ _chainGraph g))
        (Actual (_chainId p))


isWebChain :: HasChainGraph g => HasChainId p => g -> p -> Bool
isWebChain g p = isVertex (_chainId p) (_chainGraphGraph $ _chainGraph g)
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
        (Actual $ adjacents (_chainId cid) (_chainGraphGraph $ _chainGraph g))
    return (getExpected expectedAdj)

-- -------------------------------------------------------------------------- --
-- Some Graphs

singletonChainGraph :: ChainGraph
singletonChainGraph = toChainGraph (testChainId . int) singleton

pairChainGraph :: ChainGraph
pairChainGraph = toChainGraph (testChainId . int) pair

petersonChainGraph :: ChainGraph
petersonChainGraph = toChainGraph (testChainId . int) petersonGraph
