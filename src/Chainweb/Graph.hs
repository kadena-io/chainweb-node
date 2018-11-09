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

-- * Checks with a given chain graph

, isWebChain
, chainIds
, checkWebChainId
, checkAdjacentChainIds

-- * Some Graphs

, singletonChainGraph
, petersonChainGraph
) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Hashable
import qualified Data.HashSet as HS
import Data.Kind
import Data.Reflection hiding (int)

import GHC.Generics hiding (to)

-- internal imports

import Chainweb.Utils
import Chainweb.ChainId

import Data.DiGraph

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
    AdjacentChainMissmatch
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

type ChainGraph = DiGraph ChainId

toChainGraph :: (a -> ChainId) -> DiGraph a -> ChainGraph
toChainGraph = mapVertices
{-# INLINE toChainGraph #-}

validChainGraph :: DiGraph ChainId -> Bool
validChainGraph g = isDiGraph g && isSymmetric g && isRegular g
{-# INLINE validChainGraph #-}

adjacentChainIds
    :: HasChainId p
    => ChainGraph
    -> p
    -> HS.HashSet ChainId
adjacentChainIds g cid = adjacents (_chainId cid) g
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

adjs
    :: ChainGraph
    -> HS.HashSet (AdjPair ChainId)
adjs = HS.map (uncurry Adj) . edges
{-# INLINE adjs #-}

adjsOfVertex
    :: HasChainId p
    => ChainGraph
    -> p
    -> HS.HashSet (AdjPair ChainId)
adjsOfVertex g a = HS.map (Adj (_chainId a)) $ adjacentChainIds g a

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
chainIds = vertices given
{-# INLINE chainIds #-}

-- | Given a 'ChainGraph' @g@, @checkWebChainId p@ checks that @p@ is a vertex
-- in @g@.
--
checkWebChainId :: MonadThrow m => Given ChainGraph => HasChainId p => p -> m ()
checkWebChainId p = unless (isWebChain p)
    $ throwM $ ChainNotInChainGraphException
        (Expected (vertices given))
        (Actual (_chainId p))

isWebChain :: Given ChainGraph => HasChainId p => p -> Bool
isWebChain p = isVertex (_chainId p) given
{-# INLINE isWebChain #-}

-- | Given a 'ChainGraph' @g@, @checkAdjacentChainIds cid as@ checks that the
-- 'ChainId' cid is in @g@ and the set of adjacents chain ids of @cid@ is the
-- expected set @as@.
--
checkAdjacentChainIds
    :: MonadThrow m
    => Given ChainGraph
    => HasChainId cid
    => HasChainId adj
    => cid
    -> Expected (HS.HashSet adj)
    -> m (HS.HashSet adj)
checkAdjacentChainIds cid expectedAdj = do
    checkWebChainId cid
    void $ check AdjacentChainMissmatch
        (HS.map _chainId <$> expectedAdj)
        (Actual $ adjacents (_chainId cid) given)
    return (getExpected expectedAdj)

-- -------------------------------------------------------------------------- --
-- Some Graphs

singletonChainGraph :: ChainGraph
singletonChainGraph = toChainGraph (testChainId . int) singleton

petersonChainGraph :: ChainGraph
petersonChainGraph = toChainGraph (testChainId . int) petersonGraph
