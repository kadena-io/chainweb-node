{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: DiGraph
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.DiGraph
( DiGraph
, DiEdge
, adjacencySets
, vertices
, edges
, adjacents
, incidents
, isDiGraph

-- * Creation of Graphs

, insertEdge
, insertVertex
, mapVertices
, union
, fromList
, fromEdges
, transpose
, symmetric

-- * Graphs
, singleton
, clique
, pair
, triangle
, cycle
, diCycle
, line
, diLine
, petersonGraph
, twentyChainGraph
, hoffmanSingleton

-- * Properties
, order
, size
, isAdjacent
, isRegular
, isSymmetric
, isIrreflexive
, outDegree
, inDegree
, maxOutDegree
, maxInDegree
, minOutDegree
, minInDegree
, isEdge
, isVertex
, diameter
) where

import Control.Arrow
import Control.Monad

import Data.Foldable
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup
import Data.Traversable
import Data.Tuple

import GHC.Generics

import Numeric.Natural

import Prelude hiding (cycle)

-- internal modules

import qualified Data.DiGraph.FloydWarshall as FW

-- -------------------------------------------------------------------------- --
-- Utils

int :: Integral a => Num b => a -> b
int = fromIntegral

-- -------------------------------------------------------------------------- --
-- Graph

type DiEdge a = (a, a)

-- | Adjacency list representation of directed graphs.
--
-- It is assumed that each target of an edge is also explicitely a node in the
-- graph.
--
-- It is not generally required that graphs are irreflexive, but all concrete
-- graphs that are defined in this module are irreflexive.
--
-- Undirected graphs are represented as symmetric directed graphs.
--
newtype DiGraph a = DiGraph { unGraph :: HM.HashMap a (HS.HashSet a) }
    deriving (Show, Eq, Ord, Generic)

instance (Hashable a, Eq a) => Semigroup (DiGraph a) where
    (DiGraph a) <> (DiGraph b) = DiGraph (HM.unionWith (<>) a b)

instance (Hashable a, Eq a) => Monoid (DiGraph a) where
    mempty = DiGraph mempty
    mappend = (<>)

adjacencySets :: DiGraph a -> HM.HashMap a (HS.HashSet a)
adjacencySets = unGraph

-- | A predicate that asserts that every target of an edge is also a vertex in
-- the graph.
--
isDiGraph :: Eq a => Hashable a => DiGraph a -> Bool
isDiGraph g@(DiGraph m) = HS.null (HS.unions (HM.elems m) `HS.difference` vertices g)

vertices :: DiGraph a -> HS.HashSet a
vertices = HS.fromMap . HM.map (const ()) . unGraph

order :: DiGraph a -> Natural
order = int . HS.size . vertices

edges :: Eq a => Hashable a => DiGraph a -> HS.HashSet (DiEdge a)
edges = HS.fromList . concatMap (traverse HS.toList) . HM.toList . unGraph

-- | Directed Size
--
diSize :: Eq a => Hashable a => DiGraph a -> Natural
diSize = int . HS.size . edges

-- |
--
size :: Eq a => Hashable a => DiGraph a -> Natural
size g = diSize (symmetric g) `div` 2

adjacents :: Eq a => Hashable a => a -> DiGraph a -> HS.HashSet a
adjacents a (DiGraph g) = g HM.! a

incidents :: Eq a => Hashable a => a -> DiGraph a -> [(a, a)]
incidents a g = [ (a, b) | b <- toList (adjacents a g) ]

--

fromList :: Eq a => Hashable a => [(a,[a])] -> DiGraph a
fromList = DiGraph . HM.map HS.fromList . HM.fromList

fromEdges :: Eq a => Hashable a => HS.HashSet (a, a) -> DiGraph a
fromEdges = foldr insertEdge mempty

union :: Eq a => Hashable a => DiGraph a -> DiGraph a -> DiGraph a
union = (<>)

mapVertices :: Eq b => Hashable b => (a -> b) -> DiGraph a -> DiGraph b
mapVertices f = DiGraph . HM.fromList . fmap (f *** HS.map f) . HM.toList . unGraph

transpose :: Eq a => Hashable a => DiGraph a -> DiGraph a
transpose g = (DiGraph $ const mempty <$> unGraph g)
    `union` (fromEdges . HS.map swap $ edges g)

-- | Symmetric closure of a directe graph.
--
symmetric :: Eq a => Hashable a => DiGraph a -> DiGraph a
symmetric g = g <> transpose g

-- | Insert an edge. Returns the graph unmodified if the edge
-- is already in the graph. Non-existing vertices are added.
--
insertEdge :: Eq a => Hashable a => DiEdge a -> DiGraph a -> DiGraph a
insertEdge (a,b) = DiGraph
    . HM.insertWith (<>) a [b]
    . HM.insertWith (<>) b []
    . unGraph

-- | Insert a vertex. Returns the graph unmodified if the vertex
-- is already in the graph.
--
insertVertex :: Eq a => Hashable a => a -> DiGraph a -> DiGraph a
insertVertex a = DiGraph . HM.insertWith (<>) a [] . unGraph

-- -------------------------------------------------------------------------- --
-- Concrete Graph

-- | undirected clique
--
clique :: Natural -> DiGraph Int
clique i = fromList
    [ (a, b)
    | a <- [0 .. int i - 1]
    , let b = [ x | x <- [0 .. int i - 1] , x /= a ]
    ]

singleton :: DiGraph Int
singleton = clique 1

-- | Undirected pair graph
--
pair :: DiGraph Int
pair = clique 2

-- | Undirected triange graph
--
triangle :: DiGraph Int
triangle = clique 3

-- | Directed cycle
--
diCycle :: Natural -> DiGraph Int
diCycle n = fromList [ (a, [(a + 1) `mod` int n]) | a <- [0 .. int n - 1] ]

-- | Undirected cycle
--
cycle :: Natural -> DiGraph Int
cycle = symmetric . diCycle

-- | Directed line graph
--
diLine :: Natural -> DiGraph Int
diLine n = fromList [ (a, [ a + 1 | a /= int n - 1]) | a <- [0 .. int n - 1] ]

-- | Undirected line graph
--
line :: Natural -> DiGraph Int
line = symmetric . diLine

petersonGraph :: DiGraph Int
petersonGraph = DiGraph
    [ (0, [2,3,5])
    , (1, [3,4,6])
    , (2, [4,0,7])
    , (3, [0,1,8])
    , (4, [1,2,9])
    , (5, [0,6,9])
    , (6, [1,5,7])
    , (7, [2,6,8])
    , (8, [3,7,9])
    , (9, [4,8,5])
    ]

twentyChainGraph :: DiGraph Int
twentyChainGraph  = pentagram `union` pentagon1 `union` pentagon2 `union` connections
  where
    pentagram = mapVertices (+ 5) $ pentagon2pentagram $ cycle 5
    pentagon1 = mapVertices (+ 10) $ cycle 5
    pentagon2 = mapVertices (+ 15) $ cycle 5
    connections = fromEdges $ HS.fromList $ mconcat
        [ [(i, x), (x, i)]
        | i <- [0..4]
        , x <- [i + 5, i + 10, i + 15]
        ]
    pentagon2pentagram = mapVertices $ \case
        0 -> 0
        1 -> 3
        2 -> 1
        3 -> 4
        4 -> 2
        _ -> error "invalid vertex"

-- Hoffman-Singleton Graph. It is a 7-regular graph with 50 nodes and 175 edges.
-- It's the largest graph of max-degree 7 and diameter 2.
-- cf. [https://en.wikipedia.org/wiki/Hoffman–Singleton_graph]()
--
hoffmanSingleton :: DiGraph Int
hoffmanSingleton = pentagons `union` pentagrams `union` connections
  where
    pentagons = mconcat
        [ mapVertices (p_off i) $ cycle 5 | i <- [0 .. 4] ]
    pentagrams = mconcat
        [ mapVertices (q_off i) $ pentagon2pentagram $ cycle 5 | i <- [0 .. 4] ]

    p_off h = (+) (25 + 5 * h)
    q_off i = (+) (5 * i)

    pentagon2pentagram = mapVertices $ \case
        0 -> 0
        1 -> 3
        2 -> 1
        3 -> 4
        4 -> 2
        _ -> error "invalid vertex"

    connections = fromEdges $ HS.fromList $ mconcat
        [ [(a, b), (b, a)]
        | h <- [0 .. 4]
        , j <- [0 .. 4]
        , let a = p_off h j
        , i <- [0 .. 4]
        , let b = q_off i ((h * i + j) `mod` 5)
        ]

-- -------------------------------------------------------------------------- --
-- Properties

isRegular :: DiGraph a -> Bool
isRegular = (== 1)
    . length
    . L.group
    . fmap (HS.size . snd)
    . HM.toList
    . unGraph

isSymmetric :: Hashable a => Eq a => DiGraph a -> Bool
isSymmetric g = all checkNode $ HM.toList $ unGraph g
  where
    checkNode (a, e) = all (\x -> isAdjacent x a g) e

isIrreflexive :: Eq a => Hashable a => DiGraph a -> Bool
isIrreflexive = not . any (uncurry HS.member) . HM.toList . unGraph

isVertex :: Eq a => Hashable a => a -> DiGraph a -> Bool
isVertex a = HM.member a . unGraph

isEdge :: Eq a => Hashable a => DiEdge a -> DiGraph a -> Bool
isEdge (a, b) = maybe False (HS.member b) . HM.lookup a . unGraph

isAdjacent :: Eq a => Hashable a => a -> a -> DiGraph a -> Bool
isAdjacent = curry isEdge

outDegree :: Eq a => Hashable a => DiGraph a -> a -> Natural
outDegree (DiGraph g) a = int . HS.size $ g HM.! a

maxOutDegree :: Eq a => Hashable a => DiGraph a -> Natural
maxOutDegree g = maximum $ HS.map (outDegree g) (vertices g)

minOutDegree :: Eq a => Hashable a => DiGraph a -> Natural
minOutDegree g = minimum $ HS.map (outDegree g) (vertices g)

inDegree :: Eq a => Hashable a => DiGraph a -> a -> Natural
inDegree g = outDegree (transpose g)

maxInDegree :: Eq a => Hashable a => DiGraph a -> Natural
maxInDegree = maxOutDegree . transpose

minInDegree :: Eq a => Hashable a => DiGraph a -> Natural
minInDegree = minOutDegree . transpose

-- | This is expensive for larger graphs. Use with care and
-- consider caching the result.
--
diameter ::Eq a => Hashable a => DiGraph a -> Natural
diameter g = FW.diameter $ FW.fromAdjacencySets (unGraph ig)
  where
    vmap = HM.fromList $ zip (HS.toList $ vertices g) [0..]
    ig = mapVertices (vmap HM.!) g

