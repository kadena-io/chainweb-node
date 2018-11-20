{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Data.DiGraph.FloydWarshall
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.DiGraph.FloydWarshall
(
-- * Graph Representations
  DenseAdjMatrix
, AdjacencySets

-- * Conversions
, fromAdjacencySets
, toAdjacencySets

-- * FloydWarshall Algorithm
, shortestPaths
, diameter
) where

import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Massiv.Array as M

import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- Graph Representations

type DenseAdjMatrix = Array U Ix2 Int

type AdjacencySets = HM.HashMap Int (HS.HashSet Int)

-- -------------------------------------------------------------------------- --
-- Adjacency Matrix for dense Graphs
--
-- (uses massiv)

-- | Assumes that the input is an undirected graph and that the vertex
-- set is a prefix of the natural numbers.
--
fromAdjacencySets :: AdjacencySets -> DenseAdjMatrix
fromAdjacencySets g = makeArray Seq (n :. n) go
  where
    n = HM.size g
    go (i :. j)
        | isEdge (i, j) = 1
        | isEdge (j, i) = 1
        | otherwise = 0
    isEdge (a, b) = maybe False (HS.member b) $ HM.lookup a g

toAdjacencySets :: DenseAdjMatrix -> AdjacencySets
toAdjacencySets = ifoldlS f mempty
  where
    f a (i :. j) x
        | x == 0 = a
        | otherwise = HM.insertWith (<>) i (HS.singleton j) a

-- -------------------------------------------------------------------------- --
-- Floyd Warshall

distMatrix
    :: Source r Ix2 Int
    => Array r Ix2 Int
    -> Array M.D Ix2 Double
distMatrix = M.imap go
  where
    go (x :. y) e
        | x == y = 0
        | e > 0 = realToFrac e
        | otherwise = 1/0

-- | Floyd-Warshall
--
-- TODO: use a mutable array?
-- TODO: implement Dijkstra's algorithm for adj matrix representation.
--
floydWarshall
    :: Array U Ix2 Double
    -> Array U Ix2 Double
floydWarshall a = foldl' go a [0..n-1]
  where
    (n :. _) = size a

    go :: Array U Ix2 Double -> Int -> Array U Ix2 Double
    go c k = makeArray Seq (n :. n) $ \(x :. y) ->
        let
            !xy = c M.! (x :. y)
            !xk = c M.! (x :. k)
            !ky = c M.! (k :. y)
        in if xy > xk + ky then xk + ky else xy

-- | All entries of the result matrix are either whole numbers or 'Infinity'.
--
shortestPaths :: Array U Ix2 Int -> Array U Ix2 Double
shortestPaths = floydWarshall . computeAs U . distMatrix

diameter :: Array U Ix2 Int -> Maybe Natural
diameter g
    | M.isEmpty g = Just 0
    | otherwise = let x = round $ M.maximum $ shortestPaths g
        in if x == round (1/0) then Nothing else Just x

