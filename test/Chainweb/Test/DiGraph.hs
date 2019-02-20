{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.DiGraph
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Throughout the module an undirected graph is a directed graph
-- that is symmetric and irreflexive.
--
module Chainweb.Test.DiGraph
(
-- * Random Regular Graph
  rrgIO
, rrg
, RegularGraph(..)

-- * Random Graphs in the \(G_{n,p}\) model
, gnp
, Gnp(..)
, RandomGraph(..)

-- * Properties
, tests
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Data.Foldable
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Internal.RootPath as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import Data.Hashable
import Data.Massiv.Array (Array(..), U, Ix2(..), makeArray, Comp(..))
import qualified Data.Massiv.Array as M
import Data.Proxy
import qualified Data.Set as S

import GHC.Generics
import GHC.TypeNats

import Numeric.Natural

import qualified Streaming.Prelude as S

import qualified System.Random.MWC as MWC

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Text.Printf

-- internal modules

import Chainweb.Utils

import Data.DiGraph hiding (properties)
import qualified Data.DiGraph.FloydWarshall as FW

-- -------------------------------------------------------------------------- --
-- RNG

type UniformRng m = (Int, Int) -> m Int

-- -------------------------------------------------------------------------- --
-- Random Regular Graph

rrgIO
    :: Natural
    -> Natural
    -> IO (Maybe (DiGraph Int))
rrgIO n d = MWC.withSystemRandom $ \gen -> rrg @IO (`MWC.uniformR` gen) n d

-- | Undirected, irreflexive random regular graph.
--
-- The algorithm here is incomplete. For a complete approach see for instance
-- https://users.cecs.anu.edu.au/~bdm/papers/RandRegGen.pdf
--
rrg
    :: Monad m
    => UniformRng m
        -- ^ a uniform random number generator
    -> Natural
    -> Natural
    -> m (Maybe (DiGraph Int))
rrg gen n d = go 0 (S.fromList c) (emptyGraph n)
  where
    v = [0 .. int n - 1]
    c = [(x, y) | x <- v, y <- [0 :: Int .. int d - 1]]

    go i s g
        | S.null s = return $ Just g
        | (fst . fst <$> S.minView s) == (fst . fst <$> S.maxView s) = return Nothing
        | otherwise = sampleEdge s g >>= \case
            Nothing -> if i < n then go (i + 1) s g else return Nothing
            Just (s', g') -> go 0 s' g'

    sampleEdge s graph = runMaybeT $ do
        (s', v₁) <- lift $ uniformSample gen s
        (s'', v₂) <- lift $ uniformSample gen s'
        let e₁ = (fst v₁, fst v₂)
        let e₂ = (fst v₂, fst v₁)
        guard $ fst v₁ /= fst v₂ && not (isEdge e₁ graph)
        return (s'', insertEdge e₁ $ insertEdge e₂ graph)

-- | Uniformily sample an element from the input set. Returns the set with the
-- sampled element removed and the sampled element.
--
uniformSample :: Monad m => UniformRng m -> S.Set a -> m (S.Set a, a)
uniformSample gen s = do
    p <- gen (0, S.size s - 1)
    return (S.deleteAt p s, S.elemAt p s)

-- -------------------------------------------------------------------------- --
-- Gnp

-- | Undirected irreflexive random graph in the \(G_{n,p}\) model.
--
gnp
    :: forall m
    . Monad m
    => UniformRng m
    -> Natural
    -> Double
    -> m (DiGraph Int)
gnp gen n p = S.fold_ (flip insertEdge) (emptyGraph n) id
    $ S.concat
    $ S.filterM (const choice)
    $ S.each
        [ [(a,b), (b,a)]
        | a <- [0..int n - 1]
        , b <- [0..a-1]
        ]
  where
    choice = do
        v <- gen (0, maxBound)
        return $ int v <= p * int (maxBound :: Int)

-- -------------------------------------------------------------------------- --
-- Arbitrary Graph Instances

-- | Uniformily distributed regular graph in edge list representation. The type
-- parameter is the degree of the graph. The size parameter of the QuickCheck
-- Arbirary instance is the order of the graph.
--
type role RegularGraph phantom
newtype RegularGraph (d :: Nat) = RegularGraph { getRegularGraph :: DiGraph Int }
    deriving (Show, Eq, Ord, Generic)

instance KnownNat d => Arbitrary (RegularGraph d) where
    arbitrary = fmap RegularGraph
        $ scale (if d > (0 :: Int) then (+ (d + 1)) else id)
        $ sized $ \n -> maybe discard return =<< rrg choose (int n) d
      where
        d :: Num a => a
        d = int $ natVal (Proxy @d)


-- | Random graph in the \(G_{n,p}\) model in edge list representation. The type
-- parameter is the size of the graph. The size parameter of the QuickCheck
-- Arbitrary instance is the expected size of the graph (number of edges).
--
-- Notes:
--
-- For \(n * (n-1) * p / 2) = m\) the distributions \(G_{n,p}\) and \(G_{n,m}\)
-- have very similar properties.
--
type role Gnp phantom
newtype Gnp (n :: Nat) = Gnp { getGnp :: DiGraph Int }
    deriving (Show, Eq, Ord, Generic)

instance KnownNat n => Arbitrary (Gnp n) where
    arbitrary = sized $ \m -> Gnp <$> gnp choose n (p m)
      where
        -- m = p * n * (n-1) / 2
        p :: Int -> Double
        p m = min 1 ((2 * int m) / (n * (n - 1)))

        n :: Num a => a
        n = int $ natVal $ Proxy @n

-- | The uniform distribution over all undirected graphs in edge list
-- representation. This is the \(G_{n,1/2})\) random graph.
--
-- Note for graphs in the \(G_{n,1/2}\) model many important properties are
-- highly concentrated around their expectation. For instances, almost all
-- graphs of a large enough order \(n\) are connected. Therefore, in many cases
-- this is not a good testing model for test spaces where only a low coverage is
-- possible.
--
newtype RandomGraph = RandomGraph { getRandomGraph :: DiGraph Int }
    deriving (Show, Eq, Ord, Generic)

instance Arbitrary RandomGraph where
    arbitrary = sized $ \n -> RandomGraph <$> gnp choose (int n) 0.5

{-
-- | Random graph in the \(G_{n,m}\) model. The type parameter is the order
-- of the graph. The \(m\) parameter free at runtime and can be chosen
-- set to the @size@ parameter in 'Arbitrary' instances.
--
-- The random graph \(G_{n,m}\) is the uniform distribution over graphs of
-- order \(n\) and size \(m\).
--
type role Gnm phantom
newtype Gnm (n :: Nat) = Gnm { getGnm :: DiGraph Int }

instance (KnownNat n) => Arbitrary (Gnm n) where
    arbitrary = sized $ \m -> d
-}

-- -------------------------------------------------------------------------- --
-- Dijkstra's Shortest Path Algorithm

-- | On sparse Graphs Dijkstra's algorithm is, generally, much faster than
-- Floyd-Warshall. The FGL implementation, however, is very
-- inefficient. So, this is almost certainly slower than the
-- implementation of Floyd-Warshall above.
--
-- All entries of the result matrix are either whole numbers or 'Infinity'.
--
fglShortestPaths :: G.Graph g => g Int Int -> Array U Ix2 Double
fglShortestPaths g = makeArray Seq (n :. n) $ \(i :. j) ->
    maybe (1/0) realToFrac $ G.getDistance j (sp i)
  where
    sp i = G.spTree i g
    n = G.order g

fglDiameter :: G.Graph g => g Int Int -> Maybe Natural
fglDiameter g = if M.isEmpty sps
    then Just 0
    else let x = round $ M.maximum sps
        in if x == round (1/0 :: Double) then Nothing else Just x
  where
    sps = fglShortestPaths g

toFglGraph :: G.Graph g => DiGraph Int -> g Int Int
toFglGraph g = G.mkGraph vs es
  where
    vs = [(i,i) | i <- toList (vertices g)]
    es = concatMap (\(x,y) -> [(x, y, 1)]) $ edges g

-- -------------------------------------------------------------------------- --
-- Tools for collecting coverage data

collect_graphStats
    :: Testable prop
    => Natural
        -- ^ maximum value for the graph order of in the graph distribution
    -> DiGraph Int
    -> prop
    -> Property
collect_graphStats n g
    = collect (orderClass (n + 1) g)
    . collect (sizeClass (n + 1) g)
    . collect (densityClass g)
    . collect (diameterClass g)

-- | For undirected graphs. In steps of tenth.
--
densityClass :: Eq a => Hashable a => DiGraph a -> String
densityClass g
    | order g == 0 = "density: null graph"
    | otherwise = printf "density: %.1f" d
  where
    o :: Num a => a
    o = int (order g)

    d :: Double
    d = 2 * int (size g) / (o * (o - 1))

diameterClass :: Eq a => Hashable a => DiGraph a -> String
diameterClass g = "diameter: " <> show (diameter g)

orderClass :: Natural -> DiGraph a -> String
orderClass n g = classifyByRange "order" n (order g)

-- | undirected size
--
sizeClass :: Eq a => Hashable a => Natural -> DiGraph a -> String
sizeClass n g = classifyByRange "size" (n * (n - 1) `div` 2) (size g)

classifyByRange :: String -> Natural -> Natural -> String
classifyByRange s n x = printf "%s: (%d, %d)" s l u
  where
    l = (x * 10 `div` n) * (n `div` 10)
    u = ((x * 10 `div` n) + 1) * (n `div` 10)

-- -------------------------------------------------------------------------- --
-- Property Utils

graphProperty
    :: Testable prop
    => Natural
        -- ^ maximum value for the graph order of in the graph distribution
    -> (DiGraph Int -> prop)
    -> Property
graphProperty maxN p = mapSize (`mod` int maxN) $ \(RandomGraph g) ->
    collect_graphStats maxN g $ p g

rrgProperty
    :: Testable prop
    => Natural
        -- ^ maximum value for the graph order of in the graph distribution
    -> (DiGraph Int -> prop)
    -> Property
rrgProperty maxN p = mapSize (`mod` int maxN) $ \(RegularGraph g :: RegularGraph 3) ->
    collect_graphStats maxN g $ p g

-- | The first type parameter is the static order of the graph. The size of the
-- QuichCheck Arbitrary instances is the expected size of the graph (number of
-- edges).
--
gnpProperty
    :: forall (n :: Nat) prop
    . KnownNat n
    => Testable prop
    => (DiGraph Int -> prop)
    -> Property
gnpProperty p = property $ \(Gnp g :: Gnp n) ->
    collect_graphStats (natVal $ Proxy @n) g $ p g

-- -------------------------------------------------------------------------- --
-- Properties of graph algorithms

prop_shortestPaths :: DiGraph Int -> Property
prop_shortestPaths g = fglShortestPaths fglG === M.computeAs M.U (M.map fst m)
  where
    fglG = toFglGraph @G.Gr g
    denseG = FW.fromAdjacencySets $ adjacencySets g
    FW.ShortestPathMatrix m = FW.floydWarshall denseG

prop_diameter :: DiGraph Int -> Property
prop_diameter g = fglDiameter (toFglGraph @G.Gr g) === diameter g

tests_randomGraph :: TestTree
tests_randomGraph = testGroup "uniform random graph"
    [ testProperty "isDiGraph" (graphProperty 20 isDiGraph)
    , testProperty "isSymmetric" (graphProperty 20 isSymmetric)
    , testProperty "shortestPaths" (graphProperty 20 prop_shortestPaths)
    , testProperty "diameter" (graphProperty 20 prop_diameter)
    ]

tests_gnp :: TestTree
tests_gnp = testGroup "Gnp random graph"
    [ testProperty "isDiGraph" (gnpProperty @20 isDiGraph)
    , testProperty "isSymmetric" (gnpProperty @20 isSymmetric)
    , testProperty "shortestPaths" (gnpProperty @20 prop_shortestPaths)
    , testProperty "diameter" (gnpProperty @20 prop_diameter)
    ]

tests_rrg :: TestTree
tests_rrg = testGroup "random regular graph"
    [ testProperty "isDiGraph" (rrgProperty 20 isDiGraph)
    , testProperty "isRegular" (rrgProperty 20 isRegular)
    , testProperty "isSymmetric" (rrgProperty 20 isSymmetric)
    , testProperty "shortestPaths" (rrgProperty 20 prop_shortestPaths)
    , testProperty "diameter" (rrgProperty 20 prop_diameter)
    ]

tests :: TestTree
tests = testGroup "graph property tests"
    [ tests_randomGraph
    , tests_gnp
    , tests_rrg
    ]

