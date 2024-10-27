{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: P2P.Test.Node
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.Test.Node
( prop_geometric
, properties
) where

import qualified Data.List as L

import qualified System.Random as R

import Test.QuickCheck

-- internal modules

import P2P.Node

-- -------------------------------------------------------------------------- --
-- Utils

newtype ArbitraryGen = ArbitraryGen { _getGen :: R.StdGen }
    deriving (R.RandomGen, Show)

instance Arbitrary ArbitraryGen where
    arbitrary = ArbitraryGen . R.mkStdGen <$> arbitrary

-- Internal function from System.Random
--
buildRandoms
    :: R.RandomGen g
    => (a -> as -> as)
    -> (g -> (a,g))
    -> g
    -> as
buildRandoms cons rand = go
  where
    go g = x `seq` (x `cons` go g') where (x,g') = rand g

-- -------------------------------------------------------------------------- --
-- Properties of Geometric Distribution

genGeometricParam :: Gen Double
genGeometricParam = choose (0, 1)

prop_geometric :: Property
prop_geometric = forAll genGeometricParam prop_geometricParam

prop_geometricParam :: Double -> ArbitraryGen -> Property
prop_geometricParam param (ArbitraryGen gen) = counterexample (show hist__) $
    all (uncurry approx) hist_
  where

    -- Histgram of sampled (actual) values
    hist = L.length <$> L.group (L.sort mkdist)

    -- Histogram with expected and actual value
    hist_ = zip (expected n) hist

    -- When displaying counterexamples, the expected bounds are included
    hist__ = fmap (\(a,b) -> (a * (1 - (100 / a)), a * (1 + (100 / a)), a, b)) hist_

    -- Sample n geometrically distributed values
    mkdist = take n $ buildRandoms (:) (geometric param) gen

    -- check that the actual value approximately matches the expected
    -- value
    approx :: Integral a => Double -> a -> Bool
    approx e (fromIntegral @_ @Double -> a) = a < e * u && a > e * l
      where
        u = 1 + (100 / e)
        l = 1 - (100 / e)

    n :: Num a => a
    n = 1000

    -- The expected histogram
    expected :: Double -> [Double]
    expected n' = let x = n' * param in x : expected (n' - x)

-- -------------------------------------------------------------------------- --
--

properties :: [(String, Property)]
properties =
    [ ("P2P.Test.Node.prop_geometric", prop_geometric)
    ]
