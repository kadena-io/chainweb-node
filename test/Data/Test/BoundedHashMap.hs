{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.Test.BoundedHashMap
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.Test.BoundedHashMap
( tests
) where

import Data.Foldable
import Data.Hashable
import qualified Data.List as L

import Numeric.Natural

import Prelude hiding (lookup)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck.Instances ({- Arbitrary Natural -})

-- internal modules

import Data.BoundedHashMap

-- -------------------------------------------------------------------------- --
-- Properties

tests :: TestTree
tests = testGroup "Data.BoundedHashMap"
    [ testProperty "prop_newLookup" $ prop_newLookup @Int
    , testProperty "prop_zeroLimit" $ prop_zeroLimit @Int @Int
    , testProperty "prop_zeroLimitLookup" $ prop_zeroLimitLookup @Int @Int
    , testProperty "prop_insertLookup" $ prop_insertLookup @Int @Int
    , testProperty "prop_size" $ prop_size @Int @Int
    ]

prop_newLookup :: Eq k => Hashable k => k -> Natural -> Property
prop_newLookup k n = lookup k (new @_ @Int n) === Nothing

prop_zeroLimit :: Eq k => Hashable k => k -> v -> Property
prop_zeroLimit k v = size (insert k v (new 0)) === 0

prop_zeroLimitLookup :: Eq k => Hashable k => Eq v => Show v => k -> v -> Property
prop_zeroLimitLookup k v = lookup k (insert k v (new 0)) === Nothing

prop_insertLookup
    :: Eq k
    => Hashable k
    => Eq v
    => Show v
    => k
    -> v
    -> (NonZero Natural)
    -> Property
prop_insertLookup k v (NonZero n) = lookup k (insert k v (new n)) === Just v

prop_size
    :: Eq k
    => Hashable k
    => Eq v
    => Show v
    => Natural
    -> [(k, v)]
    -> Property
prop_size n entries = (size m' >= min (fromIntegral l) n)
    .&&. (size m' <= min (fromIntegral l) (2 * n))
  where
    l = length (L.nub $ fst <$> entries)
    m' = foldl' (\m (k,v) -> insert k v m) (new n) entries

