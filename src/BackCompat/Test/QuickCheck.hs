{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: BackCompat.Test.QuickCheck
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module BackCompat.Test.QuickCheck
( SortedList(..)
) where

import Test.QuickCheck

#if !MIN_VERSION_QuickCheck(2,12,2)

import Data.List
import Data.Typeable

-- -------------------------------------------------------------------------- --
-- https://github.com/nick8325/quickcheck/blob/master/Test/QuickCheck/Modifiers.hs
--

newtype SortedList a = Sorted {getSorted :: [a]}
 deriving (Eq, Ord, Show, Read, Typeable)

instance Functor SortedList where
  fmap f (Sorted x) = Sorted (map f x)

instance (Arbitrary a, Ord a) => Arbitrary (SortedList a) where
  arbitrary = fmap (Sorted . sort) arbitrary

  shrink (Sorted xs) =
    [ Sorted xs'
    | xs' <- map sort (shrink xs)
    ]

#endif
