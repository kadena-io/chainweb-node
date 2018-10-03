{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Test.Orphans.External
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Orphan instances for types that aren't defined in the chainweb package
--
module Chainweb.Test.Orphans.External
(
) where

import Data.LargeWord

import Test.QuickCheck

-- Why is this slow?
--
instance Arbitrary Word256 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

