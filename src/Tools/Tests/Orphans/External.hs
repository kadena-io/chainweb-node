{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Tools.Tests.Orphans.External
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Orphan instances for types that aren't defined in the chainweb package
--
module Tools.Tests.Orphans.External
(
) where

import Data.DoubleWord (Word256)
import Test.QuickCheck

instance Arbitrary Word256 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral
