{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- |
-- Module: Chainweb.Test.Orphans.Time
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Test.Orphans.Time
(
) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ({- Arbitrary V4.UUID -})

-- internal modules

import Chainweb.Time
import Chainweb.Utils

deriving newtype instance Arbitrary Micros

instance Arbitrary a => Arbitrary (Time a) where
    arbitrary = Time <$> arbitrary

instance Arbitrary a => Arbitrary (TimeSpan a) where
    arbitrary = TimeSpan <$> arbitrary

instance Arbitrary Seconds where
    arbitrary = int <$> (arbitrary :: Gen Integer)

