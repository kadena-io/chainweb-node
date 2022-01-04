{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
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

import Data.DoubleWord (Word256)
import Data.Word

import Network.Socket

import Test.QuickCheck

instance Arbitrary Word256 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary SockAddr where
    arbitrary = oneof
        [ SockAddrInet <$> (fromIntegral @Word16 <$> arbitrary) <*> arbitrary
        , SockAddrInet6 <$> (fromIntegral @Word16 <$> arbitrary) <*> arbitrary <*> arbitrary <*> arbitrary
        , SockAddrUnix <$> arbitrary
        ]
