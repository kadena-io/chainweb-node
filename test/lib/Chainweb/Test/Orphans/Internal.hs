{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Test.Orphans.Internal
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Orphan instances for types that are defined in the chainweb package
--
module Chainweb.Test.Orphans.Internal
(
) where

import qualified Data.ByteString as B

import Test.QuickCheck

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.NodeId
import Chainweb.Utils

instance Arbitrary ChainId where
    arbitrary = testChainId <$> arbitrary

instance Arbitrary NodeId where
    arbitrary = NodeId <$> arbitrary <*> arbitrary

instance Arbitrary BlockHash where
    arbitrary = BlockHash <$> arbitrary <*> arbitrary

instance Arbitrary BlockHashBytes where
    arbitrary = BlockHashBytes . B.pack <$> vector (int blockHashBytesCount)

instance Arbitrary BlockHeight where
    arbitrary = BlockHeight <$> arbitrary

instance Arbitrary BlockWeight where
    arbitrary = BlockWeight <$> arbitrary

instance Arbitrary HashDifficulty where
    arbitrary = HashDifficulty <$> arbitrary

instance Arbitrary BlockHashNat where
    arbitrary = blockHashNat <$> arbitrary

instance Arbitrary HashTarget where
    arbitrary = HashTarget <$> arbitrary

