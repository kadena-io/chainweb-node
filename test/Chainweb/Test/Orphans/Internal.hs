{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Word

import qualified Fake as F

import Test.QuickCheck

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.NodeId
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

instance Arbitrary ChainwebVersion where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary ChainNodeId where
    arbitrary = ChainNodeId <$> arbitrary <*> arbitrary

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

instance Arbitrary BlockHashRecord where
    arbitrary = BlockHashRecord . HM.fromList . fmap (\x -> (_chainId x, x))
        <$> arbitrary

instance Arbitrary BlockPayloadHash where
    arbitrary = BlockPayloadHash <$> arbitrary

instance Arbitrary Nonce where
    arbitrary = Nonce <$> arbitrary

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

---

instance F.Fake ChainNodeId where
    fake = ChainNodeId
      <$> pure (testChainId 0)
      <*> F.fakeEnumFromTo 0 (fromIntegral (maxBound :: Int) :: Word64)

instance F.Fake BlockHash where
    fake = BlockHash <$> pure (testChainId 0) <*> F.fake

instance F.Fake BlockHashBytes where
    fake = BlockHashBytes . B.pack <$> F.vectorOf (int blockHashBytesCount) F.fakeEnum

instance F.Fake BlockHeight where
    fake = BlockHeight <$> F.fakeEnumFromTo 0 (fromIntegral (maxBound :: Int) :: Word64)

instance F.Fake BlockWeight where
    fake = BlockWeight <$> F.fake

instance F.Fake HashDifficulty where
    fake = HashDifficulty <$> F.fake

instance F.Fake BlockHashNat where
    fake = blockHashNat <$> F.fake

instance F.Fake HashTarget where
    fake = HashTarget <$> F.fake

instance F.Fake BlockHashRecord where
    fake = BlockHashRecord . HM.fromList . fmap (\x -> (_chainId x, x))
        <$> F.listUpTo 5 F.fake

instance F.Fake BlockPayloadHash where
    fake = BlockPayloadHash <$> F.fake

instance F.Fake Nonce where
    fake = Nonce <$> F.fakeEnumFromTo 0 (fromIntegral (maxBound :: Int) :: Word64)

instance F.Fake (Time Int64) where
    fake = Time <$> F.fake

instance F.Fake (TimeSpan Int64) where
    fake = TimeSpan <$> F.fakeEnum

instance F.Fake BlockHeader where
    fake = do
      h <- BlockHeader
          <$> F.fake
          <*> F.fake
          <*> F.fake
          <*> F.fake
          <*> F.fake
          <*> F.fake
          <*> pure (testChainId 0)
          <*> F.fake
          <*> F.fake
          <*> pure Test
          <*> F.fake
          <*> F.fake
      pure $ h { _blockHash = computeBlockHash h }
