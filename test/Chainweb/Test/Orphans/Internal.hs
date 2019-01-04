{-# LANGUAGE FlexibleInstances #-}

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
import Test.QuickCheck.Gen (chooseAny)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.NodeId
import Chainweb.Utils
import Chainweb.Version

instance Arbitrary ChainwebVersion where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary ChainNodeId where
    arbitrary = ChainNodeId
      <$> pure (testChainId 0)
      <*> arbitrary

instance Arbitrary BlockHash where
    arbitrary = BlockHash <$> pure (testChainId 0) <*> arbitrary

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
    arbitrary = pure $ BlockHashRecord mempty
    -- arbitrary = BlockHashRecord . HM.fromList . fmap (\x -> (_chainId x, x))
    --     <$> arbitrary

instance Arbitrary BlockPayloadHash where
    arbitrary = BlockPayloadHash <$> arbitrary

instance Arbitrary Nonce where
    arbitrary = Nonce <$> arbitrary

instance Arbitrary BlockHeader where
    arbitrary = do
      h <- BlockHeader
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> (Nonce <$> chooseAny)
          <*> pure (testChainId 0)
          <*> arbitrary
          <*> arbitrary
          <*> pure Test
          <*> arbitrary
          <*> arbitrary
      pure $! h { _blockHash = computeBlockHash h }
