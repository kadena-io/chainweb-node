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

import Control.Applicative

import qualified Data.ByteString as B

import Test.QuickCheck
import Test.QuickCheck.Gen (chooseAny)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.MerkleLogHash
import Chainweb.NodeId
import Chainweb.PowHash
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

instance Arbitrary PowHash where
    arbitrary = unsafeMkPowHash . B.pack <$> vector (int powHashBytesCount)

instance Arbitrary MerkleLogHash where
    arbitrary = unsafeMerkleLogHash . B.pack
        <$> vector (int merkleLogHashBytesCount)

instance Arbitrary BlockHeight where
    arbitrary = BlockHeight <$> arbitrary

instance Arbitrary BlockWeight where
    arbitrary = BlockWeight <$> arbitrary

instance Arbitrary HashDifficulty where
    arbitrary = HashDifficulty <$> arbitrary

instance Arbitrary PowHashNat where
    arbitrary = powHashNat <$> arbitrary

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

instance Arbitrary BlockCreationTime where
    arbitrary = BlockCreationTime <$> arbitrary

instance Arbitrary BlockHeader where
    arbitrary = fromLog . newMerkleLog <$> entries
      where
        entries
            = liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) (Nonce <$> chooseAny)
            $ liftA2 (:+:) (pure (testChainId 0))
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) (pure Test)
            $ liftA2 (:+:) arbitrary
            $ fmap MerkleLogBody arbitrary

