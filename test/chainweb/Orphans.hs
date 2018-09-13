{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import           Chainweb.BlockHash
import           Chainweb.BlockHeader
import           Chainweb.ChainId (ChainId)
import           Chainweb.Difficulty (HashTarget(..), HashDifficulty(..), BlockHashNat)
import           Chainweb.NodeId (NodeId(..))
import           Chainweb.Time (Time, TimeSpan(..))
import           Chainweb.Version (ChainwebVersion(..))
import qualified Data.ByteString as B
import           Data.LargeWord (LargeKey(..))

---

instance Arbitrary BlockHeader where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (Time a) where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (TimeSpan a) where
  arbitrary = genericArbitrary

instance Arbitrary BlockHash where
  arbitrary = genericArbitrary

-- | This should guarantee that each hash is different.
instance Arbitrary BlockHashBytes where
  arbitrary = BlockHashBytes . B.pack . take (fromIntegral blockHashBytesCount) <$> infiniteList

instance Arbitrary BlockHashRecord where
  arbitrary = pure $ BlockHashRecord mempty  -- TODO bad?

instance Arbitrary HashTarget where
  arbitrary = genericArbitrary

instance Arbitrary BlockPayloadHash where
  arbitrary = genericArbitrary

instance Arbitrary Nonce where
  arbitrary = Nonce <$> arbitrary

-- TODO Should this be a single, fixed value?
instance Arbitrary ChainId where
  arbitrary = genericArbitrary

instance Arbitrary BlockWeight where
  arbitrary = genericArbitrary

instance Arbitrary HashDifficulty where
  arbitrary = genericArbitrary

instance Arbitrary BlockHashNat where
  arbitrary = genericArbitrary

instance Arbitrary BlockHeight where
  arbitrary = genericArbitrary

instance Arbitrary ChainwebVersion where
  arbitrary = pure Test

instance Arbitrary NodeId where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
  arbitrary = LargeKey <$> arbitrary <*> arbitrary
