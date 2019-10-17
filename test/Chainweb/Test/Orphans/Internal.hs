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
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Utils

arbitraryBytes :: Int -> Gen B.ByteString
arbitraryBytes i = B.pack <$> vector i

arbitraryBytesSized :: Gen B.ByteString
arbitraryBytesSized = sized $ \s -> choose (0, s) >>= arbitraryBytes

-- -------------------------------------------------------------------------- --
-- Basics

-- FIXME: This doesn't throw pattern-match warnings when a new `ChainwebVersion`
-- constructor is invented!
instance Arbitrary ChainwebVersion where
    arbitrary = elements
        [ Test singletonChainGraph
        , Test petersonChainGraph
        , TimedConsensus singletonChainGraph
        , TimedConsensus petersonChainGraph
        , PowConsensus singletonChainGraph
        , PowConsensus petersonChainGraph
        , TimedCPM singletonChainGraph
        , TimedCPM petersonChainGraph
        , FastTimedCPM singletonChainGraph
        , FastTimedCPM petersonChainGraph
        , Development
        , Testnet02
        ]

instance Arbitrary MerkleLogHash where
    arbitrary = unsafeMerkleLogHash . B.pack
        <$> vector (int merkleLogHashBytesCount)

-- -------------------------------------------------------------------------- --
-- POW

instance Arbitrary PowHashNat where
    arbitrary = powHashNat <$> arbitrary

instance Arbitrary PowHash where
    arbitrary = unsafeMkPowHash <$> arbitraryBytes (int powHashBytesCount)

instance Arbitrary HashTarget where
    arbitrary = HashTarget <$> arbitrary

instance Arbitrary HashDifficulty where
    arbitrary = HashDifficulty <$> arbitrary

-- -------------------------------------------------------------------------- --
-- Block Header

instance Arbitrary BlockHash where
    arbitrary = BlockHash <$> arbitrary

instance Arbitrary BlockHeight where
    arbitrary = BlockHeight <$> arbitrary

instance Arbitrary BlockWeight where
    arbitrary = BlockWeight <$> arbitrary

instance Arbitrary BlockHashRecord where
    arbitrary = pure $ BlockHashRecord mempty
    -- arbitrary = BlockHashRecord . HM.fromList . fmap (\x -> (_chainId x, x))
    --     <$> arbitrary

instance Arbitrary Nonce where
    arbitrary = Nonce <$> arbitrary

instance Arbitrary BlockCreationTime where
    arbitrary = BlockCreationTime <$> arbitrary

instance Arbitrary EpochStartTime where
    arbitrary = EpochStartTime <$> arbitrary

instance Arbitrary FeatureFlags where
    arbitrary = FeatureFlags <$> arbitrary

instance Arbitrary BlockHeader where
    arbitrary = fromLog . newMerkleLog <$> entries
      where
        entries
            = liftA2 (:+:) (Nonce <$> chooseAny)
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) (pure (unsafeChainId 0))
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) (BlockHeight . int @Int . getPositive <$> arbitrary)
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ liftA2 (:+:) arbitrary
            $ fmap MerkleLogBody arbitrary

-- -------------------------------------------------------------------------- --
-- Payload

instance Arbitrary BlockPayloadHash where
    arbitrary = BlockPayloadHash <$> arbitrary

instance Arbitrary Transaction where
    arbitrary = Transaction <$> arbitraryBytesSized

instance Arbitrary TransactionOutput where
    arbitrary = TransactionOutput <$> arbitraryBytesSized

instance Arbitrary BlockTransactionsHash where
    arbitrary = BlockTransactionsHash <$> arbitrary

instance Arbitrary BlockOutputsHash where
    arbitrary = BlockOutputsHash <$> arbitrary

instance Arbitrary MinerData where
    arbitrary = MinerData <$> arbitraryBytesSized

instance Arbitrary CoinbaseOutput where
    arbitrary = CoinbaseOutput <$> arbitraryBytesSized

instance Arbitrary BlockTransactions where
    arbitrary = snd <$> (newBlockTransactions <$> arbitrary <*> arbitrary)

instance Arbitrary BlockOutputs where
    arbitrary = snd <$> (newBlockOutputs <$> arbitrary <*> arbitrary)

instance Arbitrary BlockPayload where
    arbitrary = blockPayload <$> arbitrary <*> arbitrary

instance Arbitrary PayloadData where
    arbitrary = newPayloadData <$> arbitrary <*> arbitrary

instance Arbitrary PayloadWithOutputs where
    arbitrary = newPayloadWithOutputs <$> arbitrary <*> arbitrary <*> arbitrary
