{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
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
( arbitraryBytes
, arbitraryBytesSized
, arbitraryBlockHeaderVersion
, arbitraryBlockHeaderVersionHeight
, arbitraryBlockHeaderVersionHeightChain
, arbitraryBlockHashRecordVersionHeightChain
) where

import Control.Applicative

import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.HashMap.Strict as HM

import Test.QuickCheck.Exception (discard)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Gen

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration
import P2P.Node.PeerDB

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
        , Testnet04
        , Mainnet01
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
-- P2P

instance Arbitrary P2pConfiguration where
    arbitrary = P2pConfiguration
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary

instance Arbitrary PeerEntry where
    arbitrary = PeerEntry
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary

instance Arbitrary HostAddressIdx where
    arbitrary = hostAddressIdx <$> arbitrary
    {-# INLINE arbitrary #-}

deriving newtype instance Arbitrary LastSuccess
deriving newtype instance Arbitrary SuccessiveFailures
deriving newtype instance Arbitrary AddedTime
deriving newtype instance Arbitrary ActiveSessionCount

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

instance Arbitrary Nonce where
    arbitrary = Nonce <$> arbitrary

instance Arbitrary BlockCreationTime where
    arbitrary = BlockCreationTime . Time . TimeSpan . getPositive <$> arbitrary

instance Arbitrary EpochStartTime where
    arbitrary = EpochStartTime <$> arbitrary

instance Arbitrary FeatureFlags where
    arbitrary = return mkFeatureFlags

instance Arbitrary BlockHeader where
    arbitrary = arbitrary >>= arbitraryBlockHeaderVersion
    {-# INLINE arbitrary #-}

arbitraryBlockHashRecordVersionHeightChain
    :: ChainwebVersion
    -> BlockHeight
    -> ChainId
    -> Gen BlockHashRecord
arbitraryBlockHashRecordVersionHeightChain v h cid
    | isWebChain graph cid = BlockHashRecord
        . HM.fromList
        . zip (toList $ adjacentChainIds graph cid)
        <$> arbitrary
    | otherwise = discard
  where
    graph = chainGraphAt v h

arbitraryBlockHeaderVersion :: ChainwebVersion -> Gen BlockHeader
arbitraryBlockHeaderVersion v = do
    h <- arbitrary
    arbitraryBlockHeaderVersionHeight v h
{-# INLINE arbitraryBlockHeaderVersion #-}

arbitraryBlockHeaderVersionHeight
    :: ChainwebVersion
    -> BlockHeight
    -> Gen BlockHeader
arbitraryBlockHeaderVersionHeight v h = do
    cid <- elements $ toList $ chainIdsAtHeight v h
    arbitraryBlockHeaderVersionHeightChain v h cid
{-# INLINE arbitraryBlockHeaderVersionHeight #-}

arbitraryBlockHeaderVersionHeightChain
    :: ChainwebVersion
    -> BlockHeight
    -> ChainId
    -> Gen BlockHeader
arbitraryBlockHeaderVersionHeightChain v h cid
    | isWebChain (chainGraphAt v h) cid = do
        t <- chooseEnum (toEnum 0, maxBound)
        fromLog . newMerkleLog <$> entries t
    | otherwise = discard
  where
    entries t
        = liftA2 (:+:) arbitrary -- feature flags
        $ liftA2 (:+:) (pure $ BlockCreationTime t) -- time
        $ liftA2 (:+:) arbitrary -- parent hash
        $ liftA2 (:+:) arbitrary -- target
        $ liftA2 (:+:) arbitrary -- payload hash
        $ liftA2 (:+:) (pure cid) -- chain id
        $ liftA2 (:+:) arbitrary -- weight
        $ liftA2 (:+:) (pure h) -- height
        $ liftA2 (:+:) (pure v) -- version
        $ liftA2 (:+:) (EpochStartTime <$> chooseEnum (toEnum 0, t)) -- epoch start
        $ liftA2 (:+:) (Nonce <$> chooseAny) -- nonce
        $ fmap (MerkleLogBody . blockHashRecordToVector)
            (arbitraryBlockHashRecordVersionHeightChain v h cid) -- adjacents

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
