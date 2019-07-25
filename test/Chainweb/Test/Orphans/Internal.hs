{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
(
) where

import Control.Applicative

import qualified Data.ByteString as B
import Data.MerkleLog
import Data.Reflection (give)
import Data.String
import Data.Word

import System.IO.Unsafe
import System.Logger (defaultLoggerConfig)
import System.Logger.Backend.ColorOption

import Test.QuickCheck
import Test.QuickCheck.Gen (chooseAny)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Cut.CutHashes
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Mempool.RestAPI
import Chainweb.MerkleLogHash
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.SPV
import Chainweb.Utils
import Chainweb.Version

import Crypto.Hash.Algorithms

import Network.X509.SelfSigned

import P2P.Peer

import Utils.Logging.Config

import Chainweb.Cut.Test ({- Arbitrary Cut -})

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
        , Testnet00
        , Testnet01
        ]

instance Arbitrary ChainNodeId where
    arbitrary = ChainNodeId
      <$> pure (unsafeChainId 0)
      <*> arbitrary

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
            $ fmap MerkleLogBody arbitrary

instance Arbitrary a => Arbitrary (ObjectEncoded a) where
    arbitrary = ObjectEncoded <$> arbitrary

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
    arbitrary = snd <$> applyArbitrary2 newBlockTransactions

instance Arbitrary TransactionTree where
    arbitrary = fst <$> applyArbitrary2 newBlockTransactions

instance Arbitrary BlockOutputs where
    arbitrary = snd <$> applyArbitrary2 newBlockOutputs

instance Arbitrary BlockPayload where
    arbitrary = applyArbitrary2 blockPayload

instance Arbitrary PayloadData where
    arbitrary = applyArbitrary2 newPayloadData

instance Arbitrary PayloadWithOutputs where
    arbitrary = applyArbitrary3 newPayloadWithOutputs

instance Arbitrary OutputTree where
    arbitrary = fst <$> applyArbitrary2 newBlockOutputs

-- -------------------------------------------------------------------------- --
-- Cuts

instance Arbitrary CutHashes where
    arbitrary = do
        (v :: ChainwebVersion) <- arbitrary
        c <- give v $ arbitrary
        return $ cutToCutHashes Nothing c

instance Arbitrary CutId where
    arbitrary = (_cutId @CutHashes) <$> arbitrary

-- -------------------------------------------------------------------------- --
-- Merkle Trees and SPV

instance Arbitrary (TransactionProof SHA512t_256) where
    arbitrary = TransactionProof <$> arbitrary <*> arbitrary

instance Arbitrary (TransactionOutputProof SHA512t_256) where
    arbitrary = TransactionOutputProof <$> arbitrary <*> arbitrary

instance Arbitrary (MerkleProof SHA512t_256) where
    arbitrary = do
        NonEmpty input <- fmap (InputNode . B.pack)
            <$> arbitrary @(NonEmptyList [Word8])
        pos <- choose (0, length input - 1)
        return
            $ either (error . show) id
            $ merkleProof (input !! pos) pos (merkleTree input)

instance Arbitrary (MerkleTree SHA512t_256) where
    arbitrary = merkleTree
        <$> (fmap (InputNode . B.pack)  <$> arbitrary @[[Word8]])

-- -------------------------------------------------------------------------- --
-- Logging

arbitraryColorOption :: Gen ColorOption
arbitraryColorOption = arbitraryBoundedEnum

instance Arbitrary ClusterId where
    arbitrary = fromString <$> arbitrary

instance Arbitrary HandleConfig where
    arbitrary = oneof
        [ pure StdOut
        , pure StdErr
        , FileHandle <$> arbitrary
        , ElasticSearch <$> arbitrary
        ]

instance Arbitrary LogFormat where
    arbitrary = elements [ LogFormatText, LogFormatJson ]

instance Arbitrary BackendConfig where
    arbitrary = BackendConfig
        <$> arbitraryColorOption
        <*> arbitrary
        <*> arbitrary

instance Arbitrary LogConfig where
    arbitrary = applyArbitrary3 $ LogConfig defaultLoggerConfig

-- -------------------------------------------------------------------------- --
-- Pact

instance Arbitrary MinerInfo where
    arbitrary = pure noMiner

-- -------------------------------------------------------------------------- --
-- Misc

-- | This instance uses unsafePerformIO to generate X509 certificates.
-- Generation is thus non-deterministic.
--
-- The instances generates Ed25519 certificates in order to avoid the costly
-- generation of RSA keys. The distribution still includes arbitrary values with
-- an hardcoded RSA certificate.
--
instance Arbitrary Peer where
    arbitrary = unsafePerformIO . unsafeCreatePeer_ @Ed25519Cert <$> arbitrary

instance Arbitrary MinerConfig where
    arbitrary = MinerConfig <$> (MinerCount <$> arbitrary) <*> pure noMiner

instance Arbitrary TransactionHash where
    arbitrary = TransactionHash <$> arbitraryBytes 32

instance Arbitrary PendingTransactions where
    arbitrary = applyArbitrary2 PendingTransactions

instance Arbitrary a => Arbitrary (EnableConfig a) where
    arbitrary = applyArbitrary2 EnableConfig

instance Arbitrary MempoolP2pConfig where
    arbitrary = applyArbitrary3 MempoolP2pConfig

