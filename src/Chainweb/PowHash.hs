{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.PowHash
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.PowHash
(
-- * POW Hash
  PowHash
, powHashBytes
, mkPowHash
, unsafeMkPowHash
, PowHashBytesCount
, powHashBytesCount
, encodePowHash
, decodePowHash

-- * POW Hash Algorithms
, PowHashAlg(..)
, encodePowHashAlg
, decodePowHashAlg
, powHashAlg
, defaultPowHashAlg

-- * POW Hash Implementations
, powHash
, randomPowHash
) where

import Control.DeepSeq
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Crypto.Hash as C (hash)
import Crypto.Hash.Algorithms

import Data.Aeson
import Data.Bifunctor
import Data.Bits
import qualified Data.ByteArray as BA
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Random as BR
import qualified Data.ByteString.Short as SB
import Data.Hashable hiding (hash)
import qualified Data.List.NonEmpty as NE
import Data.Proxy

import Foreign.Storable

import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeNats

import Numeric.Natural

import System.IO.Unsafe

-- internal modules

import Chainweb.BlockHeight
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- PowHash

type PowHashBytesCount = 32

powHashBytesCount :: Natural
powHashBytesCount = natVal $ Proxy @PowHashBytesCount
{-# INLINE powHashBytesCount #-}

newtype PowHash = PowHash SB.ShortByteString
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Smart constructor
--
mkPowHash :: MonadThrow m => B.ByteString -> m PowHash
mkPowHash = runGet decodePowHash
{-# INLINE mkPowHash #-}

unsafeMkPowHash :: HasCallStack => B.ByteString -> PowHash
unsafeMkPowHash = fromJuste . runGet decodePowHash
{-# INLINE unsafeMkPowHash #-}

instance IsMerkleLogEntry ChainwebHashTag PowHash where
    type Tag PowHash = 'PowHashTag
    toMerkleNode = encodeMerkleInputNode encodePowHash
    fromMerkleNode = decodeMerkleInputNode decodePowHash
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodePowHash :: MonadPut m => PowHash -> m ()
encodePowHash (PowHash w) = putByteString $ SB.fromShort w
{-# INLINE encodePowHash #-}

powHashBytes :: PowHash -> SB.ShortByteString
powHashBytes (PowHash bytes) = bytes
{-# INLINE powHashBytes #-}

decodePowHash :: MonadGet m => m PowHash
decodePowHash = PowHash . SB.toShort <$> getBytes (int powHashBytesCount)
{-# INLINE decodePowHash #-}

instance Hashable PowHash where
    hashWithSalt s (PowHash bytes) = xor s
        . unsafePerformIO
        $ BA.withByteArray (SB.fromShort bytes) (peek @Int)
    -- PowHashs are already cryptographically strong hashes
    -- that include the chain id.
    {-# INLINE hashWithSalt #-}

instance ToJSON PowHash where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodePowHash
    {-# INLINE toJSON #-}

instance FromJSON PowHash where
    parseJSON = withText "PowHash" $ \t ->
        either (fail . show) return
            $ runGet decodePowHash =<< decodeB64UrlNoPaddingText t
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- POW Hash Algorithms

-- | Enumeration of POW Hash Algorithms that are used in Chainweb.
--
-- For backward compatibility it must hold that
--
-- @
-- fromEnum PowHash_Blake2s == 0
-- @
--
data PowHashAlg
    = PowHash_Blake2s
    | PowHash_Sha256
    deriving (Show, Eq, Ord, Bounded, Enum, Generic)

-- FIXME provide explicit Enum instance

encodePowHashAlg :: MonadPut m => PowHashAlg -> m ()
encodePowHashAlg = putWord8 . int . fromEnum
{-# INLINE encodePowHashAlg #-}

-- FIXME use Enum instance
decodePowHashAlg :: MonadGet m => m PowHashAlg
decodePowHashAlg = getWord8 >>= \case
    0 -> return PowHash_Blake2s
    1 -> return PowHash_Sha256
    x -> fail $ "unknow POW hash algorithm code: " <> sshow x
{-# INLINE decodePowHashAlg #-}

instance Hashable PowHashAlg where
    hashWithSalt s = xor s . fromEnum
    {-# INLINE hashWithSalt #-}

instance ToJSON PowHashAlg where
    toJSON = toJSON . fromEnum
    {-# INLINE toJSON #-}

-- FIXME use Enum instance
instance FromJSON PowHashAlg where
    parseJSON = withScientific "PowHashAlg" $ \case
        0 -> return PowHash_Blake2s
        1 -> return PowHash_Sha256
        x -> fail $ "unknow POW hash algorithm code: " <> sshow x
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- POW Hash Selection

-- | Hash algorithm that are supported by a given Chainweb Version.
--
-- A chainweb version can support more than a single algorithm. Each algorithm
-- has an difficulty adjustment factor, that is used as a multiplier on the
-- result before comparing with the target. A value of 0 means that the outcome
-- is always smaller than the target, and thus any nonce succeeds. A value of 1
-- leaves the target unchanged. Larger values make it more difficult to find
-- nonce.
--
-- Only natural numbers are supported for adjusting the difficulty.
-- Multiplication uses 'Chainweb.PowHashNat' as defined in
-- "Chainweb.Difficulty".
--
powHashAlg :: ChainwebVersion -> BlockHeight -> NE.NonEmpty (PowHashAlg, Natural)
powHashAlg Test{} _ = (PowHash_Blake2s, 1) NE.:| []
powHashAlg TimedConsensus{} _ = (PowHash_Blake2s, 1) NE.:| []
powHashAlg PowConsensus{} _ = (PowHash_Blake2s, 1) NE.:| []
powHashAlg TimedCPM{} _ = (PowHash_Blake2s, 1) NE.:| []
powHashAlg FastTimedCPM{} _ = (PowHash_Blake2s, 1) NE.:| []

powHashAlg Development h = (PowHash_Blake2s, a) NE.:| [(PowHash_Sha256, b)]
  where
    -- The transition starts at block height 250 has 10 steps and take about 20h
    (a, b) = transition 250 10 240 (int h)

powHashAlg Testnet04 _ = (PowHash_Blake2s, 1) NE.:| []
powHashAlg Mainnet01 _ = (PowHash_Blake2s, 1) NE.:| []

-- | This function is meant mostly for testing. For mainnet transitions it is
-- recommend to encode the transition explicitely as a table.
--
-- @stepDuration@ should be at least @2 * 120@, i.e. two DA epochs or about 2
-- hours. In mainnet, @stepDurantion@ should probably be in the order of days
-- anyways.
--
transition :: Natural -> Natural -> Natural -> Natural -> (Natural, Natural)
transition start stepCount stepDuration = bimap (+1) (+1) . go
  where
    go h
        | h < start = (0, stepCount)
        | h < start + (stepCount * stepDuration) = (i, stepCount - i)
        | otherwise = (stepCount, 0)
      where
        i = (h - start) `div` stepDuration

defaultPowHashAlg :: HasChainwebVersion v => v -> BlockHeight -> PowHashAlg
defaultPowHashAlg v h = fst $ NE.head $ powHashAlg (_chainwebVersion v) h

-- -------------------------------------------------------------------------- --
-- POW hash implementations

-- | POW hash algorithm implemenations
--
powHash :: PowHashAlg -> B.ByteString -> PowHash
powHash PowHash_Blake2s = cryptoHash @Blake2s_256
powHash PowHash_Sha256 = cryptoHash @SHA256

-- | This must be used only for testing. The result hash is uniformily
-- distributed, but not cryptographically safe.
--
randomPowHash :: MonadIO m => m PowHash
randomPowHash = PowHash . SB.toShort <$> liftIO (BR.random powHashBytesCount)

cryptoHash :: forall a . HashAlgorithm a => B.ByteString -> PowHash
cryptoHash = unsafeMkPowHash . BA.convert . C.hash @_ @a
    --
    -- The usage of unsafeMkPowHash is justified here becaue we want to fail
    -- (and actually we *will* fail) early and hard if we'd chose and invalid
    -- hash algorithm.
    --
    -- The overhead of using a smart constructor is justified because in-node
    -- mining isn't competitive in mainnet. This implementation of the hash
    -- function is only used for validation and during testing.

