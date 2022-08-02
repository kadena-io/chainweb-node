{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Difficulty
-- Copyright: Copyright © 2018 Kadena LLC
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The difficulty of a block. The difficulty is a measure for the expected work
-- that is needed to mine a block. Formally it is defined as the maximum hash
-- target value divided by the hash target for the block.
--
module Chainweb.Difficulty
(
-- * PowHashNat
  PowHashNat(..)
, powHashNat
, encodePowHashNat
, decodePowHashNat
, encodePowHashNatBe
, decodePowHashNatBe

-- * HashTarget
, HashTarget(..)
, hashTarget
, showTargetHex
, showTargetBits
, checkTarget
, maxTarget
, maxTargetWord
, encodeHashTarget
, decodeHashTarget

-- * HashDifficulty
, HashDifficulty(..)
, encodeHashDifficulty
, decodeHashDifficulty
, encodeHashDifficultyBe
, decodeHashDifficultyBe
, targetToDifficulty

-- * Difficulty Adjustment
, adjust
, legacyAdjust
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Bits
import qualified Data.ByteString.Short as SB
import Data.Coerce
import Data.DoubleWord
import Data.Hashable
import qualified Data.Text as T

import GHC.Generics
import GHC.TypeNats

import Text.Printf (printf)

-- internal imports

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.PowHash
import Chainweb.Time (Micros(..), Seconds, TimeSpan(..))
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version

import Numeric.Additive

-- -------------------------------------------------------------------------- --
-- Large Word Orphans

instance NFData Word128
instance NFData Word256

-- -------------------------------------------------------------------------- --
-- PowHashNat

-- | A type that maps block hashes to unsigned 256 bit integers by
-- projecting onto the first 8 bytes (least significant in little
-- endian encoding) and interpreting them as little endian encoded
-- unsigned integer value.
--
-- Arithmetic is defined as unsigned bounded integer arithmetic.
-- Overflows result in an exception and may result in program abort.
--
newtype PowHashNat = PowHashNat Word256
    deriving (Show, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype (Eq, Ord, Bounded, Enum)
    deriving newtype (Num, Integral, Real, Bits, FiniteBits)
        -- FIXME implement checked arithmetic
        -- FIXME avoid usage of Num and co
    deriving newtype (AdditiveSemigroup, AdditiveAbelianSemigroup)
    -- deriving newtype (MultiplicativeSemigroup, MultiplicativeAbelianSemigroup, MultiplicativeGroup)
        -- FIXME use checked arithmetic instead

powHashNat :: PowHash -> PowHashNat
powHashNat = PowHashNat . powHashToWord256
{-# INLINE powHashNat #-}

powHashToWord256 :: (32 <= PowHashBytesCount) => PowHash -> Word256
powHashToWord256 = either error id . runGetEitherS decodeWordLe . SB.fromShort . powHashBytes
{-# INLINE powHashToWord256 #-}

encodePowHashNat :: PowHashNat -> Put
encodePowHashNat (PowHashNat n) = encodeWordLe n
{-# INLINE encodePowHashNat #-}

decodePowHashNat :: Get PowHashNat
decodePowHashNat = PowHashNat <$!> decodeWordLe
{-# INLINE decodePowHashNat #-}

encodePowHashNatBe :: PowHashNat -> Put
encodePowHashNatBe (PowHashNat n) = encodeWordBe n
{-# INLINE encodePowHashNatBe #-}

decodePowHashNatBe :: Get PowHashNat
decodePowHashNatBe = PowHashNat <$!> decodeWordBe
{-# INLINE decodePowHashNatBe #-}

instance ToJSON PowHashNat where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodePowHashNat
    toEncoding = toEncoding . encodeB64UrlNoPaddingText . runPutS . encodePowHashNat
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON PowHashNat where
    parseJSON = withText "PowHashNat" $ either (fail . show) return
        . (runGetS decodePowHashNat <=< decodeB64UrlNoPaddingText)
    {-# INLINE parseJSON #-}

instance ToJSONKey PowHashNat where
    toJSONKey = toJSONKeyText
        $ encodeB64UrlNoPaddingText . runPutS . encodePowHashNat
    {-# INLINE toJSONKey #-}

instance FromJSONKey PowHashNat where
    fromJSONKey = FromJSONKeyTextParser $ either (fail . show) return
        . (runGetS decodePowHashNat <=< decodeB64UrlNoPaddingText)
    {-# INLINE fromJSONKey #-}

-- -------------------------------------------------------------------------- --
-- HashTarget

-- | HashTarget
--
-- target = maxBound / (network hash rate * block time)
--        = maxBound / difficulty
--
-- network hash rate is interpolated from observered past block times.
--
newtype HashTarget = HashTarget { _hashTarget :: PowHashNat }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable, Bounded)

hashTarget :: Lens' HashTarget PowHashNat
hashTarget = lens _hashTarget $ const HashTarget
{-# INLINE hashTarget #-}

-- | A visualization of a `HashTarget` as binary.
--
showTargetHex :: HashTarget -> T.Text
showTargetHex (HashTarget (PowHashNat n)) = T.pack . printf "%064x" $ (int n :: Integer)

-- | A visualization of a `HashTarget` as binary.
--
showTargetBits :: HashTarget -> T.Text
showTargetBits (HashTarget (PowHashNat n)) = T.pack . printf "%0256b" $ (int n :: Integer)

-- | By maximum, we mean "easiest".
--
maxTarget :: HashTarget
maxTarget = HashTarget $ PowHashNat maxTargetWord

maxTargetWord :: Word256
maxTargetWord = maxBound

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag HashTarget where
    type Tag HashTarget = 'HashTargetTag
    toMerkleNode = encodeMerkleInputNode encodeHashTarget
    fromMerkleNode = decodeMerkleInputNode decodeHashTarget
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- | The critical check in Proof-of-Work mining: did the generated hash match
-- the target?
--
checkTarget :: HashTarget -> PowHash -> Bool
checkTarget (HashTarget target) h = powHashNat h <= target
{-# INLINE checkTarget #-}

encodeHashTarget :: HashTarget -> Put
encodeHashTarget = encodePowHashNat . coerce
{-# INLINE encodeHashTarget #-}

decodeHashTarget :: Get HashTarget
decodeHashTarget = HashTarget <$!> decodePowHashNat
{-# INLINE decodeHashTarget #-}

-- -------------------------------------------------------------------------- --
-- HashDifficulty

-- | Hash Difficulty is used to measure the weight of blocks. It is defined as
--
-- difficulty = maxBound / target
--            = network hash rate * block time
--
-- where `/` is integer division.
--
-- The weight of a block is the sum of the difficulties of the block and  and
-- all it's ancestors on the chain.
--
newtype HashDifficulty = HashDifficulty PowHashNat
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Bounded, Enum)
    deriving newtype (AdditiveSemigroup, AdditiveAbelianSemigroup)
    deriving newtype (Num, Integral, Real)

encodeHashDifficulty :: HashDifficulty -> Put
encodeHashDifficulty (HashDifficulty x) = encodePowHashNat x
{-# INLINE encodeHashDifficulty #-}

decodeHashDifficulty :: Get HashDifficulty
decodeHashDifficulty = HashDifficulty <$!> decodePowHashNat
{-# INLINE decodeHashDifficulty #-}

encodeHashDifficultyBe :: HashDifficulty -> Put
encodeHashDifficultyBe (HashDifficulty x) = encodePowHashNatBe x
{-# INLINE encodeHashDifficultyBe #-}

decodeHashDifficultyBe :: Get HashDifficulty
decodeHashDifficultyBe = HashDifficulty <$!> decodePowHashNatBe
{-# INLINE decodeHashDifficultyBe #-}

-- | Given the same `ChainwebVersion`, forms an isomorphism with
-- `difficultyToTarget`.
--
targetToDifficulty :: HashTarget -> HashDifficulty
targetToDifficulty (HashTarget (PowHashNat target)) =
    HashDifficulty . PowHashNat $ maxTargetWord `div` target
{-# INLINE targetToDifficulty #-}

-- -------------------------------------------------------------------------- --
-- Difficulty Adjustment

-- | A new `HashTarget`, based on the rate of mining success over the previous N
-- blocks.
--
-- This function represents a Bitcoin-inspired, "epoch-based" adjustment
-- algorithm. For every N blocks (as defined by `WindowWidth`), we perform an
-- adjustment.
--
-- The target itself is an unsigned 256 bit integer value. Calculations are done
-- using (infinite precision) rational arithmetic and the result converted
-- to a target value by rounding down to the nearest integer value that is
-- smaller or equal than 2^256-1.
--
adjust
    :: ChainwebVersion
        -- ^ Chainweb Version
    -> WindowWidth
        -- ^ The number of blocks in an epoch
    -> TimeSpan Micros
        -- ^ the actual time of the last epoch: creation time minus the epoch
        -- start time of the last block in the (previous) epoch
    -> HashTarget
        -- ^ the hash target of the (previous) epoch, i.e. the hash target of
        -- the last header in the (previous) epoch
    -> HashTarget
        -- ^ the hash target of the new epoch
adjust v (WindowWidth ww) (TimeSpan delta) (HashTarget oldTarget) = newTarget
  where
    br :: Seconds
    br = _getBlockRate $ blockRate v

    -- TODO: the block rate should be specified in microseconds in
    -- "Chainweb.Version".
    targetedEpochTime :: Rational
    targetedEpochTime = int ww * int br * 1000000

    actualEpochTime :: Rational
    actualEpochTime = int delta

    newTarget :: HashTarget
    newTarget = min maxTarget
        $ HashTarget . PowHashNat
        $ ceiling
        $ (actualEpochTime / targetedEpochTime)
        * int oldTarget

-- | legacy target computation
--
-- This is used when 'oldDaGuard' is active.
--
legacyAdjust
    :: ChainwebVersion
        -- ^ Chainweb Version
    -> WindowWidth
        -- ^ The number of blocks in an epoch
    -> TimeSpan Micros
        -- ^ the actual time of the last epoch: creation time minus the epoch
        -- start time of the last block in the (previous) epoch
    -> HashTarget
        -- ^ the hash target of the (previous) epoch, i.e. the hash target of
        -- the last header in the (previous) epoch
    -> HashTarget
        -- ^ the hash target of the new epoch
legacyAdjust v (WindowWidth ww) (TimeSpan delta) (HashTarget oldTarget) = newTarget
  where
    br :: Seconds
    br = _getBlockRate $ blockRate v

    newDiff :: Rational
    newDiff = oldDiff * int br * int ww * 1000000 / int delta

    oldDiff :: Rational
    oldDiff = int maxTargetWord / int oldTarget

    newTarget :: HashTarget
    newTarget = HashTarget . PowHashNat $ maxTargetWord `div` ceiling newDiff

