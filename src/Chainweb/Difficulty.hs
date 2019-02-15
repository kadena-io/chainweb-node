{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module: Chainweb.Difficulty
-- Copyright: Copyright © 2018 Kadena LLC
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Difficulty
(
-- * PowHashNat
  PowHashNat(..)
, powHashNat
, encodePowHashNat
, decodePowHashNat

-- * HashTarget
, HashTarget(..)
, showTargetBits
, checkTarget
, maxTarget
, difficultyToTarget
, difficultyToTargetR
, targetToDifficulty
, targetToDifficultyR
, encodeHashTarget
, decodeHashTarget

-- * HashDifficulty
, HashDifficulty(..)
, encodeHashDifficulty
, decodeHashDifficulty

-- * Test Properties
, properties
, prop_littleEndian
) where

import Control.DeepSeq
import Control.Monad

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Bits
import Data.Bool (bool)
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Coerce
import Data.DoubleWord
import Data.Hashable
import Data.Ratio
import qualified Data.Text as T

import GHC.Generics
import GHC.TypeNats

import Test.QuickCheck (Property, property)

import Text.Printf (printf)

-- internal imports

import Chainweb.PowHash
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion, usePOW)

import Data.Word.Encoding hiding (properties)

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

powHashToWord256 :: 32 <= PowHashBytesCount => PowHash -> Word256
powHashToWord256 = either error id . runGetS decodeWordLe . SB.fromShort . powHashBytes
{-# INLINE powHashToWord256 #-}

encodePowHashNat :: MonadPut m => PowHashNat -> m ()
encodePowHashNat (PowHashNat n) = encodeWordLe n
{-# INLINE encodePowHashNat #-}

decodePowHashNat :: MonadGet m => m PowHashNat
decodePowHashNat = PowHashNat <$> decodeWordLe
{-# INLINE decodePowHashNat #-}

instance ToJSON PowHashNat where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodePowHashNat
    {-# INLINE toJSON #-}

instance FromJSON PowHashNat where
    parseJSON = withText "PowHashNat" $ either (fail . show) return
        . (runGet decodePowHashNat <=< decodeB64UrlNoPaddingText)
    {-# INLINE parseJSON #-}

instance ToJSONKey PowHashNat where
    toJSONKey = toJSONKeyText
        $ encodeB64UrlNoPaddingText . runPutS . encodePowHashNat
    {-# INLINE toJSONKey #-}

instance FromJSONKey PowHashNat where
    fromJSONKey = FromJSONKeyTextParser $ either (fail . show) return
        . (runGet decodePowHashNat <=< decodeB64UrlNoPaddingText)
    {-# INLINE fromJSONKey #-}

-- -------------------------------------------------------------------------- --
-- HashDifficulty

-- | Hash Difficulty
--
-- difficulty = maxBound / target
--            = network hash rate * block time
--
newtype HashDifficulty = HashDifficulty PowHashNat
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Bounded, Enum)
    deriving newtype (AdditiveSemigroup, AdditiveAbelianSemigroup)
    deriving newtype (Num, Integral, Real)

encodeHashDifficulty :: MonadPut m => HashDifficulty -> m ()
encodeHashDifficulty (HashDifficulty x) = encodePowHashNat x
{-# INLINE encodeHashDifficulty #-}

decodeHashDifficulty :: MonadGet m => m HashDifficulty
decodeHashDifficulty = HashDifficulty <$> decodePowHashNat
{-# INLINE decodeHashDifficulty #-}

-- -------------------------------------------------------------------------- --
-- HashTarget

-- | HashTarget
--
-- target = maxBound / (network hash rate * block time)
--        = maxBound / difficulty
--
-- network hash rate is interpolated from observered past block times.
--
newtype HashTarget = HashTarget PowHashNat
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable, Bounded)

-- | A visualization of a `HashTarget` as binary.
showTargetBits :: HashTarget -> T.Text
showTargetBits (HashTarget (PowHashNat n)) = T.pack . printf "%0256b" $ (int n :: Integer)

-- | By maximum, we mean "easiest". For POW-based chainwebs, this is reduced
-- down from `maxBound` so that the mining of initial blocks doesn't occur too
-- quickly, stressing the system, or otherwise negatively affecting difficulty
-- adjustment with very brief time deltas between blocks.
--
-- Otherwise, chainwebs with "trivial targets" expect this to be `maxBound` and
-- never change. See also `Chainweb.Version.usePOW`.
--
maxTarget :: ChainwebVersion -> HashTarget
maxTarget = HashTarget . PowHashNat . maxTargetWord

-- | A pre-reduction of 9 bits has experimentally been shown to be an
-- equilibrium point for the hash power provided by a single, reasonably
-- performant laptop in early 2019. It is further reduced from 9 to be merciful
-- to CI machines.
--
maxTargetWord :: ChainwebVersion -> Word256
maxTargetWord v = maxBound `div` (2 ^ prereduction v)

-- TODO This should probably dispatch on different values for `TestWithPow` and
-- `TestNet*` specifically.
prereduction :: ChainwebVersion -> Int
prereduction v = bool 0 7 $ usePOW v

instance IsMerkleLogEntry ChainwebHashTag HashTarget where
    type Tag HashTarget = 'HashTargetTag
    toMerkleNode = encodeMerkleInputNode encodeHashTarget
    fromMerkleNode = decodeMerkleInputNode decodeHashTarget
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- | Given the same `ChainwebVersion`, forms an isomorphism with
-- `targetToDifficulty`.
difficultyToTarget :: ChainwebVersion -> HashDifficulty -> HashTarget
difficultyToTarget v (HashDifficulty (PowHashNat difficulty)) =
    HashTarget . PowHashNat $ maxTargetWord v `div` difficulty
{-# INLINE difficultyToTarget #-}

-- | Like `difficultyToTarget`, but accepts a `Rational` that would have been
-- produced by `targetToDifficultyR` and then further manipulated during
-- Difficulty Adjustment.
difficultyToTargetR :: ChainwebVersion -> Rational -> HashTarget
difficultyToTargetR v difficulty =
    HashTarget . PowHashNat $ maxTargetWord v `div` floor difficulty
{-# INLINE difficultyToTargetR #-}

-- | Given the same `ChainwebVersion`, forms an isomorphism with
-- `difficultyToTarget`.
targetToDifficulty :: ChainwebVersion -> HashTarget -> HashDifficulty
targetToDifficulty v (HashTarget (PowHashNat target)) =
    HashDifficulty . PowHashNat $ maxTargetWord v `div` target
{-# INLINE targetToDifficulty #-}

-- | Like `targetToDifficulty`, but yields a `Rational` for lossless
-- calculations in Difficulty Adjustment.
targetToDifficultyR :: ChainwebVersion -> HashTarget -> Rational
targetToDifficultyR v (HashTarget (PowHashNat target)) =
    int (maxTargetWord v) % int target
{-# INLINE targetToDifficultyR #-}

-- | The critical check in Proof-of-Work mining: did the generated hash match
-- the target?
checkTarget :: HashTarget -> PowHash -> Bool
checkTarget (HashTarget target) h = powHashNat h <= target
{-# INLINE checkTarget #-}

encodeHashTarget :: MonadPut m => HashTarget -> m ()
encodeHashTarget = encodePowHashNat . coerce
{-# INLINE encodeHashTarget #-}

decodeHashTarget :: MonadGet m => m HashTarget
decodeHashTarget = HashTarget <$> decodePowHashNat
{-# INLINE decodeHashTarget #-}

-- -------------------------------------------------------------------------- --
-- Properties

prop_littleEndian :: Bool
prop_littleEndian = all run [1..31]
  where
    run i = (==) i
        $ length
        $ takeWhile (== 0x00)
        $ reverse
        $ B.unpack
        $ runPutS
        $ encodePowHashNat (maxBound `div` 2^(8*i))

properties :: [(String, Property)]
properties =
    [ ("BlockHashNat is encoded as little endian", property prop_littleEndian)
    ]
