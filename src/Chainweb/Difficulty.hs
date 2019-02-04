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
, checkTarget
, difficultyToTarget
, targetToDifficulty
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
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Coerce
import Data.DoubleWord
import Data.Hashable

import GHC.Generics
import GHC.TypeNats

import Test.QuickCheck (Property, property)

-- internal imports

import Chainweb.PowHash
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils

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
    deriving newtype (ToJSON, FromJSON, Hashable, Bounded, Enum)

instance IsMerkleLogEntry ChainwebHashTag HashTarget where
    type Tag HashTarget = 'HashTargetTag
    toMerkleNode = encodeMerkleInputNode encodeHashTarget
    fromMerkleNode = decodeMerkleInputNode decodeHashTarget
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

difficultyToTarget :: HashDifficulty -> HashTarget
difficultyToTarget difficulty = HashTarget $ maxBound `div` coerce difficulty
{-# INLINE difficultyToTarget #-}

targetToDifficulty :: HashTarget -> HashDifficulty
targetToDifficulty target = HashDifficulty $ maxBound `div` coerce target
{-# INLINE targetToDifficulty #-}

checkTarget :: HashTarget -> PowHash -> Bool
checkTarget target h = powHashNat h <= coerce target
{-# INLINE checkTarget #-}

encodeHashTarget :: MonadPut m => HashTarget -> m ()
encodeHashTarget = encodePowHashNat . coerce
{-# INLINE encodeHashTarget #-}

decodeHashTarget :: MonadGet m => m HashTarget
decodeHashTarget = HashTarget <$> decodePowHashNat
{-# INLINE decodeHashTarget #-}

-- -------------------------------------------------------------------------- --
-- Difficulty Computation

-- | FIXME: make the overflow checks tight
--
-- this algorithm introduces a rounding error in the order of the length of the
-- input list. We could reduce the error at the cost of larger numbers (and thus
-- more likely bound violations). We could also eliminate the risk of bound
-- violations at the cost of larger rounding errors. The current code is a
-- compromise.
--
-- calculateTarget
--     :: forall a
--     . Integral a
--     => TimeSpan a
--     -> [(HashTarget, TimeSpan a)]
--     -> HashTarget
-- calculateTarget targetTime l = HashTarget $ sum
--     [ weightedTarget trg (t2h t) w
--     | (HashTarget trg, t) <- l
--     | w <- [ (1::PowHashNat) ..]
--     ]
--   where
--     n :: PowHashNat
--     n = int $ length l

--     -- represent time span as integral number of milliseconds
--     --
--     t2h :: TimeSpan a -> PowHashNat
--     t2h t = int (coerce t :: a) `div` 1000

--     -- weight and n is in the order of 2^7
--     -- time spans are in the order of 2^17 milliseconds
--     --
--     -- Target should be < 2^231 (or difficulty should be larger than 2^25.
--     -- This corresponds to a hashrate of about 10M #/s with a 10s block time.
--     --
--     weightedTarget :: PowHashNat -> PowHashNat -> PowHashNat -> PowHashNat
--     weightedTarget target timeSpan weight
--         | numerator < target = error "arithmetic overflow in hash target calculation"
--         | denominator < timeSpan = error "arithmetic overfow in hash target calculation"
--         | otherwise = numerator `div` denominator
--       where
--         numerator = 2 * weight * target * t2h targetTime
--         denominator = n * (n + 1) * timeSpan

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
