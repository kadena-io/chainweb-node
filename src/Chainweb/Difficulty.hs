{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
-- * BlockHashNat
  BlockHashNat(..)
, blockHashNat
, encodeBlockHashNat
, decodeBlockHashNat

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
, calculateTarget
) where

import Control.Monad

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Coerce
import Data.Hashable
import Data.DoubleWord

import GHC.Generics
import GHC.TypeNats

-- internal imports

import Chainweb.BlockHash
import Numeric.Additive
import Data.Word.Encoding
import Chainweb.Time
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- BlockHashNat

-- | A type that maps block hashes to unsigned 256 bit integers by
-- projecting onto the first 8 bytes (least significant in little
-- endian encoding) and interpreting them as little endian encoded
-- unsigned integer value.
--
-- Arithmetic is defined as unsigned bounded integer arithmetic.
-- Overflows result in an exception and may result in program abort.
--
newtype BlockHashNat = BlockHashNat Word256
    deriving (Show, Generic)
    deriving anyclass (Hashable)
    deriving newtype (Eq, Ord, Bounded, Enum)
    deriving newtype (Num, Integral, Real)
        -- FIXME implement checked arithmetic
        -- FIXME avoid usage of Num and co
    deriving newtype (AdditiveSemigroup, AdditiveAbelianSemigroup)
    -- deriving newtype (MultiplicativeSemigroup, MultiplicativeAbelianSemigroup, MultiplicativeGroup)
        -- FIXME use checked arithmetic instead

blockHashNat :: BlockHash -> BlockHashNat
blockHashNat (BlockHash _ bytes) = BlockHashNat $ blockHashBytesToWord256 bytes
{-# INLINE blockHashNat #-}

blockHashBytesToWord256 :: 32 <= BlockHashBytesCount => BlockHashBytes -> Word256
blockHashBytesToWord256 (BlockHashBytes bs) = either error id $ runGetS decodeWordLe bs
{-# INLINE blockHashBytesToWord256 #-}

encodeBlockHashNat :: MonadPut m => BlockHashNat -> m ()
encodeBlockHashNat (BlockHashNat n) = encodeWordLe n
{-# INLINE encodeBlockHashNat #-}

decodeBlockHashNat :: MonadGet m => m BlockHashNat
decodeBlockHashNat = BlockHashNat <$> decodeWordLe
{-# INLINE decodeBlockHashNat #-}

instance ToJSON BlockHashNat where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodeBlockHashNat
    {-# INLINE toJSON #-}

instance FromJSON BlockHashNat where
    parseJSON = withText "BlockHashNat" $ either (fail . show) return
        . (runGet decodeBlockHashNat <=< decodeB64UrlNoPaddingText)
    {-# INLINE parseJSON #-}

instance ToJSONKey BlockHashNat where
    toJSONKey = toJSONKeyText
        $ encodeB64UrlNoPaddingText . runPutS . encodeBlockHashNat
    {-# INLINE toJSONKey #-}

instance FromJSONKey BlockHashNat where
    fromJSONKey = FromJSONKeyTextParser $ either (fail . show) return
        . (runGet decodeBlockHashNat <=< decodeB64UrlNoPaddingText)
    {-# INLINE fromJSONKey #-}

-- -------------------------------------------------------------------------- --
-- HashDifficulty

-- | Hash Difficulty
--
-- difficulty = maxBound / target
--            = network hash rate * block time
--
newtype HashDifficulty = HashDifficulty BlockHashNat
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Bounded, Enum)
    deriving newtype (AdditiveSemigroup, AdditiveAbelianSemigroup)
    deriving newtype (Num, Integral, Real)

encodeHashDifficulty :: MonadPut m => HashDifficulty -> m ()
encodeHashDifficulty (HashDifficulty x) = encodeBlockHashNat x
{-# INLINE encodeHashDifficulty #-}

decodeHashDifficulty :: MonadGet m => m HashDifficulty
decodeHashDifficulty = HashDifficulty <$> decodeBlockHashNat
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
newtype HashTarget = HashTarget BlockHashNat
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Hashable, Bounded, Enum)

difficultyToTarget :: HashDifficulty -> HashTarget
difficultyToTarget difficulty = HashTarget $ maxBound `div` coerce difficulty
{-# INLINE difficultyToTarget #-}

targetToDifficulty :: HashTarget -> HashDifficulty
targetToDifficulty target = HashDifficulty $ maxBound `div` coerce target
{-# INLINE targetToDifficulty #-}

checkTarget :: HashTarget -> BlockHash -> Bool
checkTarget target h = blockHashNat h <= coerce target
{-# INLINE checkTarget #-}

encodeHashTarget :: MonadPut m => HashTarget -> m ()
encodeHashTarget = encodeBlockHashNat . coerce
{-# INLINE encodeHashTarget #-}

decodeHashTarget :: MonadGet m => m HashTarget
decodeHashTarget = HashTarget <$> decodeBlockHashNat
{-# INLINE decodeHashTarget #-}

-- -------------------------------------------------------------------------- --
-- Difficulty Computation

-- | FIXME: make the overflow checks tight
--
-- this algorithm introduces a rounding error in the order of
-- the length of the input list. We could reduce the error
-- at the cost of larger numbers (and thus more likely bound
-- violations). We could also eliminate the risk of bound
-- violations at the cost of larger rounding errors. The current
-- code is a compromise.
--
calculateTarget
    :: forall a
    . Integral a
    => TimeSpan a
    -> [(HashTarget, TimeSpan a)]
    -> HashTarget
calculateTarget targetTime l = HashTarget $ sum
    [ weightedTarget trg (t2h t) w
    | (HashTarget trg, t) <- l
    | w <- [ (1::BlockHashNat) ..]
    ]
  where
    n :: BlockHashNat
    n = int $ length l

    -- represent time span as integral number of milliseconds
    --
    t2h :: TimeSpan a -> BlockHashNat
    t2h t = int (coerce t :: a) `div` 1000

    -- weight and n is in the order of 2^7
    -- time spans are in the order of 2^17 milliseconds
    --
    -- Target should be < 2^231 (or difficulty should be larger than 2^25.
    -- This corresponds to a hashrate of about 10M #/s with a 10s block time.
    --
    weightedTarget :: BlockHashNat -> BlockHashNat -> BlockHashNat -> BlockHashNat
    weightedTarget target timeSpan weight
        | nominator < target = error "arithmetic overflow in hash target calculation"
        | denominator < timeSpan = error "arithmetic overfow in hash target calculation"
        | otherwise = nominator `div` denominator
      where
        nominator = 2 * weight * target * t2h targetTime
        denominator = n * (n + 1) * timeSpan

