{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- * PowHashNat
  PowHashNat(..)
, powHashNat
, encodePowHashNat
, decodePowHashNat
, encodePowHashNatBe
, decodePowHashNatBe

-- * HashTarget
, HashTarget(..)
, showTargetHex
, showTargetBits
, checkTarget
, maxTarget
, maxTargetWord
, difficultyToTarget
, targetToDifficulty
, encodeHashTarget
, decodeHashTarget

-- * HashDifficulty
, HashDifficulty(..)
, encodeHashDifficulty
, decodeHashDifficulty
, encodeHashDifficultyBe
, decodeHashDifficultyBe

-- * Difficulty Adjustment
, adjust

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
import Data.Ratio ((%))
import qualified Data.Text as T

import GHC.Generics
import GHC.TypeNats

import Numeric.Natural (Natural)

import Test.QuickCheck (Property, property)

import Text.Printf (printf)

-- internal imports

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.PowHash
import Chainweb.Time (Micros(..), Seconds(..), TimeSpan(..))
import Chainweb.Utils
import Chainweb.Version

import Data.Word.Encoding hiding (properties)

import Numeric.Additive

-- DEBUGGING ---
-- import Control.Monad (when)
-- import Chainweb.ChainId (testChainId)
-- import System.IO (hFlush, stdout)
-- import Text.Printf (printf)

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
powHashToWord256 = either error id . runGetS decodeWordLe . SB.fromShort . powHashBytes
{-# INLINE powHashToWord256 #-}

encodePowHashNat :: MonadPut m => PowHashNat -> m ()
encodePowHashNat (PowHashNat n) = encodeWordLe n
{-# INLINE encodePowHashNat #-}

decodePowHashNat :: MonadGet m => m PowHashNat
decodePowHashNat = PowHashNat <$!> decodeWordLe
{-# INLINE decodePowHashNat #-}

encodePowHashNatBe :: MonadPut m => PowHashNat -> m ()
encodePowHashNatBe (PowHashNat n) = encodeWordBe n
{-# INLINE encodePowHashNatBe #-}

decodePowHashNatBe :: MonadGet m => m PowHashNat
decodePowHashNatBe = PowHashNat <$!> decodeWordBe
{-# INLINE decodePowHashNatBe #-}

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
decodeHashDifficulty = HashDifficulty <$!> decodePowHashNat
{-# INLINE decodeHashDifficulty #-}

encodeHashDifficultyBe :: MonadPut m => HashDifficulty -> m ()
encodeHashDifficultyBe (HashDifficulty x) = encodePowHashNatBe x
{-# INLINE encodeHashDifficultyBe #-}

decodeHashDifficultyBe :: MonadGet m => m HashDifficulty
decodeHashDifficultyBe = HashDifficulty <$!> decodePowHashNatBe
{-# INLINE decodeHashDifficultyBe #-}

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

instance IsMerkleLogEntry ChainwebHashTag HashTarget where
    type Tag HashTarget = 'HashTargetTag
    toMerkleNode = encodeMerkleInputNode encodeHashTarget
    fromMerkleNode = decodeMerkleInputNode decodeHashTarget
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- | Given the same `ChainwebVersion`, forms an isomorphism with
-- `targetToDifficulty`.
--
difficultyToTarget :: HashDifficulty -> HashTarget
difficultyToTarget (HashDifficulty (PowHashNat difficulty)) =
    HashTarget . PowHashNat $ maxTargetWord `div` difficulty
{-# INLINE difficultyToTarget #-}

-- | Like `difficultyToTarget`, but accepts a `Rational` that would have been
-- produced by `targetToDifficultyR` and then further manipulated during
-- Difficulty Adjustment.
--
difficultyToTargetR :: Rational -> HashTarget
difficultyToTargetR difficulty =
    -- `ceiling` is chosen here, to avoid the (hopefully rare) case where the
    -- `Rational` given is between 0 and 1. `floor` instead would drop that to
    -- 0, making the `div` crash. At most, this would "spuriously" raise the
    -- difficulty by at most 0.999... (~1) hash, which is negligible.
    HashTarget . PowHashNat $ maxTargetWord `div` ceiling difficulty
{-# INLINE difficultyToTargetR #-}

-- | Given the same `ChainwebVersion`, forms an isomorphism with
-- `difficultyToTarget`.
--
targetToDifficulty :: HashTarget -> HashDifficulty
targetToDifficulty (HashTarget (PowHashNat target)) =
    HashDifficulty . PowHashNat $ maxTargetWord `div` target
{-# INLINE targetToDifficulty #-}

-- | Like `targetToDifficulty`, but yields a `Rational` for lossless
-- calculations in Difficulty Adjustment.
--
targetToDifficultyR :: HashTarget -> Rational
targetToDifficultyR (HashTarget (PowHashNat target)) =
    int maxTargetWord % int target
{-# INLINE targetToDifficultyR #-}

-- | The critical check in Proof-of-Work mining: did the generated hash match
-- the target?
--
checkTarget :: HashTarget -> PowHash -> Bool
checkTarget (HashTarget target) h = powHashNat h <= target
{-# INLINE checkTarget #-}

encodeHashTarget :: MonadPut m => HashTarget -> m ()
encodeHashTarget = encodePowHashNat . coerce
{-# INLINE encodeHashTarget #-}

decodeHashTarget :: MonadGet m => m HashTarget
decodeHashTarget = HashTarget <$!> decodePowHashNat
{-# INLINE decodeHashTarget #-}

-- -------------------------------------------------------------------------- --
-- Difficulty Adjustment

-- | A new `HashTarget`, based on the rate of mining success over the previous N
-- blocks.
--
-- == Epoch-based Difficulty Adjustment
--
-- This function represents a Bitcoin-inspired, "epoch-based" adjustment
-- algorithm. For every N blocks (as defined by `WindowWidth`), we perform an
-- adjustment.
--
-- === Terminology
--
-- `BlockHeader` stores a 256-bit measure of difficulty: `HashTarget`. More
-- precisely, `HashTarget` is a derivation (seen below) of the `HashDifficulty`.
-- `HashDifficulty` in itself is roughly a measure of the number of hashes
-- necessary to "solve" a block. For non-POW testing scenarios that use trivial
-- targets (i.e. `maxBound`), then difficulty is exactly the number of necessary
-- hashes. For POW mining, this is offset. See `maxTarget`.
--
-- A `HashDifficulty` of 1 is considered the "easiest" difficulty, and
-- represents a `HashTarget` of `maxTarget`. There must never be a difficulty of
-- 0.
--
-- Given the same `Chainweb.Version.ChainwebVersion`, the functions
-- `targetToDifficulty` and `difficultyToTarget` form an isomorphism between the
-- above mentioned types.
--
-- === Justification
--
-- We define the maximum possible hash target (the "easiest" target) as follows:
--
-- \[
-- \begin{align*}
--   \text{MaxBound} &= 2^{256} - 1 \\
--   \text{MaxTarget} &= \frac{\text{MaxBound}}{2^{\text{offset}}}
-- \end{align*}
-- \]
--
-- where /offset/ is some number of bits, 0 for trivial scenarios and some
-- experimentally discovered \(N\) for real POW mining scenarios. For Bitcoin,
-- \(N = 32\).
--
-- Given some difficulty \(D\), its corresponding `HashTarget` can be found by:
--
-- \[
-- \text{Target} = \frac{\text{MaxTarget}}{D}
-- \]
--
-- During adjustment, we seek to solve for some new \(D\). From the above, it
-- follows that the expected number of hashes necessary to "solve" a block
-- becomes:
--
-- \[
-- \text{Expected} = \frac{D * \text{MaxBound}}{\text{MaxTarget}}
-- \]
--
-- If we expect a block to be solved every \(R\) seconds, we find our total
-- Network Hash Rate:
--
-- \[
-- \text{HashRate} = \frac{\text{Expected}}{R}
-- \]
--
-- But, as a block chain is a dynamic system, the real time it took to mine some
-- block would likely not be exactly \(R\). This implies:
--
-- \[
-- \begin{align*}
--   \frac{\text{Expected}}{R} &= \text{HashRate} = \frac{\text{Expected}'}{M} \\
--   \frac{D * \text{MaxBound}}{R * \text{MaxTarget}} &= \text{HashRate} = \frac{D' * \text{MaxBound}}{M * \text{MaxTarget}} \\
--   \frac{D}{R} &= \text{HashRate} = \frac{D'}{M}
-- \end{align*}
-- \]
--
-- where \(D'\) is the known difficulty from the previous block, \(M\) is the
-- average time in seconds it took to calculate the previous \(B\) blocks. The
-- value of \(B\) is assumed to be configurable.
--
-- Given this, our new \(D\) is a simple ratio:
--
-- \[
-- D = \frac{D' * R}{M}
-- \]
--
-- /HashRate/ will of course not stay fixed as the network grows. Luckily, the
-- difference in \(M\) values will naturally correct for this in the calculation
-- of a new \(D\).
--
-- === Precision
--
-- In real systems, the difference between \(M\) and \(R\) may be minute. To
-- ensure that:
--
--   * differences are not lost to integer-math rounding errors
--   * adjustment actually occurs
--   * small, incremental adjustments are allowed to build into greater change over time
--   * `Word256`-based overflows do not occur
--   * the algorithm is simple
--
-- we use the infinite-precision `Rational` type in the calculation of the new
-- \(D\). Only when being converted to a final `HashTarget` is the non-integer
-- precision discarded.
--
-- /Note/: Use of `Rational` is likely not our final solution, and complicates
-- any cross-language spec we would write regarding adjustment algorithm
-- expectations. For now, however, `Rational` is stable for a Haskell-only
-- environment.
--
adjust :: ChainwebVersion -> WindowWidth -> TimeSpan Micros -> HashTarget -> HashTarget
adjust ver (WindowWidth ww) (TimeSpan delta) oldTarget = newTarget
  where
    br :: Natural
    br = case blockRate ver of
        BlockRate (Seconds n) -> int n

    -- The average time in seconds that it took to mine each block in
    -- the given window.
    avg :: Rational
    avg | delta < 0 = error "adjust: Impossibly negative delta!"
        | otherwise = (int delta % int ww) / 1000000

    -- The mining difficulty of the previous block (the parent) as a
    -- function of its `HashTarget`.
    oldDiff :: Rational
    oldDiff = targetToDifficultyR oldTarget

    -- The adjusted difficulty, following the formula explained in the
    -- docstring of this function.
    newDiff :: Rational
    newDiff = oldDiff * int br / avg

    newTarget :: HashTarget
    newTarget = difficultyToTargetR newDiff

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
