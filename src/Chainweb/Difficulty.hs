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
, maxTargetWord
, difficultyToTarget
, targetToDifficulty
, encodeHashTarget
, decodeHashTarget

-- * HashDifficulty
, HashDifficulty(..)
, encodeHashDifficulty
, decodeHashDifficulty

-- * Difficulty Adjustment
-- ** Fork-specific Settings
-- | Values here represent fixed settings specific to a particular chainweb.
-- __Changing any of these for a live Proof-of-Work chainweb will result in a hard fork.__
, BlockRate(..)
, blockRate
, WindowWidth(..)
, window
, MinAdjustment(..)
, minAdjust
, prereduction
-- ** Adjustment
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
import Data.Int (Int64)
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
import Chainweb.Time (Seconds, TimeSpan(..))
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))

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
--
showTargetBits :: HashTarget -> T.Text
showTargetBits (HashTarget (PowHashNat n)) = T.pack . printf "%0256b" $ (int n :: Integer)

-- | By maximum, we mean "easiest". For POW-based chainwebs, this is reduced
-- down from `maxBound` so that the mining of initial blocks doesn't occur too
-- quickly, stressing the system, or otherwise negatively affecting difficulty
-- adjustment with very brief time deltas between blocks.
--
-- Otherwise, chainwebs with "trivial targets" expect this to be `maxBound` and
-- never change. See also `prereduction`.
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

instance IsMerkleLogEntry ChainwebHashTag HashTarget where
    type Tag HashTarget = 'HashTargetTag
    toMerkleNode = encodeMerkleInputNode encodeHashTarget
    fromMerkleNode = decodeMerkleInputNode decodeHashTarget
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- | Given the same `ChainwebVersion`, forms an isomorphism with
-- `targetToDifficulty`.
--
difficultyToTarget :: ChainwebVersion -> HashDifficulty -> HashTarget
difficultyToTarget v (HashDifficulty (PowHashNat difficulty)) =
    HashTarget . PowHashNat $ maxTargetWord v `div` difficulty
{-# INLINE difficultyToTarget #-}

-- | Like `difficultyToTarget`, but accepts a `Rational` that would have been
-- produced by `targetToDifficultyR` and then further manipulated during
-- Difficulty Adjustment.
--
difficultyToTargetR :: ChainwebVersion -> Rational -> HashTarget
difficultyToTargetR v difficulty =
    HashTarget . PowHashNat $ maxTargetWord v `div` floor difficulty
{-# INLINE difficultyToTargetR #-}

-- | Given the same `ChainwebVersion`, forms an isomorphism with
-- `difficultyToTarget`.
--
targetToDifficulty :: ChainwebVersion -> HashTarget -> HashDifficulty
targetToDifficulty v (HashTarget (PowHashNat target)) =
    HashDifficulty . PowHashNat $ maxTargetWord v `div` target
{-# INLINE targetToDifficulty #-}

-- | Like `targetToDifficulty`, but yields a `Rational` for lossless
-- calculations in Difficulty Adjustment.
--
targetToDifficultyR :: ChainwebVersion -> HashTarget -> Rational
targetToDifficultyR v (HashTarget (PowHashNat target)) =
    int (maxTargetWord v) % int target
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
decodeHashTarget = HashTarget <$> decodePowHashNat
{-# INLINE decodeHashTarget #-}

-- -------------------------------------------------------------------------- --
-- Difficulty Adjustment

-- | The gap in SECONDS that we desire between the Creation Time of subsequent
-- blocks in some chain.
--
newtype BlockRate = BlockRate Seconds

-- | The Proof-of-Work `BlockRate` for each `ChainwebVersion`. This is the
-- number of seconds we expect to pass while a miner mines on various chains,
-- eventually succeeding on one.
--
blockRate :: ChainwebVersion -> Maybe BlockRate
blockRate Test{} = Nothing
blockRate TestWithTime{} = Just $ BlockRate 4
blockRate TestWithPow{} = Just $ BlockRate 10
-- 120 blocks per hour, 2,880 per day, 20,160 per week, 1,048,320 per year.
blockRate Testnet00 = Just $ BlockRate 30
-- 120 blocks per hour, 2,880 per day, 20,160 per week, 1,048,320 per year.
blockRate Testnet01 = Just $ BlockRate 30

-- | The number of blocks to be mined after a difficulty adjustment, before
-- considering a further adjustment. Critical for the "epoch-based" adjustment
-- algorithm seen in `hashTarget`.
--
newtype WindowWidth = WindowWidth Natural

-- | The Proof-of-Work `WindowWidth` for each `ChainwebVersion`. For chainwebs
-- that do not expect to perform POW, this should be `Nothing`.
--
window :: ChainwebVersion -> Maybe WindowWidth
window Test{} = Nothing
window TestWithTime{} = Nothing
-- 5 blocks, should take 50 seconds.
window TestWithPow{} = Just $ WindowWidth 5
-- 120 blocks, should take 1 hour given a 30 second BlockRate.
window Testnet00 = Just $ WindowWidth 120
-- 120 blocks, should take 1 hour given a 30 second BlockRate.
window Testnet01 = Just $ WindowWidth 120

-- | The minimum factor of change that a single application of `adjust` must
-- apply to some `HashTarget` for it to be accepted. As mentioned in `adjust`,
-- this value should be above \(e = 2.71828\cdots\).
--
newtype MinAdjustment = MinAdjustment Natural

-- | The Proof-of-Work `MinAdjustment` for each `ChainwebVersion`. For chainwebs
-- that do not expect to perform POW, this should be `Nothing`.
--
minAdjust :: ChainwebVersion -> Maybe MinAdjustment
minAdjust Test{} = Nothing
minAdjust TestWithTime{} = Nothing
minAdjust TestWithPow{} = Just $ MinAdjustment 3
-- See `adjust` for motivation.
minAdjust Testnet00 = Just $ MinAdjustment 3
minAdjust Testnet01 = Just $ MinAdjustment 3

-- | The number of bits to offset `maxTarget` by from `maxBound`, so as to
-- enforce a "minimum difficulty", beyond which mining cannot become easier.
--
-- See `adjust`.
--
prereduction :: ChainwebVersion -> Int
prereduction Test{} = 0
prereduction TestWithTime{} = 0
prereduction TestWithPow{} = 7
-- As alluded to in `maxTarget`, 11 bits has been shown experimentally to be
-- high enough to keep mining slow during the initial conditions of a
-- single-machine-10-chain-10-miner scenario, thereby avoiding (too many)
-- aggressive forks. Therefore for a machine running a single miner across 10
-- chains, we increase this further to 14 (3 bits => 8x harder) to come as close
-- as possible to a stable, "preadjusted" state, such that mining rates won't be
-- wildly imbalanced over the first few days. Subsequent Difficulty Adjustment
-- compensates for any remaining imbalance.
prereduction Testnet00 = 14
prereduction Testnet01 = 14

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
-- === Adjustment Minimums
--
-- Spikes in /HashRate/ may occur as the mining network grows and shrinks. To
-- ensure that adjustment does not occur too quickly or with too much
-- granularity, we enforce a "minimum factor of change" ( \(Z\) ) for `adjust`.
-- If `adjust` notices that the change for the given window is not large enough,
-- then it will not occur. The overall pattern of difficulty then becomes less
-- of a rippling pond, and more of a series of plateaus with distinct jumps.
-- Analysis has been shown that \(Z\) should be greater than a factor of
-- \(e = 2.71828\cdots\) (/source needed/). See also `minAdjust`.
--
adjust :: ChainwebVersion -> TimeSpan Int64 -> HashTarget -> HashTarget
adjust ver (TimeSpan delta) oldTarget
    -- Intent: When increasing the difficulty (thereby lowering the target
    -- toward 0), the target must decrease by at least some minimum threshold
    -- (usually 3x) to be accepted.
    | nat newTarget <= (nat oldTarget `div` minAdj) = newTarget

    -- Intent: When decreasing the difficulty (thereby raising the target toward
    -- `maxTarget`), ensure that the new target increases by at least some
    -- minimum threshold.
    | nat newTarget >= (nat oldTarget * minAdj)
      && nat oldTarget <= (nat (maxTarget ver) `div` minAdj) = newTarget

    -- Intent: The target did not change enough - do not alter it!
    | otherwise = oldTarget

    -- DEBUGGING --
    -- Uncomment the following to get a live view of difficulty adjustment. You
    -- will have to readd a few imports, and also uncomment a few helper
    -- functions below.

    -- when (_blockChainId bh' == testChainId 0) $ do
    --     printf "\n=== CHAIN:%s\n=== HEIGHT:%s\n=== AVG:%f\n=== RATE:%d\n=== OLD DIFF:%f\n=== NEW DIFF:%f\n=== ORIGINAL:%s\n=== ADJUSTED:%s\n=== ACCEPTED:%s\n"
    --         (show $ _blockChainId bh')
    --         (show $ _blockHeight bh')
    --         (floating avg)
    --         blockRate
    --         (floating oldDiff)
    --         (floating newDiff)
    --         (targetBits $ _blockTarget bh')
    --         (targetBits newTarget)
    --         (targetBits actual)
    --     hFlush stdout
  where
    br :: Natural
    br = case blockRate ver of
        Just (BlockRate n) -> int n
        Nothing -> error $ "adjust: Difficulty adjustment attempted on non-POW chainweb: " <> show ver

    ww :: Natural
    ww = case window ver of
        Just (WindowWidth n) -> n
        Nothing -> error $ "adjust: Difficulty adjustment attempted on non-POW chainweb: " <> show ver

    minAdj :: PowHashNat
    minAdj = case minAdjust ver of
        Just (MinAdjustment n) -> int n
        Nothing -> error $ "adjust: Difficulty adjustment attempted on non-POW chainweb: " <> show ver

    -- The average time in seconds that it took to mine each block in
    -- the given window.
    avg :: Rational
    avg | delta < 0 = error "adjust: Impossibly negative delta!"
        | otherwise = (int delta % int ww) / 1000000

    -- The mining difficulty of the previous block (the parent) as a
    -- function of its `HashTarget`.
    oldDiff :: Rational
    oldDiff = targetToDifficultyR ver oldTarget

    -- The adjusted difficulty, following the formula explained in the
    -- docstring of this function.
    newDiff :: Rational
    newDiff = oldDiff * int br / avg

    newTarget :: HashTarget
    newTarget = difficultyToTargetR ver newDiff

    nat :: HashTarget -> PowHashNat
    nat (HashTarget n) = n

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
