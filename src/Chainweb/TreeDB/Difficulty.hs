{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.TreeDB.Difficulty
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
module Chainweb.TreeDB.Difficulty
  ( -- * Epoch-based Adjustment
    BlockRate(..)
  , WindowWidth(..)
  , hashTarget
  ) where

import Control.Lens ((^.))

import Data.Bits (countLeadingZeros)
import Data.Function ((&))
import qualified Data.HashSet as HS
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Ratio
import Data.Semigroup (Max(..), Min(..))

import Numeric.Natural (Natural)

import qualified Streaming.Prelude as P

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Difficulty
import Chainweb.Time (Time(..), TimeSpan(..))
import Chainweb.TreeDB
import Chainweb.Utils (int)
import Chainweb.Version (ChainwebVersion)

-- DEBUGGING ---
-- import Control.Monad (when)
-- import Chainweb.ChainId (testChainId)
-- import System.IO (hFlush, stdout)
-- import Text.Printf (printf)

---

-- | The gap in SECONDS that we desire between the Creation Time of subsequent
-- blocks in some chain.
--
newtype BlockRate = BlockRate Natural

-- | The number of blocks to be mined after a difficulty adjustment, before
-- considering a further adjustment. Critical for the "epoch-based" adjustment
-- algorithm seen in `hashTarget`.
--
newtype WindowWidth = WindowWidth Natural

-- | A potentially new `HashTarget`, based on the rate of mining success over
-- the previous N blocks.
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
-- === Adjustment Limits
--
-- Spikes in /HashRate/ may occur as the mining network grows. To ensure that
-- adjustment does not occur too quickly, we cap the total "significant bits of
-- change" as to no more than 3 bits in either the "harder" or "easier" direction
-- at one time. Experimentally, it has been shown than the maximum change should
-- be greater than \(e = 2.71828\cdots\) (/source needed/).
--
hashTarget
    :: forall db. TreeDb db
    => IsBlockHeader (DbEntry db)
    => db
    -> DbEntry db
    -> BlockRate
    -> WindowWidth
    -> IO HashTarget
hashTarget db bh (BlockRate blockRate) (WindowWidth ww)
    -- Intent: Neither the genesis block, nor any block whose height is not a
    -- multiple of the `BlockRate` shall be considered for adjustment.
    | isGenesisBlockHeader bh' = pure $! _blockTarget bh'
    | int (_blockHeight bh') `mod` ww /= 0 = pure $! _blockTarget bh'
    | otherwise = do
        start <- branchEntries db Nothing Nothing minr maxr lower upper
                 & P.map (^. isoBH)
                 & P.take (int ww)
                 & P.last_
                 & fmap fromJust  -- Thanks to the two guard conditions above,
                                  -- this will (should) always succeed.

        let
            -- The time difference in microseconds between when the earliest and
            -- latest blocks in the window were mined.
            delta :: Int64
            !delta = time bh' - time start

            -- The average time in seconds that it took to mine each block in
            -- the given window.
            avg :: Rational
            !avg | delta < 0 = error "hashTarget: Impossibly negative delta!"
                 | otherwise = (int delta % int ww) / 1000000

            -- The mining difficulty of the previous block (the parent) as a
            -- function of its `HashTarget`.
            oldDiff :: Rational
            !oldDiff = targetToDifficultyR ver $ _blockTarget bh'

            -- The adjusted difficulty, following the formula explained in the
            -- docstring of this function.
            newDiff :: Rational
            !newDiff = oldDiff * int blockRate / avg

            newTarget :: HashTarget
            !newTarget = difficultyToTargetR ver newDiff

            -- `newTarget` subjected to the "adjustment limit".
            actual :: HashTarget
            !actual
                -- Intent: When increasing the difficulty (thereby lowering the
                -- target toward 0), the leading 1-bit must not move more than 3
                -- bits at a time.
                | newTarget < _blockTarget bh' =
                      max newTarget (HashTarget $! bhNat `div` 8)
                -- Intent: Cap the new target back down, if it somehow managed
                -- to go over the maximum. This is possible during POW, since we
                -- assume @maxTarget < @maxBound@.
                | newTarget > maxTarget ver =
                      maxTarget ver
                -- Intent: When decreasing the difficulty (thereby raising the
                -- target toward `maxTarget`), ensure that the new target does
                -- not increase by more than 3 bits at a time. Using
                -- `countLeadingZeros` like this also helps avoid a `Word256`
                -- overflow.
                | countLeadingZeros bhNat - countLeadingZeros (nat newTarget) > 3 =
                      HashTarget $! bhNat * 8
                | otherwise =
                      newTarget

        -- DEBUGGING --
        -- Uncomment the following to get a live view of difficulty adjustment.
        -- You will have to readd a few imports, and also uncomment a few helper
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

        pure actual
  where
    bh' :: BlockHeader
    bh' = bh ^. isoBH

    bhNat :: PowHashNat
    bhNat = nat $ _blockTarget bh'

    ver :: ChainwebVersion
    ver = _blockChainwebVersion bh'

    -- Query parameters for `branchEntries`.
    minr = Just . MinRank $ Min 0
    maxr = Just . MaxRank . Max . fromIntegral $! _blockHeight bh'
    lower = HS.empty
    upper = HS.singleton . UpperBound $! key bh

    time :: BlockHeader -> Int64
    time h = case _blockCreationTime h of BlockCreationTime (Time (TimeSpan n)) -> n

    nat :: HashTarget -> PowHashNat
    nat (HashTarget n) = n

    -- floating :: Rational -> Double
    -- floating = realToFrac
