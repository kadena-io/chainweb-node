{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.TreeDB.HashTarget
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
module Chainweb.TreeDB.HashTarget
  ( BlockRate(..)
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

---

-- | The gap in SECONDS that we desire between the Creation Time of subsequent
-- blocks in some chain.
--
newtype BlockRate = BlockRate Natural

-- | The number of blocks to be mined after a difficulty adjustment, before
-- considering a further adjustment. Critical for the "epoch-based" adjustment
-- algorithm seen in `hashTarget.`
--
newtype WindowWidth = WindowWidth Natural

-- | A potentially new `HashTarget`, based on the rate of mining success over
-- the previous N blocks.
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
            !oldDiff = targetToDifficulty' $ _blockTarget bh'

            -- The adjusted difficulty, following the formula explained in the
            -- docstring of this function.
            newDiff :: Rational
            !newDiff = oldDiff * int blockRate / avg

            newTarget :: HashTarget
            !newTarget = difficultyToTarget' newDiff

            -- `newTarget` subjected to the "adjustment limit".
            actual :: HashTarget
            !actual
                -- Intent: When increasing the difficulty (thereby lowering the
                -- target toward 0), the leading 1-bit must not move more than 3
                -- bits at a time.
                | newTarget < _blockTarget bh' = max newTarget (_blockTarget bh' `div` 8)
                -- Intent: When decreasing the difficulty (thereby raising the
                -- target toward `maxTarget`), avoid a potential `Word256`
                -- overflow. TODO: count of leading zeros should depend on
                -- `maxTarget`!
                | countLeadingZeros (_blockTarget bh') < 3 = newTarget
                -- Intent: Otherwise, ensure that the new target does not
                -- increase by more than 3 bits at a time.
                | otherwise = min newTarget (_blockTarget bh' * 8)

        -- DEBUGGING --
        -- Uncomment the following to get a live view of difficulty adjustment.
        -- You will have to readd a few imports, and also uncomment a few helper
        -- functions below.

        -- when (_blockChainId bh' == testChainId 0)
        --     $ printf "\n=== CHAIN:%s\n=== HEIGHT:%s\n=== AVG: %f\n=== OLD DIFF:%f\n=== NEW DIFF:%f\n=== ORIGINAL:%s\n=== ADJUSTED:%s\n=== ACCEPTED:%s\n"
        --           (show $ _blockChainId bh')
        --           (show $ _blockHeight bh')
        --           (floating avg)
        --           (floating oldDiff)
        --           (floating newDiff)
        --           (targetBits $ _blockTarget bh')
        --           (targetBits newTarget)
        --           (targetBits actual)

        pure actual
  where
    bh' :: BlockHeader
    bh' = bh ^. isoBH

    -- Query parameters for `branchEntries`.
    minr = Just . MinRank $ Min 0
    maxr = Just . MaxRank . Max . fromIntegral $! _blockHeight bh'
    lower = HS.empty
    upper = HS.singleton . UpperBound $! key bh

    time :: BlockHeader -> Int64
    time h = case _blockCreationTime h of BlockCreationTime (Time (TimeSpan n)) -> n

    -- targetBits :: HashTarget -> String
    -- targetBits = printf "%0256b" . htInteger

    -- floating :: Rational -> Double
    -- floating = realToFrac

    -- htInteger :: HashTarget -> Integer
    -- htInteger (HashTarget (PowHashNat w)) = fromIntegral w
