{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
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
  ( hashTarget
  ) where

import Control.Lens ((^.))

import Data.Bits (countLeadingZeros)
import Data.Coerce (coerce)
import Data.DoubleWord (Word256)
import Data.Function ((&))
import qualified Data.HashSet as HS
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Semigroup (Max(..), Min(..))

import qualified Streaming.Prelude as P

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), BlockHeight, IsBlockHeader(..))
import Chainweb.Difficulty
import Chainweb.Time (Time(..), TimeSpan(..))
import Chainweb.TreeDB
import Chainweb.Utils (int)

---

-- | Compute what the `HashTarget` ought to be for a block, using its ancestry
-- in a window which goes back the given amount of time from its parent.
--
-- hashTargetFromHistory
--     :: forall db. TreeDb db
--     => IsBlockHeader (DbEntry db)
--     => db
--     -> DbEntry db
--     -> TimeSpan Int64
--     -> IO (T2 HashTarget Int)
-- hashTargetFromHistory db bh ts
--     | _blockHeight bh' < 10 = pure (T2 maxTarget 0)
--     | otherwise = do
--         -- putStrLn "Walk the branch..." >> hFlush stdout

--         -- Thanks to `P.takeWhile`, will not stream more than it has to.
--         --
--         -- INVARIANT: This excludes the genesis block, whose Creation Time is
--         -- given as the Linux Epoch, @Time (TimeSpan 0)@ (at least for TestNet).
--         -- Excluding the genesis block in this way ensures that the time gap
--         -- between it and the first mined block won't adversely affect
--         -- difficulty adjustment.
--         --
--         es <- branchEntries db Nothing Nothing minr maxr lower upper
--               & P.map (^. isoBH)
--               & P.takeWhile (\h -> _blockCreationTime h > time)
--               & P.toList_
--               & fmap (NEL.reverse . NEL.fromList)

--         -- printf "WINDOW: %d\n" (length es) >> hFlush stdout

--         let !target = min maxTarget (hashTargetFromHistory' bh' es)

--         ------------
--         -- DEBUGGING
--         ------------
--         -- printf "WINDOW SIZE: %d. TARGET BITS: %d. TIME: %s\n" (length es) (popCount $! fwip target) (show time)
--         -- hFlush stdout
--         pure (T2 target $! length es)

--     -- pure $! hashTargetFromHistory' bh' es
--   where
--     bh' :: BlockHeader
--     bh' = bh ^. isoBH

--     end :: Time Int64
--     end = _blockCreationTime bh'

--     time :: Time Int64
--     time = invert ts `add` end

--     minr = Just . MinRank $ Min 0
--     maxr = Just . MaxRank . Max . fromIntegral $! _blockHeight bh'
--     lower = HS.empty
--     upper = HS.singleton . UpperBound $! key bh

--     maxTarget :: HashTarget
--     maxTarget = HashTarget $! maxBound `div` 1024

--     fwip :: HashTarget -> Word256
--     fwip (HashTarget (BlockHashNat n)) = n

-- | A pure variant, for when you already have the window in memory. It's
-- assumed that the `NEL.NonEmpty` is sorted by `BlockHeight`.
--
-- hashTargetFromHistory' :: BlockHeader -> NEL.NonEmpty BlockHeader -> HashTarget
-- hashTargetFromHistory' bh es = calculateTarget (diff end start) deltas
--   where
--     timeDelta :: BlockHeader -> BlockHeader -> Diff (Time Int64)
--     timeDelta earlier later = diff (_blockCreationTime later) (_blockCreationTime earlier)

--     deltas :: [(HashTarget, TimeSpan Int64)]
--     deltas = zipWith (\x y -> (_blockTarget x, timeDelta x y)) (NEL.toList es) $ NEL.tail es

--     start = _blockCreationTime $ NEL.head es

--     end :: Time Int64
--     end = _blockCreationTime bh

-- | A potentially new `HashTarget`, based on the rate of mining success over
-- the previous N blocks.
--
hashTarget
    :: forall db. TreeDb db
    => IsBlockHeader (DbEntry db)
    => db
    -> DbEntry db
    -> IO HashTarget
hashTarget db bh
    | _blockHeight bh' == 0 = pure $! _blockTarget bh'
    | _blockHeight bh' `mod` magicNumber /= 0 = pure $! _blockTarget bh'
    | otherwise = do
        start <- branchEntries db Nothing Nothing minr maxr lower upper
                 & P.map (^. isoBH)
                 & P.take (int magicNumber)
                 & P.last_
                 & fmap fromJust  -- Will include at least the parent block. In
                                  -- the degenerate case, this is the genesis
                                  -- block.

        let delta :: Int64
            !delta = coerce (_blockCreationTime bh') - coerce (_blockCreationTime start)

            -- Microseconds. `succ` guards against divide-by-zero in `newDiff`,
            -- which can occur when initial blocks are mined very quickly. In
            -- this case, an average block creation time of @succ 0 == 1@ has
            -- special meaning: "far too fast".
            avg :: Int64
            !avg = succ $ delta `div` int magicNumber

            -- TODO Watch for overflows?
            newDiff :: HashDifficulty
            !newDiff = targetToDifficulty (_blockTarget bh') * int blockRate `div` int avg

            newTarget :: HashTarget
            !newTarget = difficultyToTarget newDiff

        if | newTarget < _blockTarget bh' -> pure $! max newTarget (_blockTarget bh' `div` 8)
           | countLeadingZeros (_blockTarget bh') < 3 -> pure newTarget
           | otherwise -> pure $! min newTarget (_blockTarget bh' * 8)
  where
    magicNumber :: BlockHeight
    magicNumber = 3  -- Ideally, three blocks in 30s. Not realistic, but I want
                     -- to test. In prod this would come from config.

    -- | 10 seconds as microseconds. In prod this should also come from config.
    blockRate :: Word256
    blockRate = 10 * 1000000

    bh' :: BlockHeader
    bh' = bh ^. isoBH

    minr = Just . MinRank $ Min 0
    maxr = Just . MaxRank . Max . fromIntegral $! _blockHeight bh'
    lower = HS.empty
    upper = HS.singleton . UpperBound $! key bh
