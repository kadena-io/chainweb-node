{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.TreeDB.HashTarget
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
module Chainweb.TreeDB.HashTarget
  ( hashTargetFromHistory
  ) where

import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup (Max(..), Min(..))
import qualified Data.HashSet as HS

import Numeric.Additive (invert)
import Numeric.AffineSpace (Diff, add, diff)

import qualified Streaming.Prelude as P

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.Difficulty (HashTarget, calculateTarget)
import Chainweb.Time (Time, TimeSpan)
import Chainweb.TreeDB

---

{- NOTES

A `HashTarget` is a newtype around a `BlockHashNat`, and represents:

    target = maxBound / (network hash rate * block time) = maxBound / difficulty
    network hash rate is interpolated from observered past block times.

Likewise, a `HashDifficulty` is another wrapper around a `BlockHashNat`.

A `BlockHashNat` is a newtype around a `Word256`, which comes from the `data-dword` lib.

-}


-- | Compute what the `HashTarget` ought to be for a block, using its ancestry
-- in a window which goes back the given amount of time from its parent.
--
hashTargetFromHistory
    :: TreeDb db
    => DbEntry db ~ BlockHeader  -- TODO Use `IsBlockHeader` once Git Store PR is merged.
    => db
    -> DbEntry db
    -> TimeSpan Int64
    -> IO HashTarget
hashTargetFromHistory db bh ts = do
    es <- branchEntries db Nothing Nothing minr maxr lower upper
          & P.takeWhile (\h -> _blockCreationTime h > time)
          & P.toList_
          & fmap NEL.fromList

    let deltas :: [(HashTarget, TimeSpan Int64)]
        !deltas = zipWith (\x y -> (_blockTarget x, timeDelta x y)) (NEL.toList es) $ NEL.tail es

    let !start = _blockCreationTime $ NEL.head es
        !end = _blockCreationTime bh

    -- calculateTarget :: Integral a => TimeSpan a -> [(HashTarget, TimeSpan a)] -> HashTarget
    -- TODO Why is passing the original @ts :: TimeSpan Int64@ not sufficient?
    pure $! calculateTarget (diff end start) deltas
  where
    timeDelta :: BlockHeader -> BlockHeader -> Diff (Time Int64)
    timeDelta x y = diff (_blockCreationTime y) (_blockCreationTime x)

    time :: Time Int64
    time = invert ts `add` _blockCreationTime bh

    minr = Just . MinRank $ Min 0
    maxr = Just . MaxRank . Max . fromIntegral $ _blockHeight bh
    lower = HS.empty
    upper = HS.singleton . UpperBound $ key bh
