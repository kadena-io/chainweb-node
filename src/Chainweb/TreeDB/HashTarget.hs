{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

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

import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL

import Numeric.AffineSpace (Diff, diff)

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
    :: Monad m
    => TreeDb db
    => DbEntry db ~ BlockHeader  -- TODO Use `IsBlockHeader` once Git Store PR is merged.
    => db
    -> DbEntry db
    -> TimeSpan Int64
    -> m HashTarget
hashTargetFromHistory db bh dt = do
    es <- lookupTimeSpanFrom db bh dt
    let !targetDeltas = zipWith (\x y -> (_blockTarget x, timeDelta x y)) (NEL.toList es) $ NEL.tail es
        !start = _blockCreationTime $ NEL.head es
        !end = _blockCreationTime bh
    pure $! calculateTarget (diff end start) targetDeltas
  where
    timeDelta :: BlockHeader -> BlockHeader -> Diff (Time Int64)
    timeDelta x y = diff (_blockCreationTime y) (_blockCreationTime x)

lookupTimeSpanFrom
    :: TreeDb db
    => DbEntry db ~ BlockHeader
    => db
    -> DbEntry db
    -> TimeSpan Int64
    -> m (NonEmpty (DbEntry db))
lookupTimeSpanFrom = undefined
