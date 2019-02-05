{-# LANGUAGE AllowAmbiguousTypes #-}
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
  ( hashTargetFromHistory
  , hashTargetFromHistory'
  ) where

import Control.Lens ((^.))

import Data.Function ((&))
import qualified Data.HashSet as HS
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup (Max(..), Min(..))

import Numeric.Additive (invert)
import Numeric.AffineSpace (Diff, add, diff)

import qualified Streaming.Prelude as P

-- internal modules

import Chainweb.BlockHeader
    (BlockHeader(..), IsBlockHeader(..), blockCreationTime, blockHeight)
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
    :: forall db. TreeDb db
    => IsBlockHeader (DbEntry db)
    => db
    -> DbEntry db
    -> TimeSpan Int64
    -> IO HashTarget
hashTargetFromHistory db bh ts = do
    es <- branchEntries db Nothing Nothing minr maxr lower upper
          & P.map (^. isoBH)
          & P.takeWhile (\h -> _blockCreationTime h > time)
          & P.toList_
          & fmap (NEL.reverse . NEL.fromList)

    pure $! hashTargetFromHistory' bh es
  where
    end :: Time Int64
    end = bh ^. isoBH . blockCreationTime

    time :: Time Int64
    time = invert ts `add` end

    minr = Just . MinRank $ Min 0
    maxr = Just . MaxRank . Max . fromIntegral $ bh ^. isoBH . blockHeight
    lower = HS.empty
    upper = HS.singleton . UpperBound $ key bh

-- | A pure variant, for when you already have the window in memory. It's
-- assumed that the `NEL.NonEmpty` is sorted by `BlockHeight`.
--
hashTargetFromHistory' :: IsBlockHeader bh => bh -> NEL.NonEmpty BlockHeader -> HashTarget
hashTargetFromHistory' bh es = calculateTarget (diff end start) deltas
  where
    timeDelta :: BlockHeader -> BlockHeader -> Diff (Time Int64)
    timeDelta x y = diff (_blockCreationTime y) (_blockCreationTime x)

    deltas :: [(HashTarget, TimeSpan Int64)]
    deltas = zipWith (\x y -> (_blockTarget x, timeDelta x y)) (NEL.toList es) $ NEL.tail es

    start = _blockCreationTime $ NEL.head es

    end :: Time Int64
    end = bh ^. isoBH . blockCreationTime
