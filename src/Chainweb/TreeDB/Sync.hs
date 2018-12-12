{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.TreeDB.Sync
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Sync a local `TreeDb` with that of some remote peer.
--

module Chainweb.TreeDB.Sync
  ( -- * Syncronizing a Chain
    sync
    -- * Temporary Export
  , Diameter(..)
  ) where

import Data.Semigroup (Min(..))

import Numeric.Natural (Natural)

import Refined hiding (NonEmpty)

import Streaming

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), BlockHeight(..))
import Chainweb.TreeDB

-- TODO The real version of this will be present elsewhere.
-- | The diameter of the current chain graph.
--
newtype Diameter = Diameter { diameter :: Refined (Positive && LessThan 10) Int }

-- | Given a peer to connect to, fetch all `BlockHeader`s that exist
-- in the peer's chain but not our local given `TreeDb`, and sync them.
--
sync
    :: (TreeDb local, TreeDb peer, DbEntry local ~ BlockHeader, DbEntry peer ~ BlockHeader)
    => Diameter
    -> local
    -> peer
    -> IO ()
sync d local peer = do
    h <- maxHeader local
    let m = minHeight (_blockHeight h) d
    void . insertStream local $ entries peer Nothing Nothing (Just m) Nothing

-- | Given a `BlockHeight` that represents the highest rank of some `TreeDb`,
-- find the lowest entry rank such that it's at most only
-- (diameter * 2) in height away.
--
minHeight :: BlockHeight -> Diameter -> MinRank
minHeight h d = MinRank $ Min m
  where
    m :: Natural
    m = fromIntegral (max (high - low) 0)

    -- | Using `Integer` prevents underflow errors.
    --
    high :: Integer
    high = fromIntegral h

    low :: Integer
    low = fromIntegral $ 2 * unrefine (diameter d)
