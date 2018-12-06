{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.ChainDB.Sync
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Sync a local `ChainDb` with that of some remote peer.
--

module Chainweb.ChainDB.Sync
  ( -- * Syncronizing a Chain
    sync
  -- , headers
    -- * Temporary Export
  , Diameter(..)
  ) where

import Control.Monad.Catch (throwM)

import Data.Semigroup (Min(..))

import Numeric.Natural (Natural)

import Refined hiding (NonEmpty)

import Servant.Client hiding (client)

import Streaming
import qualified Streaming.Prelude as SP

-- internal modules

import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader (BlockHeader(..), BlockHeight(..))
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI.Client (headersClient)
import Chainweb.TreeDB
import Chainweb.Utils.Paging (NextItem(..), Page(..))

-- TODO The real version of this will be present elsewhere.
-- | The diameter of the current chain graph.
--
newtype Diameter = Diameter { diameter :: Refined (Positive && LessThan 10) Int }

-- | Given a peer to connect to, fetch all `BlockHeader`s that exist
-- in the peer's chain but not our local given `ChainDb`, and sync them.
--
sync :: Diameter -> ClientEnv -> BlockHeaderDb -> IO ()
sync d env db = do
    h <- maxHeader db
    let m = minHeight (_blockHeight h) d
    insertStream db $ headers env h m

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

-- | Fetch all `BlockHeader`s from a peer from a given `BlockHeight` and higher.
--
-- INVARIANTS:
--
--   * The `BlockHeader`s are streamed roughly in order of `BlockHeight`, lowest to highest,
--     such that no child is streamed before its direct parent.
--     We assume the server will do this correctly.
--
headers :: ClientEnv -> BlockHeader -> MinRank -> Stream (Of BlockHeader) IO ()
headers env h m = g $ client Nothing
  where
    client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHeader)
    client next = headersClient (_blockChainwebVersion h) (_blockChainId h) Nothing next (Just m) Nothing

    -- | Attempt to run a servant client.
    --
    g :: ClientM (Page (NextItem BlockHash) BlockHeader) -> Stream (Of BlockHeader) IO ()
    g c = lift (runClientM c env) >>= either (lift . throwM) f

    -- | Stream every `BlockHeader` from a `Page`, automatically requesting
    -- the next `Page` if there is one.
    --
    f :: Page (NextItem BlockHash) BlockHeader -> Stream (Of BlockHeader) IO ()
    f page = do
        SP.each $ _pageItems page
        case _pageNext page of
          n@(Just (Inclusive _)) -> g $ client n
          _ -> pure ()
