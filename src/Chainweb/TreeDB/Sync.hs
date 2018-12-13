{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.TreeDB.Sync
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Synchronize the contents of two `TreeDb`s which contain `BlockHeader`s. The
-- DBs can be on the same machine or across the network - any two `TreeDb`s will
-- do.
--
module Chainweb.TreeDB.Sync
  (
    -- * Types
    BlockHeaderTreeDb
  , Depth(..)
  , PeerTree(..)
    -- * Sync Algorithms
  , branchSync
  , linearSync
    -- * Sync Session
  , syncSession
  , chainSyncSession
    -- * Utils
  , minHeight
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (SomeException, try)
import Control.Lens ((&))
import Control.Monad

import Data.Hashable
import qualified Data.HashSet as HS
import Data.Semigroup (Min(..))
import qualified Data.Text as T

import Numeric.Natural

import Streaming
import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging

import Data.LogMessage

import P2P.Session

-- -------------------------------------------------------------------------- --
-- Types

-- | An alias to be used in constraint lists, to enforce that the entry type
-- stored in the given `TreeDb` is a `BlockHeader`.
--
type BlockHeaderTreeDb db = (TreeDb db, DbEntry db ~ BlockHeader)

-- | Some Rank depth in the past, beyond which we wouldn't want to sync.
-- Branches that haven't progressed beyond this point are likely dead already.
--
newtype Depth = Depth Natural

-- | A wrapper for things which have `TreeDb` instances.
--
newtype PeerTree t = PeerTree { _peerTree :: t } deriving newtype (TreeDb)

-- -------------------------------------------------------------------------- --
-- Sync

-- | Sync branches which could reasonably have been updated since a given local
-- database was synced.
--
branchSync
    :: BlockHeaderTreeDb local
    => BlockHeaderTreeDb peer
    => local
    -> PeerTree peer
    -> Depth
    -> LogFunction
    -> IO ()
branchSync local peer d logFun = do

    logg Debug "get local leaves"
    lLeaves <- streamToHashSet_ $ leafKeys local Nothing Nothing Nothing Nothing

    logg Debug "get peer leaves"
    rLeaves <- streamToHashSet_ $ leafKeys peer Nothing Nothing Nothing Nothing

    let lower = HS.map LowerBound lLeaves
        upper = HS.map UpperBound rLeaves

    h <- maxHeader local
    let m = minHeight (_blockHeight h) d

    logg Debug "request peer branches limited by local leaves"

    -- We get remote headers in reverse order, so we have to buffer
    -- before we can start to insert in the local db. We could somewhat
    -- better by starting insertion as soon as we got a complete branch,
    -- but that's left as future optimization.
    branchEntries peer Nothing Nothing (Just m) Nothing lower upper
        & void
        & reverseStream
        & chunksOf 64 -- TODO: ideally, this would align with response pages
        & mapsM_ (insertStream local)

        -- FIXME avoid insertStream because of its problematic behavior on failure.
        -- FIXME add failure handling on a per block header basis

  where
    logg :: LogFunctionText
    logg = logFun

-- | Sync all blocks on all branches, starting from a point close in the past to
-- where our local DB currently sits.
--
linearSync
    :: BlockHeaderTreeDb local
    => BlockHeaderTreeDb peer
    => Depth
    -> local
    -> PeerTree peer
    -> IO ()
linearSync d local peer = do
    h <- maxHeader local
    let m = minHeight (_blockHeight h) d
    void . insertStream local $ entries peer Nothing Nothing (Just m) Nothing

-- -------------------------------------------------------------------------- --
-- Sync Session

-- | Form a living sync process between two `TreeDb` instances. Once our local
-- Db syncs to the peer, the local will send back all the new `BlockHeader`s
-- that it finds.
--
syncSession
    :: BlockHeaderTreeDb local
    => BlockHeaderTreeDb peer
    => local
    -> PeerTree peer
    -> Depth
    -> LogFunction
    -> IO Bool
syncSession local peer d logg = do
    receiveBlockHeaders
    m <- maxHeader local
    race_
        (S.mapM_ send $ allEntries local (Just $ Exclusive $ key m))
        (forever $ receiveBlockHeaders >> threadDelay 5000000)
            -- FIXME make this configurable or dynamic

    -- this code must not be reached
    void $ logg @T.Text Error "unexpectedly exited sync session"
    return False
  where
    send h = do
        insert peer h
        logg Debug $ "put block header " <> showHash h

    receiveBlockHeaders = do
        logg @T.Text Info "start full sync"
        branchSync local peer d logg
        logg @T.Text Debug "finished full sync"

-- | Adds a little bit more logging to syncSession
--
chainSyncSession
    :: BlockHeaderTreeDb local
    => BlockHeaderTreeDb peer
    => local
    -> PeerTree peer
    -> Depth
    -> LogFunction
    -> IO Bool
chainSyncSession local peer depth logFun =
    try (syncSession local peer depth logFun) >>= \case
        Left e -> do
            logg Warn $ "Session failed: " <> sshow @SomeException e
            return False
        Right a -> do
            logg Warn "Session succeeded"
            return a
  where
    logg :: LogFunctionText
    logg = logFun

-- -------------------------------------------------------------------------- --
-- Utils

showHash :: Hashable a => a -> T.Text
showHash = T.pack . show . abs . hash

-- | Given a `BlockHeight` that represents the highest rank of some `TreeDb`,
-- find the lowest entry rank such that it's at most only
-- (diameter * 2) in height away.
--
minHeight :: BlockHeight -> Depth -> MinRank
minHeight h (Depth d) = MinRank $ Min m
  where
    m :: Natural
    m = fromIntegral (max (high - low) 0)

    -- | Using `Integer` prevents underflow errors.
    --
    high :: Integer
    high = fromIntegral h

    low :: Integer
    low = fromIntegral $ 2 * d
