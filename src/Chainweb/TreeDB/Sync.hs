{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
  , singleChainSyncSession
  , chainwebSyncSession
    -- * Utils
  , minHeight
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (SomeException, try, throw)
import Control.Lens ((&))
import Control.Monad

import Data.Hashable
import qualified Data.HashSet as HS
import Data.Semigroup (Min(..), Max(..))
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

-- -------------------------------------------------------------------------- --
-- Types

-- | An alias to be used in constraint lists, to enforce that the entry type
-- stored in the given `TreeDb` is a `BlockHeader`.
--
type BlockHeaderTreeDb db = (TreeDb db, DbEntry db ~ BlockHeader)

-- | Some Rank depth in the past, beyond which we wouldn't want to sync.
-- Branches that haven't progressed beyond this point are likely dead already.
--
newtype Depth = Depth { _getDepth :: Natural }
    deriving (Show, Eq, Ord)

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

    -- If the remote site doesn't know about a leave no lower bounds is applied
    -- to the search and, in worst case, all entries down to the root are
    -- returned. We prevent this by including an additional limit at at depth
    -- @d@. With high probability the remote site knows about that entry and
    -- thus the query won't return blocks beyond that point.
    --
    lmax <- maxRank local
    let minr = MinRank . Min $ lmax - min (_getDepth d) lmax
    let maxr = MaxRank . Max $ lmax - min (_getDepth d) lmax
    lLowerLeaves <- streamToHashSet_
        $ branchKeys local Nothing Nothing (Just minr) (Just maxr) mempty (HS.map UpperBound lLeaves)

    logg Debug "get peer leaves"
    rLeaves <- streamToHashSet_ $ leafKeys peer Nothing Nothing Nothing Nothing

    let lower = HS.map LowerBound (lLeaves <> lLowerLeaves)
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

-- | The same function as @syncSession@, but with less logging.
--
syncSession_
    :: BlockHeaderTreeDb local
    => BlockHeaderTreeDb peer
    => Bool
        -- ^ Whether to gossip around newly discovered block headers. For a
        -- multi-chain scenario this is not needed, since new Cuts are gossiped
        -- around in the network.
    -> Int
        -- ^ Delay between full synchronizations in microseconds
    -> local
    -> PeerTree peer
    -> Depth
    -> LogFunction
    -> IO Bool
syncSession_ sendBlocks fullSyncDelay local peer d logg = do
    receiveBlockHeaders
    m <- maxHeader local

    let gossip = S.mapM_ send $ allEntries local (Just $ Exclusive $ key m)
        fullSync = forever $ receiveBlockHeaders >> threadDelay fullSyncDelay
            -- FIXME make this configurable or dynamic
    if
        | sendBlocks -> race_ gossip fullSync
        | otherwise -> fullSync

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

-- | A sync session that does
--
-- * a full sync at the beginning and then every 5 seconds, and
-- * optionally, after the first full sync it forward every newly discovered block
--   header to the peer.
--
syncSession
    :: BlockHeaderTreeDb local
    => BlockHeaderTreeDb peer
    => Bool
        -- ^ Whether to gossip around newly discovered block headers. For a
        -- multi-chain scenario this is not needed, since new Cuts are gossiped
        -- around in the network.
    -> Int
        -- ^ Delay between full synchronizations in microseconds
    -> local
    -> PeerTree peer
    -> Depth
    -> LogFunction
    -> IO Bool
syncSession sendBlocks fullSyncDelay local peer depth logFun =
    try (syncSession_ sendBlocks fullSyncDelay local peer depth logFun) >>= \case
        Left e -> do
            case sshow @SomeException e of
                "<<Timeout>>" -> logg Debug "Session timeout"
                "AsyncCancelled" -> logg Debug "Session cancelled"
                msg -> logg Debug $ "Session failed: " <> msg
            throw e
        Right a -> do
            logg Debug "Session succeeded"
            return a
  where
    logg :: LogFunctionText
    logg = logFun

-- | A sync session that does
--
-- * a full sync at the beginning and then every 5 seconds.
-- * after the first full sync it forward every newly discovered block header
--   to the peer.
--
singleChainSyncSession
    :: BlockHeaderTreeDb local
    => BlockHeaderTreeDb peer
    => local
    -> PeerTree peer
    -> Depth
    -> LogFunction
    -> IO Bool
singleChainSyncSession = syncSession True 5000000 {- 5 seconds -}

-- | A sync session that does
--
-- * a full sync at the beginning and then every minute.
--
chainwebSyncSession
    :: BlockHeaderTreeDb local
    => BlockHeaderTreeDb peer
    => local
    -> PeerTree peer
    -> Depth
    -> LogFunction
    -> IO Bool
chainwebSyncSession = syncSession False 60000000 {- 1 minute -}

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
