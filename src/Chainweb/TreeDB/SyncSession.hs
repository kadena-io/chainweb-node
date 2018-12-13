{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.TreeDB.SyncSession
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.TreeDB.SyncSession
  (
    -- * Types
    type BlockHeaderTreeDb
    -- * Sync Algorithms
  , branchSync
    -- * Sync Session
  , syncSession
  ) where

import Control.Lens ((&))
import Control.Monad

import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Streaming
import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.TreeDB.Sync
import Chainweb.BlockHeader
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging

import Data.LogMessage

import P2P.Session

-- -------------------------------------------------------------------------- --
-- BlockHeaderDb Utils

-- | An alias to be used in constraint lists, to enforce that the entry type
-- stored in the given `TreeDb` is a `BlockHeader`.
--
type BlockHeaderTreeDb db = (TreeDb db, DbEntry db ~ BlockHeader)

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
    S.mapM_ send $ allEntries local (Just $ Exclusive $ key m)

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

-- -------------------------------------------------------------------------- --
-- Utils

showHash :: Hashable a => a -> T.Text
showHash = T.pack . show . abs . hash
