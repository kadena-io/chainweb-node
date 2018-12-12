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
( syncSession
, type BlockHeaderTreeDb
) where

import Control.Lens ((&))
import Control.Monad

import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Servant.Client

import Streaming
import qualified Streaming.Prelude as S

import System.IO.Unsafe
import System.LogLevel

-- internal modules

import Chainweb.TreeDB.Sync
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.TreeDB
import Chainweb.TreeDB.RemoteDB (RemoteDb(..))
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.LogMessage

import P2P.Session

-- -------------------------------------------------------------------------- --
-- BlockHeaderDb Utils

type BlockHeaderTreeDb db = (TreeDb db, DbEntry db ~ BlockHeader)

-- -------------------------------------------------------------------------- --
-- Sync

-- | Sync branches which could reasonably have been updated since a given local
-- database was synced.
branchSync :: BlockHeaderTreeDb db => db -> Diameter -> LogFunction -> RemoteDb -> IO ()
branchSync ldb d logFun env = do

    logg Debug "get local leaves"
    lLeaves <- streamToHashSet_ $ leafKeys ldb Nothing Nothing Nothing Nothing

    logg Debug "get remote leaves"
    rLeaves <- streamToHashSet_ $ leafKeys env Nothing Nothing Nothing Nothing

    let lower = HS.map LowerBound lLeaves
        upper = HS.map UpperBound rLeaves

    h <- maxHeader ldb
    let m = minHeight (_blockHeight h) d

    logg Debug "request remote branches limited by local leaves"

    -- We get remote headers in reverse order, so we have to buffer
    -- before we can start to insert in the local db. We could somewhat
    -- better by starting insertion as soon as we got a complete branch,
    -- but that's left as future optimization.
    branchEntries env Nothing Nothing (Just m) Nothing lower upper
        & void
        & reverseStream
        & chunksOf 64 -- TODO: ideally, this would align with response pages
        & mapsM_ (insertStream ldb)

        -- FIXME avoid insertStream because of its problematic behavior on failure.
        -- FIXME add failure handling on a per block header basis

  where
    logg :: LogFunctionText
    logg = logFun

chainDbGenesisBlock :: BlockHeaderTreeDb db => db -> DbEntry db
chainDbGenesisBlock db = unsafePerformIO $ root db
{-# NOINLINE chainDbGenesisBlock #-}

chainDbChainwebVersion :: BlockHeaderTreeDb db => db -> ChainwebVersion
chainDbChainwebVersion = _blockChainwebVersion . chainDbGenesisBlock

chainDbChainId :: BlockHeaderTreeDb db => db -> ChainId
chainDbChainId = _blockChainId . chainDbGenesisBlock

chainClientEnv :: BlockHeaderTreeDb db => db -> ClientEnv -> RemoteDb
chainClientEnv db env = RemoteDb env
    (chainDbChainwebVersion db)
    (chainDbChainId db)

-- -------------------------------------------------------------------------- --
-- Sync Session

syncSession :: TreeDb db => (DbEntry db ~ BlockHeader) => db -> Diameter -> P2pSession
syncSession db d logg env = do
    receiveBlockHeaders
    m <- maxHeader db
    S.mapM_ send $ allEntries db (Just $ Exclusive $ key m)

    -- this code must not be reached
    void $ logg @T.Text Error "unexpectedly exited sync session"
    return False
  where
    cenv :: RemoteDb
    cenv = chainClientEnv db env

    send h = do
        insert cenv h
        logg Debug $ "put block header " <> showHash h

    receiveBlockHeaders = do
        logg @T.Text Info "start full sync"
        branchSync db d logg cenv
        logg @T.Text Debug "finished full sync"

-- -------------------------------------------------------------------------- --
-- Utils

showHash :: Hashable a => a -> T.Text
showHash = T.pack . show . abs . hash
