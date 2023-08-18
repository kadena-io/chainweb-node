{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.CutDB.RestAPI.Server
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Server implementation of the 'Cut' REST API.
--
module Chainweb.CutDB.RestAPI.Server
(
-- * Handlers
  cutGetHandler
, cutPutHandler

-- * Cut Server
, cutServer
, cutGetServer

-- * Some Cut Server
, someCutServer
, someCutGetServer

-- * Run server
, serveCutOnPort
) where

import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.IO.Class

import Data.IxSet.Typed
import Data.Proxy
import Data.Semigroup

import Network.Wai.Handler.Warp hiding (Port)

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockHeight
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils

import P2P.Node.PeerDB
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Handlers

cutGetHandler :: CutDb tbl -> Maybe MaxRank -> IO CutHashes
cutGetHandler db Nothing = liftIO $ cutToCutHashes Nothing <$> _cut db
cutGetHandler db (Just (MaxRank (Max mar))) = liftIO $ do
    !c <- _cut db
    let v = _chainwebVersion db
    let !bh = BlockHeight $ floor (avgBlockHeightAtCutHeight v (CutHeight $ int mar))
    !c' <- limitCut (view cutDbWebBlockHeaderDb db) bh c
    return $! cutToCutHashes Nothing c'

cutPutHandler :: PeerDb -> CutDb tbl -> CutHashes -> Handler NoContent
cutPutHandler pdb db c = case _peerAddr <$> _cutOrigin c of
    Nothing -> throwError $ setErrText "Cut is missing an origin entry" err400
    Just addr -> do
        ps <- liftIO $ peerDbSnapshot pdb
        case getOne (getEQ addr ps) of
            Nothing -> throwError $ setErrText "Unknown peer" err401
            Just{} -> NoContent <$ liftIO (addCutHashes db c)

-- -------------------------------------------------------------------------- --
-- Cut API Server

cutServer
    :: forall tbl (v :: ChainwebVersionT)
    . PeerDb
    -> CutDbT tbl v
    -> Server (CutApi v)
cutServer pdb (CutDbT db) = liftIO . cutGetHandler db :<|> cutPutHandler pdb db

cutGetServer
    :: forall tbl (v :: ChainwebVersionT)
    . CutDbT tbl v
    -> Server (CutGetApi v)
cutGetServer (CutDbT db) = liftIO . cutGetHandler db

-- -------------------------------------------------------------------------- --
-- Some Cut Server

someCutServerT :: PeerDb -> SomeCutDb tbl -> SomeServer
someCutServerT pdb (SomeCutDb (db :: CutDbT tbl v)) =
    SomeServer (Proxy @(CutApi v)) (cutServer pdb db)

someCutServer :: ChainwebVersion -> PeerDb -> CutDb tbl -> SomeServer
someCutServer v pdb = someCutServerT pdb . someCutDbVal v

someCutGetServerT :: SomeCutDb tbl -> SomeServer
someCutGetServerT (SomeCutDb (db :: CutDbT tbl v)) =
    SomeServer (Proxy @(CutGetApi v)) (cutGetServer db)

someCutGetServer :: ChainwebVersion -> CutDb tbl -> SomeServer
someCutGetServer v = someCutGetServerT . someCutDbVal v

-- -------------------------------------------------------------------------- --
-- Run Server

serveCutOnPort :: Port -> ChainwebVersion -> PeerDb -> CutDb tbl -> IO ()
serveCutOnPort p v pdb = run (int p) . someServerApplication . someCutServer v pdb


