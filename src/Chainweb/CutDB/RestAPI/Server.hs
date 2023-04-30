{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

-- * Cut Server
  cutServer
, cutGetServer

-- * Some Cut Server
, someCutServer
, someCutGetServer

-- * Run server
, serveCutOnPort
) where

import Control.Monad.Except

import Data.Proxy

import Network.Wai.Handler.Warp hiding (Port)

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.CutDB
import Chainweb.CutDB.RestAPI
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.PeerDB

-- -------------------------------------------------------------------------- --
-- Handlers

-- -------------------------------------------------------------------------- --
-- Cut API Server

cutServer
    :: forall tbl (v :: ChainwebVersionT)
    . PeerDb
    -> CutDbT tbl v
    -> Server (CutApi v)
cutServer pdb (CutDbT db) =
    (liftIO . cutGetHandler db)
    :<|> (liftIO . fmap (const NoContent) . cutPutHandler pdb db)

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


