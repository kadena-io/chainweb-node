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

import Control.Lens (view)
import Control.Monad.Except

import Data.IxSet.Typed
import Data.Proxy
import Data.Semigroup

import Network.HTTP.Media
import Network.HTTP.Types
import Network.Wai.Handler.Warp hiding (Port)
import qualified Network.Wai as Wai

import Servant.API
import Servant.Server

import Web.DeepRoute
import Web.DeepRoute.Wai

-- internal modules

import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Handlers

-- -------------------------------------------------------------------------- --
-- Cut API Server

cutServer
    :: forall cas (v :: ChainwebVersionT)
    . PeerDb
    -> CutDbT cas v
    -> Server (CutApi v)
cutServer pdb (CutDbT db) =
    (liftIO . cutGetHandler db)
    :<|> (liftIO . fmap (const NoContent) . cutPutHandler pdb db)

cutGetServer
    :: forall cas (v :: ChainwebVersionT)
    . CutDbT cas v
    -> Server (CutGetApi v)
cutGetServer (CutDbT db) = liftIO . cutGetHandler db

-- -------------------------------------------------------------------------- --
-- Some Cut Server

someCutServerT :: PeerDb -> SomeCutDb cas -> SomeServer
someCutServerT pdb (SomeCutDb (db :: CutDbT cas v)) =
    SomeServer (Proxy @(CutApi v)) (cutServer pdb db)

someCutServer :: ChainwebVersion -> PeerDb -> CutDb cas -> SomeServer
someCutServer v pdb = someCutServerT pdb . someCutDbVal v

someCutGetServerT :: SomeCutDb cas -> SomeServer
someCutGetServerT (SomeCutDb (db :: CutDbT cas v)) =
    SomeServer (Proxy @(CutGetApi v)) (cutGetServer db)

someCutGetServer :: ChainwebVersion -> CutDb cas -> SomeServer
someCutGetServer v = someCutGetServerT . someCutDbVal v

-- -------------------------------------------------------------------------- --
-- Run Server

serveCutOnPort :: Port -> ChainwebVersion -> PeerDb -> CutDb cas -> IO ()
serveCutOnPort p v pdb = run (int p) . someServerApplication . someCutServer v pdb


