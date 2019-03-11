{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
-- TODO
--
module Chainweb.CutDB.RestAPI.Server
(
-- * Handlers
  cutGetHandler
, cutPutHandler

-- * Cut Server
, cutServer

-- * Some Cut Server
, someCutServer

-- * Run server
, serveCutOnPort
) where

import Control.Monad.Except

import Data.Proxy

import Network.Wai.Handler.Warp hiding (Port)

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Handlers

-- | FIXME: include own peer info
--
cutGetHandler
    :: CutDb cas
    -> Handler CutHashes
cutGetHandler db = liftIO $ cutToCutHashes Nothing <$> _cut db

cutPutHandler
    :: CutDb cas
    -> CutHashes
    -> Handler NoContent
cutPutHandler db c = NoContent <$ liftIO (addCutHashes db c)

-- -------------------------------------------------------------------------- --
-- Cut API Server

cutServer
    :: forall cas (v :: ChainwebVersionT)
    . CutDbT cas v
    -> Server (CutApi v)
cutServer (CutDbT db) = cutGetHandler db :<|> cutPutHandler db

-- -------------------------------------------------------------------------- --
-- Some Cut Server

someCutServerT :: SomeCutDb cas -> SomeServer
someCutServerT (SomeCutDb (db :: CutDbT cas v)) =
    SomeServer (Proxy @(CutApi v)) (cutServer db)

someCutServer :: ChainwebVersion -> CutDb cas -> SomeServer
someCutServer v = someCutServerT . someCutDbVal v

-- -------------------------------------------------------------------------- --
-- Run Server

serveCutOnPort
    :: Port
    -> ChainwebVersion
    -> CutDb cas
    -> IO ()
serveCutOnPort p v = run (int p) . someServerApplication . someCutServer v

