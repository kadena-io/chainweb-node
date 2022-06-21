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
-- * Handlers
  cutGetHandler
, cutPutHandler

-- * Cut Server
, cutServer
, cutGetServer
, newCutGetServer
, newCutServer

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

cutGetHandler :: CutDb cas -> Maybe MaxRank -> IO CutHashes
cutGetHandler db Nothing = cutToCutHashes Nothing <$> _cut db
cutGetHandler db (Just (MaxRank (Max mar))) = do
    !c <- _cut db
    !c' <- limitCut (view cutDbWebBlockHeaderDb db) (int mar) c
    return $! cutToCutHashes Nothing c'

cutPutHandler :: PeerDb -> CutDb cas -> CutHashes -> IO ()
cutPutHandler pdb db c = case _peerAddr <$> _cutOrigin c of
    Nothing -> errorWithStatus badRequest400 "Cut is missing an origin entry"
    Just addr -> do
        ps <- peerDbSnapshot pdb
        case getOne (getEQ addr ps) of
            Nothing -> errorWithStatus unauthorized401 "Unknown peer"
            Just{} -> addCutHashes db c

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

cutGetEndpoint :: CutDb cas -> (Method, MediaType, Wai.Application)
cutGetEndpoint cutDb = (methodGet, "application/json",) $ \req resp -> do
    maxheight <- getParams req (queryParamMaybe "maxheight")
    resp . responseJSON status200 [] =<< cutGetHandler cutDb maxheight

newCutGetServer :: CutDb cas -> Route Wai.Application
newCutGetServer cutDb = terminus' [cutGetEndpoint cutDb]

newCutServer :: PeerDb -> CutDb cas -> Route Wai.Application
newCutServer peerDb cutDb = terminus'
    [ cutGetEndpoint cutDb
    , (methodPut, "text/plain;charset=utf-8",) $ \req resp -> do
        cutPutHandler peerDb cutDb =<< requestFromJSON req
        resp $ Wai.responseLBS noContent204 [] ""
    ]

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


