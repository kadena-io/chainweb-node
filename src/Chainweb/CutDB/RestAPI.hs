{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.CutDB.RestAPI
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- REST API for the current 'Cut' of a Chainweb node.
--
module Chainweb.CutDB.RestAPI
(
-- * Cut API
  CutGetApi
, cutGetApi
, CutPutApi
, cutPutApi
, CutApi
, cutApi
, newCutGetServer
, newCutServer

-- * Some Cut API
, someCutApi
-- * Handlers
, cutGetHandler
, cutPutHandler
-- * Request factories
, newCutGetClient
, newCutPutClient
) where

import Control.Lens
import Data.Aeson
import Data.IxSet.Typed
import Data.Proxy
import Data.Semigroup

import qualified Network.HTTP.Client.Internal as Client
import Network.HTTP.Media
import Network.HTTP.Types
import qualified Network.Wai as Wai

import Servant

import Web.DeepRoute hiding (QueryParam)
import Web.DeepRoute.Client
import Web.DeepRoute.Wai

-- internal modules

import Chainweb.BlockHeight
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Utils
import Chainweb.Version
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Orphans ()
import Chainweb.Version.Utils

import P2P.Node.PeerDB
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- @GET /chainweb/<ApiVersion>/<ChainwebVersion>/cut@

type CutGetApi_
    = QueryParam "maxheight" MaxRank
    :> Get '[JSON] CutHashes

type CutGetApi (v :: ChainwebVersionT)
    = 'ChainwebEndpoint v :> 'NetworkEndpoint 'CutNetworkT :> Reassoc CutGetApi_

cutGetApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (CutGetApi v)
cutGetApi = Proxy

-- -------------------------------------------------------------------------- --
-- @PUT /chainweb/<ApiVersion>/<ChainwebVersion>/cut/@

type CutPutApi_
    = ReqBody '[JSON] CutHashes
    :> Verb 'PUT 204 '[JSON] NoContent

type CutPutApi (v :: ChainwebVersionT)
    = 'ChainwebEndpoint v :> 'NetworkEndpoint 'CutNetworkT :> Reassoc CutPutApi_

cutPutApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (CutPutApi v)
cutPutApi = Proxy

-- -------------------------------------------------------------------------- --
-- Cut API

type CutApi v
    = CutGetApi v
    :<|> CutPutApi v

cutApi :: forall (v :: ChainwebVersionT) . Proxy (CutApi v)
cutApi = Proxy

-- -------------------------------------------------------------------------- --
-- Some Cut Api

someCutApi :: ChainwebVersion -> SomeApi
someCutApi (FromSingChainwebVersion (SChainwebVersion :: Sing v)) = SomeApi $ cutApi @v

-- new stuff

cutGetHandler :: CutDb cas -> Maybe MaxRank -> IO CutHashes
cutGetHandler db Nothing = cutToCutHashes Nothing <$> _cut db
cutGetHandler db (Just (MaxRank (Max mar))) = do
    !c <- _cut db
    let v = _chainwebVersion db
    let !bh = BlockHeight $ floor (avgBlockHeightAtCutHeight v (CutHeight $ int mar))
    !c' <- limitCut (view cutDbWebBlockHeaderDb db) bh c
    return $! cutToCutHashes Nothing c'

cutPutHandler :: PeerDb -> CutDb cas -> CutHashes -> IO ()
cutPutHandler pdb db c = case _peerAddr <$> _cutOrigin c of
    Nothing -> errorWithStatus badRequest400 "Cut is missing an origin entry"
    Just addr -> do
        ps <- peerDbSnapshot pdb
        case getOne (getEQ addr ps) of
            Nothing -> errorWithStatus unauthorized401 "Unknown peer"
            Just{} -> addCutHashes db c

cutGetEndpoint :: CutDb cas -> (Method, MediaType, Wai.Application)
cutGetEndpoint cutDb = (methodGet, "application/json",) $ \req resp -> do
    maxheight <- getParams req (queryParamMaybe "maxheight")
    resp . responseJSON status200 [] =<< cutGetHandler cutDb maxheight

newCutGetClient :: (HasRouteRoot e, HasClientEnv e) => e -> MaxRank -> (Maybe CutHashes -> IO r) -> IO r
newCutGetClient e maxheight = doJSONRequest (e ^. clientEnv) $
    withMethod e methodGet &
    requestQuery .~ [("maxheight", Just (toQueryParam maxheight))] &
    requestHeaders .~ [("Accept", "application/json")]

newCutGetServer :: CutDb cas -> Route Wai.Application
newCutGetServer cutDb = terminus' [cutGetEndpoint cutDb]

newCutServer :: PeerDb -> CutDb cas -> Route Wai.Application
newCutServer peerDb cutDb = terminus'
    [ cutGetEndpoint cutDb
    , (methodPut, "application/json",) $ \req resp -> do
        cutPutHandler peerDb cutDb =<< requestFromJSON req
        resp $ Wai.responseLBS noContent204 [] ""
    ]

newCutPutClient :: (HasRouteRoot e, HasClientEnv e) => e -> CutHashes -> IO ()
newCutPutClient e ch = doRequestForEffect (e ^. clientEnv) $
    withMethod e methodPut &
    requestBody .~ Client.RequestBodyLBS (encode ch) &
    requestHeaders .~ [("Content-Type", "application/json")]
