{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.RestAPI
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module collects and combines the APIs from all Chainweb components.
--
-- Every component that defines an API should add it to 'someChainwebApi' and
-- 'someCahinwebServer' and also re-export the module with API client functions.
--
module Chainweb.RestAPI
(
-- * Chainweb API
  someChainwebApi
, prettyShowChainwebApi
, apiVersion
, prettyApiVersion

-- * Swagger
, prettyChainwebSwagger
, chainwebSwagger

-- * Chainweb API Server
, someChainwebServer
, chainwebApplication
, serveChainwebOnPort
, serveChainweb
, Port

-- * Chainweb API Client

-- ** ChainDB API Client
, module Chainweb.ChainDB.RestAPI.Client

-- ** P2P API Client
, module P2P.Node.RestAPI.Client
) where

import Control.Lens

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.Wai.Handler.Warp hiding (Port)

import Servant.API
import Servant.Server
import Servant.Swagger

-- internal modules

import Chainweb.ChainDB
import Chainweb.ChainDB.RestAPI
import Chainweb.ChainDB.RestAPI.Client
import Chainweb.ChainDB.RestAPI.Server
import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Node.RestAPI
import P2P.Node.RestAPI.Client
import P2P.Node.RestAPI.Server

-- -------------------------------------------------------------------------- --
-- Chainweb API

someChainwebApi :: ChainwebVersion -> [ChainId] -> SomeApi
someChainwebApi v cs = someSwaggerApi
    <> someChainDbApis v cs
    <> someP2pApis v cs

prettyShowChainwebApi :: ChainwebVersion -> [ChainId] -> T.Text
prettyShowChainwebApi v cs = case someChainwebApi v cs of
    SomeApi a -> layout a

-- -------------------------------------------------------------------------- --
-- Swagger
--
-- Note that with the current approach to constructing the API a single routing
-- table is generated that contains the routes for all chains. If the number of
-- chains is large the corresponding swagger file will be very large as well.
-- We should improve the swagger spec to be more structured.

type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

someSwaggerApi :: SomeApi
someSwaggerApi = SomeApi $ Proxy @SwaggerApi

someSwaggerServer :: ChainwebVersion -> [ChainId] -> SomeServer
someSwaggerServer v cs = SomeServer (Proxy @SwaggerApi)
    $ return (chainwebSwagger v cs)

instance ToSchema Swagger where
    declareNamedSchema _ = return $ NamedSchema (Just "Swagger")
        $ sketchSchema ("swagger specification" :: T.Text)

chainwebSwagger :: ChainwebVersion -> [ChainId] -> Swagger
chainwebSwagger v cs = case someChainwebApi v cs of
    SomeApi a -> toSwagger a
        & info.title   .~ "Chainweb"
        & info.version .~ prettyApiVersion
        & info.description ?~ "Chainweb/" <> sshow v <> " API"

prettyChainwebSwagger :: ChainwebVersion -> [ChainId] -> T.Text
prettyChainwebSwagger v cs = T.decodeUtf8 . BL.toStrict . encodePretty
    $ chainwebSwagger v cs

-- -------------------------------------------------------------------------- --
-- Server

someChainwebServer
    :: ChainwebVersion
    -> [(ChainId, ChainDb)]
    -> [(ChainId, PeerDb)]
    -> SomeServer
someChainwebServer v chainDbs peerDbs = someSwaggerServer v (fst <$> chainDbs)
    <> someChainDbServers v chainDbs
    <> someP2pServers v peerDbs

chainwebApplication
    :: ChainwebVersion
    -> [(ChainId, ChainDb)]
    -> [(ChainId, PeerDb)]
    -> Application
chainwebApplication v chainDbs peerDbs = someServerApplication
    $ someChainwebServer v chainDbs peerDbs

serveChainwebOnPort
    :: Port
    -> ChainwebVersion
    -> [(ChainId, ChainDb)]
    -> [(ChainId, PeerDb)]
    -> IO ()
serveChainwebOnPort p v chainDbs peerDbs = run (int p)
    $ chainwebApplication v chainDbs peerDbs

serveChainweb
    :: Settings
    -> ChainwebVersion
    -> [(ChainId, ChainDb)]
    -> [(ChainId, PeerDb)]
    -> IO ()
serveChainweb s v chainDbs peerDbs = runSettings s
    $ chainwebApplication v chainDbs peerDbs

