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

-- * Single Chain API Server
, someSingleChainServer
, singleChainApplication
, serveSingleChainOnPort
, serveSingleChain

-- * Chainweb API Server
, someChainwebServer
, chainwebApplication
, serveChainwebOnPort
, serveChainweb
, Port

-- * Chainweb API Client

-- ** BlockHeaderDb API Client
, module Chainweb.BlockHeaderDB.RestAPI.Client

-- ** P2P API Client
, module P2P.Node.RestAPI.Client
) where

import Control.Lens

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Proxy
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.Wai.Handler.Warp hiding (Port)

import Servant.API
import Servant.Server
import Servant.Swagger

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.BlockHeaderDB.RestAPI.Server
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Server
import Chainweb.HostAddress
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Node.RestAPI
import P2P.Node.RestAPI.Client
import P2P.Node.RestAPI.Server

-- -------------------------------------------------------------------------- --
-- Chainweb API

someChainwebApi :: ChainwebVersion -> [NetworkId] -> SomeApi
someChainwebApi v cs = someSwaggerApi
    <> someBlockHeaderDbApis v (selectChainIds cs)
    <> someP2pApis v cs

selectChainIds :: [NetworkId] -> [ChainId]
selectChainIds = mapMaybe f
  where
    f (ChainNetwork c) = Just c
    f CutNetwork = Nothing

prettyShowChainwebApi :: ChainwebVersion -> [NetworkId] -> T.Text
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

someSwaggerServer :: ChainwebVersion -> [NetworkId] -> SomeServer
someSwaggerServer v cs = SomeServer (Proxy @SwaggerApi)
    $ return (chainwebSwagger v cs)

chainwebSwagger :: ChainwebVersion -> [NetworkId] -> Swagger
chainwebSwagger v cs = case someChainwebApi v cs of
    SomeApi a -> toSwagger a
        & info.title   .~ "Chainweb"
        & info.version .~ prettyApiVersion
        & info.description ?~ "Chainweb/" <> sshow v <> " API"

prettyChainwebSwagger :: ChainwebVersion -> [NetworkId] -> T.Text
prettyChainwebSwagger v cs = T.decodeUtf8 . BL.toStrict . encodePretty
    $ chainwebSwagger v cs

-- -------------------------------------------------------------------------- --
-- Singel Chain Server

someSingleChainServer
    :: ChainwebVersion
    -> [(ChainId, BlockHeaderDb)]
    -> [(NetworkId, PeerDb)]
    -> SomeServer
someSingleChainServer v chainDbs peerDbs = someSwaggerServer v (fst <$> peerDbs)
    <> someBlockHeaderDbServers v chainDbs
    <> someP2pServers v peerDbs

singleChainApplication
    :: ChainwebVersion
    -> [(ChainId, BlockHeaderDb)]
    -> [(NetworkId, PeerDb)]
    -> Application
singleChainApplication v chainDbs peerDbs = someServerApplication
    $ someSingleChainServer v chainDbs peerDbs

serveSingleChainOnPort
    :: Port
    -> ChainwebVersion
    -> [(ChainId, BlockHeaderDb)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveSingleChainOnPort p v chainDbs peerDbs = run (int p)
    $ singleChainApplication v chainDbs peerDbs

serveSingleChain
    :: Settings
    -> ChainwebVersion
    -> [(ChainId, BlockHeaderDb)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveSingleChain s v chainDbs peerDbs = runSettings s
    $ singleChainApplication v chainDbs peerDbs

-- -------------------------------------------------------------------------- --
-- Chainweb Server

someChainwebServer
    :: ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb)]
    -> [(NetworkId, PeerDb)]
    -> SomeServer
someChainwebServer v cutDb chainDbs peerDbs = someSwaggerServer v (fst <$> peerDbs)
    <> someCutServer v cutDb
    <> someBlockHeaderDbServers v chainDbs
    <> someP2pServers v peerDbs

chainwebApplication
    :: ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb)]
    -> [(NetworkId, PeerDb)]
    -> Application
chainwebApplication v cutDb chainDbs peerDbs = someServerApplication
    $ someChainwebServer v cutDb chainDbs peerDbs

serveChainwebOnPort
    :: Port
    -> ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveChainwebOnPort p v cutDb chainDbs peerDbs = run (int p)
    $ chainwebApplication v cutDb chainDbs peerDbs

serveChainweb
    :: Settings
    -> ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveChainweb s v cutDb chainDbs peerDbs = runSettings s
    $ chainwebApplication v cutDb chainDbs peerDbs
