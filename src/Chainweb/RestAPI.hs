{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#ifndef CURRENT_PACKAGE_VERSION
#define CURRENT_PACKAGE_VERSION "UNKNOWN"
#endif

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
-- 'someChainwebServer' and also re-export the module with API client functions.
--
module Chainweb.RestAPI
(
  ChainwebServerDbs(..)
, emptyChainwebServerDbs

-- * Chainweb API
, someChainwebApi
, prettyShowChainwebApi
, apiVersion
, prettyApiVersion

-- * Swagger
, prettyChainwebSwagger
, chainwebSwagger

-- * Component Triggers
, HeaderStream(..)
, Rosetta(..)

-- * Chainweb API Server
, someChainwebServer
, chainwebApplication
, serveChainwebOnPort
, serveChainweb
, serveChainwebSocket
, serveChainwebSocketTls
, Port

-- * Local API Server
, someLocalApiServer
, localApiApplication
, serveLocalApiSocket

-- * Chainweb API Client

-- ** BlockHeaderDb API Client
, module Chainweb.BlockHeaderDB.RestAPI.Client

-- ** P2P API Client
, module P2P.Node.RestAPI.Client
) where

import Control.Lens

import Data.Aeson.Encode.Pretty
import Data.Bifunctor
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Proxy
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics (Generic)

import Network.Socket
import Network.Wai (Middleware, mapResponseHeaders)
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLSSocket)
import Network.Wai.Middleware.Cors

import Servant.API
import Servant.Server
import Servant.Swagger

import System.Clock

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.BlockHeaderDB.RestAPI.Server
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.MinerResources (MiningCoordination)
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI
import Chainweb.CutDB.RestAPI.Server
import Chainweb.HostAddress
import Chainweb.Logger (Logger)
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.RestAPI.Server as Mempool
import Chainweb.Miner.RestAPI (someMiningApi)
import qualified Chainweb.Miner.RestAPI.Server as Mining
import qualified Chainweb.Pact.RestAPI as PactAPI
import qualified Chainweb.Pact.RestAPI.Server as PactAPI
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI
import Chainweb.Payload.RestAPI.Server
import Chainweb.RestAPI.Health
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.NodeInfo
import Chainweb.RestAPI.Utils
import Chainweb.Rosetta.RestAPI.Server
import Chainweb.SPV.RestAPI.Server
import Chainweb.Utils
import Chainweb.Version

import Network.X509.SelfSigned

import P2P.Node.PeerDB
import P2P.Node.RestAPI
import P2P.Node.RestAPI.Client
import P2P.Node.RestAPI.Server

-- -------------------------------------------------------------------------- --
-- Chainweb Server Storage Backends

-- | Datatype for collectively passing all storage backends to
-- functions that run a chainweb server.
--
data ChainwebServerDbs t cas = ChainwebServerDbs
    { _chainwebServerCutDb :: !(Maybe (CutDb cas))
    , _chainwebServerBlockHeaderDbs :: ![(ChainId, BlockHeaderDb)]
    , _chainwebServerMempools :: ![(ChainId, MempoolBackend t)]
    , _chainwebServerPayloadDbs :: ![(ChainId, PayloadDb cas)]
    , _chainwebServerPeerDbs :: ![(NetworkId, PeerDb)]
    }
    deriving (Generic)

emptyChainwebServerDbs :: ChainwebServerDbs t cas
emptyChainwebServerDbs = ChainwebServerDbs
    { _chainwebServerCutDb = Nothing
    , _chainwebServerBlockHeaderDbs = []
    , _chainwebServerMempools = []
    , _chainwebServerPayloadDbs = []
    , _chainwebServerPeerDbs = []
    }

-- -------------------------------------------------------------------------- --
-- Chainweb API

someChainwebApi :: ChainwebVersion -> [NetworkId] -> SomeApi
someChainwebApi v cs = someSwaggerApi
    <> someHealthCheckApi
    <> someNodeInfoApi
    <> someCutApi v
    <> someBlockHeaderDbApis v chains
    <> somePayloadApis v chains
    <> someP2pApis v cs
    <> PactAPI.somePactServiceApis v chains
    <> someMiningApi v
    <> someHeaderStreamApi v
  where
    chains = selectChainIds cs

selectChainIds :: [NetworkId] -> [ChainId]
selectChainIds = mapMaybe f
  where
    f (ChainNetwork c) = Just c
    f (MempoolNetwork c) = Just c
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
-- Component Triggers

newtype Rosetta = Rosetta Bool

newtype HeaderStream = HeaderStream Bool

-- -------------------------------------------------------------------------- --
-- Middlewares

-- Simple cors with actually simpleHeaders which includes content-type.
chainwebCors :: Middleware
chainwebCors = cors . const . Just $ simpleCorsResourcePolicy
    { corsRequestHeaders = simpleHeaders
    }

chainwebTime :: Middleware
chainwebTime app req resp = app req $ \res -> do
    timestamp <- sec <$> getTime Realtime
    resp $ mapResponseHeaders
        ((:) ("X-Server-Timestamp", sshow timestamp))
        res

chainwebNodeVersion :: Middleware
chainwebNodeVersion app req resp = app req $ \res ->
    resp $ mapResponseHeaders
        ((:) chainwebNodeVersionHeader)
        res

-- -------------------------------------------------------------------------- --
-- Chainweb Server

someChainwebServer
    :: Show t
    => PayloadCasLookup cas
    => ChainwebVersion
    -> ChainwebServerDbs t cas
    -> SomeServer
someChainwebServer v dbs =
    someSwaggerServer v (fst <$> peers)
        <> someHealthCheckServer
        <> maybe mempty (someNodeInfoServer v) cuts
        <> maybe mempty (someCutServer v cutPeerDb) cuts
        <> maybe mempty (someSpvServers v) cuts
        <> somePayloadServers v payloads
        <> someBlockHeaderDbServers v blocks
        <> Mempool.someMempoolServers v mempools
        <> someP2pServers v peers
  where
    payloads = _chainwebServerPayloadDbs dbs
    blocks = _chainwebServerBlockHeaderDbs dbs
    cuts = _chainwebServerCutDb dbs
    peers = _chainwebServerPeerDbs dbs
    mempools = _chainwebServerMempools dbs
    cutPeerDb = fromJuste $ lookup CutNetwork peers

chainwebApplication
    :: Show t
    => PayloadCasLookup cas
    => ChainwebVersion
    -> ChainwebServerDbs t cas
    -> Application
chainwebApplication v dbs
    = chainwebTime
    . chainwebNodeVersion
    . chainwebCors
    . someServerApplication
    $ someChainwebServer v dbs

serveChainwebOnPort
    :: Show t
    => PayloadCasLookup cas
    => Port
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainwebOnPort p v dbs = run (int p) $ chainwebApplication v dbs

serveChainweb
    :: Show t
    => PayloadCasLookup cas
    => Settings
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainweb s v dbs = runSettings s $ chainwebApplication v dbs

serveChainwebSocket
    :: Show t
    => PayloadCasLookup cas
    => Settings
    -> Socket
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainwebSocket s sock v dbs =
    runSettingsSocket s sock $ chainwebApplication v dbs

serveChainwebSocketTls
    :: Show t
    => PayloadCasLookup cas
    => Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> Middleware
    -> IO ()
serveChainwebSocketTls settings certChain key sock v dbs m =
    runTLSSocket tlsSettings settings sock $ m app
  where
    tlsSettings :: TLSSettings
    tlsSettings = tlsServerChainSettings certChain key

    app :: Application
    app = chainwebApplication v dbs

-- -------------------------------------------------------------------------- --
-- Local API Server

someLocalApiServer
    :: Show t
    => PayloadCasLookup cas
    => Logger logger
    => ChainwebVersion
    -> ChainwebServerDbs t cas
    -> [(ChainId, PactAPI.PactServerData logger cas)]
    -> Maybe (MiningCoordination logger cas)
    -> HeaderStream
    -> Rosetta
    -> SomeServer
someLocalApiServer v dbs pacts mr (HeaderStream hs) (Rosetta r) =
    someSwaggerServer v (fst <$> peers)
        <> someHealthCheckServer
        <> maybe mempty (someNodeInfoServer v) cuts
        <> PactAPI.somePactServers v pacts
        <> maybe mempty (Mining.someMiningServer v) mr
        <> maybe mempty (someHeaderStreamServer v) (bool Nothing cuts hs)
        <> maybe mempty (bool mempty (someRosettaServer v payloads concreteMs cutPeerDb concreteCr) r) cuts
            -- TODO: not sure if passing the correct PeerDb here
            -- TODO: simplify number of resources passing to rosetta
  where
    cuts = _chainwebServerCutDb dbs
    peers = _chainwebServerPeerDbs dbs
    concreteMs = map (second (_chainResMempool . snd)) pacts
    concreteCr = map (second snd) pacts
    cutPeerDb = fromJuste $ lookup CutNetwork peers
    payloads = _chainwebServerPayloadDbs dbs

localApiApplication
    :: Show t
    => PayloadCasLookup cas
    => Logger logger
    => ChainwebVersion
    -> ChainwebServerDbs t cas
    -> [(ChainId, PactAPI.PactServerData logger cas)]
    -> Maybe (MiningCoordination logger cas)
    -> HeaderStream
    -> Rosetta
    -> Application
localApiApplication v dbs pacts mr hs r
    = chainwebTime
    . chainwebNodeVersion
    . chainwebCors
    . someServerApplication
    $ someLocalApiServer v dbs pacts mr hs r

serveLocalApiSocket
    :: Show t
    => PayloadCasLookup cas
    => Logger logger
    => Settings
    -> Socket
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> [(ChainId, PactAPI.PactServerData logger cas)]
    -> Maybe (MiningCoordination logger cas)
    -> HeaderStream
    -> Rosetta
    -> Middleware
    -> IO ()
serveLocalApiSocket s sock v dbs pacts mr hs r m =
    runSettingsSocket s sock $ m $ localApiApplication v dbs pacts mr hs r

