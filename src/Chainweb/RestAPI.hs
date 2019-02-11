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
-- 'someChainwebServer' and also re-export the module with API client functions.
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
, serveSingleChainSocket

-- * Chainweb API Server
, someChainwebServer
, chainwebApplication
, serveChainwebOnPort
, serveChainweb
, serveChainwebSocket
, serveChainwebSocketTls
, serveChainwebSocketTlsEkg
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

import Network.Socket
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai.Handler.WarpTLS as WARP (runTLSSocket)
import Network.Wai.Metrics

import Servant.API
import Servant.Server
import Servant.Swagger

import System.Remote.Monitoring

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.BlockHeaderDB.RestAPI.Server
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Server
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.Websocket as Mempool
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import Network.X509.SelfSigned

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
-- Single Chain Server

someSingleChainServer
    :: Show t
    => ChainwebVersion
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> SomeServer
someSingleChainServer v chainDbs peerDbs = someSwaggerServer v (fst <$> peerDbs)
    <> someBlockHeaderDbServers v (map (\(x, y, _) -> (x, y)) chainDbs)
    <> Mempool.someMempoolServers v (map (\(x, _, y) -> (x, y)) chainDbs)
    <> someP2pServers v peerDbs

singleChainApplication
    :: Show t
    => ChainwebVersion
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> Application
singleChainApplication v chainDbs peerDbs = someServerApplication
    $ someSingleChainServer v chainDbs peerDbs

serveSingleChainOnPort
    :: Show t
    => Port
    -> ChainwebVersion
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveSingleChainOnPort p v chainDbs peerDbs = run (int p)
    $ singleChainApplication v chainDbs peerDbs

serveSingleChain
    :: Show t
    => Settings
    -> ChainwebVersion
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveSingleChain s v chainDbs peerDbs = runSettings s
    $ singleChainApplication v chainDbs peerDbs

serveSingleChainSocket
    :: Show t
    => Settings
    -> Socket
    -> ChainwebVersion
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveSingleChainSocket s sock v chainDbs peerDbs = runSettingsSocket s sock
    $ singleChainApplication v chainDbs peerDbs

-- -------------------------------------------------------------------------- --
-- Chainweb Server

someChainwebServer
    :: Show t
    => ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> SomeServer
someChainwebServer v cutDb chainDbs peerDbs = someSwaggerServer v (fst <$> peerDbs)
    <> someCutServer v cutDb
    <> someBlockHeaderDbServers v (map (\(x, y, _) -> (x, y)) chainDbs)
    <> Mempool.someMempoolServers v (map (\(x, _, y) -> (x, y)) chainDbs)
    <> someP2pServers v peerDbs

chainwebApplication
    :: Show t
    => ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> Application
chainwebApplication v cutDb chainDbs peerDbs = someServerApplication
    $ someChainwebServer v cutDb chainDbs peerDbs

serveChainwebOnPort
    :: Show t
    => Port
    -> ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveChainwebOnPort p v cutDb chainDbs peerDbs = run (int p)
    $ chainwebApplication v cutDb chainDbs peerDbs

serveChainweb
    :: Show t
    => Settings
    -> ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveChainweb s v cutDb chainDbs peerDbs = runSettings s
    $ chainwebApplication v cutDb chainDbs peerDbs

serveChainwebSocket
    :: Show t
    => Settings
    -> Socket
    -> ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveChainwebSocket s sock v cutDb chainDbs peerDbs = runSettingsSocket s sock
    $ chainwebApplication v cutDb chainDbs peerDbs

serveChainwebSocketTls
    :: Show t
    => Settings
    -> X509CertPem
    -> X509KeyPem
    -> Socket
    -> ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveChainwebSocketTls settings certBytes keyBytes sock v cutDb chainDbs peerDbs
    = runTLSSocket tlsSettings settings sock app
  where
    tlsSettings = tlsServerSettings certBytes keyBytes
    app = chainwebApplication v cutDb chainDbs peerDbs

serveChainwebSocketTlsEkg
    :: Show t
    => Port
        -- ^ EKG port
    -> Settings
    -> X509CertPem
    -> X509KeyPem
    -> Socket
    -> ChainwebVersion
    -> CutDb
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveChainwebSocketTlsEkg ekgPort settings certBytes keyBytes sock v cutDb chainDbs peerDbs = do
    store <- serverMetricStore <$> forkServer "127.0.0.1" (int ekgPort)
    waiMetrics <- registerWaiMetrics store
    runTLSSocket tlsSettings settings sock $ (metrics waiMetrics) app
  where
    tlsSettings = tlsServerSettings certBytes keyBytes
    app = chainwebApplication v cutDb chainDbs peerDbs

