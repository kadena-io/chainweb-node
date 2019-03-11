{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Proxy
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics (Generic)

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
import qualified Chainweb.Mempool.RestAPI.Server as Mempool
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI
import Chainweb.Payload.RestAPI.Server
import Chainweb.RestAPI.Health
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
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
    <> someBlockHeaderDbApis v (selectChainIds cs)
    <> somePayloadApis v (selectChainIds cs)
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
-- Chainweb Server

someChainwebServer
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => ChainwebVersion
    -> ChainwebServerDbs t cas
    -> SomeServer
someChainwebServer v dbs =
    someSwaggerServer v (fst <$> _chainwebServerPeerDbs dbs)
        <> someHealthCheckServer
        <> maybe mempty (someCutServer v) (_chainwebServerCutDb dbs)
        <> maybe mempty (someSpvServers v) (_chainwebServerCutDb dbs)
        <> somePayloadServers v (_chainwebServerPayloadDbs dbs)
        <> someBlockHeaderDbServers v (_chainwebServerBlockHeaderDbs dbs)
        <> Mempool.someMempoolServers v (_chainwebServerMempools dbs)
        <> someP2pServers v (_chainwebServerPeerDbs dbs)

chainwebApplication
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => ChainwebVersion
    -> ChainwebServerDbs t cas
    -> Application
chainwebApplication v = someServerApplication . someChainwebServer v

serveChainwebOnPort
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => Port
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainwebOnPort p v = run (int p) . chainwebApplication v

serveChainweb
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => Settings
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainweb s v = runSettings s . chainwebApplication v

serveChainwebSocket
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => Settings
    -> Socket
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainwebSocket s sock v = runSettingsSocket s sock . chainwebApplication v

serveChainwebSocketTls
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => Settings
    -> X509CertPem
    -> X509KeyPem
    -> Socket
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainwebSocketTls settings certBytes keyBytes sock v dbs
    = runTLSSocket tlsSettings settings sock app
  where
    tlsSettings = tlsServerSettings certBytes keyBytes
    app = chainwebApplication v dbs

serveChainwebSocketTlsEkg
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => Port
        -- ^ EKG port
    -> Settings
    -> X509CertPem
    -> X509KeyPem
    -> Socket
    -> ChainwebVersion
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainwebSocketTlsEkg ekgPort settings certBytes keyBytes sock v dbs = do
    store <- serverMetricStore <$> forkServer "127.0.0.1" (int ekgPort)
    waiMetrics <- registerWaiMetrics store
    runTLSSocket tlsSettings settings sock $ (metrics waiMetrics) app
  where
    tlsSettings = tlsServerSettings certBytes keyBytes
    app = chainwebApplication v dbs

