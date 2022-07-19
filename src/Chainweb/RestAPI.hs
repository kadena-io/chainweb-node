{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
-- * Utils
  serveSocketTls

-- * Chainweb Server DBs
, ChainwebServerDbs(..)
, emptyChainwebServerDbs

-- * Component Triggers
, HeaderStream(..)
, Rosetta(..)

-- * Chainweb P2P API Server
, someChainwebServer
, chainwebApplication
, serveChainwebOnPort
, serveChainweb
, serveChainwebSocket
, serveChainwebSocketTls

-- ** Only serve a Peer DB from the Chainweb P2P API
, servePeerDbSocketTls

-- * Service API Server
, someServiceApiServer
, serviceApiApplication
, serveServiceApiSocket

-- * Chainweb API Client

-- ** BlockHeaderDb API Client
, module Chainweb.BlockHeaderDB.RestAPI.Client

-- ** P2P API Client
, module P2P.Node.RestAPI.Client
) where

import Control.Monad (guard)

import Data.Bifunctor
import Data.Bool (bool)

import GHC.Generics (Generic)

import Network.Socket
import qualified Network.TLS.SessionManager as TLS
import Network.Wai (Middleware, mapResponseHeaders, remoteHost)
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai.Handler.WarpTLS (TLSSettings(..), runTLSSocket)
import Network.Wai.Middleware.Cors

import Servant.Server

import System.Clock

-- internal modules

import Chainweb.Backup
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.BlockHeaderDB.RestAPI.Server
import Chainweb.ChainId
import Chainweb.Chainweb.Configuration
import Chainweb.Chainweb.MinerResources (MiningCoordination)
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Server
import Chainweb.HostAddress
import Chainweb.Logger (Logger)
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.RestAPI.Server as Mempool
import qualified Chainweb.Miner.RestAPI.Server as Mining
import qualified Chainweb.Pact.RestAPI.Server as PactAPI
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI
import Chainweb.Payload.RestAPI.Server
import Chainweb.RestAPI.Backup
import Chainweb.RestAPI.Config
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
import P2P.Node.RestAPI.Client
import P2P.Node.RestAPI.Server

-- -------------------------------------------------------------------------- --
-- Utils

enableTlsSessionCache :: Bool
enableTlsSessionCache = True

-- | TLS HTTP Server
--
serveSocketTls
    :: Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> Application
    -> IO ()
serveSocketTls settings certChain key = runTLSSocket tlsSettings settings
  where
    tlsSettings :: TLSSettings
    tlsSettings = (tlsServerChainSettings certChain key)
        { tlsSessionManagerConfig = sessionManagerConfig <$ guard enableTlsSessionCache }

    sessionManagerConfig = TLS.defaultConfig
        { TLS.ticketLifetime = 3600 -- 1h
        , TLS.pruningDelay = 600 -- 10min
        , TLS.dbMaxSize = 1000
        }

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
        ((serverTimestampHeaderName, sshow timestamp) :)
        res

chainwebNodeVersion :: Middleware
chainwebNodeVersion app req resp = app req $ \res ->
    resp $ mapResponseHeaders (chainwebNodeVersionHeader :) res

chainwebPeerAddr :: Middleware
chainwebPeerAddr app req resp = app req $ \res ->
    resp $ mapResponseHeaders
        ((peerAddrHeaderName, sshow (remoteHost req)) :)
        res

chainwebP2pMiddlewares :: Middleware
chainwebP2pMiddlewares
    = chainwebTime
    . chainwebPeerAddr
    . chainwebNodeVersion
    . chainwebCors

chainwebServiceMiddlewares :: Middleware
chainwebServiceMiddlewares
    = chainwebTime
    . chainwebNodeVersion
    . chainwebCors

-- -------------------------------------------------------------------------- --
-- Chainweb Peer Server

someChainwebServer
    :: Show t
    => PayloadCasLookup cas
    => ChainwebConfiguration
    -> ChainwebServerDbs t cas
    -> SomeServer
someChainwebServer config dbs =
    maybe mempty (someCutServer v cutPeerDb) cuts
    <> maybe mempty (someSpvServers v) cuts
    <> somePayloadServers v p2pPayloadBatchLimit payloads
    <> someBlockHeaderDbServers v blocks
    <> Mempool.someMempoolServers v mempools
    <> someP2pServers v peers
    <> someGetConfigServer config
  where
    payloads = _chainwebServerPayloadDbs dbs
    blocks = _chainwebServerBlockHeaderDbs dbs
    cuts = _chainwebServerCutDb dbs
    peers = _chainwebServerPeerDbs dbs
    mempools = _chainwebServerMempools dbs
    cutPeerDb = fromJuste $ lookup CutNetwork peers
    v = _configChainwebVersion config

-- -------------------------------------------------------------------------- --
-- Chainweb P2P API Application

chainwebApplication
    :: Show t
    => PayloadCasLookup cas
    => ChainwebConfiguration
    -> ChainwebServerDbs t cas
    -> Application
chainwebApplication config dbs
    = chainwebP2pMiddlewares
    . someServerApplication
    $ someChainwebServer config dbs

serveChainwebOnPort
    :: Show t
    => PayloadCasLookup cas
    => Port
    -> ChainwebConfiguration
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainwebOnPort p c dbs = run (int p) $ chainwebApplication c dbs

serveChainweb
    :: Show t
    => PayloadCasLookup cas
    => Settings
    -> ChainwebConfiguration
    -> ChainwebServerDbs t cas
    -> IO ()
serveChainweb s c dbs = runSettings s $ chainwebApplication c dbs

serveChainwebSocket
    :: Show t
    => PayloadCasLookup cas
    => Settings
    -> Socket
    -> ChainwebConfiguration
    -> ChainwebServerDbs t cas
    -> Middleware
    -> IO ()
serveChainwebSocket settings sock c dbs m =
    runSettingsSocket settings sock $ m $ chainwebApplication c dbs

serveChainwebSocketTls
    :: Show t
    => PayloadCasLookup cas
    => Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> ChainwebConfiguration
    -> ChainwebServerDbs t cas
    -> Middleware
    -> IO ()
serveChainwebSocketTls settings certChain key sock c dbs m =
    serveSocketTls settings certChain key sock $ m
        $ chainwebApplication c dbs

-- -------------------------------------------------------------------------- --
-- Run Chainweb P2P Server that serves a single PeerDb

servePeerDbSocketTls
    :: Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> ChainwebVersion
    -> NetworkId
    -> PeerDb
    -> Middleware
    -> IO ()
servePeerDbSocketTls settings certChain key sock v nid pdb m =
    serveSocketTls settings certChain key sock $ m
        $ chainwebP2pMiddlewares
        $ someServerApplication
        $ someP2pServer
        $ somePeerDbVal v nid pdb

-- -------------------------------------------------------------------------- --
-- Chainweb Service API Application

someServiceApiServer
    :: Show t
    => PayloadCasLookup cas
    => Logger logger
    => ChainwebVersion
    -> ChainwebServerDbs t cas
    -> [(ChainId, PactAPI.PactServerData logger cas)]
    -> Maybe (MiningCoordination logger cas)
    -> HeaderStream
    -> Rosetta
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> SomeServer
someServiceApiServer v dbs pacts mr (HeaderStream hs) (Rosetta r) backupEnv pbl =
    someHealthCheckServer
    <> maybe mempty (someBackupServer v) backupEnv
    <> maybe mempty (someNodeInfoServer v) cuts
    <> PactAPI.somePactServers v pacts
    <> maybe mempty (Mining.someMiningServer v) mr
    <> maybe mempty (someHeaderStreamServer v) (bool Nothing cuts hs)
    <> maybe mempty (bool mempty (someRosettaServer v payloads concreteMs cutPeerDb concretePacts) r) cuts
        -- TODO: not sure if passing the correct PeerDb here
        -- TODO: why does Rosetta need a peer db at all?
        -- TODO: simplify number of resources passing to rosetta

    -- GET Cut, Payload, and Headers endpoints
    <> maybe mempty (someCutGetServer v) cuts
    <> somePayloadServers v pbl payloads
    <> someBlockHeaderDbServers v blocks
  where
    cuts = _chainwebServerCutDb dbs
    peers = _chainwebServerPeerDbs dbs
    concreteMs = second PactAPI._pactServerDataMempool <$> pacts
    concretePacts = second PactAPI._pactServerDataPact <$> pacts
    cutPeerDb = fromJuste $ lookup CutNetwork peers
    payloads = _chainwebServerPayloadDbs dbs
    blocks = _chainwebServerBlockHeaderDbs dbs

serviceApiApplication
    :: Show t
    => PayloadCasLookup cas
    => Logger logger
    => ChainwebVersion
    -> ChainwebServerDbs t cas
    -> [(ChainId, PactAPI.PactServerData logger cas)]
    -> Maybe (MiningCoordination logger cas)
    -> HeaderStream
    -> Rosetta
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> Application
serviceApiApplication v dbs pacts mr hs r be pbl
    = chainwebServiceMiddlewares
    . someServerApplication
    $ someServiceApiServer v dbs pacts mr hs r be pbl

serveServiceApiSocket
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
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> Middleware
    -> IO ()
serveServiceApiSocket s sock v dbs pacts mr hs r be pbl m =
    runSettingsSocket s sock $ m $ serviceApiApplication v dbs pacts mr hs r be pbl
