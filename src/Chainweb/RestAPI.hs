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

-- * Chainweb P2P API Server
, someChainwebServer
, chainwebApplication
, chainwebApplicationWithHashesAndSpvApi
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
) where

import Control.Monad (guard)

import Data.Bool (bool)
import Data.Foldable

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
import Chainweb.BlockHeaderDB.RestAPI.Server
import Chainweb.ChainId
import Chainweb.Chainweb.Configuration
import Chainweb.Miner.Coordinator (MiningCoordination)
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Server
import Chainweb.HostAddress
import Chainweb.Logger (Logger)
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.RestAPI.Server as Mempool
import qualified Chainweb.Miner.RestAPI.Server as Mining
-- import qualified Chainweb.Pact.RestAPI.Server as PactAPI
import qualified Chainweb.Pact.Transaction as Pact
import Chainweb.Payload.RestAPI
import Chainweb.RestAPI.Backup
import Chainweb.RestAPI.Config
import Chainweb.RestAPI.Health
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.NodeInfo
import Chainweb.RestAPI.Utils
import Chainweb.SPV.RestAPI.Server (someSpvServers)
import Chainweb.Utils
import Chainweb.Version

import Network.X509.SelfSigned

import P2P.Node.PeerDB
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
data ChainwebServerDbs = ChainwebServerDbs
    { _chainwebServerCutDb :: !(Maybe CutDb)
    , _chainwebServerBlockHeaderDbs :: !(ChainMap BlockHeaderDb)
    , _chainwebServerMempools :: !(ChainMap (MempoolBackend Pact.Transaction))
    , _chainwebServerPayloads :: !(ChainMap SomeServer)
    , _chainwebServerPeerDbs :: ![(NetworkId, PeerDb)]
    }
    deriving (Generic)

emptyChainwebServerDbs :: ChainwebServerDbs
emptyChainwebServerDbs = ChainwebServerDbs
    { _chainwebServerCutDb = Nothing
    , _chainwebServerBlockHeaderDbs = mempty
    , _chainwebServerMempools = mempty
    , _chainwebServerPayloads = mempty
    , _chainwebServerPeerDbs = []
    }

-- -------------------------------------------------------------------------- --
-- Component Triggers

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

chainwebServiceMiddlewares :: Middleware
chainwebServiceMiddlewares
    = chainwebTime
    . chainwebNodeVersion
    . chainwebCors

-- -------------------------------------------------------------------------- --
-- Chainweb Peer Server

someChainwebServer
    :: HasVersion
    => ChainwebConfiguration
    -> ChainwebServerDbs
    -> SomeServer
someChainwebServer config dbs =
    maybe mempty (someCutServer cutPeerDb) cuts
    <> fold payloads
    <> someP2pBlockHeaderDbServers blocks
    <> Mempool.someMempoolServers mempools
    <> someP2pServers peers
    <> someGetConfigServer config
  where
    payloads = _chainwebServerPayloads dbs
    blocks = _chainwebServerBlockHeaderDbs dbs
    cuts = _chainwebServerCutDb dbs
    peers = _chainwebServerPeerDbs dbs
    mempools = _chainwebServerMempools dbs
    cutPeerDb = fromJuste $ lookup CutNetwork peers

-- | Legacy version with Hashes API that is used in tests
--
-- When we have comprehensive testing for the service API we can remove this
--
someChainwebServerWithHashesAndSpvApi
    :: HasVersion
    => ChainwebConfiguration
    -> ChainwebServerDbs
    -> SomeServer
someChainwebServerWithHashesAndSpvApi config dbs =
    maybe mempty (someCutServer cutPeerDb) cuts
    <> fold payloads
    <> someBlockHeaderDbServers blocks
    <> Mempool.someMempoolServers mempools
    <> someP2pServers peers
    <> someGetConfigServer config
    <> maybe mempty someSpvServers cuts
  where
    payloads = _chainwebServerPayloads dbs
    blocks = _chainwebServerBlockHeaderDbs dbs
    cuts = _chainwebServerCutDb dbs
    peers = _chainwebServerPeerDbs dbs
    mempools = _chainwebServerMempools dbs
    cutPeerDb = fromJuste $ lookup CutNetwork peers

-- -------------------------------------------------------------------------- --
-- Chainweb P2P API Application

chainwebApplication
    :: HasVersion
    => ChainwebConfiguration
    -> ChainwebServerDbs
    -> Application
chainwebApplication config dbs
    = chainwebP2pMiddlewares
    . someServerApplication
    $ someChainwebServer config dbs

-- | Legacy version with Hashes API that is used in tests
--
-- When we have comprehensive testing for the service API we can remove this
--
chainwebApplicationWithHashesAndSpvApi
    :: HasVersion
    => ChainwebConfiguration
    -> ChainwebServerDbs
    -> Application
chainwebApplicationWithHashesAndSpvApi config dbs
    = chainwebP2pMiddlewares
    . someServerApplication
    $ someChainwebServerWithHashesAndSpvApi config dbs

serveChainwebOnPort
    :: HasVersion
    => Port
    -> ChainwebConfiguration
    -> ChainwebServerDbs
    -> IO ()
serveChainwebOnPort p c dbs = run (int p) $ chainwebApplication c dbs

serveChainweb
    :: HasVersion
    => Settings
    -> ChainwebConfiguration
    -> ChainwebServerDbs
    -> IO ()
serveChainweb s c dbs = runSettings s $ chainwebApplication c dbs

serveChainwebSocket
    :: HasVersion
    => Settings
    -> Socket
    -> ChainwebConfiguration
    -> ChainwebServerDbs
    -> Middleware
    -> IO ()
serveChainwebSocket settings sock c dbs m =
    runSettingsSocket settings sock $ m $ chainwebApplication c dbs

serveChainwebSocketTls
    :: HasVersion
    => Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> ChainwebConfiguration
    -> ChainwebServerDbs
    -> Middleware
    -> IO ()
serveChainwebSocketTls settings certChain key sock c dbs m =
    serveSocketTls settings certChain key sock $ m
        $ chainwebApplication c dbs

-- -------------------------------------------------------------------------- --
-- Run Chainweb P2P Server that serves a single PeerDb

servePeerDbSocketTls
    :: HasVersion
    => Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> NetworkId
    -> PeerDb
    -> Middleware
    -> IO ()
servePeerDbSocketTls settings certChain key sock nid pdb m =
    serveSocketTls settings certChain key sock $ m
        $ chainwebP2pMiddlewares
        $ someServerApplication
        $ someP2pServer
        $ somePeerDbVal nid pdb

-- -------------------------------------------------------------------------- --
-- Chainweb Service API Application

someServiceApiServer
    :: Logger logger
    => HasVersion
    => ChainwebServerDbs
    -> Maybe (MiningCoordination logger)
    -> HeaderStream
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> SomeServer
someServiceApiServer dbs mr (HeaderStream hs) backupEnv pbl =
    someHealthCheckServer
    <> maybe mempty someBackupServer backupEnv
    <> maybe mempty someNodeInfoServer cuts
    <> maybe mempty Mining.someMiningServer mr
    <> maybe mempty someSpvServers cuts -- AFAIK currently not used

    -- GET Cut, Payload, and Headers endpoints
    <> maybe mempty someCutGetServer cuts
    <> fold payloads
    <> someBlockHeaderDbServers blocks
    <> maybe mempty someBlockStreamServer (bool Nothing cuts hs)
  where
    cuts = _chainwebServerCutDb dbs
    payloads = _chainwebServerPayloads dbs
    blocks = _chainwebServerBlockHeaderDbs dbs

serviceApiApplication
    :: Logger logger
    => HasVersion
    => ChainwebServerDbs
    -> Maybe (MiningCoordination logger)
    -> HeaderStream
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> Application
serviceApiApplication dbs mr hs benv pbl
    = chainwebServiceMiddlewares
    . someServerApplication
    $ someServiceApiServer dbs mr hs benv pbl

serveServiceApiSocket
    :: Logger logger
    => HasVersion
    => Settings
    -> Socket
    -> ChainwebServerDbs
    -> Maybe (MiningCoordination logger)
    -> HeaderStream
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> Middleware
    -> IO ()
serveServiceApiSocket s sock dbs mr hs benv pbl m =
    runSettingsSocket s sock $ m $ serviceApiApplication dbs mr hs benv pbl
