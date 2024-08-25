{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- * Chainweb API Client

-- ** BlockHeaderDb API Client
, module Chainweb.BlockHeaderDB.RestAPI.Client

-- ** P2P API Client
, module P2P.Node.RestAPI.Client
) where

import Control.Monad

import Data.Bifunctor
import Data.Bool (bool)

import GHC.Generics (Generic)

import Network.Socket
import qualified Network.TLS.SessionManager as TLS
import Network.Wai
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai.Handler.WarpTLS (TLSSettings(..), runTLSSocket)
import Network.Wai.Middleware.Cors

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
import Chainweb.Logger (Logger, logFunctionText)
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
import Chainweb.SPV.RestAPI.Server (someSpvServers)
import Chainweb.Utils
import Chainweb.Version

import Network.X509.SelfSigned

import P2P.Node.PeerDB
import P2P.Node.RestAPI.Client
import P2P.Node.RestAPI.Server
import Data.IORef
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.LogLevel
import qualified Data.Text.Encoding as T

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
data ChainwebServerDbs t tbl = ChainwebServerDbs
    { _chainwebServerCutDb :: !(Maybe (CutDb tbl))
    , _chainwebServerBlockHeaderDbs :: ![(ChainId, BlockHeaderDb)]
    , _chainwebServerMempools :: ![(ChainId, MempoolBackend t)]
    , _chainwebServerPayloadDbs :: ![(ChainId, PayloadDb tbl)]
    , _chainwebServerPeerDbs :: ![(NetworkId, PeerDb)]
    }
    deriving (Generic)

emptyChainwebServerDbs :: ChainwebServerDbs t tbl
emptyChainwebServerDbs = ChainwebServerDbs
    { _chainwebServerCutDb = Nothing
    , _chainwebServerBlockHeaderDbs = []
    , _chainwebServerMempools = []
    , _chainwebServerPayloadDbs = []
    , _chainwebServerPeerDbs = []
    }

-- -------------------------------------------------------------------------- --
-- Component Triggers

data Rosetta = Rosetta
    { _rosettaDefault :: {-# UNPACK #-} !Bool
    , _rosettaConstructionApi :: {-# UNPACK #-} !Bool
    }

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

largeResponseLogger :: Logger logger => logger -> Middleware
largeResponseLogger logger app req resp = app req $ \res -> do
    responseByteCounter <- newIORef 0
    responseLoggedForSize <- newIORef False
    -- deconstruct the outgoing response into a stream
    let (status, headers, withResponseBody) = responseToStream res
    let monitoredWriteChunk writeChunk b = do
            let lbs = Builder.toLazyByteString b
            let chunks = LBS.toChunks lbs
            -- for each chunk of the outgoing stream, add its length to the accumulator.
            -- if it's over the limit, log, and don't log next time.
            -- always process the outgoing chunk after.
            forM_ chunks $ \chunk -> do
                responseByteCount' <- atomicModifyIORef' responseByteCounter $ \count ->
                    let count' = count + BS.length chunk
                    in (count', count')
                loggedAlready <- readIORef responseLoggedForSize
                when (responseByteCount' >= 50 * kilo && not loggedAlready) $ do
                    logFunctionText logger Error $ "Large response body (>50KB) outbound from path " <> T.decodeUtf8 (rawPathInfo req)
                    writeIORef responseLoggedForSize True
                writeChunk (Builder.byteString chunk)
    withResponseBody $ \originalBody -> do
        respReceived <- resp $ responseStream status headers $ \writeChunk doFlush ->
            originalBody (monitoredWriteChunk writeChunk) doFlush
        finalResponseByteCount <- readIORef responseByteCounter
        loggedAlready <- readIORef responseLoggedForSize
        -- log the full response size, too, for good measure
        when loggedAlready $
            logFunctionText logger Error $
                "Large response body (" <> sshow finalResponseByteCount <> "B) outbound from path "
                <> T.decodeUtf8 (rawPathInfo req)
        return respReceived

chainwebP2pMiddlewares :: Logger logger => logger -> Middleware
chainwebP2pMiddlewares logger
    = chainwebTime
    . chainwebPeerAddr
    . chainwebNodeVersion
    . largeResponseLogger logger

chainwebServiceMiddlewares :: Middleware
chainwebServiceMiddlewares
    = chainwebTime
    . chainwebNodeVersion
    . chainwebCors

-- -------------------------------------------------------------------------- --
-- Chainweb Peer Server

someChainwebServer
    :: Show t
    => CanReadablePayloadCas tbl
    => ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> SomeServer
someChainwebServer config dbs =
    maybe mempty (someCutServer v cutPeerDb) cuts
    <> somePayloadServers v p2pPayloadBatchLimit payloads
    <> someP2pBlockHeaderDbServers v blocks
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

-- | Legacy version with Hashes API that is used in tests
--
-- When we have comprehensive testing for the service API we can remove this
--
someChainwebServerWithHashesAndSpvApi
    :: Show t
    => CanReadablePayloadCas tbl
    => ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> SomeServer
someChainwebServerWithHashesAndSpvApi config dbs =
    maybe mempty (someCutServer v cutPeerDb) cuts
    <> somePayloadServers v p2pPayloadBatchLimit payloads
    <> someBlockHeaderDbServers v blocks payloads
    <> Mempool.someMempoolServers v mempools
    <> someP2pServers v peers
    <> someGetConfigServer config
    <> maybe mempty (someSpvServers v) cuts
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
    => CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> Application
chainwebApplication logger config dbs
    = chainwebP2pMiddlewares logger
    . someServerApplication
    $ someChainwebServer config dbs

-- | Legacy version with Hashes API that is used in tests
--
-- When we have comprehensive testing for the service API we can remove this
--
chainwebApplicationWithHashesAndSpvApi
    :: Show t
    => CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> Application
chainwebApplicationWithHashesAndSpvApi logger config dbs
    = chainwebP2pMiddlewares logger
    . someServerApplication
    $ someChainwebServerWithHashesAndSpvApi config dbs

serveChainwebOnPort
    :: Show t
    => CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> Port
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> IO ()
serveChainwebOnPort logger p c dbs = run (int p) $ chainwebApplication logger c dbs

serveChainweb
    :: Show t
    => CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> Settings
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> IO ()
serveChainweb logger s c dbs = runSettings s $ chainwebApplication logger c dbs

serveChainwebSocket
    :: Show t
    => CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> Settings
    -> Socket
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> Middleware
    -> IO ()
serveChainwebSocket logger settings sock c dbs m =
    runSettingsSocket settings sock $ m $ chainwebApplication logger c dbs

serveChainwebSocketTls
    :: Show t
    => CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> Middleware
    -> IO ()
serveChainwebSocketTls logger settings certChain key sock c dbs m =
    serveSocketTls settings certChain key sock $ m
        $ chainwebApplication logger c dbs

-- -------------------------------------------------------------------------- --
-- Run Chainweb P2P Server that serves a single PeerDb

servePeerDbSocketTls
    :: Logger logger
    => logger
    -> Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> ChainwebVersion
    -> NetworkId
    -> PeerDb
    -> Middleware
    -> IO ()
servePeerDbSocketTls logger settings certChain key sock v nid pdb m =
    serveSocketTls settings certChain key sock $ m
        $ chainwebP2pMiddlewares logger
        $ someServerApplication
        $ someP2pServer
        $ somePeerDbVal v nid pdb

-- -------------------------------------------------------------------------- --
-- Chainweb Service API Application

someServiceApiServer
    :: Show t
    => CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebVersion
    -> ChainwebServerDbs t tbl
    -> [(ChainId, PactAPI.PactServerData logger tbl)]
    -> Maybe (MiningCoordination logger tbl)
    -> HeaderStream
    -> Rosetta
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> SomeServer
someServiceApiServer v dbs pacts mr (HeaderStream hs) (Rosetta r rc) backupEnv pbl =
    someHealthCheckServer
    <> maybe mempty (someBackupServer v) backupEnv
    <> maybe mempty (someNodeInfoServer v) cuts
    <> PactAPI.somePactServers v pacts
    <> maybe mempty (Mining.someMiningServer v) mr
    <> maybe mempty (bool mempty (someRosettaServer v payloads concreteMs cutPeerDb concretePacts) r) cuts
    <> maybe mempty
        (\cdb -> bool
            -- if rosetta is enabled but the construction API is disabled the server
            -- returns a failure with a descriptive failure message instead of 404.
            (bool mempty (someRosettaConstructionDeprecationServer v) r)
            (someRosettaConstructionServer v concreteMs concretePacts cdb)
            rc
        )
        cuts
    -- <> maybe mempty (someSpvServers v) cuts -- AFAIK currently not used

    -- GET Cut, Payload, and Headers endpoints
    <> maybe mempty (someCutGetServer v) cuts
    <> somePayloadServers v pbl payloads
    <> someBlockHeaderDbServers v blocks payloads -- TODO make max limits configurable
    <> maybe mempty (someBlockStreamServer v) (bool Nothing cuts hs)
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
    => CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebVersion
    -> ChainwebServerDbs t tbl
    -> [(ChainId, PactAPI.PactServerData logger tbl)]
    -> Maybe (MiningCoordination logger tbl)
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
    => CanReadablePayloadCas tbl
    => Logger logger
    => Settings
    -> Socket
    -> ChainwebVersion
    -> ChainwebServerDbs t tbl
    -> [(ChainId, PactAPI.PactServerData logger tbl)]
    -> Maybe (MiningCoordination logger tbl)
    -> HeaderStream
    -> Rosetta
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> Middleware
    -> IO ()
serveServiceApiSocket s sock v dbs pacts mr hs r be pbl m =
    runSettingsSocket s sock $ m $ serviceApiApplication v dbs pacts mr hs r be pbl
