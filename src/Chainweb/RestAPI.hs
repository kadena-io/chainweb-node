{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

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
, chainwebApplication
-- , chainwebApplicationWithHashesAndSpvApi
, serveChainwebOnPort
, serveChainweb
, serveChainwebSocket
, serveChainwebSocketTls

-- ** Only serve a Peer DB from the Chainweb P2P API
, servePeerDbSocketTls

-- * Service API Server
, serviceApiApplication
, serveServiceApiSocket

-- * Chainweb API Client

-- ** BlockHeaderDb API Client
, module Chainweb.BlockHeaderDB.RestAPI.Client

-- ** P2P API Client
, module P2P.Node.RestAPI.Client
) where

import Control.Lens
import Control.Monad (guard)

import Data.Bifunctor
import Data.Bool (bool)
import Data.Foldable
import Data.Maybe
import qualified Data.HashSet as HS

import GHC.Generics (Generic)
import GHC.Stack

import Network.HTTP.Types
import Network.Socket
import qualified Network.TLS.SessionManager as TLS
import Network.Wai
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai.Handler.WarpTLS (TLSSettings(..), runTLSSocket)
import Network.Wai.Middleware.Cors

import System.Clock

import Web.DeepRoute
import Web.DeepRoute.Wai
import Web.HttpApiData

-- internal modules

import Chainweb.Backup
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.BlockHeaderDB.RestAPI.Server
import Chainweb.ChainId
import Chainweb.Chainweb.Configuration
import Chainweb.Chainweb.MinerResources (MiningCoordination)
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI
import Chainweb.HostAddress
import Chainweb.Logger (Logger)
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.RestAPI as Mempool
import qualified Chainweb.Miner.RestAPI as Mining
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
import Chainweb.SPV.RestAPI.Server (newSpvServer)
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

chainwebServiceMiddlewares :: Middleware
chainwebServiceMiddlewares
    = chainwebTime
    . chainwebNodeVersion
    . chainwebCors

-- -------------------------------------------------------------------------- --
-- Chainweb P2P API Application

($<$) :: Functor f => f (b -> c) -> (a -> b) -> f (a -> c)
($<$) a b = (. b) <$> a
infixr 4 $<$
{-# INLINE ($<$) #-}

data P2pApiOptions = P2pApiOptions
    { blockHeaderDbServerOptions :: !BlockHeaderDbServerOptions
    , spvServerEnabled :: !Bool
    }

defaultP2pApiOptions :: P2pApiOptions
defaultP2pApiOptions = P2pApiOptions
    { blockHeaderDbServerOptions = BlockHeaderDbServerOptions
        { enableHashesEndpoints = False
        , entryLimit = 20
        }
    , spvServerEnabled = False
    }

chainwebApplication
    :: Show t
    => CanReadablePayloadCas tbl
    => P2pApiOptions
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> Application
chainwebApplication apiOptions config dbs
    = chainwebP2pMiddlewares
    $ \req resp -> routeWaiApp req resp
        (resp $ responseLBS notFound404 [] mempty)
        $ fold
            [ choice "chainweb" $ choice "0.0" $ choice (chainwebVersionToText v) $ fold
                [ choice "cut" $ fold
                    [ maybe mempty (newCutServer cutPeerDb) cuts
                    , choice "peer" $ newP2pServer v ?? (cutPeerDb, CutNetwork)
                    ]
                , choice "chain" $
                    captureValidChainId v $ fold
                        [ fold [ choice "spv" $ choice "chain" $ captureValidChainId v $ maybe mempty newSpvServer cuts | spvServerEnabled apiOptions ]
                        , choice "payload" $ newPayloadServer p2pPayloadBatchLimit $<$ lookupResource payloads
                        , newBlockHeaderDbServer (blockHeaderDbServerOptions apiOptions) $<$ lookupResource blocks
                        , choice "peer" $ newP2pServer v $<$ p2pOn ChainNetwork
                        , choice "mempool" $ fold
                            [ Mempool.newMempoolServer $<$ lookupResource mempools
                            , choice "peer" $ newP2pServer v $<$ p2pOn MempoolNetwork
                            ]
                        ]
                ]
            , choice "config" $ newGetConfigServer config
            ]
  where
    p2pOn makeNetworkId (makeNetworkId -> nid) = (lookupResource peers nid, nid)
    payloads = _chainwebServerPayloadDbs dbs
    blocks = _chainwebServerBlockHeaderDbs dbs
    cuts = _chainwebServerCutDb dbs
    peers = _chainwebServerPeerDbs dbs
    mempools = _chainwebServerMempools dbs
    cutPeerDb = fromJuste $ lookup CutNetwork peers
    v = _configChainwebVersion config

serveChainwebOnPort
    :: Show t
    => CanReadablePayloadCas tbl
    => Port
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> IO ()
serveChainwebOnPort p c dbs = run (int p) $ chainwebApplication defaultP2pApiOptions c dbs

serveChainweb
    :: Show t
    => CanReadablePayloadCas tbl
    => Settings
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> IO ()
serveChainweb s c dbs = runSettings s $ chainwebApplication defaultP2pApiOptions c dbs

serveChainwebSocket
    :: Show t
    => CanReadablePayloadCas tbl
    => Settings
    -> Socket
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> Middleware
    -> IO ()
serveChainwebSocket settings sock c dbs m =
    runSettingsSocket settings sock $ m $ chainwebApplication defaultP2pApiOptions c dbs

serveChainwebSocketTls
    :: Show t
    => CanReadablePayloadCas tbl
    => Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> Middleware
    -> IO ()
serveChainwebSocketTls settings certChain key sock c dbs m =
    serveSocketTls settings certChain key sock $ m
        $ chainwebApplication defaultP2pApiOptions c dbs

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

serviceApiApplication
    :: Show t
    => CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebConfiguration
    -> ChainwebServerDbs t tbl
    -> [(ChainId, PactAPI.PactServerData logger tbl)]
    -> Maybe (MiningCoordination logger tbl)
    -> HeaderStream
    -> Rosetta
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> Application
serviceApiApplication config dbs pacts mr (HeaderStream hs) (Rosetta r) backupEnv pbl
    = chainwebServiceMiddlewares
    $ \req resp -> routeWaiApp req resp
        (someServerApplication (fold
            -- TODO: not sure if passing the correct PeerDb here
            -- TODO: why does Rosetta need a peer db at all?
            -- TODO: simplify number of resources passing to rosetta
            [ maybe mempty (bool mempty (someRosettaServer v payloads concreteMs cutPeerDb concretePacts) r) cuts
            , PactAPI.somePactServers v pacts
            ]) req resp)
        $ fold
        [ newHealthCheckServer
        , maybe mempty (nodeInfoApi v) cuts
        , maybe mempty Mining.miningApi mr
        , choice "config" $ newGetConfigServer config
        , choice "chainweb" $ choice "0.0" $ choice (chainwebVersionToText v) $ fold
            [ maybe mempty headerStreamServer (bool Nothing cuts hs)
            , choice "backup" $ maybe mempty newBackupApi backupEnv
            , choice "cut" $ maybe mempty newCutGetServer cuts
            , choice "chain" $
                captureValidChainId v $ fold
                    [ choice "payload" $ newPayloadServer pbl $<$ lookupResource payloads
                    , choice "spv" $
                        choice "chain" $ captureValidChainId v $ maybe mempty newSpvServer cuts
                    , newBlockHeaderDbServer
                        BlockHeaderDbServerOptions { enableHashesEndpoints = True, entryLimit = 360 } $<$
                        lookupResource blocks
                    ]
            ]
        ]
  where
    v = _chainwebVersion config
    cuts = _chainwebServerCutDb dbs
    peers = _chainwebServerPeerDbs dbs
    concreteMs = second PactAPI._pactServerDataMempool <$> pacts
    concretePacts = second PactAPI._pactServerDataPact <$> pacts
    cutPeerDb = fromJuste $ lookup CutNetwork peers
    payloads = _chainwebServerPayloadDbs dbs
    blocks = _chainwebServerBlockHeaderDbs dbs

serveServiceApiSocket
    :: Show t
    => CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebConfiguration
    -> Settings
    -> Socket
    -> ChainwebServerDbs t tbl
    -> [(ChainId, PactAPI.PactServerData logger tbl)]
    -> Maybe (MiningCoordination logger tbl)
    -> HeaderStream
    -> Rosetta
    -> Maybe (BackupEnv logger)
    -> PayloadBatchLimit
    -> Middleware
    -> IO ()
serveServiceApiSocket conf s sock dbs pacts mr hs r be pbl m =
    runSettingsSocket s sock $ m $ serviceApiApplication conf dbs pacts mr hs r be pbl

captureValidChainId :: HasChainwebVersion v => v -> Route (ChainId -> a) -> Route a
captureValidChainId v = capture' $ \p -> do
    cid <- parseUrlPieceMaybe p
    guard (HS.member cid (chainIds v))
    return cid

lookupResource :: (HasCallStack, Eq a) => [(a, b)] -> a -> b
lookupResource ress ident =
    fromMaybe (error "internal error: failed to look up resource by identifier") $
        lookup ident ress
