{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module: Chainweb.Chainweb
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module provides the tools to initialize the different components of the
-- chainweb consensus and run a chainweb-consensus node.
--
-- The overall pattern is that for each component @C@ there is
--
-- 1.  a function @withC@ that manages the resources of the component and passes
--     a handle that encapsulates those resources to an inner computation, and
--
-- 2.  a function @runC@ that takes the resource handle of @C@ and starts the
    -- component.
--
-- This pattern allows to bootstrap components with mutual dependencies and
-- resolve timing and ordering constraints for the startup of services.
--
-- Logging functions are initialized in the enviroment and passed as call backs
-- to the components.
--
module Chainweb.Chainweb
(
-- * Configuration
  ChainwebConfiguration(..)
, configNodeId
, configChainwebVersion
, configMiner
, configP2p
, configChainDbDirPath
, defaultChainwebConfiguration
, pChainwebConfiguration

-- * Chainweb Logging Functions
, ChainwebLogFunctions(..)
, chainwebNodeLogFun
, chainwebMinerLogFun
, chainwebCutLogFun
, chainwebChainLogFuns

-- * Chainweb Resources
, Chainweb(..)
, chainwebChains
, chainwebCutResources
, chainwebNodeId
, chainwebHostAddress
, chainwebMiner
, chainwebLogFun
, chainwebSocket
, chainwebPeer
, chainwebPayloadDb

-- ** Mempool integration
, ChainwebTransaction
, chainwebTransactionConfig

, withChainweb
, runChainweb

-- * Miner
, runMiner

) where

import Configuration.Utils hiding (Lens', (<.>))

import Control.Concurrent.Async
import Control.Lens hiding ((.=), (<.>))
import Control.Monad

import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import GHC.Generics hiding (from)

import Prelude hiding (log)

import qualified Network.HTTP.Client as HTTP
import Network.Socket (Socket)

import System.LogLevel

-- internal modules

import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.MinerResources
import Chainweb.Chainweb.PeerResources
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.HostAddress
import qualified Chainweb.Mempool.InMem as Mempool
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.LogMessage

import P2P.Node.Configuration
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Chainweb Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configNodeId :: !NodeId
    , _configMiner :: !MinerConfig
    , _configP2p :: !P2pConfiguration
    , _configChainDbDirPath :: !(Maybe FilePath)
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

instance HasChainwebVersion ChainwebConfiguration where
    _chainwebVersion = _configChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ChainwebConfiguration where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

defaultChainwebConfiguration :: ChainwebVersion -> ChainwebConfiguration
defaultChainwebConfiguration v = ChainwebConfiguration
    { _configChainwebVersion = v
    , _configNodeId = NodeId 0 -- FIXME
    , _configMiner = defaultMinerConfig
    , _configP2p = defaultP2pConfiguration v
    , _configChainDbDirPath = Nothing
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "nodeId" .= _configNodeId o
        , "miner" .= _configMiner o
        , "p2p" .= _configP2p o
        , "chainDbDirPath" .= _configChainDbDirPath o
        ]

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configNodeId ..: "nodeId" % o
        <*< configMiner %.: "miner" % o
        <*< configP2p %.: "p2p" % o
        <*< configChainDbDirPath ..: "chainDbDirPath" % o

pChainwebConfiguration :: MParser ChainwebConfiguration
pChainwebConfiguration = id
    <$< configChainwebVersion .:: textOption
        % long "chainweb-version"
        <> short 'v'
        <> help "the chainweb version that this node is using"
    <*< configNodeId .:: textOption
        % long "node-id"
        <> short 'i'
        <> help "unique id of the node that is used as miner id in new blocks"
    <*< configMiner %:: pMinerConfig
    <*< configP2p %:: pP2pConfiguration Nothing
    <*< configChainDbDirPath .:: fmap Just % textOption
        % long "chain-db-dir"
        <> help "directory where chain databases are persisted"

-- -------------------------------------------------------------------------- --
-- Chainweb Log Functions

data ChainwebLogFunctions = ChainwebLogFunctions
    { _chainwebNodeLogFun :: !ALogFunction
    , _chainwebMinerLogFun :: !ALogFunction
    , _chainwebCutLogFun :: !ALogFunction
    , _chainwebChainLogFuns :: !(HM.HashMap ChainId ALogFunction)
    }

makeLenses ''ChainwebLogFunctions

-- -------------------------------------------------------------------------- --
-- Chainweb Resources

data Chainweb cas = Chainweb
    { _chainwebHostAddress :: !HostAddress
    , _chainwebChains :: !(HM.HashMap ChainId ChainResources)
    , _chainwebCutResources :: !(CutResources cas)
    , _chainwebNodeId :: !NodeId
    , _chainwebMiner :: !(MinerResources cas)
    , _chainwebLogFun :: !ALogFunction
    , _chainwebPeer :: !PeerResources
    , _chainwebPayloadDb :: !(PayloadDb cas)
    , _chainwebManager :: !HTTP.Manager
    }

makeLenses ''Chainweb

chainwebSocket :: Getter (Chainweb cas) Socket
chainwebSocket = chainwebPeer . peerResSocket

instance HasChainwebVersion (Chainweb cas) where
    _chainwebVersion = _chainwebVersion . _chainwebCutResources
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph (Chainweb cas) where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainweb
    :: PayloadCas cas
    => ChainwebConfiguration
    -> ChainwebLogFunctions
    -> (Chainweb cas -> IO a)
    -> IO a
withChainweb conf logFuns inner
    = withPeerResources v (view configP2p conf) mgrLogfun $ \peer ->
        withChainwebInternal (set configP2p (_peerResConfig peer) conf) logFuns peer inner
  where
    v = _chainwebVersion conf
    mgrLogfun = _chainwebNodeLogFun logFuns

mempoolConfig :: Mempool.InMemConfig ChainwebTransaction
mempoolConfig = Mempool.InMemConfig
    chainwebTransactionConfig
    blockGasLimit
    mempoolReapInterval
  where
    blockGasLimit = 1000000                 -- TODO: policy decision
    mempoolReapInterval = 60 * 20 * 1000000   -- 20 mins

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainwebInternal
    :: PayloadCas cas
    => ChainwebConfiguration
    -> ChainwebLogFunctions
    -> PeerResources
    -> (Chainweb cas -> IO a)
    -> IO a
withChainwebInternal conf logFuns peer inner = do
    payloadDb <- emptyPayloadDb
    initializePayloadDb v payloadDb
    go payloadDb mempty (toList cids)
  where
    -- Initialize chain resources
    go payloadDb cs (cid : t) =
        case HM.lookup cid (_chainwebChainLogFuns logFuns) of
            Nothing -> error $ T.unpack
                $ "Failed to initialize chainweb node: missing log function for chain " <> toText cid
            Just logfun ->
                withChainResources v cid peer chainDbDir logfun mempoolConfig $ \c ->
                    go payloadDb (HM.insert cid c cs) t

    -- Initialize global resources
    go payloadDb cs [] = do
        let webchain = mkWebBlockHeaderDb v (HM.map _chainResBlockHeaderDb cs)
        let cutLogfun = _chainwebCutLogFun logFuns
        let mgr = _peerResManager peer
        withCutResources cutConfig peer cutLogfun webchain payloadDb mgr $ \cuts ->
            withMiner
                (_chainwebMinerLogFun logFuns)
                (_configMiner conf)
                cwnid
                (_cutResCutDb cuts)
                webchain
                payloadDb
                $ \m -> inner Chainweb
                    { _chainwebHostAddress = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
                    , _chainwebChains = cs
                    , _chainwebCutResources = cuts
                    , _chainwebNodeId = cwnid
                    , _chainwebMiner = m
                    , _chainwebLogFun = _chainwebNodeLogFun logFuns
                    , _chainwebPeer = peer
                    , _chainwebPayloadDb = payloadDb
                    , _chainwebManager = mgr
                    }

    v = _configChainwebVersion conf
    graph = _chainGraph v
    cids = chainIds_ graph
    cwnid = _configNodeId conf
    chainDbDir = _configChainDbDirPath conf

    -- FIXME: make this configurable
    cutConfig = (defaultCutDbConfig v)
        { _cutDbConfigLogLevel = Info
        , _cutDbConfigTelemetryLevel = Info
        }

-- Starts server and runs all network clients
--
runChainweb :: PayloadCas cas => Chainweb cas -> IO ()
runChainweb cw = do
    logfun Info "start chainweb node"

    let cutDb = _cutResCutDb $ _chainwebCutResources cw
        cutPeerDb = _peerResDb $ _cutResPeer $ _chainwebCutResources cw

    -- Startup sequnce:
    --
    -- 1. Start serving Rest API
    -- 2. Start Clients
    --

    -- collect server resources
    let chains = HM.toList (_chainwebChains cw)
        chainVals = map snd chains
        proj :: forall a . (ChainResources -> a) -> [(ChainId, a)]
        proj f = flip map chains $ \(k, ch) -> (k, f ch)
        chainDbsToServe = proj _chainResBlockHeaderDb
        mempoolsToServe = proj _chainResMempool
        chainP2pToServe = bimap ChainNetwork (_peerResDb . _chainResPeer) <$> itoList (_chainwebChains cw)

        payloadDbsToServe = itoList $ const (view chainwebPayloadDb cw) <$> _chainwebChains cw

        serverSettings = peerServerSettings (_peerResPeer $ _chainwebPeer cw)
        serve = serveChainwebSocketTls
            serverSettings
            (_peerCertificate $ _peerResPeer $ _chainwebPeer cw)
            (_peerKey $ _peerResPeer $ _chainwebPeer cw)
            (_peerResSocket $ _chainwebPeer cw)
            (_chainwebVersion cw)
            ChainwebServerDbs
                { _chainwebServerCutDb = Just cutDb
                , _chainwebServerBlockHeaderDbs = chainDbsToServe
                , _chainwebServerMempools = mempoolsToServe
                , _chainwebServerPayloadDbs = payloadDbsToServe
                , _chainwebServerPeerDbs = (CutNetwork, cutPeerDb) : chainP2pToServe
                }

    -- 1. start server
    --
    withAsync serve $ \server -> do
        logfun Info "started server"

        -- Configure Clients
        --
        let mgr = view chainwebManager cw

        -- 2. Run Clients
        --
        let clients :: [IO ()]
            clients = concat
                [ [runMiner (_chainwebVersion cw) (_chainwebMiner cw)]
                -- FIXME: should we start mining with some delay, so
                -- that the block header base is up to date?
                , cutNetworks mgr (_chainwebCutResources cw)
                , map (runChainSyncClient mgr) chainVals
                , map (runMempoolSyncClient mgr) chainVals
                ]

        mapConcurrently_ id clients
        wait server
  where
    logfun = alogFunction @T.Text (_chainwebLogFun cw)

