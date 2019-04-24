{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- * Pact Configuration
  TransactionIndexConfig(..)
, defaultTransactionIndexConfig
, pTransactionIndexConfig

-- * Configuration
, ChainwebConfiguration(..)
, configNodeId
, configChainwebVersion
, configMiner
, configP2p
, configTransactionIndex
, defaultChainwebConfiguration
, pChainwebConfiguration

-- * Chainweb Resources
, Chainweb(..)
, chainwebChains
, chainwebCutResources
, chainwebNodeId
, chainwebHostAddress
, chainwebMiner
, chainwebLogger
, chainwebSocket
, chainwebPeer
, chainwebPayloadDb
, chainwebPactData
, chainwebThrottler

-- ** Mempool integration
, ChainwebTransaction
, chainwebTransactionConfig

, withChainweb
, runChainweb

-- * Miner
, runMiner

) where

import Chainweb.Payload.PayloadStore.RocksDB

import Configuration.Utils hiding (Lens', (<.>))

import Control.Concurrent.Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad

import Data.Foldable
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)

import GHC.Generics hiding (from)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP
import Network.Socket (Socket)
import Network.Wai
import Network.Wai.Middleware.Throttle

import Numeric.Natural

import Prelude hiding (log)

import System.Clock
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
import Chainweb.Logger
import qualified Chainweb.Mempool.InMem as Mempool
import Chainweb.Miner.Config
import Chainweb.NodeId
import qualified Chainweb.Pact.BloomCache as Bloom
import Chainweb.Pact.RestAPI.Server (PactServerData)
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.CAS.RocksDB

import P2P.Node.Configuration
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- TransactionIndexConfig

data TransactionIndexConfig = TransactionIndexConfig
    deriving (Show, Eq, Generic)

makeLenses ''TransactionIndexConfig

defaultTransactionIndexConfig :: TransactionIndexConfig
defaultTransactionIndexConfig = TransactionIndexConfig

instance ToJSON TransactionIndexConfig where
    toJSON _ = object []

instance FromJSON (TransactionIndexConfig -> TransactionIndexConfig) where
    parseJSON = withObject "TransactionIndexConfig" $ const (return id)

pTransactionIndexConfig :: MParser TransactionIndexConfig
pTransactionIndexConfig = pure id

-- -------------------------------------------------------------------------- --
-- Chainweb Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configNodeId :: !NodeId
    , _configMiner :: !(EnableConfig MinerConfig)
    , _configP2p :: !P2pConfiguration
    , _configTransactionIndex :: !(EnableConfig TransactionIndexConfig)
    , _configIncludeOrigin :: !Bool
    , _configThrottleRate :: !Natural
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
    , _configMiner = defaultEnableConfig defaultMinerConfig
    , _configP2p = defaultP2pConfiguration
    , _configTransactionIndex = defaultEnableConfig defaultTransactionIndexConfig
    , _configIncludeOrigin = True
    , _configThrottleRate = 1000
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "nodeId" .= _configNodeId o
        , "miner" .= _configMiner o
        , "p2p" .= _configP2p o
        , "transactionIndex" .= _configTransactionIndex o
        , "includeOrigin" .= _configIncludeOrigin o
        , "throttleRate" .= _configThrottleRate o
        ]

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configNodeId ..: "nodeId" % o
        <*< configMiner %.: "miner" % o
        <*< configP2p %.: "p2p" % o
        <*< configTransactionIndex %.: "transactionIndex" % o
        <*< configIncludeOrigin ..: "includeOrigin" % o
        <*< configThrottleRate ..: "throttleRate" % o

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
    <*< configMiner %:: pEnableConfig "mining" pMinerConfig
    <*< configP2p %:: pP2pConfiguration Nothing
    <*< configTransactionIndex %::
        pEnableConfig "transaction-index" pTransactionIndexConfig
    <*< configIncludeOrigin .:: enableDisableFlag
        % long "include-origin"
        <> help "whether to include the local peer as origin when publishing cut hashes"
    <*< configThrottleRate .:: option auto
        % long "throttle-rate"
        <> help "how many requests per second are accepted from another node before it is being throttled"

-- -------------------------------------------------------------------------- --
-- Chainweb Resources

data Chainweb logger cas = Chainweb
    { _chainwebHostAddress :: !HostAddress
    , _chainwebChains :: !(HM.HashMap ChainId (ChainResources logger))
    , _chainwebCutResources :: !(CutResources logger cas)
    , _chainwebNodeId :: !NodeId
    , _chainwebMiner :: !(Maybe (MinerResources logger cas))
    , _chainwebLogger :: !logger
    , _chainwebPeer :: !(PeerResources logger)
    , _chainwebPayloadDb :: !(PayloadDb cas)
    , _chainwebManager :: !HTTP.Manager
    , _chainwebPactData :: [(ChainId, PactServerData logger cas)]
    , _chainwebThrottler :: !(Throttle Address)
    }

makeLenses ''Chainweb

chainwebSocket :: Getter (Chainweb logger cas) Socket
chainwebSocket = chainwebPeer . peerResSocket

instance HasChainwebVersion (Chainweb logger cas) where
    _chainwebVersion = _chainwebVersion . _chainwebCutResources
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph (Chainweb logger cas) where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainweb
    :: Logger logger
    => ChainwebConfiguration
    -> logger
    -> RocksDb
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainweb c logger rocksDb inner =
    withPeerResources v (view configP2p conf) logger $ \logger' peer ->
        withChainwebInternal
            (set configP2p (_peerResConfig peer) conf)
            logger'
            peer
            rocksDb
            inner
  where
    v = _chainwebVersion c

    -- Here we inject the hard-coded bootstrap peer infos for the configured
    -- chainweb version into the configuration.
    conf
        | _p2pConfigIgnoreBootstrapNodes (_configP2p c) = c
        | otherwise = configP2p . p2pConfigKnownPeers <>~ bootstrapPeerInfos v $ c

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
-- TODO: abstract cas creation
--
withChainwebInternal
    :: Logger logger
    => ChainwebConfiguration
    -> logger
    -> PeerResources logger
    -> RocksDb
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainwebInternal conf logger peer rocksDb inner = do
    initializePayloadDb v payloadDb
    cutMV <- newEmptyMVar
    go mempty (toList cids) cutMV
  where
    payloadDb = newPayloadDb rocksDb
    chainLogger cid = addLabel ("chain", toText cid) logger

    -- Initialize chain resources
    go cs (cid : t) mv =
        withChainResources v cid rocksDb peer (chainLogger cid) mempoolConfig mv payloadDb $ \c ->
            go (HM.insert cid c cs) t mv

    -- Initialize global resources
    go cs [] mv = do
        let webchain = mkWebBlockHeaderDb v (HM.map _chainResBlockHeaderDb cs)
            pact = mkWebPactExecutionService (HM.map _chainResPact cs)
            cutLogger = setComponent "cut" logger
            mgr = _peerResManager peer
        withCutResources cutConfig peer cutLogger webchain payloadDb mgr pact $ \cuts -> do
            let mLogger = setComponent "miner" logger
                mConf = _configMiner conf
                mCutDb = _cutResCutDb cuts

            -- initialize throttler
            throttler <- initThrottler (defaultThrottleSettings $ TimeSpec 4 0)
                { throttleSettingsRate = int $ _configThrottleRate conf
                , throttleSettingsPeriod = 1 / micro -- 1 second (measured in usec)
                , throttleSettingsBurst = int $ _configThrottleRate conf
                , throttleSettingsIsThrottled = const True
                -- , throttleSettingsIsThrottled = \r -> any (flip elem (pathInfo r))
                --     [ "cut"
                --     , "header"
                --     , "payload"
                --     , "mempool"
                --     , "peer"
                --     ]
                }

            -- update the cutdb mvar used by pact service with cutdb
            void $! putMVar mv mCutDb

            withPactData cs cuts $ \pactData ->
                withMinerResources mLogger mConf cwnid mCutDb $ \m ->
                inner Chainweb
                    { _chainwebHostAddress = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
                    , _chainwebChains = cs
                    , _chainwebCutResources = cuts
                    , _chainwebNodeId = cwnid
                    , _chainwebMiner = m
                    , _chainwebLogger = logger
                    , _chainwebPeer = peer
                    , _chainwebPayloadDb = payloadDb
                    , _chainwebManager = mgr
                    , _chainwebPactData = pactData
                    , _chainwebThrottler = throttler
                    }

    withPactData cs cuts m
        | _enableConfigEnabled (_configTransactionIndex conf)
            = let l = sortBy (compare `on` fst) (HM.toList cs)
                  bdbs = map (\(c, cr) -> (c, _chainResBlockHeaderDb cr)) l
              in Bloom.withCache (cuts ^. cutsCutDb) bdbs $ \bloom ->
                 m $ map (\(c, cr) -> (c, (cuts, cr, bloom))) l

        | otherwise = m []

    v = _configChainwebVersion conf
    cids = chainIds v
    cwnid = _configNodeId conf

    -- FIXME: make this configurable
    cutConfig = (defaultCutDbConfig v)
        { _cutDbConfigLogLevel = Info
        , _cutDbConfigTelemetryLevel = Info
        , _cutDbConfigUseOrigin = _configIncludeOrigin conf
        }

-- | Starts server and runs all network clients
--
runChainweb
    :: forall logger cas
    . Logger logger
    => PayloadCas cas
    => Chainweb logger cas
    -> IO ()
runChainweb cw = do
    logg Info "start chainweb node"

    let cutDb = _cutResCutDb $ _chainwebCutResources cw
        cutPeerDb = _peerResDb $ _cutResPeer $ _chainwebCutResources cw

    -- Startup sequence:
    --
    -- 1. Start serving Rest API
    -- 2. Start Clients
    --

    -- collect server resources
    let chains = HM.toList (_chainwebChains cw)
        chainVals = map snd chains
        proj :: forall a . (ChainResources logger -> a) -> [(ChainId, a)]
        proj f = flip map chains $ \(k, ch) -> (k, f ch)
        chainDbsToServe = proj _chainResBlockHeaderDb
        mempoolsToServe = proj _chainResMempool
        chainP2pToServe = bimap ChainNetwork (_peerResDb . _chainResPeer) <$> itoList (_chainwebChains cw)

        payloadDbsToServe = itoList $ const (view chainwebPayloadDb cw) <$> _chainwebChains cw
        pactDbsToServe = _chainwebPactData cw

        serverSettings = peerServerSettings (_peerResPeer $ _chainwebPeer cw)
        serve = serveChainwebSocketTls
            serverSettings
            (_peerCertificateChain $ _peerResPeer $ _chainwebPeer cw)
            (_peerKey $ _peerResPeer $ _chainwebPeer cw)
            (_peerResSocket $ _chainwebPeer cw)
            (_chainwebVersion cw)
            ChainwebServerDbs
                { _chainwebServerCutDb = Just cutDb
                , _chainwebServerBlockHeaderDbs = chainDbsToServe
                , _chainwebServerMempools = mempoolsToServe
                , _chainwebServerPayloadDbs = payloadDbsToServe
                , _chainwebServerPeerDbs = (CutNetwork, cutPeerDb) : chainP2pToServe
                , _chainwebServerPactDbs = pactDbsToServe
                }

        -- HTTP Request Logger
        httpLog :: Middleware
        httpLog = requestResponseLogger $ setComponent "http" (_chainwebLogger cw)

    -- 1. start server
    --
    withAsync (serve $ throttle (_chainwebThrottler cw) . httpLog) $ \server -> do
        logg Info "started server"

        -- Configure Clients
        --
        let mgr = view chainwebManager cw
            miner = maybe [] (\m -> [ runMiner (_chainwebVersion cw) m ]) $ _chainwebMiner cw
            -- run mempool sync without manager timeout; servant default is ok
            -- for non-streaming endpoints and mempool does its own timeout
            -- bookkeeping for the streaming endpoints
            mempoolMgr = mgr { HTTP.mResponseTimeout = HTTP.responseTimeoutNone }

        -- 2. Run Clients
        --
        let clients :: [IO ()]
            clients = concat
                [ miner
                    -- FIXME: should we start mining with some delay, so
                    -- that the block header base is up to date?
                , cutNetworks mgr (_chainwebCutResources cw)
                -- , map (runChainSyncClient mgr) chainVals
                    -- TODO: reenable once full payload and adjacent parent validation
                    -- is implemented for ChainSyncClient
                , map (runMempoolSyncClient mempoolMgr) chainVals
                ]

        mapConcurrently_ id clients
        wait server
  where
    logg = logFunctionText $ _chainwebLogger cw

