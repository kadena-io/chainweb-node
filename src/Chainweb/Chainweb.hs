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
, configReintroTxs
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
, chainwebConfig

-- ** Mempool integration
, ChainwebTransaction
, Mempool.chainwebTransactionConfig

, withChainweb
, runChainweb

-- * Miner
, runMiner

) where

import Configuration.Utils hiding (Error, Lens', disabled, (<.>))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad

import Data.CAS (casLookupM)
import Data.Foldable
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import qualified Data.Text as T

import GHC.Generics hiding (from)

import qualified Network.HTTP.Client as HTTP
import Network.Socket (Socket)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Throttle

import Numeric.Natural

import Prelude hiding (log)

import System.Clock
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.MinerResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import qualified Chainweb.Mempool.InMemTypes as Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Pact.RestAPI.Server (PactServerData)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
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
    , _configReintroTxs :: !Bool
    , _configP2p :: !P2pConfiguration
    , _configTransactionIndex :: !(EnableConfig TransactionIndexConfig)
    , _configIncludeOrigin :: !Bool
    , _configThrottleRate :: !Natural
    , _configMempoolP2p :: !(EnableConfig MempoolP2pConfig)
    , _configPruneChainDatabase :: !Bool
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
    , _configReintroTxs = True
    , _configP2p = defaultP2pConfiguration
    , _configTransactionIndex = defaultEnableConfig defaultTransactionIndexConfig
    , _configIncludeOrigin = True
    , _configThrottleRate = 1000
    , _configMempoolP2p = defaultEnableConfig defaultMempoolP2pConfig
    , _configPruneChainDatabase = True
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "nodeId" .= _configNodeId o
        , "miner" .= _configMiner o
        , "reintroTxs" .= _configReintroTxs o
        , "p2p" .= _configP2p o
        , "transactionIndex" .= _configTransactionIndex o
        , "includeOrigin" .= _configIncludeOrigin o
        , "throttleRate" .= _configThrottleRate o
        , "mempoolP2p" .= _configMempoolP2p o
        , "pruneChainDatabase" .= _configPruneChainDatabase o
        ]

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configNodeId ..: "nodeId" % o
        <*< configMiner %.: "miner" % o
        <*< configReintroTxs ..: "reintroTxs" % o
        <*< configP2p %.: "p2p" % o
        <*< configTransactionIndex %.: "transactionIndex" % o
        <*< configIncludeOrigin ..: "includeOrigin" % o
        <*< configThrottleRate ..: "throttleRate" % o
        <*< configMempoolP2p %.: "mempoolP2p" % o
        <*< configPruneChainDatabase ..: "pruneChainDatabase" % o

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

    <*< configReintroTxs .:: enableDisableFlag
        % long "tx-reintro"
        <> help "whether to enable transaction reintroduction from losing forks"
    <*< configP2p %:: pP2pConfiguration Nothing
    <*< configTransactionIndex %::
        pEnableConfig "transaction-index" pTransactionIndexConfig
    <*< configIncludeOrigin .:: enableDisableFlag
        % long "include-origin"
        <> help "whether to include the local peer as origin when publishing cut hashes"
    <*< configThrottleRate .:: option auto
        % long "throttle-rate"
        <> help "how many requests per second are accepted from another node before it is being throttled"
    <*< configMempoolP2p %::
        pEnableConfig "mempool-p2p" pMempoolP2pConfig
    <*< configPruneChainDatabase .:: enableDisableFlag
        % long "prune-chain-database"
        <> help "prune the chain database for all chains on startup"

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
    , _chainwebConfig :: !ChainwebConfiguration
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
    -> Maybe FilePath
    -> Bool
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainweb c logger rocksDb dbDir resetDb inner =
    withPeerResources v (view configP2p conf) logger $ \logger' peer ->
        withChainwebInternal
            (set configP2p (_peerResConfig peer) conf)
            logger'
            peer
            rocksDb
            dbDir
            (Just (_configNodeId c))
            resetDb
            inner
  where
    v = _chainwebVersion c

    -- Here we inject the hard-coded bootstrap peer infos for the configured
    -- chainweb version into the configuration.
    conf
        | _p2pConfigIgnoreBootstrapNodes (_configP2p c) = c
        | otherwise = configP2p . p2pConfigKnownPeers <>~ bootstrapPeerInfos v $ c

-- TODO: The type InMempoolConfig contains parameters that should be
-- configurable as well as parameters that are determined by the chainweb
-- version or the chainweb protocol. These should be separated in to two
-- different types.

validatingMempoolConfig :: Mempool.InMemConfig ChainwebTransaction
validatingMempoolConfig = Mempool.InMemConfig
    Mempool.chainwebTransactionConfig
    blockGasLimit
    maxRecentLog
  where
    blockGasLimit = 100000
    maxRecentLog = 2048

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainwebInternal
    :: Logger logger
    => ChainwebConfiguration
    -> logger
    -> PeerResources logger
    -> RocksDb
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainwebInternal conf logger peer rocksDb dbDir nodeid resetDb inner = do
    initializePayloadDb v payloadDb
    cdbv <- newEmptyMVar
    concurrentWith
        -- initialize chains concurrently
        (\cid -> withChainResources v cid rocksDb peer (chainLogger cid)
                     validatingMempoolConfig cdbv payloadDb prune dbDir nodeid
                     resetDb)

        -- initialize global resources after all chain resources are initialized
        (\cs -> global (HM.fromList $ zip cidsList cs) cdbv)
        cidsList

  where
    prune = _configPruneChainDatabase conf
    cidsList = toList cids
    payloadDb = newPayloadDb rocksDb
    chainLogger cid = addLabel ("chain", toText cid) logger
    logg = logFunctionText logger

    -- Initialize global resources
    global cs cdbv = do
        let webchain = mkWebBlockHeaderDb v (HM.map _chainResBlockHeaderDb cs)
            pact = mkWebPactExecutionService (HM.map _chainResPact cs)
            cutLogger = setComponent "cut" logger
            mgr = _peerResManager peer
        logg Info "start initializing cut resources"
        withCutResources cutConfig peer cutLogger rocksDb webchain payloadDb mgr pact $ \cuts -> do
            logg Info "finished initializing cut resources"
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
            void $! putMVar cdbv mCutDb

            -- synchronize pact dbs with latest cut before we begin mining
            logg Info "start synchronizing Pact DBs"
            synchronizePactDb cs mCutDb
            logg Info "finished synchronizing Pact DBs"

            withPactData cs cuts $ \pactData -> do
                logg Info "start initializing miner resources"
                withMinerResources mLogger mConf cwnid mCutDb $ \m -> do
                    logg Info "finished initializing miner resources"
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
                        , _chainwebConfig = conf
                        }

    withPactData cs cuts m
        | _enableConfigEnabled (_configTransactionIndex conf) = do
              -- TODO: delete this knob
              logg Info "Transaction index enabled"
              let l = sortBy (compare `on` fst) (HM.toList cs)
              m $ map (\(c, cr) -> (c, (cuts, cr))) l

        | otherwise = do
              logg Info "Transaction index disabled"
              m []

    v = _configChainwebVersion conf
    cids = chainIds v
    cwnid = _configNodeId conf

    -- FIXME: make this configurable
    cutConfig = (defaultCutDbConfig v)
        { _cutDbConfigLogLevel = Info
        , _cutDbConfigTelemetryLevel = Info
        , _cutDbConfigUseOrigin = _configIncludeOrigin conf
        }

    synchronizePactDb cs cutDb = do
        currentCut <- _cut cutDb
        mapConcurrently_ syncOne $ mergeCutResources $ _cutMap currentCut
      where
        mergeCutResources c =
            let f cid bh = (bh, fromJuste $ HM.lookup cid cs)
            in map snd $ HM.toList $ HM.mapWithKey f c
        syncOne (bh, cr) = do
            let pact = _chainResPact cr
            let logCr = logFunctionText $ _chainResLogger cr
            let hsh = _blockHash bh
            let h = _blockHeight bh
            logCr Info $ "pact db synchronizing to block "
                      <> T.pack (show (h, hsh))
            payload <- payloadWithOutputsToPayloadData
                       <$> casLookupM payloadDb (_blockPayloadHash bh)
            void $ _pactValidateBlock pact bh payload
            logCr Info "pact db synchronized"


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
    concurrently_
        -- 1. Start serving Rest API
        (serve (throttle (_chainwebThrottler cw) . httpLog))
        -- 2. Start Clients (with a delay of 500ms)
        (threadDelay 500000 >> clients)
  where
    clients = do
        mpClients <- mempoolSyncClients
        mapConcurrently_ id $ concat
              [ miner
              , cutNetworks mgr (_chainwebCutResources cw)
              , mpClients
              ]

    logg = logFunctionText $ _chainwebLogger cw

    -- chains
    chains = HM.toList (_chainwebChains cw)
    chainVals = map snd chains

    -- collect server resources
    proj :: forall a . (ChainResources logger -> a) -> [(ChainId, a)]
    proj f = flip map chains $ \(k, ch) -> (k, f ch)

    chainDbsToServe = proj _chainResBlockHeaderDb
    mempoolsToServe = proj _chainResMempool
    chainP2pToServe = bimap ChainNetwork (_peerResDb . _chainResPeer) <$> itoList (_chainwebChains cw)
    memP2pToServe = bimap MempoolNetwork (_peerResDb . _chainResPeer) <$> itoList (_chainwebChains cw)

    payloadDbsToServe = itoList $ const (view chainwebPayloadDb cw) <$> _chainwebChains cw
    pactDbsToServe = _chainwebPactData cw

    serverSettings
        = setOnException
            (\r e -> when (defaultShouldDisplayException e) (logg Error $ loggServerError r e))
        $ peerServerSettings (_peerResPeer $ _chainwebPeer cw)

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
            , _chainwebServerPeerDbs = (CutNetwork, cutPeerDb) : chainP2pToServe <> memP2pToServe
            , _chainwebServerPactDbs = pactDbsToServe
            }

    -- HTTP Request Logger

    httpLog :: Middleware
    httpLog = requestResponseLogger $ setComponent "http" (_chainwebLogger cw)

    loggServerError (Just r) e = "HTTP server error: " <> sshow e <> ". Request: " <> sshow r
    loggServerError Nothing e = "HTTP server error: " <> sshow e

    -- Cut DB and Miner

    cutDb = _cutResCutDb $ _chainwebCutResources cw
    cutPeerDb = _peerResDb $ _cutResPeer $ _chainwebCutResources cw

    miner = maybe [] (\m -> [ runMiner (_chainwebVersion cw) m ]) $ _chainwebMiner cw

    -- Configure Clients

    mgr = view chainwebManager cw

    -- Mempool

    mempoolP2pConfig = _configMempoolP2p $ _chainwebConfig cw

    -- Decide whether to enable the mempool sync clients
    mempoolSyncClients = case enabledConfig mempoolP2pConfig of
      Nothing -> disabled
      Just c -> case _chainwebVersion cw of
        Test{} -> disabled
        TimedConsensus{} -> disabled
        PowConsensus{} -> disabled
        TimedCPM{} -> enabled c
        Development -> enabled c
        Testnet02 -> enabled c
      where
        disabled = do
          logg Info "Mempool p2p sync disabled"
          return []
        enabled conf = do
          logg Info "Mempool p2p sync enabled"
          return $ map (runMempoolSyncClient mgr conf) chainVals
