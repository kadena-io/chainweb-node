{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
 {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module: Standalone
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Standalone where

import Configuration.Utils hiding (disabled, Error)

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Managed

import Data.CAS.RocksDB
import Data.LogMessage
import Data.PQueue
import Data.Typeable
import qualified Data.TaskMap as TM
import qualified Data.Text as T

import GHC.Stats
import GHC.Generics hiding (to)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import Numeric.Natural

import PkgInfo

import qualified Streaming.Prelude as S

import System.Directory
import qualified System.Logger as L
import System.LogLevel

import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

-- chainweb imports
import Chainweb.Chainweb
import Chainweb.Chainweb.CutResources
import Chainweb.Counter
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Logging.Amberdata
import Chainweb.Logging.Config
import Chainweb.Logging.Miner
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore.Types
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Standalone.Chainweb
import Standalone.Mining

-- -------------------------------------------------------------------------- --
-- Monitors

-- | Run a monitor function with a logger forever. If the monitor function exist
-- or fails the event is logged and the function is restarted.
--
-- In order to prevent the function to spin in case of a persistent failure
-- cause, only 10 immediate restart are allowed. After that restart is throttled
-- to at most one restart every 10 seconds.
--
runMonitorLoop :: Logger logger => T.Text -> logger -> IO () -> IO ()
runMonitorLoop label logger = runForeverThrottled
    (logFunction logger)
    label
    10 -- 10 bursts in case of failure
    (10 * mega) -- allow restart every 10 seconds in case of failure

runCutMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runCutMonitor logger db = L.withLoggerLabel ("component", "cut-monitor") logger $ \l ->
    runMonitorLoop "ChainwebNode.runCutMonitor" l $ do
        logFunctionText l Info $ "Initialized Cut Monitor"
        S.mapM_ (logFunctionJson l Info)
            $ S.map (cutToCutHashes Nothing)
            $ cutStream db

{-
runAmberdataBlockMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runAmberdataBlockMonitor logger db
    = L.withLoggerLabel ("component", "amberdata-block-monitor") logger $ \l ->
        runMonitorLoop "Chainweb.Logging.amberdataBlockMonitor" l (amberdataBlockMonitor l db)

-}

-- type CutLog = HM.HashMap ChainId (ObjectEncoded BlockHeader)

-- This instances are OK, since this is the "Main" module of an application
--
deriving instance Generic GCDetails
deriving instance NFData GCDetails
deriving instance ToJSON GCDetails

deriving instance Generic RTSStats
deriving instance NFData RTSStats
deriving instance ToJSON RTSStats

runRtsMonitor :: Logger logger => logger -> IO ()
runRtsMonitor logger = L.withLoggerLabel ("component", "rts-monitor") logger go
  where
    go l = getRTSStatsEnabled >>= \case
        False -> do
            logFunctionText l Warn "RTS Stats isn't enabled. Run with '+RTS -T' to enable it."
        True -> do
            logFunctionText l Info $ "Initialized RTS Monitor"
            runMonitorLoop "Chainweb.Node.runRtsMonitor" l $ do
                stats <- getRTSStats
                logFunctionText l Info $ "got stats"
                logFunctionJson logger Info stats
                logFunctionText l Info $ "logged stats"
                threadDelay 60000000 {- 1 minute -}

data QueueStats = QueueStats
    { _queueStatsCutQueueSize :: !Natural
    , _queueStatsBlockHeaderQueueSize :: !Natural
    , _queueStatsBlockHeaderTaskMapSize :: !Natural
    , _queueStatsPayloadQueueSize :: !Natural
    , _queueStatsPayloadTaskMapSize :: !Natural
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, ToJSON)

runQueueMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runQueueMonitor logger cutDb = L.withLoggerLabel ("component", "queue-monitor") logger go
  where
    go l = do
        logFunctionText l Info $ "Initialized Queue Monitor"
        runMonitorLoop "ChainwebNode.runQueueMonitor" l $ do
            stats <- QueueStats
                <$> cutDbQueueSize cutDb
                <*> pQueueSize (_webBlockHeaderStoreQueue $ view cutDbWebBlockHeaderStore cutDb)
                <*> (int <$> TM.size (_webBlockHeaderStoreMemo $ view cutDbWebBlockHeaderStore cutDb))
                <*> pQueueSize (_webBlockPayloadStoreQueue $ view cutDbPayloadStore cutDb)
                <*> (int <$> TM.size (_webBlockPayloadStoreMemo $ view cutDbPayloadStore cutDb))

            logFunctionText l Info $ "got stats"
            logFunctionJson logger Info stats
            logFunctionText l Info $ "logged stats"
            threadDelay 60000000 {- 1 minute -}

node :: Logger logger => StandaloneConfiguration -> logger -> IO ()
node conf logger = do
    rocksDbDir <- getRocksDbDir
    when (_nodeConfigResetChainDbs conf) $ destroyRocksDb rocksDbDir
    withRocksDb rocksDbDir $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory" <> sshow rocksDbDir
        withChainwebStandalone cwConf logger rocksDb (_nodeConfigDatabaseDirectory conf) (_nodeConfigResetChainDbs conf) $ \cw -> mapConcurrently_ id
            [ runChainweb' cw
            , runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            -- , runAmberdataBlockMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runQueueMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runRtsMonitor (_chainwebLogger cw)
            ]
  where
    cwConf = _nodeConfigChainweb conf
    nodeText = T.unpack (toText (_configNodeId cwConf))
    v = _configChainwebVersion cwConf
    getRocksDbDir = case _nodeConfigDatabaseDirectory conf of
      Nothing -> getXdgDirectory XdgData
            $ "chainweb-standalone-node/" <> sshow v <> "/" <> nodeText <> "/rocksDb"
      Just d -> return d

-- | Starts server and runs all network clients
--
runChainweb'
    :: forall logger cas
    . Logger logger
    => PayloadCas cas
    => Chainweb logger cas
    -> IO ()
runChainweb' cw = do
    logg Info "start chainweb node"

    -- miner
    withAsync miner wait

    -- forever (threadDelay 1000000)

{-
    -- 1. Start serving Rest API
    --
    withAsync (serve $ throttle (_chainwebThrottler cw) . httpLog) $ \_server ->
        wait _server
        -- forever (threadDelay 1000000)
        {-
        logg Info "started server"

        -- 2. Start Clients
        --
        mpClients <- mempoolSyncClients
        let clients = concat
              [ miner
              , cutNetworks mgr (_chainwebCutResources cw)
              , mpClients
              ]
        mapConcurrently_ id clients
        wait server
        -}
-}
  where
    logg = logFunctionText $ _chainwebLogger cw
    miner = maybe go (\m -> runMiner' (_chainwebVersion cw) m) $ _chainwebMiner cw
        where
          go = do
            logg Warn "No miner configured. Starting consensus without mining."
            forever (threadDelay 1000000)
    -- miner = maybe [] (\m -> [ runMiner (_chainwebVersion cw) m ]) $ _chainwebMiner cw
{-

    -- chains
    _chains = HM.toList (_chainwebChains cw)
    _chainVals = map snd _chains

    -- collect server resources
    proj :: forall a . (ChainResources logger -> a) -> [(ChainId, a)]
    proj f = flip map _chains $ \(k, ch) -> (k, f ch)

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

    -- serve = return undefined

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

    _miner = maybe [] (\m -> [ runMiner (_chainwebVersion cw) m ]) $ _chainwebMiner cw

    -- Configure Clients

    mgr = _chainwebManager cw


    -- Mempool

    _mempoolP2pConfig = _configMempoolP2p $ _chainwebConfig cw

    -- Decide whether to enable the mempool sync clients
    _mempoolSyncClients = case enabledConfig _mempoolP2pConfig of
      Nothing -> disabled
      Just c -> case _chainwebVersion cw of
        Test{} -> disabled
        TimedConsensus{} -> disabled
        PowConsensus{} -> disabled
        TimedCPM{} -> enabled c
        Development -> enabled c
        -- Testnet00 -> enabled c
        -- Testnet01 -> enabled c
        Testnet02 -> enabled c
      where
        disabled = do
          logg Info "Mempool p2p sync disabled"
          return []
        enabled conf = do
          logg Info "Mempool p2p sync enabled"
          return $ map (runMempoolSyncClient mgr conf) _chainVals
-}

data StandaloneConfiguration = StandaloneConfiguration
  { _nodeConfigChainweb :: !ChainwebConfiguration
  , _nodeConfigLog :: !LogConfig
  , _nodeConfigDatabaseDirectory :: !(Maybe FilePath)
  , _nodeConfigResetChainDbs :: !Bool
  }
  deriving (Show, Eq, Generic)

makeLenses ''StandaloneConfiguration

defaultStandaloneConfiguration :: ChainwebVersion -> StandaloneConfiguration
defaultStandaloneConfiguration v = StandaloneConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration v
    , _nodeConfigLog = defaultLogConfig
        & logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _nodeConfigDatabaseDirectory = Nothing
    , _nodeConfigResetChainDbs = False
    }

instance ToJSON StandaloneConfiguration where
    toJSON o = object
        [ "chainweb" .= _nodeConfigChainweb o
        , "logging" .= _nodeConfigLog o
        , "databaseDirectory" .= _nodeConfigDatabaseDirectory o
        , "resetChainDatabases" .= _nodeConfigResetChainDbs o
        ]

instance FromJSON (StandaloneConfiguration -> StandaloneConfiguration) where
    parseJSON = withObject "StandaloneConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb" % o
        <*< nodeConfigLog %.: "logging" % o
        <*< nodeConfigDatabaseDirectory ..: "databaseDirectory" % o
        <*< nodeConfigResetChainDbs ..: "resetChainDatabases" % o

pStandaloneConfiguration :: MParser StandaloneConfiguration
pStandaloneConfiguration = id
    <$< nodeConfigChainweb %:: pChainwebConfiguration
    <*< nodeConfigLog %:: pLogConfig
    <*< nodeConfigDatabaseDirectory .:: fmap Just % textOption
        % long "database-directory"
        <> help "directory where the databases are persisted"
    <*< nodeConfigResetChainDbs .:: enableDisableFlag
        % long "reset-chain-databases"
        <> help "Reset the chain databases for all chains on startup"


withNodeLogger
    :: LogConfig
    -> ChainwebVersion
    -> (L.Logger SomeLogMessage -> IO ())
    -> IO ()
withNodeLogger logConfig v f = runManaged $ do

    -- This manager is used only for logging backends
    mgr <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    _mgrHttps <- liftIO $ HTTPS.newTlsManager

    -- Base Backend
    baseBackend <- managed
        $ withBaseHandleBackend "ChainwebApp" mgr (_logConfigBackend logConfig)

    -- Telemetry Backends
    monitorBackend <- managed
        $ mkTelemetryLogger @CutHashes mgr teleLogConfig
    {-p2pInfoBackend <- managed
        $ mkTelemetryLogger @P2pSessionInfo mgr teleLogConfig-}
    rtsBackend <- managed
        $ mkTelemetryLogger @RTSStats mgr teleLogConfig
    counterBackend <- managed $ configureHandler
        (withJsonHandleBackend @CounterLog "connectioncounters" mgr)
        teleLogConfig
    {-newBlockAmberdataBackend <- managed $ mkAmberdataLogger mgrHttps amberdataConfig-}
    newBlockBackend <- managed
        $ mkTelemetryLogger @NewMinedBlock mgr teleLogConfig
    {- requestLogBackend <- managed
        $ mkTelemetryLogger @RequestResponseLog mgr teleLogConfig -}
    queueStatsBackend <- managed
        $ mkTelemetryLogger @QueueStats mgr teleLogConfig
    {-reintroBackend <- managed
        $ mkTelemetryLogger @ReintroducedTxsLog mgr teleLogConfig-}
    traceBackend <- managed
        $ mkTelemetryLogger @Trace mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logConfig) $ logHandles
            [ logHandler monitorBackend
            -- , logHandler p2pInfoBackend
            , logHandler rtsBackend
            , logHandler counterBackend
            -- , logHandler newBlockAmberdataBackend
            , logHandler newBlockBackend
            -- , logHandler requestLogBackend
            , logHandler queueStatsBackend
            -- , logHandler reintroBackend
            , logHandler traceBackend
            ] baseBackend

    liftIO $ f
        $ maybe id (\x -> addLabel ("cluster", toText x)) (_logConfigClusterId logConfig)
        $ addLabel ("chainwebVersion", sshow v)
        $ logger
  where
    teleLogConfig = _logConfigTelemetryBackend logConfig
    _amberdataConfig = _logConfigAmberdataBackend logConfig

mkAmberdataLogger
    :: HTTP.Manager
    -> Maybe AmberdataConfig
    -> (Backend (JsonLog AmberdataBlock) -> IO b)
    -> IO b
mkAmberdataLogger _ Nothing inner = inner (const $ return ())
mkAmberdataLogger mgr (Just config) inner = withAmberDataBlocksBackend mgr config inner

mkTelemetryLogger
    :: forall a b
    . Typeable a
    => ToJSON a
    => HTTP.Manager
    -> EnableConfig BackendConfig
    -> (Backend (JsonLog a) -> IO b)
    -> IO b
mkTelemetryLogger mgr = configureHandler
    $ withJsonHandleBackend @(JsonLog a) (sshow $ typeRep $ Proxy @a) mgr

-- -------------------------------------------------------------------------- --
-- main

mainInfo :: ProgramInfo StandaloneConfiguration
mainInfo = programInfo
    "Chainweb Node"
    pStandaloneConfiguration
    (defaultStandaloneConfiguration Testnet02)

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
    let v = _configChainwebVersion $ _nodeConfigChainweb conf
    withNodeLogger (_nodeConfigLog conf) v $ node conf
