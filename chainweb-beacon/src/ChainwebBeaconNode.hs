{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: BeaconNode
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module BeaconNode
( main
) where

import Configuration.Utils hiding (Error)
import Configuration.Utils.Validation (validateFilePath)

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Managed

import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Typeable

import GHC.Generics hiding (from)
import GHC.Stack
import GHC.Stats

import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS

import Streaming.Prelude qualified as S

import System.Directory
import System.FilePath
import System.IO
import System.Logger qualified as L
import System.LogLevel
import System.Mem

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Chainweb
import Chainweb.Beacon.Configuration
import Chainweb.Chainweb.CutResources
import Chainweb.Counter
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Logging.Config
import Chainweb.Logging.Miner
import Chainweb.Miner.Coordinator (MiningStats)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Data.Time.Format.ISO8601
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet04 (testnet04)
import Chainweb.Version.Registry

import Chainweb.Storage.Table.RocksDB

import Data.LogMessage

import P2P.Node

import PkgInfo

import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

import Utils.CheckRLimits
import Utils.InstallSignalHandlers
import Chainweb.Chainweb.PeerResources
import Chainweb.HostAddress

-- -------------------------------------------------------------------------- --
-- Beacon Chain Configuration

data BeaconNodeConfiguration = BeaconNodeConfiguration
    { _nodeConfigChainweb :: !BeaconConfiguration
    , _nodeConfigLog :: !LogConfig
    , _nodeConfigDatabaseDirectory :: !(Maybe FilePath)
    , _nodeConfigResetChainDbs :: !Bool
    }
    deriving (Show, Eq, Generic)

makeLenses ''BeaconNodeConfiguration

defaultBeaconNodeConfiguration :: BeaconNodeConfiguration
defaultBeaconNodeConfiguration = BeaconNodeConfiguration
    { _nodeConfigChainweb = defaultBeaconConfiguration Mainnet01
    , _nodeConfigLog = defaultLogConfig
        & logConfigLogger . L.loggerConfigThreshold .~ level
    , _nodeConfigDatabaseDirectory = Nothing
    , _nodeConfigResetChainDbs = False
    }
  where
    level = L.Info

validateBeaconNodeConfiguration :: ConfigValidation BeaconNodeConfiguration []
validateBeaconNodeConfiguration o = do
    validateLogConfig $ _nodeConfigLog o
    validateBeaconConfiguration $ _nodeConfigChainweb o
    mapM_ (validateFilePath "databaseDirectory") (_nodeConfigDatabaseDirectory o)

instance ToJSON BeaconNodeConfiguration where
    toJSON o = object
        [ "chainweb-beacon" .= _nodeConfigChainweb o
        , "logging" .= _nodeConfigLog o
        , "databaseDirectory" .= _nodeConfigDatabaseDirectory o
        , "resetChainDatabases" .= _nodeConfigResetChainDbs o
        ]

instance FromJSON (BeaconNodeConfiguration -> BeaconNodeConfiguration) where
    parseJSON = withObject "ChainwebNodeConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb-beacon" % o
        <*< nodeConfigLog %.: "logging" % o
        <*< nodeConfigDatabaseDirectory ..: "databaseDirectory" % o
        <*< nodeConfigResetChainDbs ..: "resetChainDatabases" % o

pBeaconNodeConfiguration :: MParser BeaconNodeConfiguration
pBeaconNodeConfiguration = id
    <$< nodeConfigChainweb %:: pBeaconConfiguration
    <*< nodeConfigLog %:: pLogConfig
    <*< nodeConfigDatabaseDirectory .:: fmap Just % textOption
        % long "database-directory"
        <> help "directory where the databases are persisted"
    <*< nodeConfigResetChainDbs .:: enableDisableFlag
        % long "reset-chain-databases"
        <> help "Reset the chain databases for all chains on startup"

getRocksDbDir :: HasCallStack => BeaconNodeConfiguration -> IO FilePath
getRocksDbDir conf = (\base -> base </> "0" </> "rocksDb") <$> getDbBaseDir conf

getBackupsDir :: HasCallStack => BeaconNodeConfiguration -> IO FilePath
getBackupsDir conf = (</> "backups") <$> getDbBaseDir conf

getDbBaseDir :: HasCallStack => BeaconNodeConfiguration -> IO FilePath
getDbBaseDir conf = case _nodeConfigDatabaseDirectory conf of
    Nothing -> getXdgDirectory XdgData
        $ "chainweb-node" </> sshow (_versionName v)
    Just d -> return d
  where
    v = _configChainwebVersion $ _nodeConfigChainweb conf

-- -------------------------------------------------------------------------- --
-- Monitors

-- | Run a monitor function with a logger forever. If the monitor function exist
-- or fails the event is logged and the function is restarted.
--
-- In order to prevent the function to spin in case of a persistent failure
-- cause, only 10 immediate restart are allowed. After that restart is throttled
-- to at most one restart every 10 seconds.
--
runMonitorLoop :: Logger logger => Text -> logger -> IO () -> IO ()
runMonitorLoop actionLabel logger = runForeverThrottled
    (logFunction logger)
    actionLabel
    10 -- 10 bursts in case of failure
    (10 * mega) -- allow restart every 10 seconds in case of failure

runCutMonitor :: Logger logger => logger -> CutDb tbl -> IO ()
runCutMonitor logger db = L.withLoggerLabel ("component", "cut-monitor") logger $ \l ->
    runMonitorLoop "ChainwebNode.runCutMonitor" l $ do
        logFunctionJson l Info . cutToCutHashes Nothing
            =<< _cut db
        threadDelay 15_000_000

data BlockUpdate = BlockUpdate
    { _blockUpdateBlockHeader :: !(ObjectEncoded BlockHeader)
    , _blockUpdateOrphaned :: !Bool
    , _blockUpdateTxCount :: !Int
    }
    deriving (Show, Eq, Ord, Generic, NFData)

instance ToJSON BlockUpdate where
    toEncoding o = pairs
        $ "header" .= _blockUpdateBlockHeader o
        <> "orphaned" .= _blockUpdateOrphaned o
        <> "txCount" .= _blockUpdateTxCount o
    toJSON o = object
        [ "header" .= _blockUpdateBlockHeader o
        , "orphaned" .= _blockUpdateOrphaned o
        , "txCount" .= _blockUpdateTxCount o
        ]

    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

runBlockUpdateMonitor :: CanReadablePayloadCas tbl => Logger logger => logger -> CutDb tbl -> IO ()
runBlockUpdateMonitor logger db = L.withLoggerLabel ("component", "block-update-monitor") logger $ \l ->
    runMonitorLoop "ChainwebNode.runBlockUpdateMonitor" l $ do
        blockDiffStream db
            & S.mapM toUpdate
            & S.mapM_ (logFunctionJson l Info)
  where
    payloadDb = view cutDbPayloadDb db

    txCount :: BlockHeader -> IO Int
    txCount bh = do
        bp <- lookupPayloadDataWithHeight payloadDb (Just $ view blockHeight bh) (view blockPayloadHash bh) >>= \case
            Nothing -> error "block payload not found"
            Just x -> return x
        return $ length $ view payloadDataTransactions bp

    toUpdate :: Either BlockHeader BlockHeader -> IO BlockUpdate
    toUpdate (Right bh) = BlockUpdate
        <$> pure (ObjectEncoded bh) -- _blockUpdateBlockHeader
        <*> pure False -- _blockUpdateOrphaned
        <*> txCount bh -- _blockUpdateTxCount
    toUpdate (Left bh) = BlockUpdate
        <$> pure (ObjectEncoded bh) -- _blockUpdateBlockHeader
        <*> pure True -- _blockUpdateOrphaned
        <*> ((0 -) <$> txCount bh) -- _blockUpdateTxCount

-- type CutLog = HM.HashMap ChainId (ObjectEncoded BlockHeader)

-- This instances are OK, since this is the "Main" module of an application
--
deriving instance NFData GCDetails
deriving instance NFData RTSStats

deriving instance ToJSON GCDetails
deriving instance ToJSON RTSStats

runRtsMonitor :: Logger logger => logger -> IO ()
runRtsMonitor logger = L.withLoggerLabel ("component", "rts-monitor") logger go
  where
    go l = getRTSStatsEnabled >>= \case
        False -> do
            logFunctionText l Warn "RTS Stats isn't enabled. Run with '+RTS -T' to enable it."
        True -> do
            runMonitorLoop "Chainweb.Node.runRtsMonitor" l $ do
                logFunctionText l Debug $ "logging RTS stats"
                stats <- getRTSStats
                logFunctionJson logger Info stats
                approximateThreadDelay 60_000_000 {- 1 minute -}

runQueueMonitor :: Logger logger => logger -> CutDb tbl -> IO ()
runQueueMonitor logger cutDb = L.withLoggerLabel ("component", "queue-monitor") logger go
  where
    go l = do
        runMonitorLoop "ChainwebNode.runQueueMonitor" l $ do
            logFunctionText l Debug $ "logging cut queue stats"
            stats <- getQueueStats cutDb
            logFunctionJson logger Info stats
            approximateThreadDelay 60_000_000 {- 1 minute -}

data DbStats = DbStats
    { dbStatsName :: !Text
    , dbStatsSize :: !Integer
    } deriving (Generic, NFData, ToJSON)

runDatabaseMonitor :: Logger logger => logger -> FilePath -> IO ()
runDatabaseMonitor logger rocksDbDir = L.withLoggerLabel ("component", "database-monitor") logger go
  where
    go l = do
        runMonitorLoop "ChainwebNode.runDatabaseMonitor" l $ do
            logFunctionText l Debug $ "logging database stats"
            logFunctionJson l Info . DbStats "rocksDb" =<< sizeOf rocksDbDir
            approximateThreadDelay 1_200_000_000 {- 20 minutes -}
    sizeOf path = do
        dir <- doesDirectoryExist path
        file <- doesFileExist path
        if dir then
            fmap sum . traverse (sizeOf . (path </>)) =<< listDirectory path
        else if file then
            getFileSize path
        else
            pure 0

-- -------------------------------------------------------------------------- --
--

-- Intializes all service API chainweb components but doesn't start any networking.
--
withBeacon
    :: forall logger
    . Logger logger
    => BeaconConfiguration
    -> logger
    -> RocksDb
    -> (StartedChainweb logger -> IO ())
    -> IO ()
withBeacon c logger rocksDb inner =
    withPeerResources v (view configP2p confWithBootstraps) logger $ \logger' peer ->
        withSocket serviceApiPort serviceApiHost $ \serviceSock -> do
            let conf' = confWithBootstraps
                    & set configP2p (_peerResConfig peer)
                    & set (configServiceApi . serviceApiConfigPort) (fst serviceSock)
            withBeaconInternal
                conf'
                logger'
                peer
                serviceSock
                rocksDb
                inner
  where
    serviceApiPort = _serviceApiConfigPort $ _configServiceApi c
    serviceApiHost = _serviceApiConfigInterface $ _configServiceApi c

    v = _chainwebVersion c

    -- Here we inject the hard-coded bootstrap peer infos for the configured
    -- chainweb version into the configuration.
    confWithBootstraps
        | _p2pConfigIgnoreBootstrapNodes (_configP2p c) = c
        | otherwise = configP2p . p2pConfigKnownPeers
            %~ (\x -> L.nub $ x <> _versionBootstraps v) $ c

-- Intializes all service chainweb components but doesn't start any networking.
--
withBeaconInternal
    :: forall logger
    .  Logger logger
    => BeaconConfiguration
    -> logger
    -> PeerResources logger
    -> (Port, _)
    -> RocksDb
    -> (StartedChainweb logger -> IO ())
    -> IO ()
withBeaconInternal conf logger peer serviceSock rocksDb inner = do

    initializePayloadDb v payloadDb

    logFunctionJson logger Info InitializingChainResources

    logg Debug "start initializing chain resources"
    logFunctionText logger Info $ "opening pact db in directory " <> sshow pactDbDir

    concurrentWith
        -- initialize chains concurrently
        (\cid x -> withChainResources
            v
            cid
            rocksDb
            (chainLogger cid)
            mcfg
            payloadDb
            x
        )

        -- initialize global resources after all chain resources are initialized
        (\cs -> do
            logg Debug "finished initializing chain resources"
            global (HM.fromList $ zip cidsList cs)
        )
        cidsList
  where
    cidsList :: [ChainId]
    cidsList = toList cids

    payloadDb :: PayloadDb RocksDbTable
    payloadDb = newPayloadDb rocksDb

    chainLogger :: ChainId -> logger
    chainLogger cid = addLabel ("chain", toText cid) logger

    initLogger :: logger
    initLogger = setComponent "init" logger

    logg :: LogFunctionText
    logg = logFunctionText initLogger

    -- Initialize global resources
    global
        :: HM.HashMap ChainId (ChainResources logger)
        -> IO ()
    global cs = do
        let !webchain = mkWebBlockHeaderDb v (HM.map _chainResBlockHeaderDb cs)

            -- TODO
            !payloadProviders = mkWebPayloadExecutionService (HM.map _chainResPayloadProvider cs)

            !cutLogger = setComponent "cut" logger
            !mgr = _peerResManager peer

        logg Debug "start initializing cut resources"
        logFunctionJson logger Info InitializingCutResources

        withCutResources cutConfig peer cutLogger rocksDb webchain payloadDb mgr payloadProviders $ \cuts -> do
            logg Debug "finished initializing cut resources"

            let !mLogger = setComponent "miner" logger
                !mConf = _configMining conf
                !mCutDb = _cutResCutDb cuts
                !throt  = _configP2pThrottling conf

            -- initialize throttler
            throttler <- mkGenericThrottler $ _throttlingRate throt
            putPeerThrottler <- mkPutPeerThrottler $ _throttlingPeerRate throt
            logg Debug "initialized throttlers"

            -- synchronize PayloadProviders with latest cut before we start the server
            -- and clients and begin mining.
            --
            -- This is a consistency check that validates the blocks in the
            -- current cut. If it fails an exception is raised. Also, if it
            -- takes long (for example, when doing a reset to a prior block
            -- height) we want this to happen before we go online.
            --
            initialCut <- _cut mCutDb
            logg Info "start synchronizing payload providers to initial cut"
            logFunctionJson logger Info InitialSyncInProgress
            synchronizePactDb cs initialCut
            logg Info "finished synchronizing payload providers to initial cut"
            withPayloadData cs cuts $ \payloadData -> do
                logg Debug "start initializing miner resources"
                logFunctionJson logger Info InitializingMinerResources

                withMiningCoordination mLogger mConf mCutDb $ \mc ->

                    -- Miner resources are used by the test-miner when in-node
                    -- mining is configured or by the mempool noop-miner (which
                    -- keeps the mempool updated) in production setups.
                    --
                    withMinerResources mLogger (_miningInNode mConf) cs mCutDb mc $ \m -> do
                        logFunctionJson logger Info ChainwebStarted
                        logg Debug "finished initializing miner resources"
                        let !haddr = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
                        inner $ StartedChainweb Chainweb
                            { _chainwebHostAddress = haddr
                            , _chainwebChains = cs
                            , _chainwebCutResources = cuts
                            , _chainwebMiner = m
                            , _chainwebCoordinator = mc
                            , _chainwebLogger = logger
                            , _chainwebPeer = peer
                            , _chainwebPayloadDb = view cutDbPayloadDb $ _cutResCutDb cuts
                            , _chainwebManager = mgr
                            , _chainwebPayloadData = pactData
                            , _chainwebGlobalThrottler = throttler
                            , _chainwebPutPeerThrottler = putPeerThrottler
                            , _chainwebConfig = conf
                            , _chainwebServiceSocket = serviceSock
                            }

    withPayloadData
        :: HM.HashMap ChainId (ChainResources logger)
        -> CutResources logger tbl
        -> ([(ChainId, PactServerData logger tbl)] -> IO b)
        -> IO b
    withPayloadData cs cuts m = do
        let l = sortBy (compare `on` fst) (HM.toList cs)
        m $ l <&> fmap (\cr -> PactServerData
            { _pactServerDataCutDb = _cutResCutDb cuts
            , _pactServerDataLogger = _chainResLogger cr
            , _pactServerDataPayloadProvider = _chainResPayloadProvider cr
            })

    v = _configChainwebVersion conf
    cids = chainIds v

    -- FIXME: make this configurable
    cutConfig :: CutDbParams
    cutConfig = (defaultCutDbParams v $ _cutFetchTimeout cutConf)
        { _cutDbParamsLogLevel = Info
        , _cutDbParamsTelemetryLevel = Info
        , _cutDbParamsInitialHeightLimit = _cutInitialBlockHeightLimit cutConf
        , _cutDbParamsFastForwardHeightLimit = _cutFastForwardBlockHeightLimit cutConf
        , _cutDbParamsReadOnly = _configOnlySyncPact conf || _configReadOnlyReplay conf
        }
      where
        cutConf = _configCuts conf

    synchronizePayloadProvider :: HM.HashMap ChainId (ChainResources logger) -> Cut -> IO ()
    synchronizePayloadProvider cs targetCut = do
        mapConcurrently_ syncOne $
            HM.intersectionWith (,) (_cutMap targetCut) cs
      where
        syncOne :: (BlockHeader, ChainResources logger) -> IO ()
        syncOne (bh, cr) = do
            let provider = _chainResPayloadProvider cr
            let logCr = logFunctionText
                    $ addLabel ("component", "pact")
                    $ addLabel ("sub-component", "init")
                    $ _chainResLogger cr
            void $ _syncToBlock provider bh
            logCr Debug "payload provider synchronized"


-- -------------------------------------------------------------------------- --
-- Run Node

node :: HasCallStack => Logger logger => BeaconNodeConfiguration -> logger -> IO ()
node conf logger = do
    dbBaseDir <- getDbBaseDir conf
    when (_nodeConfigResetChainDbs conf) $ removeDirectoryRecursive dbBaseDir
    rocksDbDir <- getRocksDbDir conf
    dbBackupsDir <- getBackupsDir conf
    withRocksDb rocksDbDir modernDefaultOptions $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory " <> sshow rocksDbDir
        withChainweb cwConf logger rocksDb $ \case
            Replayed _ _ -> return ()
            StartedChainweb cw -> do
                let telemetryEnabled =
                        _enableConfigEnabled $ _logConfigTelemetryBackend $ _nodeConfigLog conf
                concurrentlies_
                    [ runChainweb cw (\_ -> return ())
                    -- we should probably push 'onReady' deeper here but this should be ok
                    , when telemetryEnabled $
                        runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                    , when telemetryEnabled $
                        runQueueMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                    , when telemetryEnabled $
                        runRtsMonitor (_chainwebLogger cw)
                    , when telemetryEnabled $
                        runBlockUpdateMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                    , when telemetryEnabled $
                        runDatabaseMonitor (_chainwebLogger cw) rocksDbDir
                    ]
  where
    cwConf = _nodeConfigChainweb conf

withNodeLogger
    :: LogConfig
    -> BeaconConfiguration
    -> ChainwebVersion
    -> (L.Logger SomeLogMessage -> IO ())
    -> IO ()
withNodeLogger logCfg chainwebCfg v f = runManaged $ do

    -- This manager is used only for logging backends
    mgr <- liftIO HTTPS.newTlsManager

    -- Base Backend
    baseBackend <- managed
        $ withBaseHandleBackend "ChainwebApp" mgr pkgInfoScopes (_logConfigBackend logCfg)

    -- Telemetry Backends
    monitorBackend <- managed
        $ mkTelemetryLogger @CutHashes mgr teleLogConfig
    p2pInfoBackend <- managed
        $ mkTelemetryLogger @P2pSessionInfo mgr teleLogConfig
    rtsBackend <- managed
        $ mkTelemetryLogger @RTSStats mgr teleLogConfig
    counterBackend <- managed $ configureHandler
        (withJsonHandleBackend @CounterLog "counters" mgr pkgInfoScopes)
        teleLogConfig
    newBlockBackend <- managed
        $ mkTelemetryLogger @NewMinedBlock mgr teleLogConfig
    orphanedBlockBackend <- managed
        $ mkTelemetryLogger @OrphanedBlock mgr teleLogConfig
    miningStatsBackend <- managed
        $ mkTelemetryLogger @MiningStats mgr teleLogConfig
    requestLogBackend <- managed
        $ mkTelemetryLogger @RequestResponseLog mgr teleLogConfig
    queueStatsBackend <- managed
        $ mkTelemetryLogger @QueueStats mgr teleLogConfig
    traceBackend <- managed
        $ mkTelemetryLogger @Trace mgr teleLogConfig
    blockUpdateBackend <- managed
        $ mkTelemetryLogger @BlockUpdate mgr teleLogConfig
    dbStatsBackend <- managed
        $ mkTelemetryLogger @DbStats mgr teleLogConfig
    p2pNodeStatsBackend <- managed
        $ mkTelemetryLogger @P2pNodeStats mgr teleLogConfig
    topLevelStatusBackend <- managed
        $ mkTelemetryLogger @ChainwebStatus mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logCfg) $ logHandles
            [ logFilterHandle (_logConfigFilter logCfg)
            , logHandler monitorBackend
            , logHandler p2pInfoBackend
            , logHandler rtsBackend
            , logHandler counterBackend
            , logHandler newBlockBackend
            , logHandler orphanedBlockBackend
            , logHandler miningStatsBackend
            , logHandler requestLogBackend
            , logHandler queueStatsBackend
            , logHandler traceBackend
            , logHandler blockUpdateBackend
            , logHandler dbStatsBackend
            , logHandler p2pNodeStatsBackend
            , logHandler topLevelStatusBackend
            ] baseBackend

    liftIO $ f
        $ maybe id (\x -> addLabel ("cluster", toText x)) (_logConfigClusterId logCfg)
        $ addLabel ("chainwebVersion", sshow (_versionName v))
        $ logger
  where
    teleLogConfig = _logConfigTelemetryBackend logCfg

mkTelemetryLogger
    :: forall a b
    . Typeable a
    => ToJSON a
    => HTTP.Manager
    -> EnableConfig BackendConfig
    -> (Backend (JsonLog a) -> IO b)
    -> IO b
mkTelemetryLogger mgr = configureHandler
    $ withJsonHandleBackend @a (sshow $ typeRep $ Proxy @a) mgr pkgInfoScopes

-- -------------------------------------------------------------------------- --
-- Service Date

newtype ServiceDate = ServiceDate Text

instance Show ServiceDate where
    show (ServiceDate t) = "Service interval end: " <> T.unpack t

instance Exception ServiceDate where
    fromException = asyncExceptionFromException
    toException = asyncExceptionToException

withServiceDate
    :: ChainwebVersion
    -> (LogLevel -> Text -> IO ())
    -> Maybe UTCTime
    -> IO a
    -> IO a
withServiceDate v lf msd inner = case msd of
  Nothing -> do
    inner
  Just sd -> do
    if _versionCode v == _versionCode mainnet || _versionCode v == _versionCode testnet
    then do
      race (timer sd) inner >>= \case
        Left () -> error "Service date thread terminated unexpectedly"
        Right a -> return a
    else do
      inner
  where
    timer t = runForever lf "ServiceDate" $ do
      now <- getCurrentTime
      when (now >= t) $ do
        lf Error shutdownMessage
        throw $ ServiceDate shutdownMessage

      let w = diffUTCTime t now
      let micros = round $ w * 1_000_000
      lf Warn warning
      threadDelay $ min (10 * 60 * 1_000_000) micros

      where
        warning :: Text
        warning = T.concat
          [ "This version of chainweb node will stop working at " <> sshow t <> "."
          , " Please upgrade to a new version before that date."
          ]

        shutdownMessage :: Text
        shutdownMessage = T.concat
          [ "Shutting down. This version of chainweb was only valid until" <> sshow t <> "."
          , " Please upgrade to a new version."
          ]

-- -------------------------------------------------------------------------- --
-- Encode Package Info into Log mesage scopes

pkgInfoScopes :: [(Text, Text)]
pkgInfoScopes =
    [ ("revision", revision)
    , ("branch", branch)
    , ("compiler", compiler)
    , ("optimisation", optimisation)
    , ("architecture", arch)
    , ("package", package)
    ]

-- -------------------------------------------------------------------------- --
-- main

mainInfo :: ProgramInfo BeaconNodeConfiguration
mainInfo = programInfoValidate
    "Chainweb Beacon Node"
    pBeaconNodeConfiguration
    defaultBeaconNodeConfiguration
    validateBeaconNodeConfiguration

handles :: [Handler a] -> IO a -> IO a
handles = flip catches

main :: IO ()
main = do
    installFatalSignalHandlers [ sigHUP, sigTERM, sigXCPU, sigXFSZ ]
    hSetBuffering stderr LineBuffering
    checkRLimits
    runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
        let v = _configChainwebVersion $ _nodeConfigBeacon conf
        registerVersion v
        withNodeLogger (_nodeConfigLog conf) (_nodeConfigBeacon conf) v $ \logger -> do
            logFunctionJson logger Info ProcessStarted
            handles
                [ Handler $ \(e :: SomeAsyncException) ->
                    logFunctionJson logger Info (ProcessDied $ show e) >> throwIO e
                , Handler $ \(e :: SomeException) ->
                    logFunctionJson logger Error (ProcessDied $ show e) >> throwIO e
                ] $ do
                kt <- mapM iso8601ParseM (_versionServiceDate v)
                withServiceDate (_configChainwebVersion (_nodeConfigChainweb conf)) (logFunctionText logger) kt
                  $ void
                    $ race (node conf logger) (gcRunner (logFunctionText logger))
    where
    gcRunner lf = runForever lf "GarbageCollect" $ do
        performMajorGC
        threadDelay (30 * 1_000_000)
