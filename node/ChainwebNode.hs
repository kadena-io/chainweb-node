{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: ChainwebNode
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
(
-- * Configuration
  ChainwebNodeConfiguration(..)

-- * Monitor
, runCutMonitor
, runRtsMonitor

-- * Chainweb Node
, node
, withNodeLogger

-- * Main function
, main
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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable

import GHC.Generics hiding (from)
import GHC.Stack
import GHC.Stats

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import qualified Streaming.Prelude as S

import System.Directory
import System.FilePath
import System.IO
import qualified System.Logger as L
import System.LogLevel
import System.Mem

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Chainweb
import Chainweb.Chainweb.Configuration
import Chainweb.Chainweb.CutResources
import Chainweb.Counter
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Logging.Config
import Chainweb.Logging.Miner
import Chainweb.Mempool.Consensus (ReintroducedTxsLog)
import Chainweb.Mempool.InMemTypes (MempoolStats(..))
import Chainweb.Miner.Coordinator (MiningStats)
import Chainweb.Pact.Backend.DbCache (DbCacheStats)
import Chainweb.Pact.Service.PactQueue (PactQueueStats)
import Chainweb.Pact.RestAPI.Server (PactCmdLog(..))
import Chainweb.Pact.Types(TxFailureLog)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Data.Time.Format.ISO8601
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.Version.Mainnet
import Chainweb.Version.Registry

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

import Data.LogMessage

import P2P.Node

import PkgInfo

import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

import Utils.CheckRLimits
import Utils.InstallSignalHandlers

-- -------------------------------------------------------------------------- --
-- Configuration

data ChainwebNodeConfiguration = ChainwebNodeConfiguration
    { _nodeConfigChainweb :: !ChainwebConfiguration
    , _nodeConfigLog :: !LogConfig
    , _nodeConfigDatabaseDirectory :: !(Maybe FilePath)
    , _nodeConfigResetChainDbs :: !Bool
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebNodeConfiguration

defaultChainwebNodeConfiguration :: ChainwebNodeConfiguration
defaultChainwebNodeConfiguration = ChainwebNodeConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration Mainnet01
    , _nodeConfigLog = defaultLogConfig
        & logConfigLogger . L.loggerConfigThreshold .~ level
    , _nodeConfigDatabaseDirectory = Nothing
    , _nodeConfigResetChainDbs = False
    }
  where
    level = L.Info

validateChainwebNodeConfiguration :: ConfigValidation ChainwebNodeConfiguration []
validateChainwebNodeConfiguration o = do
    validateLogConfig $ _nodeConfigLog o
    validateChainwebConfiguration $ _nodeConfigChainweb o
    mapM_ (validateFilePath "databaseDirectory") (_nodeConfigDatabaseDirectory o)

instance ToJSON ChainwebNodeConfiguration where
    toJSON o = object
        [ "chainweb" .= _nodeConfigChainweb o
        , "logging" .= _nodeConfigLog o
        , "databaseDirectory" .= _nodeConfigDatabaseDirectory o
        , "resetChainDatabases" .= _nodeConfigResetChainDbs o
        ]

instance FromJSON (ChainwebNodeConfiguration -> ChainwebNodeConfiguration) where
    parseJSON = withObject "ChainwebNodeConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb" % o
        <*< nodeConfigLog %.: "logging" % o
        <*< nodeConfigDatabaseDirectory ..: "databaseDirectory" % o
        <*< nodeConfigResetChainDbs ..: "resetChainDatabases" % o

pChainwebNodeConfiguration :: MParser ChainwebNodeConfiguration
pChainwebNodeConfiguration = id
    <$< nodeConfigChainweb %:: pChainwebConfiguration
    <*< nodeConfigLog %:: pLogConfig
    <*< nodeConfigDatabaseDirectory .:: fmap Just % textOption
        % long "database-directory"
        <> help "directory where the databases are persisted"
    <*< nodeConfigResetChainDbs .:: enableDisableFlag
        % long "reset-chain-databases"
        <> help "Reset the chain databases for all chains on startup"

getRocksDbDir :: HasCallStack => ChainwebNodeConfiguration -> IO FilePath
getRocksDbDir conf = (\base -> base </> "0" </> "rocksDb") <$> getDbBaseDir conf

getPactDbDir :: HasCallStack => ChainwebNodeConfiguration -> IO FilePath
getPactDbDir conf =  (\base -> base </> "0" </> "sqlite")  <$> getDbBaseDir conf

getBackupsDir :: HasCallStack => ChainwebNodeConfiguration -> IO FilePath
getBackupsDir conf = (</> "backups") <$> getDbBaseDir conf

getDbBaseDir :: HasCallStack => ChainwebNodeConfiguration -> IO FilePath
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
        logFunctionText l Info $ "Initialized Cut Monitor"
        S.mapM_ (logFunctionJson l Info)
            $ S.map (cutToCutHashes Nothing)
            $ cutStream db

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
        logFunctionText l Info $ "Initialized tx counter"
        blockDiffStream db
            & S.mapM toUpdate
            & S.mapM_ (logFunctionJson l Info)
  where
    txsDb = view (cutDbPayloadDb . transactionDb . transactionDbBlockTransactions) db
    payloadDb = view (cutDbPayloadDb . transactionDb . transactionDbBlockPayloads) db

    txCount :: BlockHeader -> IO Int
    txCount bh = do
        bp <- casLookupM payloadDb (_blockPayloadHash bh)
        x <- casLookupM txsDb (_blockPayloadTransactionsHash bp)
        return $ length $ _blockTransactions x

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
            logFunctionText l Info $ "Initialized RTS Monitor"
            runMonitorLoop "Chainweb.Node.runRtsMonitor" l $ do
                logFunctionText l Debug $ "logging RTS stats"
                stats <- getRTSStats
                logFunctionJson logger Info stats
                approximateThreadDelay 60_000_000 {- 1 minute -}

runQueueMonitor :: Logger logger => logger -> CutDb tbl -> IO ()
runQueueMonitor logger cutDb = L.withLoggerLabel ("component", "queue-monitor") logger go
  where
    go l = do
        logFunctionText l Info $ "Initialized Queue Monitor"
        runMonitorLoop "ChainwebNode.runQueueMonitor" l $ do
            logFunctionText l Debug $ "logging cut queue stats"
            stats <- getQueueStats cutDb
            logFunctionJson logger Info stats
            approximateThreadDelay 60_000_000 {- 1 minute -}

data DbStats = DbStats
    { dbStatsName :: !Text
    , dbStatsSize :: !Integer
    } deriving (Generic, NFData, ToJSON)

runDatabaseMonitor :: Logger logger => logger -> FilePath -> FilePath -> IO ()
runDatabaseMonitor logger rocksDbDir pactDbDir = L.withLoggerLabel ("component", "database-monitor") logger go
  where
    go l = do
        logFunctionText l Info "Initialized Database monitor"
        runMonitorLoop "ChainwebNode.runDatabaseMonitor" l $ do
            logFunctionText l Debug $ "logging database stats"
            logFunctionJson l Info . DbStats "rocksDb" =<< sizeOf rocksDbDir
            logFunctionJson l Info . DbStats "pactDb" =<< sizeOf pactDbDir
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
-- Run Node

node :: HasCallStack => Logger logger => ChainwebNodeConfiguration -> logger -> IO ()
node conf logger = do
    dbBaseDir <- getDbBaseDir conf
    when (_nodeConfigResetChainDbs conf) $ removeDirectoryRecursive dbBaseDir
    rocksDbDir <- getRocksDbDir conf
    pactDbDir <- getPactDbDir conf
    dbBackupsDir <- getBackupsDir conf
    withRocksDb' <-
        if _configOnlySyncPact cwConf
        then
            if _cutPruneChainDatabase (_configCuts cwConf) == GcNone
            then withReadOnlyRocksDb <$ logFunctionText logger Info "Opening RocksDB in read-only mode"
            else withRocksDb <$ logFunctionText logger Info "Opening RocksDB in read-write mode, if this wasn't intended, ensure that cuts.pruneChainDatabase is set to none"
        else
            return withRocksDb
    withRocksDb' rocksDbDir modernDefaultOptions $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory " <> sshow rocksDbDir
        logFunctionText logger Info $ "backup config: " <> sshow (_configBackup cwConf)
        withChainweb cwConf logger rocksDb pactDbDir dbBackupsDir (_nodeConfigResetChainDbs conf) $ \case
            Replayed _ _ -> return ()
            StartedChainweb cw ->
                concurrentlies_
                    [ runChainweb cw
                    -- we should probably push 'onReady' deeper here but this should be ok
                    , runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                    , runQueueMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                    , runRtsMonitor (_chainwebLogger cw)
                    , runBlockUpdateMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                    , runDatabaseMonitor (_chainwebLogger cw) rocksDbDir pactDbDir
                    ]
  where
    cwConf = _nodeConfigChainweb conf

withNodeLogger
    :: LogConfig
    -> ChainwebConfiguration
    -> ChainwebVersion
    -> (L.Logger SomeLogMessage -> IO ())
    -> IO ()
withNodeLogger logCfg chainwebCfg v f = runManaged $ do

    -- This manager is used only for logging backends
    mgr <- liftIO HTTPS.newTlsManager

    -- Base Backend
    baseBackend <- managed
        $ withBaseHandleBackend "ChainwebApp" mgr pkgInfoScopes (_logConfigBackend logCfg)

    -- we don't log tx failures in replay
    let !txFailureHandler =
            if _configOnlySyncPact chainwebCfg
            then dropLogHandler (Proxy :: Proxy TxFailureLog)
            else passthroughLogHandler

    -- Telemetry Backends
    monitorBackend <- managed
        $ mkTelemetryLogger @CutHashes mgr teleLogConfig
    p2pInfoBackend <- managed
        $ mkTelemetryLogger @P2pSessionInfo mgr teleLogConfig
    rtsBackend <- managed
        $ mkTelemetryLogger @RTSStats mgr teleLogConfig
    counterBackend <- managed $ configureHandler
        (withJsonHandleBackend @CounterLog "connectioncounters" mgr pkgInfoScopes)
        teleLogConfig
    endpointBackend <- managed
        $ mkTelemetryLogger @PactCmdLog mgr teleLogConfig
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
    reintroBackend <- managed
        $ mkTelemetryLogger @ReintroducedTxsLog mgr teleLogConfig
    traceBackend <- managed
        $ mkTelemetryLogger @Trace mgr teleLogConfig
    mempoolStatsBackend <- managed
        $ mkTelemetryLogger @MempoolStats mgr teleLogConfig
    blockUpdateBackend <- managed
        $ mkTelemetryLogger @BlockUpdate mgr teleLogConfig
    dbCacheBackend <- managed
        $ mkTelemetryLogger @DbCacheStats mgr teleLogConfig
    dbStatsBackend <- managed
        $ mkTelemetryLogger @DbStats mgr teleLogConfig
    pactQueueStatsBackend <- managed
        $ mkTelemetryLogger @PactQueueStats mgr teleLogConfig
    topLevelStatusBackend <- managed
        $ mkTelemetryLogger @ChainwebStatus mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logCfg) $ logHandles
            [ logFilterHandle (_logConfigFilter logCfg)
            , txFailureHandler
            , logHandler monitorBackend
            , logHandler p2pInfoBackend
            , logHandler rtsBackend
            , logHandler counterBackend
            , logHandler endpointBackend
            , logHandler newBlockBackend
            , logHandler orphanedBlockBackend
            , logHandler miningStatsBackend
            , logHandler requestLogBackend
            , logHandler queueStatsBackend
            , logHandler reintroBackend
            , logHandler traceBackend
            , logHandler mempoolStatsBackend
            , logHandler blockUpdateBackend
            , logHandler dbCacheBackend
            , logHandler dbStatsBackend
            , logHandler pactQueueStatsBackend
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
    . (Typeable a, ToJSON a)
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
    :: (LogLevel -> Text -> IO ())
    -> Maybe UTCTime
    -> IO a
    -> IO a
withServiceDate _ Nothing inner = inner
withServiceDate lf (Just t) inner = race timer inner >>= \case
    Left () -> error "Service date thread terminated unexpectedly"
    Right a -> return a
  where
    timer = runForever lf "ServiceDate" $ do
        now <- getCurrentTime
        when (now >= t) $ do
            lf Error shutdownMessage
            throw $ ServiceDate shutdownMessage

        let w = diffUTCTime t now
        let micros = round $ w * 1_000_000
        lf Warn warning
        threadDelay $ min (10 * 60 * 1_000_000) micros

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

-- SERVICE DATE for version 2.22
--
serviceDate :: Maybe String
serviceDate = Just "2024-03-06T00:00:00Z"

mainInfo :: ProgramInfo ChainwebNodeConfiguration
mainInfo = programInfoValidate
    "Chainweb Node"
    pChainwebNodeConfiguration
    defaultChainwebNodeConfiguration
    validateChainwebNodeConfiguration

handles :: [Handler a] -> IO a -> IO a
handles = flip catches

main :: IO ()
main = do
    installFatalSignalHandlers [ sigHUP, sigTERM, sigXCPU, sigXFSZ ]
    checkRLimits
    runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
        let v = _configChainwebVersion $ _nodeConfigChainweb conf
        registerVersion v
        hSetBuffering stderr LineBuffering
        withNodeLogger (_nodeConfigLog conf) (_nodeConfigChainweb conf) v $ \logger -> do
            logFunctionJson logger Info ProcessStarted
            handles
                [ Handler $ \(e :: SomeAsyncException) ->
                    logFunctionJson logger Info (ProcessDied $ show e) >> throwIO e
                , Handler $ \(e :: SomeException) ->
                    logFunctionJson logger Error (ProcessDied $ show e) >> throwIO e
                ] $ do
                kt <- mapM iso8601ParseM serviceDate
                withServiceDate (logFunctionText logger) kt $ void $
                    race (node conf logger) (gcRunner (logFunctionText logger))
    where
    gcRunner lf = runForever lf "GarbageCollect" $ do
        performMajorGC
        threadDelay (30 * 1_000_000)
