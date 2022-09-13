{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Data.CAS
import Data.CAS.RocksDB
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
import System.IO (BufferMode(LineBuffering), hSetBuffering, stderr)
import qualified System.Logger as L
import System.LogLevel

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
import Chainweb.Pact.Service.PactQueue (PactQueueStats)
import Chainweb.Pact.RestAPI.Server (PactCmdLog(..))
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version

import Data.LogMessage

import P2P.Node

import PkgInfo

import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

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

defaultChainwebNodeConfiguration :: ChainwebVersion -> ChainwebNodeConfiguration
defaultChainwebNodeConfiguration v = ChainwebNodeConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration v
    , _nodeConfigLog = defaultLogConfig
        & logConfigLogger . L.loggerConfigThreshold .~ level
    , _nodeConfigDatabaseDirectory = Nothing
    , _nodeConfigResetChainDbs = False
    }
  where
    level = case v of
        Mainnet01 -> L.Info
        _ -> L.Info

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
        $ "chainweb-node" </> sshow v
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
runMonitorLoop :: Logger logger => T.Text -> logger -> IO () -> IO ()
runMonitorLoop actionLabel logger = runForeverThrottled
    (logFunction logger)
    actionLabel
    10 -- 10 bursts in case of failure
    (10 * mega) -- allow restart every 10 seconds in case of failure

runCutMonitor :: Logger logger => logger -> CutDb cas -> IO ()
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

runBlockUpdateMonitor :: PayloadCasLookup cas => Logger logger => logger -> CutDb cas -> IO ()
runBlockUpdateMonitor logger db = L.withLoggerLabel ("component", "block-update-monitor") logger $ \l ->
    runMonitorLoop "ChainwebNode.runBlockUpdateMonitor" l $ do
        logFunctionText l Info $ "Initialized tx counter"
        blockDiffStream db
            & S.mapM toUpdate
            & S.mapM_ (logFunctionJson l Info)
  where
    txsCas = view (cutDbPayloadCas . transactionDb . transactionDbBlockTransactions) db
    payloadCas = view (cutDbPayloadCas . transactionDb . transactionDbBlockPayloads) db

    txCount :: BlockHeader -> IO Int
    txCount bh = do
        bp <- casLookupM payloadCas (_blockPayloadHash bh)
        x <- casLookupM txsCas (_blockPayloadTransactionsHash bp)
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
#if !MIN_VERSION_base(4,15,0)
deriving instance Generic GCDetails
deriving instance Generic RTSStats
#endif

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

runQueueMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runQueueMonitor logger cutDb = L.withLoggerLabel ("component", "queue-monitor") logger go
  where
    go l = do
        logFunctionText l Info $ "Initialized Queue Monitor"
        runMonitorLoop "ChainwebNode.runQueueMonitor" l $ do
            logFunctionText l Debug $ "logging cut queue stats"
            stats <- getQueueStats cutDb
            logFunctionJson logger Info stats
            approximateThreadDelay 60_000_000 {- 1 minute -}

-- -------------------------------------------------------------------------- --
-- Run Node

node :: HasCallStack => Logger logger => ChainwebNodeConfiguration -> logger -> IO ()
node conf logger = do
    dbBaseDir <- getDbBaseDir conf
    when (_nodeConfigResetChainDbs conf) $ removeDirectoryRecursive dbBaseDir
    rocksDbDir <- getRocksDbDir conf
    pactDbDir <- getPactDbDir conf
    dbBackupsDir <- getBackupsDir conf
    withRocksDb rocksDbDir modernDefaultOptions $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory " <> sshow rocksDbDir
        logFunctionText logger Info $ "backup config: " <> sshow (_configBackup cwConf)
        withChainweb cwConf logger rocksDb pactDbDir dbBackupsDir (_nodeConfigResetChainDbs conf) $ \cw -> concurrentlies_
            [ runChainweb cw
            -- we should probably push 'onReady' deeper here but this should be ok
            , runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runQueueMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runRtsMonitor (_chainwebLogger cw)
            , runBlockUpdateMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            ]
  where
    cwConf = _nodeConfigChainweb conf

withNodeLogger
    :: LogConfig
    -> ChainwebVersion
    -> (L.Logger SomeLogMessage -> IO ())
    -> IO ()
withNodeLogger logConfig v f = runManaged $ do

    -- This manager is used only for logging backends
    mgr <- liftIO HTTPS.newTlsManager

    -- Base Backend
    baseBackend <- managed
        $ withBaseHandleBackend "ChainwebApp" mgr pkgInfoScopes (_logConfigBackend logConfig)

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
    pactQueueStatsBackend <- managed
        $ mkTelemetryLogger @PactQueueStats mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logConfig) $ logHandles
            [ logFilterHandle (_logConfigFilter logConfig)
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
            , logHandler pactQueueStatsBackend
            ] baseBackend

    liftIO $ f
        $ maybe id (\x -> addLabel ("cluster", toText x)) (_logConfigClusterId logConfig)
        $ addLabel ("chainwebVersion", sshow v)
        $ logger
  where
    teleLogConfig = _logConfigTelemetryBackend logConfig

mkTelemetryLogger
    :: forall a b
    . Typeable a
    => ToJSON a
    => HTTP.Manager
    -> EnableConfig BackendConfig
    -> (Backend (JsonLog a) -> IO b)
    -> IO b
mkTelemetryLogger mgr = configureHandler
    $ withJsonHandleBackend @(JsonLog a) (sshow $ typeRep $ Proxy @a) mgr pkgInfoScopes

-- -------------------------------------------------------------------------- --
-- Service Date

newtype ServiceDate = ServiceDate T.Text

instance Show ServiceDate where
    show (ServiceDate t) = "Service interval end: " <> T.unpack t

instance Exception ServiceDate where
    fromException = asyncExceptionFromException
    toException = asyncExceptionToException

withServiceDate
    :: (LogLevel -> T.Text -> IO ())
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

    warning :: T.Text
    warning = T.concat
        [ "This version of chainweb node will stop to work at " <> sshow t <> "."
        , " Please upgrade to a new version before that date."
        ]

    shutdownMessage :: T.Text
    shutdownMessage = T.concat
        [ "Shutting down. This version of chainweb was only valid until" <> sshow t <> "."
        , " Please upgrade to a new version."
        ]

-- -------------------------------------------------------------------------- --
-- Encode Package Info into Log mesage scopes

pkgInfoScopes :: [(T.Text, T.Text)]
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

-- SERVICE DATE for version 2.16
--
serviceDate :: Maybe String
serviceDate = Just "2022-12-01T00:00:00Z"

mainInfo :: ProgramInfo ChainwebNodeConfiguration
mainInfo = programInfoValidate
    "Chainweb Node"
    pChainwebNodeConfiguration
    (defaultChainwebNodeConfiguration Mainnet01)
    validateChainwebNodeConfiguration

main :: IO ()
main = do
    installFatalSignalHandlers [ sigHUP, sigTERM, sigXCPU, sigXFSZ ]
    runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
        let v = _configChainwebVersion $ _nodeConfigChainweb conf
        hSetBuffering stderr LineBuffering
        withNodeLogger (_nodeConfigLog conf) v $ \logger -> do
            kt <- mapM (parseTimeM False defaultTimeLocale timeFormat) serviceDate
            withServiceDate (logFunctionText logger) kt $
                node conf logger
  where
    timeFormat = iso8601DateFormat (Just "%H:%M:%SZ")
