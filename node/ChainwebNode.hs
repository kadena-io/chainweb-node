{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Error.Class (throwError)
import Control.Monad.Managed

import Data.Bool
import Data.CAS
import Data.CAS.RocksDB
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Typeable

import GHC.Generics hiding (from)
import GHC.Stats

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.Directory
import System.Exit (exitFailure)
import qualified System.Logger as L
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Chainweb
import Chainweb.Chainweb.CutResources
import Chainweb.Counter
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Logging.Amberdata
import Chainweb.Logging.Config
import Chainweb.Logging.Miner
import Chainweb.Mempool.Consensus (ReintroducedTxsLog)
import Chainweb.Mempool.InMemTypes (MempoolStats(..))
import Chainweb.Miner.Coordinator (MiningStats)
import Chainweb.Pact.RestAPI.Server (PactCmdLog(..))
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Utils.Watchdog (notifyReady, withWatchdog)
import Chainweb.Version

import Data.LogMessage
import Data.PQueue
import qualified Data.TaskMap as TM

import P2P.Node

import PkgInfo

import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

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
        & logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _nodeConfigDatabaseDirectory = Nothing
    , _nodeConfigResetChainDbs = False
    }

validateChainwebNodeConfiguration :: ConfigValidation ChainwebNodeConfiguration []
validateChainwebNodeConfiguration o = do
    validateLogConfig $ _nodeConfigLog o
    validateChainwebConfiguration $ _nodeConfigChainweb o
    maybe (return ())
          checkIfValidChain
          (getAmberdataChainId o)
    maybe (return ())
          (validateFilePath "databaseDirectory")
          (_nodeConfigDatabaseDirectory o)
  where
    chains = chainIds $ _nodeConfigChainweb o
    checkIfValidChain cid
      = bool
        (throwError $ "Invalid chain id provided: " <> toText cid)
        (return ())
        (HS.member cid chains)
    getAmberdataChainId = _amberdataChainId . _enableConfigConfig . _logConfigAmberdataBackend . _nodeConfigLog


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

runBlockUpdateMonitor :: PayloadCas cas => Logger logger => logger -> CutDb cas -> IO ()
runBlockUpdateMonitor logger db = L.withLoggerLabel ("component", "block-update-monitor") logger $ \l ->
    runMonitorLoop "ChainwebNode.runBlockUpdateMonitor" l $ do
        logFunctionText l Info $ "Initialized tx counter"
        blockDiffStream db
            & S.mapM toUpdate
            & S.mapM_ (logFunctionJson l Info)
  where
    payloadCas = _webBlockPayloadStoreCas $ view cutDbPayloadStore db

    txCount :: BlockHeader -> IO Int
    txCount bh = do
        x <- casLookupM payloadCas (_blockPayloadHash bh)
        return $ length $ _payloadWithOutputsTransactions x

    toUpdate :: Either BlockHeader BlockHeader -> IO BlockUpdate
    toUpdate (Right bh) = BlockUpdate
        <$> pure (ObjectEncoded bh) -- _blockUpdateBlockHeader
        <*> pure False -- _blockUpdateOrphaned
        <*> txCount bh -- _blockUpdateTxCount
    toUpdate (Left bh) = BlockUpdate
        <$> pure (ObjectEncoded bh) -- _blockUpdateBlockHeader
        <*> pure True -- _blockUpdateOrphaned
        <*> ((0 -) <$> txCount bh) -- _blockUpdateTxCount

runAmberdataBlockMonitor :: (PayloadCas cas, Logger logger) => Maybe ChainId -> logger -> CutDb cas -> IO ()
runAmberdataBlockMonitor cid logger db
    = L.withLoggerLabel ("component", "amberdata-block-monitor") logger $ \l ->
        runMonitorLoop "Chainweb.Logging.amberdataBlockMonitor" l (amberdataBlockMonitor cid l db)

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
                approximateThreadDelay 60000000 {- 1 minute -}

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
            approximateThreadDelay 60000000 {- 1 minute -}



-- -------------------------------------------------------------------------- --
-- Run Node

node :: Logger logger => ChainwebNodeConfiguration -> logger -> IO ()
node conf logger = do
    rocksDbDir <- getRocksDbDir
    when (_nodeConfigResetChainDbs conf) $ destroyRocksDb rocksDbDir
    withRocksDb rocksDbDir $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory " <> sshow rocksDbDir
        withChainweb cwConf logger rocksDb (_nodeConfigDatabaseDirectory conf) (_nodeConfigResetChainDbs conf) $ \cw -> mapConcurrently_ id
            [ notifyReady >> runChainweb cw
              -- we should probably push 'onReady' deeper here but this should be ok
            , runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runAmberdataBlockMonitor (amberdataChainId conf) (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runQueueMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runRtsMonitor (_chainwebLogger cw)
            , runBlockUpdateMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            ]
  where
    cwConf = _nodeConfigChainweb conf
    nodeText = T.unpack (toText (_configNodeId cwConf))
    v = _configChainwebVersion cwConf
    getRocksDbDir = case _nodeConfigDatabaseDirectory conf of
        Nothing -> getXdgDirectory XdgData
            $ "chainweb-node/" <> sshow v <> "/" <> nodeText <> "/rocksDb"
        Just d -> return d
    amberdataChainId = _amberdataChainId . _enableConfigConfig . _logConfigAmberdataBackend . _nodeConfigLog

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
    newBlockAmberdataBackend <- managed $ mkAmberdataLogger mgr amberdataConfig
    endpointBackend <- managed
        $ mkTelemetryLogger @PactCmdLog mgr teleLogConfig
    newBlockBackend <- managed
        $ mkTelemetryLogger @NewMinedBlock mgr teleLogConfig
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

    logger <- managed
        $ L.withLogger (_logConfigLogger logConfig) $ logHandles
            [ logFilterHandle (_logConfigFilter logConfig)
            , logHandler monitorBackend
            , logHandler p2pInfoBackend
            , logHandler rtsBackend
            , logHandler counterBackend
            , logHandler newBlockAmberdataBackend
            , logHandler endpointBackend
            , logHandler newBlockBackend
            , logHandler miningStatsBackend
            , logHandler requestLogBackend
            , logHandler queueStatsBackend
            , logHandler reintroBackend
            , logHandler traceBackend
            , logHandler mempoolStatsBackend
            , logHandler blockUpdateBackend
            ] baseBackend

    liftIO $ f
        $ maybe id (\x -> addLabel ("cluster", toText x)) (_logConfigClusterId logConfig)
        $ addLabel ("chainwebVersion", sshow v)
        $ logger
  where
    teleLogConfig = _logConfigTelemetryBackend logConfig
    amberdataConfig = _logConfigAmberdataBackend logConfig

mkAmberdataLogger
    :: HTTP.Manager
    -> EnableConfig AmberdataConfig
    -> (Backend (JsonLog AmberdataBlock) -> IO b)
    -> IO b
mkAmberdataLogger mgr = configureHandler
  $ withAmberDataBlocksBackend mgr

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

mainInfo :: ProgramInfo ChainwebNodeConfiguration
mainInfo = programInfoValidate
    "Chainweb Node"
    pChainwebNodeConfiguration
    (defaultChainwebNodeConfiguration Testnet02)
    validateChainwebNodeConfiguration

-- | KILLSWITCH: The logic surrounding `txSilenceEndDate` here is to be removed in
-- a future version of Chainweb. This prevents the Node from even starting if
-- past a specified date.
--
main :: IO ()
main = withWatchdog . runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
    let v = _configChainwebVersion $ _nodeConfigChainweb conf
    now <- getCurrentTimeIntegral
    case txSilenceEndDate v of
        Just end | now > end -> do
            putStrLn "Transactions are now possible - please update your Chainweb binary."
            exitFailure
        _ -> withNodeLogger (_nodeConfigLog conf) v $ node conf
