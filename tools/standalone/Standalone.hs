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
import System.Timeout

import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

-- chainweb imports

import Chainweb.Chainweb
import Chainweb.Chainweb.CutResources
import Chainweb.Counter
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Logging.Config
import Chainweb.Logging.Miner
import Chainweb.Mempool.InMemTypes (MempoolStats(..))
import Chainweb.Miner.Config
import Chainweb.Miner.Pact
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore.Types
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Standalone.Chainweb
import Standalone.Utils

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

node :: Logger logger => StandaloneConfiguration -> Maybe BlockStopState -> logger -> IO ()
node conf mbs logger = do
    rocksDbDir <- getRocksDbDir
    when (_nodeConfigResetChainDbs conf) $ destroyRocksDb rocksDbDir
    withRocksDb rocksDbDir $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory" <> sshow rocksDbDir
        withChainwebStandalone cwConf logger rocksDb (_nodeConfigDatabaseDirectory conf) (_nodeConfigResetChainDbs conf)
          $ \cw ->
          stopWrapper cw $
          mapConcurrently_ id
            [ runChainweb' cw
            , runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
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
    stopWrapper cw = case mbs of
        Nothing -> id
        Just (Height bh) -> concurrently_ (stopAtCutHeight bh (_cutResCutDb $ _chainwebCutResources cw))
        Just (Weight bw) -> concurrently_ (stopAtCutWeight bw (_cutResCutDb $ _chainwebCutResources cw))

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

  where
    logg = logFunctionText $ _chainwebLogger cw
    miner = maybe go (\m -> runMiner (_chainwebVersion cw) m) $ _chainwebMiner cw
        where
          go = do
            logg Warn "No miner configured. Starting consensus without mining."
            forever (threadDelay 1000000)

data StandaloneConfiguration = StandaloneConfiguration
  { _nodeConfigChainweb :: !ChainwebConfiguration
  , _nodeConfigLog :: !LogConfig
  , _nodeConfigDatabaseDirectory :: !(Maybe FilePath)
  , _nodeConfigResetChainDbs :: !Bool
  , _nodeConfigStopCondition :: !StopState
  }
  deriving (Show, Eq, Generic)

makeLenses ''StandaloneConfiguration

defaultStandaloneConfiguration :: ChainwebVersion -> StandaloneConfiguration
defaultStandaloneConfiguration v = StandaloneConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration v
        & configMiner . enableConfigEnabled .~ True
        & configMiner . enableConfigConfig . configMinerInfo .~ noMiner
    , _nodeConfigLog = defaultLogConfig
        & logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _nodeConfigDatabaseDirectory = Just "standalonedbs/"
    , _nodeConfigResetChainDbs = False
    , _nodeConfigStopCondition = BlockStopCondition (Height 2)
    -- , _nodeConfigStopCondition = TimeLength (1000000 * 60 * 10)
    }

instance ToJSON StandaloneConfiguration where
    toJSON o = object
        [ "chainweb" .= _nodeConfigChainweb o
        , "logging" .= _nodeConfigLog o
        , "databaseDirectory" .= _nodeConfigDatabaseDirectory o
        , "resetChainDatabases" .= _nodeConfigResetChainDbs o
        , "stopCondition" .= _nodeConfigStopCondition o
        ]

instance FromJSON (StandaloneConfiguration -> StandaloneConfiguration) where
    parseJSON = withObject "StandaloneConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb" % o
        <*< nodeConfigLog %.: "logging" % o
        <*< nodeConfigDatabaseDirectory ..: "databaseDirectory" % o
        <*< nodeConfigResetChainDbs ..: "resetChainDatabases" % o
        <*< nodeConfigStopCondition ..: "stopCondition" % o

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
        $ withBaseHandleBackend "ChainwebApp" mgr pkgInfoScopes (_logConfigBackend logConfig)

    -- Telemetry Backends
    monitorBackend <- managed
        $ mkTelemetryLogger @CutHashes mgr teleLogConfig
    rtsBackend <- managed
        $ mkTelemetryLogger @RTSStats mgr teleLogConfig
    counterBackend <- managed $ configureHandler
        (withJsonHandleBackend @CounterLog "connectioncounters" mgr pkgInfoScopes)
        teleLogConfig
    newBlockBackend <- managed
        $ mkTelemetryLogger @NewMinedBlock mgr teleLogConfig
    queueStatsBackend <- managed
        $ mkTelemetryLogger @QueueStats mgr teleLogConfig
    traceBackend <- managed
        $ mkTelemetryLogger @Trace mgr teleLogConfig

    mempoolStatsBackend <- managed
        $ mkTelemetryLogger @MempoolStats mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logConfig) $ logHandles
            [ logHandler monitorBackend
            , logHandler rtsBackend
            , logHandler counterBackend
            , logHandler newBlockBackend
            , logHandler queueStatsBackend
            , logHandler traceBackend
            , logHandler mempoolStatsBackend

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

mainInfo :: ProgramInfo StandaloneConfiguration
mainInfo = programInfo
    "Chainweb Node"
    pStandaloneConfiguration
    (defaultStandaloneConfiguration (FastTimedCPM petersonChainGraph))

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
    let v = _configChainwebVersion $ _nodeConfigChainweb conf
    withNodeLogger (_nodeConfigLog conf) v $ \logger ->
      case _nodeConfigStopCondition conf of
        Forever -> node conf Nothing logger
        TimeLength duration ->  void $ timeout duration $ node conf Nothing logger
        BlockStopCondition bsc -> node conf (Just bsc) logger
