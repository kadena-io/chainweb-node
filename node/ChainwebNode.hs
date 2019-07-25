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

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Managed

import Data.CAS.RocksDB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Typeable

import GHC.Generics hiding (from)
import GHC.Stats

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.Directory
import qualified System.Logger as L
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader (NewMinedBlock, AmberdataBlock(..))
import Chainweb.Chainweb
import Chainweb.Cut (Cut, _cutMap)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.Chainweb.CutResources
import Chainweb.Mempool.Consensus (ReintroducedTxsLog)
import Chainweb.Counter
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version (ChainwebVersion(..))

import Data.LogMessage
import Data.PQueue
import qualified Data.TaskMap as TM

import P2P.Node

import PkgInfo

import Utils.Logging
import Utils.Logging.Config

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

runCutMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runCutMonitor logger db = L.withLoggerLabel ("component", "cut-monitor") logger $ \l -> do
    go l `catchAllSynchronous` \e ->
        logFunctionText l Error ("Cut Monitor failed: " <> sshow e)
    logFunctionText l Info "Stopped Cut Monitor"
  where
    go l = do
        logFunctionText l Info $ "Initialized Cut Monitor"
        void
            $ S.mapM_ (logFunctionJson l Info)
            $ S.map (cutToCutHashes Nothing)
            $ cutStream db

            -- This logs complete cuts, which is much more data
            -- $ S.mapM_ (logFunctionJson logger' Info)
            -- $ S.map (fmap ObjectEncoded)
            -- $ S.map _cutMap
            -- $ cutStream db


runAmberdataBlockMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runAmberdataBlockMonitor logger db = L.withLoggerLabel ("component", "amberdata-block-monitor") logger $ \l -> do
    go l `catchAllSynchronous` \e ->
        logFunctionText l Error ("Amberdata Block Monitor failed: " <> sshow e)
    logFunctionText l Info "Stopped Amberdata Block Monitor"
  where
    go l = do
      logFunctionText l Info "Initialized Amberdata Block Monitor"
      void
        $ S.mapM_ (logAllBlocks l)
        $ S.map cutToAmberdataBlocks
        $ cutStream db

    logAllBlocks :: Logger logger => logger -> [AmberdataBlock] -> IO ()
    logAllBlocks l = mapM_ (logFunctionJson l Info)

    cutToAmberdataBlocks :: Cut -> [AmberdataBlock]
    cutToAmberdataBlocks c = map
                               (\(_,bh) -> AmberdataBlock
                                           (_blockHeight bh)
                                           (_blockHash bh)
                                           (_blockCreationTime bh)
                                           (_blockParent bh)
                                           (_blockNonce bh)
                                           (_blockMiner bh)
                                           -- undefined Blocksize
                                           -- undefined blockTransNum
                                           (_blockChainId bh)
                                           (_blockWeight bh)
                               )
                               (HM.toList (_cutMap c))


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
runRtsMonitor logger = L.withLoggerLabel ("component", "rts-monitor") logger $ \l -> do
    go l `catchAllSynchronous` \e ->
        logFunctionText l Error ("RTS Monitor failed: " <> sshow e)
    logFunctionText l Info "Stopped RTS Monitor"
  where
    go l = getRTSStatsEnabled >>= \case
        False -> logFunctionText l Warn "RTS Stats isn't enabled. Run with '+RTS -T' to enable it."
        True -> do
            logFunctionText l Info $ "Initialized RTS Monitor"
            forever $ do
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
runQueueMonitor logger cutDb = L.withLoggerLabel ("component", "queue-monitor") logger $ \l -> do
    go l `catchAllSynchronous` \e ->
        logFunctionText l Error ("Queue Monitor failed: " <> sshow e)
    logFunctionText l Info "Stopped Queue Monitor"
  where
    go l = do
        logFunctionText l Info $ "Initialized Queue Monitor"
        forever $ do
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

-- -------------------------------------------------------------------------- --
-- Run Node

node :: Logger logger => ChainwebNodeConfiguration -> logger -> IO ()
node conf logger = do
    rocksDbDir <- getRocksDbDir
    when (_nodeConfigResetChainDbs conf) $ destroyRocksDb rocksDbDir
    withRocksDb rocksDbDir $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory " <> sshow rocksDbDir
        withChainweb cwConf logger rocksDb $ \cw -> mapConcurrently_ id
            [ runChainweb cw
            , runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runAmberdataBlockMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runQueueMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runRtsMonitor (_chainwebLogger cw)
            ]
  where
    cwConf = _nodeConfigChainweb conf
    nodeText = T.unpack (toText (_configNodeId cwConf))
    v = _configChainwebVersion cwConf
    getRocksDbDir = case _nodeConfigDatabaseDirectory conf of
        Nothing -> getXdgDirectory XdgData
            $ "chainweb-node/" <> sshow v <> "/" <> nodeText <> "/rocksDb"
        Just d -> return d

withNodeLogger
    :: LogConfig
    -> ChainwebVersion
    -> (L.Logger SomeLogMessage -> IO ())
    -> IO ()
withNodeLogger logConfig v f = runManaged $ do

    -- This manager is used only for logging backends
    mgr <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    mgrHttps <- liftIO $ HTTPS.newTlsManager

    -- Base Backend
    baseBackend <- managed
        $ withBaseHandleBackend "ChainwebApp" mgr (_logConfigBackend logConfig)

    -- Telemetry Backends
    monitorBackend <- managed
        $ mkTelemetryLogger @CutHashes mgr teleLogConfig
    p2pInfoBackend <- managed
        $ mkTelemetryLogger @P2pSessionInfo mgr teleLogConfig
    rtsBackend <- managed
        $ mkTelemetryLogger @RTSStats mgr teleLogConfig
    counterBackend <- managed $ configureHandler
        (withJsonHandleBackend @CounterLog "connectioncounters" mgr)
        teleLogConfig
    newBlockAmberdataBackend <- managed $ mkAmberdataLogger mgrHttps amberdataConfig
    newBlockBackend <- managed
        $ mkTelemetryLogger @NewMinedBlock mgr teleLogConfig
    requestLogBackend <- managed
        $ mkTelemetryLogger @RequestResponseLog mgr teleLogConfig
    queueStatsBackend <- managed
        $ mkTelemetryLogger @QueueStats mgr teleLogConfig
    reintroBackend <- managed
        $ mkTelemetryLogger @ReintroducedTxsLog mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logConfig) $ logHandles
            [ logHandler monitorBackend
            , logHandler p2pInfoBackend
            , logHandler rtsBackend
            , logHandler counterBackend
            , logHandler newBlockAmberdataBackend
            , logHandler newBlockBackend
            , logHandler requestLogBackend
            , logHandler queueStatsBackend
            , logHandler reintroBackend
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

mainInfo :: ProgramInfo ChainwebNodeConfiguration
mainInfo = programInfo
    "Chainweb Node"
    pChainwebNodeConfiguration
    (defaultChainwebNodeConfiguration Testnet01)

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
    let v = _configChainwebVersion $ _nodeConfigChainweb conf
    withNodeLogger (_nodeConfigLog conf) v $ node conf
