{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Chainweb.Metrics.Database.Monitor
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Comprehensive database monitoring integration
--
module Chainweb.Metrics.Database.Monitor
( -- * Comprehensive Database Monitoring
  DatabaseMonitor(..)
, DatabaseMonitorConfig(..)
, defaultDatabaseMonitorConfig
, startDatabaseMonitor
, stopDatabaseMonitor
, withDatabaseMonitor

  -- * Per-Chain Database Monitoring
, MultiChainDatabaseMonitor(..)
, startMultiChainMonitor
, stopMultiChainMonitor
, addChainMonitor
, removeChainMonitor

  -- * Monitoring Statistics
, MonitoringStats(..)
, collectMonitoringStats
) where

import Control.Concurrent (ThreadId)
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Text as T
import Data.Word (Word64)

import qualified Database.RocksDB.Base as R

import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.Metrics.Database
import Chainweb.Metrics.Database.Statistics
import Chainweb.Metrics.Database.Compaction

-- -------------------------------------------------------------------------- --
-- Configuration

-- | Configuration for comprehensive database monitoring
data DatabaseMonitorConfig = DatabaseMonitorConfig
    { _monitorOperationMetrics :: !Bool
    -- ^ Enable operation timing and counting metrics
    , _monitorStatistics :: !Bool
    -- ^ Enable RocksDB statistics collection
    , _monitorCompaction :: !Bool
    -- ^ Enable compaction monitoring
    , _monitorStorage :: !Bool
    -- ^ Enable storage monitoring
    , _statisticsConfig :: !StatisticsConfig
    -- ^ Configuration for statistics collection
    , _compactionConfig :: !CompactionConfig
    -- ^ Configuration for compaction monitoring
    , _storageConfig :: !StorageConfig
    -- ^ Configuration for storage monitoring
    } deriving (Eq, Show)

-- | Default database monitor configuration
defaultDatabaseMonitorConfig :: DatabaseMonitorConfig
defaultDatabaseMonitorConfig = DatabaseMonitorConfig
    { _monitorOperationMetrics = True
    , _monitorStatistics = True
    , _monitorCompaction = True
    , _monitorStorage = True
    , _statisticsConfig = defaultStatisticsConfig
    , _compactionConfig = defaultCompactionConfig
    , _storageConfig = defaultStorageConfig
    }

-- -------------------------------------------------------------------------- --
-- Single Database Monitoring

-- | Complete database monitor state
data DatabaseMonitor = DatabaseMonitor
    { _monitorConfig :: !DatabaseMonitorConfig
    , _monitorMetrics :: !DatabaseMetricsInterface
    , _monitorStatisticsCollector :: !(Maybe StatisticsCollector)
    , _monitorCompactionMonitor :: !(Maybe CompactionMonitor)
    , _monitorStorageMonitor :: !(Maybe StorageMonitor)
    }

-- | Start comprehensive database monitoring
startDatabaseMonitor
    :: DatabaseMonitorConfig
    -> R.DB
    -> FilePath  -- ^ Database path
    -> DatabaseMetricsInterface
    -> IO DatabaseMonitor
startDatabaseMonitor config@DatabaseMonitorConfig{..} db dbPath metrics = do
    -- Start statistics collection if enabled
    statsCollector <- if _monitorStatistics
        then Just <$> startStatisticsCollector _statisticsConfig db metrics
        else pure Nothing

    -- Start compaction monitoring if enabled
    compactionMonitor <- if _monitorCompaction
        then Just <$> startCompactionMonitor _compactionConfig db metrics
        else pure Nothing

    -- Start storage monitoring if enabled
    storageMonitor <- if _monitorStorage
        then Just <$> startStorageMonitor _storageConfig dbPath metrics
        else pure Nothing

    pure $ DatabaseMonitor
        { _monitorConfig = config
        , _monitorMetrics = metrics
        , _monitorStatisticsCollector = statsCollector
        , _monitorCompactionMonitor = compactionMonitor
        , _monitorStorageMonitor = storageMonitor
        }

-- | Stop database monitoring
stopDatabaseMonitor :: DatabaseMonitor -> IO ()
stopDatabaseMonitor DatabaseMonitor{..} = do
    -- Stop all monitoring components
    case _monitorStatisticsCollector of
        Just collector -> stopStatisticsCollector collector
        Nothing -> pure ()

    case _monitorCompactionMonitor of
        Just monitor -> stopCompactionMonitor monitor
        Nothing -> pure ()

    case _monitorStorageMonitor of
        Just monitor -> stopStorageMonitor monitor
        Nothing -> pure ()

-- | Execute an action with database monitoring
withDatabaseMonitor
    :: DatabaseMonitorConfig
    -> R.DB
    -> FilePath
    -> DatabaseMetricsInterface
    -> (DatabaseMonitor -> IO a)
    -> IO a
withDatabaseMonitor config db dbPath metrics action =
    bracket
        (startDatabaseMonitor config db dbPath metrics)
        stopDatabaseMonitor
        action

-- -------------------------------------------------------------------------- --
-- Multi-Chain Database Monitoring

-- | Monitor for multiple chain databases
data MultiChainDatabaseMonitor = MultiChainDatabaseMonitor
    { _multiChainConfig :: !DatabaseMonitorConfig
    , _multiChainMonitors :: !(IORef (HashMap ChainId DatabaseMonitor))
    }

-- | Start multi-chain database monitoring
startMultiChainMonitor
    :: DatabaseMonitorConfig
    -> IO MultiChainDatabaseMonitor
startMultiChainMonitor config = do
    monitorsRef <- newIORef HM.empty
    pure $ MultiChainDatabaseMonitor
        { _multiChainConfig = config
        , _multiChainMonitors = monitorsRef
        }

-- | Stop multi-chain database monitoring
stopMultiChainMonitor :: MultiChainDatabaseMonitor -> IO ()
stopMultiChainMonitor MultiChainDatabaseMonitor{..} = do
    monitors <- readIORef _multiChainMonitors
    mapM_ stopDatabaseMonitor (HM.elems monitors)

-- | Add monitoring for a specific chain
addChainMonitor
    :: MultiChainDatabaseMonitor
    -> ChainId
    -> R.DB
    -> FilePath
    -> DatabaseMetricsInterface
    -> IO ()
addChainMonitor MultiChainDatabaseMonitor{..} chainId db dbPath metrics = do
    monitor <- startDatabaseMonitor _multiChainConfig db dbPath metrics
    void $ atomicModifyIORef' _multiChainMonitors $ \monitors ->
        (HM.insert chainId monitor monitors, ())

-- | Remove monitoring for a specific chain
removeChainMonitor
    :: MultiChainDatabaseMonitor
    -> ChainId
    -> IO ()
removeChainMonitor MultiChainDatabaseMonitor{..} chainId = do
    maybeMonitor <- atomicModifyIORef' _multiChainMonitors $ \monitors ->
        case HM.lookup chainId monitors of
            Just monitor -> (HM.delete chainId monitors, Just monitor)
            Nothing -> (monitors, Nothing)

    case maybeMonitor of
        Just monitor -> stopDatabaseMonitor monitor
        Nothing -> pure ()

-- -------------------------------------------------------------------------- --
-- Monitoring Statistics

-- | Statistics about the monitoring system itself
data MonitoringStats = MonitoringStats
    { _statsActiveMonitors :: !Int
    -- ^ Number of active database monitors
    , _statsOperationsRecorded :: !Word64
    -- ^ Total operations recorded
    , _statsStatisticsCollections :: !Word64
    -- ^ Number of statistics collection cycles
    , _statsCompactionEvents :: !Word64
    -- ^ Number of compaction events detected
    , _statsStorageChecks :: !Word64
    -- ^ Number of storage checks performed
    } deriving (Eq, Show)

-- | Collect statistics about the monitoring system
collectMonitoringStats :: MultiChainDatabaseMonitor -> IO MonitoringStats
collectMonitoringStats MultiChainDatabaseMonitor{..} = do
    monitors <- readIORef _multiChainMonitors
    let activeCount = HM.size monitors

    -- In a full implementation, we would collect detailed statistics
    -- from each monitor component
    pure $ MonitoringStats
        { _statsActiveMonitors = activeCount
        , _statsOperationsRecorded = 0  -- Would track from metrics
        , _statsStatisticsCollections = 0  -- Would track from collectors
        , _statsCompactionEvents = 0  -- Would track from compaction monitors
        , _statsStorageChecks = 0  -- Would track from storage monitors
        }