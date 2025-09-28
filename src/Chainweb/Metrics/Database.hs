{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Metrics.Database
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Database metrics interface for RocksDB operations
--
module Chainweb.Metrics.Database
( -- * Database Metrics Interface
  DatabaseMetricsInterface(..)
, mkDatabaseMetricsInterface
, withDatabaseMetrics

  -- * Operation Recording
, recordDatabaseRead
, recordDatabaseWrite
, recordDatabaseDelete
, recordCacheHit
, recordCacheMiss
, recordCompactionEvent
, updateDatabaseSize

  -- * Timing Utilities
, timeOperation
, timeOperationWithResult
, timeReadOperation
, timeWriteOperation
, timeDeleteOperation
, timeBatchReadOperation
, timeBatchWriteOperation

  -- * Chain-specific Labeling
, ChainLabel(..)
, operationLabel

  -- * Configuration
, DatabaseMetricsConfig(..)
, defaultDatabaseMetricsConfig

  -- * Statistics Integration
, withDatabaseStatistics
, startDatabaseStatistics
, stopDatabaseStatistics

  -- * Compaction and Storage Monitoring
, withDatabaseMonitoring
, DatabaseMonitoring(..)
) where

import Control.Exception (bracket_, finally, try, SomeException)
import Control.Monad (void, when, sequence_)
import qualified Data.Text as T
import Data.Word (Word64)
import Numeric.Natural (Natural)

import qualified System.Clock as Clock
import System.Metrics.Prometheus.Concurrent.Registry (Registry)
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Counter (Counter)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.Metric.Histogram (Histogram)

import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.Metrics.Registry (DatabaseMetrics(..))

-- For statistics integration
import qualified Database.RocksDB.Base as R

-- -------------------------------------------------------------------------- --
-- Configuration

-- | Configuration for database metrics collection
data DatabaseMetricsConfig = DatabaseMetricsConfig
    { _dbMetricsEnabled :: !Bool
    -- ^ Whether database metrics collection is enabled
    , _dbMetricsSamplingRate :: !Double
    -- ^ Sampling rate for expensive operations (0.0 to 1.0)
    , _dbMetricsPerChainLabels :: !Bool
    -- ^ Whether to include per-chain labels (may increase cardinality)
    , _dbMetricsIncludeStackTrace :: !Bool
    -- ^ Whether to include stack traces for debugging (development only)
    } deriving (Eq, Show)

-- | Default database metrics configuration
defaultDatabaseMetricsConfig :: DatabaseMetricsConfig
defaultDatabaseMetricsConfig = DatabaseMetricsConfig
    { _dbMetricsEnabled = True
    , _dbMetricsSamplingRate = 1.0  -- Track all operations by default
    , _dbMetricsPerChainLabels = True
    , _dbMetricsIncludeStackTrace = False
    }

-- -------------------------------------------------------------------------- --
-- Chain Labels

-- | Type-safe chain identifier for metrics
newtype ChainLabel = ChainLabel { unChainLabel :: T.Text }
    deriving (Eq, Show)

-- | Create chain label from ChainId
chainLabel :: ChainId -> ChainLabel
chainLabel = ChainLabel . chainIdToText

-- | Create operation label for metrics
operationLabel :: T.Text -> T.Text
operationLabel op = "operation=" <> op

-- -------------------------------------------------------------------------- --
-- Database Metrics Interface

-- | Interface for recording database metrics with proper labeling and timing
data DatabaseMetricsInterface = DatabaseMetricsInterface
    { _dmiConfig :: !DatabaseMetricsConfig
    , _dmiMetrics :: !DatabaseMetrics
    , _dmiChainLabel :: !(Maybe ChainLabel)
    }

-- | Create a database metrics interface from DatabaseMetrics and configuration
mkDatabaseMetricsInterface
    :: DatabaseMetricsConfig
    -> DatabaseMetrics
    -> Maybe ChainId
    -> DatabaseMetricsInterface
mkDatabaseMetricsInterface config metrics mChainId = DatabaseMetricsInterface
    { _dmiConfig = config
    , _dmiMetrics = metrics
    , _dmiChainLabel = chainLabel <$> mChainId
    }

-- | Execute an action with database metrics context
withDatabaseMetrics
    :: DatabaseMetricsInterface
    -> (DatabaseMetricsInterface -> IO a)
    -> IO a
withDatabaseMetrics dmi action = action dmi

-- -------------------------------------------------------------------------- --
-- Core Metric Recording Functions

-- | Record a database read operation
recordDatabaseRead :: DatabaseMetricsInterface -> IO ()
recordDatabaseRead DatabaseMetricsInterface{..}
    | _dbMetricsEnabled _dmiConfig =
        void $ Counter.inc (_databaseReads _dmiMetrics)
    | otherwise = pure ()

-- | Record a database write operation
recordDatabaseWrite :: DatabaseMetricsInterface -> IO ()
recordDatabaseWrite DatabaseMetricsInterface{..}
    | _dbMetricsEnabled _dmiConfig =
        void $ Counter.inc (_databaseWrites _dmiMetrics)
    | otherwise = pure ()

-- | Record a database delete operation (counted as writes)
recordDatabaseDelete :: DatabaseMetricsInterface -> IO ()
recordDatabaseDelete = recordDatabaseWrite

-- | Record a cache hit
recordCacheHit :: DatabaseMetricsInterface -> IO ()
recordCacheHit DatabaseMetricsInterface{..}
    | _dbMetricsEnabled _dmiConfig =
        void $ Counter.inc (_databaseCacheHits _dmiMetrics)
    | otherwise = pure ()

-- | Record a cache miss
recordCacheMiss :: DatabaseMetricsInterface -> IO ()
recordCacheMiss DatabaseMetricsInterface{..}
    | _dbMetricsEnabled _dmiConfig =
        void $ Counter.inc (_databaseCacheMisses _dmiMetrics)
    | otherwise = pure ()

-- | Record a compaction event
recordCompactionEvent :: DatabaseMetricsInterface -> Double -> IO ()
recordCompactionEvent DatabaseMetricsInterface{..} duration
    | _dbMetricsEnabled _dmiConfig = do
        void $ Counter.inc (_databaseCompactions _dmiMetrics)
        void $ Histogram.observe duration (_databaseCompactionDuration _dmiMetrics)
    | otherwise = pure ()

-- | Update database size gauge
updateDatabaseSize :: DatabaseMetricsInterface -> Word64 -> IO ()
updateDatabaseSize DatabaseMetricsInterface{..} sizeBytes
    | _dbMetricsEnabled _dmiConfig =
        Gauge.set (fromIntegral sizeBytes) (_databaseSize _dmiMetrics)
    | otherwise = pure ()

-- -------------------------------------------------------------------------- --
-- Timing Utilities

-- | Time an operation and record the duration in the appropriate histogram
timeOperation
    :: DatabaseMetricsInterface
    -> Histogram  -- ^ Which histogram to record to
    -> IO a       -- ^ Operation to time
    -> IO a
timeOperation dmi histogram action = do
    (result, duration) <- timeOperationWithResult action
    when (_dbMetricsEnabled (_dmiConfig dmi)) $
        void $ Histogram.observe duration histogram
    pure result

-- | Time an operation and return both result and duration
timeOperationWithResult :: IO a -> IO (a, Double)
timeOperationWithResult action = do
    startTime <- Clock.getTime Clock.Monotonic
    result <- action
    endTime <- Clock.getTime Clock.Monotonic
    let duration = fromIntegral (Clock.toNanoSecs (Clock.diffTimeSpec endTime startTime)) / 1e9
    pure (result, duration)

-- | Time a database read operation
timeReadOperation
    :: DatabaseMetricsInterface
    -> IO a
    -> IO a
timeReadOperation dmi action = do
    result <- timeOperation dmi (_databaseReadDuration (_dmiMetrics dmi)) action
    recordDatabaseRead dmi
    pure result

-- | Time a database write operation
timeWriteOperation
    :: DatabaseMetricsInterface
    -> IO a
    -> IO a
timeWriteOperation dmi action = do
    result <- timeOperation dmi (_databaseWriteDuration (_dmiMetrics dmi)) action
    recordDatabaseWrite dmi
    pure result

-- | Time a database delete operation
timeDeleteOperation
    :: DatabaseMetricsInterface
    -> IO a
    -> IO a
timeDeleteOperation dmi action = do
    result <- timeOperation dmi (_databaseWriteDuration (_dmiMetrics dmi)) action
    recordDatabaseDelete dmi
    pure result

-- | Time a batch read operation with per-operation metrics
timeBatchReadOperation
    :: DatabaseMetricsInterface
    -> Int  -- ^ Number of operations in batch
    -> IO a
    -> IO a
timeBatchReadOperation dmi count action = do
    result <- timeOperation dmi (_databaseReadDuration (_dmiMetrics dmi)) action
    -- Record each operation in the batch
    sequence_ $ replicate count $ recordDatabaseRead dmi
    pure result

-- | Time a batch write operation with per-operation metrics
timeBatchWriteOperation
    :: DatabaseMetricsInterface
    -> Int  -- ^ Number of operations in batch
    -> IO a
    -> IO a
timeBatchWriteOperation dmi count action = do
    result <- timeOperation dmi (_databaseWriteDuration (_dmiMetrics dmi)) action
    -- Record each operation in the batch
    sequence_ $ replicate count $ recordDatabaseWrite dmi
    pure result

-- -------------------------------------------------------------------------- --
-- Statistics Integration

-- | Start background statistics collection for a RocksDB instance
startDatabaseStatistics
    :: R.DB
    -> DatabaseMetricsInterface
    -> IO (IO ())  -- Returns stop action
startDatabaseStatistics db metrics = do
    -- For now, return a no-op stop action
    -- In a full implementation, this would start the statistics collector
    -- and return the stop action
    pure (pure ())

-- | Stop background statistics collection
stopDatabaseStatistics :: IO () -> IO ()
stopDatabaseStatistics stopAction = stopAction

-- | Execute an action with automatic statistics collection
withDatabaseStatistics
    :: R.DB
    -> DatabaseMetricsInterface
    -> IO a
    -> IO a
withDatabaseStatistics db metrics action = do
    stopAction <- startDatabaseStatistics db metrics
    finally action (stopDatabaseStatistics stopAction)

-- -------------------------------------------------------------------------- --
-- Compaction and Storage Monitoring

-- | Combined database monitoring including compaction and storage
data DatabaseMonitoring = DatabaseMonitoring
    { _monitoringStatistics :: !(IO ())  -- ^ Stop statistics collection
    -- ^ Future: compaction and storage monitor handles would go here
    }

-- | Execute an action with comprehensive database monitoring
withDatabaseMonitoring
    :: R.DB
    -> FilePath  -- ^ Database path for storage monitoring
    -> DatabaseMetricsInterface
    -> IO a
    -> IO a
withDatabaseMonitoring db dbPath metrics action = do
    -- Start statistics collection
    stopStats <- startDatabaseStatistics db metrics

    -- Future: Start compaction and storage monitoring here
    -- stopCompaction <- startCompactionMonitor defaultCompactionConfig db metrics
    -- stopStorage <- startStorageMonitor defaultStorageConfig dbPath metrics

    let monitoring = DatabaseMonitoring
            { _monitoringStatistics = stopStats
            }

    -- Execute action with monitoring
    finally action $ do
        _monitoringStatistics monitoring
        -- Future: stop other monitors here