{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Chainweb.Metrics.Database.Configuration
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Configuration for database metrics collection
--
module Chainweb.Metrics.Database.Configuration
( -- * Main Configuration
  DatabaseMetricsConfiguration(..)
, defaultDatabaseMetricsConfiguration
, validateDatabaseMetricsConfiguration

  -- * Component Configurations
, OperationMetricsConfig(..)
, StatisticsCollectionConfig(..)
, CompactionMonitoringConfig(..)
, StorageMonitoringConfig(..)

  -- * Configuration Loading
, loadDatabaseMetricsConfig
, saveDatabaseMetricsConfig

  -- * Configuration Validation
, ConfigValidationError(..)
, validateConfig
) where

import Control.Exception (Exception)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)

-- -------------------------------------------------------------------------- --
-- Configuration Types

-- | Main configuration for database metrics
data DatabaseMetricsConfiguration = DatabaseMetricsConfiguration
    { _dbMetricsEnabled :: !Bool
    -- ^ Master enable/disable switch for all database metrics
    , _dbMetricsOperationConfig :: !OperationMetricsConfig
    -- ^ Configuration for operation timing and counting
    , _dbMetricsStatisticsConfig :: !StatisticsCollectionConfig
    -- ^ Configuration for RocksDB statistics collection
    , _dbMetricsCompactionConfig :: !CompactionMonitoringConfig
    -- ^ Configuration for compaction monitoring
    , _dbMetricsStorageConfig :: !StorageMonitoringConfig
    -- ^ Configuration for storage monitoring
    } deriving (Eq, Show, Generic)

instance FromJSON DatabaseMetricsConfiguration where
    parseJSON = withObject "DatabaseMetricsConfiguration" $ \o -> DatabaseMetricsConfiguration
        <$> o .:? "enabled" .!= True
        <*> o .:? "operations" .!= defaultOperationMetricsConfig
        <*> o .:? "statistics" .!= defaultStatisticsCollectionConfig
        <*> o .:? "compaction" .!= defaultCompactionMonitoringConfig
        <*> o .:? "storage" .!= defaultStorageMonitoringConfig

instance ToJSON DatabaseMetricsConfiguration where
    toJSON DatabaseMetricsConfiguration{..} = object
        [ "enabled" .= _dbMetricsEnabled
        , "operations" .= _dbMetricsOperationConfig
        , "statistics" .= _dbMetricsStatisticsConfig
        , "compaction" .= _dbMetricsCompactionConfig
        , "storage" .= _dbMetricsStorageConfig
        ]

-- | Configuration for operation metrics (timing, counting)
data OperationMetricsConfig = OperationMetricsConfig
    { _opMetricsEnabled :: !Bool
    -- ^ Enable operation metrics collection
    , _opMetricsSamplingRate :: !Double
    -- ^ Sampling rate (0.0 to 1.0)
    , _opMetricsPerChainLabels :: !Bool
    -- ^ Include per-chain labels
    , _opMetricsIncludeStackTrace :: !Bool
    -- ^ Include stack traces for debugging
    , _opMetricsBatchThreshold :: !Int
    -- ^ Threshold for batch operation sampling
    } deriving (Eq, Show, Generic)

instance FromJSON OperationMetricsConfig
instance ToJSON OperationMetricsConfig

-- | Configuration for statistics collection
data StatisticsCollectionConfig = StatisticsCollectionConfig
    { _statsCollectionEnabled :: !Bool
    -- ^ Enable statistics collection
    , _statsCollectionInterval :: !Int
    -- ^ Collection interval in seconds
    , _statsParseFailureRetries :: !Int
    -- ^ Number of retries for parse failures
    , _statsLogParseFailures :: !Bool
    -- ^ Log parsing failures
    , _statsCacheMetrics :: !Bool
    -- ^ Collect cache-related metrics
    , _statsPerformanceCounters :: !Bool
    -- ^ Collect performance counters
    } deriving (Eq, Show, Generic)

instance FromJSON StatisticsCollectionConfig
instance ToJSON StatisticsCollectionConfig

-- | Configuration for compaction monitoring
data CompactionMonitoringConfig = CompactionMonitoringConfig
    { _compactionMonitoringEnabled :: !Bool
    -- ^ Enable compaction monitoring
    , _compactionEventLogging :: !Bool
    -- ^ Log compaction events
    , _compactionMetricsCollection :: !Bool
    -- ^ Collect compaction metrics
    , _compactionPollingInterval :: !Int
    -- ^ Polling interval in seconds
    } deriving (Eq, Show, Generic)

instance FromJSON CompactionMonitoringConfig
instance ToJSON CompactionMonitoringConfig

-- | Configuration for storage monitoring
data StorageMonitoringConfig = StorageMonitoringConfig
    { _storageMonitoringEnabled :: !Bool
    -- ^ Enable storage monitoring
    , _storageMonitoringInterval :: !Int
    -- ^ Monitoring interval in seconds
    , _storagePerChainTracking :: !Bool
    -- ^ Track per-chain storage
    , _storageGrowthTracking :: !Bool
    -- ^ Track storage growth rates
    , _storageThresholdBytes :: !(Maybe Word64)
    -- ^ Storage threshold for alerts
    , _storageGrowthHistorySize :: !Int
    -- ^ Number of measurements to keep for growth calculation
    } deriving (Eq, Show, Generic)

instance FromJSON StorageMonitoringConfig
instance ToJSON StorageMonitoringConfig

-- -------------------------------------------------------------------------- --
-- Default Configurations

-- | Default database metrics configuration
defaultDatabaseMetricsConfiguration :: DatabaseMetricsConfiguration
defaultDatabaseMetricsConfiguration = DatabaseMetricsConfiguration
    { _dbMetricsEnabled = True
    , _dbMetricsOperationConfig = defaultOperationMetricsConfig
    , _dbMetricsStatisticsConfig = defaultStatisticsCollectionConfig
    , _dbMetricsCompactionConfig = defaultCompactionMonitoringConfig
    , _dbMetricsStorageConfig = defaultStorageMonitoringConfig
    }

-- | Default operation metrics configuration
defaultOperationMetricsConfig :: OperationMetricsConfig
defaultOperationMetricsConfig = OperationMetricsConfig
    { _opMetricsEnabled = True
    , _opMetricsSamplingRate = 0.01  -- 1% sampling by default
    , _opMetricsPerChainLabels = True
    , _opMetricsIncludeStackTrace = False
    , _opMetricsBatchThreshold = 10
    }

-- | Default statistics collection configuration
defaultStatisticsCollectionConfig :: StatisticsCollectionConfig
defaultStatisticsCollectionConfig = StatisticsCollectionConfig
    { _statsCollectionEnabled = True
    , _statsCollectionInterval = 30
    , _statsParseFailureRetries = 3
    , _statsLogParseFailures = False
    , _statsCacheMetrics = True
    , _statsPerformanceCounters = True
    }

-- | Default compaction monitoring configuration
defaultCompactionMonitoringConfig :: CompactionMonitoringConfig
defaultCompactionMonitoringConfig = CompactionMonitoringConfig
    { _compactionMonitoringEnabled = True
    , _compactionEventLogging = False
    , _compactionMetricsCollection = True
    , _compactionPollingInterval = 5
    }

-- | Default storage monitoring configuration
defaultStorageMonitoringConfig :: StorageMonitoringConfig
defaultStorageMonitoringConfig = StorageMonitoringConfig
    { _storageMonitoringEnabled = True
    , _storageMonitoringInterval = 60
    , _storagePerChainTracking = True
    , _storageGrowthTracking = True
    , _storageThresholdBytes = Nothing
    , _storageGrowthHistorySize = 10
    }

-- -------------------------------------------------------------------------- --
-- Configuration Loading and Saving

-- | Load database metrics configuration from JSON
loadDatabaseMetricsConfig :: ByteString -> Either String DatabaseMetricsConfiguration
loadDatabaseMetricsConfig = eitherDecode

-- | Save database metrics configuration to JSON
saveDatabaseMetricsConfig :: DatabaseMetricsConfiguration -> ByteString
saveDatabaseMetricsConfig = encode

-- -------------------------------------------------------------------------- --
-- Configuration Validation

-- | Configuration validation errors
data ConfigValidationError
    = InvalidSamplingRate Double
    | InvalidInterval Int
    | InvalidThreshold Word64
    | InvalidHistorySize Int
    | MissingRequiredField T.Text
    deriving (Eq, Show)

instance Exception ConfigValidationError

-- | Validate database metrics configuration
validateDatabaseMetricsConfiguration
    :: DatabaseMetricsConfiguration
    -> Either ConfigValidationError DatabaseMetricsConfiguration
validateDatabaseMetricsConfiguration config@DatabaseMetricsConfiguration{..} = do
    _ <- validateOperationConfig _dbMetricsOperationConfig
    _ <- validateStatisticsConfig _dbMetricsStatisticsConfig
    _ <- validateCompactionConfig _dbMetricsCompactionConfig
    _ <- validateStorageConfig _dbMetricsStorageConfig
    pure config

-- | Validate operation metrics configuration
validateOperationConfig :: OperationMetricsConfig -> Either ConfigValidationError OperationMetricsConfig
validateOperationConfig config@OperationMetricsConfig{..}
    | _opMetricsSamplingRate < 0.0 || _opMetricsSamplingRate > 1.0 =
        Left (InvalidSamplingRate _opMetricsSamplingRate)
    | _opMetricsBatchThreshold < 1 =
        Left (InvalidInterval _opMetricsBatchThreshold)
    | otherwise = Right config

-- | Validate statistics collection configuration
validateStatisticsConfig :: StatisticsCollectionConfig -> Either ConfigValidationError StatisticsCollectionConfig
validateStatisticsConfig config@StatisticsCollectionConfig{..}
    | _statsCollectionInterval < 1 =
        Left (InvalidInterval _statsCollectionInterval)
    | _statsParseFailureRetries < 0 =
        Left (InvalidInterval _statsParseFailureRetries)
    | otherwise = Right config

-- | Validate compaction monitoring configuration
validateCompactionConfig :: CompactionMonitoringConfig -> Either ConfigValidationError CompactionMonitoringConfig
validateCompactionConfig config@CompactionMonitoringConfig{..}
    | _compactionPollingInterval < 1 =
        Left (InvalidInterval _compactionPollingInterval)
    | otherwise = Right config

-- | Validate storage monitoring configuration
validateStorageConfig :: StorageMonitoringConfig -> Either ConfigValidationError StorageMonitoringConfig
validateStorageConfig config@StorageMonitoringConfig{..}
    | _storageMonitoringInterval < 1 =
        Left (InvalidInterval _storageMonitoringInterval)
    | _storageGrowthHistorySize < 2 =
        Left (InvalidHistorySize _storageGrowthHistorySize)
    | otherwise = Right config

-- | General configuration validation function
validateConfig :: DatabaseMetricsConfiguration -> Either ConfigValidationError ()
validateConfig config = do
    _ <- validateDatabaseMetricsConfiguration config
    pure ()