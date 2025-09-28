# Database Metrics Guide

This guide explains how to use the Chainweb database metrics system to monitor RocksDB performance and storage utilization.

## Overview

The database metrics system provides comprehensive monitoring for RocksDB operations including:

- **Operation Metrics**: Timing and counting of read/write/delete operations
- **Statistics Collection**: Internal RocksDB cache and performance statistics
- **Compaction Monitoring**: Tracking compaction events and performance
- **Storage Monitoring**: Per-chain storage utilization and growth tracking

## Quick Start

### Basic Usage

```haskell
import Chainweb.Metrics.Database
import Chainweb.Metrics.Registry

-- Initialize metrics
databaseMetrics <- initializeDatabaseMetrics registry
let metricsInterface = mkDatabaseMetricsInterface
        defaultDatabaseMetricsConfig
        databaseMetrics
        (Just chainId)

-- Record operations
recordDatabaseRead metricsInterface
recordDatabaseWrite metricsInterface
recordCacheHit metricsInterface
```

### Instrumented Operations

```haskell
import Chainweb.Metrics.Database.RocksDB

-- Create instrumented table
let instrumentedTable = instrumentRocksDbTable
        originalTable
        (Just metricsInterface)
        defaultSamplingConfig
        (Just chainId)

-- Operations are automatically timed and counted
result <- instrumentedTableLookup instrumentedTable key
```

### Comprehensive Monitoring

```haskell
import Chainweb.Metrics.Database.Monitor

-- Start complete monitoring
withDatabaseMonitor defaultDatabaseMonitorConfig db dbPath metrics $ \monitor -> do
    -- Your database operations here
    -- All metrics are collected automatically
```

## Configuration

### Database Metrics Configuration

The system supports extensive configuration through `DatabaseMetricsConfiguration`:

```haskell
config = DatabaseMetricsConfiguration
    { _dbMetricsEnabled = True
    , _dbMetricsOperationConfig = OperationMetricsConfig
        { _opMetricsEnabled = True
        , _opMetricsSamplingRate = 0.01  -- 1% sampling
        , _opMetricsPerChainLabels = True
        , _opMetricsIncludeStackTrace = False
        , _opMetricsBatchThreshold = 10
        }
    , _dbMetricsStatisticsConfig = StatisticsCollectionConfig
        { _statsCollectionEnabled = True
        , _statsCollectionInterval = 30  -- seconds
        , _statsParseFailureRetries = 3
        , _statsLogParseFailures = False
        , _statsCacheMetrics = True
        , _statsPerformanceCounters = True
        }
    , _dbMetricsCompactionConfig = CompactionMonitoringConfig
        { _compactionMonitoringEnabled = True
        , _compactionEventLogging = False
        , _compactionMetricsCollection = True
        , _compactionPollingInterval = 5  -- seconds
        }
    , _dbMetricsStorageConfig = StorageMonitoringConfig
        { _storageMonitoringEnabled = True
        , _storageMonitoringInterval = 60  -- seconds
        , _storagePerChainTracking = True
        , _storageGrowthTracking = True
        , _storageThresholdBytes = Nothing
        , _storageGrowthHistorySize = 10
        }
    }
```

### JSON Configuration

Configuration can be loaded from JSON:

```json
{
  "enabled": true,
  "operations": {
    "enabled": true,
    "samplingRate": 0.01,
    "perChainLabels": true,
    "includeStackTrace": false,
    "batchThreshold": 10
  },
  "statistics": {
    "enabled": true,
    "collectionInterval": 30,
    "parseFailureRetries": 3,
    "logParseFailures": false,
    "cacheMetrics": true,
    "performanceCounters": true
  },
  "compaction": {
    "enabled": true,
    "eventLogging": false,
    "metricsCollection": true,
    "pollingInterval": 5
  },
  "storage": {
    "enabled": true,
    "monitoringInterval": 60,
    "perChainTracking": true,
    "growthTracking": true,
    "thresholdBytes": null,
    "growthHistorySize": 10
  }
}
```

## Available Metrics

### Operation Metrics

| Metric Name | Type | Description |
|-------------|------|-------------|
| `chainweb_database_reads_total` | Counter | Total database read operations |
| `chainweb_database_writes_total` | Counter | Total database write operations |
| `chainweb_database_read_duration_seconds` | Histogram | Read operation latency |
| `chainweb_database_write_duration_seconds` | Histogram | Write operation latency |
| `chainweb_database_cache_hits_total` | Counter | Cache hit count |
| `chainweb_database_cache_misses_total` | Counter | Cache miss count |

### Storage Metrics

| Metric Name | Type | Description |
|-------------|------|-------------|
| `chainweb_database_size_bytes` | Gauge | Current database size |
| `chainweb_database_compactions_total` | Counter | Number of compactions |
| `chainweb_database_compaction_duration_seconds` | Histogram | Compaction duration |

### Per-Chain Labels

All metrics include `chain_id` labels when per-chain tracking is enabled:

```
chainweb_database_reads_total{chain_id="0"} 1000
chainweb_database_reads_total{chain_id="1"} 950
```

## Performance Considerations

### Sampling

The metrics system uses sampling to minimize performance impact:

- **Default sampling rate**: 1% of operations
- **Configurable per operation type**
- **Adaptive sampling** for error conditions
- **Batch operation handling** with thresholds

### Memory Usage

- **Bounded history**: Storage growth tracking keeps limited history
- **Efficient data structures**: HashMap-based per-chain tracking
- **Periodic cleanup**: Old statistics are garbage collected

### Thread Safety

- **STM-based**: All shared state uses Software Transactional Memory
- **Lock-free counters**: Prometheus metrics are thread-safe
- **Background threads**: Monitoring runs in separate threads

## Integration with Existing Code

### Minimal Integration

For existing code, minimal integration can be achieved by wrapping database initialization:

```haskell
-- Before
initializeDatabases chainId dbPath

-- After
initializeDatabases chainId dbPath >>= \db ->
    withDatabaseStatistics db metrics $ \_ ->
        -- Your existing database code
```

### Full Integration

For new code, use instrumented tables directly:

```haskell
-- Create instrumented tables instead of raw RocksDB tables
table <- instrumentRocksDbTable
    <$> newTable db valCodec keyCodec namespace
    <*> pure (Just metricsInterface)
    <*> pure defaultSamplingConfig
    <*> pure (Just chainId)
```

## Troubleshooting

### Common Issues

1. **High memory usage**: Reduce sampling rate or disable per-chain tracking
2. **Performance impact**: Increase sampling interval or disable timing metrics
3. **Missing metrics**: Check that metrics collection is enabled in configuration
4. **Parse failures**: Enable parse failure logging to debug statistics parsing

### Debugging

Enable debug logging for metrics collection:

```haskell
config = defaultConfig
    { _statsLogParseFailures = True
    , _compactionEventLogging = True
    }
```

### Monitoring the Monitor

The system includes meta-metrics to monitor the monitoring system itself:

```haskell
stats <- collectMonitoringStats multiChainMonitor
print $ _statsActiveMonitors stats
print $ _statsOperationsRecorded stats
```

## Best Practices

1. **Start with defaults**: Use `defaultDatabaseMetricsConfiguration` initially
2. **Monitor overhead**: Track the performance impact of metrics collection
3. **Adjust sampling**: Tune sampling rates based on traffic patterns
4. **Use per-chain labels**: Enable for debugging but consider cardinality impact
5. **Regular review**: Periodically review which metrics are actually used

## Example: Multi-Chain Setup

```haskell
-- Initialize multi-chain monitoring
multiMonitor <- startMultiChainMonitor defaultDatabaseMonitorConfig

-- Add monitoring for each chain
forM_ allChainIds $ \chainId -> do
    db <- openChainDatabase chainId
    metrics <- createMetricsInterface chainId
    addChainMonitor multiMonitor chainId db (chainDbPath chainId) metrics

-- Monitor will track all chains independently
-- Stop monitoring when shutting down
stopMultiChainMonitor multiMonitor
```

This provides comprehensive database monitoring across all chains with minimal performance impact.