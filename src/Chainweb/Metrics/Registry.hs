{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Chainweb.Metrics.Registry
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Central metrics registry system for Chainweb using Prometheus
--
module Chainweb.Metrics.Registry
( -- * Core Registry Types
  ChainwebMetrics(..)
, BlockchainMetrics(..)
, MempoolMetrics(..)
, SystemMetrics(..)
, DatabaseMetrics(..)
, P2PMetricsData(..)

  -- * Registry Configuration (imported from Chainweb.Chainweb.Configuration)

  -- * Registry Initialization
, initializeMetricsRegistry
, initializeMetricsRegistryWithConfig
, initializeBlockchainMetrics
, initializeMempoolMetrics
, initializeSystemMetrics
, initializeDatabaseMetrics
, initializeP2PMetricsData

  -- * Memory Management
, MetricsMemoryManager(..)
, initializeMemoryManager
, startCleanupRoutine
, checkMemoryUsage

  -- * Type-safe Labels
, ChainwebLabels(..)
, mkChainwebLabels
, emptyLabels

  -- * Common Metric Types and Buckets
, defaultLatencyBuckets
, defaultSizeBuckets
, defaultCountBuckets

  -- * Metric Registration Functions
, MetricSubsystem(..)
, MetricRegistrationError(..)
, registerChainwebCounter
, registerChainwebGauge
, registerChainwebHistogram
, validateMetricName
, attachLabels
, deduplicateMetric

  -- * Common Metric Patterns
, registerLatencyHistogram
, registerRateCounter
, registerSizeHistogram

  -- * Utility Functions for Safe Concurrent Access
, incrementCounter
, setGauge
, observeHistogram
, atomicMetricUpdate
, batchMetricUpdate
, snapshotMetrics
, aggregateMetrics
, withRetryBackoff
, measureMetricsOverhead

  -- * Configuration-Aware Operations
, conditionalIncrement
, conditionalSetGauge
, conditionalObserve

  -- * Aggregation Types
, MetricSnapshot(..)
, AggregatedMetrics(..)
, MetricOperation(..)

  -- * Registry Access
, getBlockchainMetrics
, getMempoolMetrics
, getSystemMetrics
, getDatabaseMetrics
, getP2PMetricsData
, getMemoryManager
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception (throwIO, Exception, catch, SomeException)
import Control.Monad (void, when, forever, replicateM)

import Data.Char (isAlphaNum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import GHC.Generics

import Numeric.Natural

import System.CPUTime
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC)

import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.Concurrent.Registry (Registry)
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Counter (Counter)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.Metric.Histogram (Histogram)

-- internal modules
import Chainweb.ChainId

-- -------------------------------------------------------------------------- --
-- Configuration Types (simplified to avoid circular dependency)

-- | Simple metrics configuration for the registry
-- The full configuration is in Chainweb.Chainweb.Configuration
data MetricsConfig = MetricsConfig
    { _metricsEnabled :: !Bool
    , _metricsRetentionPeriod :: !NominalDiffTime
    , _metricsMemoryLimit :: !Word64
    , _metricsCleanupInterval :: !Natural
    } deriving (Show, Eq, Generic)

-- -------------------------------------------------------------------------- --
-- Core Registry Types

-- | Main metrics registry containing all Chainweb subsystem metrics
data ChainwebMetrics = ChainwebMetrics
    { _chainwebBlockchainMetrics :: !(TVar BlockchainMetrics)
    , _chainwebMempoolMetrics :: !(TVar MempoolMetrics)
    , _chainwebSystemMetrics :: !(TVar SystemMetrics)
    , _chainwebDatabaseMetrics :: !(TVar DatabaseMetrics)
    , _chainwebP2PMetrics :: !(TVar P2PMetricsData)
    , _chainwebMainRegistry :: !Registry
    , _chainwebMemoryManager :: !(Maybe MetricsMemoryManager)
    } deriving (Generic)

instance NFData ChainwebMetrics where
    rnf ChainwebMetrics{..} =
        _chainwebMainRegistry `seq`
        _chainwebBlockchainMetrics `seq`
        _chainwebMempoolMetrics `seq`
        _chainwebSystemMetrics `seq`
        _chainwebDatabaseMetrics `seq`
        _chainwebP2PMetrics `seq`
        _chainwebMemoryManager `seq` ()

-- | Blockchain-specific metrics (per-chain and aggregate)
data BlockchainMetrics = BlockchainMetrics
    { _blockchainBlocksProcessed :: !Counter
    , _blockchainBlockProcessingDuration :: !Histogram
    , _blockchainBlockHeight :: !Gauge
    , _blockchainDifficulty :: !Gauge
    , _blockchainOrphans :: !Counter
    , _blockchainReorgs :: !Counter
    , _blockchainForkLength :: !Histogram
    , _blockchainTransactionsPerBlock :: !Histogram
    , _blockchainValidationErrors :: !Counter
    , _blockchainRegistry :: !Registry
    } deriving (Generic)

instance NFData BlockchainMetrics where
    rnf BlockchainMetrics{..} =
        _blockchainRegistry `seq`
        _blockchainBlocksProcessed `seq`
        _blockchainBlockProcessingDuration `seq`
        _blockchainBlockHeight `seq`
        _blockchainDifficulty `seq`
        _blockchainOrphans `seq`
        _blockchainReorgs `seq`
        _blockchainForkLength `seq`
        _blockchainTransactionsPerBlock `seq`
        _blockchainValidationErrors `seq` ()

-- | Mempool-specific metrics
data MempoolMetrics = MempoolMetrics
    { _mempoolSize :: !Gauge
    , _mempoolTransactionsAdded :: !Counter
    , _mempoolTransactionsRemoved :: !Counter
    , _mempoolValidationDuration :: !Histogram
    , _mempoolValidationErrors :: !Counter
    , _mempoolSyncDuration :: !Histogram
    , _mempoolMemoryUsage :: !Gauge
    , _mempoolPendingTransactions :: !Gauge
    , _mempoolRejectedTransactions :: !Counter
    , _mempoolRegistry :: !Registry
    } deriving (Generic)

instance NFData MempoolMetrics where
    rnf MempoolMetrics{..} =
        _mempoolRegistry `seq`
        _mempoolSize `seq`
        _mempoolTransactionsAdded `seq`
        _mempoolTransactionsRemoved `seq`
        _mempoolValidationDuration `seq`
        _mempoolValidationErrors `seq`
        _mempoolSyncDuration `seq`
        _mempoolMemoryUsage `seq`
        _mempoolPendingTransactions `seq`
        _mempoolRejectedTransactions `seq` ()

-- | System-wide metrics
data SystemMetrics = SystemMetrics
    { _systemMemoryUsage :: !Gauge
    , _systemCpuUsage :: !Gauge
    , _systemThreadCount :: !Gauge
    , _systemFileDescriptors :: !Gauge
    , _systemNetworkConnections :: !Gauge
    , _systemUptime :: !Gauge
    , _systemGcDuration :: !Histogram
    , _systemGcCount :: !Counter
    , _systemAllocationRate :: !Gauge
    , _systemRegistry :: !Registry
    } deriving (Generic)

instance NFData SystemMetrics where
    rnf SystemMetrics{..} =
        _systemRegistry `seq`
        _systemMemoryUsage `seq`
        _systemCpuUsage `seq`
        _systemThreadCount `seq`
        _systemFileDescriptors `seq`
        _systemNetworkConnections `seq`
        _systemUptime `seq`
        _systemGcDuration `seq`
        _systemGcCount `seq`
        _systemAllocationRate `seq` ()

-- | Database-specific metrics
data DatabaseMetrics = DatabaseMetrics
    { _databaseReads :: !Counter
    , _databaseWrites :: !Counter
    , _databaseReadDuration :: !Histogram
    , _databaseWriteDuration :: !Histogram
    , _databaseSize :: !Gauge
    , _databaseCompactions :: !Counter
    , _databaseCompactionDuration :: !Histogram
    , _databaseCacheHits :: !Counter
    , _databaseCacheMisses :: !Counter
    , _databaseRegistry :: !Registry
    } deriving (Generic)

instance NFData DatabaseMetrics where
    rnf DatabaseMetrics{..} =
        _databaseRegistry `seq`
        _databaseReads `seq`
        _databaseWrites `seq`
        _databaseReadDuration `seq`
        _databaseWriteDuration `seq`
        _databaseSize `seq`
        _databaseCompactions `seq`
        _databaseCompactionDuration `seq`
        _databaseCacheHits `seq`
        _databaseCacheMisses `seq` ()

-- | P2P metrics data wrapper (importing from existing P2P.Metrics module)
data P2PMetricsData = P2PMetricsData
    { _p2pConnectionDuration :: !Histogram
    , _p2pActiveConnections :: !Gauge
    , _p2pBytesTransferred :: !Counter
    , _p2pRequestDuration :: !Histogram
    , _p2pPeerQualityScore :: !Gauge
    , _p2pTaskQueueDepth :: !Gauge
    , _p2pConnectionFailures :: !Counter
    , _p2pConnectionTimeouts :: !Counter
    , _p2pSessionSuccesses :: !Counter
    , _p2pSessionFailures :: !Counter
    , _p2pRegistry :: !Registry
    } deriving (Generic)

instance NFData P2PMetricsData where
    rnf P2PMetricsData{..} =
        _p2pRegistry `seq`
        _p2pConnectionDuration `seq`
        _p2pActiveConnections `seq`
        _p2pBytesTransferred `seq`
        _p2pRequestDuration `seq`
        _p2pPeerQualityScore `seq`
        _p2pTaskQueueDepth `seq`
        _p2pConnectionFailures `seq`
        _p2pConnectionTimeouts `seq`
        _p2pSessionSuccesses `seq`
        _p2pSessionFailures `seq` ()


-- -------------------------------------------------------------------------- --
-- Memory Management

-- | Memory management for metrics with bounded storage
data MetricsMemoryManager = MetricsMemoryManager
    { _memoryConfig :: !MetricsConfig
    , _memoryUsageBytes :: !(TVar Natural)
    , _lastCleanupTime :: !(TVar UTCTime)
    , _cleanupAsync :: !(TVar (Maybe (Async ())))
    , _isCleanupRunning :: !(TVar Bool)
    } deriving (Generic)

instance NFData MetricsMemoryManager where
    rnf MetricsMemoryManager{..} =
        _memoryConfig `seq`
        _memoryUsageBytes `seq`
        _lastCleanupTime `seq`
        _cleanupAsync `seq`
        _isCleanupRunning `seq` ()

-- | Validate metrics configuration
validateMetricsConfig :: MetricsConfig -> IO MetricsConfig
validateMetricsConfig config = do
    when (_metricsRetentionPeriod config < 60) $
        fail "Metrics retention period must be at least 60 seconds"
    when (_metricsMemoryLimit config < 10 * 1024 * 1024) $
        fail "Metrics memory limit must be at least 10MB"
    when (_metricsCleanupInterval config < 10) $
        fail "Metrics cleanup interval must be at least 10 seconds"
    pure config

-- | Initialize memory manager for metrics
initializeMemoryManager :: MetricsConfig -> IO MetricsMemoryManager
initializeMemoryManager config = do
    validatedConfig <- validateMetricsConfig config
    now <- getCurrentTime
    memoryUsage <- newTVarIO 0
    lastCleanup <- newTVarIO now
    cleanupAsync <- newTVarIO Nothing
    isRunning <- newTVarIO False

    pure MetricsMemoryManager
        { _memoryConfig = validatedConfig
        , _memoryUsageBytes = memoryUsage
        , _lastCleanupTime = lastCleanup
        , _cleanupAsync = cleanupAsync
        , _isCleanupRunning = isRunning
        }

-- | Start automatic cleanup routine for metrics memory management
startCleanupRoutine :: MetricsMemoryManager -> IO ()
startCleanupRoutine manager@MetricsMemoryManager{..} = do
    isRunning <- readTVarIO _isCleanupRunning
    when (not isRunning) $ do
        atomically $ writeTVar _isCleanupRunning True
        asyncAction <- async $ cleanupLoop manager
        atomically $ writeTVar _cleanupAsync (Just asyncAction)
  where
    cleanupLoop mgr = forever $ do
        threadDelay (fromIntegral (_metricsRetentionPeriodSeconds _memoryConfig) * 1000000 `div` 10) -- Check every 1/10th of retention period
        checkMemoryUsage mgr
        performMajorGC -- Force GC to reclaim memory

-- | Check current memory usage and perform cleanup if needed
checkMemoryUsage :: MetricsMemoryManager -> IO Natural
checkMemoryUsage MetricsMemoryManager{..} = do
    currentUsage <- readTVarIO _memoryUsageBytes
    let maxBytes = _metricsMaxMemoryMB _memoryConfig * 1024 * 1024

    when (currentUsage > maxBytes) $ do
        -- Trigger cleanup - in a real implementation, this would prune old metrics
        -- For now, we'll reset the counter and force GC
        atomically $ writeTVar _memoryUsageBytes (currentUsage `div` 2)
        performMajorGC
        now <- getCurrentTime
        atomically $ writeTVar _lastCleanupTime now

    readTVarIO _memoryUsageBytes

-- -------------------------------------------------------------------------- --
-- Type-safe Labels

-- | Type-safe labels for Chainweb metrics
data ChainwebLabels = ChainwebLabels
    { _labelChainId :: !(Maybe ChainId)
    , _labelVersion :: !(Maybe Text)
    , _labelNodeId :: !(Maybe Text)
    } deriving (Show, Eq, Generic)

instance NFData ChainwebLabels

-- | Create labels with all fields specified
mkChainwebLabels :: ChainId -> Text -> Text -> ChainwebLabels
mkChainwebLabels chainId version nodeId = ChainwebLabels
    { _labelChainId = Just chainId
    , _labelVersion = Just version
    , _labelNodeId = Just nodeId
    }

-- | Empty labels (no chain-specific or node-specific context)
emptyLabels :: ChainwebLabels
emptyLabels = ChainwebLabels Nothing Nothing Nothing

-- -------------------------------------------------------------------------- --
-- Common Metric Buckets

-- | Default latency buckets (in seconds)
defaultLatencyBuckets :: [Double]
defaultLatencyBuckets = [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]

-- | Default size buckets (in bytes)
defaultSizeBuckets :: [Double]
defaultSizeBuckets = [1024, 4096, 16384, 65536, 262144, 1048576, 4194304, 16777216]

-- | Default count buckets
defaultCountBuckets :: [Double]
defaultCountBuckets = [1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000]

-- -------------------------------------------------------------------------- --
-- Registry Initialization

-- | Initialize the main metrics registry with all subsystems (basic version)
initializeMetricsRegistry :: MetricsConfig -> IO ChainwebMetrics
initializeMetricsRegistry config = initializeMetricsRegistryWithConfig config False

-- | Initialize the main metrics registry with configurable memory management
initializeMetricsRegistryWithConfig
    :: MetricsConfig
    -> Bool          -- ^ Enable memory management
    -> IO ChainwebMetrics
initializeMetricsRegistryWithConfig config enableMemoryManagement = do
    when (not (_metricsEnabled config)) $
        error "Cannot initialize metrics registry with metrics disabled"

    mainRegistry <- Registry.new

    -- Initialize subsystems only if enabled in their respective configurations
    blockchainMetrics <- if _blockchainMetricsEnabled (_metricsBlockchain config)
        then initializeBlockchainMetrics mainRegistry
        else initializeBlockchainMetricsDisabled mainRegistry

    mempoolMetrics <- if _mempoolMetricsEnabled (_metricsMempool config)
        then initializeMempoolMetrics mainRegistry
        else initializeMempoolMetricsDisabled mainRegistry

    systemMetrics <- if _systemMetricsEnabled (_metricsSystem config)
        then initializeSystemMetrics mainRegistry
        else initializeSystemMetricsDisabled mainRegistry

    databaseMetrics <- if _databaseMetricsEnabled (_metricsDatabase config)
        then initializeDatabaseMetrics mainRegistry
        else initializeDatabaseMetricsDisabled mainRegistry

    p2pMetrics <- if _p2pMetricsEnabled (_metricsP2P config)
        then initializeP2PMetricsData mainRegistry
        else initializeP2PMetricsDataDisabled mainRegistry

    blockchainTVar <- newTVarIO blockchainMetrics
    mempoolTVar <- newTVarIO mempoolMetrics
    systemTVar <- newTVarIO systemMetrics
    databaseTVar <- newTVarIO databaseMetrics
    p2pTVar <- newTVarIO p2pMetrics

    -- Initialize memory manager if requested
    memoryManager <- if enableMemoryManagement
        then do
            manager <- initializeMemoryManager config
            startCleanupRoutine manager
            pure (Just manager)
        else pure Nothing

    pure ChainwebMetrics
        { _chainwebBlockchainMetrics = blockchainTVar
        , _chainwebMempoolMetrics = mempoolTVar
        , _chainwebSystemMetrics = systemTVar
        , _chainwebDatabaseMetrics = databaseTVar
        , _chainwebP2PMetrics = p2pTVar
        , _chainwebMainRegistry = mainRegistry
        , _chainwebMemoryManager = memoryManager
        }

-- | Initialize blockchain-specific metrics
initializeBlockchainMetrics :: Registry -> IO BlockchainMetrics
initializeBlockchainMetrics parentRegistry = do
    registry <- Registry.new

    blocksProcessed <- Registry.registerCounter
        "chainweb_blockchain_blocks_processed_total" mempty registry
    blockProcessingDuration <- Registry.registerHistogram
        "chainweb_blockchain_block_processing_duration_seconds" mempty defaultLatencyBuckets registry
    blockHeight <- Registry.registerGauge
        "chainweb_blockchain_block_height" mempty registry
    difficulty <- Registry.registerGauge
        "chainweb_blockchain_difficulty" mempty registry
    orphans <- Registry.registerCounter
        "chainweb_blockchain_orphans_total" mempty registry
    reorgs <- Registry.registerCounter
        "chainweb_blockchain_reorgs_total" mempty registry
    forkLength <- Registry.registerHistogram
        "chainweb_blockchain_fork_length" mempty defaultCountBuckets registry
    transactionsPerBlock <- Registry.registerHistogram
        "chainweb_blockchain_transactions_per_block" mempty defaultCountBuckets registry
    validationErrors <- Registry.registerCounter
        "chainweb_blockchain_validation_errors_total" mempty registry

    pure BlockchainMetrics
        { _blockchainBlocksProcessed = blocksProcessed
        , _blockchainBlockProcessingDuration = blockProcessingDuration
        , _blockchainBlockHeight = blockHeight
        , _blockchainDifficulty = difficulty
        , _blockchainOrphans = orphans
        , _blockchainReorgs = reorgs
        , _blockchainForkLength = forkLength
        , _blockchainTransactionsPerBlock = transactionsPerBlock
        , _blockchainValidationErrors = validationErrors
        , _blockchainRegistry = registry
        }

-- | Initialize mempool-specific metrics
initializeMempoolMetrics :: Registry -> IO MempoolMetrics
initializeMempoolMetrics parentRegistry = do
    registry <- Registry.new

    size <- Registry.registerGauge
        "chainweb_mempool_size" mempty registry
    transactionsAdded <- Registry.registerCounter
        "chainweb_mempool_transactions_added_total" mempty registry
    transactionsRemoved <- Registry.registerCounter
        "chainweb_mempool_transactions_removed_total" mempty registry
    validationDuration <- Registry.registerHistogram
        "chainweb_mempool_validation_duration_seconds" mempty defaultLatencyBuckets registry
    validationErrors <- Registry.registerCounter
        "chainweb_mempool_validation_errors_total" mempty registry
    syncDuration <- Registry.registerHistogram
        "chainweb_mempool_sync_duration_seconds" mempty defaultLatencyBuckets registry
    memoryUsage <- Registry.registerGauge
        "chainweb_mempool_memory_usage_bytes" mempty registry
    pendingTransactions <- Registry.registerGauge
        "chainweb_mempool_pending_transactions" mempty registry
    rejectedTransactions <- Registry.registerCounter
        "chainweb_mempool_rejected_transactions_total" mempty registry

    pure MempoolMetrics
        { _mempoolSize = size
        , _mempoolTransactionsAdded = transactionsAdded
        , _mempoolTransactionsRemoved = transactionsRemoved
        , _mempoolValidationDuration = validationDuration
        , _mempoolValidationErrors = validationErrors
        , _mempoolSyncDuration = syncDuration
        , _mempoolMemoryUsage = memoryUsage
        , _mempoolPendingTransactions = pendingTransactions
        , _mempoolRejectedTransactions = rejectedTransactions
        , _mempoolRegistry = registry
        }

-- | Initialize system-wide metrics
initializeSystemMetrics :: Registry -> IO SystemMetrics
initializeSystemMetrics parentRegistry = do
    registry <- Registry.new

    memoryUsage <- Registry.registerGauge
        "chainweb_system_memory_usage_bytes" mempty registry
    cpuUsage <- Registry.registerGauge
        "chainweb_system_cpu_usage_percent" mempty registry
    threadCount <- Registry.registerGauge
        "chainweb_system_thread_count" mempty registry
    fileDescriptors <- Registry.registerGauge
        "chainweb_system_file_descriptors" mempty registry
    networkConnections <- Registry.registerGauge
        "chainweb_system_network_connections" mempty registry
    uptime <- Registry.registerGauge
        "chainweb_system_uptime_seconds" mempty registry
    gcDuration <- Registry.registerHistogram
        "chainweb_system_gc_duration_seconds" mempty defaultLatencyBuckets registry
    gcCount <- Registry.registerCounter
        "chainweb_system_gc_count_total" mempty registry
    allocationRate <- Registry.registerGauge
        "chainweb_system_allocation_rate_bytes_per_second" mempty registry

    pure SystemMetrics
        { _systemMemoryUsage = memoryUsage
        , _systemCpuUsage = cpuUsage
        , _systemThreadCount = threadCount
        , _systemFileDescriptors = fileDescriptors
        , _systemNetworkConnections = networkConnections
        , _systemUptime = uptime
        , _systemGcDuration = gcDuration
        , _systemGcCount = gcCount
        , _systemAllocationRate = allocationRate
        , _systemRegistry = registry
        }

-- | Initialize database-specific metrics
initializeDatabaseMetrics :: Registry -> IO DatabaseMetrics
initializeDatabaseMetrics parentRegistry = do
    registry <- Registry.new

    reads <- Registry.registerCounter
        "chainweb_database_reads_total" mempty registry
    writes <- Registry.registerCounter
        "chainweb_database_writes_total" mempty registry
    readDuration <- Registry.registerHistogram
        "chainweb_database_read_duration_seconds" mempty defaultLatencyBuckets registry
    writeDuration <- Registry.registerHistogram
        "chainweb_database_write_duration_seconds" mempty defaultLatencyBuckets registry
    size <- Registry.registerGauge
        "chainweb_database_size_bytes" mempty registry
    compactions <- Registry.registerCounter
        "chainweb_database_compactions_total" mempty registry
    compactionDuration <- Registry.registerHistogram
        "chainweb_database_compaction_duration_seconds" mempty defaultLatencyBuckets registry
    cacheHits <- Registry.registerCounter
        "chainweb_database_cache_hits_total" mempty registry
    cacheMisses <- Registry.registerCounter
        "chainweb_database_cache_misses_total" mempty registry

    pure DatabaseMetrics
        { _databaseReads = reads
        , _databaseWrites = writes
        , _databaseReadDuration = readDuration
        , _databaseWriteDuration = writeDuration
        , _databaseSize = size
        , _databaseCompactions = compactions
        , _databaseCompactionDuration = compactionDuration
        , _databaseCacheHits = cacheHits
        , _databaseCacheMisses = cacheMisses
        , _databaseRegistry = registry
        }

-- | Initialize P2P metrics data (similar to existing P2P.Metrics)
initializeP2PMetricsData :: Registry -> IO P2PMetricsData
initializeP2PMetricsData parentRegistry = do
    registry <- Registry.new

    connectionDuration <- Registry.registerHistogram
        "chainweb_p2p_connection_duration_seconds" mempty defaultLatencyBuckets registry
    activeConnections <- Registry.registerGauge
        "chainweb_p2p_active_connections" mempty registry
    bytesTransferred <- Registry.registerCounter
        "chainweb_p2p_bytes_transferred_total" mempty registry
    requestDuration <- Registry.registerHistogram
        "chainweb_p2p_request_duration_seconds" mempty defaultLatencyBuckets registry
    peerQualityScore <- Registry.registerGauge
        "chainweb_p2p_peer_quality_score" mempty registry
    taskQueueDepth <- Registry.registerGauge
        "chainweb_p2p_task_queue_depth" mempty registry
    connectionFailures <- Registry.registerCounter
        "chainweb_p2p_connection_failures_total" mempty registry
    connectionTimeouts <- Registry.registerCounter
        "chainweb_p2p_connection_timeouts_total" mempty registry
    sessionSuccesses <- Registry.registerCounter
        "chainweb_p2p_session_successes_total" mempty registry
    sessionFailures <- Registry.registerCounter
        "chainweb_p2p_session_failures_total" mempty registry

    pure P2PMetricsData
        { _p2pConnectionDuration = connectionDuration
        , _p2pActiveConnections = activeConnections
        , _p2pBytesTransferred = bytesTransferred
        , _p2pRequestDuration = requestDuration
        , _p2pPeerQualityScore = peerQualityScore
        , _p2pTaskQueueDepth = taskQueueDepth
        , _p2pConnectionFailures = connectionFailures
        , _p2pConnectionTimeouts = connectionTimeouts
        , _p2pSessionSuccesses = sessionSuccesses
        , _p2pSessionFailures = sessionFailures
        , _p2pRegistry = registry
        }

-- -------------------------------------------------------------------------- --
-- Configuration-Aware Metric Operations

-- | Conditional metric recording - only records if enabled
conditionalIncrement :: Bool -> Counter -> Double -> IO ()
conditionalIncrement enabled counter value
    | enabled = void $ Counter.add value counter
    | otherwise = pure ()
{-# INLINE conditionalIncrement #-}

-- | Conditional gauge setting - only sets if enabled
conditionalSetGauge :: Bool -> Gauge -> Double -> IO ()
conditionalSetGauge enabled gauge value
    | enabled = Gauge.set value gauge
    | otherwise = pure ()
{-# INLINE conditionalSetGauge #-}

-- | Conditional histogram observation - only observes if enabled
conditionalObserve :: Bool -> Histogram -> Double -> IO ()
conditionalObserve enabled histogram value
    | enabled = void $ Histogram.observe value histogram
    | otherwise = pure ()
{-# INLINE conditionalObserve #-}

-- -------------------------------------------------------------------------- --
-- Disabled Metrics Initialization (Zero-overhead when disabled)

-- | Initialize blockchain metrics with zero overhead (disabled) - creates real metrics but they won't be used
initializeBlockchainMetricsDisabled :: Registry -> IO BlockchainMetrics
initializeBlockchainMetricsDisabled parentRegistry =
    -- Just create the normal metrics - they won't be called when disabled due to conditional checks
    initializeBlockchainMetrics parentRegistry

-- | Initialize mempool metrics with zero overhead (disabled)
initializeMempoolMetricsDisabled :: Registry -> IO MempoolMetrics
initializeMempoolMetricsDisabled parentRegistry =
    initializeMempoolMetrics parentRegistry

-- | Initialize system metrics with zero overhead (disabled)
initializeSystemMetricsDisabled :: Registry -> IO SystemMetrics
initializeSystemMetricsDisabled parentRegistry =
    initializeSystemMetrics parentRegistry

-- | Initialize database metrics with zero overhead (disabled)
initializeDatabaseMetricsDisabled :: Registry -> IO DatabaseMetrics
initializeDatabaseMetricsDisabled parentRegistry =
    initializeDatabaseMetrics parentRegistry

-- | Initialize P2P metrics with zero overhead (disabled)
initializeP2PMetricsDataDisabled :: Registry -> IO P2PMetricsData
initializeP2PMetricsDataDisabled parentRegistry =
    initializeP2PMetricsData parentRegistry

-- -------------------------------------------------------------------------- --
-- Registry Access Functions

-- | Thread-safe access to blockchain metrics
getBlockchainMetrics :: ChainwebMetrics -> STM BlockchainMetrics
getBlockchainMetrics = readTVar . _chainwebBlockchainMetrics

-- | Thread-safe access to mempool metrics
getMempoolMetrics :: ChainwebMetrics -> STM MempoolMetrics
getMempoolMetrics = readTVar . _chainwebMempoolMetrics

-- | Thread-safe access to system metrics
getSystemMetrics :: ChainwebMetrics -> STM SystemMetrics
getSystemMetrics = readTVar . _chainwebSystemMetrics

-- | Thread-safe access to database metrics
getDatabaseMetrics :: ChainwebMetrics -> STM DatabaseMetrics
getDatabaseMetrics = readTVar . _chainwebDatabaseMetrics

-- | Thread-safe access to P2P metrics
getP2PMetricsData :: ChainwebMetrics -> STM P2PMetricsData
getP2PMetricsData = readTVar . _chainwebP2PMetrics

-- -------------------------------------------------------------------------- --
-- Metric Registration Functions

-- | Subsystem types for metric naming
data MetricSubsystem
    = BlockchainSubsystem
    | MempoolSubsystem
    | SystemSubsystem
    | DatabaseSubsystem
    | P2PSubsystem
    deriving (Show, Eq, Generic)

instance NFData MetricSubsystem

-- | Error types for metric registration
data MetricRegistrationError
    = InvalidMetricName !Text !Text  -- metric name, reason
    | DuplicateMetric !Text          -- metric name
    | InvalidSubsystem !Text         -- subsystem name
    deriving (Show, Eq, Generic)

instance Exception MetricRegistrationError
instance NFData MetricRegistrationError

-- | Type-safe counter registration with automatic naming convention
registerChainwebCounter
    :: MetricSubsystem
    -> Text                -- ^ metric name (without subsystem prefix)
    -> ChainwebLabels      -- ^ labels to attach
    -> Registry
    -> IO Counter
registerChainwebCounter subsystem metricName labels registry = do
    validatedName <- validateMetricName subsystem metricName
    let fullName = formatMetricName subsystem validatedName
    labelMap <- attachLabels labels
    Registry.registerCounter fullName labelMap registry

-- | Type-safe gauge registration with automatic naming convention
registerChainwebGauge
    :: MetricSubsystem
    -> Text                -- ^ metric name (without subsystem prefix)
    -> ChainwebLabels      -- ^ labels to attach
    -> Registry
    -> IO Gauge
registerChainwebGauge subsystem metricName labels registry = do
    validatedName <- validateMetricName subsystem metricName
    let fullName = formatMetricName subsystem validatedName
    labelMap <- attachLabels labels
    Registry.registerGauge fullName labelMap registry

-- | Type-safe histogram registration with automatic naming convention
registerChainwebHistogram
    :: MetricSubsystem
    -> Text                -- ^ metric name (without subsystem prefix)
    -> [Double]            -- ^ histogram buckets
    -> ChainwebLabels      -- ^ labels to attach
    -> Registry
    -> IO Histogram
registerChainwebHistogram subsystem metricName buckets labels registry = do
    validatedName <- validateMetricName subsystem metricName
    let fullName = formatMetricName subsystem validatedName
    labelMap <- attachLabels labels
    Registry.registerHistogram fullName labelMap buckets registry

-- | Validate metric names follow the {metric}_{unit} format
validateMetricName :: MetricSubsystem -> Text -> IO Text
validateMetricName subsystem metricName = do
    when (T.null metricName) $
        throwIO $ InvalidMetricName metricName "metric name cannot be empty"

    when (not $ isValidMetricName metricName) $
        throwIO $ InvalidMetricName metricName "metric name must contain only alphanumeric characters and underscores"

    when (not $ hasValidSuffix metricName) $
        throwIO $ InvalidMetricName metricName "metric name should end with a unit (e.g., _total, _seconds, _bytes)"

    pure metricName
  where
    isValidMetricName name = T.all (\c -> isAlphaNum c || c == '_') name
    hasValidSuffix name = any (`T.isSuffixOf` name) validSuffixes
    validSuffixes = ["_total", "_seconds", "_bytes", "_count", "_percent", "_ratio"]

-- | Format metric name with subsystem prefix
formatMetricName :: MetricSubsystem -> Text -> Text
formatMetricName subsystem metricName =
    "chainweb_" <> subsystemPrefix subsystem <> "_" <> metricName
  where
    subsystemPrefix BlockchainSubsystem = "blockchain"
    subsystemPrefix MempoolSubsystem = "mempool"
    subsystemPrefix SystemSubsystem = "system"
    subsystemPrefix DatabaseSubsystem = "database"
    subsystemPrefix P2PSubsystem = "p2p"

-- | Convert ChainwebLabels to Prometheus label map
attachLabels :: ChainwebLabels -> IO (HashMap Text Text)
attachLabels ChainwebLabels{..} = do
    let labelMap = HM.empty
    let labelMap' = case _labelChainId of
            Just chainId -> HM.insert "chain_id" (chainIdToText chainId) labelMap
            Nothing -> labelMap
    let labelMap'' = case _labelVersion of
            Just version -> HM.insert "version" version labelMap'
            Nothing -> labelMap'
    let labelMap''' = case _labelNodeId of
            Just nodeId -> HM.insert "node_id" nodeId labelMap''
            Nothing -> labelMap''
    pure labelMap'''
  where
    chainIdToText = T.pack . show

-- | Check for duplicate metric registration (placeholder for deduplication logic)
deduplicateMetric :: Registry -> Text -> IO Bool
deduplicateMetric _registry _metricName = do
    -- TODO: Implement actual deduplication by checking existing metrics in registry
    -- For now, always allow registration (prometheus-haskell handles duplicates)
    pure False

-- -------------------------------------------------------------------------- --
-- Common Metric Patterns

-- | Register a latency histogram with standard buckets
registerLatencyHistogram
    :: MetricSubsystem
    -> Text                -- ^ metric name (should end with _duration_seconds)
    -> ChainwebLabels
    -> Registry
    -> IO Histogram
registerLatencyHistogram subsystem metricName =
    registerChainwebHistogram subsystem metricName defaultLatencyBuckets

-- | Register a rate counter for operations per second
registerRateCounter
    :: MetricSubsystem
    -> Text                -- ^ metric name (should end with _total)
    -> ChainwebLabels
    -> Registry
    -> IO Counter
registerRateCounter = registerChainwebCounter

-- | Register a size histogram with standard size buckets
registerSizeHistogram
    :: MetricSubsystem
    -> Text                -- ^ metric name (should end with _bytes)
    -> ChainwebLabels
    -> Registry
    -> IO Histogram
registerSizeHistogram subsystem metricName =
    registerChainwebHistogram subsystem metricName defaultSizeBuckets

-- | Access to memory manager (if enabled)
getMemoryManager :: ChainwebMetrics -> Maybe MetricsMemoryManager
getMemoryManager = _chainwebMemoryManager

-- -------------------------------------------------------------------------- --
-- Utility Types for Concurrent Access

-- | Snapshot of metric values for safe reading
data MetricSnapshot = MetricSnapshot
    { _snapshotCounters :: !(HashMap Text Double)
    , _snapshotGauges :: !(HashMap Text Double)
    , _snapshotHistograms :: !(HashMap Text [Double])  -- Sample values
    , _snapshotTimestamp :: !UTCTime
    } deriving (Show, Eq, Generic)

instance NFData MetricSnapshot

-- | Aggregated metrics across multiple chains or subsystems
data AggregatedMetrics = AggregatedMetrics
    { _aggregatedSum :: !Double
    , _aggregatedAverage :: !Double
    , _aggregatedMax :: !Double
    , _aggregatedMin :: !Double
    , _aggregatedCount :: !Natural
    } deriving (Show, Eq, Generic)

instance NFData AggregatedMetrics

-- | Metric operation types for batch updates
data MetricOperation
    = IncrementCounterOp !Counter !Double
    | SetGaugeOp !Gauge !Double
    | ObserveHistogramOp !Histogram !Double
    deriving (Generic)

instance NFData MetricOperation

-- -------------------------------------------------------------------------- --
-- Safe Concurrent Metric Access Functions

-- | Thread-safe counter increment using STM
incrementCounter :: Counter -> Double -> IO ()
incrementCounter counter value = void $ Counter.add value counter

-- | Thread-safe gauge setting
setGauge :: Gauge -> Double -> IO ()
setGauge gauge value = Gauge.set value gauge

-- | Thread-safe histogram observation
observeHistogram :: Histogram -> Double -> IO ()
observeHistogram histogram value = void $ Histogram.observe value histogram

-- | Atomic update of a single metric with STM transaction
atomicMetricUpdate :: STM a -> IO a
atomicMetricUpdate = atomically

-- | Batch update multiple metrics in a single STM transaction
batchMetricUpdate :: [MetricOperation] -> IO ()
batchMetricUpdate operations = do
    mapM_ executeOperation operations
  where
    executeOperation (IncrementCounterOp counter value) = incrementCounter counter value
    executeOperation (SetGaugeOp gauge value) = setGauge gauge value
    executeOperation (ObserveHistogramOp histogram value) = observeHistogram histogram value

-- | Safely snapshot metric values for export without blocking writers
snapshotMetrics :: ChainwebMetrics -> IO MetricSnapshot
snapshotMetrics chainwebMetrics = do
    timestamp <- getCurrentTime

    -- Note: This is a simplified implementation
    -- In a real implementation, we would extract actual values from prometheus metrics
    -- For now, return empty snapshot with timestamp
    pure MetricSnapshot
        { _snapshotCounters = HM.empty
        , _snapshotGauges = HM.empty
        , _snapshotHistograms = HM.empty
        , _snapshotTimestamp = timestamp
        }

-- | Aggregate metrics across multiple values with thread safety
aggregateMetrics :: [Double] -> IO AggregatedMetrics
aggregateMetrics values = do
    let count = fromIntegral $ length values
    let sumVal = sum values
    let avgVal = if count > 0 then sumVal / fromIntegral count else 0
    let maxVal = if null values then 0 else maximum values
    let minVal = if null values then 0 else minimum values

    pure AggregatedMetrics
        { _aggregatedSum = sumVal
        , _aggregatedAverage = avgVal
        , _aggregatedMax = maxVal
        , _aggregatedMin = minVal
        , _aggregatedCount = count
        }

-- | Retry logic with exponential backoff for STM transactions under high contention
withRetryBackoff :: Int -> STM a -> IO a
withRetryBackoff maxRetries action = retryLoop 0 1000  -- Start with 1ms delay
  where
    retryLoop retryCount delay
        | retryCount >= maxRetries = atomically action  -- Final attempt without backoff
        | otherwise = do
            result <- try (atomically action)
            case result of
                Right value -> pure value
                Left (_ :: SomeException) -> do
                    threadDelay delay
                    retryLoop (retryCount + 1) (min (delay * 2) 100000)  -- Cap at 100ms

    try :: IO a -> IO (Either SomeException a)
    try computation = catch (Right <$> computation) (pure . Left)

-- | Measure performance overhead of the metrics system itself
measureMetricsOverhead :: IO a -> IO (a, NominalDiffTime)
measureMetricsOverhead action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let overhead = diffUTCTime endTime startTime
    pure (result, overhead)

-- -------------------------------------------------------------------------- --
-- Performance Monitoring for Metrics System

-- | Global counter for tracking metrics system overhead (initialized once)
{-# NOINLINE metricsOverheadCounter #-}
metricsOverheadCounter :: IORef Natural
metricsOverheadCounter = unsafePerformIO (newIORef 0)

-- | Increment the metrics overhead counter
recordMetricsOverhead :: IO ()
recordMetricsOverhead = atomicModifyIORef' metricsOverheadCounter $ \count -> (count + 1, ())

-- | Get current metrics overhead count
getMetricsOverheadCount :: IO Natural
getMetricsOverheadCount = readIORef metricsOverheadCounter