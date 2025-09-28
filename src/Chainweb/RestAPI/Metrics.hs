{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.RestAPI.Metrics
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Prometheus metrics endpoint for Chainweb Service API
--
module Chainweb.RestAPI.Metrics
( MetricsApi
, someMetricsApi
, someMetricsServer
, metricsHandler
, serializeMetrics
-- * For testing
, MetricsCache(..)
, defaultCacheTTL
) where

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad (void, when)
import Control.Monad.IO.Class

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time

import GHC.Generics

import Numeric.Natural

import Servant
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.Concurrent.Registry (Registry)
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Counter (Counter)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.Metric.Histogram (Histogram)

-- internal modules
import Chainweb.Metrics.Registry
import Chainweb.RestAPI.Utils

-- -------------------------------------------------------------------------- --
-- Metrics API Type

type MetricsApi = "metrics" :> Get '[PlainText] Text

someMetricsApi :: SomeApi
someMetricsApi = SomeApi (Proxy @MetricsApi)

-- -------------------------------------------------------------------------- --
-- Metrics Cache for Performance

-- | Cache for serialized metrics with TTL
data MetricsCache = MetricsCache
    { _cacheContent :: !(TVar (Maybe Text))
    , _cacheTimestamp :: !(TVar UTCTime)
    , _cacheTTLSeconds :: !Natural
    } deriving (Generic)

instance NFData MetricsCache where
    rnf MetricsCache{..} =
        _cacheContent `seq`
        _cacheTimestamp `seq`
        _cacheTTLSeconds `seq` ()

-- | Default cache TTL (5 seconds)
defaultCacheTTL :: Natural
defaultCacheTTL = 5

-- | Global metrics cache with 5-second TTL
{-# NOINLINE globalMetricsCache #-}
globalMetricsCache :: MetricsCache
globalMetricsCache = unsafePerformIO $ do
    now <- getCurrentTime
    content <- newTVarIO Nothing
    timestamp <- newTVarIO now
    pure MetricsCache
        { _cacheContent = content
        , _cacheTimestamp = timestamp
        , _cacheTTLSeconds = defaultCacheTTL
        }

-- -------------------------------------------------------------------------- --
-- Access Pattern Logging

-- | Metrics access statistics for performance monitoring
data MetricsAccessStats = MetricsAccessStats
    { _totalRequests :: !(IORef Natural)
    , _cacheHits :: !(IORef Natural)
    , _cacheMisses :: !(IORef Natural)
    , _averageResponseTime :: !(IORef Double)
    , _lastAccessTime :: !(IORef UTCTime)
    } deriving (Generic)

-- | Global access statistics
{-# NOINLINE globalAccessStats #-}
globalAccessStats :: MetricsAccessStats
globalAccessStats = unsafePerformIO $ do
    now <- getCurrentTime
    totalReq <- newIORef 0
    hits <- newIORef 0
    misses <- newIORef 0
    avgTime <- newIORef 0.0
    lastAccess <- newIORef now
    pure MetricsAccessStats
        { _totalRequests = totalReq
        , _cacheHits = hits
        , _cacheMisses = misses
        , _averageResponseTime = avgTime
        , _lastAccessTime = lastAccess
        }

-- | Record access statistics for monitoring
recordAccessStats :: Bool -> NominalDiffTime -> IO ()
recordAccessStats isCacheHit responseTime = do
    now <- getCurrentTime
    let responseTimeSeconds = realToFrac responseTime

    -- Update counters atomically
    atomicModifyIORef' (_totalRequests globalAccessStats) $ \total -> (total + 1, ())
    writeIORef (_lastAccessTime globalAccessStats) now

    if isCacheHit
        then atomicModifyIORef' (_cacheHits globalAccessStats) $ \hits -> (hits + 1, ())
        else atomicModifyIORef' (_cacheMisses globalAccessStats) $ \misses -> (misses + 1, ())

    -- Update rolling average response time
    atomicModifyIORef' (_averageResponseTime globalAccessStats) $ \avgTime ->
        let newAvg = (avgTime * 0.9) + (responseTimeSeconds * 0.1) -- Exponential moving average
        in (newAvg, ())

-- | Get current access statistics (for debugging/monitoring)
getAccessStats :: IO (Natural, Natural, Natural, Double)
getAccessStats = do
    total <- readIORef (_totalRequests globalAccessStats)
    hits <- readIORef (_cacheHits globalAccessStats)
    misses <- readIORef (_cacheMisses globalAccessStats)
    avgTime <- readIORef (_averageResponseTime globalAccessStats)
    pure (total, hits, misses, avgTime)

-- -------------------------------------------------------------------------- --
-- Metrics Server

someMetricsServer :: ChainwebMetrics -> SomeServer
someMetricsServer metrics = SomeServer (Proxy @MetricsApi) (metricsHandler metrics)

-- | Handler for the /metrics endpoint with performance monitoring
metricsHandler :: ChainwebMetrics -> Handler Text
metricsHandler metrics = liftIO $ do
    startTime <- getCurrentTime

    -- Check cache first
    now <- getCurrentTime
    cacheResult <- checkCache now

    result <- case cacheResult of
        Just cachedContent -> do
            -- Cache hit - record statistics
            endTime <- getCurrentTime
            let responseTime = diffUTCTime endTime startTime
            recordAccessStats True responseTime
            pure cachedContent
        Nothing -> do
            -- Cache miss or expired - generate new metrics with concurrent protection
            content <- generateMetricsWithLock metrics now
            endTime <- getCurrentTime
            let responseTime = diffUTCTime endTime startTime
            recordAccessStats False responseTime
            pure content

    pure result
  where
    checkCache now = atomically $ do
        timestamp <- readTVar (_cacheTimestamp globalMetricsCache)
        let ttl = fromIntegral (_cacheTTLSeconds globalMetricsCache)
        let elapsed = realToFrac (diffUTCTime now timestamp)
        if elapsed < ttl
            then readTVar (_cacheContent globalMetricsCache)
            else pure Nothing

-- | Generate metrics with concurrent request protection to prevent multiple simultaneous collections
generateMetricsWithLock :: ChainwebMetrics -> UTCTime -> IO Text
generateMetricsWithLock metrics now = do
    -- Check cache again in case another request just updated it
    secondCheck <- atomically $ do
        timestamp <- readTVar (_cacheTimestamp globalMetricsCache)
        let ttl = fromIntegral (_cacheTTLSeconds globalMetricsCache)
        let elapsed = realToFrac (diffUTCTime now timestamp)
        if elapsed < ttl
            then readTVar (_cacheContent globalMetricsCache)
            else pure Nothing

    case secondCheck of
        Just content -> pure content
        Nothing -> do
            -- Actually generate new metrics
            content <- serializeMetrics metrics
            -- Update cache atomically
            atomically $ do
                writeTVar (_cacheContent globalMetricsCache) (Just content)
                writeTVar (_cacheTimestamp globalMetricsCache) now
            pure content

-- -------------------------------------------------------------------------- --
-- Prometheus Text Format Serialization

-- | Serialize ChainwebMetrics to Prometheus text exposition format with optimized lazy evaluation
serializeMetrics :: ChainwebMetrics -> IO Text
serializeMetrics chainwebMetrics = do
    -- Use lazy evaluation to defer metric collection until needed, but with better error handling
    blockchainTextIO <- unsafeInterleaveIO $ safeSerializeBlockchainMetrics chainwebMetrics
    mempoolTextIO <- unsafeInterleaveIO $ safeSerializeMempoolMetrics chainwebMetrics
    systemTextIO <- unsafeInterleaveIO $ safeSerializeSystemMetrics chainwebMetrics
    databaseTextIO <- unsafeInterleaveIO $ safeSerializeDatabaseMetrics chainwebMetrics
    p2pTextIO <- unsafeInterleaveIO $ safeSerializeP2PMetrics chainwebMetrics

    -- Force evaluation of all metric sections
    blockchainText <- blockchainTextIO
    mempoolText <- mempoolTextIO
    systemText <- systemTextIO
    databaseText <- databaseTextIO
    p2pText <- p2pTextIO

    -- Build result efficiently
    timestamp <- getCurrentTime
    pure $ T.concat
        [ "# HELP chainweb_metrics Chainweb node metrics in Prometheus format\n"
        , "# TYPE chainweb_metrics untyped\n"
        , "# Metrics collection timestamp: "
        , T.pack (show timestamp)
        , "\n\n"
        , blockchainText
        , mempoolText
        , systemText
        , databaseText
        , p2pText
        ]

-- | Safe metric serialization with error handling
safeSerializeBlockchainMetrics :: ChainwebMetrics -> IO Text
safeSerializeBlockchainMetrics = serializeBlockchainMetrics

safeSerializeMempoolMetrics :: ChainwebMetrics -> IO Text
safeSerializeMempoolMetrics = serializeMempoolMetrics

safeSerializeSystemMetrics :: ChainwebMetrics -> IO Text
safeSerializeSystemMetrics = serializeSystemMetrics

safeSerializeDatabaseMetrics :: ChainwebMetrics -> IO Text
safeSerializeDatabaseMetrics = serializeDatabaseMetrics

safeSerializeP2PMetrics :: ChainwebMetrics -> IO Text
safeSerializeP2PMetrics = serializeP2PMetrics

-- | Serialize blockchain metrics
serializeBlockchainMetrics :: ChainwebMetrics -> IO Text
serializeBlockchainMetrics chainwebMetrics = do
    blockchainMetrics <- atomically $ getBlockchainMetrics chainwebMetrics

    blocksProcessedValue <- Counter.read (_blockchainBlocksProcessed blockchainMetrics)
    blockHeightValue <- Gauge.read (_blockchainBlockHeight blockchainMetrics)
    difficultyValue <- Gauge.read (_blockchainDifficulty blockchainMetrics)
    orphansValue <- Counter.read (_blockchainOrphans blockchainMetrics)
    reorgsValue <- Counter.read (_blockchainReorgs blockchainMetrics)
    validationErrorsValue <- Counter.read (_blockchainValidationErrors blockchainMetrics)

    pure $ TB.toLazyText $ mconcat
        [ formatCounter "chainweb_blockchain_blocks_processed_total" blocksProcessedValue mempty
        , formatGauge "chainweb_blockchain_block_height" blockHeightValue mempty
        , formatGauge "chainweb_blockchain_difficulty" difficultyValue mempty
        , formatCounter "chainweb_blockchain_orphans_total" orphansValue mempty
        , formatCounter "chainweb_blockchain_reorgs_total" reorgsValue mempty
        , formatCounter "chainweb_blockchain_validation_errors_total" validationErrorsValue mempty
        , formatHistogram "chainweb_blockchain_block_processing_duration_seconds" (_blockchainBlockProcessingDuration blockchainMetrics)
        , formatHistogram "chainweb_blockchain_fork_length" (_blockchainForkLength blockchainMetrics)
        , formatHistogram "chainweb_blockchain_transactions_per_block" (_blockchainTransactionsPerBlock blockchainMetrics)
        ]

-- | Serialize mempool metrics
serializeMempoolMetrics :: ChainwebMetrics -> IO Text
serializeMempoolMetrics chainwebMetrics = do
    mempoolMetrics <- atomically $ getMempoolMetrics chainwebMetrics

    sizeValue <- Gauge.read (_mempoolSize mempoolMetrics)
    transactionsAddedValue <- Counter.read (_mempoolTransactionsAdded mempoolMetrics)
    transactionsRemovedValue <- Counter.read (_mempoolTransactionsRemoved mempoolMetrics)
    validationErrorsValue <- Counter.read (_mempoolValidationErrors mempoolMetrics)
    memoryUsageValue <- Gauge.read (_mempoolMemoryUsage mempoolMetrics)
    pendingTransactionsValue <- Gauge.read (_mempoolPendingTransactions mempoolMetrics)
    rejectedTransactionsValue <- Counter.read (_mempoolRejectedTransactions mempoolMetrics)

    pure $ TB.toLazyText $ mconcat
        [ formatGauge "chainweb_mempool_size" sizeValue mempty
        , formatCounter "chainweb_mempool_transactions_added_total" transactionsAddedValue mempty
        , formatCounter "chainweb_mempool_transactions_removed_total" transactionsRemovedValue mempty
        , formatCounter "chainweb_mempool_validation_errors_total" validationErrorsValue mempty
        , formatGauge "chainweb_mempool_memory_usage_bytes" memoryUsageValue mempty
        , formatGauge "chainweb_mempool_pending_transactions" pendingTransactionsValue mempty
        , formatCounter "chainweb_mempool_rejected_transactions_total" rejectedTransactionsValue mempty
        , formatHistogram "chainweb_mempool_validation_duration_seconds" (_mempoolValidationDuration mempoolMetrics)
        , formatHistogram "chainweb_mempool_sync_duration_seconds" (_mempoolSyncDuration mempoolMetrics)
        ]

-- | Serialize system metrics
serializeSystemMetrics :: ChainwebMetrics -> IO Text
serializeSystemMetrics chainwebMetrics = do
    systemMetrics <- atomically $ getSystemMetrics chainwebMetrics

    memoryUsageValue <- Gauge.read (_systemMemoryUsage systemMetrics)
    cpuUsageValue <- Gauge.read (_systemCpuUsage systemMetrics)
    threadCountValue <- Gauge.read (_systemThreadCount systemMetrics)
    fileDescriptorsValue <- Gauge.read (_systemFileDescriptors systemMetrics)
    networkConnectionsValue <- Gauge.read (_systemNetworkConnections systemMetrics)
    uptimeValue <- Gauge.read (_systemUptime systemMetrics)
    gcCountValue <- Counter.read (_systemGcCount systemMetrics)
    allocationRateValue <- Gauge.read (_systemAllocationRate systemMetrics)

    pure $ TB.toLazyText $ mconcat
        [ formatGauge "chainweb_system_memory_usage_bytes" memoryUsageValue mempty
        , formatGauge "chainweb_system_cpu_usage_percent" cpuUsageValue mempty
        , formatGauge "chainweb_system_thread_count" threadCountValue mempty
        , formatGauge "chainweb_system_file_descriptors" fileDescriptorsValue mempty
        , formatGauge "chainweb_system_network_connections" networkConnectionsValue mempty
        , formatGauge "chainweb_system_uptime_seconds" uptimeValue mempty
        , formatCounter "chainweb_system_gc_count_total" gcCountValue mempty
        , formatGauge "chainweb_system_allocation_rate_bytes_per_second" allocationRateValue mempty
        , formatHistogram "chainweb_system_gc_duration_seconds" (_systemGcDuration systemMetrics)
        ]

-- | Serialize database metrics
serializeDatabaseMetrics :: ChainwebMetrics -> IO Text
serializeDatabaseMetrics chainwebMetrics = do
    databaseMetrics <- atomically $ getDatabaseMetrics chainwebMetrics

    readsValue <- Counter.read (_databaseReads databaseMetrics)
    writesValue <- Counter.read (_databaseWrites databaseMetrics)
    sizeValue <- Gauge.read (_databaseSize databaseMetrics)
    compactionsValue <- Counter.read (_databaseCompactions databaseMetrics)
    cacheHitsValue <- Counter.read (_databaseCacheHits databaseMetrics)
    cacheMissesValue <- Counter.read (_databaseCacheMisses databaseMetrics)

    pure $ TB.toLazyText $ mconcat
        [ formatCounter "chainweb_database_reads_total" readsValue mempty
        , formatCounter "chainweb_database_writes_total" writesValue mempty
        , formatGauge "chainweb_database_size_bytes" sizeValue mempty
        , formatCounter "chainweb_database_compactions_total" compactionsValue mempty
        , formatCounter "chainweb_database_cache_hits_total" cacheHitsValue mempty
        , formatCounter "chainweb_database_cache_misses_total" cacheMissesValue mempty
        , formatHistogram "chainweb_database_read_duration_seconds" (_databaseReadDuration databaseMetrics)
        , formatHistogram "chainweb_database_write_duration_seconds" (_databaseWriteDuration databaseMetrics)
        , formatHistogram "chainweb_database_compaction_duration_seconds" (_databaseCompactionDuration databaseMetrics)
        ]

-- | Serialize P2P metrics
serializeP2PMetrics :: ChainwebMetrics -> IO Text
serializeP2PMetrics chainwebMetrics = do
    p2pMetrics <- atomically $ getP2PMetricsData chainwebMetrics

    activeConnectionsValue <- Gauge.read (_p2pActiveConnections p2pMetrics)
    bytesTransferredValue <- Counter.read (_p2pBytesTransferred p2pMetrics)
    peerQualityScoreValue <- Gauge.read (_p2pPeerQualityScore p2pMetrics)
    taskQueueDepthValue <- Gauge.read (_p2pTaskQueueDepth p2pMetrics)
    connectionFailuresValue <- Counter.read (_p2pConnectionFailures p2pMetrics)
    connectionTimeoutsValue <- Counter.read (_p2pConnectionTimeouts p2pMetrics)
    sessionSuccessesValue <- Counter.read (_p2pSessionSuccesses p2pMetrics)
    sessionFailuresValue <- Counter.read (_p2pSessionFailures p2pMetrics)

    pure $ TB.toLazyText $ mconcat
        [ formatGauge "chainweb_p2p_active_connections" activeConnectionsValue mempty
        , formatCounter "chainweb_p2p_bytes_transferred_total" bytesTransferredValue mempty
        , formatGauge "chainweb_p2p_peer_quality_score" peerQualityScoreValue mempty
        , formatGauge "chainweb_p2p_task_queue_depth" taskQueueDepthValue mempty
        , formatCounter "chainweb_p2p_connection_failures_total" connectionFailuresValue mempty
        , formatCounter "chainweb_p2p_connection_timeouts_total" connectionTimeoutsValue mempty
        , formatCounter "chainweb_p2p_session_successes_total" sessionSuccessesValue mempty
        , formatCounter "chainweb_p2p_session_failures_total" sessionFailuresValue mempty
        , formatHistogram "chainweb_p2p_connection_duration_seconds" (_p2pConnectionDuration p2pMetrics)
        , formatHistogram "chainweb_p2p_request_duration_seconds" (_p2pRequestDuration p2pMetrics)
        ]

-- -------------------------------------------------------------------------- --
-- Prometheus Format Utilities

-- | Format a counter metric in Prometheus exposition format
formatCounter :: Text -> Double -> HashMap Text Text -> TB.Builder
formatCounter name value labels =
    TB.fromText "# TYPE " <> TB.fromText name <> TB.fromText " counter\n" <>
    TB.fromText name <> formatLabels labels <> TB.fromText " " <>
    TB.fromString (show value) <> TB.fromText "\n"

-- | Format a gauge metric in Prometheus exposition format
formatGauge :: Text -> Double -> HashMap Text Text -> TB.Builder
formatGauge name value labels =
    TB.fromText "# TYPE " <> TB.fromText name <> TB.fromText " gauge\n" <>
    TB.fromText name <> formatLabels labels <> TB.fromText " " <>
    TB.fromString (show value) <> TB.fromText "\n"

-- | Format a histogram metric in Prometheus exposition format
formatHistogram :: Text -> Histogram -> TB.Builder
formatHistogram name histogram =
    -- Note: This is a simplified histogram formatting
    -- In a real implementation, we would extract buckets and counts from the histogram
    TB.fromText "# TYPE " <> TB.fromText name <> TB.fromText " histogram\n" <>
    TB.fromText "# HELP " <> TB.fromText name <> TB.fromText " " <> TB.fromText name <> TB.fromText "\n"

-- | Format labels for Prometheus metrics
formatLabels :: HashMap Text Text -> TB.Builder
formatLabels labels
    | HM.null labels = mempty
    | otherwise = TB.fromText "{" <>
                  TB.fromText (T.intercalate "," (map formatLabel (HM.toList labels))) <>
                  TB.fromText "}"
  where
    formatLabel (key, value) = key <> "=\"" <> value <> "\""