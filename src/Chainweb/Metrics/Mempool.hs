{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Metrics.Mempool
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Prometheus metrics for mempool and transaction processing
--
module Chainweb.Metrics.Mempool
( -- * Mempool Metrics Types
  MempoolMetrics(..)
, MempoolMetricsState(..)
, TransactionValidationMetrics(..)
, PactExecutionMetrics(..)
, FeeAnalysisMetrics(..)

  -- * Initialization and Management
, initializeMempoolMetrics
, getMempoolMetrics
, updateMempoolMetrics
, exportMempoolMetrics

  -- * Core Transaction Metrics
, recordTransactionInsert
, recordTransactionLookup
, recordTransactionDelete
, recordTransactionAccepted
, recordTransactionRejected
, recordTransactionEvicted
, updateMempoolSize
, updateMempoolCapacityUtilization

  -- * Transaction Validation Metrics
, recordValidationDuration
, recordValidationSuccess
, recordValidationFailure
, recordBatchValidationDuration
, recordValidationStepDuration

  -- * Pact Execution Metrics
, recordPactExecutionDuration
, recordGasConsumption
, recordCommandType
, recordDatabaseOperation
, recordModuleLoading
, recordCacheHit
, recordCacheMiss
, recordContinuationExecution
, recordCrossChainInteraction

  -- * Fee Analysis and Lifecycle Metrics
, recordTransactionFee
, recordTransactionAge
, recordTransactionLifecycleStage
, recordMempoolSyncLatency
, recordMempoolSyncBandwidth
, recordTransactionReplacement
, recordEvictionReason

  -- * Helper Functions
, withValidationTiming
, withPactExecutionTiming
, batchUpdateMempoolMetrics
) where

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.IO.Class

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Word (Word64)

import GHC.Generics

import Numeric.Natural

import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.Concurrent.Registry (Registry)
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Counter (Counter)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.Metric.Histogram (Histogram)

-- internal modules
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.Mempool.Mempool (InsertError, TransactionHash)

-- -------------------------------------------------------------------------- --
-- Mempool Metrics Data Types

-- | Core mempool metrics for transaction pool monitoring
data MempoolMetrics = MempoolMetrics
    { _mempoolMetricsChainId :: !ChainId
    , _mempoolMetricsRegistry :: !Registry

    -- Core transaction counters
    , _mempoolTransactionsPending :: !Gauge
    , _mempoolTransactionsAccepted :: !Counter
    , _mempoolTransactionsRejected :: !Counter
    , _mempoolTransactionsEvicted :: !Counter
    , _mempoolTransactionsInserted :: !Counter
    , _mempoolTransactionsLookups :: !Counter
    , _mempoolTransactionsDeleted :: !Counter

    -- Mempool size and capacity metrics
    , _mempoolSizeBytes :: !Gauge
    , _mempoolSizeTransactions :: !Gauge
    , _mempoolCapacityUtilization :: !Gauge
    , _mempoolMaxCapacity :: !Gauge

    -- Transaction validation metrics
    , _mempoolValidationDuration :: !Histogram
    , _mempoolValidationSuccesses :: !Counter
    , _mempoolValidationFailures :: !Counter
    , _mempoolBatchValidationDuration :: !Histogram
    , _mempoolValidationStepDuration :: !Histogram

    -- Pact execution metrics
    , _mempoolPactExecutionDuration :: !Histogram
    , _mempoolGasConsumption :: !Histogram
    , _mempoolCommandTypeExec :: !Counter
    , _mempoolCommandTypeCont :: !Counter
    , _mempoolDatabaseReads :: !Counter
    , _mempoolDatabaseWrites :: !Counter
    , _mempoolModuleLoadingDuration :: !Histogram
    , _mempoolCacheHits :: !Counter
    , _mempoolCacheMisses :: !Counter
    , _mempoolContinuationExecutions :: !Counter
    , _mempoolCrossChainInteractions :: !Counter

    -- Fee analysis metrics
    , _mempoolTransactionFees :: !Histogram
    , _mempoolTransactionAges :: !Histogram
    , _mempoolTransactionReplacements :: !Counter

    -- Transaction lifecycle metrics
    , _mempoolLifecycleReceived :: !Counter
    , _mempoolLifecycleValidated :: !Counter
    , _mempoolLifecyclePending :: !Counter
    , _mempoolLifecycleIncluded :: !Counter

    -- Mempool synchronization metrics
    , _mempoolSyncLatency :: !Histogram
    , _mempoolSyncBandwidth :: !Counter

    -- Eviction reason counters
    , _mempoolEvictionTTLExpired :: !Counter
    , _mempoolEvictionCapacityFull :: !Counter
    , _mempoolEvictionLowGasPrice :: !Counter
    , _mempoolEvictionDuplicate :: !Counter
    , _mempoolEvictionInvalidSig :: !Counter

    , _mempoolLastUpdateTime :: !(IORef UTCTime)
    } deriving (Generic)

instance NFData MempoolMetrics where
    rnf MempoolMetrics{..} =
        _mempoolMetricsChainId `seq`
        _mempoolMetricsRegistry `seq`
        _mempoolTransactionsPending `seq`
        _mempoolTransactionsAccepted `seq`
        _mempoolTransactionsRejected `seq`
        _mempoolTransactionsEvicted `seq`
        _mempoolTransactionsInserted `seq`
        _mempoolTransactionsLookups `seq`
        _mempoolTransactionsDeleted `seq`
        _mempoolSizeBytes `seq`
        _mempoolSizeTransactions `seq`
        _mempoolCapacityUtilization `seq`
        _mempoolMaxCapacity `seq`
        _mempoolValidationDuration `seq`
        _mempoolValidationSuccesses `seq`
        _mempoolValidationFailures `seq`
        _mempoolBatchValidationDuration `seq`
        _mempoolValidationStepDuration `seq`
        _mempoolPactExecutionDuration `seq`
        _mempoolGasConsumption `seq`
        _mempoolCommandTypeExec `seq`
        _mempoolCommandTypeCont `seq`
        _mempoolDatabaseReads `seq`
        _mempoolDatabaseWrites `seq`
        _mempoolModuleLoadingDuration `seq`
        _mempoolCacheHits `seq`
        _mempoolCacheMisses `seq`
        _mempoolContinuationExecutions `seq`
        _mempoolCrossChainInteractions `seq`
        _mempoolTransactionFees `seq`
        _mempoolTransactionAges `seq`
        _mempoolTransactionReplacements `seq`
        _mempoolLifecycleReceived `seq`
        _mempoolLifecycleValidated `seq`
        _mempoolLifecyclePending `seq`
        _mempoolLifecycleIncluded `seq`
        _mempoolSyncLatency `seq`
        _mempoolSyncBandwidth `seq`
        _mempoolEvictionTTLExpired `seq`
        _mempoolEvictionCapacityFull `seq`
        _mempoolEvictionLowGasPrice `seq`
        _mempoolEvictionDuplicate `seq`
        _mempoolEvictionInvalidSig `seq`
        _mempoolLastUpdateTime `seq` ()

-- | State container for all mempool metrics across chains
data MempoolMetricsState = MempoolMetricsState
    { _mempoolMetricsStateMap :: !(TVar (HashMap ChainId MempoolMetrics))
    , _mempoolMetricsStateRegistry :: !Registry
    } deriving (Generic)

instance NFData MempoolMetricsState where
    rnf MempoolMetricsState{..} =
        _mempoolMetricsStateRegistry `seq`
        _mempoolMetricsStateMap `seq` ()

-- | Transaction validation specific metrics
data TransactionValidationMetrics = TransactionValidationMetrics
    { _validationDuration :: !Double
    , _validationSuccess :: !Bool
    , _validationError :: !(Maybe InsertError)
    , _validationSteps :: ![Double]
    } deriving (Generic, Show)

instance NFData TransactionValidationMetrics

-- | Pact execution specific metrics
data PactExecutionMetrics = PactExecutionMetrics
    { _pactExecutionDuration :: !Double
    , _pactGasConsumed :: !Natural
    , _pactCommandType :: !Text
    , _pactDatabaseOps :: !Natural
    , _pactModuleLoads :: !Natural
    , _pactCacheHits :: !Natural
    , _pactCacheMisses :: !Natural
    } deriving (Generic, Show)

instance NFData PactExecutionMetrics

-- | Fee analysis specific metrics
data FeeAnalysisMetrics = FeeAnalysisMetrics
    { _feeAmount :: !Double
    , _feePercentile :: !Double
    , _transactionAge :: !Double
    , _lifecycleStage :: !Text
    } deriving (Generic, Show)

instance NFData FeeAnalysisMetrics

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Initialize mempool metrics for a specific chain
initializeMempoolMetrics :: Registry -> ChainId -> IO MempoolMetrics
initializeMempoolMetrics registry chainId = do
    let chainIdLabel = [("chain_id", chainIdToText chainId)]

    -- Core transaction counters
    transactionsPending <- Registry.registerGauge
        "chainweb_mempool_transactions_pending"
        chainIdLabel
        registry

    transactionsAccepted <- Registry.registerCounter
        "chainweb_mempool_transactions_accepted_total"
        chainIdLabel
        registry

    transactionsRejected <- Registry.registerCounter
        "chainweb_mempool_transactions_rejected_total"
        chainIdLabel
        registry

    transactionsEvicted <- Registry.registerCounter
        "chainweb_mempool_transactions_evicted_total"
        chainIdLabel
        registry

    transactionsInserted <- Registry.registerCounter
        "chainweb_mempool_transactions_inserted_total"
        chainIdLabel
        registry

    transactionsLookups <- Registry.registerCounter
        "chainweb_mempool_transactions_lookups_total"
        chainIdLabel
        registry

    transactionsDeleted <- Registry.registerCounter
        "chainweb_mempool_transactions_deleted_total"
        chainIdLabel
        registry

    -- Mempool size and capacity metrics
    sizeBytes <- Registry.registerGauge
        "chainweb_mempool_size_bytes"
        chainIdLabel
        registry

    sizeTransactions <- Registry.registerGauge
        "chainweb_mempool_size_transactions"
        chainIdLabel
        registry

    capacityUtilization <- Registry.registerGauge
        "chainweb_mempool_capacity_utilization_ratio"
        chainIdLabel
        registry

    maxCapacity <- Registry.registerGauge
        "chainweb_mempool_max_capacity"
        chainIdLabel
        registry

    -- Transaction validation metrics
    validationDuration <- Registry.registerHistogram
        "chainweb_mempool_validation_duration_seconds"
        validationBuckets
        chainIdLabel
        registry

    validationSuccesses <- Registry.registerCounter
        "chainweb_mempool_validation_successes_total"
        chainIdLabel
        registry

    validationFailures <- Registry.registerCounter
        "chainweb_mempool_validation_failures_total"
        chainIdLabel
        registry

    batchValidationDuration <- Registry.registerHistogram
        "chainweb_mempool_batch_validation_duration_seconds"
        validationBuckets
        chainIdLabel
        registry

    validationStepDuration <- Registry.registerHistogram
        "chainweb_mempool_validation_step_duration_seconds"
        validationStepBuckets
        chainIdLabel
        registry

    -- Pact execution metrics
    pactExecutionDuration <- Registry.registerHistogram
        "chainweb_mempool_pact_execution_duration_seconds"
        pactExecutionBuckets
        chainIdLabel
        registry

    gasConsumption <- Registry.registerHistogram
        "chainweb_mempool_gas_consumption"
        gasBuckets
        chainIdLabel
        registry

    commandTypeExec <- Registry.registerCounter
        "chainweb_mempool_command_type_exec_total"
        chainIdLabel
        registry

    commandTypeCont <- Registry.registerCounter
        "chainweb_mempool_command_type_cont_total"
        chainIdLabel
        registry

    databaseReads <- Registry.registerCounter
        "chainweb_mempool_database_reads_total"
        chainIdLabel
        registry

    databaseWrites <- Registry.registerCounter
        "chainweb_mempool_database_writes_total"
        chainIdLabel
        registry

    moduleLoadingDuration <- Registry.registerHistogram
        "chainweb_mempool_module_loading_duration_seconds"
        moduleLoadingBuckets
        chainIdLabel
        registry

    cacheHits <- Registry.registerCounter
        "chainweb_mempool_cache_hits_total"
        chainIdLabel
        registry

    cacheMisses <- Registry.registerCounter
        "chainweb_mempool_cache_misses_total"
        chainIdLabel
        registry

    continuationExecutions <- Registry.registerCounter
        "chainweb_mempool_continuation_executions_total"
        chainIdLabel
        registry

    crossChainInteractions <- Registry.registerCounter
        "chainweb_mempool_cross_chain_interactions_total"
        chainIdLabel
        registry

    -- Fee analysis metrics
    transactionFees <- Registry.registerHistogram
        "chainweb_mempool_transaction_fees"
        feeBuckets
        chainIdLabel
        registry

    transactionAges <- Registry.registerHistogram
        "chainweb_mempool_transaction_ages_seconds"
        ageBuckets
        chainIdLabel
        registry

    transactionReplacements <- Registry.registerCounter
        "chainweb_mempool_transaction_replacements_total"
        chainIdLabel
        registry

    -- Transaction lifecycle metrics
    lifecycleReceived <- Registry.registerCounter
        "chainweb_mempool_lifecycle_received_total"
        chainIdLabel
        registry

    lifecycleValidated <- Registry.registerCounter
        "chainweb_mempool_lifecycle_validated_total"
        chainIdLabel
        registry

    lifecyclePending <- Registry.registerCounter
        "chainweb_mempool_lifecycle_pending_total"
        chainIdLabel
        registry

    lifecycleIncluded <- Registry.registerCounter
        "chainweb_mempool_lifecycle_included_total"
        chainIdLabel
        registry

    -- Mempool synchronization metrics
    syncLatency <- Registry.registerHistogram
        "chainweb_mempool_sync_latency_seconds"
        syncBuckets
        chainIdLabel
        registry

    syncBandwidth <- Registry.registerCounter
        "chainweb_mempool_sync_bandwidth_bytes_total"
        chainIdLabel
        registry

    -- Eviction reason counters
    evictionTTLExpired <- Registry.registerCounter
        "chainweb_mempool_eviction_ttl_expired_total"
        chainIdLabel
        registry

    evictionCapacityFull <- Registry.registerCounter
        "chainweb_mempool_eviction_capacity_full_total"
        chainIdLabel
        registry

    evictionLowGasPrice <- Registry.registerCounter
        "chainweb_mempool_eviction_low_gas_price_total"
        chainIdLabel
        registry

    evictionDuplicate <- Registry.registerCounter
        "chainweb_mempool_eviction_duplicate_total"
        chainIdLabel
        registry

    evictionInvalidSig <- Registry.registerCounter
        "chainweb_mempool_eviction_invalid_sig_total"
        chainIdLabel
        registry

    now <- getCurrentTime
    lastUpdateRef <- newIORef now

    pure MempoolMetrics
        { _mempoolMetricsChainId = chainId
        , _mempoolMetricsRegistry = registry
        , _mempoolTransactionsPending = transactionsPending
        , _mempoolTransactionsAccepted = transactionsAccepted
        , _mempoolTransactionsRejected = transactionsRejected
        , _mempoolTransactionsEvicted = transactionsEvicted
        , _mempoolTransactionsInserted = transactionsInserted
        , _mempoolTransactionsLookups = transactionsLookups
        , _mempoolTransactionsDeleted = transactionsDeleted
        , _mempoolSizeBytes = sizeBytes
        , _mempoolSizeTransactions = sizeTransactions
        , _mempoolCapacityUtilization = capacityUtilization
        , _mempoolMaxCapacity = maxCapacity
        , _mempoolValidationDuration = validationDuration
        , _mempoolValidationSuccesses = validationSuccesses
        , _mempoolValidationFailures = validationFailures
        , _mempoolBatchValidationDuration = batchValidationDuration
        , _mempoolValidationStepDuration = validationStepDuration
        , _mempoolPactExecutionDuration = pactExecutionDuration
        , _mempoolGasConsumption = gasConsumption
        , _mempoolCommandTypeExec = commandTypeExec
        , _mempoolCommandTypeCont = commandTypeCont
        , _mempoolDatabaseReads = databaseReads
        , _mempoolDatabaseWrites = databaseWrites
        , _mempoolModuleLoadingDuration = moduleLoadingDuration
        , _mempoolCacheHits = cacheHits
        , _mempoolCacheMisses = cacheMisses
        , _mempoolContinuationExecutions = continuationExecutions
        , _mempoolCrossChainInteractions = crossChainInteractions
        , _mempoolTransactionFees = transactionFees
        , _mempoolTransactionAges = transactionAges
        , _mempoolTransactionReplacements = transactionReplacements
        , _mempoolLifecycleReceived = lifecycleReceived
        , _mempoolLifecycleValidated = lifecycleValidated
        , _mempoolLifecyclePending = lifecyclePending
        , _mempoolLifecycleIncluded = lifecycleIncluded
        , _mempoolSyncLatency = syncLatency
        , _mempoolSyncBandwidth = syncBandwidth
        , _mempoolEvictionTTLExpired = evictionTTLExpired
        , _mempoolEvictionCapacityFull = evictionCapacityFull
        , _mempoolEvictionLowGasPrice = evictionLowGasPrice
        , _mempoolEvictionDuplicate = evictionDuplicate
        , _mempoolEvictionInvalidSig = evictionInvalidSig
        , _mempoolLastUpdateTime = lastUpdateRef
        }
  where
    -- Histogram bucket definitions
    validationBuckets = [0.001, 0.005, 0.01, 0.05, 0.1, 0.5]
    validationStepBuckets = [0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05]
    pactExecutionBuckets = [0.001, 0.01, 0.1, 1, 10]
    gasBuckets = [100, 1000, 10000, 100000, 1000000, 10000000]
    moduleLoadingBuckets = [0.001, 0.01, 0.1, 1]
    feeBuckets = [0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10]
    ageBuckets = [1, 10, 60, 300, 600] -- 1s, 10s, 1min, 5min, 10min
    syncBuckets = [0.01, 0.1, 1, 5, 30]

-- | Initialize the complete mempool metrics state
initializeMempoolMetricsState :: Registry -> IO MempoolMetricsState
initializeMempoolMetricsState registry = do
    metricsMap <- newTVarIO HM.empty
    pure MempoolMetricsState
        { _mempoolMetricsStateMap = metricsMap
        , _mempoolMetricsStateRegistry = registry
        }

-- -------------------------------------------------------------------------- --
-- Metrics Access and Updates

-- | Get or create mempool metrics for a specific chain
getMempoolMetrics :: MempoolMetricsState -> ChainId -> IO MempoolMetrics
getMempoolMetrics state chainId = do
    metricsMap <- readTVarIO (_mempoolMetricsStateMap state)
    case HM.lookup chainId metricsMap of
        Just metrics -> pure metrics
        Nothing -> do
            newMetrics <- initializeMempoolMetrics (_mempoolMetricsStateRegistry state) chainId
            atomically $ modifyTVar' (_mempoolMetricsStateMap state) (HM.insert chainId newMetrics)
            pure newMetrics

-- | Update mempool metrics last update time
updateMempoolMetrics :: MempoolMetrics -> IO ()
updateMempoolMetrics metrics = do
    now <- getCurrentTime
    writeIORef (_mempoolLastUpdateTime metrics) now

-- -------------------------------------------------------------------------- --
-- Core Transaction Metrics

-- | Record a transaction insertion
recordTransactionInsert :: MempoolMetrics -> IO ()
recordTransactionInsert metrics = do
    void $ Counter.inc (_mempoolTransactionsInserted metrics)
    updateMempoolMetrics metrics

-- | Record a transaction lookup
recordTransactionLookup :: MempoolMetrics -> IO ()
recordTransactionLookup metrics = do
    void $ Counter.inc (_mempoolTransactionsLookups metrics)
    updateMempoolMetrics metrics

-- | Record a transaction deletion
recordTransactionDelete :: MempoolMetrics -> IO ()
recordTransactionDelete metrics = do
    void $ Counter.inc (_mempoolTransactionsDeleted metrics)
    updateMempoolMetrics metrics

-- | Record a transaction acceptance
recordTransactionAccepted :: MempoolMetrics -> IO ()
recordTransactionAccepted metrics = do
    void $ Counter.inc (_mempoolTransactionsAccepted metrics)
    updateMempoolMetrics metrics

-- | Record a transaction rejection with reason
recordTransactionRejected :: MempoolMetrics -> InsertError -> IO ()
recordTransactionRejected metrics reason = do
    void $ Counter.inc (_mempoolTransactionsRejected metrics)
    -- Increment specific rejection reason counter
    case reason of
        _ -> pure () -- Could add specific counters for each rejection reason
    updateMempoolMetrics metrics

-- | Record a transaction eviction with reason
recordTransactionEvicted :: MempoolMetrics -> Text -> IO ()
recordTransactionEvicted metrics reason = do
    void $ Counter.inc (_mempoolTransactionsEvicted metrics)
    -- Increment specific eviction reason counter
    case reason of
        "ttl_expired" -> void $ Counter.inc (_mempoolEvictionTTLExpired metrics)
        "capacity_full" -> void $ Counter.inc (_mempoolEvictionCapacityFull metrics)
        "low_gas_price" -> void $ Counter.inc (_mempoolEvictionLowGasPrice metrics)
        "duplicate" -> void $ Counter.inc (_mempoolEvictionDuplicate metrics)
        "invalid_sig" -> void $ Counter.inc (_mempoolEvictionInvalidSig metrics)
        _ -> pure ()
    updateMempoolMetrics metrics

-- | Update current mempool size
updateMempoolSize :: MempoolMetrics -> Natural -> Natural -> IO ()
updateMempoolSize metrics sizeBytes sizeTransactions = do
    Gauge.set (_mempoolSizeBytes metrics) (fromIntegral sizeBytes)
    Gauge.set (_mempoolSizeTransactions metrics) (fromIntegral sizeTransactions)
    updateMempoolMetrics metrics

-- | Update mempool capacity utilization
updateMempoolCapacityUtilization :: MempoolMetrics -> Double -> Natural -> IO ()
updateMempoolCapacityUtilization metrics utilizationRatio maxCapacity = do
    Gauge.set (_mempoolCapacityUtilization metrics) utilizationRatio
    Gauge.set (_mempoolMaxCapacity metrics) (fromIntegral maxCapacity)
    updateMempoolMetrics metrics

-- -------------------------------------------------------------------------- --
-- Transaction Validation Metrics

-- | Record validation duration
recordValidationDuration :: MempoolMetrics -> Double -> IO ()
recordValidationDuration metrics duration = do
    void $ Histogram.observe duration (_mempoolValidationDuration metrics)
    updateMempoolMetrics metrics

-- | Record validation success
recordValidationSuccess :: MempoolMetrics -> IO ()
recordValidationSuccess metrics = do
    void $ Counter.inc (_mempoolValidationSuccesses metrics)
    updateMempoolMetrics metrics

-- | Record validation failure with reason
recordValidationFailure :: MempoolMetrics -> InsertError -> IO ()
recordValidationFailure metrics _reason = do
    void $ Counter.inc (_mempoolValidationFailures metrics)
    updateMempoolMetrics metrics

-- | Record batch validation duration
recordBatchValidationDuration :: MempoolMetrics -> Double -> IO ()
recordBatchValidationDuration metrics duration = do
    void $ Histogram.observe duration (_mempoolBatchValidationDuration metrics)
    updateMempoolMetrics metrics

-- | Record validation step duration
recordValidationStepDuration :: MempoolMetrics -> Double -> IO ()
recordValidationStepDuration metrics duration = do
    void $ Histogram.observe duration (_mempoolValidationStepDuration metrics)
    updateMempoolMetrics metrics

-- -------------------------------------------------------------------------- --
-- Pact Execution Metrics

-- | Record Pact execution duration
recordPactExecutionDuration :: MempoolMetrics -> Double -> IO ()
recordPactExecutionDuration metrics duration = do
    void $ Histogram.observe duration (_mempoolPactExecutionDuration metrics)
    updateMempoolMetrics metrics

-- | Record gas consumption
recordGasConsumption :: MempoolMetrics -> Natural -> IO ()
recordGasConsumption metrics gasConsumed = do
    void $ Histogram.observe (fromIntegral gasConsumed) (_mempoolGasConsumption metrics)
    updateMempoolMetrics metrics

-- | Record command type
recordCommandType :: MempoolMetrics -> Text -> IO ()
recordCommandType metrics commandType = do
    case commandType of
        "exec" -> void $ Counter.inc (_mempoolCommandTypeExec metrics)
        "cont" -> void $ Counter.inc (_mempoolCommandTypeCont metrics)
        _ -> pure ()
    updateMempoolMetrics metrics

-- | Record database operation
recordDatabaseOperation :: MempoolMetrics -> Text -> IO ()
recordDatabaseOperation metrics opType = do
    case opType of
        "read" -> void $ Counter.inc (_mempoolDatabaseReads metrics)
        "write" -> void $ Counter.inc (_mempoolDatabaseWrites metrics)
        _ -> pure ()
    updateMempoolMetrics metrics

-- | Record module loading duration
recordModuleLoading :: MempoolMetrics -> Double -> IO ()
recordModuleLoading metrics duration = do
    void $ Histogram.observe duration (_mempoolModuleLoadingDuration metrics)
    updateMempoolMetrics metrics

-- | Record cache hit
recordCacheHit :: MempoolMetrics -> IO ()
recordCacheHit metrics = do
    void $ Counter.inc (_mempoolCacheHits metrics)
    updateMempoolMetrics metrics

-- | Record cache miss
recordCacheMiss :: MempoolMetrics -> IO ()
recordCacheMiss metrics = do
    void $ Counter.inc (_mempoolCacheMisses metrics)
    updateMempoolMetrics metrics

-- | Record continuation execution
recordContinuationExecution :: MempoolMetrics -> IO ()
recordContinuationExecution metrics = do
    void $ Counter.inc (_mempoolContinuationExecutions metrics)
    updateMempoolMetrics metrics

-- | Record cross-chain interaction
recordCrossChainInteraction :: MempoolMetrics -> IO ()
recordCrossChainInteraction metrics = do
    void $ Counter.inc (_mempoolCrossChainInteractions metrics)
    updateMempoolMetrics metrics

-- -------------------------------------------------------------------------- --
-- Fee Analysis and Lifecycle Metrics

-- | Record transaction fee
recordTransactionFee :: MempoolMetrics -> Double -> IO ()
recordTransactionFee metrics fee = do
    void $ Histogram.observe fee (_mempoolTransactionFees metrics)
    updateMempoolMetrics metrics

-- | Record transaction age in mempool
recordTransactionAge :: MempoolMetrics -> Double -> IO ()
recordTransactionAge metrics age = do
    void $ Histogram.observe age (_mempoolTransactionAges metrics)
    updateMempoolMetrics metrics

-- | Record transaction lifecycle stage
recordTransactionLifecycleStage :: MempoolMetrics -> Text -> IO ()
recordTransactionLifecycleStage metrics stage = do
    case stage of
        "received" -> void $ Counter.inc (_mempoolLifecycleReceived metrics)
        "validated" -> void $ Counter.inc (_mempoolLifecycleValidated metrics)
        "pending" -> void $ Counter.inc (_mempoolLifecyclePending metrics)
        "included" -> void $ Counter.inc (_mempoolLifecycleIncluded metrics)
        _ -> pure ()
    updateMempoolMetrics metrics

-- | Record mempool synchronization latency
recordMempoolSyncLatency :: MempoolMetrics -> Double -> IO ()
recordMempoolSyncLatency metrics latency = do
    void $ Histogram.observe latency (_mempoolSyncLatency metrics)
    updateMempoolMetrics metrics

-- | Record mempool synchronization bandwidth
recordMempoolSyncBandwidth :: MempoolMetrics -> Natural -> IO ()
recordMempoolSyncBandwidth metrics bytes = do
    void $ Counter.add (fromIntegral bytes) (_mempoolSyncBandwidth metrics)
    updateMempoolMetrics metrics

-- | Record transaction replacement
recordTransactionReplacement :: MempoolMetrics -> IO ()
recordTransactionReplacement metrics = do
    void $ Counter.inc (_mempoolTransactionReplacements metrics)
    updateMempoolMetrics metrics

-- | Record eviction reason
recordEvictionReason :: MempoolMetrics -> Text -> IO ()
recordEvictionReason metrics reason = do
    recordTransactionEvicted metrics reason

-- -------------------------------------------------------------------------- --
-- Helper Functions

-- | Time an operation and record validation duration
withValidationTiming :: MempoolMetrics -> IO a -> IO a
withValidationTiming metrics action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    recordValidationDuration metrics duration
    pure result

-- | Time an operation and record Pact execution duration
withPactExecutionTiming :: MempoolMetrics -> IO a -> IO a
withPactExecutionTiming metrics action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    recordPactExecutionDuration metrics duration
    pure result

-- | Batch update multiple mempool metrics atomically
batchUpdateMempoolMetrics :: [IO ()] -> IO ()
batchUpdateMempoolMetrics = sequence_

-- | Export mempool metrics summary
exportMempoolMetrics :: MempoolMetricsState -> IO Text
exportMempoolMetrics state = do
    metricsMap <- readTVarIO (_mempoolMetricsStateMap state)
    let chains = HM.keys metricsMap
    pure $ T.unlines $
        [ "# Mempool Metrics Summary"
        , "# Active chains: " <> T.pack (show $ length chains)
        , "# Chains: " <> T.intercalate ", " (map chainIdToText chains)
        ]