{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Metrics.Mempool.Lifecycle
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Transaction lifecycle tracking and fee analysis metrics
--
module Chainweb.Metrics.Mempool.Lifecycle
( -- * Transaction Lifecycle Types
  TransactionLifecycle(..)
, LifecycleStage(..)
, LifecycleTransition(..)
, FeeDistribution(..)
, TransactionAge(..)
, MempoolSyncMetrics(..)
, EvictionReason(..)

  -- * Lifecycle Tracking
, initializeTransactionLifecycle
, recordLifecycleTransition
, recordTransactionReceived
, recordTransactionValidated
, recordTransactionPending
, recordTransactionIncluded
, recordTransactionEvicted
, calculateTransactionAge
, getLifecycleStage

  -- * Fee Analysis
, recordTransactionFee
, recordFeeDistribution
, calculateFeePercentile
, analyzeFeeMarket
, recordGasPriceCorrelation
, trackFeeTrends

  -- * Transaction Age Tracking
, recordTransactionAge
, updateTransactionAge
, categorizeTransactionAge
, recordAgeDistribution
, trackAgeVersusInclusion

  -- * Mempool Synchronization Metrics
, recordSyncLatency
, recordSyncBandwidth
, recordSyncThroughput
, recordPeerSyncPerformance
, recordSyncFailure
, recordSyncSuccess

  -- * Priority and Eviction Analysis
, recordTransactionPriority
, recordEvictionEvent
, recordReplacementEvent
, analyzePriorityInversion
, trackCapacityUtilization
, recordEvictionReason

  -- * Advanced Analytics
, TransactionMetrics(..)
, MempoolHealthMetrics(..)
, recordTransactionMetrics
, calculateMempoolHealth
, detectAnomalies
, generateLifecycleReport

  -- * Instrumented Sync Functions
, instrumentedSyncMempools
, instrumentedMempoolInsert
, instrumentedMempoolEvict
, withLifecycleTracking
, withFeeAnalysis
) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)

import GHC.Generics

import Numeric.Natural

import qualified Pact.Types.Gas as Pact4

-- internal modules
import Chainweb.ChainId (ChainId)
import Chainweb.Mempool.Mempool (TransactionHash, InsertError)
import Chainweb.Metrics.Mempool

-- -------------------------------------------------------------------------- --
-- Transaction Lifecycle Data Types

-- | Complete transaction lifecycle tracking
data TransactionLifecycle = TransactionLifecycle
    { _lifecycleTransactionHash :: !TransactionHash
    , _lifecycleReceived :: !UTCTime
    , _lifecycleValidated :: !(Maybe UTCTime)
    , _lifecyclePending :: !(Maybe UTCTime)
    , _lifecycleIncluded :: !(Maybe UTCTime)
    , _lifecycleEvicted :: !(Maybe UTCTime)
    , _lifecycleFee :: !Double
    , _lifecycleGasPrice :: !Double
    , _lifecycleGasLimit :: !Natural
    , _lifecyclePriority :: !Natural
    , _lifecycleSize :: !Natural
    , _lifecycleRetryCount :: !Natural
    , _lifecycleLastSeen :: !UTCTime
    } deriving (Generic, Show)

instance NFData TransactionLifecycle

-- | Lifecycle stages
data LifecycleStage
    = StageReceived      -- ^ Transaction received from client or peer
    | StageValidated     -- ^ Transaction passed validation
    | StagePending       -- ^ Transaction in mempool pending inclusion
    | StageIncluded      -- ^ Transaction included in block
    | StageEvicted       -- ^ Transaction evicted from mempool
    | StageReplaced      -- ^ Transaction replaced by higher fee version
    deriving (Generic, Show, Eq, Ord)

instance NFData LifecycleStage

-- | Lifecycle transition with timing
data LifecycleTransition = LifecycleTransition
    { _transitionFrom :: !LifecycleStage
    , _transitionTo :: !LifecycleStage
    , _transitionTime :: !UTCTime
    , _transitionDuration :: !NominalDiffTime
    } deriving (Generic, Show)

instance NFData LifecycleTransition

-- | Fee distribution analysis
data FeeDistribution = FeeDistribution
    { _feeMin :: !Double
    , _feeMax :: !Double
    , _feeMean :: !Double
    , _feeMedian :: !Double
    , _feeP95 :: !Double
    , _feeP99 :: !Double
    , _feeStdDev :: !Double
    , _feeSampleSize :: !Natural
    , _feeTimestamp :: !UTCTime
    } deriving (Generic, Show)

instance NFData FeeDistribution

-- | Transaction age categories
data TransactionAge
    = AgeFresh      -- ^ < 1 second
    | AgeRecent     -- ^ 1-10 seconds
    | AgeWaiting    -- ^ 10-60 seconds
    | AgeOld        -- ^ 1-5 minutes
    | AgeStale      -- ^ > 5 minutes
    deriving (Generic, Show, Eq, Ord)

instance NFData TransactionAge

-- | Mempool synchronization metrics
data MempoolSyncMetrics = MempoolSyncMetrics
    { _syncLatency :: !Double
    , _syncBandwidth :: !Natural
    , _syncThroughput :: !Double
    , _syncPeerCount :: !Natural
    , _syncSuccessRate :: !Double
    , _syncFailureCount :: !Natural
    , _syncDuplicates :: !Natural
    , _syncNewTransactions :: !Natural
    } deriving (Generic, Show)

instance NFData MempoolSyncMetrics

-- | Eviction reasons
data EvictionReason
    = EvictionTTLExpired
    | EvictionCapacityFull
    | EvictionLowGasPrice
    | EvictionDuplicate
    | EvictionInvalidSignature
    | EvictionReplacedByHigherFee
    | EvictionSystemShutdown
    deriving (Generic, Show, Eq, Ord)

instance NFData EvictionReason

-- | Comprehensive transaction metrics
data TransactionMetrics = TransactionMetrics
    { _metricsHash :: !TransactionHash
    , _metricsLifecycle :: !TransactionLifecycle
    , _metricsTransitions :: ![LifecycleTransition]
    , _metricsFeeAnalysis :: !(Maybe FeeDistribution)
    , _metricsAgeCategory :: !TransactionAge
    , _metricsSyncMetrics :: !(Maybe MempoolSyncMetrics)
    } deriving (Generic, Show)

instance NFData TransactionMetrics

-- | Mempool health metrics
data MempoolHealthMetrics = MempoolHealthMetrics
    { _healthTotalTransactions :: !Natural
    , _healthValidationRate :: !Double
    , _healthInclusionRate :: !Double
    , _healthEvictionRate :: !Double
    , _healthAverageAge :: !Double
    , _healthFeeEfficiency :: !Double
    , _healthCapacityUtilization :: !Double
    , _healthSyncPerformance :: !Double
    } deriving (Generic, Show)

instance NFData MempoolHealthMetrics

-- -------------------------------------------------------------------------- --
-- Lifecycle Tracking Functions

-- | Initialize a new transaction lifecycle
initializeTransactionLifecycle :: TransactionHash -> Double -> Double -> Natural -> IO TransactionLifecycle
initializeTransactionLifecycle txHash fee gasPrice gasLimit = do
    now <- getCurrentTime
    pure TransactionLifecycle
        { _lifecycleTransactionHash = txHash
        , _lifecycleReceived = now
        , _lifecycleValidated = Nothing
        , _lifecyclePending = Nothing
        , _lifecycleIncluded = Nothing
        , _lifecycleEvicted = Nothing
        , _lifecycleFee = fee
        , _lifecycleGasPrice = gasPrice
        , _lifecycleGasLimit = gasLimit
        , _lifecyclePriority = 0
        , _lifecycleSize = 0
        , _lifecycleRetryCount = 0
        , _lifecycleLastSeen = now
        }

-- | Record a lifecycle transition
recordLifecycleTransition :: MempoolMetrics -> LifecycleStage -> LifecycleStage -> IO ()
recordLifecycleTransition metrics fromStage toStage = do
    let stageName = lifecycleStageText toStage
    recordTransactionLifecycleStage metrics stageName

-- | Record transaction received
recordTransactionReceived :: MempoolMetrics -> TransactionHash -> Double -> IO ()
recordTransactionReceived metrics _txHash fee = do
    recordTransactionLifecycleStage metrics "received"
    recordTransactionFee metrics fee

-- | Record transaction validated
recordTransactionValidated :: MempoolMetrics -> TransactionHash -> IO ()
recordTransactionValidated metrics _txHash = do
    recordTransactionLifecycleStage metrics "validated"

-- | Record transaction pending
recordTransactionPending :: MempoolMetrics -> TransactionHash -> IO ()
recordTransactionPending metrics _txHash = do
    recordTransactionLifecycleStage metrics "pending"

-- | Record transaction included in block
recordTransactionIncluded :: MempoolMetrics -> TransactionHash -> Double -> IO ()
recordTransactionIncluded metrics _txHash age = do
    recordTransactionLifecycleStage metrics "included"
    recordTransactionAge metrics age

-- | Record transaction evicted
recordTransactionEvicted :: MempoolMetrics -> TransactionHash -> EvictionReason -> IO ()
recordTransactionEvicted metrics _txHash reason = do
    let reasonText = evictionReasonText reason
    recordTransactionEvicted metrics reasonText

-- | Calculate transaction age
calculateTransactionAge :: UTCTime -> UTCTime -> Double
calculateTransactionAge startTime currentTime =
    realToFrac $ diffUTCTime currentTime startTime

-- | Get current lifecycle stage
getLifecycleStage :: TransactionLifecycle -> LifecycleStage
getLifecycleStage lifecycle
    | isNothing (_lifecycleIncluded lifecycle) = StageIncluded
    | isNothing (_lifecycleEvicted lifecycle) = StageEvicted
    | isNothing (_lifecyclePending lifecycle) = StagePending
    | isNothing (_lifecycleValidated lifecycle) = StageValidated
    | otherwise = StageReceived
  where
    isNothing = (== Nothing)

-- -------------------------------------------------------------------------- --
-- Fee Analysis Functions

-- | Record transaction fee
recordTransactionFee :: MempoolMetrics -> Double -> IO ()
recordTransactionFee metrics fee = do
    recordTransactionFee metrics fee

-- | Record fee distribution analysis
recordFeeDistribution :: MempoolMetrics -> [Double] -> IO ()
recordFeeDistribution metrics fees = do
    let distribution = calculateFeeDistribution fees
    -- Record percentiles as individual metrics
    recordTransactionFee metrics (_feeMedian distribution)

-- | Calculate fee percentile
calculateFeePercentile :: [Double] -> Double -> Double
calculateFeePercentile fees percentile
    | null fees = 0.0
    | otherwise =
        let sorted = sortBy compare fees
            index = round $ percentile * fromIntegral (length sorted - 1) / 100.0
        in sorted !! min (length sorted - 1) (max 0 index)

-- | Analyze fee market conditions
analyzeFeeMarket :: [Double] -> [Double] -> FeeDistribution
analyzeFeeMarket currentFees _historicalFees = calculateFeeDistribution currentFees

-- | Record gas price correlation
recordGasPriceCorrelation :: MempoolMetrics -> Double -> Double -> IO ()
recordGasPriceCorrelation metrics _gasPrice _inclusionTime = do
    -- Could implement correlation analysis between gas price and inclusion time
    pure ()

-- | Track fee trends over time
trackFeeTrends :: MempoolMetrics -> [FeeDistribution] -> IO ()
trackFeeTrends _metrics _distributions = do
    -- Could implement trend analysis
    pure ()

-- -------------------------------------------------------------------------- --
-- Transaction Age Tracking

-- | Record transaction age
recordTransactionAge :: MempoolMetrics -> Double -> IO ()
recordTransactionAge metrics age = do
    recordTransactionAge metrics age

-- | Update transaction age
updateTransactionAge :: TransactionLifecycle -> IO TransactionLifecycle
updateTransactionAge lifecycle = do
    now <- getCurrentTime
    pure lifecycle { _lifecycleLastSeen = now }

-- | Categorize transaction age
categorizeTransactionAge :: Double -> TransactionAge
categorizeTransactionAge age
    | age < 1 = AgeFresh
    | age < 10 = AgeRecent
    | age < 60 = AgeWaiting
    | age < 300 = AgeOld
    | otherwise = AgeStale

-- | Record age distribution
recordAgeDistribution :: MempoolMetrics -> [Double] -> IO ()
recordAgeDistribution metrics ages = do
    forM_ ages $ recordTransactionAge metrics

-- | Track age versus inclusion correlation
trackAgeVersusInclusion :: MempoolMetrics -> [(Double, Bool)] -> IO ()
trackAgeVersusInclusion _metrics _ageInclusionPairs = do
    -- Could implement correlation analysis
    pure ()

-- -------------------------------------------------------------------------- --
-- Mempool Synchronization Metrics

-- | Record sync latency
recordSyncLatency :: MempoolMetrics -> Double -> IO ()
recordSyncLatency metrics latency = do
    recordMempoolSyncLatency metrics latency

-- | Record sync bandwidth
recordSyncBandwidth :: MempoolMetrics -> Natural -> IO ()
recordSyncBandwidth metrics bytes = do
    recordMempoolSyncBandwidth metrics bytes

-- | Record sync throughput
recordSyncThroughput :: MempoolMetrics -> Double -> IO ()
recordSyncThroughput _metrics _throughput = do
    -- Could add specific throughput metrics
    pure ()

-- | Record peer sync performance
recordPeerSyncPerformance :: MempoolMetrics -> Text -> Double -> Natural -> IO ()
recordPeerSyncPerformance metrics _peerId latency bandwidth = do
    recordSyncLatency metrics latency
    recordSyncBandwidth metrics bandwidth

-- | Record sync failure
recordSyncFailure :: MempoolMetrics -> Text -> IO ()
recordSyncFailure _metrics _reason = do
    -- Could add specific failure tracking
    pure ()

-- | Record sync success
recordSyncSuccess :: MempoolMetrics -> Natural -> IO ()
recordSyncSuccess metrics _transactionCount = do
    -- Could add specific success tracking
    pure ()

-- -------------------------------------------------------------------------- --
-- Priority and Eviction Analysis

-- | Record transaction priority
recordTransactionPriority :: MempoolMetrics -> TransactionHash -> Natural -> IO ()
recordTransactionPriority _metrics _txHash _priority = do
    -- Could implement priority tracking
    pure ()

-- | Record eviction event
recordEvictionEvent :: MempoolMetrics -> TransactionHash -> EvictionReason -> IO ()
recordEvictionEvent metrics txHash reason = do
    recordTransactionEvicted metrics txHash reason

-- | Record replacement event
recordReplacementEvent :: MempoolMetrics -> TransactionHash -> TransactionHash -> IO ()
recordReplacementEvent metrics _oldTxHash _newTxHash = do
    recordTransactionReplacement metrics

-- | Analyze priority inversion
analyzePriorityInversion :: [TransactionLifecycle] -> IO [(TransactionHash, Natural)]
analyzePriorityInversion _lifecycles = do
    -- Could implement priority inversion detection
    pure []

-- | Track capacity utilization
trackCapacityUtilization :: MempoolMetrics -> Natural -> Natural -> IO ()
trackCapacityUtilization metrics currentSize maxCapacity = do
    let utilization = fromIntegral currentSize / fromIntegral maxCapacity
    updateMempoolCapacityUtilization metrics utilization maxCapacity

-- | Record eviction reason
recordEvictionReason :: MempoolMetrics -> EvictionReason -> IO ()
recordEvictionReason metrics reason = do
    let reasonText = evictionReasonText reason
    recordEvictionReason metrics reasonText

-- -------------------------------------------------------------------------- --
-- Advanced Analytics

-- | Record comprehensive transaction metrics
recordTransactionMetrics :: MempoolMetrics -> TransactionMetrics -> IO ()
recordTransactionMetrics metrics txMetrics = do
    let lifecycle = _metricsLifecycle txMetrics
    recordTransactionFee metrics (_lifecycleFee lifecycle)
    let age = calculateTransactionAge (_lifecycleReceived lifecycle) (_lifecycleLastSeen lifecycle)
    recordTransactionAge metrics age

-- | Calculate mempool health metrics
calculateMempoolHealth :: [TransactionLifecycle] -> IO MempoolHealthMetrics
calculateMempoolHealth lifecycles = do
    let totalTxs = fromIntegral $ length lifecycles
    let validatedTxs = length $ filter ((/= Nothing) . _lifecycleValidated) lifecycles
    let includedTxs = length $ filter ((/= Nothing) . _lifecycleIncluded) lifecycles
    let evictedTxs = length $ filter ((/= Nothing) . _lifecycleEvicted) lifecycles

    now <- getCurrentTime
    let ages = map (\lc -> calculateTransactionAge (_lifecycleReceived lc) now) lifecycles
    let avgAge = if null ages then 0.0 else sum ages / fromIntegral (length ages)

    pure MempoolHealthMetrics
        { _healthTotalTransactions = round totalTxs
        , _healthValidationRate = fromIntegral validatedTxs / totalTxs
        , _healthInclusionRate = fromIntegral includedTxs / totalTxs
        , _healthEvictionRate = fromIntegral evictedTxs / totalTxs
        , _healthAverageAge = avgAge
        , _healthFeeEfficiency = 1.0  -- Placeholder
        , _healthCapacityUtilization = 0.8  -- Placeholder
        , _healthSyncPerformance = 0.9  -- Placeholder
        }

-- | Detect anomalies in transaction patterns
detectAnomalies :: [TransactionLifecycle] -> IO [Text]
detectAnomalies _lifecycles = do
    -- Could implement anomaly detection
    pure []

-- | Generate lifecycle report
generateLifecycleReport :: [TransactionLifecycle] -> IO Text
generateLifecycleReport lifecycles = do
    health <- calculateMempoolHealth lifecycles
    pure $ T.unlines
        [ "Transaction Lifecycle Report"
        , "============================"
        , "Total Transactions: " <> T.pack (show $ _healthTotalTransactions health)
        , "Validation Rate: " <> T.pack (show $ _healthValidationRate health)
        , "Inclusion Rate: " <> T.pack (show $ _healthInclusionRate health)
        , "Eviction Rate: " <> T.pack (show $ _healthEvictionRate health)
        , "Average Age: " <> T.pack (show $ _healthAverageAge health) <> " seconds"
        ]

-- -------------------------------------------------------------------------- --
-- Instrumented Functions

-- | Instrumented version of syncMempools
instrumentedSyncMempools
    :: MempoolMetrics
    -> (a -> b -> c -> d -> IO e)  -- Original syncMempools function
    -> a -> b -> c -> d -> IO e
instrumentedSyncMempools metrics originalSync logFn interval localMempool remoteMempool = do
    startTime <- getCurrentTime
    result <- originalSync logFn interval localMempool remoteMempool
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    recordSyncLatency metrics duration
    pure result

-- | Instrumented mempool insert
instrumentedMempoolInsert
    :: MempoolMetrics
    -> (a -> b -> IO c)  -- Original insert function
    -> a -> b -> IO c
instrumentedMempoolInsert metrics originalInsert insertType txs = do
    -- Record received transactions
    -- In real implementation, would iterate through txs
    recordTransactionReceived metrics undefined 0.0  -- Placeholder
    originalInsert insertType txs

-- | Instrumented mempool evict
instrumentedMempoolEvict
    :: MempoolMetrics
    -> (a -> IO b)  -- Original evict function
    -> a -> IO b
instrumentedMempoolEvict metrics originalEvict txHashes = do
    -- Record evicted transactions
    -- In real implementation, would iterate through txHashes
    recordEvictionEvent metrics undefined EvictionCapacityFull  -- Placeholder
    originalEvict txHashes

-- | Generic lifecycle tracking wrapper
withLifecycleTracking :: MempoolMetrics -> TransactionHash -> IO a -> IO a
withLifecycleTracking metrics txHash action = do
    recordTransactionReceived metrics txHash 0.0
    result <- action
    recordTransactionValidated metrics txHash
    pure result

-- | Generic fee analysis wrapper
withFeeAnalysis :: MempoolMetrics -> Double -> IO a -> IO a
withFeeAnalysis metrics fee action = do
    recordTransactionFee metrics fee
    action

-- -------------------------------------------------------------------------- --
-- Helper Functions

-- | Convert lifecycle stage to text
lifecycleStageText :: LifecycleStage -> Text
lifecycleStageText = \case
    StageReceived -> "received"
    StageValidated -> "validated"
    StagePending -> "pending"
    StageIncluded -> "included"
    StageEvicted -> "evicted"
    StageReplaced -> "replaced"

-- | Convert eviction reason to text
evictionReasonText :: EvictionReason -> Text
evictionReasonText = \case
    EvictionTTLExpired -> "ttl_expired"
    EvictionCapacityFull -> "capacity_full"
    EvictionLowGasPrice -> "low_gas_price"
    EvictionDuplicate -> "duplicate"
    EvictionInvalidSignature -> "invalid_sig"
    EvictionReplacedByHigherFee -> "replaced_higher_fee"
    EvictionSystemShutdown -> "system_shutdown"

-- | Calculate fee distribution statistics
calculateFeeDistribution :: [Double] -> FeeDistribution
calculateFeeDistribution fees =
    let sorted = sortBy compare fees
        len = length sorted
        safeIndex i = sorted !! min (len - 1) (max 0 i)
        mean = if null fees then 0.0 else sum fees / fromIntegral len
        variance = if len <= 1 then 0.0 else sum (map (\x -> (x - mean) ** 2) fees) / fromIntegral (len - 1)
        stdDev = sqrt variance
    in FeeDistribution
        { _feeMin = if null sorted then 0.0 else head sorted
        , _feeMax = if null sorted then 0.0 else last sorted
        , _feeMean = mean
        , _feeMedian = if null sorted then 0.0 else safeIndex (len `div` 2)
        , _feeP95 = calculateFeePercentile fees 95.0
        , _feeP99 = calculateFeePercentile fees 99.0
        , _feeStdDev = stdDev
        , _feeSampleSize = fromIntegral len
        , _feeTimestamp = undefined  -- Would be set by caller
        }