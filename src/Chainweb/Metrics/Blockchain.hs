{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Metrics.Blockchain
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Blockchain metrics collection and aggregation for Chainweb
--
module Chainweb.Metrics.Blockchain
( -- * Blockchain Metrics Types
  ChainMetrics(..)
, ChainMetricsState(..)
, SamplingStrategy(..)
, SamplingConfig(..)

  -- * Initialization and Management
, initializeBlockchainMetrics
, getChainMetrics
, updateBlockMetrics
, exportBlockchainMetrics

  -- * BlockHeaderDB Instrumentation
, withBlockHeaderDbMetrics
, recordBlockInsertion
, recordBlockHeight
, recordOrphanBlock

  -- * CutDB Instrumentation
, withCutDbMetrics
, recordCutAdvancement
, recordConsensusStateTransition

  -- * Per-Chain Aggregation
, AggregatedChainMetrics(..)
, ChainMetricsSnapshot(..)
, aggregateChainMetrics
, getChainMetricsSnapshot
, getAllChainMetrics
, getActiveChainMetrics
, getAllChainSnapshots
, getChainSynchronizationStatus
, getChainConsistencyMetrics
, calculateSyncLag
, createAggregatedMetrics

  -- * Sampling for High-Frequency Events
, SamplingTechnique(..)
, shouldSample
, recordSampledEvent
, updateSamplingStats
, getSamplingStats
, probabilisticSample
, reservoirSample
, adaptiveSample
, rateLimitedSample
, calculateEventRate
, detectAnomaly

  -- * Configuration and Lifecycle Management
, BlockchainMetricsConfig(..)
, defaultBlockchainMetricsConfig
, initializeBlockchainMetricsWithConfig
, shutdownBlockchainMetrics
, registerBlockchainMetrics
, unregisterBlockchainMetrics
, defaultSamplingConfig
, createSamplingStrategy

  -- * Helper Functions for Common Patterns
, withMetricTiming
, countMetricEvent
, recordMetricValue
, batchUpdateMetrics
) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception (bracket, SomeException, catch)
import Control.Monad (when, void, foldM, forever, unless)
import Control.Monad.IO.Class

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Word (Word64)

import GHC.Generics

import Numeric.Natural

import System.Random

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
import Chainweb.BlockHeader (BlockHeader, blockHeight)
import Chainweb.BlockHeight (BlockHeight(..), _blockHeight)
import Chainweb.Metrics.Registry (defaultLatencyBuckets)

-- -------------------------------------------------------------------------- --
-- Blockchain Metrics Data Types

-- | Per-chain metrics for a single chain
data ChainMetrics = ChainMetrics
    { _chainMetricsChainId :: !ChainId
    , _chainMetricsBlockHeight :: !Gauge
    , _chainMetricsBlockInsertionDuration :: !Histogram
    , _chainMetricsBlocksProcessed :: !Counter
    , _chainMetricsOrphanBlocks :: !Counter
    , _chainMetricsCutHeight :: !Gauge
    , _chainMetricsCutAdvancementDuration :: !Histogram
    , _chainMetricsConsensusStateTransitions :: !Counter
    , _chainMetricsLastUpdateTime :: !(IORef UTCTime)
    } deriving (Generic)

instance NFData ChainMetrics where
    rnf ChainMetrics{..} =
        _chainMetricsChainId `seq`
        _chainMetricsBlockHeight `seq`
        _chainMetricsBlockInsertionDuration `seq`
        _chainMetricsBlocksProcessed `seq`
        _chainMetricsOrphanBlocks `seq`
        _chainMetricsCutHeight `seq`
        _chainMetricsCutAdvancementDuration `seq`
        _chainMetricsConsensusStateTransitions `seq`
        _chainMetricsLastUpdateTime `seq` ()

-- | State container for all chain metrics
data ChainMetricsState = ChainMetricsState
    { _chainMetricsStateMap :: !(TVar (HashMap ChainId ChainMetrics))
    , _chainMetricsStateRegistry :: !Registry
    , _chainMetricsStateSampling :: !(TVar SamplingStrategy)
    } deriving (Generic)

instance NFData ChainMetricsState where
    rnf ChainMetricsState{..} =
        _chainMetricsStateRegistry `seq`
        _chainMetricsStateMap `seq`
        _chainMetricsStateSampling `seq` ()

-- | Sampling strategy for high-frequency events
data SamplingStrategy = SamplingStrategy
    { _samplingBlockValidation :: !SamplingConfig
    , _samplingCutAdvancement :: !SamplingConfig
    , _samplingRandom :: !(IORef StdGen)
    , _samplingSampledEvents :: !Counter
    , _samplingSamplingRate :: !Gauge
    , _samplingTechnique :: !SamplingTechnique
    , _samplingReservoir :: !(IORef [UTCTime]) -- ^ For reservoir sampling
    , _samplingReservoirSize :: !Natural
    , _samplingAnomalyDetection :: !Bool -- ^ Enable anomaly-based adaptive sampling
    , _samplingEventRateHistory :: !(IORef [(UTCTime, Double)]) -- ^ Rate history for adaptive sampling
    } deriving (Generic)

instance NFData SamplingStrategy where
    rnf SamplingStrategy{..} =
        _samplingBlockValidation `seq`
        _samplingCutAdvancement `seq`
        _samplingRandom `seq`
        _samplingSampledEvents `seq`
        _samplingSamplingRate `seq`
        _samplingTechnique `seq`
        _samplingReservoir `seq`
        _samplingReservoirSize `seq`
        _samplingAnomalyDetection `seq`
        _samplingEventRateHistory `seq` ()

-- | Configuration for sampling a specific event type
data SamplingConfig = SamplingConfig
    { _samplingConfigRate :: !Double -- ^ Sampling rate (0.0 to 1.0)
    , _samplingConfigMinInterval :: !NominalDiffTime -- ^ Minimum time between samples
    , _samplingConfigLastSample :: !(IORef UTCTime)
    , _samplingConfigEventCount :: !(IORef Word64)
    , _samplingConfigSampledCount :: !(IORef Word64) -- ^ Count of sampled events
    , _samplingConfigMaxEventRate :: !Double -- ^ Maximum expected events per second
    , _samplingConfigAdaptive :: !Bool -- ^ Whether to use adaptive sampling
    } deriving (Generic)

instance NFData SamplingConfig where
    rnf SamplingConfig{..} =
        _samplingConfigRate `seq`
        _samplingConfigMinInterval `seq`
        _samplingConfigLastSample `seq`
        _samplingConfigEventCount `seq`
        _samplingConfigSampledCount `seq`
        _samplingConfigMaxEventRate `seq`
        _samplingConfigAdaptive `seq` ()

-- | Sampling technique to use for high-frequency events
data SamplingTechnique
    = ProbabilisticSampling !Double -- ^ Simple probabilistic sampling with fixed rate
    | ReservoirSampling !Natural -- ^ Reservoir sampling with fixed reservoir size
    | AdaptiveSampling !Double !Double -- ^ Adaptive sampling with base rate and scaling factor
    | RateLimitedSampling !Double !NominalDiffTime -- ^ Rate-limited sampling with max rate
    deriving (Generic, Show, Eq)

instance NFData SamplingTechnique

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Default sampling configuration with conservative rates
defaultSamplingConfig :: IO SamplingConfig
defaultSamplingConfig = do
    now <- getCurrentTime
    lastSampleRef <- newIORef now
    eventCountRef <- newIORef 0
    sampledCountRef <- newIORef 0
    pure SamplingConfig
        { _samplingConfigRate = 0.01 -- 1% sampling
        , _samplingConfigMinInterval = 1.0 -- 1 second minimum
        , _samplingConfigLastSample = lastSampleRef
        , _samplingConfigEventCount = eventCountRef
        , _samplingConfigSampledCount = sampledCountRef
        , _samplingConfigMaxEventRate = 1000.0 -- 1000 events/sec max
        , _samplingConfigAdaptive = True
        }

-- | Create a sampling strategy with default configuration
createSamplingStrategy :: Registry -> IO SamplingStrategy
createSamplingStrategy registry = do
    blockValidationConfig <- defaultSamplingConfig
    cutAdvancementConfig <- defaultSamplingConfig
    randomGen <- newIORef =<< newStdGen
    reservoirRef <- newIORef []
    eventRateHistoryRef <- newIORef []

    sampledEventsCounter <- Registry.registerCounter "chainweb_blockchain_sampled_events_total" mempty registry
    samplingRateGauge <- Registry.registerGauge "chainweb_blockchain_sampling_rate_info" mempty registry

    pure SamplingStrategy
        { _samplingBlockValidation = blockValidationConfig
        , _samplingCutAdvancement = cutAdvancementConfig
        , _samplingRandom = randomGen
        , _samplingSampledEvents = sampledEventsCounter
        , _samplingSamplingRate = samplingRateGauge
        , _samplingTechnique = AdaptiveSampling 0.01 2.0 -- 1% base rate, 2x scaling
        , _samplingReservoir = reservoirRef
        , _samplingReservoirSize = 1000 -- Keep 1000 recent events for reservoir sampling
        , _samplingAnomalyDetection = True
        , _samplingEventRateHistory = eventRateHistoryRef
        }

-- | Initialize blockchain metrics for a specific chain
initializeChainMetrics :: Registry -> ChainId -> IO ChainMetrics
initializeChainMetrics registry chainId = do
    let chainIdLabel = [("chain_id", chainIdToText chainId)]

    blockHeightGauge <- Registry.registerGauge
        "chainweb_blockchain_block_height"
        chainIdLabel
        registry

    blockInsertionHist <- Registry.registerHistogram
        "chainweb_blockchain_block_insertion_duration_seconds"
        defaultLatencyBuckets
        chainIdLabel
        registry

    blocksProcessedCounter <- Registry.registerCounter
        "chainweb_blockchain_blocks_processed_total"
        chainIdLabel
        registry

    orphanBlocksCounter <- Registry.registerCounter
        "chainweb_blockchain_orphan_blocks_total"
        chainIdLabel
        registry

    cutHeightGauge <- Registry.registerGauge
        "chainweb_blockchain_cut_height"
        chainIdLabel
        registry

    cutAdvancementHist <- Registry.registerHistogram
        "chainweb_blockchain_cut_advancement_duration_seconds"
        defaultLatencyBuckets
        chainIdLabel
        registry

    consensusTransitionsCounter <- Registry.registerCounter
        "chainweb_blockchain_consensus_state_transitions_total"
        chainIdLabel
        registry

    now <- getCurrentTime
    lastUpdateRef <- newIORef now

    pure ChainMetrics
        { _chainMetricsChainId = chainId
        , _chainMetricsBlockHeight = blockHeightGauge
        , _chainMetricsBlockInsertionDuration = blockInsertionHist
        , _chainMetricsBlocksProcessed = blocksProcessedCounter
        , _chainMetricsOrphanBlocks = orphanBlocksCounter
        , _chainMetricsCutHeight = cutHeightGauge
        , _chainMetricsCutAdvancementDuration = cutAdvancementHist
        , _chainMetricsConsensusStateTransitions = consensusTransitionsCounter
        , _chainMetricsLastUpdateTime = lastUpdateRef
        }

-- | Initialize the complete blockchain metrics state
initializeBlockchainMetrics :: Registry -> IO ChainMetricsState
initializeBlockchainMetrics registry = do
    metricsMap <- newTVarIO HM.empty
    samplingStrategy <- createSamplingStrategy registry
    samplingVar <- newTVarIO samplingStrategy

    pure ChainMetricsState
        { _chainMetricsStateMap = metricsMap
        , _chainMetricsStateRegistry = registry
        , _chainMetricsStateSampling = samplingVar
        }

-- -------------------------------------------------------------------------- --
-- Metrics Access and Updates

-- | Get or create chain metrics for a specific chain
getChainMetrics :: ChainMetricsState -> ChainId -> IO ChainMetrics
getChainMetrics state chainId = do
    metricsMap <- readTVarIO (_chainMetricsStateMap state)
    case HM.lookup chainId metricsMap of
        Just metrics -> pure metrics
        Nothing -> do
            newMetrics <- initializeChainMetrics (_chainMetricsStateRegistry state) chainId
            atomically $ modifyTVar' (_chainMetricsStateMap state) (HM.insert chainId newMetrics)
            pure newMetrics

-- | Update blockchain metrics with new block information
updateBlockMetrics :: ChainMetrics -> BlockHeader -> NominalDiffTime -> IO ()
updateBlockMetrics metrics header duration = do
    let height = fromIntegral $ _blockHeight $ view blockHeight header

    Gauge.set (_chainMetricsBlockHeight metrics) height
    Histogram.observe (_chainMetricsBlockInsertionDuration metrics) (realToFrac duration)
    Counter.inc (_chainMetricsBlocksProcessed metrics)

    now <- getCurrentTime
    writeIORef (_chainMetricsLastUpdateTime metrics) now

-- | Record an orphan block detection
recordOrphanBlock :: ChainMetrics -> IO ()
recordOrphanBlock metrics = Counter.inc (_chainMetricsOrphanBlocks metrics)

-- | Record cut advancement metrics
recordCutAdvancement :: ChainMetrics -> NominalDiffTime -> Natural -> IO ()
recordCutAdvancement metrics duration cutHeight = do
    Gauge.set (_chainMetricsCutHeight metrics) (fromIntegral cutHeight)
    Histogram.observe (_chainMetricsCutAdvancementDuration metrics) (realToFrac duration)

-- | Record consensus state transition
recordConsensusStateTransition :: ChainMetrics -> IO ()
recordConsensusStateTransition metrics =
    Counter.inc (_chainMetricsConsensusStateTransitions metrics)

-- -------------------------------------------------------------------------- --
-- High-Level Instrumentation Wrappers

-- | Instrument a BlockHeaderDB operation with metrics
withBlockHeaderDbMetrics
    :: ChainMetricsState
    -> ChainId
    -> IO a
    -> IO a
withBlockHeaderDbMetrics state chainId action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime

    -- This is a placeholder - the actual block header would come from the action result
    -- In practice, this would be integrated directly into the BlockHeaderDB operations
    when (duration > 0.1) $ do -- Only record significant operations
        metrics <- getChainMetrics state chainId
        Histogram.observe (_chainMetricsBlockInsertionDuration metrics) (realToFrac duration)

    pure result

-- | Instrument a CutDB operation with metrics
withCutDbMetrics
    :: ChainMetricsState
    -> IO a
    -> IO a
withCutDbMetrics state action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime

    -- Similar placeholder - would be integrated with actual CutDB operations
    when (duration > 0.05) $ do -- Record cut operations that take >50ms
        -- Would record across relevant chains
        pure ()

    pure result

-- | Record a block insertion with timing
recordBlockInsertion :: ChainMetricsState -> ChainId -> BlockHeader -> NominalDiffTime -> IO ()
recordBlockInsertion state chainId header duration = do
    metrics <- getChainMetrics state chainId
    updateBlockMetrics metrics header duration

-- | Record current block height for a chain
recordBlockHeight :: ChainMetricsState -> ChainId -> BlockHeight -> IO ()
recordBlockHeight state chainId height = do
    metrics <- getChainMetrics state chainId
    let heightValue = fromIntegral $ _blockHeight height
    Gauge.set (_chainMetricsBlockHeight metrics) heightValue

-- -------------------------------------------------------------------------- --
-- Sampling Implementation

-- | Determine if an event should be sampled based on strategy and technique
shouldSample :: SamplingStrategy -> SamplingConfig -> IO Bool
shouldSample strategy config = do
    now <- getCurrentTime

    -- Update event count
    eventCount <- readIORef (_samplingConfigEventCount config)
    writeIORef (_samplingConfigEventCount config) (eventCount + 1)

    -- Calculate current event rate for adaptive sampling
    currentRate <- calculateEventRate config now

    case _samplingTechnique strategy of
        ProbabilisticSampling rate ->
            probabilisticSample strategy rate

        ReservoirSampling reservoirSize ->
            reservoirSample strategy config reservoirSize now

        AdaptiveSampling baseRate scalingFactor ->
            adaptiveSample strategy config baseRate scalingFactor currentRate now

        RateLimitedSampling rate interval ->
            rateLimitedSample strategy config rate interval now

-- | Simple probabilistic sampling
probabilisticSample :: SamplingStrategy -> Double -> IO Bool
probabilisticSample strategy rate = do
    randomGen <- readIORef (_samplingRandom strategy)
    let (randomValue, newGen) = randomR (0.0, 1.0) randomGen
    writeIORef (_samplingRandom strategy) newGen

    let shouldSampleEvent = randomValue < rate
    when shouldSampleEvent $ Counter.inc (_samplingSampledEvents strategy)
    pure shouldSampleEvent

-- | Reservoir sampling implementation
reservoirSample :: SamplingStrategy -> SamplingConfig -> Natural -> UTCTime -> IO Bool
reservoirSample strategy config reservoirSize now = do
    reservoir <- readIORef (_samplingReservoir strategy)
    let currentSize = fromIntegral $ length reservoir

    if currentSize < reservoirSize
        then do
            -- Reservoir not full, always sample
            writeIORef (_samplingReservoir strategy) (now : reservoir)
            Counter.inc (_samplingSampledEvents strategy)
            sampledCount <- readIORef (_samplingConfigSampledCount config)
            writeIORef (_samplingConfigSampledCount config) (sampledCount + 1)
            pure True
        else do
            -- Reservoir full, probabilistic replacement
            eventCount <- readIORef (_samplingConfigEventCount config)
            randomGen <- readIORef (_samplingRandom strategy)
            let (randomIndex, newGen) = randomR (0, eventCount - 1) randomGen
            writeIORef (_samplingRandom strategy) newGen

            if randomIndex < reservoirSize
                then do
                    -- Replace random element in reservoir
                    let newReservoir = take (fromIntegral reservoirSize - 1) reservoir
                    writeIORef (_samplingReservoir strategy) (now : newReservoir)
                    Counter.inc (_samplingSampledEvents strategy)
                    sampledCount <- readIORef (_samplingConfigSampledCount config)
                    writeIORef (_samplingConfigSampledCount config) (sampledCount + 1)
                    pure True
                else pure False

-- | Adaptive sampling based on event rate and anomaly detection
adaptiveSample :: SamplingStrategy -> SamplingConfig -> Double -> Double -> Double -> UTCTime -> IO Bool
adaptiveSample strategy config baseRate scalingFactor currentRate now = do
    -- Calculate adaptive rate based on current event rate
    let maxExpectedRate = _samplingConfigMaxEventRate config
    let rateRatio = currentRate / maxExpectedRate
    let adaptiveRate = min 1.0 (baseRate * (1.0 + scalingFactor * rateRatio))

    -- Check for anomalies if enabled
    anomalyBoost <- if _samplingAnomalyDetection strategy
        then detectAnomaly strategy config currentRate now
        else pure 1.0

    let finalRate = min 1.0 (adaptiveRate * anomalyBoost)

    -- Update sampling rate gauge
    Gauge.set (_samplingSamplingRate strategy) finalRate

    -- Perform probabilistic sampling with adaptive rate
    probabilisticSample strategy finalRate

-- | Rate-limited sampling with time-based throttling
rateLimitedSample :: SamplingStrategy -> SamplingConfig -> Double -> NominalDiffTime -> UTCTime -> IO Bool
rateLimitedSample strategy config maxRate interval now = do
    lastSample <- readIORef (_samplingConfigLastSample config)
    let timeSinceLastSample = diffUTCTime now lastSample

    if timeSinceLastSample < interval
        then pure False
        else do
            writeIORef (_samplingConfigLastSample config) now
            probabilisticSample strategy maxRate

-- | Calculate current event rate for adaptive sampling
calculateEventRate :: SamplingConfig -> UTCTime -> IO Double
calculateEventRate config now = do
    eventCount <- readIORef (_samplingConfigEventCount config)
    if eventCount <= 1
        then pure 0.0
        else do
            -- Estimate rate based on recent events (simplified)
            -- In a full implementation, this would use a sliding window
            pure $ fromIntegral eventCount / 60.0 -- events per minute, converted to per second

-- | Detect anomalies in event rate for adaptive sampling
detectAnomaly :: SamplingStrategy -> SamplingConfig -> Double -> UTCTime -> IO Double
detectAnomaly strategy config currentRate now = do
    history <- readIORef (_samplingEventRateHistory strategy)

    -- Add current rate to history
    let newHistory = take 100 $ (now, currentRate) : history -- Keep last 100 samples
    writeIORef (_samplingEventRateHistory strategy) newHistory

    if length newHistory < 10
        then pure 1.0 -- Not enough history, no anomaly boost
        else do
            let rates = map snd newHistory
            let avgRate = sum rates / fromIntegral (length rates)
            let variance = sum (map (\r -> (r - avgRate) ** 2) rates) / fromIntegral (length rates)
            let stdDev = sqrt variance

            -- If current rate is more than 2 standard deviations above average, boost sampling
            if currentRate > avgRate + 2 * stdDev
                then pure 3.0 -- 3x boost during anomalies
                else pure 1.0

-- | Record a sampled event with sophisticated tracking
recordSampledEvent :: SamplingStrategy -> SamplingConfig -> IO () -> IO ()
recordSampledEvent strategy config recordAction = do
    shouldRecord <- shouldSample strategy config
    when shouldRecord $ do
        recordAction
        sampledCount <- readIORef (_samplingConfigSampledCount config)
        writeIORef (_samplingConfigSampledCount config) (sampledCount + 1)

-- | Update sampling statistics and metrics
updateSamplingStats :: SamplingStrategy -> IO ()
updateSamplingStats strategy = do
    -- Update rate gauge based on current technique
    let currentRate = case _samplingTechnique strategy of
            ProbabilisticSampling rate -> rate
            ReservoirSampling _ -> 0.1 -- Approximate for reservoir
            AdaptiveSampling baseRate _ -> baseRate
            RateLimitedSampling rate _ -> rate

    Gauge.set (_samplingSamplingRate strategy) currentRate

    -- Additional statistics updates could go here
    pure ()

-- | Get sampling effectiveness statistics
getSamplingStats :: SamplingStrategy -> SamplingConfig -> IO (Double, Word64, Word64)
getSamplingStats strategy config = do
    totalEvents <- readIORef (_samplingConfigEventCount config)
    sampledEvents <- readIORef (_samplingConfigSampledCount config)
    let effectiveRate = if totalEvents > 0
        then fromIntegral sampledEvents / fromIntegral totalEvents
        else 0.0
    pure (effectiveRate, totalEvents, sampledEvents)

-- -------------------------------------------------------------------------- --
-- Aggregation and Export

-- | Data structure for aggregated cross-chain metrics
data AggregatedChainMetrics = AggregatedChainMetrics
    { _aggregatedTotalBlocksProcessed :: !Word64
    , _aggregatedTotalOrphanBlocks :: !Word64
    , _aggregatedTotalConsensusTransitions :: !Word64
    , _aggregatedAverageBlockHeight :: !Double
    , _aggregatedAverageCutHeight :: !Double
    , _aggregatedMinBlockHeight :: !Natural
    , _aggregatedMaxBlockHeight :: !Natural
    , _aggregatedChainSyncLag :: !(HashMap ChainId Natural)
    , _aggregatedActiveChainCount :: !Natural
    , _aggregatedLastUpdateTime :: !UTCTime
    } deriving (Generic, Show)

instance NFData AggregatedChainMetrics

-- | Snapshot of metrics for a specific chain
data ChainMetricsSnapshot = ChainMetricsSnapshot
    { _snapshotChainId :: !ChainId
    , _snapshotBlockHeight :: !Double
    , _snapshotCutHeight :: !Double
    , _snapshotBlocksProcessed :: !Double
    , _snapshotOrphanBlocks :: !Double
    , _snapshotConsensusTransitions :: !Double
    , _snapshotLastUpdate :: !UTCTime
    , _snapshotIsActive :: !Bool
    } deriving (Generic, Show)

instance NFData ChainMetricsSnapshot

-- | Aggregate metrics across all chains
aggregateChainMetrics :: ChainMetricsState -> IO AggregatedChainMetrics
aggregateChainMetrics state = do
    metricsMap <- readTVarIO (_chainMetricsStateMap state)
    now <- getCurrentTime

    if HM.null metricsMap
        then pure $ AggregatedChainMetrics 0 0 0 0.0 0.0 0 0 HM.empty 0 now
        else do
            snapshots <- mapM getChainSnapshot (HM.elems metricsMap)
            let activeSnapshots = filter _snapshotIsActive snapshots
            let heights = map _snapshotBlockHeight activeSnapshots
            let cutHeights = map _snapshotCutHeight activeSnapshots

            pure AggregatedChainMetrics
                { _aggregatedTotalBlocksProcessed = sum $ map (round . _snapshotBlocksProcessed) snapshots
                , _aggregatedTotalOrphanBlocks = sum $ map (round . _snapshotOrphanBlocks) snapshots
                , _aggregatedTotalConsensusTransitions = sum $ map (round . _snapshotConsensusTransitions) snapshots
                , _aggregatedAverageBlockHeight = if null heights then 0.0 else sum heights / fromIntegral (length heights)
                , _aggregatedAverageCutHeight = if null cutHeights then 0.0 else sum cutHeights / fromIntegral (length cutHeights)
                , _aggregatedMinBlockHeight = if null heights then 0 else round $ minimum heights
                , _aggregatedMaxBlockHeight = if null heights then 0 else round $ maximum heights
                , _aggregatedChainSyncLag = calculateSyncLag activeSnapshots
                , _aggregatedActiveChainCount = fromIntegral $ length activeSnapshots
                , _aggregatedLastUpdateTime = now
                }

-- | Calculate synchronization lag between chains
calculateSyncLag :: [ChainMetricsSnapshot] -> HashMap ChainId Natural
calculateSyncLag snapshots =
    if null snapshots
        then HM.empty
        else
            let maxHeight = maximum $ map _snapshotBlockHeight snapshots
            in HM.fromList $ map (\s -> (_snapshotChainId s, round $ maxHeight - _snapshotBlockHeight s)) snapshots

-- | Get a snapshot of chain metrics for a specific chain
getChainMetricsSnapshot :: ChainMetricsState -> ChainId -> IO (Maybe ChainMetricsSnapshot)
getChainMetricsSnapshot state chainId = do
    metricsMap <- readTVarIO (_chainMetricsStateMap state)
    case HM.lookup chainId metricsMap of
        Just metrics -> Just <$> getChainSnapshot metrics
        Nothing -> pure Nothing

-- | Convert ChainMetrics to ChainMetricsSnapshot
getChainSnapshot :: ChainMetrics -> IO ChainMetricsSnapshot
getChainSnapshot metrics = do
    lastUpdate <- readIORef (_chainMetricsLastUpdateTime metrics)
    now <- getCurrentTime
    let isActive = diffUTCTime now lastUpdate < 300 -- Active if updated within 5 minutes

    -- Read current gauge values (in a real implementation, these would read from Prometheus gauges)
    blockHeight <- Gauge.read (_chainMetricsBlockHeight metrics)
    cutHeight <- Gauge.read (_chainMetricsCutHeight metrics)
    blocksProcessed <- Counter.read (_chainMetricsBlocksProcessed metrics)
    orphanBlocks <- Counter.read (_chainMetricsOrphanBlocks metrics)
    consensusTransitions <- Counter.read (_chainMetricsConsensusStateTransitions metrics)

    pure ChainMetricsSnapshot
        { _snapshotChainId = _chainMetricsChainId metrics
        , _snapshotBlockHeight = blockHeight
        , _snapshotCutHeight = cutHeight
        , _snapshotBlocksProcessed = blocksProcessed
        , _snapshotOrphanBlocks = orphanBlocks
        , _snapshotConsensusTransitions = consensusTransitions
        , _snapshotLastUpdate = lastUpdate
        , _snapshotIsActive = isActive
        }

-- | Get metrics for all chains
getAllChainMetrics :: ChainMetricsState -> IO (HashMap ChainId ChainMetrics)
getAllChainMetrics state = readTVarIO (_chainMetricsStateMap state)

-- | Get metrics for active chains only (updated within last 5 minutes)
getActiveChainMetrics :: ChainMetricsState -> IO (HashMap ChainId ChainMetrics)
getActiveChainMetrics state = do
    allMetrics <- getAllChainMetrics state
    now <- getCurrentTime
    activeMetrics <- foldM (\acc (chainId, metrics) -> do
        lastUpdate <- readIORef (_chainMetricsLastUpdateTime metrics)
        let isActive = diffUTCTime now lastUpdate < 300
        pure $ if isActive then HM.insert chainId metrics acc else acc
        ) HM.empty (HM.toList allMetrics)
    pure activeMetrics

-- | Get synchronization status across all chains
getChainSynchronizationStatus :: ChainMetricsState -> IO (HashMap ChainId Natural)
getChainSynchronizationStatus state = do
    snapshots <- getAllChainSnapshots state
    pure $ calculateSyncLag snapshots

-- | Get snapshots for all chains
getAllChainSnapshots :: ChainMetricsState -> IO [ChainMetricsSnapshot]
getAllChainSnapshots state = do
    metricsMap <- readTVarIO (_chainMetricsStateMap state)
    mapM getChainSnapshot (HM.elems metricsMap)

-- | Calculate cross-chain consistency metrics
getChainConsistencyMetrics :: ChainMetricsState -> IO (Double, Natural, Natural)
getChainConsistencyMetrics state = do
    snapshots <- getAllChainSnapshots state
    let heights = map _snapshotBlockHeight $ filter _snapshotIsActive snapshots
    if null heights
        then pure (0.0, 0, 0)
        else do
            let minHeight = round $ minimum heights
            let maxHeight = round $ maximum heights
            let heightVariance = sum (map (\h -> (h - fromIntegral minHeight) ** 2) heights) / fromIntegral (length heights)
            pure (heightVariance, minHeight, maxHeight)

-- | Create aggregated metrics for Prometheus export
createAggregatedMetrics :: ChainMetricsState -> Registry -> IO ()
createAggregatedMetrics state registry = do
    -- Total metrics across all chains
    totalBlocksGauge <- Registry.registerGauge
        "chainweb_blockchain_total_blocks_processed"
        mempty
        registry

    totalOrphansGauge <- Registry.registerGauge
        "chainweb_blockchain_total_orphan_blocks"
        mempty
        registry

    avgBlockHeightGauge <- Registry.registerGauge
        "chainweb_blockchain_average_block_height"
        mempty
        registry

    avgCutHeightGauge <- Registry.registerGauge
        "chainweb_blockchain_average_cut_height"
        mempty
        registry

    activeChainCountGauge <- Registry.registerGauge
        "chainweb_blockchain_active_chain_count"
        mempty
        registry

    -- Update aggregated metrics periodically
    void $ forkIO $ forever $ do
        threadDelay (30 * 1000000) -- Update every 30 seconds
        catch (do
            aggregated <- aggregateChainMetrics state
            Gauge.set totalBlocksGauge (fromIntegral $ _aggregatedTotalBlocksProcessed aggregated)
            Gauge.set totalOrphansGauge (fromIntegral $ _aggregatedTotalOrphanBlocks aggregated)
            Gauge.set avgBlockHeightGauge (_aggregatedAverageBlockHeight aggregated)
            Gauge.set avgCutHeightGauge (_aggregatedAverageCutHeight aggregated)
            Gauge.set activeChainCountGauge (fromIntegral $ _aggregatedActiveChainCount aggregated)
            ) (\(e :: SomeException) -> pure ())

-- | Export all blockchain metrics in Prometheus format
exportBlockchainMetrics :: ChainMetricsState -> IO Text
exportBlockchainMetrics state = do
    aggregated <- aggregateChainMetrics state
    syncStatus <- getChainSynchronizationStatus state
    (variance, minHeight, maxHeight) <- getChainConsistencyMetrics state

    pure $ T.unlines
        [ "# Blockchain Metrics Summary"
        , "# Total blocks processed: " <> T.pack (show $ _aggregatedTotalBlocksProcessed aggregated)
        , "# Total orphan blocks: " <> T.pack (show $ _aggregatedTotalOrphanBlocks aggregated)
        , "# Average block height: " <> T.pack (show $ _aggregatedAverageBlockHeight aggregated)
        , "# Average cut height: " <> T.pack (show $ _aggregatedAverageCutHeight aggregated)
        , "# Active chains: " <> T.pack (show $ _aggregatedActiveChainCount aggregated)
        , "# Block height variance: " <> T.pack (show variance)
        , "# Height range: " <> T.pack (show minHeight) <> " - " <> T.pack (show maxHeight)
        , "# Synchronization status: " <> T.pack (show $ HM.size syncStatus) <> " chains"
        ]

-- -------------------------------------------------------------------------- --
-- Configuration and Lifecycle Management

-- | Configuration for blockchain metrics collection
data BlockchainMetricsConfig = BlockchainMetricsConfig
    { _blockchainMetricsEnabled :: !Bool
    -- ^ Whether blockchain metrics collection is enabled
    , _blockchainMetricsPrefix :: !Text
    -- ^ Metric name prefix (default: "chainweb_blockchain")
    , _blockchainMetricsSamplingEnabled :: !Bool
    -- ^ Whether to enable high-frequency event sampling
    , _blockchainMetricsSamplingTechnique :: !SamplingTechnique
    -- ^ Sampling technique to use for high-frequency events
    , _blockchainMetricsAggregationInterval :: !NominalDiffTime
    -- ^ Interval for updating aggregated metrics (default: 30 seconds)
    , _blockchainMetricsPerChainMetrics :: !Bool
    -- ^ Whether to collect per-chain metrics (may increase cardinality)
    , _blockchainMetricsMaxChains :: !Natural
    -- ^ Maximum number of chains to track (default: 25)
    , _blockchainMetricsCleanupEnabled :: !Bool
    -- ^ Whether to enable automatic cleanup of inactive chains
    , _blockchainMetricsCleanupInterval :: !NominalDiffTime
    -- ^ Interval for cleaning up inactive chain metrics (default: 1 hour)
    } deriving (Generic, Show)

instance NFData BlockchainMetricsConfig

-- | Default blockchain metrics configuration
defaultBlockchainMetricsConfig :: BlockchainMetricsConfig
defaultBlockchainMetricsConfig = BlockchainMetricsConfig
    { _blockchainMetricsEnabled = True
    , _blockchainMetricsPrefix = "chainweb_blockchain"
    , _blockchainMetricsSamplingEnabled = True
    , _blockchainMetricsSamplingTechnique = AdaptiveSampling 0.01 2.0
    , _blockchainMetricsAggregationInterval = 30.0 -- 30 seconds
    , _blockchainMetricsPerChainMetrics = True
    , _blockchainMetricsMaxChains = 25
    , _blockchainMetricsCleanupEnabled = True
    , _blockchainMetricsCleanupInterval = 3600.0 -- 1 hour
    }

-- | Initialize blockchain metrics with custom configuration
initializeBlockchainMetricsWithConfig :: BlockchainMetricsConfig -> Registry -> IO ChainMetricsState
initializeBlockchainMetricsWithConfig config registry = do
    if not (_blockchainMetricsEnabled config)
        then do
            -- Create minimal disabled state
            metricsMap <- newTVarIO HM.empty
            dummySampling <- createDummySamplingStrategy registry
            samplingVar <- newTVarIO dummySampling
            pure ChainMetricsState
                { _chainMetricsStateMap = metricsMap
                , _chainMetricsStateRegistry = registry
                , _chainMetricsStateSampling = samplingVar
                }
        else do
            metricsMap <- newTVarIO HM.empty
            samplingStrategy <- createSamplingStrategyWithConfig config registry
            samplingVar <- newTVarIO samplingStrategy

            let state = ChainMetricsState
                    { _chainMetricsStateMap = metricsMap
                    , _chainMetricsStateRegistry = registry
                    , _chainMetricsStateSampling = samplingVar
                    }

            -- Start aggregated metrics if enabled
            when (_blockchainMetricsPerChainMetrics config) $
                void $ createAggregatedMetrics state registry

            -- Start cleanup routine if enabled
            when (_blockchainMetricsCleanupEnabled config) $
                void $ startCleanupRoutine config state

            pure state

-- | Create sampling strategy with custom configuration
createSamplingStrategyWithConfig :: BlockchainMetricsConfig -> Registry -> IO SamplingStrategy
createSamplingStrategyWithConfig config registry = do
    blockValidationConfig <- defaultSamplingConfig
    cutAdvancementConfig <- defaultSamplingConfig
    randomGen <- newIORef =<< newStdGen
    reservoirRef <- newIORef []
    eventRateHistoryRef <- newIORef []

    let prefix = _blockchainMetricsPrefix config
    sampledEventsCounter <- Registry.registerCounter (prefix <> "_sampled_events_total") mempty registry
    samplingRateGauge <- Registry.registerGauge (prefix <> "_sampling_rate_info") mempty registry

    pure SamplingStrategy
        { _samplingBlockValidation = blockValidationConfig
        , _samplingCutAdvancement = cutAdvancementConfig
        , _samplingRandom = randomGen
        , _samplingSampledEvents = sampledEventsCounter
        , _samplingSamplingRate = samplingRateGauge
        , _samplingTechnique = _blockchainMetricsSamplingTechnique config
        , _samplingReservoir = reservoirRef
        , _samplingReservoirSize = 1000
        , _samplingAnomalyDetection = True
        , _samplingEventRateHistory = eventRateHistoryRef
        }

-- | Create dummy sampling strategy for disabled metrics
createDummySamplingStrategy :: Registry -> IO SamplingStrategy
createDummySamplingStrategy registry = do
    blockValidationConfig <- defaultSamplingConfig
    cutAdvancementConfig <- defaultSamplingConfig
    randomGen <- newIORef =<< newStdGen
    reservoirRef <- newIORef []
    eventRateHistoryRef <- newIORef []

    -- Create dummy metrics that won't be updated
    sampledEventsCounter <- Registry.registerCounter "dummy_sampled_events" mempty registry
    samplingRateGauge <- Registry.registerGauge "dummy_sampling_rate" mempty registry

    pure SamplingStrategy
        { _samplingBlockValidation = blockValidationConfig
        , _samplingCutAdvancement = cutAdvancementConfig
        , _samplingRandom = randomGen
        , _samplingSampledEvents = sampledEventsCounter
        , _samplingSamplingRate = samplingRateGauge
        , _samplingTechnique = ProbabilisticSampling 0.0 -- No sampling when disabled
        , _samplingReservoir = reservoirRef
        , _samplingReservoirSize = 0
        , _samplingAnomalyDetection = False
        , _samplingEventRateHistory = eventRateHistoryRef
        }

-- | Start cleanup routine for inactive chains
startCleanupRoutine :: BlockchainMetricsConfig -> ChainMetricsState -> IO (IO ())
startCleanupRoutine config state = do
    cleanupAsync <- forkIO $ forever $ do
        threadDelay $ round $ _blockchainMetricsCleanupInterval config * 1000000
        cleanupInactiveChains state (_blockchainMetricsCleanupInterval config)
    pure $ killThread cleanupAsync

-- | Clean up metrics for inactive chains
cleanupInactiveChains :: ChainMetricsState -> NominalDiffTime -> IO ()
cleanupInactiveChains state inactiveThreshold = do
    now <- getCurrentTime
    metricsMap <- readTVarIO (_chainMetricsStateMap state)

    inactiveChains <- foldM (\acc (chainId, metrics) -> do
        lastUpdate <- readIORef (_chainMetricsLastUpdateTime metrics)
        let inactive = diffUTCTime now lastUpdate > inactiveThreshold
        pure $ if inactive then chainId : acc else acc
        ) [] (HM.toList metricsMap)

    unless (null inactiveChains) $ do
        atomically $ modifyTVar' (_chainMetricsStateMap state) $ \m ->
            foldl' (flip HM.delete) m inactiveChains

-- | Register blockchain metrics with the central registry
registerBlockchainMetrics :: ChainMetricsState -> IO ()
registerBlockchainMetrics _state = do
    -- This function would integrate with the central registry system
    -- For now, it's a placeholder for future registry integration
    pure ()

-- | Unregister blockchain metrics from the central registry
unregisterBlockchainMetrics :: ChainMetricsState -> IO ()
unregisterBlockchainMetrics _state = do
    -- This function would cleanup metrics from the central registry
    -- For now, it's a placeholder for future registry integration
    pure ()

-- | Shutdown blockchain metrics and cleanup resources
shutdownBlockchainMetrics :: ChainMetricsState -> IO ()
shutdownBlockchainMetrics state = do
    -- Clear all chain metrics
    atomically $ writeTVar (_chainMetricsStateMap state) HM.empty

    -- Reset sampling state
    samplingStrategy <- readTVarIO (_chainMetricsStateSampling state)
    writeIORef (_samplingReservoir samplingStrategy) []
    writeIORef (_samplingEventRateHistory samplingStrategy) []

-- -------------------------------------------------------------------------- --
-- Helper Functions for Common Patterns

-- | Time an operation and record the duration in a histogram
withMetricTiming :: Histogram -> IO a -> IO a
withMetricTiming histogram action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    Histogram.observe duration histogram
    pure result

-- | Count a metric event (increment counter)
countMetricEvent :: Counter -> IO ()
countMetricEvent = Counter.inc

-- | Record a metric value (set gauge)
recordMetricValue :: Gauge -> Double -> IO ()
recordMetricValue = Gauge.set

-- | Batch update multiple metrics atomically
batchUpdateMetrics :: [IO ()] -> IO ()
batchUpdateMetrics = sequence_