{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Chainweb.Metrics.Database.RocksDB
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Instrumented RocksDB operations with metrics collection
--
module Chainweb.Metrics.Database.RocksDB
( -- * Instrumented Table Operations
  instrumentedTableLookup
, instrumentedTableInsert
, instrumentedTableDelete
, instrumentedTableLookupBatch
, instrumentedTableInsertBatch
, instrumentedTableDeleteBatch

  -- * Instrumented RocksDB Instance
, InstrumentedRocksDbTable(..)
, instrumentRocksDbTable
, withInstrumentedTable

  -- * Sampling Configuration
, SamplingConfig(..)
, defaultSamplingConfig
, shouldSample

  -- * Re-exports
, module Chainweb.Storage.Table.RocksDB
) where

import Control.Exception (finally, try, SomeException, throw)
import Control.Monad (when, void, sequence_)
import Data.Maybe (isJust)
import Data.Foldable (toList)
import System.Random (randomRIO)

import qualified System.Clock as Clock

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Metrics.Database
import Chainweb.ChainId (ChainId)

-- For unsafePartsOf
import Control.Lens (unsafePartsOf)

-- -------------------------------------------------------------------------- --
-- Sampling Configuration

-- | Configuration for operation sampling to reduce metrics overhead
data SamplingConfig = SamplingConfig
    { _samplingEnabled :: !Bool
    -- ^ Whether sampling is enabled
    , _samplingRate :: !Double
    -- ^ Sampling rate between 0.0 and 1.0
    , _alwaysSampleErrors :: !Bool
    -- ^ Whether to always sample operations that result in errors
    , _batchSamplingThreshold :: !Int
    -- ^ Minimum batch size before applying sampling to batch operations
    } deriving (Eq, Show)

-- | Default sampling configuration with conservative settings
defaultSamplingConfig :: SamplingConfig
defaultSamplingConfig = SamplingConfig
    { _samplingEnabled = True
    , _samplingRate = 0.01  -- Sample 1% of operations by default
    , _alwaysSampleErrors = True
    , _batchSamplingThreshold = 10
    }

-- | Determine if an operation should be sampled based on configuration
shouldSample :: SamplingConfig -> IO Bool
shouldSample SamplingConfig{..}
    | not _samplingEnabled = pure False
    | _samplingRate >= 1.0 = pure True
    | _samplingRate <= 0.0 = pure False
    | otherwise = do
        rand <- randomRIO (0.0, 1.0)
        pure (rand <= _samplingRate)

-- -------------------------------------------------------------------------- --
-- Instrumented RocksDB Table

-- | A RocksDB table enhanced with metrics collection capabilities
data InstrumentedRocksDbTable k v = InstrumentedRocksDbTable
    { _instrumentedTable :: !(RocksDbTable k v)
    -- ^ The underlying RocksDB table
    , _instrumentedMetrics :: !(Maybe DatabaseMetricsInterface)
    -- ^ Optional metrics interface for recording operations
    , _instrumentedSampling :: !SamplingConfig
    -- ^ Sampling configuration
    , _instrumentedChainId :: !(Maybe ChainId)
    -- ^ Optional chain ID for labeling
    }

-- | Create an instrumented version of a RocksDB table
instrumentRocksDbTable
    :: RocksDbTable k v
    -> Maybe DatabaseMetricsInterface
    -> SamplingConfig
    -> Maybe ChainId
    -> InstrumentedRocksDbTable k v
instrumentRocksDbTable table metrics sampling chainId = InstrumentedRocksDbTable
    { _instrumentedTable = table
    , _instrumentedMetrics = metrics
    , _instrumentedSampling = sampling
    , _instrumentedChainId = chainId
    }

-- | Execute an action with an instrumented table
withInstrumentedTable
    :: InstrumentedRocksDbTable k v
    -> (InstrumentedRocksDbTable k v -> IO a)
    -> IO a
withInstrumentedTable table action = action table

-- -------------------------------------------------------------------------- --
-- Instrumented Operations

-- | Instrumented version of tableLookup that records metrics
instrumentedTableLookup
    :: InstrumentedRocksDbTable k v
    -> k
    -> IO (Maybe v)
instrumentedTableLookup InstrumentedRocksDbTable{..} key = do
    sample <- shouldSample _instrumentedSampling
    case (_instrumentedMetrics, sample) of
        (Just metrics, True) -> do
            result <- timeReadOperation metrics (tableLookup _instrumentedTable key)

            -- Record cache hit/miss based on result
            case result of
                Just _ -> recordCacheHit metrics
                Nothing -> recordCacheMiss metrics

            pure result
        _ -> tableLookup _instrumentedTable key

-- | Instrumented version of tableInsert that records metrics
instrumentedTableInsert
    :: InstrumentedRocksDbTable k v
    -> k
    -> v
    -> IO ()
instrumentedTableInsert InstrumentedRocksDbTable{..} key value = do
    sample <- shouldSample _instrumentedSampling
    case (_instrumentedMetrics, sample) of
        (Just metrics, True) -> do
            result <- try (timeWriteOperation metrics (tableInsert _instrumentedTable key value))

            case result of
                Left (e :: SomeException) ->
                    -- Always sample errors if configured
                    when (_alwaysSampleErrors _instrumentedSampling) $ do
                        recordDatabaseWrite metrics
                    throw e
                Right () -> pure ()
        _ -> tableInsert _instrumentedTable key value

-- | Instrumented version of tableDelete that records metrics
instrumentedTableDelete
    :: InstrumentedRocksDbTable k v
    -> k
    -> IO ()
instrumentedTableDelete InstrumentedRocksDbTable{..} key = do
    sample <- shouldSample _instrumentedSampling
    case (_instrumentedMetrics, sample) of
        (Just metrics, True) -> do
            result <- try (timeDeleteOperation metrics (tableDelete _instrumentedTable key))

            case result of
                Left (e :: SomeException) ->
                    when (_alwaysSampleErrors _instrumentedSampling) $ do
                        recordDatabaseDelete metrics
                    throw e
                Right () -> pure ()
        _ -> tableDelete _instrumentedTable key

-- | Instrumented version of tableLookupBatch
instrumentedTableLookupBatch
    :: (Traversable t)
    => InstrumentedRocksDbTable k v
    -> t k
    -> IO (t (Maybe v))
instrumentedTableLookupBatch InstrumentedRocksDbTable{..} keys = do
    let keyCount = length (toList keys)
    sample <- if keyCount >= _batchSamplingThreshold _instrumentedSampling
              then shouldSample _instrumentedSampling
              else pure True  -- Always sample small batches

    case (_instrumentedMetrics, sample) of
        (Just metrics, True) -> do
            result <- timeBatchReadOperation metrics keyCount (tableLookupBatch _instrumentedTable keys)

            -- Record cache hit/miss based on results
            let hits = length $ filter isJust $ toList result
            let misses = keyCount - hits
            sequence_ $ replicate hits $ recordCacheHit metrics
            sequence_ $ replicate misses $ recordCacheMiss metrics

            pure result
        _ -> tableLookupBatch _instrumentedTable keys

-- | Instrumented version of tableInsertBatch
instrumentedTableInsertBatch
    :: InstrumentedRocksDbTable k v
    -> [(k, v)]
    -> IO ()
instrumentedTableInsertBatch InstrumentedRocksDbTable{..} pairs = do
    let keyCount = length pairs
    sample <- if keyCount >= _batchSamplingThreshold _instrumentedSampling
              then shouldSample _instrumentedSampling
              else pure True

    case (_instrumentedMetrics, sample) of
        (Just metrics, True) -> do
            result <- try (timeBatchWriteOperation metrics keyCount (tableInsertBatch _instrumentedTable pairs))

            case result of
                Left (e :: SomeException) ->
                    when (_alwaysSampleErrors _instrumentedSampling) $ do
                        sequence_ $ replicate keyCount $ recordDatabaseWrite metrics
                    throw e
                Right () -> pure ()
        _ -> tableInsertBatch _instrumentedTable pairs

-- | Instrumented version of tableDeleteBatch
instrumentedTableDeleteBatch
    :: InstrumentedRocksDbTable k v
    -> [k]
    -> IO ()
instrumentedTableDeleteBatch InstrumentedRocksDbTable{..} keys = do
    let keyCount = length keys
    sample <- if keyCount >= _batchSamplingThreshold _instrumentedSampling
              then shouldSample _instrumentedSampling
              else pure True

    case (_instrumentedMetrics, sample) of
        (Just metrics, True) -> do
            result <- try (timeBatchWriteOperation metrics keyCount (tableDeleteBatch _instrumentedTable keys))

            case result of
                Left (e :: SomeException) ->
                    when (_alwaysSampleErrors _instrumentedSampling) $ do
                        sequence_ $ replicate keyCount $ recordDatabaseDelete metrics
                    throw e
                Right () -> pure ()
        _ -> tableDeleteBatch _instrumentedTable keys

-- -------------------------------------------------------------------------- --
-- ReadableTable and Table Instances

instance ReadableTable (InstrumentedRocksDbTable k v) k v where
    tableLookup = instrumentedTableLookup
    tableMember table key = isJust <$> instrumentedTableLookup table key
    tableLookupBatch' table t = unsafePartsOf t (instrumentedTableLookupBatch table)

instance Table (InstrumentedRocksDbTable k v) k v where
    tableInsert = instrumentedTableInsert
    tableDelete = instrumentedTableDelete
    tableInsertBatch table pairs = instrumentedTableInsertBatch table pairs
    tableDeleteBatch = instrumentedTableDeleteBatch