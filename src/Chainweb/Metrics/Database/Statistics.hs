{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Metrics.Database.Statistics
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- RocksDB statistics collection and monitoring
--
module Chainweb.Metrics.Database.Statistics
( -- * Statistics Collection
  RocksDbStatistics(..)
, collectRocksDbStatistics
, parseStatistics

  -- * Periodic Statistics Collection
, StatisticsCollector(..)
, startStatisticsCollector
, stopStatisticsCollector
, withStatisticsCollector

  -- * Cache Statistics
, CacheStatistics(..)
, parseCacheStatistics

  -- * Performance Counters
, PerformanceCounters(..)
, parsePerformanceCounters

  -- * Configuration
, StatisticsConfig(..)
, defaultStatisticsConfig
) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM (STM, TVar, newTVarIO, readTVar, writeTVar, atomically)
import Control.Exception (finally, try, SomeException, bracket)
import Control.Monad (forever, when, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word64)
import Text.Read (readMaybe)

import qualified Database.RocksDB.Base as R

import Chainweb.Metrics.Database

-- -------------------------------------------------------------------------- --
-- Configuration

-- | Configuration for RocksDB statistics collection
data StatisticsConfig = StatisticsConfig
    { _statsCollectionInterval :: !Int
    -- ^ Collection interval in seconds
    , _statsEnabled :: !Bool
    -- ^ Whether statistics collection is enabled
    , _statsParseFailureRetries :: !Int
    -- ^ Number of retries for parsing failures
    , _statsLogParseFailures :: !Bool
    -- ^ Whether to log parsing failures
    } deriving (Eq, Show)

-- | Default statistics configuration
defaultStatisticsConfig :: StatisticsConfig
defaultStatisticsConfig = StatisticsConfig
    { _statsCollectionInterval = 30  -- Collect every 30 seconds
    , _statsEnabled = True
    , _statsParseFailureRetries = 3
    , _statsLogParseFailures = False  -- Avoid spam in production
    }

-- -------------------------------------------------------------------------- --
-- Statistics Data Types

-- | Cache-related statistics
data CacheStatistics = CacheStatistics
    { _cacheBlockCacheHits :: !Word64
    , _cacheBlockCacheMisses :: !Word64
    , _cacheBlockCacheHitRatio :: !Double
    , _cacheIndexAndFilterBlocksHits :: !Word64
    , _cacheIndexAndFilterBlocksMisses :: !Word64
    , _cacheBloomFilterHits :: !Word64
    , _cacheBloomFilterMisses :: !Word64
    } deriving (Eq, Show)

-- | Performance counters
data PerformanceCounters = PerformanceCounters
    { _perfGetCalls :: !Word64
    , _perfPutCalls :: !Word64
    , _perfDeleteCalls :: !Word64
    , _perfIteratorCalls :: !Word64
    , _perfBytesWritten :: !Word64
    , _perfBytesRead :: !Word64
    , _perfCompactionBytesRead :: !Word64
    , _perfCompactionBytesWritten :: !Word64
    } deriving (Eq, Show)

-- | Complete RocksDB statistics
data RocksDbStatistics = RocksDbStatistics
    { _statsCache :: !CacheStatistics
    , _statsPerformance :: !PerformanceCounters
    , _statsLastUpdated :: !Word64  -- Unix timestamp
    , _statsRawData :: !T.Text      -- Raw statistics string for debugging
    } deriving (Eq, Show)

-- -------------------------------------------------------------------------- --
-- Statistics Collection

-- | Collect statistics from a RocksDB instance
collectRocksDbStatistics :: R.DB -> IO (Maybe RocksDbStatistics)
collectRocksDbStatistics db = do
    result <- try $ R.getProperty db R.Stats
    case result of
        Left (e :: SomeException) -> do
            -- Log error but don't fail
            pure Nothing
        Right maybeStats -> case maybeStats of
            Nothing -> pure Nothing
            Just rawStats -> do
                let statsText = T.decodeUtf8 rawStats
                currentTime <- getCurrentUnixTime
                pure $ parseStatistics currentTime statsText

-- | Parse raw statistics text into structured data
parseStatistics :: Word64 -> T.Text -> Maybe RocksDbStatistics
parseStatistics timestamp rawText = do
    cache <- parseCacheStatistics rawText
    perf <- parsePerformanceCounters rawText
    pure $ RocksDbStatistics
        { _statsCache = cache
        , _statsPerformance = perf
        , _statsLastUpdated = timestamp
        , _statsRawData = rawText
        }

-- | Parse cache-related statistics from raw text
parseCacheStatistics :: T.Text -> Maybe CacheStatistics
parseCacheStatistics rawText = do
    let lines = T.lines rawText

    blockCacheHits <- parseStatLine lines "rocksdb.block.cache.hit"
    blockCacheMisses <- parseStatLine lines "rocksdb.block.cache.miss"

    let hitRatio = if blockCacheHits + blockCacheMisses > 0
                   then fromIntegral blockCacheHits / fromIntegral (blockCacheHits + blockCacheMisses)
                   else 0.0

    let indexHits = fromMaybe 0 $ parseStatLine lines "rocksdb.block.cache.index.hit"
    let indexMisses = fromMaybe 0 $ parseStatLine lines "rocksdb.block.cache.index.miss"
    let bloomHits = fromMaybe 0 $ parseStatLine lines "rocksdb.bloom.filter.useful"
    let bloomMisses = fromMaybe 0 $ parseStatLine lines "rocksdb.bloom.filter.checked"

    pure $ CacheStatistics
        { _cacheBlockCacheHits = blockCacheHits
        , _cacheBlockCacheMisses = blockCacheMisses
        , _cacheBlockCacheHitRatio = hitRatio
        , _cacheIndexAndFilterBlocksHits = indexHits
        , _cacheIndexAndFilterBlocksMisses = indexMisses
        , _cacheBloomFilterHits = bloomHits
        , _cacheBloomFilterMisses = bloomMisses
        }

-- | Parse performance counters from raw text
parsePerformanceCounters :: T.Text -> Maybe PerformanceCounters
parsePerformanceCounters rawText = do
    let lines = T.lines rawText

    let getCalls = fromMaybe 0 $ parseStatLine lines "rocksdb.number.db.seek" <|> parseStatLine lines "rocksdb.get.hit.l0"
    let putCalls = fromMaybe 0 $ parseStatLine lines "rocksdb.number.keys.written"
    let deleteCalls = fromMaybe 0 $ parseStatLine lines "rocksdb.number.deletes.filtered"
    let iterCalls = fromMaybe 0 $ parseStatLine lines "rocksdb.number.db.seek"
    let bytesWritten = fromMaybe 0 $ parseStatLine lines "rocksdb.bytes.written"
    let bytesRead = fromMaybe 0 $ parseStatLine lines "rocksdb.bytes.read"
    let compactionRead = fromMaybe 0 $ parseStatLine lines "rocksdb.compact.read.bytes"
    let compactionWritten = fromMaybe 0 $ parseStatLine lines "rocksdb.compact.write.bytes"

    pure $ PerformanceCounters
        { _perfGetCalls = getCalls
        , _perfPutCalls = putCalls
        , _perfDeleteCalls = deleteCalls
        , _perfIteratorCalls = iterCalls
        , _perfBytesWritten = bytesWritten
        , _perfBytesRead = bytesRead
        , _perfCompactionBytesRead = compactionRead
        , _perfCompactionBytesWritten = compactionWritten
        }

-- | Parse a specific statistic line
parseStatLine :: [T.Text] -> T.Text -> Maybe Word64
parseStatLine lines statName = do
    line <- listToMaybe $ filter (T.isPrefixOf statName) lines
    let parts = T.words line
    if length parts >= 2
        then readMaybe . T.unpack $ parts !! 1
        else Nothing

-- -------------------------------------------------------------------------- --
-- Periodic Statistics Collection

-- | Statistics collector that runs in a background thread
data StatisticsCollector = StatisticsCollector
    { _collectorThreadId :: !ThreadId
    , _collectorStats :: !(TVar (Maybe RocksDbStatistics))
    , _collectorConfig :: !StatisticsConfig
    }

-- | Start a background statistics collector
startStatisticsCollector
    :: StatisticsConfig
    -> R.DB
    -> DatabaseMetricsInterface
    -> IO StatisticsCollector
startStatisticsCollector config db metrics = do
    statsVar <- newTVarIO Nothing
    threadId <- forkIO $ statisticsCollectionLoop config db metrics statsVar
    pure $ StatisticsCollector
        { _collectorThreadId = threadId
        , _collectorStats = statsVar
        , _collectorConfig = config
        }

-- | Stop a statistics collector
stopStatisticsCollector :: StatisticsCollector -> IO ()
stopStatisticsCollector collector = killThread (_collectorThreadId collector)

-- | Execute an action with a statistics collector
withStatisticsCollector
    :: StatisticsConfig
    -> R.DB
    -> DatabaseMetricsInterface
    -> (StatisticsCollector -> IO a)
    -> IO a
withStatisticsCollector config db metrics action =
    bracket
        (startStatisticsCollector config db metrics)
        stopStatisticsCollector
        action

-- | Background loop for collecting statistics
statisticsCollectionLoop
    :: StatisticsConfig
    -> R.DB
    -> DatabaseMetricsInterface
    -> TVar (Maybe RocksDbStatistics)
    -> IO ()
statisticsCollectionLoop StatisticsConfig{..} db metrics statsVar =
    when _statsEnabled $ forever $ do
        maybeStats <- collectRocksDbStatistics db
        case maybeStats of
            Just stats -> do
                atomically $ writeTVar statsVar (Just stats)
                updateMetricsFromStatistics metrics stats
            Nothing -> pure ()

        -- Sleep for configured interval
        threadDelay (_statsCollectionInterval * 1000000)  -- Convert to microseconds

-- | Update Prometheus metrics from RocksDB statistics
updateMetricsFromStatistics :: DatabaseMetricsInterface -> RocksDbStatistics -> IO ()
updateMetricsFromStatistics metrics RocksDbStatistics{..} = do
    let CacheStatistics{..} = _statsCache
    let PerformanceCounters{..} = _statsPerformance

    -- Update cache metrics
    sequence_ $ replicate (fromIntegral _cacheBlockCacheHits) $ recordCacheHit metrics
    sequence_ $ replicate (fromIntegral _cacheBlockCacheMisses) $ recordCacheMiss metrics

    -- Note: We would need to track previous values to calculate deltas for counters
    -- For now, we'll update the database size if available
    let totalBytes = _perfBytesWritten + _perfBytesRead
    when (totalBytes > 0) $ updateDatabaseSize metrics totalBytes

-- -------------------------------------------------------------------------- --
-- Utility Functions

-- | Get current Unix timestamp (placeholder - would use actual time library)
getCurrentUnixTime :: IO Word64
getCurrentUnixTime = do
    -- Placeholder implementation - in real code would use proper time library
    pure 0

-- | Alternative operator for Maybe fallback
(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> mb = mb
Just a <|> _ = Just a