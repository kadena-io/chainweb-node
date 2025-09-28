{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Metrics.Database.Compaction
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- RocksDB compaction and storage monitoring
--
module Chainweb.Metrics.Database.Compaction
( -- * Compaction Monitoring
  CompactionMonitor(..)
, CompactionEvent(..)
, startCompactionMonitor
, stopCompactionMonitor
, withCompactionMonitor

  -- * Storage Monitoring
, StorageMonitor(..)
, StorageInfo(..)
, startStorageMonitor
, stopStorageMonitor
, collectStorageInfo
, withStorageMonitor

  -- * Per-Chain Storage Tracking
, ChainStorageTracker(..)
, createChainStorageTracker
, updateChainStorage
, getChainStorageMetrics

  -- * Configuration
, CompactionConfig(..)
, StorageConfig(..)
, defaultCompactionConfig
, defaultStorageConfig
) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, try, SomeException)
import Control.Monad (forever, when, void)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word (Word64)
import System.Directory (getFileSize, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import qualified Database.RocksDB.Base as R
import qualified System.Clock as Clock

import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.Metrics.Database

-- -------------------------------------------------------------------------- --
-- Configuration

-- | Configuration for compaction monitoring
data CompactionConfig = CompactionConfig
    { _compactionMonitoringEnabled :: !Bool
    -- ^ Whether compaction monitoring is enabled
    , _compactionEventLogging :: !Bool
    -- ^ Whether to log compaction events
    , _compactionMetricsEnabled :: !Bool
    -- ^ Whether to collect compaction metrics
    } deriving (Eq, Show)

-- | Default compaction configuration
defaultCompactionConfig :: CompactionConfig
defaultCompactionConfig = CompactionConfig
    { _compactionMonitoringEnabled = True
    , _compactionEventLogging = False  -- Avoid log spam
    , _compactionMetricsEnabled = True
    }

-- | Configuration for storage monitoring
data StorageConfig = StorageConfig
    { _storageMonitoringInterval :: !Int
    -- ^ Monitoring interval in seconds
    , _storagePerChainTracking :: !Bool
    -- ^ Whether to track per-chain storage
    , _storageGrowthTracking :: !Bool
    -- ^ Whether to track storage growth rates
    , _storageThresholdAlerts :: !(Maybe Word64)
    -- ^ Storage threshold for alerts (bytes)
    } deriving (Eq, Show)

-- | Default storage configuration
defaultStorageConfig :: StorageConfig
defaultStorageConfig = StorageConfig
    { _storageMonitoringInterval = 60  -- Check every minute
    , _storagePerChainTracking = True
    , _storageGrowthTracking = True
    , _storageThresholdAlerts = Nothing  -- No alerts by default
    }

-- -------------------------------------------------------------------------- --
-- Compaction Monitoring

-- | Compaction event information
data CompactionEvent = CompactionEvent
    { _compactionStartTime :: !POSIXTime
    , _compactionEndTime :: !(Maybe POSIXTime)
    , _compactionLevel :: !Int
    , _compactionInputBytes :: !Word64
    , _compactionOutputBytes :: !Word64
    , _compactionInputFiles :: !Int
    , _compactionOutputFiles :: !Int
    , _compactionReason :: !T.Text
    } deriving (Eq, Show)

-- | Compaction monitoring state
data CompactionMonitor = CompactionMonitor
    { _compactionConfig :: !CompactionConfig
    , _compactionEvents :: !(TVar [CompactionEvent])
    , _compactionMetrics :: !DatabaseMetricsInterface
    , _compactionThreadId :: !(Maybe ThreadId)
    }

-- | Start monitoring RocksDB compactions
startCompactionMonitor
    :: CompactionConfig
    -> R.DB
    -> DatabaseMetricsInterface
    -> IO CompactionMonitor
startCompactionMonitor config db metrics = do
    eventsVar <- newTVarIO []

    -- Start background monitoring thread if enabled
    threadId <- if _compactionMonitoringEnabled config
        then Just <$> forkIO (compactionMonitoringLoop config db metrics eventsVar)
        else pure Nothing

    pure $ CompactionMonitor
        { _compactionConfig = config
        , _compactionEvents = eventsVar
        , _compactionMetrics = metrics
        , _compactionThreadId = threadId
        }

-- | Stop compaction monitoring
stopCompactionMonitor :: CompactionMonitor -> IO ()
stopCompactionMonitor CompactionMonitor{..} =
    case _compactionThreadId of
        Just tid -> killThread tid
        Nothing -> pure ()

-- | Execute an action with compaction monitoring
withCompactionMonitor
    :: CompactionConfig
    -> R.DB
    -> DatabaseMetricsInterface
    -> (CompactionMonitor -> IO a)
    -> IO a
withCompactionMonitor config db metrics action =
    bracket
        (startCompactionMonitor config db metrics)
        stopCompactionMonitor
        action

-- | Background loop for monitoring compactions
compactionMonitoringLoop
    :: CompactionConfig
    -> R.DB
    -> DatabaseMetricsInterface
    -> TVar [CompactionEvent]
    -> IO ()
compactionMonitoringLoop CompactionConfig{..} db metrics eventsVar =
    when _compactionMonitoringEnabled $ forever $ do
        -- Check for compaction statistics from RocksDB
        maybeStats <- try $ R.getProperty db R.Stats
        case maybeStats of
            Right (Just statsBytes) -> do
                let statsText = T.decodeUtf8 statsBytes
                parseAndRecordCompactionStats statsText metrics eventsVar
            _ -> pure ()

        -- Sleep for a short interval (compactions are relatively infrequent)
        threadDelay (5 * 1000000)  -- 5 seconds

-- | Parse compaction statistics and record metrics
parseAndRecordCompactionStats
    :: T.Text
    -> DatabaseMetricsInterface
    -> TVar [CompactionEvent]
    -> IO ()
parseAndRecordCompactionStats statsText metrics eventsVar = do
    -- Parse compaction-related statistics from the stats text
    let compactionLines = filter (T.isInfixOf "compact") (T.lines statsText)

    -- Look for compaction duration info
    case parseCompactionDuration compactionLines of
        Just duration -> recordCompactionEvent metrics duration
        Nothing -> pure ()

-- | Parse compaction duration from statistics lines
parseCompactionDuration :: [T.Text] -> Maybe Double
parseCompactionDuration lines = do
    -- Look for compaction timing information in the statistics
    -- This is a simplified parser - real implementation would be more robust
    compactionLine <- listToMaybe $ filter (T.isInfixOf "rocksdb.compact") lines
    let words = T.words compactionLine
    if length words >= 2
        then readMaybe . T.unpack $ words !! 1
        else Nothing
  where
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

-- -------------------------------------------------------------------------- --
-- Storage Monitoring

-- | Storage information for a database
data StorageInfo = StorageInfo
    { _storageSize :: !Word64
    -- ^ Total size in bytes
    , _storageFiles :: !Int
    -- ^ Number of files
    , _storageLastModified :: !POSIXTime
    -- ^ Last modification time
    , _storageGrowthRate :: !(Maybe Double)
    -- ^ Growth rate in bytes per second
    } deriving (Eq, Show)

-- | Storage monitoring state
data StorageMonitor = StorageMonitor
    { _storageConfig :: !StorageConfig
    , _storageDbPath :: !FilePath
    , _storageHistory :: !(IORef [(POSIXTime, Word64)])
    , _storageMetrics :: !DatabaseMetricsInterface
    , _storageThreadId :: !(Maybe ThreadId)
    }

-- | Start storage monitoring
startStorageMonitor
    :: StorageConfig
    -> FilePath  -- ^ Database path
    -> DatabaseMetricsInterface
    -> IO StorageMonitor
startStorageMonitor config dbPath metrics = do
    historyRef <- newIORef []

    -- Start background monitoring thread
    threadId <- Just <$> forkIO (storageMonitoringLoop config dbPath metrics historyRef)

    pure $ StorageMonitor
        { _storageConfig = config
        , _storageDbPath = dbPath
        , _storageHistory = historyRef
        , _storageMetrics = metrics
        , _storageThreadId = threadId
        }

-- | Stop storage monitoring
stopStorageMonitor :: StorageMonitor -> IO ()
stopStorageMonitor StorageMonitor{..} =
    case _storageThreadId of
        Just tid -> killThread tid
        Nothing -> pure ()

-- | Execute an action with storage monitoring
withStorageMonitor
    :: StorageConfig
    -> FilePath
    -> DatabaseMetricsInterface
    -> (StorageMonitor -> IO a)
    -> IO a
withStorageMonitor config dbPath metrics action =
    bracket
        (startStorageMonitor config dbPath metrics)
        stopStorageMonitor
        action

-- | Collect current storage information
collectStorageInfo :: FilePath -> IO (Maybe StorageInfo)
collectStorageInfo dbPath = do
    result <- try $ do
        exists <- doesDirectoryExist dbPath
        if exists
            then do
                size <- getDirectorySize dbPath
                files <- length <$> listDirectory dbPath
                lastModified <- getPOSIXTime  -- Simplified - would check actual modification time
                pure $ StorageInfo
                    { _storageSize = size
                    , _storageFiles = files
                    , _storageLastModified = lastModified
                    , _storageGrowthRate = Nothing  -- Calculated separately
                    }
            else pure $ StorageInfo 0 0 0 Nothing
    case result of
        Left (_ :: SomeException) -> pure Nothing
        Right info -> pure (Just info)

-- | Background loop for storage monitoring
storageMonitoringLoop
    :: StorageConfig
    -> FilePath
    -> DatabaseMetricsInterface
    -> IORef [(POSIXTime, Word64)]
    -> IO ()
storageMonitoringLoop StorageConfig{..} dbPath metrics historyRef = forever $ do
    maybeInfo <- collectStorageInfo dbPath
    case maybeInfo of
        Just StorageInfo{..} -> do
            currentTime <- getPOSIXTime

            -- Update size metric
            updateDatabaseSize metrics _storageSize

            -- Track growth rate if enabled
            when _storageGrowthTracking $ do
                history <- readIORef historyRef
                let newHistory = take 10 $ (currentTime, _storageSize) : history
                writeIORef historyRef newHistory

                -- Calculate growth rate from history
                case calculateGrowthRate newHistory of
                    Just growthRate -> pure ()  -- Could record growth rate metric
                    Nothing -> pure ()

        Nothing -> pure ()

    -- Sleep for configured interval
    threadDelay (_storageMonitoringInterval * 1000000)

-- | Calculate storage growth rate from history
calculateGrowthRate :: [(POSIXTime, Word64)] -> Maybe Double
calculateGrowthRate history
    | length history < 2 = Nothing
    | otherwise = do
        let (newestTime, newestSize) = head history
        let (oldestTime, oldestSize) = last history
        let timeDiff = realToFrac (newestTime - oldestTime)
        let sizeDiff = fromIntegral (newestSize - oldestSize)
        if timeDiff > 0
            then Just (sizeDiff / timeDiff)
            else Nothing

-- | Get directory size recursively
getDirectorySize :: FilePath -> IO Word64
getDirectorySize path = do
    result <- try $ do
        exists <- doesDirectoryExist path
        if exists
            then do
                contents <- listDirectory path
                sizes <- mapM (getItemSize . (path </>)) contents
                pure (sum sizes)
            else pure 0
    case result of
        Left (_ :: SomeException) -> pure 0
        Right size -> pure size
  where
    getItemSize itemPath = do
        isDir <- doesDirectoryExist itemPath
        if isDir
            then getDirectorySize itemPath
            else fromIntegral <$> getFileSize itemPath

-- -------------------------------------------------------------------------- --
-- Per-Chain Storage Tracking

-- | Per-chain storage tracking state
data ChainStorageTracker = ChainStorageTracker
    { _chainStorageMap :: !(TVar (HashMap ChainId StorageInfo))
    , _chainStorageConfig :: !StorageConfig
    , _chainStorageMetrics :: !DatabaseMetricsInterface
    }

-- | Create a new chain storage tracker
createChainStorageTracker
    :: StorageConfig
    -> DatabaseMetricsInterface
    -> IO ChainStorageTracker
createChainStorageTracker config metrics = do
    storageMap <- newTVarIO HM.empty
    pure $ ChainStorageTracker
        { _chainStorageMap = storageMap
        , _chainStorageConfig = config
        , _chainStorageMetrics = metrics
        }

-- | Update storage info for a specific chain
updateChainStorage
    :: ChainStorageTracker
    -> ChainId
    -> StorageInfo
    -> IO ()
updateChainStorage ChainStorageTracker{..} chainId info = do
    atomically $ modifyTVar' _chainStorageMap (HM.insert chainId info)

    -- Update metrics for this chain
    updateDatabaseSize _chainStorageMetrics (_storageSize info)

-- | Get storage metrics for all chains
getChainStorageMetrics
    :: ChainStorageTracker
    -> IO (HashMap ChainId StorageInfo)
getChainStorageMetrics ChainStorageTracker{..} =
    readTVarIO _chainStorageMap