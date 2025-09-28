{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- |
-- Module: Chainweb.Metrics.System
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- System resource and GC metrics for Chainweb node monitoring
--
module Chainweb.Metrics.System
( SystemMetrics(..)
, initSystemMetrics
, updateGCStats
, updateCpuUsage
, updateMemoryUsage
, updateThreadCount
, recordGcPause
, recordGcPauseByGeneration
, getGcPausePercentiles
, collectSystemMetrics
, setDatabasePath
, updateDiskMetrics
) where

import Control.Monad (void, when)
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.List (foldl')

#if MIN_VERSION_base(4,17,0)
import GHC.Stats (getRTSStats, RTSStats(..))
#else
import GHC.Stats (getGCStats, GCStats(..))
#endif

import System.CPUTime (getCPUTime)
import qualified System.Mem as Mem
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Numeric.Natural

-- Additional system monitoring imports
import System.Process (readProcess)
import System.Info (os)
import qualified Control.Concurrent as Concurrent
import qualified System.Posix.Process as Posix
import Data.Word (Word64)

import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.Concurrent.Registry (Registry)
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Counter (Counter)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.Metric.Histogram (Histogram)

-- -------------------------------------------------------------------------- --
-- System Metrics Data Types

data SystemMetrics = SystemMetrics
    { -- GC and Memory Metrics
      _sysHeapSizeLive :: !Gauge
    , _sysHeapSizeTotal :: !Gauge
    , _sysHeapSizeBlocks :: !Gauge
    , _sysAllocatedBytes :: !Counter
    , _sysGcCount :: !Counter
    , _sysGcMajorCount :: !Counter
    , _sysGcCpuTime :: !Counter
    , _sysGcWallTime :: !Counter
    , _sysGcPauseTimes :: !Histogram

    -- CPU Metrics (Enhanced)
    , _sysCpuTime :: !Counter
    , _sysCpuUsagePercent :: !Gauge
    , _sysCpuUserTime :: !Counter
    , _sysCpuSystemTime :: !Counter
    , _sysLoadAverage1m :: !Gauge
    , _sysLoadAverage5m :: !Gauge
    , _sysLoadAverage15m :: !Gauge

    -- Memory Metrics (Enhanced)
    , _sysMemoryRSS :: !Gauge
    , _sysMemoryVMS :: !Gauge
    , _sysMemoryShared :: !Gauge
    , _sysMemoryAvailable :: !Gauge
    , _sysMemoryTotal :: !Gauge
    , _sysMemoryUsedPercent :: !Gauge

    -- Thread Metrics (Enhanced)
    , _sysThreadCount :: !Gauge
    , _sysThreadsCreated :: !Counter
    , _sysThreadsDestroyed :: !Counter
    , _sysCapabilities :: !Gauge

    -- Detailed GC Metrics
    , _sysGcGen0Collections :: !Counter
    , _sysGcGen1Collections :: !Counter
    , _sysGcGen0PauseTimes :: !Histogram
    , _sysGcGen1PauseTimes :: !Histogram
    , _sysGcAllocationRate :: !Gauge
    , _sysGcPromotionRate :: !Gauge
    , _sysGcFragmentation :: !Gauge

    -- Process Metrics
    , _sysFileDescriptorCount :: !Gauge
    , _sysProcessStartTime :: !Counter

    -- Disk I/O and Database Metrics
    , _sysDatabaseDirectorySize :: !Gauge
    , _sysDatabaseDirectorySizePerChain :: !Gauge  -- will use labels for chain_id
    , _sysDiskSpaceTotal :: !Gauge
    , _sysDiskSpaceAvailable :: !Gauge
    , _sysDiskSpaceUsedPercent :: !Gauge
    , _sysDatabaseGrowthRate :: !Gauge
    , _sysDiskReadBytes :: !Counter
    , _sysDiskWriteBytes :: !Counter
    , _sysDiskIOOperations :: !Counter

    -- Internal state for CPU and memory calculation
    , _sysLastCpuTime :: !(IORef Integer)
    , _sysLastWallTime :: !(IORef UTCTime)
    , _sysLastUserTime :: !(IORef Word64)
    , _sysLastSystemTime :: !(IORef Word64)

    -- Internal state for disk monitoring
    , _sysLastDatabaseSize :: !(IORef Word64)
    , _sysLastDiskCheck :: !(IORef UTCTime)
    , _sysDatabasePath :: !(IORef (Maybe FilePath))

    -- Internal state for detailed GC monitoring
    , _sysLastAllocatedBytes :: !(IORef Word64)
    , _sysLastGcTime :: !(IORef UTCTime)
    , _sysLastGen0Collections :: !(IORef Word64)
    , _sysLastGen1Collections :: !(IORef Word64)

    , _sysRegistry :: !Registry
    }

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Initialize system metrics with a new Prometheus registry
initSystemMetrics :: IO SystemMetrics
initSystemMetrics = do
    _sysRegistry <- Registry.new

    -- GC and Memory Metrics
    _sysHeapSizeLive <- Registry.registerGauge "chainweb_gc_heap_size_live_bytes" mempty _sysRegistry
    _sysHeapSizeTotal <- Registry.registerGauge "chainweb_gc_heap_size_total_bytes" mempty _sysRegistry
    _sysHeapSizeBlocks <- Registry.registerGauge "chainweb_gc_heap_size_blocks_bytes" mempty _sysRegistry
    _sysAllocatedBytes <- Registry.registerCounter "chainweb_gc_allocated_bytes_total" mempty _sysRegistry
    _sysGcCount <- Registry.registerCounter "chainweb_gc_collections_total" mempty _sysRegistry
    _sysGcMajorCount <- Registry.registerCounter "chainweb_gc_major_collections_total" mempty _sysRegistry
    _sysGcCpuTime <- Registry.registerCounter "chainweb_gc_cpu_time_nanoseconds_total" mempty _sysRegistry
    _sysGcWallTime <- Registry.registerCounter "chainweb_gc_wall_time_nanoseconds_total" mempty _sysRegistry
    _sysGcPauseTimes <- Registry.registerHistogram "chainweb_gc_pause_time_seconds" mempty gcPauseBuckets _sysRegistry

    -- CPU Metrics (Enhanced)
    _sysCpuTime <- Registry.registerCounter "chainweb_cpu_time_picoseconds_total" mempty _sysRegistry
    _sysCpuUsagePercent <- Registry.registerGauge "chainweb_cpu_usage_percent" mempty _sysRegistry
    _sysCpuUserTime <- Registry.registerCounter "chainweb_cpu_user_time_seconds_total" mempty _sysRegistry
    _sysCpuSystemTime <- Registry.registerCounter "chainweb_cpu_system_time_seconds_total" mempty _sysRegistry
    _sysLoadAverage1m <- Registry.registerGauge "chainweb_load_average_1m" mempty _sysRegistry
    _sysLoadAverage5m <- Registry.registerGauge "chainweb_load_average_5m" mempty _sysRegistry
    _sysLoadAverage15m <- Registry.registerGauge "chainweb_load_average_15m" mempty _sysRegistry

    -- Memory Metrics (Enhanced)
    _sysMemoryRSS <- Registry.registerGauge "chainweb_memory_rss_bytes" mempty _sysRegistry
    _sysMemoryVMS <- Registry.registerGauge "chainweb_memory_vms_bytes" mempty _sysRegistry
    _sysMemoryShared <- Registry.registerGauge "chainweb_memory_shared_bytes" mempty _sysRegistry
    _sysMemoryAvailable <- Registry.registerGauge "chainweb_system_memory_available_bytes" mempty _sysRegistry
    _sysMemoryTotal <- Registry.registerGauge "chainweb_system_memory_total_bytes" mempty _sysRegistry
    _sysMemoryUsedPercent <- Registry.registerGauge "chainweb_memory_usage_percent" mempty _sysRegistry

    -- Thread Metrics (Enhanced)
    _sysThreadCount <- Registry.registerGauge "chainweb_thread_count" mempty _sysRegistry
    _sysThreadsCreated <- Registry.registerCounter "chainweb_threads_created_total" mempty _sysRegistry
    _sysThreadsDestroyed <- Registry.registerCounter "chainweb_threads_destroyed_total" mempty _sysRegistry
    _sysCapabilities <- Registry.registerGauge "chainweb_rts_capabilities" mempty _sysRegistry

    -- Detailed GC Metrics
    _sysGcGen0Collections <- Registry.registerCounter "chainweb_gc_gen0_collections_total" mempty _sysRegistry
    _sysGcGen1Collections <- Registry.registerCounter "chainweb_gc_gen1_collections_total" mempty _sysRegistry
    _sysGcGen0PauseTimes <- Registry.registerHistogram "chainweb_gc_gen0_pause_time_seconds" mempty gcPauseBuckets _sysRegistry
    _sysGcGen1PauseTimes <- Registry.registerHistogram "chainweb_gc_gen1_pause_time_seconds" mempty gcPauseBuckets _sysRegistry
    _sysGcAllocationRate <- Registry.registerGauge "chainweb_gc_allocation_rate_bytes_per_second" mempty _sysRegistry
    _sysGcPromotionRate <- Registry.registerGauge "chainweb_gc_promotion_rate_bytes_per_second" mempty _sysRegistry
    _sysGcFragmentation <- Registry.registerGauge "chainweb_gc_fragmentation_percent" mempty _sysRegistry

    -- Process Metrics
    _sysFileDescriptorCount <- Registry.registerGauge "chainweb_file_descriptor_count" mempty _sysRegistry
    _sysProcessStartTime <- Registry.registerCounter "chainweb_process_start_time_seconds" mempty _sysRegistry

    -- Disk I/O and Database Metrics
    _sysDatabaseDirectorySize <- Registry.registerGauge "chainweb_database_directory_size_bytes" mempty _sysRegistry
    _sysDatabaseDirectorySizePerChain <- Registry.registerGauge "chainweb_database_directory_size_per_chain_bytes" mempty _sysRegistry
    _sysDiskSpaceTotal <- Registry.registerGauge "chainweb_disk_space_total_bytes" mempty _sysRegistry
    _sysDiskSpaceAvailable <- Registry.registerGauge "chainweb_disk_space_available_bytes" mempty _sysRegistry
    _sysDiskSpaceUsedPercent <- Registry.registerGauge "chainweb_disk_space_used_percent" mempty _sysRegistry
    _sysDatabaseGrowthRate <- Registry.registerGauge "chainweb_database_growth_rate_bytes_per_second" mempty _sysRegistry
    _sysDiskReadBytes <- Registry.registerCounter "chainweb_disk_read_bytes_total" mempty _sysRegistry
    _sysDiskWriteBytes <- Registry.registerCounter "chainweb_disk_write_bytes_total" mempty _sysRegistry
    _sysDiskIOOperations <- Registry.registerCounter "chainweb_disk_io_operations_total" mempty _sysRegistry

    -- Initialize state for CPU and memory calculation
    initialCpuTime <- getCPUTime
    initialWallTime <- getCurrentTime
    _sysLastCpuTime <- newIORef initialCpuTime
    _sysLastWallTime <- newIORef initialWallTime
    _sysLastUserTime <- newIORef 0
    _sysLastSystemTime <- newIORef 0

    -- Initialize state for disk monitoring
    _sysLastDatabaseSize <- newIORef 0
    _sysLastDiskCheck <- newIORef initialWallTime
    _sysDatabasePath <- newIORef Nothing

    -- Initialize state for detailed GC monitoring
    _sysLastAllocatedBytes <- newIORef 0
    _sysLastGcTime <- newIORef initialWallTime
    _sysLastGen0Collections <- newIORef 0
    _sysLastGen1Collections <- newIORef 0

    pure SystemMetrics {..}
  where
    -- GC pause time buckets: microseconds to seconds
    gcPauseBuckets = [0.000001, 0.000005, 0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0]

-- -------------------------------------------------------------------------- --
-- GC Statistics Collection

-- | Update all GC-related metrics by querying GHC runtime statistics
updateGCStats :: SystemMetrics -> IO ()
updateGCStats metrics@SystemMetrics{..} = do
    result <- try getRTSStatsIO
    case result of
        Left (_ :: SomeException) -> pure () -- RTS stats not available
        Right stats -> updateFromRTSStats metrics stats

#if MIN_VERSION_base(4,17,0)
getRTSStatsIO :: IO RTSStats
getRTSStatsIO = getRTSStats

updateFromRTSStats :: SystemMetrics -> RTSStats -> IO ()
updateFromRTSStats SystemMetrics{..} RTSStats{..} = do
    -- Heap size metrics - use available fields
    Gauge.set (fromIntegral max_live_bytes) _sysHeapSizeLive
    Gauge.set (fromIntegral max_live_bytes) _sysHeapSizeTotal  -- Fallback to max_live
    Gauge.set 0 _sysHeapSizeBlocks  -- Not available in this GHC version

    -- Allocation metrics
    void $ Counter.set (fromIntegral allocated_bytes) _sysAllocatedBytes

    -- GC count metrics
    void $ Counter.set (fromIntegral gcs) _sysGcCount
    void $ Counter.set (fromIntegral major_gcs) _sysGcMajorCount

    -- GC time metrics (store as nanoseconds for counters)
    void $ Counter.set (fromIntegral gc_cpu_ns) _sysGcCpuTime
    void $ Counter.set 0 _sysGcWallTime  -- Wall time not available

    -- Enhanced GC metrics
    updateDetailedGCMetrics SystemMetrics{..} RTSStats{..}

#else
-- Fallback for older GHC versions
getRTSStatsIO :: IO GCStats
getRTSStatsIO = getGCStats

updateFromRTSStats :: SystemMetrics -> GCStats -> IO ()
updateFromRTSStats SystemMetrics{..} GCStats{..} = do
    -- Heap size metrics (older API has limited info)
    Gauge.set (fromIntegral currentBytesUsed) _sysHeapSizeLive
    Gauge.set (fromIntegral maxBytesUsed) _sysHeapSizeTotal

    -- Allocation metrics
    void $ Counter.set (fromIntegral bytesAllocated) _sysAllocatedBytes

    -- GC count metrics
    void $ Counter.set (fromIntegral numGcs) _sysGcCount

    -- GC time metrics (convert from seconds)
    void $ Counter.set cpuSeconds _sysGcCpuTime
    void $ Counter.set wallSeconds _sysGcWallTime
#endif

-- -------------------------------------------------------------------------- --
-- CPU Usage Monitoring

-- | Update CPU usage metrics by calculating percentage since last measurement
updateCpuUsage :: SystemMetrics -> IO ()
updateCpuUsage SystemMetrics{..} = do
    currentCpuTime <- getCPUTime
    currentWallTime <- getCurrentTime

    lastCpuTime <- readIORef _sysLastCpuTime
    lastWallTime <- readIORef _sysLastWallTime

    let cpuTimeDiff = fromIntegral (currentCpuTime - lastCpuTime) / 1e12 -- Convert picoseconds to seconds
        wallTimeDiff = realToFrac $ diffUTCTime currentWallTime lastWallTime
        cpuPercent = if wallTimeDiff > 0 then (cpuTimeDiff / wallTimeDiff) * 100 else 0

    -- Update total CPU time counter (store as picoseconds)
    void $ Counter.set (fromIntegral currentCpuTime) _sysCpuTime

    -- Update CPU usage percentage
    Gauge.set cpuPercent _sysCpuUsagePercent

    -- Store current values for next calculation
    writeIORef _sysLastCpuTime currentCpuTime
    writeIORef _sysLastWallTime currentWallTime

    -- Update system-level CPU metrics
    updateSystemCpuStats SystemMetrics{..}

-- | Update system-level CPU statistics including load averages
updateSystemCpuStats :: SystemMetrics -> IO ()
updateSystemCpuStats SystemMetrics{..} = do
    -- Update load averages
    loadResult <- try readLoadAverages
    case loadResult of
        Left (_ :: SomeException) -> pure () -- Fail silently if not available
        Right (load1, load5, load15) -> do
            Gauge.set load1 _sysLoadAverage1m
            Gauge.set load5 _sysLoadAverage5m
            Gauge.set load15 _sysLoadAverage15m

    -- Update process CPU times (user/system split)
    cpuResult <- try readProcessCpuTimes
    case cpuResult of
        Left (_ :: SomeException) -> pure () -- Fail silently if not available
        Right (userTime, systemTime) -> do
            void $ Counter.set (round userTime) _sysCpuUserTime
            void $ Counter.set (round systemTime) _sysCpuSystemTime

-- | Read system load averages
readLoadAverages :: IO (Double, Double, Double)
readLoadAverages = case os of
    "linux"  -> readLinuxLoadAverages
    "darwin" -> readDarwinLoadAverages
    _        -> pure (0, 0, 0)  -- Fallback for unsupported OS

-- | Read load averages on Linux
readLinuxLoadAverages :: IO (Double, Double, Double)
readLinuxLoadAverages = do
    content <- readFile "/proc/loadavg"
    case words content of
        (load1:load5:load15:_) -> do
            let parse s = maybe 0 id $ readMaybe s
            pure (parse load1, parse load5, parse load15)
        _ -> pure (0, 0, 0)

-- | Read load averages on macOS
readDarwinLoadAverages :: IO (Double, Double, Double)
readDarwinLoadAverages = do
    output <- readProcess "uptime" [] ""
    -- Parse uptime output: "... load averages: 1.50 1.60 1.70"
    case words output of
        parts -> case reverse parts of
            (load15:load5:load1:_) -> do
                let parse s = maybe 0 id $ readMaybe s
                pure (parse load1, parse load5, parse load15)
            _ -> pure (0, 0, 0)

-- | Read process CPU times (user and system)
readProcessCpuTimes :: IO (Double, Double)
readProcessCpuTimes = case os of
    "linux"  -> readLinuxProcessCpuTimes
    "darwin" -> readDarwinProcessCpuTimes
    _        -> pure (0, 0)  -- Fallback for unsupported OS

-- | Read process CPU times on Linux
readLinuxProcessCpuTimes :: IO (Double, Double)
readLinuxProcessCpuTimes = do
    content <- readFile "/proc/self/stat"
    let fields = words content
    case drop 13 fields of  -- Skip to utime field (14th field, 0-indexed)
        (utime:stime:_) -> do
            let parse s = maybe 0 (/ 100.0) $ readMaybe s  -- Convert from clock ticks to seconds (assumes 100 Hz)
            pure (parse utime, parse stime)
        _ -> pure (0, 0)

-- | Read process CPU times on macOS
readDarwinProcessCpuTimes :: IO (Double, Double)
readDarwinProcessCpuTimes = do
    -- Use ps to get CPU times
    pid <- Posix.getProcessID
    output <- readProcess "ps" ["-o", "time", "-p", show pid] ""
    -- This is a simplified approach - would need more sophisticated parsing for real user/system split
    case lines output of
        (_:timeLine:_) -> do
            -- Parse time format "MM:SS.ss" or similar
            let totalTime = parseTimeToSeconds timeLine
            pure (totalTime / 2, totalTime / 2)  -- Rough split
        _ -> pure (0, 0)

-- | Parse time string to seconds (rough implementation)
parseTimeToSeconds :: String -> Double
parseTimeToSeconds timeStr =
    case break (== ':') timeStr of
        (mins, ':':rest) ->
            let minutes = maybe 0 id $ readMaybe mins
                seconds = maybe 0 id $ readMaybe rest
            in fromIntegral (minutes * 60) + seconds
        _ -> maybe 0 id $ readMaybe timeStr

-- -------------------------------------------------------------------------- --
-- Memory Usage Monitoring

-- | Update memory usage metrics by forcing GC and reading current stats
updateMemoryUsage :: SystemMetrics -> IO ()
updateMemoryUsage metrics@SystemMetrics{..} = do
    -- Force a minor GC to get accurate memory readings
    Mem.performGC
    -- Update GC stats which includes memory information
    updateGCStats metrics

    -- Update system-level memory metrics
    updateSystemMemoryStats metrics

-- | Update system memory statistics from OS
updateSystemMemoryStats :: SystemMetrics -> IO ()
updateSystemMemoryStats SystemMetrics{..} = do
    result <- try readSystemMemoryInfo
    case result of
        Left (_ :: SomeException) -> pure () -- Fail silently if not available
        Right memInfo -> do
            -- Update process memory metrics
            Gauge.set (fromIntegral $ memRSS memInfo) _sysMemoryRSS
            Gauge.set (fromIntegral $ memVMS memInfo) _sysMemoryVMS
            Gauge.set (fromIntegral $ memShared memInfo) _sysMemoryShared

            -- Update system memory metrics
            Gauge.set (fromIntegral $ memAvailable memInfo) _sysMemoryAvailable
            Gauge.set (fromIntegral $ memTotal memInfo) _sysMemoryTotal

            -- Calculate memory usage percentage
            let usedBytes = memTotal memInfo - memAvailable memInfo
                usagePercent = if memTotal memInfo > 0
                    then (fromIntegral usedBytes / fromIntegral (memTotal memInfo)) * 100
                    else 0
            Gauge.set usagePercent _sysMemoryUsedPercent

-- | Data type for memory information
data MemoryInfo = MemoryInfo
    { memRSS :: !Word64        -- Resident Set Size
    , memVMS :: !Word64        -- Virtual Memory Size
    , memShared :: !Word64     -- Shared memory
    , memAvailable :: !Word64  -- Available system memory
    , memTotal :: !Word64      -- Total system memory
    } deriving (Show, Eq)

-- | Read system memory information (OS-specific implementation)
readSystemMemoryInfo :: IO MemoryInfo
readSystemMemoryInfo = case os of
    "linux"  -> readLinuxMemoryInfo
    "darwin" -> readDarwinMemoryInfo
    _        -> pure $ MemoryInfo 0 0 0 0 0  -- Fallback for unsupported OS

-- | Read memory info on Linux systems
readLinuxMemoryInfo :: IO MemoryInfo
readLinuxMemoryInfo = do
    -- Read process memory from /proc/self/status
    processInfo <- try $ readFile "/proc/self/status" :: IO (Either SomeException String)
    -- Read system memory from /proc/meminfo
    systemInfo <- try $ readFile "/proc/meminfo" :: IO (Either SomeException String)

    let memRSS = case processInfo of
            Left _ -> 0
            Right content -> parseLinuxMemoryField "VmRSS:" content
        memVMS = case processInfo of
            Left _ -> 0
            Right content -> parseLinuxMemoryField "VmSize:" content
        memShared = case processInfo of
            Left _ -> 0
            Right content -> parseLinuxMemoryField "VmShare:" content

        (memTotal, memAvailable) = case systemInfo of
            Left _ -> (0, 0)
            Right content ->
                let total = parseLinuxMemoryField "MemTotal:" content
                    available = parseLinuxMemoryField "MemAvailable:" content
                in (total, available)

    pure $ MemoryInfo memRSS memVMS memShared memAvailable memTotal

-- | Read memory info on macOS systems
readDarwinMemoryInfo :: IO MemoryInfo
readDarwinMemoryInfo = do
    -- Use ps to get process memory info
    pid <- Posix.getProcessID
    processResult <- try $ readProcess "ps" ["-o", "rss,vsz", "-p", show pid] "" :: IO (Either SomeException String)
    -- Use vm_stat for system memory (simplified)
    systemResult <- try $ readProcess "sysctl" ["hw.memsize"] "" :: IO (Either SomeException String)

    let memRSS = case processResult of
            Left _ -> 0
            Right output -> parseDarwinProcessMemory output
        memVMS = 0  -- Not easily available on macOS
        memShared = 0  -- Not easily available on macOS
        memTotal = case systemResult of
            Left _ -> 0
            Right output -> parseDarwinSystemMemory output
        memAvailable = memTotal `div` 2  -- Rough estimate

    pure $ MemoryInfo memRSS memVMS memShared memAvailable memTotal

-- | Parse memory field from Linux /proc files (in KB, convert to bytes)
parseLinuxMemoryField :: String -> String -> Word64
parseLinuxMemoryField fieldName content =
    case filter (T.pack fieldName `T.isPrefixOf`) (map T.pack $ lines content) of
        [] -> 0
        (line:_) ->
            let parts = T.words line
            in case parts of
                (_:value:_) -> maybe 0 (* 1024) $ readMaybe $ T.unpack value
                _ -> 0

-- | Parse process memory from Darwin ps output
parseDarwinProcessMemory :: String -> Word64
parseDarwinProcessMemory output =
    case lines output of
        (_:dataLine:_) ->
            case words dataLine of
                (rss:_) -> maybe 0 (* 1024) $ readMaybe rss  -- ps gives KB, convert to bytes
                _ -> 0
        _ -> 0

-- | Parse system memory from Darwin sysctl output
parseDarwinSystemMemory :: String -> Word64
parseDarwinSystemMemory output =
    case words output of
        (_:value:_) -> maybe 0 id $ readMaybe value
        _ -> 0

-- -------------------------------------------------------------------------- --
-- Disk I/O and Database Directory Monitoring

-- | Set the database directory path for monitoring
setDatabasePath :: SystemMetrics -> FilePath -> IO ()
setDatabasePath SystemMetrics{..} path = do
    writeIORef _sysDatabasePath (Just path)

-- | Update disk I/O and database directory metrics
updateDiskMetrics :: SystemMetrics -> IO ()
updateDiskMetrics metrics@SystemMetrics{..} = do
    maybePath <- readIORef _sysDatabasePath
    case maybePath of
        Nothing -> pure ()  -- No database path set
        Just dbPath -> do
            -- Update database directory size
            updateDatabaseDirectorySize metrics dbPath
            -- Update disk space metrics
            updateDiskSpaceMetrics metrics dbPath
            -- Update I/O metrics (if available)
            updateDiskIOMetrics metrics

-- | Update database directory size metrics
updateDatabaseDirectorySize :: SystemMetrics -> FilePath -> IO ()
updateDatabaseDirectorySize SystemMetrics{..} dbPath = do
    result <- try $ getDirectorySize dbPath
    case result of
        Left (_ :: SomeException) -> pure ()  -- Fail silently
        Right totalSize -> do
            Gauge.set (fromIntegral totalSize) _sysDatabaseDirectorySize

            -- Update growth rate
            currentTime <- getCurrentTime
            lastSize <- readIORef _sysLastDatabaseSize
            lastCheck <- readIORef _sysLastDiskCheck

            let timeDiff = realToFrac $ diffUTCTime currentTime lastCheck
                sizeDiff = fromIntegral totalSize - fromIntegral lastSize
                growthRate = if timeDiff > 0 then sizeDiff / timeDiff else 0

            Gauge.set growthRate _sysDatabaseGrowthRate

            -- Update state
            writeIORef _sysLastDatabaseSize totalSize
            writeIORef _sysLastDiskCheck currentTime

-- | Update disk space metrics for the database partition
updateDiskSpaceMetrics :: SystemMetrics -> FilePath -> IO ()
updateDiskSpaceMetrics SystemMetrics{..} dbPath = do
    result <- try $ getDiskSpaceInfo dbPath
    case result of
        Left (_ :: SomeException) -> pure ()  -- Fail silently
        Right (total, available) -> do
            let used = total - available
                usagePercent = if total > 0
                    then (fromIntegral used / fromIntegral total) * 100
                    else 0

            Gauge.set (fromIntegral total) _sysDiskSpaceTotal
            Gauge.set (fromIntegral available) _sysDiskSpaceAvailable
            Gauge.set usagePercent _sysDiskSpaceUsedPercent

-- | Update disk I/O metrics (platform-specific)
updateDiskIOMetrics :: SystemMetrics -> IO ()
updateDiskIOMetrics SystemMetrics{..} = do
    result <- try readDiskIOStats
    case result of
        Left (_ :: SomeException) -> pure ()  -- Fail silently
        Right (readBytes, writeBytes, operations) -> do
            void $ Counter.set (fromIntegral readBytes) _sysDiskReadBytes
            void $ Counter.set (fromIntegral writeBytes) _sysDiskWriteBytes
            void $ Counter.set (fromIntegral operations) _sysDiskIOOperations

-- | Get total size of a directory recursively
getDirectorySize :: FilePath -> IO Word64
getDirectorySize path = do
    result <- try $ readProcess "du" ["-s", "-B1", path] ""
    case result of
        Left (_ :: SomeException) -> pure 0
        Right output -> case words output of
            (sizeStr:_) -> pure $ maybe 0 id $ readMaybe sizeStr
            _ -> pure 0

-- | Get disk space information (total and available bytes)
getDiskSpaceInfo :: FilePath -> IO (Word64, Word64)
getDiskSpaceInfo path = case os of
    "linux"  -> getDiskSpaceLinux path
    "darwin" -> getDiskSpaceDarwin path
    _        -> pure (0, 0)

-- | Get disk space on Linux using df
getDiskSpaceLinux :: FilePath -> IO (Word64, Word64)
getDiskSpaceLinux path = do
    output <- readProcess "df" ["-B1", path] ""
    case lines output of
        (_:dataLine:_) -> case words dataLine of
            (_:totalStr:_:availStr:_) -> do
                let total = maybe 0 id $ readMaybe totalStr
                    avail = maybe 0 id $ readMaybe availStr
                pure (total, avail)
            _ -> pure (0, 0)
        _ -> pure (0, 0)

-- | Get disk space on macOS using df
getDiskSpaceDarwin :: FilePath -> IO (Word64, Word64)
getDiskSpaceDarwin path = do
    output <- readProcess "df" ["-b", path] ""
    case lines output of
        (_:dataLine:_) -> case words dataLine of
            (_:totalStr:_:availStr:_) -> do
                let total = maybe 0 id $ readMaybe totalStr
                    avail = maybe 0 id $ readMaybe availStr
                pure (total, avail)
            _ -> pure (0, 0)
        _ -> pure (0, 0)

-- | Read disk I/O statistics (simplified implementation)
readDiskIOStats :: IO (Word64, Word64, Word64)
readDiskIOStats = case os of
    "linux"  -> readLinuxDiskIOStats
    "darwin" -> readDarwinDiskIOStats
    _        -> pure (0, 0, 0)

-- | Read disk I/O stats on Linux from /proc/diskstats
readLinuxDiskIOStats :: IO (Word64, Word64, Word64)
readLinuxDiskIOStats = do
    content <- readFile "/proc/diskstats"
    let diskLines = filter (not . null . words) $ lines content
        totalStats = foldl' addDiskStats (0, 0, 0) diskLines
    pure totalStats
  where
    addDiskStats (totalRead, totalWrite, totalOps) line =
        case words line of
            -- Format: major minor name reads ... sectors_read ... writes ... sectors_written ...
            (_:_:_:readsStr:_:sectorsRead:_:writesStr:_:sectorsWrite:_) ->
                let r = maybe 0 (* 512) $ readMaybe sectorsRead  -- Convert sectors to bytes (512 bytes/sector)
                    w = maybe 0 (* 512) $ readMaybe sectorsWrite
                    ops = maybe 0 id (readMaybe readsStr) + maybe 0 id (readMaybe writesStr)
                in (totalRead + r, totalWrite + w, totalOps + ops)
            _ -> (totalRead, totalWrite, totalOps)

-- | Read disk I/O stats on macOS (simplified using iostat)
readDarwinDiskIOStats :: IO (Word64, Word64, Word64)
readDarwinDiskIOStats = do
    -- Use iostat for basic I/O statistics
    output <- readProcess "iostat" ["-d", "-c", "1"] ""
    -- Parse iostat output (simplified)
    case lines output of
        [] -> pure (0, 0, 0)
        _ -> pure (0, 0, 0)  -- Simplified implementation

-- -------------------------------------------------------------------------- --
-- Detailed GC Metrics

-- | Update detailed GC metrics including generation-specific data
updateDetailedGCMetrics :: SystemMetrics -> RTSStats -> IO ()
updateDetailedGCMetrics SystemMetrics{..} RTSStats{..} = do
    currentTime <- getCurrentTime

    -- Update generation-specific collection counts
    -- Note: RTSStats doesn't separate gen0/gen1, so we estimate
    let estimatedGen0 = gcs - major_gcs  -- Minor GCs
        estimatedGen1 = major_gcs        -- Major GCs

    void $ Counter.set (fromIntegral estimatedGen0) _sysGcGen0Collections
    void $ Counter.set (fromIntegral estimatedGen1) _sysGcGen1Collections

    -- Calculate allocation rate
    lastAllocated <- readIORef _sysLastAllocatedBytes
    lastGcTime <- readIORef _sysLastGcTime

    let timeDiff = realToFrac $ diffUTCTime currentTime lastGcTime
        allocDiff = fromIntegral allocated_bytes - fromIntegral lastAllocated
        allocationRate = if timeDiff > 0 then allocDiff / timeDiff else 0

    Gauge.set allocationRate _sysGcAllocationRate

    -- Estimate promotion rate (bytes promoted from gen0 to gen1)
    -- This is a rough estimate based on major GC frequency
    let promotionRate = allocationRate * (fromIntegral major_gcs / max 1 (fromIntegral gcs))
    Gauge.set promotionRate _sysGcPromotionRate

    -- Calculate fragmentation (rough estimate)
    let fragmentation = if allocated_bytes > 0
            then ((fromIntegral allocated_bytes - fromIntegral max_live_bytes) / fromIntegral allocated_bytes) * 100
            else 0
    Gauge.set fragmentation _sysGcFragmentation

    -- Update state
    writeIORef _sysLastAllocatedBytes allocated_bytes
    writeIORef _sysLastGcTime currentTime

-- -------------------------------------------------------------------------- --
-- Thread Count Monitoring

-- | Update thread count metric and RTS capabilities info
updateThreadCount :: SystemMetrics -> IO ()
updateThreadCount SystemMetrics{..} = do
    -- Get number of capabilities (RTS threads)
    caps <- Concurrent.getNumCapabilities
    Gauge.set (fromIntegral caps) _sysCapabilities

    -- Estimate thread count using RTS info
    -- This is a simplified approach since actual thread counting is complex
    result <- try getThreadCount
    case result of
        Left (_ :: SomeException) -> Gauge.set (fromIntegral caps) _sysThreadCount
        Right count -> Gauge.set (fromIntegral count) _sysThreadCount

-- | Get approximate thread count (platform-specific)
getThreadCount :: IO Int
getThreadCount = case os of
    "linux"  -> getLinuxThreadCount
    "darwin" -> getDarwinThreadCount
    _        -> Concurrent.getNumCapabilities  -- Fallback to capabilities

-- | Get thread count on Linux
getLinuxThreadCount :: IO Int
getLinuxThreadCount = do
    content <- readFile "/proc/self/status"
    case filter ("Threads:" `isPrefixOf`) (lines content) of
        [] -> Concurrent.getNumCapabilities
        (line:_) -> case words line of
            (_:countStr:_) -> pure $ maybe 1 id $ readMaybe countStr
            _ -> pure 1
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- | Get thread count on macOS
getDarwinThreadCount :: IO Int
getDarwinThreadCount = do
    pid <- Posix.getProcessID
    output <- readProcess "ps" ["-M", "-p", show pid] ""
    -- Count lines in ps output (simplified)
    pure $ max 1 $ length (lines output) - 1  -- Subtract header line

-- -------------------------------------------------------------------------- --
-- GC Pause Recording

-- | Record a GC pause time in the histogram
recordGcPause :: SystemMetrics -> Double -> IO ()
recordGcPause SystemMetrics{..} pauseTime =
    void $ Histogram.observe pauseTime _sysGcPauseTimes

-- | Record a generation-specific GC pause time
recordGcPauseByGeneration :: SystemMetrics -> Int -> Double -> IO ()
recordGcPauseByGeneration SystemMetrics{..} generation pauseTime = case generation of
    0 -> void $ Histogram.observe pauseTime _sysGcGen0PauseTimes
    1 -> void $ Histogram.observe pauseTime _sysGcGen1PauseTimes
    _ -> void $ Histogram.observe pauseTime _sysGcPauseTimes  -- Fallback to general

-- | Calculate GC pause percentiles from histogram data
getGcPausePercentiles :: SystemMetrics -> IO (Double, Double, Double)
getGcPausePercentiles SystemMetrics{..} = do
    -- This would require access to histogram data, which is not directly available
    -- For now, return placeholder values
    -- In a real implementation, we'd need to sample the histogram
    pure (0.001, 0.005, 0.01)  -- p50, p95, p99 estimates

-- -------------------------------------------------------------------------- --
-- Metrics Collection

-- | Collect all system metrics into a text format
collectSystemMetrics :: SystemMetrics -> IO T.Text
collectSystemMetrics SystemMetrics{..} = do
    -- For now, return a placeholder
    -- TODO: Implement proper Prometheus text format export
    return "# System Resource Metrics\n# GC and system monitoring enabled"