{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.Configuration
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Chainweb.Configuration
(
-- * Throttling Configuration
  ThrottlingConfig(..)
, throttlingRate
, throttlingPeerRate
, throttlingMempoolRate
, defaultThrottlingConfig

-- * Cut Configuration
, ChainDatabaseGcConfig(..)
, chainDatabaseGcToText
, chainDatabaseGcFromText

, CutConfig(..)
, cutPruneChainDatabase
, cutFetchTimeout
, cutInitialBlockHeightLimit
, cutFastForwardBlockHeightLimit
, defaultCutConfig
, pCutConfig

-- * Service API Configuration
, ServiceApiConfig(..)
, serviceApiConfigPort
, serviceApiConfigInterface
, defaultServiceApiConfig
, pServiceApiConfig

-- * Backup configuration
, BackupApiConfig(..)
, configBackupApi
, BackupConfig(..)
, defaultBackupConfig

-- * Metrics Configuration
, MetricsConfig(..)
, BlockchainMetricsConfig(..)
, MempoolMetricsConfig(..)
, P2PMetricsConfig(..)
, DatabaseMetricsConfig(..)
, SystemMetricsConfig(..)
, defaultMetricsConfig
, defaultBlockchainMetricsConfig
, defaultMempoolMetricsConfig
, defaultP2PMetricsConfig
, defaultDatabaseMetricsConfig
, defaultSystemMetricsConfig
, pMetricsConfig
, validateMetricsConfig
, validateBlockchainMetricsConfig
, validateMempoolMetricsConfig
, validateP2PMetricsConfig
, validateDatabaseMetricsConfig
, validateSystemMetricsConfig

-- * Chainweb Configuration
, ChainwebConfiguration(..)
, configChainwebVersion
, configCuts
, configMining
, configHeaderStream
, configReintroTxs
, configP2p
, configBlockGasLimit
, configMinGasPrice
, configThrottling
, configReorgLimit
, configFullHistoricPactState
, configBackup
, configServiceApi
, configOnlySyncPact
, configSyncPactChains
, configEnableLocalTimeout
, configMetrics
, defaultChainwebConfiguration
, pChainwebConfiguration
, validateChainwebConfiguration

) where

import Configuration.Utils hiding (Error, Lens', disabled)

import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except
import Control.Monad.Writer

import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import qualified Data.Text as T

import GHC.Generics hiding (from)

import Network.Wai.Handler.Warp hiding (Port)

import Numeric.Natural (Natural)

import Data.Ratio (Ratio)

import qualified Pact.JSON.Encode as J

import Prelude hiding (log)

import System.Directory

-- internal modules

import Chainweb.BlockHeight
import Chainweb.Difficulty
import Chainweb.HostAddress
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Miner.Config
import Chainweb.Pact.Types (defaultReorgLimit, defaultModuleCacheLimit, defaultPreInsertCheckTimeout)
import Chainweb.Pact.Types (RewindLimit(..))
import Chainweb.Payload.RestAPI (PayloadBatchLimit(..), defaultServicePayloadBatchLimit)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Registry
import Chainweb.Time

import Data.Time.Clock (NominalDiffTime)

import P2P.Node.Configuration
import Chainweb.Pact.Backend.DbCache (DbCacheLimitBytes)

-- -------------------------------------------------------------------------- --
-- Throttling Configuration

data ThrottlingConfig = ThrottlingConfig
    { _throttlingRate :: !Double
    , _throttlingPeerRate :: !Double
        -- ^ This should throttle aggressively. This endpoint does an expensive
        -- check of the client. And we want to keep bad actors out of the
        -- system. There should be no need for a client to call this endpoint on
        -- the same node more often than at most few times peer minute.
    , _throttlingMempoolRate :: !Double
    }
    deriving stock (Eq, Show)

makeLenses ''ThrottlingConfig

defaultThrottlingConfig :: ThrottlingConfig
defaultThrottlingConfig = ThrottlingConfig
    { _throttlingRate = 50 -- per second, in a 100 burst
    , _throttlingPeerRate = 11 -- per second, 1 for each p2p network
    , _throttlingMempoolRate = 20 -- one every seconds per mempool.
    }

instance ToJSON ThrottlingConfig where
    toJSON o = object
        [ "global" .= _throttlingRate o
        , "putPeer" .= _throttlingPeerRate o
        , "mempool" .= _throttlingMempoolRate o
        ]

instance FromJSON (ThrottlingConfig -> ThrottlingConfig) where
    parseJSON = withObject "ThrottlingConfig" $ \o -> id
        <$< throttlingRate ..: "global" % o
        <*< throttlingPeerRate ..: "putPeer" % o
        <*< throttlingMempoolRate ..: "mempool" % o

-- -------------------------------------------------------------------------- --
-- Cut Configuration

data ChainDatabaseGcConfig
    = GcNone
    | GcHeaders
    | GcHeadersChecked
    | GcFull
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

chainDatabaseGcToText :: ChainDatabaseGcConfig -> T.Text
chainDatabaseGcToText GcNone = "none"
chainDatabaseGcToText GcHeaders = "headers"
chainDatabaseGcToText GcHeadersChecked = "headers-checked"
chainDatabaseGcToText GcFull = "full"

chainDatabaseGcFromText :: MonadThrow m => T.Text -> m ChainDatabaseGcConfig
chainDatabaseGcFromText t = case T.toCaseFold t of
    "none" -> return GcNone
    "headers" -> return GcHeaders
    "headers-checked" -> return GcHeadersChecked
    "full" -> return GcFull
    x -> throwM $ TextFormatException $ "unknown value for database pruning configuration: " <> sshow x

instance HasTextRepresentation ChainDatabaseGcConfig where
    toText = chainDatabaseGcToText
    fromText = chainDatabaseGcFromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance ToJSON ChainDatabaseGcConfig where
    toJSON = toJSON . chainDatabaseGcToText
    {-# INLINE toJSON #-}

instance FromJSON ChainDatabaseGcConfig where
    parseJSON v = parseJsonFromText "ChainDatabaseGcConfig" v <|> legacy v
      where
        legacy = withBool "ChainDatabaseGcConfig" $ \case
            True -> return GcHeaders
            False -> return GcNone
    {-# INLINE parseJSON #-}

data CutConfig = CutConfig
    { _cutPruneChainDatabase :: !ChainDatabaseGcConfig
    , _cutFetchTimeout :: !Int
    , _cutInitialBlockHeightLimit :: !(Maybe BlockHeight)
    , _cutFastForwardBlockHeightLimit :: !(Maybe BlockHeight)
    } deriving (Eq, Show)

makeLenses ''CutConfig

instance ToJSON CutConfig where
    toJSON o = object
        [ "pruneChainDatabase" .= _cutPruneChainDatabase o
        , "fetchTimeout" .= _cutFetchTimeout o
        , "initialBlockHeightLimit" .= _cutInitialBlockHeightLimit o
        , "fastForwardBlockHeightLimit" .= _cutFastForwardBlockHeightLimit o
        ]

instance FromJSON (CutConfig -> CutConfig) where
    parseJSON = withObject "CutConfig" $ \o -> id
        <$< cutPruneChainDatabase ..: "pruneChainDatabase" % o
        <*< cutFetchTimeout ..: "fetchTimeout" % o
        <*< cutInitialBlockHeightLimit ..: "initialBlockHeightLimit" % o
        <*< cutFastForwardBlockHeightLimit ..: "fastForwardBlockHeightLimit" % o

defaultCutConfig :: CutConfig
defaultCutConfig = CutConfig
    { _cutPruneChainDatabase = GcNone
    , _cutFetchTimeout = 3_000_000
    , _cutInitialBlockHeightLimit = Nothing
    , _cutFastForwardBlockHeightLimit = Nothing
    }

pCutConfig :: MParser CutConfig
pCutConfig = id
    <$< cutPruneChainDatabase .:: textOption
        % long "prune-chain-database"
        <> help
            ( "How to prune the chain database on startup."
            <> " This can take several hours."
            )
        <> metavar "none|headers|headers-checked|full"
    <*< cutFetchTimeout .:: option auto
        % long "cut-fetch-timeout"
        <> help "The timeout for processing new cuts in microseconds"
    <*< cutInitialBlockHeightLimit .:: fmap (Just . BlockHeight) . option auto
        % long "initial-block-height-limit"
        <> help "Reset initial cut to this block height."
        <> metavar "INT"
    <*< cutFastForwardBlockHeightLimit .:: fmap (Just . BlockHeight) . option auto
        % long "fast-forward-block-height-limit"
        <> help "When --only-sync-pact is given fast forward to this height. Ignored otherwise."
        <> metavar "INT"

-- -------------------------------------------------------------------------- --
-- Service API Configuration

data ServiceApiConfig = ServiceApiConfig
    { _serviceApiConfigPort :: !Port
        -- ^ The public host address for service APIs.
        -- A port number of 0 means that a free port is assigned by the system.
        --
        -- The default is 1917

    , _serviceApiConfigInterface :: !HostPreference
        -- ^ The network interface that the service APIs are bound to. Default is to
        -- bind to all available interfaces ('*').
    , _serviceApiConfigValidateSpec :: !Bool
        -- ^ Validate requests and responses against the latest OpenAPI specification.
        -- Disabled by default for performance reasons

    , _serviceApiPayloadBatchLimit :: PayloadBatchLimit
        -- ^ maximum size for payload batches on the service API. Default is
        -- 'Chainweb.Payload.RestAPI.defaultServicePayloadBatchLimit'.
    }
    deriving (Show, Eq, Generic)

makeLenses ''ServiceApiConfig

defaultServiceApiConfig :: ServiceApiConfig
defaultServiceApiConfig = ServiceApiConfig
    { _serviceApiConfigPort = 1848
    , _serviceApiConfigInterface = "*"
    , _serviceApiConfigValidateSpec = False
    , _serviceApiPayloadBatchLimit = defaultServicePayloadBatchLimit
    }

instance ToJSON ServiceApiConfig where
    toJSON o = object
        [ "port" .= _serviceApiConfigPort o
        , "interface" .= hostPreferenceToText (_serviceApiConfigInterface o)
        , "validateSpec" .= _serviceApiConfigValidateSpec o
        , "payloadBatchLimit" .= _serviceApiPayloadBatchLimit o
        ]

instance FromJSON (ServiceApiConfig -> ServiceApiConfig) where
    parseJSON = withObject "ServiceApiConfig" $ \o -> id
        <$< serviceApiConfigPort ..: "port" % o
        <*< setProperty serviceApiConfigInterface "interface" (parseJsonFromText "interface") o
        <*< serviceApiConfigValidateSpec ..: "validateSpec" % o
        <*< serviceApiPayloadBatchLimit ..: "payloadBatchLimit" % o

pServiceApiConfig :: MParser ServiceApiConfig
pServiceApiConfig = id
    <$< serviceApiConfigPort .:: pPort service
    <*< serviceApiConfigInterface .:: textOption
        % prefixLong service "interface"
        <> suffixHelp service "interface that the service Rest API binds to (see HostPreference documentation for details)"
    -- serviceApiBackups isn't supported on the command line
    <*< serviceApiPayloadBatchLimit .:: fmap PayloadBatchLimit . option auto
        % prefixLong service "payload-batch-limit"
        <> suffixHelp service "upper limit for the size of payload batches on the service API"
    <*< serviceApiConfigValidateSpec .:: enableDisableFlag
        % prefixLong service "validate-spec"
        <> internal -- hidden option, for expert use
  where
    service = Just "service"

-- -------------------------------------------------------------------------- --
-- Backup configuration

data BackupApiConfig = BackupApiConfig
    deriving (Show, Eq, Generic)

defaultBackupApiConfig :: BackupApiConfig
defaultBackupApiConfig = BackupApiConfig

data BackupConfig = BackupConfig
    { _configBackupApi :: !(EnableConfig BackupApiConfig)
    , _configBackupDirectory :: !(Maybe FilePath)
    -- ^ Should be a path in the same partition as the database directory to
    --   avoid the slow path of the rocksdb checkpoint mechanism.
    }
    deriving (Show, Eq, Generic)

defaultBackupConfig :: BackupConfig
defaultBackupConfig = BackupConfig
    { _configBackupApi = EnableConfig False defaultBackupApiConfig
    , _configBackupDirectory = Nothing
    }

makeLenses ''BackupApiConfig
makeLenses ''BackupConfig

instance ToJSON BackupApiConfig where
    toJSON _cfg = toJSON $ object [ ]

instance FromJSON (BackupApiConfig -> BackupApiConfig) where
    parseJSON = withObject "BackupApiConfig" $ \_ -> return id

pBackupApiConfig :: MParser BackupApiConfig
pBackupApiConfig = pure id

instance ToJSON BackupConfig where
    toJSON cfg = object
        [ "api" .= _configBackupApi cfg
        , "directory" .= _configBackupDirectory cfg
        ]

instance FromJSON (BackupConfig -> BackupConfig) where
    parseJSON = withObject "BackupConfig" $ \o -> id
        <$< configBackupApi %.: "api" % o
        <*< configBackupDirectory ..: "directory" % o

pBackupConfig :: MParser BackupConfig
pBackupConfig = id
    <$< configBackupApi %:: pEnableConfig "backup-api" pBackupApiConfig
    <*< configBackupDirectory .:: fmap Just % textOption
        % prefixLong backup "directory"
        <> suffixHelp backup "Directory in which backups will be placed when using the backup API endpoint"
  where
    backup = Just "backup"

-- -------------------------------------------------------------------------- --
-- Metrics Configuration

-- | Blockchain metrics configuration
data BlockchainMetricsConfig = BlockchainMetricsConfig
    { _blockchainMetricsEnabled :: !Bool
    , _blockchainMetricsCollectionInterval :: !Natural  -- seconds
    , _blockchainMetricsSamplingRate :: !Double  -- 0.0 to 1.0
    , _blockchainMetricsRetentionPeriod :: !NominalDiffTime  -- seconds
    , _blockchainMetricsMaxHistogramBuckets :: !Natural
    } deriving (Show, Eq, Generic)

-- | Mempool metrics configuration
data MempoolMetricsConfig = MempoolMetricsConfig
    { _mempoolMetricsEnabled :: !Bool
    , _mempoolMetricsCollectionInterval :: !Natural  -- seconds
    , _mempoolMetricsSamplingRate :: !Double  -- 0.0 to 1.0
    , _mempoolMetricsRetentionPeriod :: !NominalDiffTime  -- seconds
    , _mempoolMetricsValidationTrackingEnabled :: !Bool
    } deriving (Show, Eq, Generic)

-- | P2P metrics configuration
data P2PMetricsConfig = P2PMetricsConfig
    { _p2pMetricsEnabled :: !Bool
    , _p2pMetricsCollectionInterval :: !Natural  -- seconds
    , _p2pMetricsSamplingRate :: !Double  -- 0.0 to 1.0
    , _p2pMetricsRetentionPeriod :: !NominalDiffTime  -- seconds
    , _p2pMetricsConnectionTrackingEnabled :: !Bool
    , _p2pMetricsGeographicTrackingEnabled :: !Bool
    } deriving (Show, Eq, Generic)

-- | Database metrics configuration
data DatabaseMetricsConfig = DatabaseMetricsConfig
    { _databaseMetricsEnabled :: !Bool
    , _databaseMetricsCollectionInterval :: !Natural  -- seconds
    , _databaseMetricsSamplingRate :: !Double  -- 0.0 to 1.0
    , _databaseMetricsRetentionPeriod :: !NominalDiffTime  -- seconds
    , _databaseMetricsCompactionTrackingEnabled :: !Bool
    , _databaseMetricsCacheStatsEnabled :: !Bool
    } deriving (Show, Eq, Generic)

-- | System metrics configuration
data SystemMetricsConfig = SystemMetricsConfig
    { _systemMetricsEnabled :: !Bool
    , _systemMetricsCollectionInterval :: !Natural  -- seconds
    , _systemMetricsSamplingRate :: !Double  -- 0.0 to 1.0
    , _systemMetricsRetentionPeriod :: !NominalDiffTime  -- seconds
    , _systemMetricsGcStatsEnabled :: !Bool
    , _systemMetricsResourceUsageEnabled :: !Bool
    } deriving (Show, Eq, Generic)

-- | Comprehensive metrics configuration
data MetricsConfig = MetricsConfig
    { _metricsEnabled :: !Bool  -- Global enable/disable flag
    , _metricsPort :: !(Maybe Port)  -- Optional port for metrics endpoint
    , _metricsInterface :: !HostPreference  -- Interface to bind metrics endpoint
    , _metricsMaxMemoryMB :: !Natural  -- Maximum memory usage for metrics
    , _metricsBlockchain :: !BlockchainMetricsConfig
    , _metricsMempool :: !MempoolMetricsConfig
    , _metricsP2P :: !P2PMetricsConfig
    , _metricsDatabase :: !DatabaseMetricsConfig
    , _metricsSystem :: !SystemMetricsConfig
    } deriving (Show, Eq, Generic)

makeLenses ''BlockchainMetricsConfig
makeLenses ''MempoolMetricsConfig
makeLenses ''P2PMetricsConfig
makeLenses ''DatabaseMetricsConfig
makeLenses ''SystemMetricsConfig
makeLenses ''MetricsConfig

-- | Default blockchain metrics configuration
defaultBlockchainMetricsConfig :: BlockchainMetricsConfig
defaultBlockchainMetricsConfig = BlockchainMetricsConfig
    { _blockchainMetricsEnabled = False  -- Disabled by default for performance
    , _blockchainMetricsCollectionInterval = 30  -- 30 seconds
    , _blockchainMetricsSamplingRate = 1.0  -- 100% sampling
    , _blockchainMetricsRetentionPeriod = 3600  -- 1 hour
    , _blockchainMetricsMaxHistogramBuckets = 50
    }

-- | Default mempool metrics configuration
defaultMempoolMetricsConfig :: MempoolMetricsConfig
defaultMempoolMetricsConfig = MempoolMetricsConfig
    { _mempoolMetricsEnabled = False  -- Disabled by default for performance
    , _mempoolMetricsCollectionInterval = 10  -- 10 seconds
    , _mempoolMetricsSamplingRate = 1.0  -- 100% sampling
    , _mempoolMetricsRetentionPeriod = 1800  -- 30 minutes
    , _mempoolMetricsValidationTrackingEnabled = True
    }

-- | Default P2P metrics configuration
defaultP2PMetricsConfig :: P2PMetricsConfig
defaultP2PMetricsConfig = P2PMetricsConfig
    { _p2pMetricsEnabled = False  -- Disabled by default for performance
    , _p2pMetricsCollectionInterval = 15  -- 15 seconds
    , _p2pMetricsSamplingRate = 1.0  -- 100% sampling
    , _p2pMetricsRetentionPeriod = 3600  -- 1 hour
    , _p2pMetricsConnectionTrackingEnabled = True
    , _p2pMetricsGeographicTrackingEnabled = False  -- Can be expensive
    }

-- | Default database metrics configuration
defaultDatabaseMetricsConfig :: DatabaseMetricsConfig
defaultDatabaseMetricsConfig = DatabaseMetricsConfig
    { _databaseMetricsEnabled = False  -- Disabled by default for performance
    , _databaseMetricsCollectionInterval = 60  -- 1 minute
    , _databaseMetricsSamplingRate = 0.1  -- 10% sampling for expensive ops
    , _databaseMetricsRetentionPeriod = 7200  -- 2 hours
    , _databaseMetricsCompactionTrackingEnabled = True
    , _databaseMetricsCacheStatsEnabled = True
    }

-- | Default system metrics configuration
defaultSystemMetricsConfig :: SystemMetricsConfig
defaultSystemMetricsConfig = SystemMetricsConfig
    { _systemMetricsEnabled = False  -- Disabled by default for performance
    , _systemMetricsCollectionInterval = 30  -- 30 seconds
    , _systemMetricsSamplingRate = 1.0  -- 100% sampling
    , _systemMetricsRetentionPeriod = 3600  -- 1 hour
    , _systemMetricsGcStatsEnabled = True
    , _systemMetricsResourceUsageEnabled = True
    }

-- | Default metrics configuration
defaultMetricsConfig :: MetricsConfig
defaultMetricsConfig = MetricsConfig
    { _metricsEnabled = False  -- Disabled by default for performance
    , _metricsPort = Nothing  -- Use same port as service API by default
    , _metricsInterface = "*"  -- Bind to all interfaces
    , _metricsMaxMemoryMB = 128  -- 128 MB limit
    , _metricsBlockchain = defaultBlockchainMetricsConfig
    , _metricsMempool = defaultMempoolMetricsConfig
    , _metricsP2P = defaultP2PMetricsConfig
    , _metricsDatabase = defaultDatabaseMetricsConfig
    , _metricsSystem = defaultSystemMetricsConfig
    }

-- ToJSON instances for metrics configuration
instance ToJSON BlockchainMetricsConfig where
    toJSON o = object
        [ "enabled" .= _blockchainMetricsEnabled o
        , "collectionInterval" .= _blockchainMetricsCollectionInterval o
        , "samplingRate" .= _blockchainMetricsSamplingRate o
        , "retentionPeriod" .= (realToFrac (_blockchainMetricsRetentionPeriod o) :: Double)
        , "maxHistogramBuckets" .= _blockchainMetricsMaxHistogramBuckets o
        ]

instance ToJSON MempoolMetricsConfig where
    toJSON o = object
        [ "enabled" .= _mempoolMetricsEnabled o
        , "collectionInterval" .= _mempoolMetricsCollectionInterval o
        , "samplingRate" .= _mempoolMetricsSamplingRate o
        , "retentionPeriod" .= (realToFrac (_mempoolMetricsRetentionPeriod o) :: Double)
        , "validationTrackingEnabled" .= _mempoolMetricsValidationTrackingEnabled o
        ]

instance ToJSON P2PMetricsConfig where
    toJSON o = object
        [ "enabled" .= _p2pMetricsEnabled o
        , "collectionInterval" .= _p2pMetricsCollectionInterval o
        , "samplingRate" .= _p2pMetricsSamplingRate o
        , "retentionPeriod" .= (realToFrac (_p2pMetricsRetentionPeriod o) :: Double)
        , "connectionTrackingEnabled" .= _p2pMetricsConnectionTrackingEnabled o
        , "geographicTrackingEnabled" .= _p2pMetricsGeographicTrackingEnabled o
        ]

instance ToJSON DatabaseMetricsConfig where
    toJSON o = object
        [ "enabled" .= _databaseMetricsEnabled o
        , "collectionInterval" .= _databaseMetricsCollectionInterval o
        , "samplingRate" .= _databaseMetricsSamplingRate o
        , "retentionPeriod" .= (realToFrac (_databaseMetricsRetentionPeriod o) :: Double)
        , "compactionTrackingEnabled" .= _databaseMetricsCompactionTrackingEnabled o
        , "cacheStatsEnabled" .= _databaseMetricsCacheStatsEnabled o
        ]

instance ToJSON SystemMetricsConfig where
    toJSON o = object
        [ "enabled" .= _systemMetricsEnabled o
        , "collectionInterval" .= _systemMetricsCollectionInterval o
        , "samplingRate" .= _systemMetricsSamplingRate o
        , "retentionPeriod" .= (realToFrac (_systemMetricsRetentionPeriod o) :: Double)
        , "gcStatsEnabled" .= _systemMetricsGcStatsEnabled o
        , "resourceUsageEnabled" .= _systemMetricsResourceUsageEnabled o
        ]

instance ToJSON MetricsConfig where
    toJSON o = object
        [ "enabled" .= _metricsEnabled o
        , "port" .= _metricsPort o
        , "interface" .= hostPreferenceToText (_metricsInterface o)
        , "maxMemoryMB" .= _metricsMaxMemoryMB o
        , "blockchain" .= _metricsBlockchain o
        , "mempool" .= _metricsMempool o
        , "p2p" .= _metricsP2P o
        , "database" .= _metricsDatabase o
        , "system" .= _metricsSystem o
        ]

-- FromJSON instances for metrics configuration
instance FromJSON (BlockchainMetricsConfig -> BlockchainMetricsConfig) where
    parseJSON = withObject "BlockchainMetricsConfig" $ \o -> id
        <$< blockchainMetricsEnabled ..: "enabled" % o
        <*< blockchainMetricsCollectionInterval ..: "collectionInterval" % o
        <*< blockchainMetricsSamplingRate ..: "samplingRate" % o
        <*< setProperty blockchainMetricsRetentionPeriod "retentionPeriod"
            (fmap realToFrac . parseJSON) o
        <*< blockchainMetricsMaxHistogramBuckets ..: "maxHistogramBuckets" % o

instance FromJSON (MempoolMetricsConfig -> MempoolMetricsConfig) where
    parseJSON = withObject "MempoolMetricsConfig" $ \o -> id
        <$< mempoolMetricsEnabled ..: "enabled" % o
        <*< mempoolMetricsCollectionInterval ..: "collectionInterval" % o
        <*< mempoolMetricsSamplingRate ..: "samplingRate" % o
        <*< setProperty mempoolMetricsRetentionPeriod "retentionPeriod"
            (fmap realToFrac . parseJSON) o
        <*< mempoolMetricsValidationTrackingEnabled ..: "validationTrackingEnabled" % o

instance FromJSON (P2PMetricsConfig -> P2PMetricsConfig) where
    parseJSON = withObject "P2PMetricsConfig" $ \o -> id
        <$< p2pMetricsEnabled ..: "enabled" % o
        <*< p2pMetricsCollectionInterval ..: "collectionInterval" % o
        <*< p2pMetricsSamplingRate ..: "samplingRate" % o
        <*< setProperty p2pMetricsRetentionPeriod "retentionPeriod"
            (fmap realToFrac . parseJSON) o
        <*< p2pMetricsConnectionTrackingEnabled ..: "connectionTrackingEnabled" % o
        <*< p2pMetricsGeographicTrackingEnabled ..: "geographicTrackingEnabled" % o

instance FromJSON (DatabaseMetricsConfig -> DatabaseMetricsConfig) where
    parseJSON = withObject "DatabaseMetricsConfig" $ \o -> id
        <$< databaseMetricsEnabled ..: "enabled" % o
        <*< databaseMetricsCollectionInterval ..: "collectionInterval" % o
        <*< databaseMetricsSamplingRate ..: "samplingRate" % o
        <*< setProperty databaseMetricsRetentionPeriod "retentionPeriod"
            (fmap realToFrac . parseJSON) o
        <*< databaseMetricsCompactionTrackingEnabled ..: "compactionTrackingEnabled" % o
        <*< databaseMetricsCacheStatsEnabled ..: "cacheStatsEnabled" % o

instance FromJSON (SystemMetricsConfig -> SystemMetricsConfig) where
    parseJSON = withObject "SystemMetricsConfig" $ \o -> id
        <$< systemMetricsEnabled ..: "enabled" % o
        <*< systemMetricsCollectionInterval ..: "collectionInterval" % o
        <*< systemMetricsSamplingRate ..: "samplingRate" % o
        <*< setProperty systemMetricsRetentionPeriod "retentionPeriod"
            (fmap realToFrac . parseJSON) o
        <*< systemMetricsGcStatsEnabled ..: "gcStatsEnabled" % o
        <*< systemMetricsResourceUsageEnabled ..: "resourceUsageEnabled" % o

instance FromJSON (MetricsConfig -> MetricsConfig) where
    parseJSON = withObject "MetricsConfig" $ \o -> id
        <$< metricsEnabled ..: "enabled" % o
        <*< metricsPort ..: "port" % o
        <*< setProperty metricsInterface "interface" (parseJsonFromText "interface") o
        <*< metricsMaxMemoryMB ..: "maxMemoryMB" % o
        <*< metricsBlockchain %.: "blockchain" % o
        <*< metricsMempool %.: "mempool" % o
        <*< metricsP2P %.: "p2p" % o
        <*< metricsDatabase %.: "database" % o
        <*< metricsSystem %.: "system" % o

-- Parser for metrics configuration
pMetricsConfig :: MParser MetricsConfig
pMetricsConfig = id
    <$< metricsEnabled .:: enableDisableFlag
        % long "metrics-enabled"
        <> help "Enable metrics collection and endpoint"
    <*< metricsPort .:: fmap Just . pPort (Just "metrics")
    <*< metricsInterface .:: textOption
        % long "metrics-interface"
        <> help "Interface for metrics endpoint (default: same as service API)"
        <> value "*"
    <*< metricsMaxMemoryMB .:: option auto
        % long "metrics-max-memory-mb"
        <> help "Maximum memory usage for metrics in MB"
        <> value 128
    <*< metricsBlockchain %:: pBlockchainMetricsConfig
    <*< metricsMempool %:: pMempoolMetricsConfig
    <*< metricsP2P %:: pP2PMetricsConfig
    <*< metricsDatabase %:: pDatabaseMetricsConfig
    <*< metricsSystem %:: pSystemMetricsConfig

pBlockchainMetricsConfig :: MParser BlockchainMetricsConfig
pBlockchainMetricsConfig = id
    <$< blockchainMetricsEnabled .:: enableDisableFlag
        % long "blockchain-metrics-enabled"
        <> help "Enable blockchain metrics collection"
    <*< blockchainMetricsCollectionInterval .:: option auto
        % long "blockchain-metrics-interval"
        <> help "Blockchain metrics collection interval in seconds"
        <> value 30
    <*< blockchainMetricsSamplingRate .:: option auto
        % long "blockchain-metrics-sampling-rate"
        <> help "Blockchain metrics sampling rate (0.0-1.0)"
        <> value 1.0
    <*< blockchainMetricsRetentionPeriod .:: fmap realToFrac . option auto
        % long "blockchain-metrics-retention"
        <> help "Blockchain metrics retention period in seconds"
        <> value 3600
    <*< blockchainMetricsMaxHistogramBuckets .:: option auto
        % long "blockchain-metrics-max-buckets"
        <> help "Maximum histogram buckets for blockchain metrics"
        <> value 50

pMempoolMetricsConfig :: MParser MempoolMetricsConfig
pMempoolMetricsConfig = id
    <$< mempoolMetricsEnabled .:: enableDisableFlag
        % long "mempool-metrics-enabled"
        <> help "Enable mempool metrics collection"
    <*< mempoolMetricsCollectionInterval .:: option auto
        % long "mempool-metrics-interval"
        <> help "Mempool metrics collection interval in seconds"
        <> value 10
    <*< mempoolMetricsSamplingRate .:: option auto
        % long "mempool-metrics-sampling-rate"
        <> help "Mempool metrics sampling rate (0.0-1.0)"
        <> value 1.0
    <*< mempoolMetricsRetentionPeriod .:: fmap realToFrac . option auto
        % long "mempool-metrics-retention"
        <> help "Mempool metrics retention period in seconds"
        <> value 1800
    <*< mempoolMetricsValidationTrackingEnabled .:: boolOption_
        % long "mempool-metrics-validation-tracking"
        <> help "Enable mempool validation tracking"

pP2PMetricsConfig :: MParser P2PMetricsConfig
pP2PMetricsConfig = id
    <$< p2pMetricsEnabled .:: enableDisableFlag
        % long "p2p-metrics-enabled"
        <> help "Enable P2P metrics collection"
    <*< p2pMetricsCollectionInterval .:: option auto
        % long "p2p-metrics-interval"
        <> help "P2P metrics collection interval in seconds"
        <> value 15
    <*< p2pMetricsSamplingRate .:: option auto
        % long "p2p-metrics-sampling-rate"
        <> help "P2P metrics sampling rate (0.0-1.0)"
        <> value 1.0
    <*< p2pMetricsRetentionPeriod .:: fmap realToFrac . option auto
        % long "p2p-metrics-retention"
        <> help "P2P metrics retention period in seconds"
        <> value 3600
    <*< p2pMetricsConnectionTrackingEnabled .:: boolOption_
        % long "p2p-metrics-connection-tracking"
        <> help "Enable P2P connection tracking"
    <*< p2pMetricsGeographicTrackingEnabled .:: boolOption_
        % long "p2p-metrics-geographic-tracking"
        <> help "Enable P2P geographic tracking"

pDatabaseMetricsConfig :: MParser DatabaseMetricsConfig
pDatabaseMetricsConfig = id
    <$< databaseMetricsEnabled .:: enableDisableFlag
        % long "database-metrics-enabled"
        <> help "Enable database metrics collection"
    <*< databaseMetricsCollectionInterval .:: option auto
        % long "database-metrics-interval"
        <> help "Database metrics collection interval in seconds"
        <> value 60
    <*< databaseMetricsSamplingRate .:: option auto
        % long "database-metrics-sampling-rate"
        <> help "Database metrics sampling rate (0.0-1.0)"
        <> value 0.1
    <*< databaseMetricsRetentionPeriod .:: fmap realToFrac . option auto
        % long "database-metrics-retention"
        <> help "Database metrics retention period in seconds"
        <> value 7200
    <*< databaseMetricsCompactionTrackingEnabled .:: boolOption_
        % long "database-metrics-compaction-tracking"
        <> help "Enable database compaction tracking"
    <*< databaseMetricsCacheStatsEnabled .:: boolOption_
        % long "database-metrics-cache-stats"
        <> help "Enable database cache statistics"

pSystemMetricsConfig :: MParser SystemMetricsConfig
pSystemMetricsConfig = id
    <$< systemMetricsEnabled .:: enableDisableFlag
        % long "system-metrics-enabled"
        <> help "Enable system metrics collection"
    <*< systemMetricsCollectionInterval .:: option auto
        % long "system-metrics-interval"
        <> help "System metrics collection interval in seconds"
        <> value 30
    <*< systemMetricsSamplingRate .:: option auto
        % long "system-metrics-sampling-rate"
        <> help "System metrics sampling rate (0.0-1.0)"
        <> value 1.0
    <*< systemMetricsRetentionPeriod .:: fmap realToFrac . option auto
        % long "system-metrics-retention"
        <> help "System metrics retention period in seconds"
        <> value 3600
    <*< systemMetricsGcStatsEnabled .:: boolOption_
        % long "system-metrics-gc-stats"
        <> help "Enable system GC statistics"
    <*< systemMetricsResourceUsageEnabled .:: boolOption_
        % long "system-metrics-resource-usage"
        <> help "Enable system resource usage tracking"

-- -------------------------------------------------------------------------- --
-- Chainweb Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configCuts :: !CutConfig
    , _configMining :: !MiningConfig
    , _configHeaderStream :: !Bool
    , _configReintroTxs :: !Bool
    , _configP2p :: !P2pConfiguration
    , _configThrottling :: !ThrottlingConfig
    , _configMempoolP2p :: !(EnableConfig MempoolP2pConfig)
    , _configBlockGasLimit :: !Mempool.GasLimit
    , _configLogGas :: !Bool
    , _configMinGasPrice :: !Mempool.GasPrice
    , _configPactQueueSize :: !Natural
    , _configReorgLimit :: !RewindLimit
    , _configPreInsertCheckTimeout :: !Micros
    , _configAllowReadsInLocal :: !Bool
    , _configFullHistoricPactState :: !Bool
    , _configBackup :: !BackupConfig
    , _configServiceApi :: !ServiceApiConfig
    , _configReadOnlyReplay :: !Bool
        -- ^ do a read-only replay using the cut db params for the block heights
    , _configOnlySyncPact :: !Bool
        -- ^ exit after synchronizing pact dbs to the latest cut
    , _configSyncPactChains :: !(Maybe [ChainId])
        -- ^ the only chains to be synchronized on startup to the latest cut.
        --   if unset, all chains will be synchronized.
    , _configModuleCacheLimit :: !DbCacheLimitBytes
        -- ^ module cache size limit in bytes
    , _configEnableLocalTimeout :: !Bool
    , _configMetrics :: !MetricsConfig
        -- ^ metrics collection configuration
    } deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

instance HasChainwebVersion ChainwebConfiguration where
    _chainwebVersion = _configChainwebVersion
    {-# INLINE _chainwebVersion #-}

validateChainwebConfiguration :: ConfigValidation ChainwebConfiguration []
validateChainwebConfiguration c = do
    validateMinerConfig (_configChainwebVersion c) (_configMining c)
    validateBackupConfig (_configBackup c)
    unless (c ^. chainwebVersion . versionDefaults . disablePeerValidation) $
        validateP2pConfiguration (_configP2p c)
    validateChainwebVersion (_configChainwebVersion c)
    validateMetricsConfig (_configMetrics c)

validateChainwebVersion :: ConfigValidation ChainwebVersion []
validateChainwebVersion v = do
    unless (isDevelopment || elem v knownVersions) $
        throwError $ T.unwords
            [ "Specifying version properties is only legal with chainweb-version"
            , "set to recap-development or development, but version is set to"
            , sshow (_versionName v)
            ]
    where
    isDevelopment = _versionCode v `elem` [_versionCode dv | dv <- [recapDevnet, devnet]]

validateBackupConfig :: ConfigValidation BackupConfig []
validateBackupConfig c =
    for_ (_configBackupDirectory c) $ \dir -> do
        liftIO $ createDirectoryIfMissing True dir
        perms <- liftIO (getPermissions dir)
        unless (writable perms) $
            throwError $ "Backup directory " <> T.pack dir <> " is not writable"

-- | Validate metrics configuration for performance and safety
validateMetricsConfig :: ConfigValidation MetricsConfig []
validateMetricsConfig config = do
    -- Only validate if metrics are enabled
    when (_metricsEnabled config) $ do
        -- Validate memory limits
        when (_metricsMaxMemoryMB config < 10) $
            throwError "Metrics maximum memory must be at least 10 MB"

        -- Validate each subsystem configuration
        validateBlockchainMetricsConfig (_metricsBlockchain config)
        validateMempoolMetricsConfig (_metricsMempool config)
        validateP2PMetricsConfig (_metricsP2P config)
        validateDatabaseMetricsConfig (_metricsDatabase config)
        validateSystemMetricsConfig (_metricsSystem config)

-- | Validate blockchain metrics configuration
validateBlockchainMetricsConfig :: ConfigValidation BlockchainMetricsConfig []
validateBlockchainMetricsConfig config =
    when (_blockchainMetricsEnabled config) $ do
        -- Validate sampling rate
        unless (_blockchainMetricsSamplingRate config > 0.0 && _blockchainMetricsSamplingRate config <= 1.0) $
            throwError "Blockchain metrics sampling rate must be between 0.0 and 1.0"

        -- Validate collection interval
        when (_blockchainMetricsCollectionInterval config < 5) $
            throwError "Blockchain metrics collection interval must be at least 5 seconds"

        -- Validate retention period
        when (_blockchainMetricsRetentionPeriod config < 60) $
            throwError "Blockchain metrics retention period must be at least 60 seconds"

        -- Validate histogram buckets
        when (_blockchainMetricsMaxHistogramBuckets config < 1 || _blockchainMetricsMaxHistogramBuckets config > 1000) $
            throwError "Blockchain metrics histogram buckets must be between 1 and 1000"

        -- Performance warnings
        when (_blockchainMetricsCollectionInterval config < 10) $
            tell ["Warning: Blockchain metrics collection interval under 10 seconds may impact performance"]
        when (_blockchainMetricsSamplingRate config > 0.8) $
            tell ["Warning: High blockchain metrics sampling rate may impact performance"]

-- | Validate mempool metrics configuration
validateMempoolMetricsConfig :: ConfigValidation MempoolMetricsConfig []
validateMempoolMetricsConfig config =
    when (_mempoolMetricsEnabled config) $ do
        -- Validate sampling rate
        unless (_mempoolMetricsSamplingRate config > 0.0 && _mempoolMetricsSamplingRate config <= 1.0) $
            throwError "Mempool metrics sampling rate must be between 0.0 and 1.0"

        -- Validate collection interval
        when (_mempoolMetricsCollectionInterval config < 2) $
            throwError "Mempool metrics collection interval must be at least 2 seconds"

        -- Validate retention period
        when (_mempoolMetricsRetentionPeriod config < 60) $
            throwError "Mempool metrics retention period must be at least 60 seconds"

        -- Performance warnings
        when (_mempoolMetricsCollectionInterval config < 5) $
            tell ["Warning: Mempool metrics collection interval under 5 seconds may impact performance"]

-- | Validate P2P metrics configuration
validateP2PMetricsConfig :: ConfigValidation P2PMetricsConfig []
validateP2PMetricsConfig config =
    when (_p2pMetricsEnabled config) $ do
        -- Validate sampling rate
        unless (_p2pMetricsSamplingRate config > 0.0 && _p2pMetricsSamplingRate config <= 1.0) $
            throwError "P2P metrics sampling rate must be between 0.0 and 1.0"

        -- Validate collection interval
        when (_p2pMetricsCollectionInterval config < 5) $
            throwError "P2P metrics collection interval must be at least 5 seconds"

        -- Validate retention period
        when (_p2pMetricsRetentionPeriod config < 60) $
            throwError "P2P metrics retention period must be at least 60 seconds"

        -- Performance warnings
        when (_p2pMetricsGeographicTrackingEnabled config) $
            tell ["Warning: Geographic tracking can be expensive in high-peer environments"]

-- | Validate database metrics configuration
validateDatabaseMetricsConfig :: ConfigValidation DatabaseMetricsConfig []
validateDatabaseMetricsConfig config =
    when (_databaseMetricsEnabled config) $ do
        -- Validate sampling rate
        unless (_databaseMetricsSamplingRate config > 0.0 && _databaseMetricsSamplingRate config <= 1.0) $
            throwError "Database metrics sampling rate must be between 0.0 and 1.0"

        -- Validate collection interval
        when (_databaseMetricsCollectionInterval config < 10) $
            throwError "Database metrics collection interval must be at least 10 seconds"

        -- Validate retention period
        when (_databaseMetricsRetentionPeriod config < 60) $
            throwError "Database metrics retention period must be at least 60 seconds"

        -- Performance warnings
        when (_databaseMetricsSamplingRate config > 0.5) $
            tell ["Warning: High database metrics sampling rate may impact database performance"]

-- | Validate system metrics configuration
validateSystemMetricsConfig :: ConfigValidation SystemMetricsConfig []
validateSystemMetricsConfig config =
    when (_systemMetricsEnabled config) $ do
        -- Validate sampling rate
        unless (_systemMetricsSamplingRate config > 0.0 && _systemMetricsSamplingRate config <= 1.0) $
            throwError "System metrics sampling rate must be between 0.0 and 1.0"

        -- Validate collection interval
        when (_systemMetricsCollectionInterval config < 5) $
            throwError "System metrics collection interval must be at least 5 seconds"

        -- Validate retention period
        when (_systemMetricsRetentionPeriod config < 60) $
            throwError "System metrics retention period must be at least 60 seconds"

        -- Performance warnings
        when (_systemMetricsCollectionInterval config < 15) $
            tell ["Warning: System metrics collection interval under 15 seconds may impact performance"]

defaultChainwebConfiguration :: ChainwebVersion -> ChainwebConfiguration
defaultChainwebConfiguration v = ChainwebConfiguration
    { _configChainwebVersion = v
    , _configCuts = defaultCutConfig
    , _configMining = defaultMining
    , _configHeaderStream = False
    , _configReintroTxs = True
    , _configP2p = defaultP2pConfiguration
    , _configThrottling = defaultThrottlingConfig
    , _configMempoolP2p = defaultEnableConfig defaultMempoolP2pConfig
    , _configBlockGasLimit = 150_000
    , _configLogGas = False
    , _configMinGasPrice = 1e-8
    , _configPactQueueSize = 2000
    , _configReorgLimit = defaultReorgLimit
    , _configPreInsertCheckTimeout = defaultPreInsertCheckTimeout
    , _configAllowReadsInLocal = False
    , _configFullHistoricPactState = True
    , _configServiceApi = defaultServiceApiConfig
    , _configOnlySyncPact = False
    , _configReadOnlyReplay = False
    , _configSyncPactChains = Nothing
    , _configBackup = defaultBackupConfig
    , _configModuleCacheLimit = defaultModuleCacheLimit
    , _configEnableLocalTimeout = False
    , _configMetrics = defaultMetricsConfig
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _versionName (_configChainwebVersion o)
        , "cuts" .= _configCuts o
        , "mining" .= _configMining o
        , "headerStream" .= _configHeaderStream o
        , "reintroTxs" .= _configReintroTxs o
        , "p2p" .= _configP2p o
        , "throttling" .= _configThrottling o
        , "mempoolP2p" .= _configMempoolP2p o
        , "gasLimitOfBlock" .= J.toJsonViaEncode (_configBlockGasLimit o)
        , "logGas" .= _configLogGas o
        , "minGasPrice" .= J.toJsonViaEncode (_configMinGasPrice o)
        , "pactQueueSize" .= _configPactQueueSize o
        , "reorgLimit" .= _configReorgLimit o
        , "preInsertCheckTimeout" .= _configPreInsertCheckTimeout o
        , "allowReadsInLocal" .= _configAllowReadsInLocal o
        , "fullHistoricPactState" .= _configFullHistoricPactState o
        , "serviceApi" .= _configServiceApi o
        , "onlySyncPact" .= _configOnlySyncPact o
        , "readOnlyReplay" .= _configReadOnlyReplay o
        , "syncPactChains" .= _configSyncPactChains o
        , "backup" .= _configBackup o
        , "moduleCacheLimit" .= _configModuleCacheLimit o
        , "enableLocalTimeout" .= _configEnableLocalTimeout o
        , "metrics" .= _configMetrics o
        ]

instance FromJSON ChainwebConfiguration where
    parseJSON = fmap ($ defaultChainwebConfiguration Mainnet01) . parseJSON

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfiguration" $ \o -> id
        <$< setProperty configChainwebVersion "chainwebVersion"
            (findKnownVersion <=< parseJSON) o
        <*< configCuts %.: "cuts" % o
        <*< configMining %.: "mining" % o
        <*< configHeaderStream ..: "headerStream" % o
        <*< configReintroTxs ..: "reintroTxs" % o
        <*< configP2p %.: "p2p" % o
        <*< configThrottling %.: "throttling" % o
        <*< configMempoolP2p %.: "mempoolP2p" % o
        <*< configBlockGasLimit ..: "gasLimitOfBlock" % o
        <*< configLogGas ..: "logGas" % o
        <*< configMinGasPrice ..: "minGasPrice" % o
        <*< configPactQueueSize ..: "pactQueueSize" % o
        <*< configReorgLimit ..: "reorgLimit" % o
        <*< configAllowReadsInLocal ..: "allowReadsInLocal" % o
        <*< configPreInsertCheckTimeout ..: "preInsertCheckTimeout" % o
        <*< configFullHistoricPactState ..: "fullHistoricPactState" % o
        <*< configServiceApi %.: "serviceApi" % o
        <*< configOnlySyncPact ..: "onlySyncPact" % o
        <*< configReadOnlyReplay ..: "readOnlyReplay" % o
        <*< configSyncPactChains ..: "syncPactChains" % o
        <*< configBackup %.: "backup" % o
        <*< configModuleCacheLimit ..: "moduleCacheLimit" % o
        <*< configEnableLocalTimeout ..: "enableLocalTimeout" % o
        <*< configMetrics %.: "metrics" % o

pChainwebConfiguration :: MParser ChainwebConfiguration
pChainwebConfiguration = id
    <$< configChainwebVersion %:: parseVersion
    <*< configHeaderStream .:: boolOption_
        % long "header-stream"
        <> help "whether to enable an endpoint for streaming block updates"
    <*< configReintroTxs .:: enableDisableFlag
        % long "tx-reintro"
        <> help "whether to enable transaction reintroduction from losing forks"
    <*< configP2p %:: pP2pConfiguration
    <*< configMempoolP2p %::
        pEnableConfig "mempool-p2p" pMempoolP2pConfig
    <*< configBlockGasLimit .:: jsonOption
        % long "block-gas-limit"
        <> help "the sum of all transaction gas fees in a block must not exceed this number"
    <*< configLogGas .:: boolOption_
        % long "log-gas"
        <> help "log gas consumed by Pact commands"
    <*< configMinGasPrice .:: jsonOption
        % long "min-gas-price"
        <> help "the gas price of an individual transaction in a block must not be beneath this number"
    <*< configPactQueueSize .:: jsonOption
        % long "pact-queue-size"
        <> help "max size of pact internal queue"
    <*< configReorgLimit .:: jsonOption
        % long "reorg-limit"
        <> help "Max allowed reorg depth.\
                \ Consult https://github.com/kadena-io/chainweb-node/blob/master/docs/RecoveringFromDeepForks.md for\
                \ more information. "
    <*< configPreInsertCheckTimeout .:: jsonOption
        % long "pre-insert-check-timeout"
        <> help "Max allowed time in microseconds for the transactions validation in the PreInsertCheck command."
    <*< configAllowReadsInLocal .:: boolOption_
        % long "allowReadsInLocal"
        <> help "Enable direct database reads of smart contract tables in local queries."
    <*< configFullHistoricPactState .:: boolOption_
        % long "full-historic-pact-state"
        <> help "Write full historic Pact state; only enable for custodial or archival nodes."
    <*< configCuts %:: pCutConfig
    <*< configServiceApi %:: pServiceApiConfig
    <*< configMining %:: pMiningConfig
    <*< configOnlySyncPact .:: boolOption_
        % long "only-sync-pact"
        <> help "Terminate after synchronizing the pact databases to the latest cut"
    <*< configReadOnlyReplay .:: boolOption_
        % long "read-only-replay"
        <> help "Replay the block history non-destructively"
    <*< configSyncPactChains .:: fmap Just % jsonOption
        % long "sync-pact-chains"
        <> help "The only Pact databases to synchronize. If empty or unset, all chains will be synchronized."
        <> metavar "JSON list of chain ids"
    <*< configBackup %:: pBackupConfig
    <*< configModuleCacheLimit .:: option auto
        % long "module-cache-limit"
        <> help "Maximum size of the per-chain checkpointer module cache in bytes"
        <> metavar "INT"
    <*< configEnableLocalTimeout .:: option auto
        % long "enable-local-timeout"
        <> help "Enable timeout support on /local endpoints"
    <*< configMetrics %:: pMetricsConfig

parseVersion :: MParser ChainwebVersion
parseVersion = constructVersion
    <$> optional
        (option (findKnownVersion =<< textReader)
            % long "chainweb-version"
            <> short 'v'
            <> help "the chainweb version that this node is using"
        )
    <*> optional (textOption @Fork (long "fork-upper-bound" <> help "(development mode only) the latest fork the node will enable"))
    <*> optional (BlockDelay <$> textOption (long "block-delay" <> help "(development mode only) the block delay in seconds per block"))
    <*> switch (long "disable-pow" <> help "(development mode only) disable proof of work check")
    where
    constructVersion cliVersion fub bd disablePow' oldVersion = winningVersion
        & versionBlockDelay .~ fromMaybe (_versionBlockDelay winningVersion) bd
        & versionForks %~ HM.filterWithKey (\fork _ -> fork <= fromMaybe maxBound fub)
        & versionUpgrades .~
            maybe (_versionUpgrades winningVersion) (\fub' ->
                OnChains $ HM.mapWithKey
                    (\cid _ ->
                        case winningVersion ^?! versionForks . at fub' . _Just . atChain cid of
                            ForkNever -> error "Chainweb.Chainweb.Configuration.parseVersion: the fork upper bound never occurs in this version."
                            ForkAtBlockHeight fubHeight -> HM.filterWithKey (\bh _ -> bh <= fubHeight) (winningVersion ^?! versionUpgrades . atChain cid)
                            ForkAtGenesis -> winningVersion ^?! versionUpgrades . atChain cid
                    )
                    (HS.toMap (chainIds winningVersion))
            ) fub
        & versionCheats . disablePow .~ disablePow'
        where
        winningVersion = fromMaybe oldVersion cliVersion
