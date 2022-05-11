{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Chainweb.Configuration
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Chainweb.Configuration
(
-- * Throttling Configuration
  ThrottlingConfig(..)
, throttlingRate
, throttlingMiningRate
, throttlingPeerRate
, throttlingLocalRate
, defaultThrottlingConfig

-- * Cut Configuration
, ChainDatabaseGcConfig(..)
, chainDatabaseGcToText
, chainDatabaseGcFromText

, CutConfig(..)
, cutIncludeOrigin
, cutPruneChainDatabase
, cutFetchTimeout
, cutInitialCutHeightLimit
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

-- * Chainweb Configuration
, ChainwebConfiguration(..)
, configChainwebVersion
, configMining
, configHeaderStream
, configReintroTxs
, configP2p
, configBlockGasLimit
, configMinGasPrice
, configThrottling
, configReorgLimit
, configRosetta
, configBackup
, configServiceApi
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
import Data.Maybe
import qualified Data.Text as T

import GHC.Generics hiding (from)

import Network.Wai.Handler.Warp hiding (Port)

import Numeric.Natural (Natural)

import Prelude hiding (log)

import System.Directory

-- internal modules

import Chainweb.BlockHeight
import Chainweb.HostAddress
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Miner.Config
import Chainweb.Pact.Types (defaultReorgLimit)
import Chainweb.Payload.RestAPI (PayloadBatchLimit(..), defaultServicePayloadBatchLimit)
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration

-- -------------------------------------------------------------------------- --
-- Throttling Configuration

data ThrottlingConfig = ThrottlingConfig
    { _throttlingRate :: !Double
    , _throttlingMiningRate :: !Double
        -- ^ The rate should be sufficient to make at least on call per cut. We
        -- expect an cut to arrive every few seconds.
        --
        -- Default is 10 per second.
    , _throttlingPeerRate :: !Double
        -- ^ This should throttle aggressively. This endpoint does an expensive
        -- check of the client. And we want to keep bad actors out of the
        -- system. There should be no need for a client to call this endpoint on
        -- the same node more often than at most few times peer minute.
        --
        -- Default is 1 per second
        --
    , _throttlingLocalRate :: !Double
    }
    deriving stock (Eq, Show)

makeLenses ''ThrottlingConfig

defaultThrottlingConfig :: ThrottlingConfig
defaultThrottlingConfig = ThrottlingConfig
    { _throttlingRate = 200 -- per second
    , _throttlingMiningRate = 5 --  per second
    , _throttlingPeerRate = 21 -- per second, one for each p2p network
    , _throttlingLocalRate = 0.1  -- per 10 seconds
    }

instance ToJSON ThrottlingConfig where
    toJSON o = object
        [ "global" .= _throttlingRate o
        , "mining" .= _throttlingMiningRate o
        , "putPeer" .= _throttlingPeerRate o
        , "local" .= _throttlingLocalRate o
        ]

instance FromJSON (ThrottlingConfig -> ThrottlingConfig) where
    parseJSON = withObject "ThrottlingConfig" $ \o -> id
        <$< throttlingRate ..: "global" % o
        <*< throttlingMiningRate ..: "mining" % o
        <*< throttlingPeerRate ..: "putPeer" % o
        <*< throttlingLocalRate ..: "local" % o

-- -------------------------------------------------------------------------- --
-- Cut Coniguration

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
    { _cutIncludeOrigin :: !Bool
    , _cutPruneChainDatabase :: !ChainDatabaseGcConfig
    , _cutFetchTimeout :: !Int
    , _cutInitialCutHeightLimit :: !(Maybe CutHeight)
    } deriving (Eq, Show)

makeLenses ''CutConfig

instance ToJSON CutConfig where
    toJSON o = object
        [ "pruneChainDatabase" .= _cutPruneChainDatabase o
        , "fetchTimeout" .= _cutFetchTimeout o
        , "initialCutHeightLimit" .= _cutInitialCutHeightLimit o ]

instance FromJSON (CutConfig -> CutConfig) where
    parseJSON = withObject "CutConfig" $ \o -> id
        <$< cutIncludeOrigin ..: "includeOrigin" % o
        <*< cutPruneChainDatabase ..: "pruneChainDatabase" % o
        <*< cutFetchTimeout ..: "fetchTimeout" % o
        <*< cutInitialCutHeightLimit ..: "initialCutHeightLimit" % o

defaultCutConfig :: CutConfig
defaultCutConfig = CutConfig
    { _cutIncludeOrigin = True
    , _cutPruneChainDatabase = GcNone
    , _cutFetchTimeout = 3_000_000
    , _cutInitialCutHeightLimit = Nothing
    }

pCutConfig :: MParser CutConfig
pCutConfig = id
    <$< cutIncludeOrigin .:: boolOption_
        % long "cut-include-origin"
        <> hidden
        <> internal
        <> help "whether to include the origin when sending cuts"
    <*< cutPruneChainDatabase .:: textOption
        % long "prune-chain-database"
        <> help
            ( "How to prune the chain database on startup."
            <> " Pruning headers takes about between 10s to 2min. "
            <> " Pruning headers with full header validation (headers-checked) and full GC can take"
            <> " a longer time (up to 10 minutes or more)."
            )
        <> metavar "none|headers|headers-checked|full"
    <*< cutFetchTimeout .:: option auto
        % long "cut-fetch-timeout"
        <> help "The timeout for processing new cuts in microseconds"
    -- cutInitialCutHeightLimit isn't supported on the command line

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
    , _serviceApiPayloadBatchLimit = defaultServicePayloadBatchLimit
    }

instance ToJSON ServiceApiConfig where
    toJSON o = object
        [ "port" .= _serviceApiConfigPort o
        , "interface" .= hostPreferenceToText (_serviceApiConfigInterface o)
        , "payloadBatchLimit" .= _serviceApiPayloadBatchLimit o
        ]

instance FromJSON (ServiceApiConfig -> ServiceApiConfig) where
    parseJSON = withObject "ServiceApiConfig" $ \o -> id
        <$< serviceApiConfigPort ..: "port" % o
        <*< setProperty serviceApiConfigInterface "interface" (parseJsonFromText "interface") o
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
    , _configMinGasPrice :: !Mempool.GasPrice
    , _configPactQueueSize :: !Natural
    , _configReorgLimit :: !Natural
    , _configValidateHashesOnReplay :: !Bool
        -- ^ Re-validate payload hashes during replay.
    , _configAllowReadsInLocal :: !Bool
    , _configRosetta :: !Bool
    , _configBackup :: !BackupConfig
    , _configServiceApi :: !ServiceApiConfig
    , _configOnlySyncPact :: !Bool
        -- ^ exit after synchronizing pact dbs to the latest cut
    } deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

instance HasChainwebVersion ChainwebConfiguration where
    _chainwebVersion = _configChainwebVersion
    {-# INLINE _chainwebVersion #-}

validateChainwebConfiguration :: ConfigValidation ChainwebConfiguration []
validateChainwebConfiguration c = do
    validateMinerConfig (_configMining c)
    validateBackupConfig (_configBackup c)
    case _configChainwebVersion c of
        Mainnet01 -> validateP2pConfiguration (_configP2p c)
        Testnet04 -> validateP2pConfiguration (_configP2p c)
        _ -> return ()

validateBackupConfig :: ConfigValidation BackupConfig []
validateBackupConfig c =
    for_ (_configBackupDirectory c) $ \dir -> do
        liftIO $ createDirectoryIfMissing True dir
        perms <- liftIO (getPermissions dir)
        unless (writable perms) $
            throwError $ "Backup directory " <> T.pack dir <> " is not writable"

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
    , _configBlockGasLimit = 150000
    , _configMinGasPrice = 1e-8
    , _configPactQueueSize = 2000
    , _configReorgLimit = int defaultReorgLimit
    , _configValidateHashesOnReplay = False
    , _configAllowReadsInLocal = False
    , _configRosetta = False
    , _configServiceApi = defaultServiceApiConfig
    , _configOnlySyncPact = False
    , _configBackup = defaultBackupConfig
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "cuts" .= _configCuts o
        , "mining" .= _configMining o
        , "headerStream" .= _configHeaderStream o
        , "reintroTxs" .= _configReintroTxs o
        , "p2p" .= _configP2p o
        , "throttling" .= _configThrottling o
        , "mempoolP2p" .= _configMempoolP2p o
        , "gasLimitOfBlock" .= _configBlockGasLimit o
        , "minGasPrice" .= _configMinGasPrice o
        , "pactQueueSize" .= _configPactQueueSize o
        , "reorgLimit" .= _configReorgLimit o
        , "validateHashesOnReplay" .= _configValidateHashesOnReplay o
        , "allowReadsInLocal" .= _configAllowReadsInLocal o
        , "rosetta" .= _configRosetta o
        , "serviceApi" .= _configServiceApi o
        , "onlySyncPact" .= _configOnlySyncPact o
        , "backup" .= _configBackup o
        ]

instance FromJSON ChainwebConfiguration where
    parseJSON = withObject "ChainwebConfiguration" $ \o -> do
        v <- o .: "chainwebVersion" .!= Mainnet01
        ($ defaultChainwebConfiguration v) <$> parseJSON (Object o)

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configCuts %.: "cuts" % o
        <*< configMining %.: "mining" % o
        <*< configHeaderStream ..: "headerStream" % o
        <*< configReintroTxs ..: "reintroTxs" % o
        <*< configP2p %.: "p2p" % o
        <*< configThrottling %.: "throttling" % o
        <*< configMempoolP2p %.: "mempoolP2p" % o
        <*< configBlockGasLimit ..: "gasLimitOfBlock" % o
        <*< configMinGasPrice ..: "minGasPrice" % o
        <*< configPactQueueSize ..: "pactQueueSize" % o
        <*< configReorgLimit ..: "reorgLimit" % o
        <*< configValidateHashesOnReplay ..: "validateHashesOnReplay" % o
        <*< configAllowReadsInLocal ..: "allowReadsInLocal" % o
        <*< configRosetta ..: "rosetta" % o
        <*< configServiceApi %.: "serviceApi" % o
        <*< configOnlySyncPact ..: "onlySyncPact" % o
        <*< configBackup %.: "backup" % o

pChainwebConfiguration :: MParser ChainwebConfiguration
pChainwebConfiguration = id
    <$< configChainwebVersion .:: textOption
        % long "chainweb-version"
        <> short 'v'
        <> help "the chainweb version that this node is using"
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
    <*< configValidateHashesOnReplay .:: boolOption_
        % long "validateHashesOnReplay"
        <> help "Re-validate payload hashes during transaction replay."
    <*< configAllowReadsInLocal .:: boolOption_
        % long "allowReadsInLocal"
        <> help "Enable direct database reads of smart contract tables in local queries."
    <*< configRosetta .:: boolOption_
        % long "rosetta"
        <> help "Enable the Rosetta endpoints."
    <*< configCuts %:: pCutConfig
    <*< configServiceApi %:: pServiceApiConfig
    <*< configMining %:: pMiningConfig
    <*< configOnlySyncPact .:: boolOption_
        % long "only-sync-pact"
        <> help "Terminate after synchronizing the pact databases to the latest cut"
    <*< configBackup %:: pBackupConfig

