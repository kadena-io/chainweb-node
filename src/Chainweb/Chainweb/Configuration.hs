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
, configRosetta
, configBackup
, configServiceApi
, configOnlySyncPact
, configSyncPactChains
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
import Chainweb.Pact.Types (defaultReorgLimit, defaultModuleCacheLimit, defaultLocalRewindDepthLimit, defaultPreInsertCheckTimeout)
import Chainweb.Pact.Service.Types (RewindLimit(..))
import Chainweb.Payload.RestAPI (PayloadBatchLimit(..), defaultServicePayloadBatchLimit)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.FastDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Registry
import Chainweb.Time

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
    , _configLocalRewindDepthLimit :: !RewindLimit
    , _configPreInsertCheckTimeout :: !Micros
    , _configAllowReadsInLocal :: !Bool
    , _configRosetta :: !Bool
    , _configBackup :: !BackupConfig
    , _configServiceApi :: !ServiceApiConfig
    , _configOnlySyncPact :: !Bool
        -- ^ exit after synchronizing pact dbs to the latest cut
    , _configSyncPactChains :: !(Maybe [ChainId])
        -- ^ the only chains to be synchronized on startup to the latest cut.
        --   if unset, all chains will be synchronized.
    , _configModuleCacheLimit :: !DbCacheLimitBytes
        -- ^ module cache size limit in bytes
    } deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

instance HasChainwebVersion ChainwebConfiguration where
    _chainwebVersion = _configChainwebVersion
    {-# INLINE _chainwebVersion #-}

validateChainwebConfiguration :: ConfigValidation ChainwebConfiguration []
validateChainwebConfiguration c = do
    validateMinerConfig (_configMining c)
    validateBackupConfig (_configBackup c)
    unless (c ^. chainwebVersion . versionDefaults . disablePeerValidation) $
        validateP2pConfiguration (_configP2p c)
    validateChainwebVersion (_configChainwebVersion c)

validateChainwebVersion :: ConfigValidation ChainwebVersion []
validateChainwebVersion v = unless (isDevelopment || elem v knownVersions) $
    throwError $ T.unwords
        [ "Specifying version properties is only legal with chainweb-version"
        , "set to development or fast-development, but version is set to"
        , sshow (_versionName v)
        ]
    where
    isDevelopment = _versionCode v `elem` [_versionCode dv | dv <- [devnet, fastDevnet]]

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
    , _configBlockGasLimit = 150_000
    , _configLogGas = False
    , _configMinGasPrice = 1e-8
    , _configPactQueueSize = 2000
    , _configReorgLimit = defaultReorgLimit
    , _configLocalRewindDepthLimit = defaultLocalRewindDepthLimit
    , _configPreInsertCheckTimeout = defaultPreInsertCheckTimeout
    , _configAllowReadsInLocal = False
    , _configRosetta = False
    , _configServiceApi = defaultServiceApiConfig
    , _configOnlySyncPact = False
    , _configSyncPactChains = Nothing
    , _configBackup = defaultBackupConfig
    , _configModuleCacheLimit = defaultModuleCacheLimit
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
        , "localRewindDepthLimit" .= _configLocalRewindDepthLimit o
        , "preInsertCheckTimeout" .= _configPreInsertCheckTimeout o
        , "allowReadsInLocal" .= _configAllowReadsInLocal o
        , "rosetta" .= _configRosetta o
        , "serviceApi" .= _configServiceApi o
        , "onlySyncPact" .= _configOnlySyncPact o
        , "syncPactChains" .= _configSyncPactChains o
        , "backup" .= _configBackup o
        , "moduleCacheLimit" .= _configModuleCacheLimit o
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
        <*< configRosetta ..: "rosetta" % o
        <*< configServiceApi %.: "serviceApi" % o
        <*< configOnlySyncPact ..: "onlySyncPact" % o
        <*< configSyncPactChains ..: "syncPactChains" % o
        <*< configBackup %.: "backup" % o
        <*< configModuleCacheLimit ..: "moduleCacheLimit" % o

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
    <*< configLocalRewindDepthLimit .:: jsonOption
        % long "local-rewind-depth-limit"
        <> help "Max allowed rewind depth for the local command."
    <*< configPreInsertCheckTimeout .:: jsonOption
        % long "pre-insert-check-timeout"
        <> help "Max allowed time in microseconds for the transactions validation in the PreInsertCheck command."
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
    <*< configSyncPactChains .:: fmap Just % jsonOption
        % long "sync-pact-chains"
        <> help "The only Pact databases to synchronize. If empty or unset, all chains will be synchronized."
        <> metavar "JSON list of chain ids"
    <*< configBackup %:: pBackupConfig
    <*< configModuleCacheLimit .:: option auto
        % long "module-cache-limit"
        <> help "Maximum size of the per-chain checkpointer module cache in bytes"
        <> metavar "INT"
    where

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
                        case winningVersion ^?! versionForks . at fub' . _Just . onChain cid of
                            ForkNever -> error "Chainweb.Chainweb.Configuration.parseVersion: the fork upper bound never occurs in this version."
                            ForkAtBlockHeight fubHeight -> HM.filterWithKey (\bh _ -> bh <= fubHeight) (winningVersion ^?! versionUpgrades . onChain cid)
                            ForkAtGenesis -> winningVersion ^?! versionUpgrades . onChain cid
                    )
                    (HS.toMap (chainIds winningVersion))
            ) fub
        & versionCheats . disablePow .~ disablePow'
        where
        winningVersion = fromMaybe oldVersion cliVersion

