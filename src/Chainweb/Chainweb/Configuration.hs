{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

-- * Placeholder for Pact Provider Config
  PactProviderConfig(..)
, defaultPactProviderConfig

-- * Payload Provider Config
, PayloadProviderConfig(..)
, payloadProviderConfigMinimal
, payloadProviderConfigPact
, payloadProviderConfigEvm
, defaultPayloadProviderConfig
, minimalPayloadProviderConfig
, validatePayloadProviderConfig

-- * Throttling Configuration
, ThrottlingConfig(..)
, throttlingRate
, throttlingPeerRate
, throttlingMempoolRate
, defaultThrottlingConfig

-- * Cut Configuration
, CutConfig(..)
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
, configFullHistoricPactState
, configBackup
, configServiceApi
, configOnlySyncPact
, configSyncPactChains
, configEnableLocalTimeout
, defaultChainwebConfiguration
, pChainwebConfiguration
, validateChainwebConfiguration

) where

import Chainweb.BlockHeight
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool qualified as Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Miner.Config
import Chainweb.Pact.Backend.DbCache (DbCacheLimitBytes)
import Chainweb.Pact.Types (RewindLimit(..))
import Chainweb.Pact.Types (defaultReorgLimit, defaultModuleCacheLimit, defaultPreInsertCheckTimeout)
import Chainweb.Payload.RestAPI (PayloadBatchLimit(..), defaultServicePayloadBatchLimit)
import Chainweb.PayloadProvider.EVM (EvmProviderConfig, defaultEvmProviderConfig, pEvmProviderConfig)
import Chainweb.PayloadProvider.Minimal (MinimalProviderConfig, defaultMinimalProviderConfig, pMinimalProviderConfig)
import Chainweb.Time hiding (second)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.EvmDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Registry
import Configuration.Utils hiding (Error, Lens', disabled)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Bifunctor
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Read qualified as T
import GHC.Generics hiding (from)
import Network.Wai.Handler.Warp hiding (Port)
import Numeric.Natural (Natural)
import P2P.Node.Configuration
import Pact.JSON.Encode qualified as J
import Prelude hiding (log)
import System.Directory

-- -------------------------------------------------------------------------- --
-- Payload Provider Configuration

-- | Placeholder for PactProviderConfig
--
data PactProviderConfig = PactProviderConfig
    deriving (Show, Eq, Generic)

instance ToJSON PactProviderConfig where
    toJSON PactProviderConfig = object []

instance FromJSON PactProviderConfig where
    parseJSON = withObject "PactProviderConfig" $ \_ -> pure PactProviderConfig

instance FromJSON (PactProviderConfig -> PactProviderConfig) where
    parseJSON = withObject "PactProviderConfig" $ \_ -> pure id

defaultPactProviderConfig :: PactProviderConfig
defaultPactProviderConfig = PactProviderConfig

-- | Payload Provider Configurations
--
-- There is only a single Default Minimal Payload Provider Configuration.
--
data PayloadProviderConfig = PayloadProviderConfig
    { _payloadProviderConfigMinimal :: !MinimalProviderConfig
    , _payloadProviderConfigPact :: !(HM.HashMap ChainId PactProviderConfig)
    , _payloadProviderConfigEvm :: !(HM.HashMap ChainId EvmProviderConfig)
    }
    deriving (Show, Eq, Generic)

makeLenses ''PayloadProviderConfig

-- | This has to depend on the Chainweb Version
--
defaultPayloadProviderConfig :: ChainwebVersion -> PayloadProviderConfig
defaultPayloadProviderConfig v = PayloadProviderConfig
    { _payloadProviderConfigMinimal = defaultMinimalProviderConfig
    , _payloadProviderConfigPact = pacts
    , _payloadProviderConfigEvm = evms
    }
  where
    (pacts, evms) = go (toList $ chainIds v)
    go :: [ChainId] -> (HM.HashMap ChainId PactProviderConfig, HM.HashMap ChainId EvmProviderConfig)
    go [] = (mempty, mempty)
    go (h:t) = case payloadProviderTypeForChain v h of
        MinimalProvider -> go t
        PactProvider -> first (HM.insert h defaultPactProviderConfig) $ go t
        EvmProvider _ -> second (HM.insert h defaultEvmProviderConfig) $ go t


minimalPayloadProviderConfig :: MinimalProviderConfig -> PayloadProviderConfig
minimalPayloadProviderConfig m = PayloadProviderConfig
    { _payloadProviderConfigMinimal = m
    , _payloadProviderConfigPact = mempty
    , _payloadProviderConfigEvm = mempty
    }

validatePayloadProviderConfig :: ChainwebVersion -> ConfigValidation PayloadProviderConfig []
validatePayloadProviderConfig v conf = do
    go (toList $ chainIds v)
  where
    go [] = return ()
    go (c:t) = go t <* case payloadProviderTypeForChain v c of
        PactProvider -> unless (HM.member c (_payloadProviderConfigPact conf)) $
            if (HM.member c (_payloadProviderConfigEvm conf))
              then throwError $ mconcat $
                [ "Wrong payload provdider type configuration for chain " <> sshow c
                , ". Expected Pact but found EVM"
                ] <> msg
              else throwError $ mconcat
                    $ "Missing Pact payload provider configuration for chain " <> sshow c
                    : msg
        EvmProvider _ -> unless (HM.member c (_payloadProviderConfigEvm conf)) $
            if (HM.member c (_payloadProviderConfigPact conf))
              then throwError $ mconcat $
                [ "Wrong payload provdider type configuration for chain " <> sshow c
                , ". Expected EVM but found Pact"
                ] <> msg
              else throwError $ mconcat
                    $ "Missing EVM payload provider configuration for chain " <> sshow c
                    : msg
        MinimalProvider -> return ()
    msg =
        [ ". In order to use chainweb-node with chainweb version " <> sshow v
        , " you must provide a configuraton for all payload providers except"
        , " the chains that use the default payload provider."
        , " The following is the default payload provider configuration for"
        , " chainweb version " <> sshow v
        , ": " <> encodeToText (defaultPayloadProviderConfig v)
        ]

instance ToJSON PayloadProviderConfig where
    toJSON o = object
        $ ("default" .= _payloadProviderConfigMinimal o)
        : others
      where
        pacts =
            [ key c .= tag "pact" v
            | (c, v) <- HM.toList (_payloadProviderConfigPact o)
            ]
        evms =
            [ key c .= tag "evm" v
            | (c, v) <- HM.toList (_payloadProviderConfigEvm o)
            ]
        others = L.sort $ pacts <> evms

        tag :: ToJSON v => T.Text -> v -> Value
        tag t v = case toJSON v of
            Object l -> Object $ KM.insert "type" (toJSON t) l
            x -> x

        key :: ChainId -> Key
        key cid = K.fromText $ "chain-" <> toText cid

-- | NOTE: This creates unsafe ChainIds. The result should only be used after
-- validation against the chainweb version.
--
instance FromJSON PayloadProviderConfig where
    parseJSON = withObject "PayloadProviderConfig" $ \o -> do
        minimal <- o .: "default"
        ifoldlM go (minimalPayloadProviderConfig minimal) (KM.toMapText o)
      where
        parseKey k = case T.stripPrefix "chain-" k of
            Nothing -> fail $ "failed to parse chain key: " <> sshow k
            Just x -> case T.decimal x of
                Left e -> fail $ "failed to parse chain value: " <> e
                Right (n, "") -> return $ unsafeChainId n
                Right _ -> fail $ "trailng garabage when parsing chain value: " <> sshow x

        go "default" c = const (return c)
        go k c = withObject "ProviderConfig for Chain" $ \o -> do
            cid <- parseKey k
            (o .: "type") >>= \case
                "pact" -> do
                    x <- parseJSON (Object o)
                    return $ c & payloadProviderConfigPact . at cid .~ Just x
                "evm" -> do
                    x <- parseJSON (Object o)
                    return $ c & payloadProviderConfigEvm . at cid .~ Just x
                (x :: T.Text) -> fail $ "unknown payload provider type: " <> sshow x

-- | FIXME: test this instance.
--
instance FromJSON (PayloadProviderConfig -> PayloadProviderConfig) where
    parseJSON = withObject "PayloadProviderConfig" $ \o -> do
        updateMinimal <- payloadProviderConfigMinimal %.: "default" % o
        ifoldlM go updateMinimal (KM.toMapText o)
      where
        parseKey k = case T.stripPrefix "chain-" k of
            Nothing -> fail $ "failed to parse chain key: " <> sshow k
            Just x -> case T.decimal x of
                Left e -> fail $ "failed to parse chain value: " <> e
                Right (n, "") -> return $ unsafeChainId n
                Right _ -> fail $ "trailng garabage when parsing chain value: " <> sshow x

        go "default" c = const (return c)
        go k c = withObject "ProviderConfig for Chain" $ \o -> do
            cid <- parseKey k
            (o .: "type") >>= \case
                "pact" ->  do
                    x <- parseJSON (Object o)
                    return $ (payloadProviderConfigPact . at cid %~ x) . c
                "evm" -> do
                    x <- parseJSON (Object o)
                    return $ (payloadProviderConfigEvm . at cid %~ x) . c
                (x :: T.Text) -> fail $ "unknown payload provider type: " <> sshow x

pPayloadProviderConfig :: MParser PayloadProviderConfig
pPayloadProviderConfig = id
    <$< payloadProviderConfigMinimal %:: pMinimalProviderConfig
    <*< pevm
  where
    cids = [ unsafeChainId i | i <- [0..20]]
        -- FIXME this is is really ugly and also clutters the help message.
        -- At least use the largest know graph. Ideally, we would use the
        -- chainweb version -- but we don't know it yet.
        --
        -- FIXME: at the very least we should hide the all but the first options
        -- from the help message!
    pevm = foldr (\a b -> a . b) id <$> traverse go cids
    go cid = (payloadProviderConfigEvm . ix cid %~) <$> (pEvmProviderConfig cid)

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

data CutConfig = CutConfig
    { _cutFetchTimeout :: !Int
    , _cutInitialBlockHeightLimit :: !(Maybe BlockHeight)
    , _cutFastForwardBlockHeightLimit :: !(Maybe BlockHeight)
    } deriving (Eq, Show)

makeLenses ''CutConfig

instance ToJSON CutConfig where
    toJSON o = object
        [ "fetchTimeout" .= _cutFetchTimeout o
        , "initialBlockHeightLimit" .= _cutInitialBlockHeightLimit o
        , "fastForwardBlockHeightLimit" .= _cutFastForwardBlockHeightLimit o
        ]

instance FromJSON (CutConfig -> CutConfig) where
    parseJSON = withObject "CutConfig" $ \o -> id
        <$< cutFetchTimeout ..: "fetchTimeout" % o
        <*< cutInitialBlockHeightLimit ..: "initialBlockHeightLimit" % o
        <*< cutFastForwardBlockHeightLimit ..: "fastForwardBlockHeightLimit" % o

defaultCutConfig :: CutConfig
defaultCutConfig = CutConfig
    { _cutFetchTimeout = 3_000_000
    , _cutInitialBlockHeightLimit = Nothing
    , _cutFastForwardBlockHeightLimit = Nothing
    }

pCutConfig :: MParser CutConfig
pCutConfig = id
    <$< cutFetchTimeout .:: option auto
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
    , _configPayloadProviders :: PayloadProviderConfig
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
    validatePayloadProviderConfig (_configChainwebVersion c) (_configPayloadProviders c)

validateChainwebVersion :: ConfigValidation ChainwebVersion []
validateChainwebVersion v = do
    unless (isDevelopment || elem v knownVersions) $
        throwError $ T.unwords
            [ "Specifying version properties is only legal with chainweb-version"
            , "set to recap-development or development, but version is set to"
            , sshow (_versionName v)
            ]
    where
    isDevelopment = _versionCode v `elem` [_versionCode dv | dv <- [recapDevnet, devnet, evmDevnet]]

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
    , _configPayloadProviders = minimalPayloadProviderConfig defaultMinimalProviderConfig
        -- Similar to bootstrap-peers, there is no default configuration that
        -- is valid accross chainweb versions.
        --
        -- We have to options:
        -- 1. require that users explicitely configure all payload providers
        --    that they want to use (FIXME: implement support for opting out of
        --    payload providers for some chains, and force miners to keep them
        --    all)
        -- 2. use the default value for mainnet. That configuration will simply
        --    fail validation on other networks.
        --
        -- However, even on mainnet users will most likely have to provide an
        -- explicit configuration once we have external providers on mainet. At
        -- that point it probably makes sense to use the first option.
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
        , "payloadProviders" .= _configPayloadProviders o
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
        <*< configPayloadProviders %.: "payloadProviders" % o

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

    -- FIXME support payload providers
    <*< configPayloadProviders %:: pPayloadProviderConfig

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
