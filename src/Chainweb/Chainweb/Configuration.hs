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
-- * Payload Provider Config
  PayloadProviderConfig(..)
, payloadProviderConfigMinimal
, payloadProviderConfigPact
, payloadProviderConfigEvm
, defaultPayloadProviderConfig
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
, defaultCutConfig
, pCutConfig

-- * Service API Configuration
, ServiceApiConfig(..)
, serviceApiConfigPort
, serviceApiConfigInterface
, serviceApiConfigHeaderStream
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
, configP2p
, configThrottling
, configReorgLimit
, configBackup
, configServiceApi
, configReadOnlyReplay
, configSyncChains
, configPayloadProviders
, defaultChainwebConfiguration
, pChainwebConfiguration
, validateChainwebConfiguration

) where

import Chainweb.BlockHeight
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Miner.Config
import Chainweb.Pact.Types (RewindLimit)
import Chainweb.Pact.Types (defaultReorgLimit)
import Chainweb.Payload.RestAPI (PayloadBatchLimit(..), defaultServicePayloadBatchLimit)
import Chainweb.PayloadProvider.EVM (EvmProviderConfig, pEvmProviderConfig, defaultEvmProviderConfig, validateEvmProviderConfig)
import Chainweb.PayloadProvider.Minimal (MinimalProviderConfig, defaultMinimalProviderConfig, pMinimalProviderConfig)
import Chainweb.PayloadProvider.Pact.Configuration
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.EvmDevelopment
import Chainweb.Version.EvmDevelopmentSingleton
import Chainweb.Version.Mainnet
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Registry (findKnownVersion, knownVersions)
import Configuration.Utils hiding (Error, Lens', disabled)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Read qualified as T
import GHC.Generics hiding (from, to)
import Network.Wai.Handler.Warp hiding (Port)
import P2P.Node.Configuration
import Prelude hiding (log)
import System.Directory

-- -------------------------------------------------------------------------- --
-- Payload Provider Configuration

-- | Payload Provider Configurations
--
-- There is only a single Default Minimal Payload Provider Configuration.
-- Minimal payload provider cannot be disabled.
--
-- The default configuration for the pact and evm payload providers is to be
-- disabled.
--
-- When a payload provider is enabled for a chain it must match the payload
-- provider type for the respective chain.
--
data PayloadProviderConfig = PayloadProviderConfig
    { _payloadProviderConfigMinimal :: !MinimalProviderConfig
    , _payloadProviderConfigPact :: !(ChainMap PactProviderConfig)
    , _payloadProviderConfigEvm :: !(ChainMap EvmProviderConfig)
    }
    deriving (Show, Eq, Generic)

makeLenses ''PayloadProviderConfig

-- | By default only the minimal payload provider is enabled. For the pact and
-- evm chains the payload providers are disabled by default.
--
defaultPayloadProviderConfig :: PayloadProviderConfig
defaultPayloadProviderConfig = PayloadProviderConfig
    { _payloadProviderConfigMinimal = defaultMinimalProviderConfig
    , _payloadProviderConfigPact = mempty
    , _payloadProviderConfigEvm = mempty
    }

validatePayloadProviderConfig :: HasVersion => ConfigValidation PayloadProviderConfig []
validatePayloadProviderConfig conf = do
    void $ itraverse checkPactProvider $ _payloadProviderConfigPact conf
    void $ itraverse checkEvmProvider $ _payloadProviderConfigEvm conf
  where
    checkPactProvider cid _conf = case payloadProviderTypeForChain cid of
        PactProvider -> return () -- FIXME implement validation
        e -> do
            tell [ "Pact provider configured for chain " <> sshow cid <> ": " <> sshow conf ]
            throwError $ mconcat $
                [ "Wrong payload provider type configuration for chain " <> sshow cid
                , ". Expected " <> sshow e <> " but found Pact"
                ]

    checkEvmProvider cid _conf = case payloadProviderTypeForChain cid of
        EvmProvider _ -> validateEvmProviderConfig cid _conf
        e -> do
            tell [ "EVM provider configured for chain " <> sshow cid <> ": " <> sshow conf ]
            throwError $ mconcat $
                [ "Wrong payload provider type configuration for chain " <> sshow cid
                , ". Expected " <> sshow e <> " but found EVM"
                ]


instance ToJSON PayloadProviderConfig where
    toJSON o = object
        $ ("default" .= _payloadProviderConfigMinimal o)
        : others
      where
        pacts =
            [ key c .= tag "pact" v
            | (c, v) <- itoList (_payloadProviderConfigPact o)
            ]
        evms =
            [ key c .= tag "evm" v
            | (c, v) <- itoList (_payloadProviderConfigEvm o)
            ]
        others = L.sort $ pacts <> evms

        tag :: ToJSON v => T.Text -> v -> Value
        tag t v = case toJSON v of
            Object l -> Object $ KM.insert "type" (toJSON t) l
            x -> x

        key :: ChainId -> Key
        key cid = K.fromText $ "chain-" <> toText cid

-- | Configuration parser for the payload provider configuration.
--
-- * if the value for a chain is Null, then the provider is disabled
-- * if the value is an object, then
--   * the provider is enabled
--   * the value updates a given value or the default value if no
--     previous value is present
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

        go "default" c _ = return c
        go k c v = do
            cid <- parseKey k
            go2 cid c v

        -- disable provider:
        go2 cid c Null = return $ (payloadProviderConfigPact . at cid .~ Nothing) . c

        -- enabled provider:
        go2 cid c v = flip (withObject ("ProviderConfig for chain " <> sshow cid)) v $ \o -> do
            (o .: "type") >>= \case
                "pact" ->  do
                    x <- parseJSON (Object o)
                    let f Nothing = Just (x defaultPactProviderConfig)
                        f (Just y) = Just (x y)
                    return $ (payloadProviderConfigPact . at cid %~ f) . c
                "evm" -> do
                    x <- parseJSON (Object o)
                    let f Nothing = Just (x defaultEvmProviderConfig)
                        f (Just y) = Just (x y)
                    return $ (payloadProviderConfigEvm . at cid %~ f) . c
                (x :: T.Text) -> fail $ "unknown payload provider type: " <> sshow x

-- | Command line option parser for the payload provider configuration.
--
-- * if --disable-chain-X is set: disable provider
-- * otherwise parse update function and return a with function
--   f Nothing = Just (x defaultValue), if --enable-chain-X is set
--   f Nothing = Nothing, if --enable-chain-X is not set
--   f (Just y) = Just (x y), i.e. if the payload provider is already enabled
--
-- Note, that --disable-chain-X takes precendence over --enable-chain-X.
--
pPayloadProviderConfig :: MParser PayloadProviderConfig
pPayloadProviderConfig = id
    <$< parserOptionGroup "Minimal Payload Provider"
        (payloadProviderConfigMinimal %:: pMinimalProviderConfig)
    <*< pevm
  where
    cids = [ unsafeChainId i | i <- [0..100]]
        -- FIXME this is is ugly. At least use the largest know graph. Ideally,
        -- we would use the chainweb version -- but we don't know it yet.
        -- For the help message we just display options for chain 0.
    pevm = foldr (\a b -> a . b) id <$> traverse go cids
    go cid
        = parserOptionGroup "EVM [only options for chain 0 are shown]" evmChains
        <|> parserOptionGroup "Pact [only for chain 0 are shown]" pactChains
      where
        evmChains = (payloadProviderConfigEvm . at cid %~)
            <$> providerOpt "evm" cid defaultEvmProviderConfig pEvmProviderConfig
        pactChains = (payloadProviderConfigPact . at cid %~)
            <$> providerOpt "pact" cid defaultPactProviderConfig pPactProviderConfig

-- | Utility for parsing payload provider options:
--
providerOpt
    :: forall a
    . String
        -- ^ name of provider
    -> ChainId
    -> a
        -- default value
    -> (ChainId -> MParser a)
        -- option parser for the payload provider
    -> MParser (Maybe a)
providerOpt prov cid a p = f <$> dis <*> ena <*> p cid
  where
    f :: Bool -> Bool -> (a -> a) -> Maybe a -> Maybe a
    f True _ _ _ = Nothing
    f False False _ Nothing = Nothing
    f False True update Nothing = Just (update a)
    f False _ update (Just y) = Just (update y)

    ena = switch
        % long ("enable-chain-" <> T.unpack (toText cid) <> "-" <> prov)
        <> help ("enable the payload provider for this chain")
        <> mconcat [ hidden <> internal | chainIdInt @Int cid /= 0 ]

    dis = switch
        % long ("disable-chain-" <> T.unpack (toText cid) <> "-" <> prov)
        <> help ("disabled the payload provider for this chain")
        <> mconcat [ hidden <> internal | chainIdInt @Int cid /= 0 ]

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
    , _cutInitialCutFile :: !(Maybe FilePath)
    } deriving (Eq, Show)

makeLenses ''CutConfig

instance ToJSON CutConfig where
    toJSON o = object
        [ "fetchTimeout" .= _cutFetchTimeout o
        , "initialBlockHeightLimit" .= _cutInitialBlockHeightLimit o
        , "initialCutFile" .= _cutInitialCutFile o
        ]

instance FromJSON (CutConfig -> CutConfig) where
    parseJSON = withObject "CutConfig" $ \o -> id
        <$< cutFetchTimeout ..: "fetchTimeout" % o
        <*< cutInitialBlockHeightLimit ..: "initialBlockHeightLimit" % o
        <*< cutInitialCutFile ..: "initialCutFile" % o

defaultCutConfig :: CutConfig
defaultCutConfig = CutConfig
    { _cutFetchTimeout = 3_000_000
    , _cutInitialBlockHeightLimit = Nothing
    , _cutInitialCutFile = Nothing
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
    <*< cutInitialCutFile .:: fmap Just . textOption
        % long "initial-cut-file"
        <> help "When --initial-cut-file is given, use the cut in the given file as the initial cut. Note that this will not contact the P2P network."

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
    , _serviceApiConfigHeaderStream :: !Bool
        -- ^ whether to serve a header update stream endpoint.
    }
    deriving (Show, Eq, Generic)

makeLenses ''ServiceApiConfig

defaultServiceApiConfig :: ServiceApiConfig
defaultServiceApiConfig = ServiceApiConfig
    { _serviceApiConfigPort = 1848
    , _serviceApiConfigInterface = "*"
    , _serviceApiConfigValidateSpec = False
    , _serviceApiPayloadBatchLimit = defaultServicePayloadBatchLimit
    , _serviceApiConfigHeaderStream = False
    }

instance ToJSON ServiceApiConfig where
    toJSON o = object
        [ "port" .= _serviceApiConfigPort o
        , "interface" .= hostPreferenceToText (_serviceApiConfigInterface o)
        , "validateSpec" .= _serviceApiConfigValidateSpec o
        , "payloadBatchLimit" .= _serviceApiPayloadBatchLimit o
        , "headerStream" .= _serviceApiConfigHeaderStream o
        ]

instance FromJSON (ServiceApiConfig -> ServiceApiConfig) where
    parseJSON = withObject "ServiceApiConfig" $ \o -> id
        <$< serviceApiConfigPort ..: "port" % o
        <*< setProperty serviceApiConfigInterface "interface" (parseJsonFromText "interface") o
        <*< serviceApiConfigValidateSpec ..: "validateSpec" % o
        <*< serviceApiPayloadBatchLimit ..: "payloadBatchLimit" % o
        <*< serviceApiConfigHeaderStream ..: "headerStream" % o

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
    <*< serviceApiConfigHeaderStream .:: boolOption_
        % prefixLong service "header-stream"
        <> help "whether to enable an endpoint for streaming block updates"

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
    , _configP2p :: !P2pConfiguration
    , _configThrottling :: !ThrottlingConfig
    , _configReorgLimit :: !RewindLimit
    , _configBackup :: !BackupConfig
    , _configServiceApi :: !ServiceApiConfig
    , _configPayloadProviders :: PayloadProviderConfig

    -- The following properties are deprecated: history replay should not be
    -- part of normal operation mode. It should probably use a completely
    -- separate configuration.

    , _configReadOnlyReplay :: !Bool
        -- ^ do a read-only replay using the cut db params for the block heights
    , _configSyncChains :: !(Maybe [ChainId])
        -- ^ the only chains to be synchronized on startup to the latest cut.
        --   if unset, all chains will be synchronized.

    } deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

validateChainwebConfiguration :: ConfigValidation ChainwebConfiguration []
validateChainwebConfiguration c = do
    validateChainwebVersion (_configChainwebVersion c)
    withVersion (_configChainwebVersion c) $ do
        validateMinerConfig (_configMining c)
        validateBackupConfig (_configBackup c)
        unless (c ^. configChainwebVersion . versionDefaults . disablePeerValidation) $
            validateP2pConfiguration (_configP2p c)
        validatePayloadProviderConfig (_configPayloadProviders c)

validateChainwebVersion :: ConfigValidation ChainwebVersion []
validateChainwebVersion v = do
    unless (isDevelopment || elem v knownVersions) $
        throwError $ T.unwords
            [ "Specifying version properties is only legal with chainweb-version"
            , "set to recap-development or development, but version is set to"
            , sshow (_versionName v)
            ]
    where
    isDevelopment = _versionCode v `elem`
        [_versionCode dv | dv <-
            [recapDevnet, devnet, evmDevnet, evmDevnetSingleton, evmDevnetPair]
        ]

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
    , _configP2p = defaultP2pConfiguration
    , _configThrottling = defaultThrottlingConfig
    , _configReorgLimit = defaultReorgLimit
    , _configServiceApi = defaultServiceApiConfig
    , _configReadOnlyReplay = False
    , _configSyncChains = Nothing
    , _configBackup = defaultBackupConfig
    , _configPayloadProviders = defaultPayloadProviderConfig
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _versionName (_configChainwebVersion o)
        , "cuts" .= _configCuts o
        , "mining" .= _configMining o
        , "p2p" .= _configP2p o
        , "throttling" .= _configThrottling o
        , "reorgLimit" .= _configReorgLimit o
        , "serviceApi" .= _configServiceApi o
        , "readOnlyReplay" .= _configReadOnlyReplay o
        , "syncChains" .= _configSyncChains o
        , "backup" .= _configBackup o
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
        <*< configP2p %.: "p2p" % o
        <*< configThrottling %.: "throttling" % o
        <*< configReorgLimit ..: "reorgLimit" % o
        <*< configServiceApi %.: "serviceApi" % o
        <*< configReadOnlyReplay ..: "readOnlyReplay" % o
        <*< configSyncChains ..: "syncChains" % o
        <*< configBackup %.: "backup" % o
        <*< configPayloadProviders %.: "payloadProviders" % o

pChainwebConfiguration :: MParser ChainwebConfiguration
pChainwebConfiguration = id
    <$< configChainwebVersion %:: parseVersion
    <*< parserOptionGroup "P2P" (configP2p %:: pP2pConfiguration)
    <*< configReorgLimit .:: jsonOption
        % long "reorg-limit"
        <> help "Max allowed reorg depth.\
                \ Consult https://github.com/kadena-io/chainweb-node/blob/master/docs/RecoveringFromDeepForks.md for\
                \ more information. "
    <*< parserOptionGroup "Cut Processing" (configCuts %:: pCutConfig)
    <*< parserOptionGroup "Service API" (configServiceApi %:: pServiceApiConfig)
    <*< parserOptionGroup "Mining Coordination" (configMining %:: pMiningConfig)
    <*< configReadOnlyReplay .:: boolOption_
        % long "read-only-replay"
        <> help "Replay the block history non-destructively"
    <*< configSyncChains .:: fmap Just % jsonOption
        % long "sync-chains"
        <> help "The only Pact databases to synchronize. If empty or unset, all chains will be synchronized."
        <> metavar "JSON list of chain ids"
    <*< parserOptionGroup "Backup" (configBackup %:: pBackupConfig)

    -- FIXME support payload providers
    <*< configPayloadProviders %:: pPayloadProviderConfig

parseVersion :: MParser ChainwebVersion
parseVersion = constructVersion
    <$> optional
        (option (findKnownVersion =<< textReader)
            % long "chainweb-version"
            <> short 'v'
            <> help "the chainweb version that this node is using"
            <> metavar (T.unpack $
                "[" <> T.intercalate "," (getChainwebVersionName . _versionName <$> knownVersions) <> "]")
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
                ChainMap $ HM.mapWithKey
                    (\cid _ ->
                        case winningVersion ^?! versionForks . at fub' . _Just . atChain cid of
                            ForkNever -> error "Chainweb.Chainweb.Configuration.parseVersion: the fork upper bound never occurs in this version."
                            ForkAtBlockHeight fubHeight -> HM.filterWithKey (\bh _ -> bh <= fubHeight) (winningVersion ^?! versionUpgrades . atChain cid)
                            ForkAtGenesis -> winningVersion ^?! versionUpgrades . atChain cid
                    )
                    (HS.toMap (withVersion winningVersion chainIds))
            ) fub
        & versionCheats . disablePow .~ disablePow'
        where
        winningVersion = fromMaybe oldVersion cliVersion
