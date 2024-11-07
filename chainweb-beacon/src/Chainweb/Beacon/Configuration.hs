{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Beacon.Configuration
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Beacon.Configuration
(
-- * Throttling Configuration
  P2pThrottlingConfig(..)
, p2pThrottlingRate
, p2pThrottlingPutPeerRate
, defaultP2pThrottlingConfig
, pP2pThrottlingConfig

-- * Service API Configuration
, ServiceApiConfig(..)
, serviceApiConfigPort
, serviceApiConfigInterface
, defaultServiceApiConfig
, pServiceApiConfig

-- * Chainweb Configuration
, BeaconConfiguration(..)
, configChainwebVersion
, configHistoryLimit
, configMining
, configP2p
, configP2pThrottling
, configServiceApi
, configPayloadProviders
, defaultBeaconConfiguration
, pBeaconConfiguration
, validateBeaconConfiguration

) where

import Configuration.Utils hiding (Error, Lens', disabled)

import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch (throwM)
import Control.Monad.Except

import Data.Foldable
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import GHC.Generics hiding (from)

import Network.Wai.Handler.Warp hiding (Port)

import Numeric.Natural (Natural)

import Prelude hiding (log)

-- internal modules

import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Miner.Config
import Chainweb.Payload.RestAPI (PayloadBatchLimit(..), defaultServicePayloadBatchLimit)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.Mainnet
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Registry

import P2P.Node.Configuration

-- -------------------------------------------------------------------------- --
-- Outline

-- databaseDirectory: null
--
-- chainweb-beacon:
--
--   chainwebVersion: mainnet01
--
--   # This must not be smaller than the reorg limit
--   minimumHistoryDepth: 480
--
--   mining:
--     enabled: false
--
--     # what does this?
--     limit: 1200
--
--     # what should we support here? Ideally all kinds of keys including gas
--     # station accounts and cross-chain support.
--     miners: []
--
--     # Microsecond seems a bit to aggressive, but, well, its future proof at
--     # least.
--     payloadRefreshDelay: 15000000
--
--     # Do we need this? We do not support public mining since a long time now.
--     # updateStreamLimit: 2000
--
--     # Even this may not be needed nodays in any more. If somebody connects a
--     # client that is not a good citizen it is not our problem.
--     # updateStreamTimeout: 240
--
--   p2p:
--     bootstrapReachability: 0.5
--     ignoreBootstrapNodes: false
--     maxPeerCount: 50
--     maxSessionCount: 10
--     peer:
--       certificateChain: null
--       certificateChainFile: null
--       hostaddress:
--         hostname: 0.0.0.0
--         port: 1789
--       interface: '*'
--       key: null
--       keyFile: null
--     peers: []
--     private: false
--     sessionTimeout: 240
--
--   # TODO this should be more finegrained with default values based on the
--   # protocol.
--   p2pThrottling:
--     global: 50.0
--     putPeer: 11
--
--   # Should the service API move completely to EL?
--   # We need at least some basic API for the beacon chain:
--   #
--   # - cuts
--   # - headers
--   # - what about payloads and outputs? At least for now we need that.
--   # - what about SPV?
--   serviceApi:
--     interface: '*'
--     payloadBatchLimit: 1000
--     port: 1848
--
--   # How do we actually enforce a particular payload provider?
--   # How do we call it? a protocol?
--
--   # 'enabled: false' is the default for a chain.
--   payloadProviders:
--   - chain: 0
--     enabled: true
--     hostaddress:
--       host: localhost
--       port: 19890
--     type: evm
--     x-auth:
--      jwt-secret: abcdef
--   - chain: 1
--     enabled: true
--     hostaddress:
--       host: localhost
--       port: 19891
--     type: evm
--     x-auth:
--      jwt-secret: abcdef
--   - chain: 2
--     enabled: true
--     hostaddress:
--       host: localhost
--       port: 19891
--     type: evm
--     x-auth:
--      jwt-secret: abcdef
--   - chain: 3
--     enabled: true
--     hostaddress:
--       host: localhost
--       port: 19891
--     type: evm
--     x-auth:
--      jwt-secret: abcdef
--

-- -------------------------------------------------------------------------- --
-- P2p Throttling Configuration

data P2pThrottlingConfig = P2pThrottlingConfig
    { _p2pThrottlingRate :: !Double
    , _p2pThrottlingPutPeerRate :: !Double
        -- ^ This should throttle aggressively. This endpoint does an expensive
        -- check of the client. And we want to keep bad actors out of the
        -- system. There should be no need for a client to call this endpoint on
        -- the same node more often than at most few times peer minute.
    }
    deriving stock (Eq, Show)

makeLenses ''P2pThrottlingConfig

defaultP2pThrottlingConfig :: P2pThrottlingConfig
defaultP2pThrottlingConfig = P2pThrottlingConfig
    { _p2pThrottlingRate = 50 -- per second, in a 100 burst
    , _p2pThrottlingPutPeerRate = 11 -- per second, 1 for each p2p network
    }

instance ToJSON P2pThrottlingConfig where
    toJSON o = object
        [ "global" .= _p2pThrottlingRate o
        , "putPeer" .= _p2pThrottlingPutPeerRate o
        ]

instance FromJSON (P2pThrottlingConfig -> P2pThrottlingConfig) where
    parseJSON = withObject "P2pThrottlingConfig" $ \o -> id
        <$< p2pThrottlingRate ..: "global" % o
        <*< p2pThrottlingPutPeerRate ..: "putPeer" % o

pP2pThrottlingConfig :: MParser P2pThrottlingConfig
pP2pThrottlingConfig = id
  <$< p2pThrottlingRate .:: option auto
    % long "p2p-throttle-global"
    <> help "Set the global rate limiting on the P2P API in requests per second (with 100x burst)."
    <> metavar "FLOAT"
  <*< p2pThrottlingPutPeerRate .:: option auto
    % long "p2p-throttle-put-peer"
    <> help "Set the rate limiting on the P2P API in requests per second (with 100x burst)."
    <> metavar "FLOAT"

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
    <*< serviceApiPayloadBatchLimit .:: fmap PayloadBatchLimit . option auto
        % prefixLong service "payload-batch-limit"
        <> suffixHelp service "upper limit for the size of payload batches on the service API"
  where
    service = Just "service"

-- -------------------------------------------------------------------------- --
-- Payload Provider Config

data PayloadProviderType
    = Pact
    | Evm
    deriving (Show, Eq, Ord, Generic)

instance HasTextRepresentation PayloadProviderType where
    toText Pact = "pact"
    toText Evm = "evm"

    fromText "pact" = return Pact
    fromText "Pact" = return Pact
    fromText "evm" = return Evm
    fromText "EVM" = return Evm
    fromText t = throwM $ TextFormatException $ "failed to parse payload provider type " <> sshow t

instance FromJSON PayloadProviderType where
    parseJSON = parseJsonFromText "PayloadProviderType"

instance ToJSON PayloadProviderType where
    toEncoding = toEncoding . toText
    toJSON = toJSON . toText

data PayloadProviderConfig = PayloadProviderConfig
    { _payloadProviderConfigChain :: !ChainId
    , _payloadProviderConfigHostAddress :: !HostAddress
    , _payloadProviderConfigType :: !PayloadProviderType
    , _payloadProviderXAuth :: !Value
        -- ^ provider specific authentication information in JSON format, that
        -- is passed to the provider plugin
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON PayloadProviderConfig where
    toJSON o = object
        [ "chain" .= _payloadProviderConfigChain o
        , "hostaddress" .= _payloadProviderConfigHostAddress o
        , "type" .= _payloadProviderConfigType o
        , "x-auth" .= _payloadProviderXAuth o
        ]

instance FromJSON PayloadProviderConfig where
    parseJSON = withObject "PayloadProviderConfig" $ \o -> PayloadProviderConfig
        <$> o .: "chain"
        <*> o .: "hostaddress"
        <*> o .: "type"
        <*> o .: "x-auth"

-- -------------------------------------------------------------------------- --
-- History Limit

newtype HistoryLimit = HistoryLimit Natural
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Num, ToJSON, FromJSON)

-- -------------------------------------------------------------------------- --
-- Beacon Configuration

data BeaconConfiguration = BeaconConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configHistoryLimit :: !(Maybe HistoryLimit)
    , _configMining :: !MiningConfig
    , _configP2p :: !P2pConfiguration
    , _configP2pThrottling :: !P2pThrottlingConfig
    , _configServiceApi :: !ServiceApiConfig
    , _configPayloadProviders :: ![PayloadProviderConfig]
    } deriving (Show, Eq, Generic)

makeLenses ''BeaconConfiguration

instance HasChainwebVersion BeaconConfiguration where
    _chainwebVersion = _configChainwebVersion
    {-# INLINE _chainwebVersion #-}

validateBeaconConfiguration :: ConfigValidation BeaconConfiguration []
validateBeaconConfiguration c = do
    validateMinerConfig (_configChainwebVersion c) (_configMining c)
    unless (c ^. chainwebVersion . versionDefaults . disablePeerValidation) $
        validateP2pConfiguration (_configP2p c)
    validateChainwebVersion (_configChainwebVersion c)
    validatePayloadProvidersConfig (_configPayloadProviders c)

validateChainwebVersion :: ConfigValidation ChainwebVersion []
validateChainwebVersion v = unless (isDevelopment || elem v knownVersions) $
    throwError $ T.unwords
        [ "Specifying version properties is only legal with chainweb-version"
        , "set to recap-development or development, but version is set to"
        , sshow (_versionName v)
        ]
  where
    isDevelopment = _versionCode v `elem` [_versionCode dv | dv <- [recapDevnet, devnet]]

validatePayloadProvidersConfig :: ConfigValidation [PayloadProviderConfig] []
validatePayloadProvidersConfig = error "validatePayloadProviders: TODO"

defaultBeaconConfiguration :: ChainwebVersion -> BeaconConfiguration
defaultBeaconConfiguration v = BeaconConfiguration
    { _configChainwebVersion = v
    , _configHistoryLimit = Nothing
    , _configMining = defaultMining
    , _configP2p = defaultP2pConfiguration
    , _configP2pThrottling = defaultP2pThrottlingConfig
    , _configServiceApi = defaultServiceApiConfig
    , _configPayloadProviders = []
    }

instance ToJSON BeaconConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _versionName (_configChainwebVersion o)
        , "historyLimit" .= _configHistoryLimit o
        , "mining" .= _configMining o
        , "p2p" .= _configP2p o
        , "p2pThrottling" .= _configP2pThrottling o
        , "serviceApi" .= _configServiceApi o
        , "payloadProviders" .= _configPayloadProviders o
        ]

instance FromJSON BeaconConfiguration where
    parseJSON = fmap ($ defaultBeaconConfiguration Mainnet01) . parseJSON

instance FromJSON (BeaconConfiguration -> BeaconConfiguration) where
    parseJSON = withObject "BeaconConfiguration" $ \o -> id
        <$< setProperty configChainwebVersion "chainwebVersion"
            (findKnownVersion <=< parseJSON) o
        <*< configHistoryLimit ..: "historyLimit" % o
        <*< configMining %.: "mining" % o
        <*< configP2p %.: "p2p" % o
        <*< configP2pThrottling %.: "p2pThrottling" % o
        <*< configServiceApi %.: "serviceApi" % o
        <*< configPayloadProviders .from leftMonoidalUpdate %.: "payloadProviders" % o

pBeaconConfiguration :: MParser BeaconConfiguration
pBeaconConfiguration = id
    <$< configChainwebVersion %:: parseVersion
    <*< configHistoryLimit .:: fmap (Just . HistoryLimit) . option auto
        % long "history-limit"
        <> help "Minimum history in block heights that is kept by this node"
    <*< configMining %:: pMiningConfig
    <*< configP2p %:: pP2pConfiguration
    <*< configP2pThrottling %:: pP2pThrottlingConfig
    <*< configServiceApi %:: pServiceApiConfig

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
