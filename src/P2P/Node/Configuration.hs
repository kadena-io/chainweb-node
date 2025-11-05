{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: P2P.Node.Configuration
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Configuration for Chainweb P2P network nodes.
--
module P2P.Node.Configuration
(
-- * P2P Configuration
  P2pConfiguration(..)
, p2pConfigPeer
, p2pConfigMaxSessionCount
, p2pConfigMaxPeerCount
, p2pConfigSessionTimeout
, p2pConfigKnownPeers
, p2pConfigIgnoreBootstrapNodes
, p2pConfigBootstrapReachability
, defaultP2pConfiguration
, validateP2pConfiguration
, pP2pConfiguration
, p2pRequestSizeLimit
) where

import Configuration.Utils
import Configuration.Utils.Validation

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Word

import GHC.Generics (Generic)

import Numeric.Natural

-- Internal imports

import Chainweb.Time
import Chainweb.Utils hiding (check)

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- P2P Configuration

-- | Configuration of the Network
--
-- TODO: add ChainwebVersion?
--
data P2pConfiguration = P2pConfiguration
    { _p2pConfigPeer :: !PeerConfig
        -- ^ the local peer.

    , _p2pConfigMaxSessionCount :: !Natural
        -- ^ the number of active peers.

    , _p2pConfigMaxPeerCount :: !Natural
        -- ^ total number of peers

    , _p2pConfigSessionTimeout :: !Seconds
        -- ^ interval at which peers are rotated out of the active set

    , _p2pConfigKnownPeers :: ![PeerInfo]
        -- ^ List of known peers. Must not be empty.

    , _p2pConfigIgnoreBootstrapNodes :: !Bool
        -- ^ ignore builtin bootstrap nodes.

    , _p2pConfigPrivate :: !Bool
        -- ^ make this node private, so that it only communicates with the
        -- initially configured known peers. Use this option with care, because
        -- it may result in networks that are not well connected with the
        -- overall consensus.

    , _p2pConfigBootstrapReachability :: !Double
        -- ^ the fraction of the bootstrap nodes that must be reachable and must
        -- be able to reach this node on startup. Default value
        -- is 0.5.

    , _p2pConfigTls :: !Bool
        -- ^ enable TLS. WARNING: is is an expert setting. Disabling this flag
        -- requires a particular setup of a proxy server that terminates TLS. A
        -- valid CA signed TLS certificate must be used by the proxy and also
        -- provided in the chainweb-node configuration if flag is disabled.
        --
        -- The user must also ensure that the proxy sets a valid X-Peer-Addr
        -- response header.
    , _p2pConfigValidateSpec :: !Bool
        -- ^ enable OpenAPI specification validation for requests and responses.
        -- this will likely cause significant performance degradation.
    }
    deriving (Show, Eq, Generic)

-- | Despite the name, this is only a limit for the request _body_ size.
-- This is *not* configurable on a per-node basis, because nodes with a
-- different limit may become effectively partitioned from one another in the
-- network.
p2pRequestSizeLimit :: Word64
p2pRequestSizeLimit = 2 * 1024 * 1024 -- 2 MB

makeLenses ''P2pConfiguration

-- | These are acceptable values for both test and production chainwebs.
--
defaultP2pConfiguration :: P2pConfiguration
defaultP2pConfiguration = P2pConfiguration
    { _p2pConfigPeer = defaultPeerConfig
    , _p2pConfigMaxSessionCount = 10
    , _p2pConfigMaxPeerCount = 50
    , _p2pConfigSessionTimeout = 240
    , _p2pConfigKnownPeers = mempty
        -- by default we start with an empty list. The hard-coded bootstrap peer
        -- infos depend on the chainweb version which may change depending on
        -- the configuration. So we have to wait until all configuration parsing
        -- is complete

    , _p2pConfigIgnoreBootstrapNodes = False
    , _p2pConfigPrivate = False
    , _p2pConfigBootstrapReachability = 0.5
    , _p2pConfigTls = True
    , _p2pConfigValidateSpec = False
    }

validateP2pConfiguration :: Applicative a => ConfigValidation P2pConfiguration a
validateP2pConfiguration c = do
    validatePeerConfig $ _p2pConfigPeer c

    when (null (_p2pConfigKnownPeers c)) $ do
        if _p2pConfigPrivate c && _p2pConfigIgnoreBootstrapNodes c
        then tell $ pure "This node is configured to not communicate with any other nodes, including bootstrap nodes."

        else if _p2pConfigPrivate c
        then tell $ pure "This node is configured to communicate only with the default bootstrap nodes."

        else if _p2pConfigIgnoreBootstrapNodes c
        then tell $ pure "Default bootstrap nodes are ignored and no known peers are configured. This node won't be able to communicate with the network."

        else return ()

    validateRange "sessionTimeout" (60 {- 1 min -}, 900 {- 15 min -}) (_p2pConfigSessionTimeout c)

    when (_p2pConfigSessionTimeout c < 120) $ tell
        $ pure "This node is configured with a p2p session timeout of less than 120. This causes network overhead for creating new sessions. A connection timeout between 180 and 600 seconds is recommended."

    validateRange "maxSessionCount" (2, 30) (_p2pConfigMaxSessionCount c)

    when (_p2pConfigMaxSessionCount c < 3) $ tell
        $ pure "This node is configured to have a maximum session count of less than 5. This will limit the ability of this node to communicate with the rest of the network. A max session count between 5 and 15 is advised."

    when (_p2pConfigMaxSessionCount c > 30) $ throwError
        "This node is configured with a maximum session count of more than 30. This may put a high load on the network stack of the node and may cause connectivity problems. A max session count between 5 and 15 is advised."

    when (_p2pConfigBootstrapReachability c > 1) $ throwError
        "The bootstrap reachability factor must be a value between 0 and 1"

instance ToJSON P2pConfiguration where
    toJSON o = object $
        [ "peer" .= _p2pConfigPeer o
        , "maxSessionCount" .= _p2pConfigMaxSessionCount o
        , "maxPeerCount" .= _p2pConfigMaxPeerCount o
        , "sessionTimeout" .= _p2pConfigSessionTimeout o
        , "peers" .= _p2pConfigKnownPeers o
        , "ignoreBootstrapNodes" .= _p2pConfigIgnoreBootstrapNodes o
        , "private" .= _p2pConfigPrivate o
        , "bootstrapReachability" .= _p2pConfigBootstrapReachability o
        ]
        -- hidden: Do not print the default value.
        <> [ "tls" .= _p2pConfigTls o | not (_p2pConfigTls o) ]
        <> [ "validateSpec" .= _p2pConfigValidateSpec o | _p2pConfigValidateSpec o ]

instance FromJSON (P2pConfiguration -> P2pConfiguration) where
    parseJSON = withObject "P2pConfiguration" $ \o -> id
        <$< p2pConfigPeer %.: "peer" % o
        <*< p2pConfigMaxSessionCount ..: "maxSessionCount" % o
        <*< p2pConfigMaxPeerCount ..: "maxPeerCount" % o
        <*< p2pConfigSessionTimeout ..: "sessionTimeout" % o
        <*< p2pConfigKnownPeers . from leftMonoidalUpdate %.: "peers" % o
        <*< p2pConfigIgnoreBootstrapNodes ..: "ignoreBootstrapNodes" % o
        <*< p2pConfigPrivate ..: "private" % o
        <*< p2pConfigBootstrapReachability ..: "bootstrapReachability" % o
        <*< p2pConfigTls ..: "tls" % o
        <*< p2pConfigValidateSpec ..: "validateSpec" % o

instance FromJSON P2pConfiguration where
    parseJSON = withObject "P2pExampleConfig" $ \o -> P2pConfiguration
        <$> o .: "peer"
        <*> o .: "maxSessionCount"
        <*> o .: "maxPeerCount"
        <*> o .: "sessionTimeout"
        <*> o .: "peers"
        <*> o .: "ignoreBootstrapNodes"
        <*> o .: "private"
        <*> o .: "bootstrapReachability"
        <*> o .:? "tls" .!= True
        <*> o .:? "validateSpec" .!= False

pP2pConfiguration :: MParser P2pConfiguration
pP2pConfiguration = id
    <$< p2pConfigPeer %:: pPeerConfig (Just "p2p")
    <*< p2pConfigMaxSessionCount .:: option auto
        % prefixLong net "p2p-max-session-count"
        <> suffixHelp net "maximum number of sessions that are active at any time"
    <*< p2pConfigMaxPeerCount .:: option auto
        % prefixLong net "p2p-max-peer-count"
        <> suffixHelp net "maximum number of entries in the peer database"
    <*< p2pConfigSessionTimeout .:: textOption
        % prefixLong net "p2p-session-timeout"
        <> suffixHelp net "timeout for sessions in seconds"
    <*< p2pConfigKnownPeers %:: pLeftMonoidalUpdate
        (pure <$> pKnownPeerInfo)
    <*< p2pConfigIgnoreBootstrapNodes .:: enableDisableFlag
        % prefixLong net "ignore-bootstrap-nodes"
        <> help "when enabled the hard-coded bootstrap nodes for network are ignored"
    <*< p2pConfigPrivate .:: enableDisableFlag
        % prefixLong net "private"
        <> help "when enabled this node becomes private and communicates only with the initially configured known peers"
    <*< p2pConfigBootstrapReachability .:: option auto
        % prefixLong net "bootstrap-reachability"
        <> help "the fraction of bootstrap nodes that must be reachable at startup"
        <> metavar "[0,1]"
    <*< p2pConfigTls .:: enableDisableFlag
        % prefixLong net "tls"
        <> internal -- hidden option, only for expert use
    <*< p2pConfigValidateSpec .:: enableDisableFlag
        % prefixLong net "validate-spec"
        <> internal -- hidden option, only for expert use
  where
    net = Nothing

    pKnownPeerInfo = textOption
        % prefixLong net "known-peer-info"
        <> suffixHelp net
            "peer info that is added to the list of known peers. This option can be used multiple times."
        <> metavar "[<PEERID>@]<HOSTADDRESS>"
