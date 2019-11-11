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
-- TODO
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
, p2pConfigPeerDbFilePath
, p2pConfigIgnoreBootstrapNodes
, defaultP2pConfiguration
, validateP2pConfiguration
, pP2pConfiguration
) where

import Configuration.Utils
import Configuration.Utils.Validation

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer

import qualified Data.Text as T

import GHC.Generics (Generic)

import Numeric.Natural

import Test.QuickCheck

import Test.QuickCheck.Instances ({- Arbitrary V4.UUID -})

-- Internal imports

import Chainweb.RestAPI.NetworkID
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

    , _p2pConfigPeerDbFilePath :: !(Maybe FilePath)
        -- ^ the path where the peer database is persisted

    , _p2pConfigIgnoreBootstrapNodes :: !Bool
        -- ^ ignore builtin bootstrap nodes.

    , _p2pConfigPrivate :: !Bool
        -- ^ make this node private, so that it only communicates with the
        -- initially configured known peers. Use this option with care, because
        -- it may result in networks that are not well connected with the
        -- overall consensus.
    }
    deriving (Show, Eq, Generic)

makeLenses ''P2pConfiguration

instance Arbitrary P2pConfiguration where
    arbitrary = P2pConfiguration
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary

-- | These are acceptable values for both test and production chainwebs.
--
defaultP2pConfiguration :: P2pConfiguration
defaultP2pConfiguration = P2pConfiguration
    { _p2pConfigPeer = defaultPeerConfig
    , _p2pConfigMaxSessionCount = 10
    , _p2pConfigMaxPeerCount = 50
    , _p2pConfigSessionTimeout = 60
    , _p2pConfigKnownPeers = mempty
        -- by default we start with an empty list. The hard-coded bootstrap peer
        -- infos depend on the chainweb version which may change depending on
        -- the configuration. So we have to wait until all configuration parsing
        -- is complete

    , _p2pConfigPeerDbFilePath = Nothing
    , _p2pConfigIgnoreBootstrapNodes = False
    , _p2pConfigPrivate = False
    }

validateP2pConfiguration :: Applicative a => ConfigValidation P2pConfiguration a
validateP2pConfiguration c = do
    validatePeerConfig $ _p2pConfigPeer c

    when (_p2pConfigIgnoreBootstrapNodes c && null (_p2pConfigKnownPeers c)) $ throwError
        $ "Default bootstrap nodes are ignored and no known peers are configured. This node won't be able to communicate with the network"

    when (_p2pConfigPrivate c && null (_p2pConfigKnownPeers c)) $ tell
        $ pure "This node is configured to communicate only with the default bootstrap nodes"

    mapM_ (validateDirectory "peerDbFilePath") (_p2pConfigPeerDbFilePath c)

    validateRange "sessionTimeout" (60 {- 1 min -}, 900 {- 15 min -}) (_p2pConfigSessionTimeout c)

    when (_p2pConfigSessionTimeout c < 120) $ tell
        $ pure "This node is configured with a p2p session timeout of less than 120. This causes network overhead for creating new sessions. A connection timeout between 180 and 600 seconds is recommended."

    validateRange "maxSessionCount" (2, 30) (_p2pConfigMaxSessionCount c)

    when (_p2pConfigMaxSessionCount c < 5) $ tell
        $ pure "This node is configured to have a maximum session count of less than 5. This will limit the ability of this node to communicate with the rest of the network. A max session count between 10 and 20 is adviced"

    when (_p2pConfigMaxSessionCount c > 50) $ tell
        $ pure "This node is configured with a maximum session count of more than 50. This may put a high load on the network stack of the node and may cause connectivity problems. A max session count between 10 and 20 is adviced"

instance ToJSON P2pConfiguration where
    toJSON o = object
        [ "peer" .= _p2pConfigPeer o
        , "maxSessionCount" .= _p2pConfigMaxSessionCount o
        , "maxPeerCount" .= _p2pConfigMaxPeerCount o
        , "sessionTimeout" .= _p2pConfigSessionTimeout o
        , "peers" .= _p2pConfigKnownPeers o
        , "peerDbFilePath" .= _p2pConfigPeerDbFilePath o
        , "ignoreBootstrapNodes" .= _p2pConfigIgnoreBootstrapNodes o
        , "private" .= _p2pConfigPrivate o
        ]

instance FromJSON (P2pConfiguration -> P2pConfiguration) where
    parseJSON = withObject "P2pConfiguration" $ \o -> id
        <$< p2pConfigPeer %.: "peer" % o
        <*< p2pConfigMaxSessionCount ..: "maxSessionCount" % o
        <*< p2pConfigMaxPeerCount ..: "maxPeerCount" % o
        <*< p2pConfigSessionTimeout ..: "sessionTimeout" % o
        <*< p2pConfigKnownPeers . from leftMonoidalUpdate %.: "peers" % o
        <*< p2pConfigPeerDbFilePath ..: "peerDbFilePath" % o
        <*< p2pConfigIgnoreBootstrapNodes ..: "ignoreBootstrapNodes" % o
        <*< p2pConfigPrivate ..: "private" % o

instance FromJSON P2pConfiguration where
    parseJSON = withObject "P2pExampleConfig" $ \o -> P2pConfiguration
        <$> o .: "peer"
        <*> o .: "maxSessionCount"
        <*> o .: "maxPeerCount"
        <*> o .: "sessionTimeout"
        <*> o .: "peers"
        <*> o .: "peerDbFilePath"
        <*> o .: "ignoreBootstrapNodes"
        <*> o .: "private"

pP2pConfiguration :: Maybe NetworkId -> MParser P2pConfiguration
pP2pConfiguration networkId = id
    <$< p2pConfigPeer %:: pPeerConfig (T.unpack . toText <$> networkId)
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
    <*< p2pConfigPeerDbFilePath .:: fmap Just % fileOption
        % prefixLong net "p2p-peer-database-filepath"
        <> suffixHelp net "file where the peer database is stored"
    <*< p2pConfigIgnoreBootstrapNodes .:: enableDisableFlag
        % prefixLong net "ignore-bootstrap-nodes"
        <> help ("when enabled the hard-coded bootstrap nodes for network are ignored")
    <*< p2pConfigPrivate .:: enableDisableFlag
        % prefixLong net "private"
        <> help ("when enabled this node becomes private and communicates only with the initially configured known peers")
  where
    net = T.unpack . networkIdToText <$> networkId

    pKnownPeerInfo = textOption
        % prefixLong net "known-peer-info"
        <> suffixHelp net
            "peer info that is added to the list of known peers. This option can be used multiple times."
        <> metavar "[<PEERID>@]<HOSTADDRESS>"
