{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
, pP2pConfiguration
) where

import Configuration.Utils

import Control.Lens hiding ((.=))

import qualified Data.Text as T

import GHC.Generics (Generic)

import Numeric.Natural

import Test.QuickCheck

import Test.QuickCheck.Instances ({- Arbitrary V4.UUID -})

-- Internal imports

import Chainweb.HostAddress
import Chainweb.RestAPI.NetworkID
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version (ChainwebVersion(..))

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
    }
    deriving (Show, Eq, Generic)

makeLenses ''P2pConfiguration

instance Arbitrary P2pConfiguration where
    arbitrary = P2pConfiguration
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary

-- | Default known peers
-- by default we start with an empty list. The hard-coded bootstrap peer
-- infos depend on the chainweb version which may change depending on
-- the configuration. So we have to wait until all configuration parsing
-- is complete
defaultKnownPeers ::  ChainwebVersion -> [PeerInfo]
defaultKnownPeers Mainnet01 =
  [ bootstrap "us-e1.chainweb.com"
  , bootstrap "us-e2.chainweb.com"
  , bootstrap "us-e3.chainweb.com"
  
  , bootstrap "us-w1.chainweb.com"
  , bootstrap "us-w2.chainweb.com"
  , bootstrap "us-w3.chainweb.com"
  
  , bootstrap "fr1.chainweb.com"
  , bootstrap "fr2.chainweb.com"
  , bootstrap "fr3.chainweb.com"
  
  , bootstrap "jp1.chainweb.com"
  , bootstrap "jp2.chainweb.com"
  , bootstrap "jp3.chainweb.com"
  ]
  where
    bootstrap h = PeerInfo Nothing (unsafeHostAddressFromText (h <> ":443"))
defaultKnownPeers _ = mempty

-- | These are acceptable values for both test and production chainwebs.
--
defaultP2pConfiguration ::  ChainwebVersion -> P2pConfiguration
defaultP2pConfiguration v = P2pConfiguration
    { _p2pConfigPeer = defaultPeerConfig
    , _p2pConfigMaxSessionCount = 10
    , _p2pConfigMaxPeerCount = 50
    , _p2pConfigSessionTimeout = 60
    , _p2pConfigKnownPeers = defaultKnownPeers v
    , _p2pConfigPeerDbFilePath = Nothing
    , _p2pConfigIgnoreBootstrapNodes = False
    }

instance ToJSON P2pConfiguration where
    toJSON o = object
        [ "peer" .= _p2pConfigPeer o
        , "maxSessionCount" .= _p2pConfigMaxSessionCount o
        , "maxPeerCount" .= _p2pConfigMaxPeerCount o
        , "sessionTimeout" .= _p2pConfigSessionTimeout o
        , "peers" .= _p2pConfigKnownPeers o
        , "peerDbFilePath" .= _p2pConfigPeerDbFilePath o
        , "ignoreBootstrapNodes" .= _p2pConfigIgnoreBootstrapNodes o
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

instance FromJSON P2pConfiguration where
    parseJSON = withObject "P2pExampleConfig" $ \o -> P2pConfiguration
        <$> o .: "peer"
        <*> o .: "maxSessionCount"
        <*> o .: "maxPeerCount"
        <*> o .: "sessionTimeout"
        <*> o .: "peers"
        <*> o .: "peerDbFilePath"
        <*> o .: "ignoreBootstrapNodes"

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
  where
    net = T.unpack . networkIdToText <$> networkId

    pKnownPeerInfo = textOption
        % prefixLong net "known-peer-info"
        <> suffixHelp net
            "peer info that is added to the list of known peers. This option can be used multiple times."
        <> metavar "[<PEERID>@]<HOSTADDRESS>"
