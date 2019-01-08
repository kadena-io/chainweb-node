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

import Chainweb.RestAPI.NetworkID
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- P2P Configuration

-- | Configuration of the Network
--
-- TODO: add ChainwebVersion?
--
data P2pConfiguration = P2pConfiguration
    { _p2pConfigPeer:: !PeerConfig
        -- ^ the local peer.

    , _p2pConfigMaxSessionCount :: !Natural
        -- ^ the number of active peers.

    , _p2pConfigMaxPeerCount :: !Natural
        -- ^ total number of peers

    , _p2pConfigSessionTimeout :: !Seconds
        -- ^ interval at which peers are rotated out of the active set

    , _p2pConfigKnownPeers :: ![PeerInfo]
        -- ^ List of know peers. Must not be empty.

    , _p2pConfigPeerDbFilePath :: !(Maybe FilePath)
        -- ^ the path where the peer database is persisted
    }
    deriving (Show, Eq, Generic)

makeLenses ''P2pConfiguration

instance Arbitrary P2pConfiguration where
    arbitrary = P2pConfiguration
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary

defaultP2pConfiguration :: ChainwebVersion -> P2pConfiguration
defaultP2pConfiguration Test = P2pConfiguration
    { _p2pConfigPeer = defaultPeerConfig
    , _p2pConfigMaxSessionCount = 10
    , _p2pConfigMaxPeerCount = 50
    , _p2pConfigSessionTimeout = 60
    , _p2pConfigKnownPeers = bootstrapPeerInfos Test
    , _p2pConfigPeerDbFilePath = Nothing
    }

defaultP2pConfiguration _ = error "TODO not implemented"

instance ToJSON P2pConfiguration where
    toJSON o = object
        [ "peer" .= _p2pConfigPeer o
        , "maxSessionCount" .= _p2pConfigMaxSessionCount o
        , "maxPeerCount" .= _p2pConfigMaxPeerCount o
        , "sessionTimeout" .= _p2pConfigSessionTimeout o
        , "peers" .= _p2pConfigKnownPeers o
        , "peerDbFilePath" .= _p2pConfigPeerDbFilePath o
        ]

instance FromJSON (P2pConfiguration -> P2pConfiguration) where
    parseJSON = withObject "P2pConfiguration" $ \o -> id
        <$< p2pConfigPeer %.: "peer" % o
        <*< p2pConfigMaxSessionCount ..: "maxSessionCount" % o
        <*< p2pConfigMaxPeerCount ..: "maxPeerCount" % o
        <*< p2pConfigSessionTimeout ..: "sessionTimeout" % o
        <*< p2pConfigKnownPeers . from leftMonoidalUpdate %.: "peers" % o
        <*< p2pConfigPeerDbFilePath ..: "peerDbFilePath" % o

instance FromJSON P2pConfiguration where
    parseJSON = withObject "P2pExampleConfig" $ \o -> P2pConfiguration
        <$> o .: "peer"
        <*> o .: "maxSessionCount"
        <*> o .: "maxPeerCount"
        <*> o .: "sessionTimeout"
        <*> o .: "peers"
        <*> o .: "peerDbFilePath"

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
    <*< p2pConfigKnownPeers %:: pLeftMonoidalUpdate (pure <$> pPeerInfoCompact net)
    <*< p2pConfigPeerDbFilePath .:: fmap Just % fileOption
        % prefixLong net "p2p-peer-database-filepath"
        <> suffixHelp net "file where the peer database is stored"
  where
    net = T.unpack . networkIdToText <$> networkId

