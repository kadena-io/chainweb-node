{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Mempool.P2pConfig
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- P2p Configuration for the Mempool.
--
module Chainweb.Mempool.P2pConfig
( MempoolP2pConfig(..)
, mempoolP2pConfigMaxSessionCount
, mempoolP2pConfigSessionTimeout
, mempoolP2pConfigPollInterval
, defaultMempoolP2pConfig
, pMempoolP2pConfig
) where

import Configuration.Utils

import Control.Lens.TH

import GHC.Generics

import Numeric.Natural

-- internal modules

import Chainweb.Time
import Chainweb.Utils

data MempoolP2pConfig = MempoolP2pConfig
    { _mempoolP2pConfigMaxSessionCount :: !Natural
        -- ^ max session count
    , _mempoolP2pConfigSessionTimeout :: !Seconds
        -- ^ timeout in seconds
    , _mempoolP2pConfigPollInterval :: !Seconds
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''MempoolP2pConfig

defaultMempoolP2pConfig :: MempoolP2pConfig
defaultMempoolP2pConfig = MempoolP2pConfig
    { _mempoolP2pConfigMaxSessionCount = 6
    , _mempoolP2pConfigSessionTimeout = 300
    , _mempoolP2pConfigPollInterval = 30
    }

instance ToJSON MempoolP2pConfig where
    toJSON o = object
        [ "maxSessionCount" .= _mempoolP2pConfigMaxSessionCount o
        , "sessionTimeout" .= _mempoolP2pConfigSessionTimeout o
        , "pollInterval" .= _mempoolP2pConfigPollInterval o
        ]

instance FromJSON (MempoolP2pConfig -> MempoolP2pConfig) where
    parseJSON = withObject "MempoolP2pConfig" $ \o -> id
        <$< mempoolP2pConfigMaxSessionCount ..: "maxSessionCount" % o
        <*< mempoolP2pConfigSessionTimeout ..: "sessionTimeout" % o
        <*< mempoolP2pConfigPollInterval ..: "pollInterval" % o

instance FromJSON MempoolP2pConfig where
    parseJSON = withObject "P2pExampleConfig" $ \o -> MempoolP2pConfig
        <$> o .: "maxSessionCount"
        <*> o .: "sessionTimeout"
        <*> o .: "pollInterval"

pMempoolP2pConfig :: MParser MempoolP2pConfig
pMempoolP2pConfig = id
    <$< mempoolP2pConfigMaxSessionCount .:: option auto
        % long "mempool-p2p-max-session-count"
        <> help "maximum number of sessions that are active at any time"
    <*< mempoolP2pConfigSessionTimeout .:: textOption
        % long "mempool-p2p-session-timeout"
        <> help "timeout for sessions in seconds"
    <*< mempoolP2pConfigPollInterval .:: textOption
        % long "mempool-p2p-poll-interval"
        <> help "poll interval for synchronizing mempools in seconds"
