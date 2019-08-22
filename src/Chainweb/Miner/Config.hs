{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Miner.Config
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Chainweb.Miner.Config where

import Configuration.Utils

import Control.Lens hiding ((.=))

import Data.Set (Set)
import qualified Data.Set as S

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

-- internal modules

import Chainweb.HostAddress (HostAddress)
import Chainweb.Miner
import Chainweb.Utils (textOption)

---

newtype MinerCount = MinerCount { _minerCount :: Natural }
    deriving (Eq, Ord, Show)

makeLenses ''MinerCount

data MinerConfig = MinerConfig
    { _configTestMiners :: !MinerCount
    , _configMinerInfo :: !Miner
    , _configRemoteMiners :: !(Set HostAddress)
    }
    deriving (Show, Eq, Generic)

makeLenses ''MinerConfig

defaultMinerConfig :: MinerConfig
defaultMinerConfig = MinerConfig
    { _configTestMiners = MinerCount 10
    , _configMinerInfo = noMiner
    , _configRemoteMiners = S.empty
    }

instance ToJSON MinerConfig where
    toJSON o = object
        [ "testMiners" .= _minerCount (_configTestMiners o)
        , "minerInfo" .= _configMinerInfo o
        , "remoteMiners" .= _configRemoteMiners o
        ]

instance FromJSON (MinerConfig -> MinerConfig) where
    parseJSON = withObject "MinerConfig" $ \o -> id
        <$< (configTestMiners . minerCount) ..: "testMiners" % o
        <*< configMinerInfo ..: "minerInfo" % o
        <*< configRemoteMiners ..: "remoteMiners" % o

pMinerConfig :: MParser MinerConfig
pMinerConfig = id
    <$< (configTestMiners . minerCount) .:: option auto
        % long "test-miners"
        <> short 'm'
        <> help "testing only: number of known miner nodes"
    <*< configRemoteMiners %:: pLeftMonoidalUpdate (S.singleton <$> pRemoteMiner)
  where
    pRemoteMiner = textOption
        % long "remote-miner"
        <> help "Remote address of a process that obeys the Chainweb Mining API. This option can be used multiple times."
        <> metavar "<HOSTNAME:PORT>"
