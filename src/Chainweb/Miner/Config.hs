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

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

-- internal modules

import Chainweb.Miner.Pact (Miner, noMiner)

---

newtype MinerCount = MinerCount { _minerCount :: Natural }
    deriving (Eq, Ord, Show)

makeLenses ''MinerCount

data MinerConfig = MinerConfig
    { _configTestMiners :: !MinerCount
    , _configMinerInfo :: !Miner
    }
    deriving (Show, Eq, Generic)

makeLenses ''MinerConfig

defaultMinerConfig :: MinerConfig
defaultMinerConfig = MinerConfig
    { _configTestMiners = MinerCount 10
    , _configMinerInfo = noMiner
    }

instance ToJSON MinerConfig where
    toJSON o = object
        [ "testMiners" .= _minerCount (_configTestMiners o)
        , "minerInfo" .= _configMinerInfo o
        ]

instance FromJSON (MinerConfig -> MinerConfig) where
    parseJSON = withObject "MinerConfig" $ \o -> id
        <$< (configTestMiners . minerCount) ..: "testMiners" % o
        <*< configMinerInfo ..: "minerInfo" % o

-- TODO Test if setting `Miner` in config is respected!
pMinerConfig :: MParser MinerConfig
pMinerConfig = id
    <$< (configTestMiners . minerCount) .:: option auto
        % long "test-miners"
        <> short 'm'
        <> help "testing only: number of known miner nodes"
