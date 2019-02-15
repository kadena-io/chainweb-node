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

---

data MinerConfig = MinerConfig
    { _configMeanBlockTimeSeconds :: !Natural
        -- ^ The time in seconds that a Miner expects to mine a block on any
        -- given chain. Used in difficulty adjustment.
    , _configWindowWidth :: !Natural
        -- ^ Some N, where difficulty adjustment will be performed by the Miner
        -- after every N blocks on a given chain.
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''MinerConfig

defaultMinerConfig :: MinerConfig
defaultMinerConfig = MinerConfig
    { _configMeanBlockTimeSeconds = 10
    , _configWindowWidth = 5
       -- TODO: The ideal value for this is yet unknown. In general, the more
       -- often one readjusts, the less they'll be punished by sudden changes in
       -- either direction (say due to a spike in overall network hash power).
    }

instance ToJSON MinerConfig where
    toJSON o = object
        [ "meanBlockTimeSeconds" .= _configMeanBlockTimeSeconds o
        , "windowWidth" .= _configWindowWidth o
        ]

instance FromJSON (MinerConfig -> MinerConfig) where
    parseJSON = withObject "MinerConfig" $ \o -> id
        <$< configMeanBlockTimeSeconds ..: "meanBlockTimeSeconds" % o
        <*< configWindowWidth ..: "windowWidth" % o

pMinerConfig :: MParser MinerConfig
pMinerConfig = id
    <$< configMeanBlockTimeSeconds .:: option auto
        % long "mean-block-time"
        <> short 'b'
        <> help "mean time for mining a block seconds"
    <*< configWindowWidth .:: option auto
        % long "window-width"
        <> short 'w'
        <> help "the number of blocks to consider as an 'epoch window' during POW mining"
