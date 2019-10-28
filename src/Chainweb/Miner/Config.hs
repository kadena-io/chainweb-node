{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Miner.Config
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Chainweb.Miner.Config
( MinerConfig(..)
, MinerCount(..)
, configTestMiners
, configMinerInfo
, defaultMinerConfig
, validateMinerConfig
, pMinerConfig
) where

import Configuration.Utils

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except (throwError)

import Data.Default

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

import Pact.Types.Names
import Pact.Types.Term (KeySet(..))

-- internal modules

import Chainweb.Miner.Pact (Miner(..), MinerKeys(..), minerId)

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
        -- hidden configuration option that is only used in testing
    , _configMinerInfo = invalidMiner
    }
  where
    invalidMiner = Miner
        ""
        (MinerKeys (KeySet [] (Name $ BareName "keys-all" def)))

-- configTestMiner is only used for testing and hidden from the output
-- of --printConfig
--
instance ToJSON MinerConfig where
    toJSON o = object
        [ "minerInfo" .= _configMinerInfo o
        ]

instance FromJSON (MinerConfig -> MinerConfig) where

    parseJSON = withObject "MinerConfig" $ \o -> id
        <$< (configTestMiners . minerCount) ..: "testMiners" % o
        <*< configMinerInfo ..: "minerInfo" % o

-- TODO Options for parsing `Miner` on the command line.
--
pMinerConfig :: MParser MinerConfig
pMinerConfig = id
    <$< (configTestMiners . minerCount) .:: option auto
        % long "test-miners"
        <> short 'm'
        <> hidden
        <> internal

validateMinerConfig :: ConfigValidation MinerConfig l
validateMinerConfig c =
    when (view (configMinerInfo . minerId) c == "")
        $ throwError "Mining is enabled but no miner id is configured"

