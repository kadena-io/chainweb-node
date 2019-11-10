{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
( MiningConfig(..)
, defaultMining
, CoordinationConfig(..)
, NodeMiningConfig(..)
) where

import Configuration.Utils

import Data.Generics.Product.Fields (field)
import qualified Data.HashSet as HS

import GHC.Generics (Generic)

import Pact.Types.Term (mkKeySet)

-- internal modules

import Chainweb.Miner.Pact (Miner(..), MinerId, MinerKeys(..))

---

-- validateMinerConfig :: ConfigValidation MinerConfig l
-- validateMinerConfig c =
--     when (view (configMinerInfo . minerId) c == "")
--         $ throwError "Mining is enabled but no miner id is configured"

data MiningConfig = MiningConfig
    { _miningCoordination :: !CoordinationConfig
    , _miningInNode :: !NodeMiningConfig }
    deriving stock (Eq, Show, Generic)

instance ToJSON MiningConfig where
    toJSON o = object
        [ "coordination" .= _miningCoordination o
        , "nodeMining" .= _miningInNode o ]

instance FromJSON (MiningConfig -> MiningConfig) where
    parseJSON = withObject "MiningConfig" $ \o -> id
        <$< field @"_miningCoordination" %.: "coordination" % o
        <*< field @"_miningInNode" %.: "nodeMining" % o

defaultMining :: MiningConfig
defaultMining = MiningConfig
    { _miningCoordination = defaultCoordination
    , _miningInNode = defaultNodeMining }

data CoordinationConfig = CoordinationConfig
    { _coordinationEnabled :: !Bool
    , _coordinationMode :: !CoordinationMode
    , _coordinationMiners :: !(HS.HashSet MinerId) }
    deriving stock (Eq, Show, Generic)

instance ToJSON CoordinationConfig where
    toJSON o = object
        [ "enabled" .= _coordinationEnabled o
        , "mode" .= _coordinationMode o
        , "miners" .= _coordinationMiners o ]

instance FromJSON (CoordinationConfig -> CoordinationConfig) where
    parseJSON = withObject "CoordinationConfig" $ \o -> id
        <$< field @"_coordinationEnabled" ..: "enabled" % o
        <*< field @"_coordinationMode" ..: "mode" % o
        <*< field @"_coordinationMiners" ..: "miners" % o

defaultCoordination :: CoordinationConfig
defaultCoordination = CoordinationConfig
    { _coordinationEnabled = False
    , _coordinationMode = Private
    , _coordinationMiners = mempty }

data CoordinationMode = Public | Private
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data NodeMiningConfig = NodeMiningConfig
    { _nodeMiningEnabled :: !Bool
    , _nodeMiner :: !Miner }
    deriving stock (Eq, Show, Generic)

instance ToJSON NodeMiningConfig where
    toJSON o = object
        [ "enabled" .= _nodeMiningEnabled o
        , "miner" .= _nodeMiner o ]

instance FromJSON (NodeMiningConfig -> NodeMiningConfig) where
    parseJSON = withObject "NodeMiningConfig" $ \o -> id
        <$< field @"_nodeMiningEnabled" ..: "enabled" % o
        <*< field @"_nodeMiner" ..: "miner" % o

defaultNodeMining :: NodeMiningConfig
defaultNodeMining = NodeMiningConfig
    { _nodeMiningEnabled = False
    , _nodeMiner = invalidMiner }
  where
    invalidMiner = Miner "" . MinerKeys $ mkKeySet [] "keys-all"
