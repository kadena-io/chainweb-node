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
, miningCoordination
, miningInNode
, CoordinationConfig(..)
, coordinationEnabled
, coordinationMode
, coordinationMiners
, NodeMiningConfig(..)
, nodeMiningEnabled
, nodeMiner
) where

import Configuration.Utils

import Control.Lens (lens)

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
    deriving stock (Eq, Show)

miningCoordination :: Lens' MiningConfig CoordinationConfig
miningCoordination = lens _miningCoordination (\m c -> m { _miningCoordination = c })

miningInNode :: Lens' MiningConfig NodeMiningConfig
miningInNode = lens _miningInNode (\m c -> m { _miningInNode = c })

instance ToJSON MiningConfig where
    toJSON o = object
        [ "coordination" .= _miningCoordination o
        , "nodeMining" .= _miningInNode o ]

instance FromJSON (MiningConfig -> MiningConfig) where
    parseJSON = withObject "MiningConfig" $ \o -> id
        <$< miningCoordination %.: "coordination" % o
        <*< miningInNode %.: "nodeMining" % o

defaultMining :: MiningConfig
defaultMining = MiningConfig
    { _miningCoordination = defaultCoordination
    , _miningInNode = defaultNodeMining }

data CoordinationConfig = CoordinationConfig
    { _coordinationEnabled :: !Bool
    , _coordinationMode :: !CoordinationMode
    , _coordinationMiners :: !(HS.HashSet MinerId) }
    deriving stock (Eq, Show, Generic)

coordinationEnabled :: Lens' CoordinationConfig Bool
coordinationEnabled = lens _coordinationEnabled (\m c -> m { _coordinationEnabled = c })

coordinationMode :: Lens' CoordinationConfig CoordinationMode
coordinationMode = lens _coordinationMode (\m c -> m { _coordinationMode = c })

coordinationMiners :: Lens' CoordinationConfig (HS.HashSet MinerId)
coordinationMiners = lens _coordinationMiners (\m c -> m { _coordinationMiners = c })

instance ToJSON CoordinationConfig where
    toJSON o = object
        [ "enabled" .= _coordinationEnabled o
        , "mode" .= _coordinationMode o
        , "miners" .= _coordinationMiners o ]

instance FromJSON (CoordinationConfig -> CoordinationConfig) where
    parseJSON = withObject "CoordinationConfig" $ \o -> id
        <$< coordinationEnabled ..: "enabled" % o
        <*< coordinationMode ..: "mode" % o
        <*< coordinationMiners ..: "miners" % o

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

nodeMiningEnabled :: Lens' NodeMiningConfig Bool
nodeMiningEnabled = lens _nodeMiningEnabled (\m c -> m { _nodeMiningEnabled = c })

nodeMiner :: Lens' NodeMiningConfig Miner
nodeMiner = lens _nodeMiner (\m c -> m { _nodeMiner = c })

instance ToJSON NodeMiningConfig where
    toJSON o = object
        [ "enabled" .= _nodeMiningEnabled o
        , "miner" .= _nodeMiner o ]

instance FromJSON (NodeMiningConfig -> NodeMiningConfig) where
    parseJSON = withObject "NodeMiningConfig" $ \o -> id
        <$< nodeMiningEnabled ..: "enabled" % o
        <*< nodeMiner ..: "miner" % o

defaultNodeMining :: NodeMiningConfig
defaultNodeMining = NodeMiningConfig
    { _nodeMiningEnabled = False
    , _nodeMiner = invalidMiner }
  where
    invalidMiner = Miner "" . MinerKeys $ mkKeySet [] "keys-all"
