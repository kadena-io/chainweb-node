{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
, validateMinerConfig
, CoordinationConfig(..)
, coordinationEnabled
, coordinationMode
, coordinationMiners
, CoordinationMode(..)
, NodeMiningConfig(..)
, defaultNodeMining
, nodeMiningEnabled
, nodeMiner
, nodeTestMiners
, MinerCount(..)
) where

import Configuration.Utils

import Control.Lens (lens, view)
import Control.Monad (when)
import Control.Monad.Except (throwError)

import qualified Data.HashSet as HS

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

import Pact.Types.Term (mkKeySet)

-- internal modules

import Chainweb.Miner.Pact (Miner(..), MinerId, MinerKeys(..), minerId)

---

newtype MinerCount = MinerCount { _minerCount :: Natural }
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromJSON)

validateMinerConfig :: ConfigValidation MiningConfig l
validateMinerConfig c =
    when (_nodeMiningEnabled nmc && view minerId (_nodeMiner nmc) == "")
        $ throwError "In-node Mining is enabled but no miner id is configured"
  where
    nmc = _miningInNode c

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
    , _coordinationMiners :: !(HS.HashSet MinerId)
    , _coordinationReqLimit :: !Int }
    deriving stock (Eq, Show, Generic)

coordinationEnabled :: Lens' CoordinationConfig Bool
coordinationEnabled = lens _coordinationEnabled (\m c -> m { _coordinationEnabled = c })

coordinationLimit :: Lens' CoordinationConfig Int
coordinationLimit = lens _coordinationReqLimit (\m c -> m { _coordinationReqLimit = c })

coordinationMode :: Lens' CoordinationConfig CoordinationMode
coordinationMode = lens _coordinationMode (\m c -> m { _coordinationMode = c })

coordinationMiners :: Lens' CoordinationConfig (HS.HashSet MinerId)
coordinationMiners = lens _coordinationMiners (\m c -> m { _coordinationMiners = c })

instance ToJSON CoordinationConfig where
    toJSON o = object
        [ "enabled" .= _coordinationEnabled o
        , "limit" .= _coordinationReqLimit o
        , "mode" .= _coordinationMode o
        , "miners" .= _coordinationMiners o ]

instance FromJSON (CoordinationConfig -> CoordinationConfig) where
    parseJSON = withObject "CoordinationConfig" $ \o -> id
        <$< coordinationEnabled ..: "enabled" % o
        <*< coordinationLimit ..: "limit" % o
        <*< coordinationMode ..: "mode" % o
        <*< coordinationMiners ..: "miners" % o

defaultCoordination :: CoordinationConfig
defaultCoordination = CoordinationConfig
    { _coordinationEnabled = False
    , _coordinationMode = Private
    , _coordinationMiners = mempty
    , _coordinationReqLimit = 1200 }

data CoordinationMode = Public | Private
    deriving stock (Eq, Show)

instance ToJSON CoordinationMode where
    toJSON Public = "public"
    toJSON Private = "private"

instance FromJSON CoordinationMode where
    parseJSON (String "private") = pure Private
    parseJSON (String "public") = pure Public
    parseJSON _ = fail "Failed to parse CoordinationMode. Expected 'private' or 'public'."

data NodeMiningConfig = NodeMiningConfig
    { _nodeMiningEnabled :: !Bool
    , _nodeMiner :: !Miner
    , _nodeTestMiners :: !MinerCount }
    deriving stock (Eq, Show, Generic)

nodeMiningEnabled :: Lens' NodeMiningConfig Bool
nodeMiningEnabled = lens _nodeMiningEnabled (\m c -> m { _nodeMiningEnabled = c })

nodeMiner :: Lens' NodeMiningConfig Miner
nodeMiner = lens _nodeMiner (\m c -> m { _nodeMiner = c })

nodeTestMiners :: Lens' NodeMiningConfig MinerCount
nodeTestMiners = lens _nodeTestMiners (\m c -> m { _nodeTestMiners = c })

instance ToJSON NodeMiningConfig where
    toJSON o = object
        [ "enabled" .= _nodeMiningEnabled o
        , "miner" .= _nodeMiner o ]

instance FromJSON (NodeMiningConfig -> NodeMiningConfig) where
    parseJSON = withObject "NodeMiningConfig" $ \o -> id
        <$< nodeMiningEnabled ..: "enabled" % o
        <*< nodeMiner ..: "miner" % o
        <*< nodeTestMiners ..: "testMiners" % o

defaultNodeMining :: NodeMiningConfig
defaultNodeMining = NodeMiningConfig
    { _nodeMiningEnabled = False
    , _nodeMiner = invalidMiner
    , _nodeTestMiners = MinerCount 10 }
  where
    invalidMiner = Miner "" . MinerKeys $ mkKeySet [] "keys-all"
