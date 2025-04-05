{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.Miner.Config
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Mining configuration for a Chainweb node.
--
module Chainweb.Miner.Config
( MiningConfig(..)
, defaultMining
, pMiningConfig
, miningCoordination
, miningInNode
, miningMiner
, invalidMiner
, validateMinerConfig
, CoordinationConfig(..)
, pCoordinationConfig
, coordinationEnabled
, NodeMiningConfig(..)
, defaultNodeMining
, nodeMiningEnabled
, nodeTestMiners
, MinerCount(..)
) where

import Configuration.Utils

import Control.Lens (lens, view)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Writer (tell)
import Data.Set qualified as Set

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

import Options.Applicative

import Pact.JSON.Encode qualified as J

-- internal modules

import Pact.Core.Guards qualified as Pact

import Chainweb.Miner.Pact (Miner(..), MinerGuard(..), MinerId(..), minerId)
import Chainweb.Time
import Chainweb.Utils (hostArch, sshow, textOption)
import Chainweb.Version
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet04

-- -------------------------------------------------------------------------- --

-- | Strictly for testing.
--
newtype MinerCount = MinerCount { _minerCount :: Natural }
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromJSON)

-- -------------------------------------------------------------------------- --
-- Mining Config

validateMinerConfig :: ChainwebVersion -> ConfigValidation MiningConfig []
validateMinerConfig v c = do
    when (_nodeMiningEnabled nmc) $ do
        tell
            [ "In-node mining is enabled. This should only be used for testing"
            , "In order to use in-node mining, mining-coordination must be enabled, too"
            ]
        when (not (_coordinationEnabled cc))
            $ throwError "In-node mining is enabled but mining coordination is disabled"
        when (view minerId (_miningMiner c) == "")
            $ throwError "In-node Mining is enabled but no miner id is configured"

    when (_coordinationEnabled cc && isProd) $ do
        when (hostArch `notElem` supportedArchs) $ do
            throwError $ mconcat
                [ "Unsupported host architecture for mining on production networks: " <> sshow hostArch <> "."
                , " Supported architectures are " <> sshow supportedArchs
                ]
        when (view minerId (_miningMiner c) == "")
            $ throwError "Mining is enabled but no miner id is configured"
  where
    nmc = _miningInNode c
    cc = _miningCoordination c

    -- This is a heuristic and we are rather a little too restrictiv. In the
    -- future we may also consider uname -m and/or cpuinfo (including flags) here.
    --
    supportedArchs = [ "x86_64" ]
    isProd = v `elem` [Mainnet01, Testnet04]

-- | Full configuration for Mining.
--
data MiningConfig = MiningConfig
    { _miningCoordination :: !CoordinationConfig
    , _miningInNode :: !NodeMiningConfig
    , _miningMiner :: !Miner
    }
    deriving stock (Eq, Show)

miningCoordination :: Lens' MiningConfig CoordinationConfig
miningCoordination = lens _miningCoordination (\m c -> m { _miningCoordination = c })

miningInNode :: Lens' MiningConfig NodeMiningConfig
miningInNode = lens _miningInNode (\m c -> m { _miningInNode = c })

miningMiner :: Lens' MiningConfig Miner
miningMiner = lens _miningMiner (\m c -> m { _miningMiner = c })

instance ToJSON MiningConfig where
    toJSON o = object
        [ "coordination" .= _miningCoordination o
        , "nodeMining" .= _miningInNode o
        , "miner" .= J.toJsonViaEncode (_miningMiner o)
        ]

instance FromJSON (MiningConfig -> MiningConfig) where
    parseJSON = withObject "MiningConfig" $ \o -> id
        <$< miningCoordination %.: "coordination" % o
        <*< miningInNode %.: "nodeMining" % o
        <*< miningMiner ..: "miner" % o

instance FromJSON MiningConfig where
    parseJSON v = do
        f <- parseJSON v
        return $ f defaultMining

pMiningConfig :: MParser MiningConfig
pMiningConfig = id
    <$< miningCoordination %:: pCoordinationConfig
    <*< miningInNode %:: pNodeMiningConfig
    <*< miningMiner .:: pMiner ""

defaultMining :: MiningConfig
defaultMining = MiningConfig
    { _miningCoordination = defaultCoordination
    , _miningInNode = defaultNodeMining
    , _miningMiner = invalidMiner
    }

invalidMiner :: Miner
invalidMiner = Miner "" . MinerGuard $ Pact.GKeyset (Pact.KeySet mempty Pact.KeysAll)

-- -------------------------------------------------------------------------- --
-- Mining Coordination Config

-- | Configuration for Mining Coordination.
data CoordinationConfig = CoordinationConfig
    { _coordinationEnabled :: !Bool
      -- ^ Is mining coordination enabled? If not, the @/mining/@ won't even be
      -- present on the node.
    , _coordinationUpdateStreamTimeout :: !Seconds
        -- ^ the duration that an update stream is kept open in seconds
    , _coordinationPayloadRefreshDelay :: !(TimeSpan Micros)
        -- ^ the duration between payload refreshes in microseconds
    } deriving stock (Eq, Show, Generic)

coordinationEnabled :: Lens' CoordinationConfig Bool
coordinationEnabled = lens _coordinationEnabled (\m c -> m { _coordinationEnabled = c })

coordinationUpdateStreamTimeout :: Lens' CoordinationConfig Seconds
coordinationUpdateStreamTimeout =
    lens _coordinationUpdateStreamTimeout (\m c -> m { _coordinationUpdateStreamTimeout = c })

coordinationPayloadRefreshDelay :: Lens' CoordinationConfig (TimeSpan Micros)
coordinationPayloadRefreshDelay =
    lens _coordinationPayloadRefreshDelay (\m c -> m { _coordinationPayloadRefreshDelay = c })

instance ToJSON CoordinationConfig where
    toJSON o = object
        [ "enabled" .= _coordinationEnabled o
        , "updateStreamTimeout" .= _coordinationUpdateStreamTimeout o
        , "payloadRefreshDelay" .= _coordinationPayloadRefreshDelay o
        ]

instance FromJSON (CoordinationConfig -> CoordinationConfig) where
    parseJSON = withObject "CoordinationConfig" $ \o -> id
        <$< coordinationEnabled ..: "enabled" % o
        <*< coordinationUpdateStreamTimeout ..: "updateStreamTimeout" % o
        <*< coordinationPayloadRefreshDelay ..: "payloadRefreshDelay" % o

defaultCoordination :: CoordinationConfig
defaultCoordination = CoordinationConfig
    { _coordinationEnabled = False
    , _coordinationUpdateStreamTimeout = 240
    , _coordinationPayloadRefreshDelay = TimeSpan (Micros 15_000_000)
    }

pCoordinationConfig :: MParser CoordinationConfig
pCoordinationConfig = id
    <$< coordinationEnabled .:: enableDisableFlag
        % long "mining-coordination"
        <> help "whether to enable the mining coordination API"
    <*< coordinationUpdateStreamTimeout .:: jsonOption
        % long "mining-update-stream-timeout"
        <> help "duration that an update stream is kept open in seconds"
    <*< coordinationPayloadRefreshDelay .:: jsonOption
        % long "mining-payload-refresh-delay"
        <> help "frequency that the mining payload is refreshed"

pMiner :: String -> Parser Miner
pMiner prefix = pkToMiner <$> pPk
  where
    pkToMiner pk = Miner
        (MinerId $ "k:" <> Pact._pubKey pk)
        (MinerGuard $ Pact.GKeyset $ Pact.KeySet (Set.singleton pk) Pact.KeysAll)
    pPk = Pact.PublicKeyText <$> textOption
        % long (prefix <> "mining-public-key")
        <> help "public key of a miner in hex decimal encoding. The account name is the public key prefix by 'k:'. (This option can be provided multiple times.)"

-- -------------------------------------------------------------------------- --
-- Node Mining Config

data NodeMiningConfig = NodeMiningConfig
    { _nodeMiningEnabled :: !Bool
      -- ^ If enabled, this node will mine with a single CPU along with its
      -- other responsibilities.
    , _nodeTestMiners :: !MinerCount
      -- ^ Strictly for testing.
    } deriving stock (Eq, Show, Generic)

nodeMiningEnabled :: Lens' NodeMiningConfig Bool
nodeMiningEnabled = lens _nodeMiningEnabled (\m c -> m { _nodeMiningEnabled = c })

nodeTestMiners :: Lens' NodeMiningConfig MinerCount
nodeTestMiners = lens _nodeTestMiners (\m c -> m { _nodeTestMiners = c })

instance ToJSON NodeMiningConfig where
    toJSON o = object
        [ "enabled" .= _nodeMiningEnabled o
        ]

instance FromJSON (NodeMiningConfig -> NodeMiningConfig) where
    parseJSON = withObject "NodeMiningConfig" $ \o -> id
        <$< nodeMiningEnabled ..: "enabled" % o

pNodeMiningConfig :: MParser NodeMiningConfig
pNodeMiningConfig = id
    <$< nodeMiningEnabled .:: enableDisableFlag
        % long "node-mining"
        <> help "ONLY FOR TESTING NETWORKS: whether to enable in node mining"

defaultNodeMining :: NodeMiningConfig
defaultNodeMining = NodeMiningConfig
    { _nodeMiningEnabled = False
    , _nodeTestMiners = MinerCount 10
    }
