{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.MinerResources
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Chainweb.MinerResources
  ( MinerResources(..)
  , withMinerResources
  , runMiner
  ) where

import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.CutDB (CutDb)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config (MinerConfig(..), MinerCount(..))
import Chainweb.Miner.Coordinator (mining)
import Chainweb.Miner.Miners
import Chainweb.NodeId (NodeId)
import Chainweb.Payload.PayloadStore
import Chainweb.Utils (EnableConfig(..))
import Chainweb.Version (ChainwebVersion(..))

import Data.LogMessage (LogFunction)

-- -------------------------------------------------------------------------- --
-- Miner

data MinerResources logger cas = MinerResources
    { _minerResLogger :: !logger
    , _minerResNodeId :: !NodeId
    , _minerResCutDb :: !(CutDb cas)
    , _minerResConfig :: !MinerConfig
    }

withMinerResources
    :: logger
    -> EnableConfig MinerConfig
    -> NodeId
    -> CutDb cas
    -> (Maybe (MinerResources logger cas) -> IO a)
    -> IO a
withMinerResources logger (EnableConfig enabled conf) nid cutDb inner
    | not enabled = inner Nothing
    | otherwise = inner . Just $ MinerResources
        { _minerResLogger = logger
        , _minerResNodeId = nid
        , _minerResCutDb = cutDb
        , _minerResConfig = conf
        }

runMiner
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> MinerResources logger cas
    -> IO ()
runMiner v m = do
    gen <- MWC.createSystemRandom
    (chooseMiner gen v)
      (logFunction $ _minerResLogger m)
      conf
      (_minerResNodeId m)
      (_minerResCutDb m)
  where
    conf :: MinerConfig
    conf = _minerResConfig m

    miners :: MinerCount
    miners = _configTestMiners conf

    chooseMiner
        :: PayloadCas cas
        => MWC.GenIO
        -> ChainwebVersion
        -> LogFunction
        -> MinerConfig
        -> NodeId
        -> CutDb cas
        -> IO ()
    chooseMiner g Test{} = mining (localTest g miners)
    chooseMiner g TimedConsensus{} = mining (localTest g miners)
    chooseMiner _ PowConsensus{} = mining (localPOW v)
    chooseMiner g TimedCPM{} = mining (localTest g miners)
    chooseMiner _ Development = mining (localPOW v)
    chooseMiner _ Testnet00 = mining (localPOW v)
    chooseMiner _ Testnet01 = mining (localPOW v)
    chooseMiner _ Testnet02 = mining (localPOW v)
