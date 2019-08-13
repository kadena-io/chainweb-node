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

import Control.Concurrent (threadDelay)

import Numeric.Natural (Natural)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.CutDB (CutDb)
import Chainweb.Difficulty (BlockRate(..), blockRate)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config (MinerConfig(..), MinerCount(..))
import Chainweb.Miner.Coordinator (mining)
import Chainweb.Miner.Core (mine, usePowHash)
import Chainweb.NodeId (NodeId)
import Chainweb.Payload.PayloadStore
import Chainweb.Time (Seconds(..))
import Chainweb.Utils (EnableConfig(..), int)
import Chainweb.Version (ChainwebVersion(..), order, _chainGraph)

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

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
localTest :: MWC.GenIO -> MinerCount -> BlockHeader -> IO BlockHeader
localTest gen miners bh = MWC.geometric1 t gen >>= threadDelay >> pure bh
  where
    v :: ChainwebVersion
    v = _blockChainwebVersion bh

    t :: Double
    t = int graphOrder / (int (_minerCount miners) * meanBlockTime * 1000000)

    graphOrder :: Natural
    graphOrder = order $ _chainGraph v

    meanBlockTime :: Double
    meanBlockTime = case blockRate v of
        Just (BlockRate (Seconds n)) -> int n
        Nothing -> error $ "No BlockRate available for given ChainwebVersion: " <> show v

-- | A single-threaded in-process Proof-of-Work mining loop.
--
localPOW :: ChainwebVersion -> BlockHeader -> IO BlockHeader
localPOW v = usePowHash v mine

-- remoteMining :: BlockHeader -> IO BlockHeader
-- remoteMining = undefined  -- TODO!
