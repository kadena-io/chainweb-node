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

-- internal modules

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.CutDB (CutDb)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config (MinerConfig)
import Chainweb.Miner.Core (mine, usePowHash)
import Chainweb.Miner.POW (powMiner)
import Chainweb.Miner.Test (testMiner)
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
runMiner v m = (chooseMiner v)
    (logFunction $ _minerResLogger m)
    (_minerResConfig m)
    (_minerResNodeId m)
    (_minerResCutDb m)
  where
    chooseMiner
        :: PayloadCas cas
        => ChainwebVersion
        -> LogFunction
        -> MinerConfig
        -> NodeId
        -> CutDb cas
        -> IO ()
    chooseMiner Test{} = testMiner
    chooseMiner TimedConsensus{} = testMiner
    chooseMiner PowConsensus{} = powMiner (localMining v)
    chooseMiner TimedCPM{} = testMiner
    chooseMiner Development = powMiner (localMining v)
    chooseMiner Testnet00 = powMiner (localMining v)
    chooseMiner Testnet01 = powMiner (localMining v)
    chooseMiner Testnet02 = powMiner (localMining v)

-- | A single-threaded in-process mining loop.
localMining :: ChainwebVersion -> BlockHeader -> IO BlockHeader
localMining v = usePowHash v mine

-- remoteMining :: BlockHeader -> IO BlockHeader
-- remoteMining = undefined  -- TODO!
