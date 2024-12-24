{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.Chainweb.MinerResources
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Resources for initializing mining and related components.
--
-- This datastructure must only be used during node startup. No heap reference
-- should be kept after intialization of the node is complete.
--
module Chainweb.Chainweb.MinerResources
  ( -- * In-process Mining
    MinerResources(..)
  , withMinerResources
  , runMiner
    -- * Remote Work Requests
  , MiningCoordination(..)
  , withMiningCoordination
  ) where

import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Miners
import Chainweb.Version
import Control.Concurrent.Async
import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.LogMessage (LogFunction)
import System.Random.MWC qualified as MWC

-- -------------------------------------------------------------------------- --
-- Miner

-- | Setup and Start Mining Coordination.
--
-- Provides the payload caches that can be filled by payload providers and
-- listens on new payloads and cuts and creates and delivers work to the mining
-- clients.
--
-- FIXME: We may hide the constructors of MiningCoordination and only expose
-- what is useful to other components
--
withMiningCoordination
    :: Logger logger
    => logger
    -> MiningConfig
    -> CutDb
    -> (Maybe (MiningCoordination logger) -> IO a)
    -> IO a
withMiningCoordination logger conf cdb inner
    | not (_coordinationEnabled coordConf) = inner Nothing
    | otherwise = do
        coord <- newMiningCoordination logger coordConf cdb
        fmap snd $ concurrently
            -- maintain mining state
            (runCoordination coord)
            -- run inner computation
            (inner (Just coord))
  where
    -- providers = view cutDbPayloadProviders cdb
    coordConf = _miningCoordination conf

-- -------------------------------------------------------------------------- --

-- | Miner resources are used by the test-miner when in-node mining is
-- configured or by the mempool noop-miner (which keeps the mempool updated) in
-- production setups.
--
data MinerResources logger = MinerResources
    { _minerResLogger :: !logger
    , _minerResCutDb :: !CutDb
    , _minerChainResources :: !(HashMap ChainId (ChainResources logger))
    , _minerResConfig :: !NodeMiningConfig
    , _minerResCoordination :: !(Maybe (MiningCoordination logger))
        -- ^ The primed work cache. This is Nothing when coordination is
        -- disabled. It is needed by the in-node test miner. The mempoolNoopMiner
        -- does not use it.
    }

withMinerResources
    :: logger
    -> NodeMiningConfig
    -> HashMap ChainId (ChainResources logger)
    -> CutDb
    -> Maybe (MiningCoordination logger)
    -> (Maybe (MinerResources logger) -> IO a)
    -> IO a
withMinerResources logger conf chainRes cutDb tpw inner =
    inner . Just $ MinerResources
        { _minerResLogger = logger
        , _minerResCutDb = cutDb
        , _minerChainResources = chainRes
        , _minerResConfig = conf
        , _minerResCoordination = tpw
        }

-- | This runs the internal in-node miner. It is only used during testing.
--
-- When mining coordination is disabled, this function exits with an error.
--
-- FIXME: is the above true? I think, the mempoolNoopMiner is important for
-- keeping the mempools up to date when mining coordination is disabled.
--
runMiner
    :: forall logger
    .  Logger logger
    => ChainwebVersion
    -> MinerResources logger
    -> IO ()
runMiner v mr
    | enabled = case _minerResCoordination mr of
        Nothing -> error
            "Mining coordination must be enabled in order to use the in-node test miner"
        Just coord -> case v ^. versionCheats . disablePow of
            True -> testMiner coord
            False -> powMiner coord
    | otherwise = return ()
        -- FIXME: this is needed / possible only for pact chain
        -- (and should be handled internally by pact service)
        -- mempoolNoopMiner lf (_chainResMempool <$> _minerChainResources mr)

  where
    enabled = _nodeMiningEnabled $ _minerResConfig mr

    cdb :: CutDb
    cdb = _minerResCutDb mr

    conf :: NodeMiningConfig
    conf = _minerResConfig mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    testMiner coord = do
        gen <- MWC.createSystemRandom
        localTest lf v coord cdb gen (_nodeTestMiners conf)

    powMiner coord = localPOW lf coord cdb
