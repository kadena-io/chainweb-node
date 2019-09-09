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

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TVar (newTVarIO)

import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.CutDB (CutDb)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config (MinerConfig(..))
import Chainweb.Miner.Coordinator (MiningState(..))
import Chainweb.Miner.Miners
import Chainweb.NodeId (NodeId)
import Chainweb.Payload.PayloadStore
import Chainweb.Utils (EnableConfig(..))
import Chainweb.Version (ChainwebVersion(..), window)

import Data.LogMessage (LogFunction)

-- -------------------------------------------------------------------------- --
-- Miner

data MinerResources logger cas = MinerResources
    { _minerResLogger :: !logger
    , _minerResNodeId :: !NodeId
    , _minerResCutDb :: !(CutDb cas)
    , _minerResConfig :: !MinerConfig
    , _minerResState :: TVar MiningState
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
    | otherwise = do
        tms <- newTVarIO mempty
        inner . Just $ MinerResources
            { _minerResLogger = logger
            , _minerResNodeId = nid
            , _minerResCutDb = cutDb
            , _minerResConfig = conf
            , _minerResState = tms
            }

runMiner
    :: forall logger cas
    .  Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> MinerResources logger cas
    -> IO ()
runMiner v mr = case window v of
    Nothing -> testMiner
    Just _ -> powMiner
  where
    cdb :: CutDb cas
    cdb = _minerResCutDb mr

    conf :: MinerConfig
    conf = _minerResConfig mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    testMiner :: IO ()
    testMiner = do
        gen <- MWC.createSystemRandom
        localTest lf v (_configMinerInfo conf) cdb gen (_configTestMiners conf)

    powMiner :: IO ()
    powMiner = localPOW lf v (_configMinerInfo conf) cdb
