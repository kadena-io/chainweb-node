{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
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
  ( -- * In-process Mining
    MinerResources(..)
  , withMinerResources
  , runMiner
    -- * Remote Work Requests
  , MiningCoordination(..)
  , withMiningCoordination
  ) where

import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.Map.Strict as M
import Data.Tuple.Strict (T3(..))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Lens (over)

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHeader (BlockCreationTime(..), _blockCreationTime)
import Chainweb.CutDB (CutDb)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config (MinerConfig(..))
import Chainweb.Miner.Coordinator (MiningState(..), PrevBlock(..), MiningStats(..))
import Chainweb.Miner.Miners
import Chainweb.Payload.PayloadStore
import Chainweb.Time (Micros, Time(..), getCurrentTimeIntegral)
import Chainweb.Utils (EnableConfig(..), int, runForever)
import Chainweb.Version (ChainwebVersion(..), window)

import Data.LogMessage (JsonLog(..), LogFunction)

-- -------------------------------------------------------------------------- --
-- Miner

-- | For coordinating requests for work and mining solutions from remote Mining
-- Clients.
--
data MiningCoordination logger cas = MiningCoordination
    { _coordLogger :: !logger
    , _coordCutDb :: !(CutDb cas)
    , _coordState :: !(TVar MiningState)
    , _coordLimit :: !Int }

withMiningCoordination
    :: Logger logger
    => logger
    -> Bool
    -> CutDb cas
    -> (Maybe (MiningCoordination logger cas) -> IO a)
    -> IO a
withMiningCoordination logger enabled cutDb inner
    | not enabled = inner Nothing
    | otherwise = do
        t <- newTVarIO mempty
        fmap snd . concurrently (prune t) $ inner . Just $ MiningCoordination
            { _coordLogger = logger
            , _coordCutDb = cutDb
            , _coordState = t
            , _coordLimit = 2500 }
  where
    prune :: TVar MiningState -> IO ()
    prune t = runForever (logFunction logger) "Chainweb.Chainweb.MinerResources.prune" $ do
        let !d = 300000000  -- 5 minutes
        threadDelay d
        ago <- over (_Unwrapped . _Unwrapped) (subtract (int d)) <$> getCurrentTimeIntegral
        MiningState ms <- atomically $ do
            modifyTVar' t . over _Unwrapped $ M.filter (f ago)
            readTVar t
        logFunction logger Info . JsonLog $ MiningStats (M.size ms)

    f :: Time Micros -> T3 a PrevBlock b -> Bool
    f ago (T3 _ (PrevBlock p) _) = _blockCreationTime p > BlockCreationTime ago

-- | For in-process CPU mining by a Chainweb Node.
--
data MinerResources logger cas = MinerResources
    { _minerResLogger :: !logger
    , _minerResCutDb :: !(CutDb cas)
    , _minerResConfig :: !MinerConfig
    }

withMinerResources
    :: logger
    -> EnableConfig MinerConfig
    -> CutDb cas
    -> (Maybe (MinerResources logger cas) -> IO a)
    -> IO a
withMinerResources logger (EnableConfig enabled conf) cutDb inner
    | not enabled = inner Nothing
    | otherwise = do
        inner . Just $ MinerResources
            { _minerResLogger = logger
            , _minerResCutDb = cutDb
            , _minerResConfig = conf
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
