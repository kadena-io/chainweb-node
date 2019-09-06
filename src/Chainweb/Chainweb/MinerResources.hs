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

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (TVar, atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar)
import Control.Concurrent.STM.TVar (newTVarIO)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Set (Set)
import qualified Data.Set as S

import Network.HTTP.Client (defaultManagerSettings, newManager)

import Servant.Client (BaseUrl(..), Scheme(..))

import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.CutDB (CutDb)
import Chainweb.HostAddress
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config (MinerConfig(..), MinerCount(..))
import Chainweb.Miner.Coordinator (MiningState(..), publishing, working)
import Chainweb.Miner.Miners
import Chainweb.NodeId (NodeId)
import Chainweb.Payload.PayloadStore
import Chainweb.Utils (EnableConfig(..), runForever)
import Chainweb.Version (ChainwebVersion(..), window)

import Data.LogMessage (LogFunction)

-- -------------------------------------------------------------------------- --
-- Miner

data MinerResources logger cas = MinerResources
    { _minerResLogger :: !logger
    , _minerResNodeId :: !NodeId
    , _minerResCutDb :: !(CutDb cas)
    , _minerResConfig :: !MinerConfig
    , _minerResState :: TVar (Maybe MiningState)
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
        tms <- newTVarIO Nothing
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
runMiner v mr = do
    tmv   <- newEmptyTMVarIO
    inner <- chooseMiner tmv
    concurrently_ (loop $ working inner tms conf nid cdb) (listener tmv)
  where
    nid :: NodeId
    nid = _minerResNodeId mr

    cdb :: CutDb cas
    cdb = _minerResCutDb mr

    conf :: MinerConfig
    conf = _minerResConfig mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    tms :: TVar (Maybe MiningState)
    tms = _minerResState mr

    miners :: MinerCount
    miners = _configTestMiners conf

    loop :: IO () -> IO ()
    loop = runForever lf "Chainweb.Miner.Coordinator.working"

    -- | This thread will sit silently forever without burning resources when a
    -- remote miner is being used, since the `TMVar` will never be written to in
    -- that case.
    --
    listener :: TMVar BlockHeader -> IO ()
    listener tmv = runForever lf "Chainweb.Miner.listener" $ do
        atomically (takeTMVar tmv) >>= publishing lf tms cdb

    chooseMiner :: TMVar BlockHeader -> IO (BlockHeader -> IO ())
    chooseMiner = case window v of
        Nothing -> testMiner -- no difficulty adjustment defined
        Just _ -> powMiner -- difficulty adjustement defined

    testMiner :: TMVar BlockHeader -> IO (BlockHeader -> IO ())
    testMiner tmv = do
        gen <- MWC.createSystemRandom
        pure $ localTest tmv gen miners

    powMiner :: TMVar BlockHeader -> IO (BlockHeader -> IO ())
    powMiner tmv = case g $ _configRemoteMiners conf of
        Nothing -> pure $ localPOW tmv v
        Just rs -> do
            m <- newManager defaultManagerSettings
            pure $ remoteMining m rs

    g :: Set HostAddress -> Maybe (NonEmpty BaseUrl)
    g = fmap (NEL.map (hostAddressToBaseUrl Http)) . NEL.nonEmpty . S.toList
