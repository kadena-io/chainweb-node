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

import qualified Data.List.NonEmpty as NEL
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T

import Network.HTTP.Client (defaultManagerSettings, newManager)

import Servant.Client.Core (BaseUrl(..), Scheme(..))

import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.CutDB (CutDb)
import Chainweb.HostAddress (HostAddress(..), hostnameToText)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config (MinerConfig(..), MinerCount(..))
import Chainweb.Miner.Coordinator (mining)
import Chainweb.Miner.Miners
import Chainweb.NodeId (NodeId)
import Chainweb.Payload.PayloadStore
import Chainweb.Utils (EnableConfig(..))
import Chainweb.Version
    (ChainwebVersion(..), MiningProtocol(..), miningProtocol)

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
runMiner v mr = do
    inner <- chooseMiner
    mining
      inner
      (logFunction $ _minerResLogger mr)
      conf
      (_minerResNodeId mr)
      (_minerResCutDb mr)
  where
    conf :: MinerConfig
    conf = _minerResConfig mr

    miners :: MinerCount
    miners = _configTestMiners conf

    chooseMiner :: IO (BlockHeader -> IO BlockHeader)
    chooseMiner = case miningProtocol v of
        Timed -> testMiner
        ProofOfWork -> powMiner

    testMiner :: IO (BlockHeader -> IO BlockHeader)
    testMiner = do
        gen <- MWC.createSystemRandom
        pure $ localTest gen miners

    powMiner :: IO (BlockHeader -> IO BlockHeader)
    powMiner = case NEL.nonEmpty $ _configRemoteMiners conf of
        Nothing -> pure $ localPOW v
        Just rs -> do
            m <- newManager defaultManagerSettings
            pure . remoteMining m . NES.fromList $ NEL.map f rs

    f :: HostAddress -> BaseUrl
    f (HostAddress hn p) = BaseUrl Http hn' p' ""
      where
        hn' = T.unpack $ hostnameToText hn
        p'  = fromIntegral p
