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

import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.POW
import Chainweb.Miner.Test
import Chainweb.NodeId
import Chainweb.Payload.PayloadStore
import Chainweb.Utils (EnableConfig(..))
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Miner

data MinerResources logger cas = MinerResources
    { _minerResLogger :: !logger
    , _minerResNodeId :: !NodeId
    , _minerResCutDb :: !(CutDb cas)
    , _minerResWebBlockHeaderDb :: !WebBlockHeaderDb
    , _minerResWebPayloadDb :: !(PayloadDb cas)
    , _minerResConfig :: !MinerConfig
    }

withMinerResources
    :: logger
    -> EnableConfig MinerConfig
    -> NodeId
    -> CutDb cas
    -> WebBlockHeaderDb
    -> PayloadDb cas
    -> (Maybe (MinerResources logger cas) -> IO a)
    -> IO a
withMinerResources logger (EnableConfig enabled conf) nid cutDb webDb payloadDb inner
    | not enabled = inner Nothing
    | otherwise = inner . Just $ MinerResources
        { _minerResLogger = logger
        , _minerResNodeId = nid
        , _minerResCutDb = cutDb
        , _minerResWebBlockHeaderDb = webDb
        , _minerResWebPayloadDb = payloadDb
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
    (_minerResWebBlockHeaderDb m)
    (_minerResWebPayloadDb m)
  where
    chooseMiner
        :: PayloadCas cas
        => ChainwebVersion
        -> LogFunction
        -> MinerConfig
        -> NodeId
        -> CutDb cas
        -> WebBlockHeaderDb
        -> PayloadDb cas
        -> IO ()
    chooseMiner Test{} = testMiner
    chooseMiner TestWithTime{} = testMiner
    chooseMiner TestWithPow{} = powMiner
    chooseMiner Simulation{} = testMiner
    chooseMiner Testnet00 = powMiner
