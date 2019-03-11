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
, withMiner
, runMiner
) where

-- internal modules

import Chainweb.CutDB
import Chainweb.Miner.Config
import Chainweb.Miner.POW
import Chainweb.Miner.Test
import Chainweb.NodeId
import Chainweb.Payload.PayloadStore
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Miner

data MinerResources cas = MinerResources
    { _minerResLogFun :: !ALogFunction
    , _minerResNodeId :: !NodeId
    , _minerResCutDb :: !(CutDb cas)
    , _minerResWebBlockHeaderDb :: !WebBlockHeaderDb
    , _minerResWebPayloadDb :: !(PayloadDb cas)
    , _minerResConfig :: !MinerConfig
    }

withMiner
    :: ALogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> WebBlockHeaderDb
    -> PayloadDb cas
    -> (MinerResources cas -> IO a)
    -> IO a
withMiner logFun conf nid cutDb webDb payloadDb inner = inner $ MinerResources
    { _minerResLogFun = logFun
    , _minerResNodeId = nid
    , _minerResCutDb = cutDb
    , _minerResWebBlockHeaderDb = webDb
    , _minerResWebPayloadDb = payloadDb
    , _minerResConfig = conf
    }

runMiner :: PayloadCas cas => ChainwebVersion -> MinerResources cas -> IO ()
runMiner v m = (chooseMiner v)
    (_getLogFunction $ _minerResLogFun m)
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

