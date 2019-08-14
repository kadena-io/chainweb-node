-- |
-- Module: Chainweb.Miner.Miners
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Miner.Miners where

import Control.Concurrent (threadDelay)

import Numeric.Natural (Natural)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.Difficulty (BlockRate(..), blockRate)
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Core (mine, usePowHash)
import Chainweb.Time (Seconds(..))
import Chainweb.Utils (int)
import Chainweb.Version (ChainwebVersion(..), order, _chainGraph)

---

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

-- | Some remote process which is performing the low-level mining for us. May be
-- on a different machine, may be on multiple machines, may be arbitrarily
-- multithreaded.
--
remoteMining :: BlockHeader -> IO BlockHeader
remoteMining = do
    -- Get miner addresses from config
    -- Submit work to remote miners
    -- Ask for result every second
    -- Return result
    undefined
