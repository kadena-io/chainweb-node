{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Miner.Miners
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Implementation of in-node miners. These miners are only used in testing or
-- in-node mining on development or testnets.
--
-- `localPOW` is capable of mining on production networks and is used for
-- in-node mining on the development and test networks. However, single threaded
-- CPU mining is by far not powerful enough to win blocks on mainnet.
--
module Chainweb.Miner.Miners
  ( -- * Local Mining
    localPOW
  , localTest
  , mempoolNoopMiner
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Lens
import Control.Monad

import qualified Data.ByteString.Short as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy

import Numeric.Natural (Natural)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut.Create
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Core
import Chainweb.Miner.Pact
import Chainweb.RestAPI.Orphans ()
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version

import Data.LogMessage (LogFunction)

--------------------------------------------------------------------------------
-- Local Mining

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
-- This is not production POW, but only for testing.
--
localTest
    :: Logger logger
    => LogFunction
    -> ChainwebVersion
    -> MiningCoordination logger cas
    -> Miner
    -> CutDb cas
    -> MWC.GenIO
    -> MinerCount
    -> IO ()
localTest lf v coord m cdb gen miners =
    runForever lf "Chainweb.Miner.Miners.localTest" $ do
        c <- _cut cdb
        wh <- work coord Nothing m
        let height = c ^?! ixg (_workHeaderChainId wh) . blockHeight

        race (awaitNewCutByChainId cdb (_workHeaderChainId wh) c) (go height wh) >>= \case
            Left _ -> return ()
            Right new -> do
                solve coord new
                void $ awaitNewCut cdb c
  where
    meanBlockTime :: Double
    meanBlockTime = int $ _getBlockRate $ blockRate v

    go :: BlockHeight -> WorkHeader -> IO SolvedWork
    go height w = do
        MWC.geometric1 t gen >>= threadDelay
        runGetS decodeSolvedWork $ BS.fromShort $ _workHeaderBytes w
      where
        t :: Double
        t = int graphOrder / (int (_minerCount miners) * meanBlockTime * 1000000)

        graphOrder :: Natural
        graphOrder = order $ chainGraphAt v height

-- | A miner that grabs new blocks from mempool and discards them. Mempool
-- pruning happens during new-block time, so we need to ask for a new block
-- regularly to prune mempool.
--
mempoolNoopMiner
    :: LogFunction
    -> HashMap ChainId (MempoolBackend ChainwebTransaction)
    -> IO ()
mempoolNoopMiner lf chainRes =
    runForever lf "Chainweb.Miner.Miners.mempoolNoopMiner" $ do
        mapM_ runOne $ HashMap.toList chainRes
        approximateThreadDelay 60_000_000 -- wake up once a minute
  where
    runOne (_, cr) = Mempool.mempoolPrune cr

-- | A single-threaded in-process Proof-of-Work mining loop.
--
localPOW
    :: Logger logger
    => LogFunction
    -> ChainwebVersion
    -> MiningCoordination logger cas
    -> Miner
    -> CutDb cas
    -> IO ()
localPOW lf v coord m cdb = runForever lf "Chainweb.Miner.Miners.localPOW" $ do
    c <- _cut cdb
    wh <- work coord Nothing m
    race (awaitNewCutByChainId cdb (_workHeaderChainId wh) c) (go wh) >>= \case
        Left _ -> return ()
        Right new -> do
            solve coord new
            void $ awaitNewCut cdb c
  where
    go :: WorkHeader -> IO SolvedWork
    go wh = usePowHash v $ \(_ :: Proxy a) -> mine @a (Nonce 0) wh
