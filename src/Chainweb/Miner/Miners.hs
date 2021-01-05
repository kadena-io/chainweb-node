{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Lens
import Control.Monad

import qualified Data.ByteString.Short as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.Tuple.Strict (T2(..))

import Numeric.Natural (Natural)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut.Create
import Chainweb.CutDB
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Core
import Chainweb.Miner.Pact
import Chainweb.RestAPI.Orphans ()
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.LogMessage (LogFunction)

--------------------------------------------------------------------------------
-- Local Mining

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
-- This is not production POW, but only for testing.
--
localTest
    :: LogFunction
    -> ChainwebVersion
    -> Miner
    -> CutDb cas
    -> MWC.GenIO
    -> MinerCount
    -> IO ()
localTest lf v m@(Miner mid _) cdb gen miners = do
    tpw <- newTVarIO (PrimedWork $ HashMap.singleton mid mempty)
    let loop :: IO a
        loop = do
            c <- _cut cdb
            T2 wh pd <- newWork lf Anything m hdb pact tpw c
            let height = c ^?! ixg (_workHeaderChainId wh) . blockHeight
            work height wh >>= publish lf cdb (view minerId m) pd
            void $ awaitNewCut cdb c
            loop
    runForever lf "Chainweb.Miner.Miners.localTest" loop
  where
    pact :: PactExecutionService
    pact = _webPactExecutionService $ view cutDbPactService cdb

    hdb :: WebBlockHeaderDb
    hdb = view cutDbWebBlockHeaderDb cdb

    meanBlockTime :: Double
    meanBlockTime = int $ _getBlockRate $ blockRate v

    work :: BlockHeight -> WorkHeader -> IO SolvedWork
    work height w = do
        MWC.geometric1 t gen >>= threadDelay
        runGet decodeSolvedWork $ BS.fromShort $ _workHeaderBytes w
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
    runForever lf "Chainweb.Miner.Miners.mempoolNoopMiner" loop
  where
    loop = do
        mapM_ runOne $ HashMap.toList chainRes
        approximateThreadDelay 60_000_000 -- wake up once a minute

    runOne (_, cr) = Mempool.mempoolPrune cr

-- | A single-threaded in-process Proof-of-Work mining loop.
--
localPOW
    :: LogFunction
    -> ChainwebVersion
    -> Miner
    -> CutDb cas
    -> IO ()
localPOW lf v m@(Miner mid _) cdb = do
    tpw <- newTVarIO (PrimedWork $ HashMap.singleton mid mempty)
    let loop :: IO a
        loop = do
            c <- _cut cdb
            T2 wh pd <- newWork lf Anything m hdb pact tpw c
            race (awaitNewCutByChainId cdb (_workHeaderChainId wh) c) (work wh) >>= \case
                Left _ -> loop
                Right new -> do
                    publish lf cdb (view minerId m) pd new
                    void $ awaitNewCut cdb c
                    loop
    runForever lf "Chainweb.Miner.Miners.localPOW" loop
  where
    pact :: PactExecutionService
    pact = _webPactExecutionService $ view cutDbPactService cdb

    hdb :: WebBlockHeaderDb
    hdb = view cutDbWebBlockHeaderDb cdb

    work :: WorkHeader -> IO SolvedWork
    work wh = usePowHash v $ \(_ :: Proxy a) -> sfst <$> mine @a (Nonce 0) wh

