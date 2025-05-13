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
{-# LANGUAGE ImportQualifiedPost #-}

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

import Crypto.Hash.Algorithms (Blake2s_256)

import Data.ByteString.Short qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

import Numeric.Natural (Natural)

import GHC.Stack

import System.Random.MWC qualified as MWC
import System.Random.MWC.Distributions qualified as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut.Create
import Chainweb.CutDB
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.Mempool qualified as Mempool
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Core
import Chainweb.RestAPI.Orphans ()
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version

import Data.LogMessage (LogFunction, LogFunctionText)
import System.LogLevel
import Control.Concurrent.STM

--------------------------------------------------------------------------------
-- Local Mining

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
-- This is not production POW, but only for testing.
--
localTest
    :: HasCallStack
    => Logger logger
    => HasVersion
    => LogFunction
    -> MiningCoordination logger
    -> CutDb
    -> MWC.GenIO
    -> MinerCount
    -> IO ()
localTest lf coord cdb gen miners =
    runForever lf "Chainweb.Miner.Miners.localTest" $ do
        c <- _cut cdb
        wh <- work coord
        let height = c ^?! ixg (_miningWorkChainId wh) . blockHeight

        race (awaitNewCutByChainId cdb (_miningWorkChainId wh) c) (go height wh) >>= \case
            Left _ -> return ()
            Right new -> do
                solve coord new
                void $ awaitNewCut cdb c
  where
    meanBlockTime :: Double
    meanBlockTime = int (_getBlockDelay (_versionBlockDelay implicitVersion)) / 1_000_000

    go :: BlockHeight -> MiningWork -> IO SolvedWork
    go height w = do
        MWC.geometric1 t gen >>= threadDelay
        runGetS decodeSolvedWork $ BS.fromShort $ _miningWorkBytes w
      where
        t :: Double
        t = int graphOrder / (int (_minerCount miners) * meanBlockTime * 1_000_000)

        graphOrder :: Natural
        graphOrder = order $ chainGraphAt height

-- | A miner that grabs new blocks from mempool and discards them. Mempool
-- pruning happens during new-block time, so we need to ask for a new block
-- regularly to prune mempool.
--
mempoolNoopMiner
    :: LogFunction
    -> HashMap ChainId (MempoolBackend Pact.Transaction)
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
    => HasVersion
    => LogFunctionText
    -> MiningCoordination logger
    -> CutDb
    -> IO ()
localPOW lf coord cdb = runForever lf "Chainweb.Miner.Miners.localPOW" $ do
    c <- _cut cdb
    lf Debug "request new work for localPOW miner"
    wh <- work coord
    let cid = _miningWorkChainId wh
    lf Debug $ "run localPOW miner on chain " <> toText cid
    race (awaitNewCutByChainId cdb cid c) (go wh) >>= \case
        Left _ -> do
            lf Debug "abondond work due to chain update"
            return ()
        Right (new, h) -> do
            lf Debug $ "solved work on chain " <> toText cid
            solve coord new

            -- There is a potential race here, if the solved block got orphaned.
            -- If work isn't updated quickly enough, it can happen that the
            -- miner uses an old header. We resolve that by awaiting that the
            -- chain is at least as high as the solved work.
            -- This can still dead-lock if for some reason the solved work is
            -- invalid.
            awaitHeight (_chainId new) h
  where
    go :: MiningWork -> IO (SolvedWork, BlockHeight)
    go w = do
        h <- workHeight w
        s <- mine @Blake2s_256 (Nonce 0) w
        return (s, h)

    workHeight w = runGetS (skip 258 >> decodeBlockHeight)
        $ BS.fromShort
        $ _miningWorkBytes w

    awaitHeight cid h = atomically $ do
        c <- _cutStm cdb
        let h' = view blockHeight $ c ^?! ixg cid
        guard (h <= h')
