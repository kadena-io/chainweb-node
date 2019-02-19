{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Chainweb.Miner.Test
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This is a miner for testing purposes only - it performs no Proof of Work, but
-- instead simulates mining via a geometrically distributed thread delay.
--

module Chainweb.Miner.Test ( testMiner ) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^?!))
import Control.Monad.STM (atomically)

import Data.Reflection (Given, give)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Word (Word64)

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Difficulty (BlockRate(..), blockRate)
import Chainweb.Graph
import Chainweb.Miner.Config (MinerConfig(..))
import Chainweb.NodeId (NodeId)
import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion)
import Chainweb.WebBlockHeaderDB (WebBlockHeaderDb)

import Data.DiGraph (order)
import Data.LogMessage (LogFunction)

-- -------------------------------------------------------------------------- --
-- Test Miner

testMiner
    :: LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb
    -> WebBlockHeaderDb
    -> IO ()
testMiner logFun _ nid cutDb wcdb = do
    logg Info "Started Test Miner"
    gen <- MWC.createSystemRandom

    ver <- getVer

    give wcdb $ go gen ver 1
  where
    logg :: LogLevel -> T.Text -> IO ()
    logg = logFun

    graph :: ChainGraph
    graph = _chainGraph cutDb

    getVer :: IO ChainwebVersion
    getVer = do
        c <- _cut cutDb
        cid <- randomChainId c
        pure . _blockChainwebVersion $ c ^?! ixg cid

    go :: Given WebBlockHeaderDb => MWC.GenIO -> ChainwebVersion -> Int -> IO ()
    go gen ver !i = do
        nonce0 <- MWC.uniform gen

        -- Artificially delay the mining process since we are not using
        -- proof-of-work mining.
        --
        d <- MWC.geometric1
                (int (order graph) / (meanBlockTime * 1000000))
                gen
        threadDelay d

        -- Mine a new block
        --
        c' <- mine gen nonce0

        logg Info $! "created new block" <> sshow i

        -- Publish the new Cut into the CutDb (add to queue).
        --
        atomically $! addCutHashes cutDb (cutToCutHashes Nothing c')

        go gen ver (i + 1)
      where
        meanBlockTime :: Double
        meanBlockTime = case blockRate ver of
            Just (BlockRate n) -> int n
            Nothing -> error $ "No BlockRate available for given ChainwebVersion: " <> show ver

    mine :: Given WebBlockHeaderDb => MWC.GenIO -> Word64 -> IO Cut
    mine gen !nonce = do
        -- Get the current longest cut.
        --
        c <- _cut cutDb

        -- Randomly pick a chain to mine on.
        --
        cid <- randomChainId c

        -- The parent block the mine on. Any given chain will always
        -- contain at least a genesis block, so this otherwise naughty
        -- `^?!` will always succeed.
        --
        let !p = c ^?! ixg cid

        -- The hashing target to be lower than.
        --
        let !target = _blockTarget p

        -- The new block's creation time. Must come after any simulated
        -- delay.
        --
        ct <- getCurrentTimeIntegral

        -- Loops (i.e. "mines") if a non-matching nonce was generated.
        --
        -- INVARIANT: `testMine` will succeed on the first attempt when
        -- POW is not used.
        --
        testMine (Nonce nonce) target ct nid cid c >>= \case
            Left BadNonce -> mine gen (succ nonce)
            Left BadAdjacents -> mine gen nonce
            Right (T2 _ newCut) -> pure newCut
