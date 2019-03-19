{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

import Data.Reflection (give)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Word (Word64)

import Numeric.Natural (Natural)

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty (BlockRate(..), blockRate)
import Chainweb.Graph
import Chainweb.Miner.Config (MinerConfig(..), MinerCount(..))
import Chainweb.NodeId (NodeId)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Test Miner

testMiner
    :: forall cas
    . PayloadCas cas
    => LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> WebBlockHeaderDb
    -> PayloadDb cas
    -> IO ()
testMiner logFun conf nid cutDb wcdb payloadDb = do
    logg Info "Started Test Miner"
    gen <- MWC.createSystemRandom

    ver <- getVer

    go gen ver 1
  where
    logg :: LogLevel -> T.Text -> IO ()
    logg = logFun

    graph :: ChainGraph
    graph = _chainGraph cutDb

    graphOrder :: Natural
    graphOrder = order graph

    getVer :: IO ChainwebVersion
    getVer = do
        c <- _cut cutDb
        cid <- randomChainId c
        pure . _blockChainwebVersion $ c ^?! ixg cid

    miners :: Natural
    miners = _minerCount $ _configTestMiners conf

    go
        :: MWC.GenIO
        -> ChainwebVersion
        -> Int
        -> IO ()
    go gen ver !i = do
        nonce0 <- MWC.uniform gen

        -- Artificially delay the mining process since we are not using
        -- Proof-of-Work mining.
        --
        d <- MWC.geometric1
                (int graphOrder / (int miners * meanBlockTime * 1000000))
                gen
        threadDelay d

        -- Mine a new block
        --
        c' <- mine gen nonce0

        logg Info $! "created new block" <> sshow i

        -- Publish the new Cut into the CutDb (add to queue).
        --
        addCutHashes cutDb (cutToCutHashes Nothing c')

        go gen ver (i + 1)
      where
        meanBlockTime :: Double
        meanBlockTime = case blockRate ver of
            Just (BlockRate n) -> int n
            Nothing -> error $ "No BlockRate available for given ChainwebVersion: " <> show ver

    mine
        :: MWC.GenIO
        -> Word64
        -> IO Cut
    mine gen !nonce = do
        -- Get the current longest cut.
        --
        c <- _cut cutDb

        -- Randomly pick a chain to mine on.
        --
        cid <- randomChainId c

        -- The parent block to mine on. Any given chain will always
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
        let payload = newPayloadWithOutputs (MinerData "miner") (CoinbaseOutput "coinbase") $ S.fromList
                [ (Transaction "testTransaction", TransactionOutput "testOutput")
                ]
        (give payloadDb $ give wcdb $ testMineWithPayload @cas (Nonce nonce) target ct payload nid cid c)
            >>= \case
                Left BadNonce -> mine gen (succ nonce)
                Left BadAdjacents -> mine gen nonce
                Right (T2 _ newCut) -> pure newCut
