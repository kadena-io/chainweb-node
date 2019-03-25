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
import Control.Lens (view, (^?!))

import Data.Reflection (give)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Word (Word64)

import Numeric.Natural (Natural)

import System.Environment
import System.IO.Unsafe
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
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- TESTING: Disable Pact

mockPact :: Bool
mockPact = unsafePerformIO $ do
    lookupEnv "CHAINWEB_DISABLE_PACT" >>= \case
        Nothing -> return False
        Just "0" -> return False
        _ -> return True
{-# NOINLINE mockPact #-}

-- -------------------------------------------------------------------------- --
-- Test Miner

-- | Test miner (no real POW)
--
-- Overall the mining process works as follows:
--
-- 1. pick chain
-- 2. get head
-- 3. check if chain can be mined (isn't blocked)
-- 4. get payload
-- 5. get BlockPayloadHash
-- 7. get block creation time
-- 8. pick nonce
-- 9. compute POW hash
-- 10 check target
-- 11. compute merkle hash
-- 12. insert block payloads into payload db
--     (this means we don't re-evluate, which should be fine, since we should
--     trust our own block, if we don't we can trigger re-evaluation)
-- 13. submit BlockHeader and BlockTransactions to cut validation pipeline.
--
-- If 10 fails goto 8.
-- If CreationTime timeout goto 7.
-- If new payload event goto 4.
-- If new head event goto 2.
--
-- Currently 3 and 4 are swapped.
--
testMiner
    :: forall cas
    . PayloadCas cas
    => LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> IO ()
testMiner logFun conf nid cutDb = runForever logFun "Test Miner" $ do
    gen <- MWC.createSystemRandom
    ver <- getVer
    go gen ver 1
  where
    wcdb = view cutDbWebBlockHeaderDb cutDb
    payloadDb = view cutDbPayloadCas cutDb
    payloadStore = view cutDbPayloadStore cutDb

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

    -- | Assemble the new block.
    --
    -- For real POW mining we don't want to do this to early. However, we don't
    -- want to spent too much time on it neither, so we can't do it on each
    -- attempt. Instead we want to do it on a regular schedule.
    --
    -- Here we are guarenteed to succeed on our first attempt, so we do it after
    -- waiting, just before computing the POW hash.
    --
    mine :: MWC.GenIO -> Word64 -> IO Cut
    mine gen !nonce = do

        -- Get the current longest cut.
        --
        c <- _cut cutDb

        -- Randomly pick a chain to mine on. For a real POW miner we should
        -- mine on all chains in parallel.
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

        -- Get the PayloadHash
        --
        let pact = _webPactExecutionService $ _webBlockPayloadStorePact payloadStore
        payload <- case mockPact of
            False -> _pactNewBlock pact (_configMinerInfo conf) p
            True -> return
                $ newPayloadWithOutputs (MinerData "miner") (CoinbaseOutput "coinbase")
                $ S.fromList
                    [ (Transaction "testTransaction", TransactionOutput "testOutput")
                    ]

        -- The new block's creation time. Must come after any simulated
        -- delay.
        --
        ct <- getCurrentTimeIntegral

        -- Loops (i.e. "mines") if a non-matching nonce was generated.
        --
        -- INVARIANT: `testMine` will succeed on the first attempt when
        -- POW is not used.
        --
        result <- give payloadDb $ give wcdb
            $ testMineWithPayload @cas (Nonce nonce) target ct payload nid cid c pact

        case result of
            Left BadNonce -> do
                logg Info "retry test mining because nonce doesn't meet target"
                mine gen (succ nonce)
            Left BadAdjacents -> do
                logg Info "retry test mining because adajencent dependencies are missing"
                mine gen nonce
            Right (T2 _ newCut) -> pure newCut
