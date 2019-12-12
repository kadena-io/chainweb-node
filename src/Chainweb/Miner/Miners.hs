{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Miner.Miners
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Miner.Miners
  ( -- * Local Mining
    localPOW
  , localTest
  , mempoolNoopMiner
    -- * Remote Mining
  , transferableBytes
  ) where

import Data.Bytes.Put (runPutS)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as M
import Data.Tuple.Strict (T2(..), T3(..))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM.TVar (TVar)
import Control.Lens (view)

import Numeric.Natural (Natural)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.Difficulty (encodeHashTarget)
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Core
import Chainweb.Miner.Pact (Miner)
import Chainweb.RestAPI.Orphans ()
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Seconds(..))
import Chainweb.Transaction
import Chainweb.Utils (approximateThreadDelay, int, runForever, runGet)
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.LogMessage (LogFunction)

---

--------------------------------------------------------------------------------
-- Local Mining

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
localTest
    :: LogFunction
    -> ChainwebVersion
    -> TVar PrimedWork
    -> Miner
    -> CutDb cas
    -> MWC.GenIO
    -> MinerCount
    -> IO ()
localTest lf v tpw m cdb gen miners = runForever lf "Chainweb.Miner.Miners.localTest" loop
  where
    loop :: IO a
    loop = do
        c <- _cut cdb
        T3 p bh pl <- newWork lf Anything (Plebian m) pact tpw c
        let !phash = _blockPayloadHash bh
            !bct = _blockCreationTime bh
            ms = MiningState $ M.singleton (T2 bct phash) (T3 m p pl)
        work bh >>= publish lf ms cdb >> awaitNewCut cdb c >> loop

    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

    t :: Double
    t = int graphOrder / (int (_minerCount miners) * meanBlockTime * 1000000)

    graphOrder :: Natural
    graphOrder = order $ _chainGraph v

    meanBlockTime :: Double
    meanBlockTime = case blockRate v of
        BlockRate (Seconds n) -> int n

    work :: BlockHeader -> IO BlockHeader
    work bh = MWC.geometric1 t gen >>= threadDelay >> pure bh

-- | A miner that grabs new blocks from mempool and discards them. Mempool
-- pruning happens during new-block time, so we need to ask for a new block
-- regularly to prune mempool.
mempoolNoopMiner
    :: LogFunction
    -> HashMap ChainId (MempoolBackend ChainwebTransaction)
    -> IO ()
mempoolNoopMiner lf chainRes =
    runForever lf "Chainweb.Miner.Miners.mempoolNoopMiner" loop
  where
    loop = do
        mapM_ runOne $ HashMap.toList chainRes
        approximateThreadDelay 60000000 -- wake up once a minute

    runOne (_, cr) = Mempool.mempoolPrune cr

-- | A single-threaded in-process Proof-of-Work mining loop.
--
localPOW :: LogFunction -> ChainwebVersion -> TVar PrimedWork -> Miner -> CutDb cas -> IO ()
localPOW lf v tpw m cdb = runForever lf "Chainweb.Miner.Miners.localPOW" loop
  where
    loop :: IO a
    loop = do
        c <- _cut cdb
        T3 p bh pl <- newWork lf Anything (Plebian m) pact tpw c
        let !phash = _blockPayloadHash bh
            !bct = _blockCreationTime bh
            ms = MiningState $ M.singleton (T2 bct phash) (T3 m p pl)
        race (awaitNewCutByChainId cdb (_chainId bh) c) (work bh) >>= \case
            Left _ -> loop
            Right new -> publish lf ms cdb new >> awaitNewCut cdb c >> loop

    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

    work :: BlockHeader -> IO BlockHeader
    work bh = do
        let T3 _ tbytes hbytes = transferableBytes bh
        T2 (HeaderBytes new) _ <- usePowHash v (\p -> mine p (_blockNonce bh) tbytes) hbytes
        runGet decodeBlockHeaderWithoutHash new

-- | Can be piped to `workBytes` for a form suitable to use with
-- `Chainweb.Miner.RestAPI.MiningApi_`.
--
transferableBytes :: BlockHeader -> T3 ChainBytes TargetBytes HeaderBytes
transferableBytes bh = T3 c t h
  where
    t = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh
    h = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh
    c = ChainBytes  . runPutS . encodeChainId $ _chainId bh
