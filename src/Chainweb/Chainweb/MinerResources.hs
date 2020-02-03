{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
  ( -- * In-process Mining
    MinerResources(..)
  , withMinerResources
  , runMiner
    -- * Remote Work Requests
  , MiningCoordination(..)
  , withMiningCoordination
  ) where

import Data.Generics.Wrapped (_Unwrapped)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tuple.Strict (T2(..), T3(..))
import qualified Data.Vector as V

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Lens (at, over, view, (&), (?~), (^?!))

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Cut (Cut, _cutMap)
import Chainweb.CutDB (CutDb, awaitNewCutByChainId, cutDbPayloadStore, _cut)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Miners
import Chainweb.Miner.Pact (Miner(..), minerId)
import Chainweb.Payload (PayloadWithOutputs(..))
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore (_webBlockPayloadStorePact)
import Chainweb.Sync.WebBlockHeaderStore (PactExecutionService, _pactNewBlock)
import Chainweb.Time (Micros, Time(..), getCurrentTimeIntegral)
import Chainweb.Utils (fromJuste, ixg, runForever, thd)
import Chainweb.Version
import Chainweb.WebPactExecutionService (_webPactExecutionService)

import Data.LogMessage (JsonLog(..), LogFunction)
import Utils.Logging.Trace (trace)

-- -------------------------------------------------------------------------- --
-- Miner

-- | For coordinating requests for work and mining solutions from remote Mining
-- Clients.
--
data MiningCoordination logger cas = MiningCoordination
    { _coordLogger :: !logger
    , _coordCutDb :: !(CutDb cas)
    , _coordState :: !(TVar MiningState)
    , _coordLimit :: !Int
    , _coord503s :: !(IORef Int)
    , _coord403s :: !(IORef Int)
    , _coordConf :: !CoordinationConfig
    , _coordUpdateStreamCount :: !(IORef Int)
    , _coordPrimedWork :: !(TVar PrimedWork)
    }

withMiningCoordination
    :: Logger logger
    => logger
    -> CoordinationConfig
    -> CutDb cas
    -> (Maybe (MiningCoordination logger cas) -> IO a)
    -> IO a
withMiningCoordination logger conf cdb inner
    | not (_coordinationEnabled conf) = inner Nothing
    | otherwise = do
        cut <- _cut cdb
        t <- newTVarIO mempty
        let !miners = S.toList $ _coordinationMiners conf
        m <- initialPayloads cut miners >>= newTVarIO
        c503 <- newIORef 0
        c403 <- newIORef 0
        l <- newIORef (_coordinationUpdateStreamLimit conf)
        fmap thd . runConcurrently $ (,,)
            <$> Concurrently (prune t m c503 c403)
            <*> Concurrently (mapConcurrently_ (primeWork miners m cut) cids)
            <*> Concurrently (inner . Just $ MiningCoordination
                { _coordLogger = logger
                , _coordCutDb = cdb
                , _coordState = t
                , _coordLimit = _coordinationReqLimit conf
                , _coord503s = c503
                , _coord403s = c403
                , _coordConf = conf
                , _coordUpdateStreamCount = l
                , _coordPrimedWork = m })
  where
    cids :: [ChainId]
    cids = HS.toList . chainIds $ _chainwebVersion cdb

    -- | THREAD: Keep a live-updated cache of Payloads for specific miners, such
    -- that when they request new work, the block can be instantly constructed
    -- without interacting with the Pact Queue.
    --
    primeWork :: [Miner] -> TVar PrimedWork -> Cut -> ChainId -> IO ()
    primeWork miners tpw c cid = runForever (logFunction logger) "primeWork" $ go c
      where
        go :: Cut -> IO a
        go cut = do
            -- Even if the `cut` passed to the function is old, this call here will
            -- immediately detect the newest `BlockHeader` on the given chain.
            new <- awaitNewCutByChainId cdb cid cut
            -- Temporarily block this chain from being considered for queries --
            atomically $ modifyTVar' tpw (silenceChain cid)
            -- Generate new payloads, one for each Miner we're managing --
            let !newParent = ParentHeader . fromJuste . HM.lookup cid $ _cutMap new
            payloads <- traverse (\m -> T2 m <$> getPayload newParent m) miners
            -- Update the cache in a single step --
            atomically $ modifyTVar' tpw (\pw -> foldl' (updateCache cid) pw payloads)
            go new

    -- | Declare that a particular Chain is temporarily unavailable for new work
    -- requests while a new payload is being formed.
    --
    silenceChain :: ChainId -> PrimedWork -> PrimedWork
    silenceChain cid (PrimedWork pw) = PrimedWork (pw & traverse . at cid ?~ Nothing)

    updateCache
        :: ChainId
        -> PrimedWork
        -> T2 Miner (T2 PayloadWithOutputs BlockCreationTime)
        -> PrimedWork
    updateCache cid (PrimedWork pw) (T2 (Miner mid _) payload) =
        PrimedWork (pw & at mid . traverse . at cid ?~ Just payload)

    initialPayloads :: Cut -> [Miner] -> IO PrimedWork
    initialPayloads cut ms =
        PrimedWork . HM.fromList <$> traverse (\m -> (view minerId m,) <$> fromCut m pairs) ms
      where
        pairs :: [T2 ChainId ParentHeader]
        pairs = map (\cid -> T2 cid (ParentHeader (cut ^?! ixg cid))) cids

    -- | Based on a `Cut`, form payloads for each chain for a given `Miner`.
    --
    fromCut
      :: Miner
      -> [T2 ChainId ParentHeader]
      -> IO (HM.HashMap ChainId (Maybe (T2 PayloadWithOutputs BlockCreationTime)))
    fromCut m cut =
        HM.fromList <$> traverse (\(T2 cid bh) -> (cid,) . Just <$> getPayload bh m) cut

    getPayload :: ParentHeader -> Miner -> IO (T2 PayloadWithOutputs BlockCreationTime)
    getPayload (ParentHeader parent) m = do
        creationTime <- BlockCreationTime <$> getCurrentTimeIntegral
        payload <- trace (logFunction logger) "Chainweb.Chainweb.MinerResources.withMiningCoordination.newBlock"
            () 1 (_pactNewBlock pact m parent creationTime)
        pure $ T2 payload creationTime

    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

    -- | THREAD: Periodically clear out the cached payloads kept for Mining
    -- Coordination.
    --
    prune :: TVar MiningState -> TVar PrimedWork -> IORef Int -> IORef Int -> IO ()
    prune t tpw c503 c403 = runForever (logFunction logger) "MinerResources.prune" $ do
        let !d = 30_000_000  -- 30 seconds
        let !maxAge = 300_000_000  -- 5 minutes
        threadDelay d
        ago <- over (_Unwrapped . _Unwrapped) (subtract maxAge) <$> getCurrentTimeIntegral
        m@(MiningState ms) <- atomically $ do
            ms <- readTVar t
            modifyTVar' t . over _Unwrapped $ M.filter (f ago)
            pure ms
        count503 <- readIORef c503
        count403 <- readIORef c403
        PrimedWork pw <- readTVarIO tpw
        atomicWriteIORef c503 0
        atomicWriteIORef c403 0
        logFunction logger Info . JsonLog $ MiningStats
            { _statsCacheSize = M.size ms
            , _stats503s = count503
            , _stats403s = count403
            , _statsAvgTxs = avgTxs m
            , _statsPrimedSize = HM.foldl' (\acc xs -> acc + HM.size xs) 0 pw }

    -- Filter for work items that are not older than maxAge
    --
    -- NOTE: Should difficulty ever become that hard that five minutes aren't
    -- sufficient to mine a block this constant must be changed in order to
    -- recover.
    --
    f :: Time Micros -> T3 a PrevTime b -> Bool
    f ago (T3 _ (PrevTime p) _) = p > BlockCreationTime ago

    avgTxs :: MiningState -> Int
    avgTxs (MiningState ms) = summed `div` max 1 (M.size ms)
      where
        summed :: Int
        summed = M.foldl' (\acc (T3 _ _ ps) -> acc + g ps) 0 ms

        g :: PayloadWithOutputs -> Int
        g = V.length . _payloadWithOutputsTransactions

-- | For in-process CPU mining by a Chainweb Node.
--
data MinerResources logger cas = MinerResources
    { _minerResLogger :: !logger
    , _minerResCutDb :: !(CutDb cas)
    , _minerChainResources :: HashMap ChainId (ChainResources logger)
    , _minerResConfig :: !NodeMiningConfig
    }

withMinerResources
    :: logger
    -> NodeMiningConfig
    -> HashMap ChainId (ChainResources logger)
    -> CutDb cas
    -> (Maybe (MinerResources logger cas) -> IO a)
    -> IO a
withMinerResources logger conf chainRes cutDb inner =
        inner . Just $ MinerResources
            { _minerResLogger = logger
            , _minerResCutDb = cutDb
            , _minerChainResources = chainRes
            , _minerResConfig = conf
            }

runMiner
    :: forall logger cas
    .  Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> MinerResources logger cas
    -> IO ()
runMiner v mr =
    if enabled
        then case window v of
                 Nothing -> testMiner
                 Just _ -> powMiner
        else mempoolNoopMiner lf (_chainResMempool <$> _minerChainResources mr)

  where
    enabled = _nodeMiningEnabled $ _minerResConfig mr

    cdb :: CutDb cas
    cdb = _minerResCutDb mr

    conf :: NodeMiningConfig
    conf = _minerResConfig mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    testMiner :: IO ()
    testMiner = do
        gen <- MWC.createSystemRandom
        tpw <- newTVarIO mempty
        localTest lf v tpw (_nodeMiner conf) cdb gen (_nodeTestMiners conf)

    powMiner :: IO ()
    powMiner = do
        tpw <- newTVarIO mempty
        localPOW lf v tpw (_nodeMiner conf) cdb
