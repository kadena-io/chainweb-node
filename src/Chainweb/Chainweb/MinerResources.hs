{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.MinerResources
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Resources for initializing mining and related components.
--
-- This datastructure must only be used during node startup. No heap reference
-- should be kept after intialization of the node is complete.
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

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Lens (at, over, view, (&), (?~))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Cut (Cut, _cutMap)
import Chainweb.CutDB (CutDb, awaitNewCutByChainId, cutDbPactService, _cut)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Miners
import Chainweb.Miner.Pact (Miner(..), minerId)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Micros, Time, minute, getCurrentTimeIntegral, scaleTimeSpan)
import Chainweb.Utils (fromJuste, runForever, thd, T2(..), T3(..))
import Chainweb.Version
import Chainweb.WebPactExecutionService (_webPactExecutionService)

import Data.LogMessage (JsonLog(..), LogFunction)

import Numeric.AffineSpace

import Utils.Logging.Trace (trace)

-- -------------------------------------------------------------------------- --
-- Miner

withMiningCoordination
    :: Logger logger
    => logger
    -> MiningConfig
    -> CutDb cas
    -> (Maybe (MiningCoordination logger cas) -> IO a)
    -> IO a
withMiningCoordination logger conf cdb inner
    | not (_coordinationEnabled coordConf) = inner Nothing
    | otherwise = do
        cut <- _cut cdb
        t <- newTVarIO mempty
        m <- initialPayloads cut >>= newTVarIO
        c503 <- newIORef 0
        c403 <- newIORef 0
        l <- newIORef (_coordinationUpdateStreamLimit coordConf)
        fmap thd . runConcurrently $ (,,)
            <$> Concurrently (prune t m c503 c403)
            <*> Concurrently (mapConcurrently_ (primeWork m cut) cids)
            <*> Concurrently (inner . Just $ MiningCoordination
                { _coordLogger = logger
                , _coordCutDb = cdb
                , _coordState = t
                , _coordLimit = _coordinationReqLimit coordConf
                , _coord503s = c503
                , _coord403s = c403
                , _coordConf = coordConf
                , _coordUpdateStreamCount = l
                , _coordPrimedWork = m
                })
  where
    coordConf = _miningCoordination conf
    inNodeConf = _miningInNode conf

    cids :: [ChainId]
    cids = HS.toList . chainIds $ _chainwebVersion cdb

    !miners = S.toList (_coordinationMiners coordConf)
        <> [ _nodeMiner inNodeConf | _nodeMiningEnabled inNodeConf ]

    -- | THREAD: Keep a live-updated cache of Payloads for specific miners, such
    -- that when they request new work, the block can be instantly constructed
    -- without interacting with the Pact Queue.
    --
    primeWork :: TVar PrimedWork -> Cut -> ChainId -> IO ()
    primeWork tpw c cid = runForever (logFunction logger) "primeWork" $ go c
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
                !newParentHash = _blockHash $ _parentHeader newParent
            payloads <- traverse (\m -> T2 m <$> getPayload newParent m) miners
            -- Update the cache in a single step --
            atomically $ modifyTVar' tpw $ \pw ->
                foldl' (updateCache cid newParentHash) pw payloads
            go new

    -- | Declare that a particular Chain is temporarily unavailable for new work
    -- requests while a new payload is being formed.
    --
    silenceChain :: ChainId -> PrimedWork -> PrimedWork
    silenceChain cid (PrimedWork pw) = PrimedWork (pw & traverse . at cid ?~ Nothing)

    updateCache
        :: ChainId
        -> BlockHash
        -> PrimedWork
        -> T2 Miner PayloadData
        -> PrimedWork
    updateCache cid !parent (PrimedWork pw) (T2 (Miner mid _) !payload) =
        PrimedWork (pw & at mid . traverse . at cid ?~ Just (payload, parent))

    -- TODO: Should we initialize new chains, too, ahead of time?
    -- It seems that it's not needed and 'awaitNewCutByChainId' in 'primedWork'
    -- will take are of it.
    --
    initialPayloads :: Cut -> IO PrimedWork
    initialPayloads cut = PrimedWork . HM.fromList
        <$> traverse (\m -> (view minerId m,) <$> fromCut m pairs) miners
      where
        pairs :: [T2 ChainId ParentHeader]
        pairs = fmap (uncurry T2) $ HM.toList $ ParentHeader <$> _cutMap cut

    -- | Based on a `Cut`, form payloads for each chain for a given `Miner`.
    --
    fromCut
      :: Miner
      -> [T2 ChainId ParentHeader]
      -> IO (HM.HashMap ChainId (Maybe (PayloadData, BlockHash)))
    fromCut m cut = HM.fromList
        <$> traverse
            (\(T2 cid bh) -> (cid,) . Just . (, _blockHash (_parentHeader bh)) <$> getPayload bh m)
            cut

    getPayload :: ParentHeader -> Miner -> IO PayloadData
    getPayload parent m = trace (logFunction logger)
        "Chainweb.Chainweb.MinerResources.withMiningCoordination.newBlock"
        () 1 (payloadWithOutputsToPayloadData <$> _pactNewBlock pact m parent)

    pact :: PactExecutionService
    pact = _webPactExecutionService $ view cutDbPactService cdb

    -- | THREAD: Periodically clear out the cached payloads kept for Mining
    -- Coordination.
    --
    prune :: TVar MiningState -> TVar PrimedWork -> IORef Int -> IORef Int -> IO ()
    prune t tpw c503 c403 = runForever (logFunction logger) "MinerResources.prune" $ do
        let !d = 30_000_000  -- 30 seconds
        let !maxAge = (5 :: Int) `scaleTimeSpan` minute -- 5 minutes
        threadDelay d
        ago <- (.-^ maxAge) <$> getCurrentTimeIntegral
        m@(MiningState ms) <- atomically $ do
            ms <- readTVar t
            modifyTVar' t . over miningState $ M.filter (f ago)
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
    f :: Time Micros -> T3 a b (Time Micros) -> Bool
    f ago (T3 _ _ added) = added > ago

    avgTxs :: MiningState -> Int
    avgTxs (MiningState ms) = summed `div` max 1 (M.size ms)
      where
        summed :: Int
        summed = M.foldl' (\acc (T3 _ ps _) -> acc + g ps) 0 ms

        g :: PayloadData -> Int
        g = V.length . _payloadDataTransactions

-- | Miner resources are used by the test-miner when in-node mining is
-- configured or by the mempool noop-miner (which keeps the mempool updated) in
-- production setups.
--
data MinerResources logger cas = MinerResources
    { _minerResLogger :: !logger
    , _minerResCutDb :: !(CutDb cas)
    , _minerChainResources :: HashMap ChainId (ChainResources logger)
    , _minerResConfig :: !NodeMiningConfig
    , _minerResCoordination :: !(Maybe (MiningCoordination logger cas))
        -- ^ The primed work cache. This is Nothing when coordination is
        -- disabled. It is needed by the in-node test miner. The mempoolNoopMiner
        -- does not use it.
    }

withMinerResources
    :: logger
    -> NodeMiningConfig
    -> HashMap ChainId (ChainResources logger)
    -> CutDb cas
    -> Maybe (MiningCoordination logger cas)
    -> (Maybe (MinerResources logger cas) -> IO a)
    -> IO a
withMinerResources logger conf chainRes cutDb tpw inner =
    inner . Just $ MinerResources
        { _minerResLogger = logger
        , _minerResCutDb = cutDb
        , _minerChainResources = chainRes
        , _minerResConfig = conf
        , _minerResCoordination = tpw
        }

-- | This runs the internal in-node miner. It is only used during testing.
--
-- When mining coordination is disabled, this function exits with an error.
--
runMiner
    :: forall logger cas
    .  Logger logger
    => PayloadCasLookup cas
    => ChainwebVersion
    -> MinerResources logger cas
    -> IO ()
runMiner v mr
    | enabled = case _minerResCoordination mr of
        Nothing -> error
            "Mining coordination must be enabled in order to use the in-node test miner"
        Just coord -> case window v of
            Nothing -> testMiner coord
            Just _ -> powMiner coord
    | otherwise = mempoolNoopMiner lf (_chainResMempool <$> _minerChainResources mr)

  where
    enabled = _nodeMiningEnabled $ _minerResConfig mr

    cdb :: CutDb cas
    cdb = _minerResCutDb mr

    conf :: NodeMiningConfig
    conf = _minerResConfig mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    testMiner coord = do
        gen <- MWC.createSystemRandom
        localTest lf v coord (_nodeMiner conf) cdb gen (_nodeTestMiners conf)

    powMiner coord = localPOW lf v coord (_nodeMiner conf) cdb
