{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Lens
import Control.Monad

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Cut (_cutMap)
import Chainweb.CutDB (CutDb, awaitNewBlock, cutDbPactService, _cut)
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Miners
import Chainweb.Miner.Pact (Miner(..), minerId)
import Chainweb.Pact.Types
import Chainweb.Pact.Utils
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Utils.Logging.Trace

import Data.LogMessage (JsonLog(..), LogFunction)

import Numeric.AffineSpace

-- -------------------------------------------------------------------------- --
-- Miner

withMiningCoordination
    :: Logger logger
    => logger
    -> MiningConfig
    -> CutDb tbl
    -> (Maybe (MiningCoordination logger tbl) -> IO a)
    -> IO a
withMiningCoordination logger conf cdb inner
    | not (_coordinationEnabled coordConf) = inner Nothing
    | otherwise = do
        cut <- _cut cdb
        t <- newTVarIO mempty
        initialPw <- fmap (PrimedWork . HM.fromList) $
            forM miners $ \miner ->
                let mid = view minerId miner
                in fmap ((mid,) . HM.fromList) $
                    forM cids $ \cid -> do
                        let bh = fromMaybe (genesisBlockHeader v cid) (HM.lookup cid (_cutMap cut))
                        newBlock <- throwIfNoHistory =<< getPayload cid miner (ParentHeader bh)
                        return (cid, WorkReady newBlock)

        m <- newTVarIO initialPw
        c503 <- newIORef 0
        c403 <- newIORef 0
        l <- newIORef (_coordinationUpdateStreamLimit coordConf)
        fmap thd . runConcurrently $ (,,)
            <$> Concurrently (prune t m c503 c403)
            <*> Concurrently (mapConcurrently_ (primeWork m) cids)
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
    v = _chainwebVersion cdb

    cids :: [ChainId]
    cids = HS.toList (chainIds v)

    !miners = S.toList (_coordinationMiners coordConf)
        <> [ _nodeMiner inNodeConf | _nodeMiningEnabled inNodeConf ]

    chainLogger cid = addLabel ("chain", toText cid)

    -- we assume that this path always exists in PrimedWork and never delete it.
    workForMiner :: Miner -> ChainId -> Traversal' PrimedWork WorkState
    workForMiner miner cid = _Wrapped' . ix (view minerId miner) . ix cid

    periodicallyRefreshPayload :: TVar PrimedWork -> ChainId -> Miner -> IO a
    periodicallyRefreshPayload tpw cid ourMiner = forever $ do
        let delay =
                timeSpanToMicros $ _coordinationPayloadRefreshDelay coordConf
        threadDelay (fromIntegral @Micros @Int delay)
        when (not $ v ^. versionCheats . disablePact) $ do
            -- "stale" in the sense of not having all of the transactions
            -- that it could. it still has the latest possible parent
            mContinuableBlockInProgress <- atomically $ do
                primed <- readTVar tpw <&> (^?! workForMiner ourMiner cid)
                case primed of
                    WorkReady (NewBlockInProgress bip) -> return (Just bip)
                    WorkReady (NewBlockPayload {}) ->
                        error "periodicallyRefreshPayload: encountered NewBlockPayload in PrimedWork, which cannot be refreshed"
                    WorkAlreadyMined {} -> return Nothing
                    WorkStale -> return Nothing

            forM_ mContinuableBlockInProgress $ \continuableBlockInProgress -> do
                maybeNewBlock <- case continuableBlockInProgress of
                    ForPact4 block -> fmap ForPact4 <$> _pactContinueBlock pact cid block
                    ForPact5 block -> fmap ForPact5 <$> _pactContinueBlock pact cid block
                -- if continuing returns NoHistory then the parent header
                -- isn't available in the checkpointer right now.
                -- in that case we just mark the payload as not stale.
                let newBlock = case maybeNewBlock of
                        NoHistory -> continuableBlockInProgress
                        Historical b -> b

                logFunctionText (chainLogger cid logger) Debug
                    $ "refreshed block, old and new tx count: "
                    <> sshow
                        ( forAnyPactVersion (V.length . _transactionPairs . _blockInProgressTransactions) continuableBlockInProgress
                        , forAnyPactVersion (V.length . _transactionPairs . _blockInProgressTransactions) newBlock
                        )

                atomically $ modifyTVar' tpw $
                    workForMiner ourMiner cid .~ WorkReady (NewBlockInProgress newBlock)

    -- | THREAD: Keep a live-updated cache of Payloads for specific miners, such
    -- that when they request new work, the block can be instantly constructed
    -- without interacting with the Pact Queue.
    --
    primeWork :: TVar PrimedWork -> ChainId -> IO ()
    primeWork tpw cid =
        forConcurrently_ miners $ \miner ->
            runForever (logFunction (chainLogger cid logger)) "primeWork" (go miner)
        where
        go :: Miner -> IO ()
        go miner = do
            pw <- readTVarIO tpw
            let
                -- we assume that this path always exists in PrimedWork and never delete it.
                ourMiner :: Traversal' PrimedWork WorkState
                ourMiner = workForMiner miner cid
            let !outdatedPayload = fromJuste $ pw ^? ourMiner
            let outdatedParentHash = case outdatedPayload of
                    WorkReady outdatedBlock -> view _1 (newBlockParent outdatedBlock)
                    WorkAlreadyMined outdatedBlockHash -> outdatedBlockHash
                    WorkStale -> error "primeWork loop: Invariant Violation: Stale work should be an impossibility"

            newParent <- either ParentHeader id <$> race
                -- wait for a block different from what we've got primed work for
                (awaitNewBlock cdb cid outdatedParentHash)
                -- in the meantime, periodically refresh the payload to make sure
                -- it has all of the transactions it can have
                (periodicallyRefreshPayload tpw cid miner)

            -- Temporarily block this chain from being considered for queries
            atomically $ modifyTVar' tpw (ourMiner .~ WorkStale)

            -- Get a payload for the new block
            getPayload cid miner newParent >>= \case
                NoHistory -> do
                    logFunctionText (addLabel ("chain", toText cid) logger) Warn
                        "current block is not in the checkpointer; halting primed work loop temporarily"
                    approximateThreadDelay 1_000_000
                    atomically $ modifyTVar' tpw (ourMiner .~ outdatedPayload)
                Historical newBlock ->
                    atomically $ modifyTVar' tpw (ourMiner .~ WorkReady newBlock)

    getPayload :: ChainId -> Miner -> ParentHeader -> IO (Historical NewBlock)
    getPayload cid m ph =
        if v ^. versionCheats . disablePact
        -- if pact is disabled, we must keep track of the latest header
        -- ourselves. otherwise we use the header we get from newBlock as the
        -- real parent. newBlock may return a header in the past due to a race
        -- with rocksdb though that shouldn't cause a problem, just wasted work,
        -- see docs for
        -- Chainweb.Pact.PactService.Checkpointer.findLatestValidBlockHeader'
        then return $ Historical $
            NewBlockPayload ph emptyPayload
        else trace (logFunction (chainLogger cid logger))
            "Chainweb.Chainweb.MinerResources.withMiningCoordination.newBlock"
            () 1 (_pactNewBlock pact cid m NewBlockFill ph)

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

        g :: PayloadWithOutputs -> Int
        g = V.length . _payloadWithOutputsTransactions

-- | Miner resources are used by the test-miner when in-node mining is
-- configured or by the mempool noop-miner (which keeps the mempool updated) in
-- production setups.
--
data MinerResources logger tbl = MinerResources
    { _minerResLogger :: !logger
    , _minerResCutDb :: !(CutDb tbl)
    , _minerChainResources :: !(HashMap ChainId (ChainResources logger))
    , _minerResConfig :: !NodeMiningConfig
    , _minerResCoordination :: !(Maybe (MiningCoordination logger tbl))
        -- ^ The primed work cache. This is Nothing when coordination is
        -- disabled. It is needed by the in-node test miner. The mempoolNoopMiner
        -- does not use it.
    }

withMinerResources
    :: logger
    -> NodeMiningConfig
    -> HashMap ChainId (ChainResources logger)
    -> CutDb tbl
    -> Maybe (MiningCoordination logger tbl)
    -> (Maybe (MinerResources logger tbl) -> IO a)
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
    :: forall logger tbl
    .  Logger logger
    => CanReadablePayloadCas tbl
    => ChainwebVersion
    -> MinerResources logger tbl
    -> IO ()
runMiner v mr
    | enabled = case _minerResCoordination mr of
        Nothing -> error
            "Mining coordination must be enabled in order to use the in-node test miner"
        Just coord -> case v ^. versionCheats . disablePow of
            True -> testMiner coord
            False -> powMiner coord
    | otherwise = mempoolNoopMiner lf (_chainResMempool <$> _minerChainResources mr)

  where
    enabled = _nodeMiningEnabled $ _minerResConfig mr

    cdb :: CutDb tbl
    cdb = _minerResCutDb mr

    conf :: NodeMiningConfig
    conf = _minerResConfig mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    testMiner coord = do
        gen <- MWC.createSystemRandom
        localTest lf v coord (_nodeMiner conf) cdb gen (_nodeTestMiners conf)

    powMiner coord = localPOW lf coord (_nodeMiner conf) cdb
