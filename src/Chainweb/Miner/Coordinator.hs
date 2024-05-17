{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module: Chainweb.Miner.Coordinator
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--

module Chainweb.Miner.Coordinator
( -- * Types
  MiningState(..)
, miningState
, MiningStats(..)
, PrevTime(..)
, ChainChoice(..)
, PrimedWork(..)
, MiningCoordination(..)
, NoAsscociatedPayload(..)

-- * Mining API Functions
, work
, solve

-- ** Internal Functions
, publish

, withMiningCoordination
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar
import Control.DeepSeq (NFData)
import Control.Exception.Safe
import Control.Lens
import Control.Monad

import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.List(sort)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V

import GHC.Generics (Generic)
import GHC.Stack

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.Cut hiding (join)
import Chainweb.Cut.Create
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Logging.Miner
import Chainweb.Miner.Config
import Chainweb.Miner.Pact (Miner(..), MinerId(..), minerId)
import Chainweb.Pact.Utils
import Chainweb.Payload
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.LogMessage (JsonLog(..), LogFunction)
import Numeric.AffineSpace
import Utils.Logging.Trace (trace)

--
-- | Data shared between the mining threads represented by `newWork` and
-- `publish`.
--
-- The key is hash of the current block's payload.
--
newtype MiningState = MiningState
    { _miningState :: M.Map BlockPayloadHash (T3 Miner PayloadWithOutputs (Time Micros)) }
    deriving stock (Generic)
    deriving newtype (Semigroup, Monoid)

-- | For logging during `MiningState` manipulation.
--
data MiningStats = MiningStats
    { _statsCacheSize :: !Int
    , _stats503s :: !Int
    , _stats403s :: !Int
    , _statsAvgTxs :: !Int
    , _statsPrimedSize :: !Int }
    deriving stock (Generic)
    deriving anyclass (ToJSON, NFData)

makeLenses ''MiningState

-- -------------------------------------------------------------------------- --
-- Utils

-- | Lookup a 'BlockHeader' for a 'ChainId' in a cut and raise a meaningfull
-- error if the lookup fails.
--
-- Generally, failing lookup in a cut is a code invariant violation. In almost
-- all circumstances there should be a invariant in scope that guarantees that
-- the lookup succeeds. This function is useful when debugging corner cases of
-- new code logic, like graph changes.
--
lookupInCut :: HasCallStack => HasChainId cid => Cut -> cid -> BlockHeader
lookupInCut c cid
    | Just x <- lookupCutM cid c = x
    | otherwise = error $ T.unpack
        $ "Chainweb.Miner.Coordinator.lookupInCut: failed to lookup chain in cut."
        <> " Chain: " <> sshow (_chainId cid) <> "."
        <> " Cut Hashes: " <> encodeToText (cutToCutHashes Nothing c) <> "."

-- -------------------------------------------------------------------------- --
-- MiningCoordination

-- | For coordinating requests for work and mining solutions from remote Mining
-- Clients.
--
data MiningCoordination logger tbl = MiningCoordination
    { _coordLogger :: !logger
    , _coordCutDb :: !(CutDb tbl)
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
    -> MiningConfig
    -> CutDb tbl
    -> (Maybe (MiningCoordination logger tbl) -> IO a)
    -> IO a
withMiningCoordination logger conf cdb inner
    | not (_coordinationEnabled coordConf) = inner Nothing
    | otherwise = do
        initialCut <- _cut cdb
        t <- newTVarIO mempty
        initialPw <- fmap (PrimedWork . HM.fromList) $
            forM miners $ \miner ->
                let mid = view minerId miner
                in fmap ((mid,) . HM.fromList) $
                    forM cids $ \cid -> do
                        let bh = fromMaybe
                                (genesisBlockHeader v cid)
                                (HM.lookup cid (_cutMap initialCut))
                        fmap ((cid,) . over _2 Just) $
                            getPayload logger cdb (ParentHeader bh) cid miner
        m <- newTVarIO initialPw
        c503 <- newIORef 0
        c403 <- newIORef 0
        l <- newIORef (_coordinationUpdateStreamLimit coordConf)
        fmap thd . runConcurrently $ (,,)
            <$> Concurrently (prune t m c503 c403)
            <*> Concurrently (mapConcurrently_ (primeWork logger cdb miners m) cids)
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

    -- | THREAD: Periodically clear out the cached payloads kept for Mining
    -- Coordination.
    --
    -- This cache is used so that, when a miner returns a solved header to the
    -- node, the node knows the payload for that header.
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

-- | THREAD: Keep a live-updated cache of Payloads for specific miners, such
-- that when they request new work, the block can be instantly constructed
-- without interacting with the Pact Queue.
--
primeWork
    :: Logger logger
    => logger
    -> CutDb tbl
    -> [Miner]
    -> TVar PrimedWork
    -> ChainId
    -> IO ()
primeWork logger cdb miners tpw cid =
    forConcurrently_ miners $ \miner ->
        runForever (logFunction logger) "primeWork" (go miner)
  where
    go :: Miner -> IO ()
    go miner = do
        pw <- readTVarIO tpw
        let
            -- we assume that this path always exists in PrimedWork and never delete it.
            ourMiner :: Traversal' PrimedWork (T2 ParentHeader (Maybe PayloadWithOutputs))
            ourMiner = _Wrapped' . at (view minerId miner) . _Just . at cid . _Just
        let !(T2 ph _) = fromJuste $ pw ^? ourMiner
        -- wait for a block different from what we've got primed work for
        new <- awaitNewBlock cdb cid (_parentHeader ph)
        -- Temporarily block this chain from being considered for queries
        atomically $ modifyTVar' tpw (ourMiner . _2 .~ Nothing)
        -- Generate new payload for this miner
        newParentAndPayload <- getPayload logger cdb (ParentHeader new) cid miner
        atomically $ modifyTVar' tpw (ourMiner .~ over _2 Just newParentAndPayload)
        threadDelay 100_000

getPayload
    :: Logger logger
    => logger
    -> CutDb tbl
    -> ParentHeader
    -> ChainId
    -> Miner
    -> IO (T2 ParentHeader PayloadWithOutputs)
getPayload logger cdb new cid m =
    if v ^. versionCheats . disablePact
    -- if pact is disabled, we must keep track of the latest header
    -- ourselves. otherwise we use the header we get from newBlock as the
    -- real parent. newBlock may return a header in the past due to a race
    -- with rocksdb though that shouldn't cause a problem, just wasted work,
    -- see docs for
    -- Chainweb.Pact.PactService.Checkpointer.findLatestValidBlockHeader';
    -- still, this race is enough that the primed work loop should pause after
    -- producing a new block
    then return $ T2 new emptyPayload
    else
        catchAny
            (trace (logFunction logger)
            "Chainweb.Chainweb.MinerResources.withMiningCoordination.newBlock"
            () 1 (_pactNewBlock pact cid m))
            $ \e -> do
                logFunctionText logger Error $
                    "ERROR: HALTING MINING LOOP. Exception occurred producing block: " <> sshow e
                throwIO e
    where
    v = _chainwebVersion new

    pact = _webPactExecutionService $ view cutDbPactService cdb


-- | Precached payloads for Private Miners. This allows new work requests to be
-- made as often as desired, without clogging the Pact queue.
--
newtype PrimedWork =
    PrimedWork (HM.HashMap MinerId (HM.HashMap ChainId (T2 ParentHeader (Maybe PayloadWithOutputs))))
    deriving newtype (Semigroup, Monoid)
    deriving stock Generic
    deriving anyclass (Wrapped)

resetPrimed :: MinerId -> ChainId -> PrimedWork -> PrimedWork
resetPrimed mid cid (PrimedWork pw) = PrimedWork
    $! HM.adjust (HM.adjust (_2 .~ Nothing) cid) mid pw

-- | The `BlockCreationTime` of the parent of some current, "working"
-- `BlockHeader`.
--
newtype PrevTime = PrevTime BlockCreationTime

data ChainChoice = Anything | TriedLast !ChainId | Suggestion !ChainId

-- | Construct a new `BlockHeader` to mine on.
--
newWork
    :: LogFunction
    -> ChainChoice
    -> Miner
    -> WebBlockHeaderDb
        -- ^ this is used to lookup parent headers that are not in the cut
        -- itself.
    -> PactExecutionService
    -> TVar PrimedWork
    -> Cut
    -> IO (Maybe (T2 WorkHeader PayloadWithOutputs))
newWork logFun choice eminer@(Miner mid _) hdb pact tpw c = do

    -- Randomly pick a chain to mine on. we no longer support the caller
    -- specifying any particular one.
    --
    cid <- case choice of
        Anything -> randomChainIdAt c (minChainHeight c)
        Suggestion cid' -> pure cid'
        TriedLast _ -> randomChainIdAt c (minChainHeight c)
    logFun @T.Text Debug $ "newWork: picked chain " <> toText cid

    -- wait until at least one chain has primed work. we don't wait until *our*
    -- chain has primed work, because if other chains have primed work, we want
    -- to loop and select one of those chains. it is not a normal situation to
    -- have no chains with primed work if there are more than a couple chains.
    mpw <- atomically $ do
        PrimedWork pw <- readTVar tpw
        mpw <- maybe retry return (HM.lookup mid pw)
        guard (any (isJust . ssnd) mpw)
        return mpw
    let mr = T2
            <$> HM.lookup cid mpw
            <*> getCutExtension c cid

    case mr of
        Just (T2 (T2 _ Nothing) _) -> do
            logFun @T.Text Debug $ "newWork: chain " <> toText cid <> " has stale work"
            newWork logFun Anything eminer hdb pact tpw c
        Nothing -> do
            logFun @T.Text Debug $ "newWork: chain " <> toText cid <> " not mineable"
            newWork logFun Anything eminer hdb pact tpw c
        Just (T2 (T2 (ParentHeader primedParent) (Just payload)) extension)
            | _blockHash primedParent == _blockHash (_parentHeader (_cutExtensionParent extension)) -> do
                let !phash = _payloadWithOutputsPayloadHash payload
                !wh <- newWorkHeader hdb extension phash
                pure $ Just $ T2 wh payload
            | otherwise -> do
                -- The cut is too old or the primed work is outdated. Probably
                -- the former because it the mining coordination background job
                -- is updating the primed work cache regularly. We could try
                -- another chain, but it's safer to just return 'Nothing' here
                -- and retry with an updated cut.
                --
                let !extensionParent = _parentHeader (_cutExtensionParent extension)
                logFun @T.Text Info
                    $ "newWork: chain " <> toText cid <> " not mineable because of parent header mismatch"
                    <> ". Primed parent hash: " <> toText (_blockHash primedParent)
                    <> ". Primed parent height: " <> sshow (_blockHeight primedParent)
                    <> ". Extension parent: " <> toText (_blockHash extensionParent)
                    <> ". Extension height: " <> sshow (_blockHeight extensionParent)

                return Nothing

-- | Accepts a "solved" `BlockHeader` from some external source (e.g. a remote
-- mining client), attempts to reassociate it with the current best `Cut`, and
-- publishes the result to the `Cut` network.
--
-- There are a number of "fail fast" conditions which will kill the candidate
-- `BlockHeader` before it enters the Cut pipeline.
--
publish
    :: LogFunction
    -> CutDb tbl
    -> TVar PrimedWork
    -> MinerId
    -> PayloadWithOutputs
    -> SolvedWork
    -> IO ()
publish lf cdb pwVar miner pwo s = do
    c <- _cut cdb
    now <- getCurrentTimeIntegral
    try (extend c pwo s) >>= \case

        -- Publish CutHashes to CutDb and log success
        Right (bh, Just ch) -> do

            -- reset the primed payload for this cut extension
            atomically $ modifyTVar pwVar $ resetPrimed miner (_chainId bh)
            addCutHashes cdb ch

            let bytes = sum . fmap (BS.length . _transactionBytes . fst) $
                        _payloadWithOutputsTransactions pwo
            lf Info $ JsonLog $ NewMinedBlock
                { _minedBlockHeader = ObjectEncoded bh
                , _minedBlockTrans = int . V.length $ _payloadWithOutputsTransactions pwo
                , _minedBlockSize = int bytes
                , _minedBlockMiner = _minerId miner
                , _minedBlockDiscoveredAt = now
                }

        -- Log Orphaned Block
        Right (bh, Nothing) -> do
            let !p = lookupInCut c bh
            lf Info $ orphandMsg now p bh "orphaned solution"

        -- Log failure and rethrow
        Left e@(InvalidSolvedHeader bh msg) -> do
            let !p = lookupInCut c bh
            lf Info $ orphandMsg now p bh msg
            throwM e
  where
    orphandMsg now p bh msg = JsonLog OrphanedBlock
        { _orphanedHeader = ObjectEncoded bh
        , _orphanedBestOnCut = ObjectEncoded p
        , _orphanedDiscoveredAt = now
        , _orphanedMiner = _minerId miner
        , _orphanedReason = msg
        }

-- -------------------------------------------------------------------------- --
-- Mining API

-- | Get new work
--
-- This function does not check if the miner is authorized. If the miner doesn't
-- yet exist in the primed work cache it is added.
--
work
    :: forall l tbl
    .  Logger l
    => MiningCoordination l tbl
    -> Maybe ChainId
    -> Miner
    -> IO WorkHeader
work mr mcid m = do
    T2 wh pwo <-
        withAsync (logDelays False 0) $ \_ -> newWorkForCut
    now <- getCurrentTimeIntegral
    atomically
        . modifyTVar' (_coordState mr)
        . over miningState
        . M.insert (_payloadWithOutputsPayloadHash pwo)
        $ T3 m pwo now
    return wh
  where
    -- here we log the case that the work loop has stalled.
    logDelays :: Bool -> Int -> IO ()
    logDelays loggedOnce n = do
        if loggedOnce
        then threadDelay 60_000_000
        else threadDelay 10_000_000
        let !n' = n + 1
        PrimedWork primedWork <- readTVarIO (_coordPrimedWork mr)
        -- technically this is in a race with the newWorkForCut function,
        -- which is likely benign when the mining loop has stalled for 10 seconds.
        currentCut <- _cut cdb
        let primedWorkMsg =
                case HM.lookup (view minerId m) primedWork of
                    Nothing ->
                        "no primed work for miner key" <> sshow m
                    Just mpw ->
                        let chainsWithBlocks = HS.fromMap $ flip HM.mapMaybe mpw $ \case
                                T2 _ (Just _) -> Just ()
                                _ -> Nothing
                        in if
                            | HS.null chainsWithBlocks ->
                                "no chains have primed blocks"
                            | cids == chainsWithBlocks ->
                                "all chains have primed blocks"
                            | otherwise ->
                                "chains with primed blocks may be stalled. chains with primed work: "
                                <> sshow (toText <$> sort (HS.toList chainsWithBlocks))
        let extensibleChains =
                HS.fromList $ mapMaybe (\cid -> cid <$ getCutExtension currentCut cid) $ HS.toList cids
        let extensibleChainsMsg =
                if HS.null extensibleChains
                then "no chains are extensible in the current cut! here it is: " <> sshow currentCut
                else "the following chains can be extended in the current cut: " <> sshow (toText <$> HS.toList extensibleChains)
        logf @T.Text Warn $
          "findWork: stalled for " <>
          (
          if loggedOnce
          then "10s"
          else sshow n' <> "m"
          ) <>
          ". " <> primedWorkMsg <> ". " <> extensibleChainsMsg

        logDelays True n'

    v  = _chainwebVersion hdb
    cids = chainIds v

    -- There is no strict synchronization between the primed work cache and the
    -- new work selection. There is a chance that work selection picks a primed
    -- work that is out of sync with the current cut. In that case we just try
    -- again with a new cut. In case the cut was good but the primed work was
    -- outdated, chances are that in the next attempt we pick a different chain
    -- with update work or that the primed work cache caught up in the meantime.
    --
    newWorkForCut = do
        c' <- _cut cdb
        newWork logf choice m hdb pact (_coordPrimedWork mr) c' >>= \case
            Nothing -> newWorkForCut
            Just x -> return x

    logf :: LogFunction
    logf = logFunction $ _coordLogger mr

    hdb :: WebBlockHeaderDb
    hdb = view cutDbWebBlockHeaderDb cdb

    choice :: ChainChoice
    choice = maybe Anything Suggestion mcid

    cdb :: CutDb tbl
    cdb = _coordCutDb mr

    pact :: PactExecutionService
    pact = _webPactExecutionService $ view cutDbPactService cdb

data NoAsscociatedPayload = NoAsscociatedPayload
    deriving (Show, Eq)

instance Exception NoAsscociatedPayload

solve
    :: forall l tbl
    . Logger l
    => MiningCoordination l tbl
    -> SolvedWork
    -> IO ()
solve mr solved@(SolvedWork hdr) = do
    -- Fail Early: If a `BlockHeader` comes in that isn't associated with any
    -- Payload we know about, reject it.
    --
    MiningState ms <- readTVarIO tms
    case M.lookup key ms of
        Nothing -> throwM NoAsscociatedPayload
        Just x -> publishWork x `finally` deleteKey
            -- There is a race here, but we don't care if the same cut
            -- is published twice. There is also the risk that an item
            -- doesn't get deleted. Items get GCed on a regular basis by
            -- the coordinator.
  where
    key = _blockPayloadHash hdr
    tms = _coordState mr

    lf :: LogFunction
    lf = logFunction $ _coordLogger mr

    deleteKey = atomically . modifyTVar' tms . over miningState $ M.delete key
    publishWork (T3 m pwo _) =
        publish lf (_coordCutDb mr) (_coordPrimedWork mr) (view minerId m) pwo solved
