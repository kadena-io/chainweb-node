{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Miner.Coordinator
-- Copyright: Copyright © 2018 - 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
module Chainweb.Miner.Coordinator
( -- * Mining Coordination API
  MiningCoordination(..)
, NoAsscociatedPayload(..)
, runCoordination
, newMiningCoordination
, work
, solve

-- * Internal

-- * WorkState
, WorkState(..)
, awaitLatestPayloadForWorkStateSTM

-- ** Payload Caches
, type PayloadCaches
, newPayloadCaches
, awaitPayloadsNext

-- ** MiningState
, updateForCut
, updateForSolved

-- ** Delivery
, randomWork

) where

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainValue
import Chainweb.Core.Brief
import Chainweb.Cut hiding (join)
import Chainweb.Cut.Create
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Logging.Miner
import Chainweb.Miner.Config
import Chainweb.Miner.PayloadCache
import Chainweb.Parent
import Chainweb.PayloadProvider
import Chainweb.Storage.Table
import Chainweb.Time (Micros(..), getCurrentTimeIntegral)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically, STM, retry)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable
import Data.LogMessage (JsonLog(..), LogFunction, LogFunctionText)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Stack
import Numeric.Natural
import Streaming.Prelude qualified as S
import System.LogLevel (LogLevel(..))
import System.Random (randomRIO)

-- -------------------------------------------------------------------------- --
-- Utils

type WorkParentsId = (Parent BlockHash, AdjacentsHash)

parentsId :: WorkParents -> WorkParentsId
parentsId ps =
    ( view blockHash <$> _workParent ps
    , adjacentsHash (_workParentsAdjacentHashes ps)
    )

solvedId :: SolvedWork -> WorkParentsId
solvedId s =
    ( _solvedParentHash s
    , _solvedAdjacentHash s
    )

-- -------------------------------------------------------------------------- --
-- Payload Caches

type PayloadCaches = ChainMap PayloadCache

newPayloadCaches
    :: HasChainwebVersion v
    => HasChainGraph v
    => v
    -> IO PayloadCaches
newPayloadCaches v = tabulateChainsM (_chainwebVersion v) (\_ -> newIO depth)
  where
    -- FIXME: Make this configurable?
    depth :: Natural
    depth = diameter (_chainGraph v)

-- | Await the next payload for a cut that is different from the latest payload.
--
-- FIXME: can this race if some chains get new payloads at a very high rate?
-- How can we make this fair? Maybe shuffle xs?
--
awaitPayloadsNext
    :: PayloadCaches
    -> Cut
        -- ^ The cut for which the new payload is awaited.
    -> V.Vector Int
        -- ^ The hash/fingerprint of the previous value for each chain.
    -> STM NewPayload
awaitPayloadsNext caches c prevs = msum (awaitChain <$> itoList xs)
  where
    xs = chainIntersect (,) caches rhs
    rhs = ChainMap $ Parent . _rankedBlockHash <$> _cutHeaders c

    awaitChain (ChainId cid, (ca, rh)) = do
        x <- awaitLatestSTM ca rh
        -- unsafeIndex relies on caches prevs being as least as long as caches
        -- and chainids being a prefix of the natural numbers.
        guard (hash x /= V.unsafeIndex prevs(int cid))
        return x

-- -------------------------------------------------------------------------- --
-- Work State

-- | The mining work state of a chain.
--
-- Two things must happen for work to be ready:
--
-- 1. The payload provider must provide a 'NewPayload' and
-- 2. Consensus must provide a cut on which the chain is mineable.
--
-- When both is available a 'WorkHeader' can be created.
--
-- We call a chain "blocked" when it can not be mined in the current cut because
-- some adjacent header is missing for a correctly braided extension of the
-- chain.
--
-- We call a chain "stale" when there is no payload available for extending the
-- chain with a new block.
--
-- We call a chain "not ready" if a chain is "blocked" and "stale". We call a
-- chain "ready" when it is neither "blocked" nor "stale".
--
-- We call a chain "solved" if new block has been mined on the chain for the
-- current cut, but the cut has not yet been updated to include the new block.
--
-- The following demonstrates how cuts are extended:
--
-- - n: new work
-- - p: work parent
-- - a: work adjacent parent
--
-- @
--   *   n
--   | x | \
--   a   p   a
--   | x | x |
--   *   *   *
-- @
--
-- The extension of indiviual chains in cuts is non-monotonic. It is possible
-- that the parent header remains the same but the adjacent parents change. It
-- is also possible that stale work becomes NotReady or that ready work becomes
-- blocked. Similarly, solved work can become blocked again. If the parent
-- header stayls the same the same payload may be used again. Only if the parent
-- header changes the a new (or an previously used) payload must be obtained.
--
-- The transition function for the work state machine is as follows:
--
-- @
--   "" -> NotReady [label="header"];
--   NotReady -> Blocked [label=payload];
--   NotReady -> Stale [label=unblock];
--   Blocked -> Ready [label=unblock];
--   Blocked -> Blocked [label=payload];
--   Stale -> Ready [label=payload];
--   Stale -> NotReady [label=block];
--   Stale -> Stale [label=unblock];
--   Ready -> Solved [label=solve];
--   Ready -> Blocked [label=block];
--   Ready -> Ready [label="payload,unblock"];
--   Solved -> Blocked [label=block];
--   Solved -> Ready [label=unblock];
--   Solved -> Solved [label=payload];
-- @
--
data WorkState = WorkState
    { workStateParents :: !WorkParents
    -- ^ Invariant: The work parents must match the ranked block hash.
    , workStateSolved :: !(Maybe SolvedWork)
    -- ^ A block with these parents has already been solved and submitted to the
    --   cut pipeline - we don't want to mine it again. So we wait until the
    --   current cut is updated with a new parent header for this chain.
    }
    deriving stock (Show, Eq, Generic)

awaitLatestPayloadForWorkStateSTM :: PayloadCache -> WorkState -> STM NewPayload
awaitLatestPayloadForWorkStateSTM payloadCache workState = do
    let parent = fmap (view rankedBlockHash) $ _workParent' $ workStateParents workState
    awaitLatestSTM payloadCache parent

instance Brief WorkState where
    brief WorkState{..} =
            "WorkState:" <> brief workStateParents
            <> ":solved=" <> brief workStateSolved

-- -------------------------------------------------------------------------- --
-- Mining State

newMiningState :: Cut -> IO (ChainMap (TVar (Maybe WorkState)))
newMiningState c = do
    states <- forM cids $ \cid -> do
        var <- newTVarIO Nothing
        return (cid, var)
    return $! onChains states
  where
    v = _chainwebVersion c

    cids :: [ChainId]
    cids = HS.toList (chainIds v)

-- TODO: consider storing the mining state more efficiently:
--
-- Do not recompute cut extensions more often than needed.
--
-- * store the current cut
-- * when a new cut arrive compute which chains need update

-- | Update work state for all chains for a new cut.
--
updateForCut
    :: LogFunctionText
    -> (ChainValue BlockHash -> IO BlockHeader)
    -> (ChainMap (TVar (Maybe WorkState)))
    -> Cut
    -> IO ()
updateForCut lf hdb ms c = do
    iforM_ ms $ \cid var ->
        forChain cid var
  where
    forChain cid var =  do
        maybeNewParents <- workParents hdb c cid
        atomically $ do
            maybeOldWorkState <- readTVar var
            case (maybeOldWorkState, maybeNewParents) of
                (_, Nothing) ->
                    writeTVar var Nothing
                (Just oldWorkState, Just newParents)
                    | workStateParents oldWorkState == newParents
                        -> return ()
                (_, Just newParents) ->
                    writeTVar var $ Just
                        WorkState
                            { workStateParents = newParents
                            , workStateSolved = Nothing
                            }

updateForSolved :: LogFunction -> CutDb -> PayloadCache -> TVar (Maybe WorkState) -> SolvedWork -> IO ()
updateForSolved lf cdb payloadCache var sw = do
    stateOrErr <- runExceptT $ do
        solvedWorkState <- mapExceptT atomically $ do
            lift (readTVar var) >>= \case
                Just workState
                    | solvedId sw /= parentsId (workStateParents workState) ->
                        throwError (Just workState, Info, "orphaned; this block is for an older parent set")
                    | Just _ <- workStateSolved workState ->
                        throwError (Just workState, Debug, "this block has already been solved")
                    | otherwise -> do
                        let newState = workState { workStateSolved = Just sw }
                        -- speculatively set the work state's solved work. if we have
                        -- trouble integrating this block, we will revert this after.
                        lift (writeTVar var (Just newState))
                        return newState
                -- orphaned by parent disappearance
                Nothing ->
                    throwError (Nothing, Info, "orphaned; this block is for a blocked chain")

        c <- lift $ _cut cdb
        lift (lookupIO payloadCache (fmap (view rankedBlockHash) $ _workParent $ workStateParents solvedWorkState) (_solvedPayloadHash sw)) >>= \case
            Nothing -> throwError (Just solvedWorkState, Warn, "updateForSolved: missing payload in cache: " <> brief solvedWorkState)
            Just payload -> do
                let pld = _newPayloadEncodedPayloadData payload
                let pwo = _newPayloadEncodedPayloadOutputs payload

                try (extend c pld pwo (workStateParents solvedWorkState) sw) >>= \case

                    -- Publish CutHashes to CutDb and log success
                    Right (bh, Just ch) -> do
                        lift $ publish cdb ch
                        lift $ logMinedBlock lf bh payload
                        return solvedWorkState

                    -- Log Orphaned Block
                    Right (_, Nothing) -> do
                        throwError (Just solvedWorkState, Info, "orphaned; this block is for an older parent set")

                    Left (InvalidSolvedHeader msg) -> do
                        throwError (Just solvedWorkState, Warn, "invalid solved header: " <> msg)

    case stateOrErr of
        Right s' -> lf Info $ "updateForSolved: new block solved: " <> brief s'
        Left (staleMaybeWorkState, level, err) -> do
            lf level $ "updateForSolved: block not solved: " <> err
            -- an error has occurred when trying to integrate this block;
            -- reset the work state's solved work, if the parents have not
            -- changed since the failure.
            forM_ staleMaybeWorkState $ \staleWorkState -> atomically $ do
                latestMaybeWorkState <- readTVar var
                case latestMaybeWorkState of
                    Just latestWorkState
                        | parentsId (workStateParents staleWorkState)
                            == parentsId (workStateParents latestWorkState)
                        -> writeTVar var $ Just latestWorkState { workStateSolved = Nothing }
                    _ -> return ()
            now <- getCurrentTimeIntegral
            lf Info $ orphandMsg now err
    where

    orphandMsg now msg = JsonLog OrphanedBlock
        { _orphanedParent = _solvedParentHash sw
        , _orphanedPayloadHash = _solvedPayloadHash sw
        , _orphanedDiscoveredAt = now
        , _orphanedReason = msg
        }

-- -------------------------------------------------------------------------- --
-- MiningCoordination

-- | For coordinating requests for work and mining solutions from remote Mining
-- Clients.
--
data MiningCoordination logger = MiningCoordination
    { _coordLogger :: !logger
    , _coordCutDb :: !CutDb
    , _coordState :: !(ChainMap (TVar (Maybe WorkState)))
    , _coordConf :: !CoordinationConfig
    , _coordPayloadCache :: !PayloadCaches
    }
instance HasChainwebVersion (MiningCoordination logger) where
    _chainwebVersion = _chainwebVersion . _coordCutDb

newMiningCoordination
    :: Logger logger
    => logger
    -> CoordinationConfig
    -> CutDb
    -> IO (MiningCoordination logger)
newMiningCoordination logger conf cdb = do
    c <- _cut cdb
    state <- newMiningState c
    caches <- newPayloadCaches c
    return $ MiningCoordination
        { _coordLogger = logger
        , _coordCutDb = cdb
        , _coordState = state
        , _coordConf = conf
        , _coordPayloadCache = caches
        }

-- | Listen on the cut stream and payloads stream and update the mining state
-- accordingly.
--
-- FIXME: be more concrete about the following:
--
-- Updates are intentionally not transactional. We value low latencies and and
-- progress over consistency with respect to the latest state of the payload
-- caches or cut pipeline. However, individual payload states are internally
-- consistent.
--
-- It can thus happen that updates to payloads and cuts are lost due to races.
-- This is supposed to be rare.
--
runCoordination
    :: forall l
    .  Logger l
    => MiningCoordination l
    -> IO ()
runCoordination mr = do
    -- Initialize Work State for provider caches, without this isolated networks
    -- fail to start mining.
    initializeState

    concurrentlies_
        $ updateWork
        : toList (imap updateCache caches)
  where
    lf :: LogFunctionText
    lf = logFunction $ _coordLogger mr

    caches = _coordPayloadCache mr
    state = _coordState mr

    hdb :: WebBlockHeaderDb
    hdb = view cutDbWebBlockHeaderDb (_coordCutDb mr)

    f :: ChainValue BlockHash -> IO BlockHeader
    f = fmap _chainValueValue . casLookupM hdb

    cdb = _coordCutDb mr

    providers = view cutDbPayloadProviders $ _coordCutDb mr
    withProvider :: ChainId -> (forall p. PayloadProvider p => p -> a) -> a
    withProvider cid k = case providers ^?! atChain cid of
        ConfiguredPayloadProvider p -> k p
        DisabledPayloadProvider ->
            error $ "payload provider disabled on chain " <> sshow cid <> ", which is illegal for miners"

    -- Update the payload cache with the latest payloads from the the provider
    --
    updateCache cid cache =
        withProvider cid $ \provider ->
        runForever lf label $ do
        payloadStream provider
            & S.chain (\_ -> lf Info $ "update cache on chain " <> toText cid)
            & S.mapM_ (insertIO cache)
        where
        label =  "miningCoordination.updateCache." <> toText cid

    -- Update the work state
    --
    updateWork = runForever lf "miningCoordination" $ do
        lf Info "start updateWork event stream"
        eventStream cdb caches
            & S.chain (\e -> lf Info $ "coordination event: " <> brief e)
            & S.mapM_ \case
                CutEvent c -> updateForCut lf f state c
                NewPayloadEvent _ -> return ()
                -- There is a race with solved events. Does it matter?
                -- We could synchronize those by delivering those via an
                -- STM variable, too.
                -- TODO: is there still?

    -- FIXME: this is probably more aggressive than needed
    initializeState = do
        lf Info $ "initialize mining state"
        forConcurrently_ (itoList caches) $ \(cid, cache) -> do
            lf Info $ "initialize mining state for chain " <> brief cid
            pld <- withProvider cid latestPayloadIO
            lf Info $ "got latest payload for chain " <> brief cid
            insertIO cache pld
        curCut <- _cut $ cdb
        updateForCut lf f state curCut
        lf Info "done initializing mining state for all chains"

-- | Note that this stream is lossy. It always delivers the latest available
-- item and skips over any previous items that have not been consumed.
--
-- We want this behavior, because we want to alway operate on the latest
-- available cut and payload.
--
-- FIXME: would it be better to use a mutable vector in an IORef (or TVar, if
-- supported)? It would probably be more efficient.
--
eventStream
    :: MonadIO m
    => CutDb
    -> PayloadCaches
    -> S.Stream (S.Of MiningStateEvent) m r
eventStream cdb caches = do
    liftIO (_cut cdb) >>= \c -> do
        S.yield (CutEvent c)

        -- Initialze the vector with the previous NewPayload fingerprints with
        -- all 0 hashes.
        go c (V.replicate (length caches) 0)
  where
    go cur prevs = do
        timeout <- liftIO $ registerDelay 1_000_000
        new <- liftIO $ atomically $ do
            CutEvent cur <$ (readTVar timeout >>= guard)
            <|>
            awaitEvent cdb caches cur prevs
        S.yield new
        case new of
            CutEvent c -> go c prevs
            NewPayloadEvent p ->
                go cur (V.unsafeUpd prevs [(chainIdInt (_chainId p), hash p)])

data MiningStateEvent
    = CutEvent Cut
    | NewPayloadEvent NewPayload
    deriving (Show, Eq, Generic)

instance Brief MiningStateEvent where
    brief (CutEvent c) = "CutEvent::" <> brief c
    brief (NewPayloadEvent p) = "NewPayloadEvent::" <> brief p

-- | NOTE: Cut and payload event race, with precedence given to cuts. If cut
-- arrive at a rate faster than they can be consumed, no payloads are going to
-- be delivered and work will always be 'Stale'.
--
-- On average cuts arrive at a rate of 1.5 seconds, which is plenty of time to
-- process payload events in between. However, we should monitor for this
-- condition and either warn or throttle cut processing if it ever happens.
--
awaitEvent
    :: CutDb
    -> PayloadCaches
    -> Cut
    -> V.Vector Int
    -> STM MiningStateEvent
awaitEvent cdb caches c p =
    CutEvent <$> awaitNewCutStm cdb c
    <|>
    NewPayloadEvent <$> awaitPayloadsNext caches c p

-- -------------------------------------------------------------------------- --
-- Work Delivery Strategy
-- -------------------------------------------------------------------------- --

-- | Get Work from a random Chain.
--
-- In a chainweb it can never happen that all chains are blocked at the same
-- time. Therefore, if there is no work ready, some chains must be stale. This
-- means that either
--
-- 1.  mining is disableled for some payload providers,
-- 2.  consensus did not request new payloads from providers in the latest
--    'syncToBlock' calls,
-- 3. some payload providers are deadlocked, or
-- 4. some payload providers are very slow in producing new payloads.
--
randomWork :: LogFunction -> PayloadCaches -> ChainMap (TVar (Maybe WorkState)) -> IO MiningWork
randomWork logFun caches state = do

    -- Pick a random chain.
    --
    -- This is done by picking a random start index and traversing the work
    -- states for all chains in order until ready work is found.
    --
    -- Before a graph transition (from when the new graph is declared until it
    -- goes into effect) this code is somewhat inefficient, because the new
    -- chains do already exist but are always blocked. This means that sometimes
    -- the algorithm has to iterate over all new chains to find the next chain
    -- that has work ready. We could prune the random range and search space,
    -- but at this, point the slight, temporary overhead seems acceptable.
    --
    n <- randomRIO (0, length state)

    -- NOTE: it is tempting to search for a matching chain within a single STM
    -- transaction. However, that is problematic: the search restarts when the
    -- work for a chain is updated, which could result in redundant and possibly
    -- unbound spinning. Even worse, it could also skew the result due to some
    -- chains being more likely to trigger a retry than others.
    --
    -- The actual implemention is not transactional but the returned work will
    -- still be up to date for the chain. There might be a risk for a small bias
    -- towards chains for which block are produced more quickly, but we think,
    -- that it is negligible.
    --
    let (s0, s1) = splitAt n (itoList state)
    go (s1 <> s0)
  where
    awaitWorkReady :: ChainId -> TVar (Maybe WorkState) -> STM (WorkParents, NewPayload)
    awaitWorkReady cid var = do
        workState <- maybe retry return =<< readTVar var
        payload <- awaitLatestPayloadForWorkStateSTM (caches ^?! atChain cid) workState
        return (workStateParents workState, payload)

    go [] = do

        logFun @T.Text Warn $ "randomWork: no work is ready. Awaiting work"

        -- We shall check for the following conditions:
        --
        -- 1.  No chain is ready and we haven't received neither new cuts nor
        --     new payloads. This is some sort of deadlock in the system.
        --
        -- 2.  We get new cuts (i.e. chains become unblocked and blocked), but
        --     no chain becomes ready. Either payload providers are
        --     misconfigured or consensus does not request payload creation.
        --
        --     It is also possible that payload production is too slow and
        --     chains get updated before a value payload is produced
        --
        -- We shall detect the problem by waiting for the respective condition
        -- and return the info from the STM loop.
        --
        -- We may also retry before we fail definitely.
        --
        -- There is a small risk that work gets updated to often and this check
        -- spins. But this is very unlikely, because otherwise we wouldn't have
        -- gotten stuck in first place.
        --
        -- There is a (small) chance that the overall system got too hot, i.e.
        -- block are produced too fast for this node to keep up. But the fact
        -- that blocks are produced means other mining nodes can keep up. This
        -- node will recover, once DA causes the system to cool down.
        --
        timeoutVar <- registerDelay (int staleMiningStateDelay)
        w <- atomically $
            Right <$> msum (imap awaitWorkReady state) <|> awaitTimeout timeoutVar
        case w of
            Right (ps, npld) -> do
                ct <- BlockCreationTime <$> getCurrentTimeIntegral
                return $ newWork ct ps (_newPayloadBlockPayloadHash npld)
            Left e -> error e -- FIXME: throw a proper exception and log what is going on

    go ((cid, var):t) = do
        readyCheck <- atomically $
            (do
                workState <- readTVar var >>= \case
                    Just workState
                        | Nothing <- workStateSolved workState ->
                        -- ^ solved chains are not ready to mine on yet
                        return workState
                    _ -> retry
                payload <- awaitLatestPayloadForWorkStateSTM (caches ^?! atChain cid) workState
                return $ Just (workStateParents workState, payload)
            ) <|> pure Nothing
        case readyCheck of
            Just (parents, payload) -> do
                ct <- BlockCreationTime <$> getCurrentTimeIntegral
                logFun @T.Text Debug $ "randomWork: picked chain " <> brief cid
                return $ newWork ct parents (_newPayloadBlockPayloadHash payload)
            Nothing -> do
                logFun @T.Text Info $ "randomWork: not ready for " <> brief cid
                go t

    awaitTimeout var = do

        -- this retries until the timeout triggers
        readTVar var >>= guard

        -- FIXME:
        -- Try to find out what happend:
        --
        -- if all chains are not ready or stale we did not get any payload
        -- We log a warning and retry
        --
        -- if all chains are not ready or blocked we have a consensus problem
        -- This is clearly an error, probably even a bug.
        -- We should try to find out what happend.
        --
        return $ Left "Chainweb.Miner.Coordinator.randomWork: timeout while waiting for work to become ready"

staleMiningStateDelay :: Micros
staleMiningStateDelay = 2_000_000

-- | This is the legacy work delivery API
--
work
    :: forall l
    .  Logger l
    => MiningCoordination l
    -> IO MiningWork
work mr = randomWork lf (_coordPayloadCache mr) (_coordState mr)
  where
    lf :: LogFunction
    lf = logFunction $ _coordLogger mr

-- -------------------------------------------------------------------------- --
-- Solve Work

data NoAsscociatedPayload = NoAsscociatedPayload
    deriving (Show, Eq)

instance Exception NoAsscociatedPayload

-- | Accepts a "solved" `BlockHeader` from some external source (e.g. a remote
-- mining client).
--
-- It looks up the payload of the solved header from the payload cache and
-- attempts to reassociate it with the current best `Cut`.
--
-- When the solved work is still valid it is marked as solved in the mining
-- state, logged, injected into the local cut pipline, and finally published to
-- the cut P2P network.
--
-- There are a number of "fail fast" conditions which will kill the candidate
-- `BlockHeader` before it enters the Cut pipeline.
--
-- NEW DOC:
--
-- First we check wether the state is still up to date. If the parents in the
-- mining state for the chain do not match the parents of  the solved work, we
-- can't build a header for it and reject it.
--
-- Next we lookup the payload in the cache. This is expected to succeed. It is
-- an exceptional case if it does not.
--
-- We then try to extend the current cut. This can fail if the current cut
-- changed asynchronously. If it fails we log an orphaned block.
--
-- If the cut is extended successfully we publish the new cut and updated
-- the mining state. This can race with other updates to the mining state.
-- However, at this point we don't care.
-- (Should we remove the solved state all together?)
--
solve
    :: forall l
    . Logger l
    => MiningCoordination l
    -> SolvedWork
    -> IO ()
solve mr solved =
    updateForSolved lf (_coordCutDb mr) (_coordPayloadCache mr ^?! atChain cid) (_coordState mr ^?! atChain cid) solved
  where
    cid = _chainId solved

    lf :: LogFunction
    lf = logFunction $ _coordLogger mr

logMinedBlock
    :: LogFunction
    -> BlockHeader
    -> NewPayload
    -> IO ()
logMinedBlock lf bh np = do
    now <- getCurrentTimeIntegral
    lf Info $ JsonLog $ NewMinedBlock
        { _minedBlockHeader = ObjectEncoded bh
        , _minedBlockTrans = _newPayloadTxCount np
        , _minedBlockSize = _newPayloadSize np
        , _minedBlockOutputSize = _newPayloadOutputSize np
        , _minedBlockFees = _newPayloadFees np
        , _minedBlockDiscoveredAt = now
        }

publish :: CutDb -> CutHashes -> IO ()
publish cdb ch = addCutHashes cdb ch
