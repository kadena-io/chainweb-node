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

-- * WorkState
, WorkState(..)

-- * Internal

-- ** Payload Caches
, type PayloadCaches
, newPayloadCaches
, awaitPayloadsNext

-- ** MiningState
, MiningState
, updateForCut
, updateForPayload
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
import Chainweb.Cut
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
import Chainweb.WebBlockHeaderDB
import Control.Applicative
import Control.Concurrent.STM (atomically, STM, retry)
import Control.Concurrent.STM.TVar
import Control.Exception.Safe
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable
import Data.Foldable
import Data.LogMessage (JsonLog(..), LogFunction, LogFunctionText)
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Numeric.Natural
import Streaming.Prelude qualified as S
import System.LogLevel (LogLevel(..))
import System.Random (randomRIO)
import GHC.Stack (HasCallStack)
import qualified Data.Map.Strict as M
import Control.Concurrent (threadDelay)

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
lookupInCut :: HasCallStack => HasVersion => HasChainId cid => Cut -> cid -> BlockHeader
lookupInCut c cid
    | Just x <- lookupCutM cid c = x
    | otherwise = error $ T.unpack
        $ "Chainweb.Miner.Coordinator.lookupInCut: failed to lookup chain in cut."
        <> " Chain: " <> sshow (_chainId cid) <> "."
        <> " Cut Hashes: " <> encodeToText (cutToCutHashes Nothing c) <> "."

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

newPayloadCaches :: HasVersion => IO PayloadCaches
newPayloadCaches = tabulateChainsM (\_ -> newIO depth)
  where
    -- FIXME: Make this configurable?
    depth :: Natural
    depth = diameter (chainGraphAt (maxBound :: BlockHeight))

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
data WorkState
  = WorkNotReady !(Parent RankedBlockHash)
    -- ^ Chain is blocked and no payload has yet been produced

  | WorkStale !(Parent RankedBlockHash) !WorkParents
    -- ^ The chain is unblocked but no payload has been produced yet.
    --
    -- Invariant: The work parents must match the ranked block hash.

  | WorkBlocked !(Parent RankedBlockHash) !NewPayload
    -- ^ A payload is ready but the chain is still blocked
    --
    -- Invariant: The payload must match the ranked block hash.

  | WorkReady !(Parent RankedBlockHash) !NewPayload !WorkParents
    -- ^ The chain is ready for mining
    --
    -- Invariant: The payload, parents, work header must match the ranked block
    -- hash.

  | WorkSolved !(Parent RankedBlockHash) !NewPayload !WorkParents
  deriving stock (Show, Eq, Generic)

_workRankedHash :: WorkState -> Parent RankedBlockHash
_workRankedHash (WorkNotReady rh) = rh
_workRankedHash (WorkStale rh _) = rh
_workRankedHash (WorkBlocked rh _) = rh
_workRankedHash (WorkReady rh _ _) = rh
_workRankedHash (WorkSolved rh _ _) = rh

_workStateHash :: WorkState -> Parent BlockHash
_workStateHash = fmap _rankedBlockHashHash . _workRankedHash

_workStateHeight :: WorkState -> Parent BlockHeight
_workStateHeight = fmap _rankedBlockHashHeight . _workRankedHash

workReady
    :: Parent RankedBlockHash
    -> NewPayload
    -> WorkParents
    -> WorkState
workReady rbh pld ps' = WorkReady rbh pld ps'

_newPayloadRankedHash :: NewPayload -> RankedBlockHash
_newPayloadRankedHash p =
    RankedBlockHash (unwrapParent $ _newPayloadParentHeight p) (unwrapParent $ _newPayloadParentHash p)

instance Brief WorkState where
    brief (WorkNotReady rh) = "WorkNotReady" <> ":" <> brief rh
    brief (WorkStale rh _) = "WorkStale" <> ":" <> brief rh
    brief (WorkBlocked rh _) = "WorkBlocked" <> ":" <> brief rh
    brief (WorkReady rh _ _) = "WorkReady" <> ":" <> brief rh
    brief (WorkSolved rh _ _) = "WorkSolved" <> ":" <> brief rh

-- -------------------------------------------------------------------------- --
-- WorkState Transition Function

-- | Called on headers event
--
onHeader
    :: Parent RankedBlockHash
    -> WorkState
    -> Maybe WorkState
onHeader rh cur
    | rh == _workRankedHash cur = Nothing
    | otherwise = Just $ WorkNotReady rh

-- | Called on a work headers event.
--
-- If the parent header changes, 'onHeader' is called first, which also resets
-- the payload. For that reason it should be also checked whether there is a
-- payload available for the new header.
--
onParents
    :: Maybe WorkParents
        -- Just the work parents or 'Nothing' when the chain is blocked.
    -> WorkState
    -> Maybe WorkState
onParents (Just ps) cur | psRankedHash /= _workRankedHash cur =
    onHeader psRankedHash cur >>= onParents (Just ps)
  where
    psRankedHash = _rankedBlockHash <$> _workParent ps
onParents (Just ps') (WorkNotReady rh) = Just $ WorkStale rh ps'
onParents (Just ps') (WorkBlocked rh pld) = Just $ workReady rh pld ps'
onParents (Just ps') (WorkStale rh ps)
    | ps /= ps' = Just $ WorkStale rh ps'
    | otherwise = Nothing
onParents (Just ps') (WorkReady rh pld ps)
    | ps /= ps' = Just $ workReady rh pld ps'
    | otherwise =  Nothing
onParents (Just ps') (WorkSolved rh pld ps)
    | ps /= ps' = Just $ workReady rh pld ps'
    | otherwise = Nothing
onParents Nothing WorkNotReady{} = Nothing
onParents Nothing WorkBlocked{} = Nothing
onParents Nothing (WorkStale rh _) = Just $ WorkNotReady rh
onParents Nothing (WorkReady rh pld _) = Just $ WorkBlocked rh pld
onParents Nothing (WorkSolved rh pld _) = Just $ WorkBlocked rh pld

-- | Called on a new payload event.
--
-- If the parent header of the new payload does not match the current parent
-- header, it is ignored. Only an 'onParents' event can update the current
-- parent header.
--
onPayload
    :: NewPayload
    -> WorkState
    -> Maybe WorkState
onPayload pld' cur | _newPayloadRankedParentHash pld' /= _workRankedHash cur = Nothing
onPayload pld' (WorkNotReady rh) = Just $ WorkBlocked rh pld'
onPayload pld' (WorkStale rh ps) = Just $ workReady rh pld' ps
onPayload pld' (WorkBlocked rh pld)
    | pld /= pld' = Just $ WorkBlocked rh pld'
    | otherwise = Nothing
onPayload pld' (WorkReady rh pld ps)
    | pld /= pld' = Just $ workReady rh pld' ps
    | otherwise = Nothing
onPayload _ WorkSolved{} = Nothing

-- | Called when work is solved for the chain.
--
onSolved
    :: SolvedWork
    -> WorkState
    -> Maybe WorkState
onSolved s (WorkReady rh pld ps)
    -- If work is currently ready for this header in this cut, we mark it solved.
    -- We checked that before, when we processed the solution. But we have to do
    -- it again, since the state is updated asynchronously and there could be a
    -- race.
    | solvedId s == parentsId ps = Just $ WorkSolved rh pld ps
-- otherwise do not change the state.
-- If we solved this header in this cut before we do not change the work
-- state, even if the payload differs.
onSolved _ _ = Nothing

-- -------------------------------------------------------------------------- --
-- Mining State


-- | Current Mining State on each chain. It is updated
--
-- 1. each time a new cut is received
-- 2. each time a block is solved
-- 3. each time a new payload is received for a chain
--
newtype MiningState = MiningState
    { _miningState :: M.Map ChainId (TVar WorkState)
    }
    deriving stock (Generic)
    deriving newtype (Semigroup, Monoid)

makeLenses ''MiningState

type instance Index MiningState = ChainId
type instance IxValue MiningState = TVar WorkState

instance Ixed MiningState where
    ix i = miningState . ix i

instance IxedGet MiningState

instance Each MiningState MiningState (TVar WorkState) (TVar WorkState) where
    each f = fmap MiningState . each f . _miningState

newMiningState :: HasVersion => Cut -> IO MiningState
newMiningState c = do
    states <- forM cids $ \cid -> do
        var <- newTVarIO
            $ WorkNotReady
            $ Parent
            $ _rankedBlockHash
            $ fromMaybe (genesisBlockHeader cid) (HM.lookup cid (_cutMap c))
        return (cid, var)
    return $ MiningState $! M.fromList states
  where

    cids :: [ChainId]
    cids = HS.toList chainIds

updateStateVar :: LogFunctionText -> ChainId -> TVar WorkState -> WorkState -> IO ()
updateStateVar lf cid var new = do
    -- Logging. This can race, but we don't care
    cur <- readTVarIO var
    lf Debug $ "update work state"
        <> "; chain: " <> toText cid
        <> "; cur: " <> brief cur
        <> "; new: " <> brief new
    atomically $ writeTVar var new

-- TODO: consider storing the mining state more efficiently:
--
-- Do not recompute cut extensions more often than needed.
--
-- * store the current cut
-- * when a new cut arrive compute which chains need update

-- | Update work state for all chains for a new cut.
--
updateForCut
    :: HasVersion
    => LogFunctionText
    -> (ChainValue BlockHash -> IO BlockHeader)
    -> PayloadCaches
    -> MiningState
    -> Cut
    -> IO ()
updateForCut lf hdb caches ms c = do
    forM_ (M.toList $ _miningState ms) $ \(cid, var) ->
        forChain cid var (caches ^?! ix cid)
  where
    forChain cid var cache =  do
        ps <- workParents hdb c cid
        cur <- readTVarIO var

        -- logging
        -- cs <- sizeIO cache
        -- cl <- getLatestIO cache (_workRankedHash cur)
        -- ch <- payloadHashesIO cache
        -- lf @T.Text Debug $ "updateForCut for chain: " <> brief cid
        --     <> "; cur: " <> brief cur
        --     <> "; cut: " <> brief (c ^?! ixg cid)
        --     <> "; parent: " <> brief (_workParent <$> ps)
        --     <> "; cache size: " <> sshow cs
        --     <> "; cache depth: " <> sshow (_payloadCacheDepth cache)
        --     <> "; cache latest: " <> brief cl
        --     <> "; cache hashes: " <> brief ch

        case onParents ps cur of
            Nothing -> return ()
            Just !new

                -- Check whether the parent header is still the same
                | _workRankedHash new == _workRankedHash cur ->
                    updateStateVar lf cid var new

                -- if the parent header changed, check if a payload is available
                | otherwise -> getLatestIO cache (_workRankedHash new) >>= \case
                    Nothing -> updateStateVar lf cid var new
                    Just pld -> case onPayload pld new of
                        Nothing -> updateStateVar lf cid var new
                        Just !newnew -> updateStateVar lf cid var newnew

updateForPayload :: LogFunctionText -> MiningState -> NewPayload -> IO ()
updateForPayload lf ms pld = do
    cur <- readTVarIO var
    -- lf @T.Text Debug $ "updateForPayload on chain: " <> toText cid
    --     <> "; cur: " <> brief cur
    --     <> "; new payload: " <> brief pld
    case onPayload pld cur of
        Nothing -> return ()
        Just !new -> updateStateVar lf cid var new
  where
    cid = _chainId pld
    var = ms ^?! ixg cid

updateForSolved :: LogFunctionText -> MiningState -> SolvedWork -> IO ()
updateForSolved lf ms sw = do
    cur <- readTVarIO var
    -- lf @T.Text Debug $ "updateForSolved on chain: " <> toText cid
    --     <> "; cur: " <> brief cur
    --     <> "; sw: " <> brief sw
    case onSolved sw cur of
        Nothing -> return ()
        Just !new -> do
            updateStateVar lf cid var new
  where
    cid = _chainId sw
    var = ms ^?! ixg cid

awaitAnyReady :: MiningState -> STM (WorkParents, NewPayload)
awaitAnyReady s = msum $ awaitWorkReady <$> _miningState s
  where
    awaitWorkReady :: TVar WorkState -> STM (WorkParents, NewPayload)
    awaitWorkReady var = readTVar var >>= \case
        WorkReady _ pld ps -> return (ps, pld)
        _ -> retry

-- -------------------------------------------------------------------------- --
-- MiningCoordination

-- | For coordinating requests for work and mining solutions from remote Mining
-- Clients.
--
data MiningCoordination logger = MiningCoordination
    { _coordLogger :: !logger
    , _coordCutDb :: !CutDb
    , _coordState :: !MiningState
    , _coordConf :: !CoordinationConfig
    , _coordPayloadCache :: !PayloadCaches
    }

newMiningCoordination
    :: Logger logger
    => HasVersion
    => logger
    -> CoordinationConfig
    -> CutDb
    -> IO (MiningCoordination logger)
newMiningCoordination logger conf cdb = do
    c <- _cut cdb
    state <- newMiningState c
    caches <- newPayloadCaches
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
    => HasVersion
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
        lf Debug "start updateWork event stream"
        eventStream cdb caches
            & S.chain (\e -> lf Debug $ "coordination event: " <> brief e)
            & S.mapM_ \case
                CutEvent c -> updateForCut lf f caches state c
                NewPayloadEvent c -> updateForPayload lf state c
                -- There is a race with solved events. Does it matter?
                -- We could synchronize those by delivering those via an
                -- STM variable, too.

    initializeState = do
        lf Info $ "initialize mining state"
        -- FIXME: this is probably more aggressive than needed
        -- forConcurrently_ (itoList caches) $ \(cid, cache) -> do
        --     lf Info $ "initialize mining state for chain " <> brief cid
        --     pld <- withProvider cid latestPayloadIO
        --     lf Info $ "got latest payload for chain " <> brief cid
        --     insertIO cache pld
        curCut <- _cut $ cdb
        updateForCut lf f caches state curCut
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
--     'syncToBlock' calls,
-- 3.  some payload providers are deadlocked, or
-- 4.  some payload providers are very slow in producing new payloads.
--
randomWork
    :: HasVersion
    => LogFunction
    -> MiningState
    -> IO MiningWork
randomWork logFun state = do

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
    n <- randomRIO (0, M.size m)

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
    let (s0, s1) = M.splitAt n m
    go (M.toList s1 <> M.toList s0)
  where
    m = _miningState state

    go [] = do

        logFun @T.Text Info $ "randomWork: no work is ready. Awaiting work"

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

        -- timeoutVar <- registerDelay (int staleMiningStateDelay)
        -- w <- atomically $
        --     Right <$> awaitAnyReady state <|> awaitTimeout timeoutVar
        -- case w of
        --     Right (ps, npld) -> do
        --         ct <- BlockCreationTime <$> getCurrentTimeIntegral
        --         return $ newWork ct ps (_newPayloadBlockPayloadHash npld)
        --     Left e -> error $
        --         "Chainweb.Miner.Coordinator.randomWork: " <> T.unpack e
        --         -- FIXME: throw a proper exception and log what is going on

        -- Let's try the following instead (it's probably a stupid idea):
        threadDelay 500_000
        randomWork logFun state

    go ((cid, var):t) = readTVarIO var >>= \case
        WorkReady _ npld ps -> do
            logFun @T.Text Debug $ "randomWork: picked chain " <> brief cid
            ct <- BlockCreationTime <$> getCurrentTimeIntegral
            return $ newWork ct ps (_newPayloadBlockPayloadHash npld)
        e -> do
            logFun @T.Text Info $ "randomWork: not ready for " <> brief cid
                <> "; state: " <> brief e
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

        -- workStatusSummary <- summarizeWorkStatus

        return $ Left $ "Chainweb.Miner.Coordinator.randomWork: timeout while waiting for work to become ready." -- <> workStatusSummary

    -- summarizeWorkStatus = do
    --     parentStates <- forM parentStateVars readTVar
    --     let chainsWithMissingParents =
    --             [ cid | (cid, Nothing) <- itoList parentStates ]
    --     let chainsWithParents = onChains
    --             [ (cid, p) | (cid, Just p) <- itoList parentStates ]
    --     payloads <- forM
    --         (chainIntersect (,) chainsWithParents caches)
    --         (\(parent, cache) -> Just <$> awaitLatestPayloadForParentStateSTM cache parent <|> return Nothing)
    --     let chainsWithMissingPayloads =
    --             [ cid | (cid, Nothing) <- itoList payloads ]
    --     c <- _cutStm cdb
    --     return $ J.encodeText $ J.object
    --         [ "blocked" J..= J.array (List.sort (toText <$> chainsWithMissingParents))
    --         , "stale" J..= J.array (List.sort (toText <$> chainsWithMissingPayloads))
    --         , "cut" J..= brief c
    --         ]

staleMiningStateDelay :: Micros
staleMiningStateDelay = 2_000_000

-- | This is the legacy work delivery API
--
work
    :: forall l
    .  Logger l
    => HasVersion
    => MiningCoordination l
    -> IO MiningWork
work mr = randomWork lf (_coordState mr)
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
    => HasVersion
    => MiningCoordination l
    -> SolvedWork
    -> IO ()
solve mr solved = (readTVarIO $ _coordState mr ^?! ixg cid) >>= \case

    WorkSolved{} ->
        -- The solved work is already in the mining state
        lf Info $ "solve: ignoring solution for mining state that was already solved before"
            <> ". solved " <> brief solved
        -- TODO check and log whether the solved state matches the solution

    WorkReady rh npld ps -> do
        -- The solved work is still valid and the payload is available

        let pldh = _newPayloadBlockPayloadHash npld

        -- Check that the solved parents match the current mining state parents.
        -- We can be lenient on the payloads, but the parents must match the
        -- current cut.
        if solvedId solved /= parentsId ps
          then do
            -- ignore solved work
            lf Info $ "solve: solved work does not match the current mining state"
                <> "; solved: " <> brief solved
                <> "; current work parents: " <> brief ps

          -- check that we have the payload for the solution in the cache.
          else lookupIO cache rh pldh >>= \case

            Nothing -> do
                ch <- payloadHashesIO cache
                lf Error $ "solve: no payload for " <> brief solved
                    <> "; cache key: " <> brief rh
                    <> "; cache content: " <> brief ch
                throwM NoAsscociatedPayload
                -- FIXME Do we really need to restart the coordinator?

            Just np -> do
                c <- _cut cdb
                now <- getCurrentTimeIntegral
                let pld = _newPayloadEncodedPayloadData np
                let pwo = _newPayloadEncodedPayloadOutputs np

                try (extend c pld pwo ps solved) >>= \case

                    -- Publish CutHashes to CutDb and log success
                    Right (bh, Just ch) -> do
                        updateForSolved lf (_coordState mr) solved
                        publish cdb ch
                        logMinedBlock lf bh np

                    -- Log Orphaned Block
                    Right (_, Nothing) -> do
                        let !p = lookupInCut c cid
                        lf Info $ orphandMsg now p solved "orphaned solution"

                    -- Log failure and rethrow
                    Left e@(InvalidSolvedHeader msg) -> do
                        let !p = lookupInCut c cid
                        lf Info $ orphandMsg now p solved msg
                        throwM e
    _ -> do
        -- The solved work is orphaned.
        lf Info $ "solve: ignoring outdated solution for. Mining state is not ready or solved"
            <> ". solved: " <> brief solved
        c <- _cut cdb
        now <- getCurrentTimeIntegral
        let !p = lookupInCut c cid
        lf Info $ orphandMsg now p solved "orphaned solution"
  where
    cid = _chainId solved
    cdb = _coordCutDb mr
    caches = _coordPayloadCache mr
    cache = caches ^?! ix cid

    lf :: LogFunction
    lf = logFunction $ _coordLogger mr

    orphandMsg now p s msg = JsonLog OrphanedBlock
        { _orphanedParent = _solvedParentHash s
        , _orphanedPayloadHash = _solvedPayloadHash s
        , _orphanedDiscoveredAt = now
        , _orphanedReason = msg
        }

logMinedBlock
    :: HasVersion
    => LogFunction
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

publish :: HasVersion => CutDb -> CutHashes -> IO ()
publish cdb ch = addCutHashes cdb ch
