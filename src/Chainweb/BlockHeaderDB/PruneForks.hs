{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module: Chainweb.BlockHeaderDB.PruneForks
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Prune old forks from BlockHeader DB
--
module Chainweb.BlockHeaderDB.PruneForks
( pruneForks
, pruneForks_
, safeDepth
, pruneForksJob
, DoPrune(..)
, PruneStats(..)
) where

import Control.DeepSeq (NFData)
import Control.Exception (evaluate, throw)
import Control.Lens (ix, view, (^?!), iforM_, (%~))
import Control.Monad
import Control.Monad.Catch

import Data.Function
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T

import GHC.Generics
import GHC.Stack

import Numeric.Natural

import Prelude hiding (lookup)

import Streaming.Prelude qualified as S

import System.LogLevel

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Parent
import Chainweb.Ranked
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec)
import Chainweb.Version

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Time qualified
import Data.LogMessage
import Chainweb.WebBlockHeaderDB
import Chainweb.Cut
import Chainweb.CutDB
import Control.Monad.Cont
import Data.Void (Void)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Ord
import Control.Concurrent
import Chainweb.BlockHeader.Validation (validateIntrinsicM)
import Data.Aeson (FromJSON, ToJSON)

-- -------------------------------------------------------------------------- --
-- Chain Database Pruning

safeDepth :: BlockHeight
safeDepth = 10000

data PruneStats = PruneStats
    { blocksPruned :: !Int
    }
    deriving stock Generic
    deriving anyclass (NFData, ToJSON, FromJSON)
    deriving LogMessage via (JsonLog PruneStats)

-- | `ForcePrune` restarts from genesis. `PruneDryRun` does the prune without deleting
-- anything, but it also records the prune as having happened.
-- Prune
data DoPrune = Prune | ForcePrune | PruneDryRun
    deriving (Show, Eq, Ord)
    deriving (ToJSON, FromJSON) via (JsonTextRepresentation "DoPrune" DoPrune)

instance HasTextRepresentation DoPrune where
    toText Prune = "prune"
    toText ForcePrune = "force-prune"
    toText PruneDryRun = "prune-dry-run"
    fromText "prune"= return Prune
    fromText "force-prune" = return ForcePrune
    fromText "prune-dry-run" = return PruneDryRun
    fromText t =
        throwM $ TextFormatException $ "HasTextRepresentation DoPrune: invalid DoPrune value " <> sshow t

pruneForksJob
    :: HasVersion
    => Logger logger
    => logger
    -> IO Cut
    -> WebBlockHeaderDb
    -> DoPrune
    -> Natural
    -> IO Void
pruneForksJob logger getCut wbhdb doPrune depth = do
    runForever (logFunction logger) "prune_forks" $ do
        initialCut <- getCut
        void $ pruneForks logger initialCut wbhdb doPrune depth
        threadDelay (1_000_000 * 60 * 60)

-- | Prunes most block headers from forks that are older than the given number
-- of blocks.
--
-- This function guarantees that the predecessors of all remaining nodes also
-- remain in the database and that there are are no tangling references.
--
-- This function doesn't guarantee to delete all blocks on forks. A small number
-- of fork blocks may not get deleted.
--
pruneForks
    :: HasVersion
    => Logger logger
    => logger
    -> Cut
    -> WebBlockHeaderDb
    -> DoPrune
    -> Natural
        -- ^ The depth at which pruning starts. Block at this depth are used as
        -- initial live set and actual deletion starts at (depth - 1).
        --
        -- Note, that the max rank isn't necessarly included in the current best
        -- cut. So one, should choose a depth for which one is confident that
        -- all forks are resolved.

    -> IO Int
pruneForks logger initialCut wbhdb doPrune depth = do
    let highestSafePruneTarget =
            _cutMinHeight initialCut - min (int depth) (_cutMinHeight initialCut)

    (resumptionPoint, startedFrom) <- tableLookup (_webCurrentPruneJob wbhdb) () >>= \case
        Just j | doPrune /= ForcePrune -> return j
        _ -> return (highestSafePruneTarget, highestSafePruneTarget)

    -- the lower bound is different from the job start point because some
    -- heights have already been pruned
    lowerBound <- tableLookup (_webHighestPruned wbhdb) () >>= \case
        Just highestPruned | doPrune /= ForcePrune -> return highestPruned
        _ -> return 0

    tableInsert (_webHighestPruned wbhdb) () lowerBound

    numPruned <- if
        | int (_cutMinHeight initialCut) <= depth -> do
            logFunctionText logger Info
                $ "Skipping database pruning because the maximum block height "
                <> sshow (_cutMinHeight initialCut) <> " is not larger than then requested depth "
                <> sshow depth
            return 0
        | lowerBound > resumptionPoint -> do
            -- as far as I know, this can only happen if we write highestPruned
            -- below and we stop before we can delete the current job.  if the
            -- lower bound is already pruned, the prune job is done.
            logFunctionText logger Info
                $ "Skipping database pruning because the lower bound is already pruned"
            return 0
        | otherwise -> do
            numPruned <-
                pruneForks_ logger wbhdb doPrune PruneJob
                    { resumptionPoint
                    , startedFrom
                    , lowerBound
                    }
            logFunctionText logger Info
                $ "Pruned "
                <> sshow numPruned
                <> " blocks, now pruned up to "
                <> sshow startedFrom

            return numPruned

    tableInsert (_webHighestPruned wbhdb) () startedFrom
    tableDelete (_webCurrentPruneJob wbhdb) ()
    return numPruned

data PruneForksException
    = PruneForksDbInvariantViolation BlockHeight [BlockHeight] T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception PruneForksException

data PruneState = PruneState
    { liveSet :: HashSet BlockHash
    , prevHeight :: BlockHeight
    , prevRecordedHeight :: BlockHeight
    , numPruned :: Int
    , pendingDeletes :: ChainMap [RankedBlockHash]
    , pendingDeleteCount :: Int
    } deriving Show

data PruneJob = PruneJob
    { resumptionPoint :: BlockHeight
    , startedFrom :: BlockHeight
    -- this is the lower bound we actually work down to; we know inductively
    -- that the interval [genesis, lowerbound] is already pruned.
    , lowerBound :: BlockHeight
    }
    deriving Show

-- | Prune forks between the given min rank and max rank.
--
-- Only block headers that are ancestors of a block header that has block height
-- max rank are kept.
--
-- This function is mostly for internal usage. For details about the arguments
-- and behavior see the documentation of 'pruneForks'.
--
-- TODO add option to also validate the block headers
--
pruneForks_
    :: (HasCallStack, HasVersion)
    => Logger logger
    => logger
    -> WebBlockHeaderDb
    -> DoPrune
    -> PruneJob
    -> IO Int
pruneForks_ logger wbhdb doPrune pruneJob = do
    logFunctionText logger Info $ "Pruning block header database job "
        <> sshow pruneJob

    -- parent hashes of all blocks at height max rank @mar@.
    -- it's fine if we miss some chains here, because we never remove chains
    -- from the chain graph.
    --
    !initialLiveSet <- webEntries wbhdb (Just $ MinRank $ Min $ _getMaxRank mar) (Just mar)
        $ S.foldMap_
        $ \blk ->
            HashSet.singleton (view (blockParent . _Parent) blk)
            <> foldMap HashSet.singleton (getAdjs blk)
            -- the initial live set is expected to be very small. In fact it is
            -- almost always a singleton set on each chain.
    logFunctionText logger Debug $
        "Initial live set at " <> sshow mar <> ": " <> sshow initialLiveSet

    let initialPruneState = PruneState
            { liveSet = initialLiveSet
            , prevHeight = resumptionPoint pruneJob
            , prevRecordedHeight = resumptionPoint pruneJob
            , numPruned = 0
            , pendingDeletes = noDeletes
            , pendingDeleteCount = 0
            }

    now <- Chainweb.Time.getCurrentTimeIntegral

    if null initialLiveSet
    then do
        logFunctionText logger Warn
            $ "Skipping database pruning because of an empty set of headers at upper pruning bound "
            <> sshow mar
        return 0
    else do
        withWebReverseHeaderStream wbhdb (max 1 mar - 1)
            $ pruneBlocks now initialPruneState

  where
    !noDeletes = onAllChains []
    mar = MaxRank $ int $ resumptionPoint pruneJob
    !action = case doPrune of
        PruneDryRun -> "Would have pruned "
        _ -> "Pruned "
    getAdjs bh =
        view _Parent <$> _getBlockHashRecord (view blockAdjacentHashes bh)

    executePendingDeletes PruneState{..} = do
        iforM_ pendingDeletes $ \cid pendingDeletesForCid -> do
            let cdb = _webBlockHeaderDb wbhdb ^?! atChain cid
            case doPrune of
                PruneDryRun -> return ()
                _ -> do
                    tableDeleteBatch (_chainDbCas cdb) pendingDeletesForCid
                    tableDeleteBatch (_chainDbRankTable cdb) (_ranked <$> pendingDeletesForCid)
        logFunctionText logger Info $
            action <> sshow pendingDeleteCount <> " block headers at height " <> sshow prevHeight
        logFunctionText logger Debug $
            action <> sshow pendingDeletes <> ", at height " <> sshow prevHeight
        logFunction logger Info $
            PruneStats pendingDeleteCount

    deleteBatchSize = 1000
    checkpointSize = 1000

    pruneBlocks now initialState =
        go initialState
        where
        go state strm =
            if pendingDeleteCount state >= deleteBatchSize
                || prevRecordedHeight state - prevHeight state > checkpointSize
            then do
                -- first execute pending deletes, then record checkpoint. that
                -- way we never miss deletes.
                executePendingDeletes state
                -- make a checkpoint of our progress. we want to resume a level
                -- higher, because we really don't prune the first height, we
                -- use it for the initial live set.
                tableInsert (_webCurrentPruneJob wbhdb) ()
                    (succ $ prevHeight state, startedFrom pruneJob)
                go
                    state
                        { prevRecordedHeight = prevHeight state
                        , pendingDeletes = noDeletes
                        , pendingDeleteCount = 0
                        }
                    strm
            else do
                S.uncons strm >>= \case
                    Nothing -> do
                        executePendingDeletes state
                        return $! numPruned state
                    Just (block, strm') -> do
                        pruneBlock state block strm'

        pruneBlock :: PruneState -> BlockHeader -> S.Stream (S.Of BlockHeader) IO () -> IO Int
        pruneBlock PruneState {liveSet, prevHeight} _ _ | HashSet.null liveSet =
            throw $ InternalInvariantViolation
                $ "PruneForks.pruneForks_: no live blocks left at height " <> sshow prevHeight
        pruneBlock PruneState{..} cur strm
            -- This checks some structural consistency. It's not a comprehensive
            -- check. Comprehensive checks on various levels are available through
            -- callbacks that are offered in the module "Chainweb.Chainweb.PruneChainDatabase"
            | prevHeight /= curHeight && prevHeight /= curHeight + 1 =
                throw $ InternalInvariantViolation
                    $ "PruneForks.pruneForks_: detected a corrupted database. "
                    <> "Some block headers are missing."
                    <> "Current live set: " <> encodeToText liveSet
                    <> ". Current header: " <> encodeToText (ObjectEncoded cur)
                    <> ". Previous height: " <> sshow prevHeight
            -- if we are at or beyond the lower bound,
            -- and we have no more pending fork tips to prune,
            -- we're done early.
            | curHeight <= lowerBound pruneJob
            , curHeight < prevHeight
            = do
                when (pendingDeleteCount > 0) $
                    executePendingDeletes PruneState{..}
                return numPruned
            -- the current block is live.
            -- its parents take its place as live blocks.
            | curHash `elem` liveSet = do
                let !liveSet' =
                        flip (foldl' (flip HashSet.insert)) curAdjs $
                        HashSet.insert curParent $
                        HashSet.delete curHash liveSet

                validateIntrinsicM now cur

                go
                    PruneState
                        { liveSet = liveSet'
                        , prevHeight = curHeight
                        , prevRecordedHeight
                        , numPruned
                        , pendingDeletes
                        , pendingDeleteCount
                        } strm
            -- the current block is not live, delete it.
            | otherwise = do
                let !newDelete = view rankedBlockHash cur
                let !(!pendingDeletes', !pendingDeleteCount') =
                        (pendingDeletes & ix cid %~ (newDelete :)
                        , pendingDeleteCount + 1)
                let !numPruned' = numPruned + 1
                go
                    PruneState
                        { liveSet
                        , prevHeight = curHeight
                        , prevRecordedHeight
                        , numPruned = numPruned'
                        , pendingDeletes = pendingDeletes'
                        , pendingDeleteCount = pendingDeleteCount'
                        } strm
            where
            curParent = view (blockParent . _Parent) cur
            curAdjs = getAdjs cur
            !cid = view chainId cur
            !curHeight = view blockHeight cur
            !curHash = view blockHash cur

-- -------------------------------------------------------------------------- --
-- Utils

-- TODO: provide this function in chainweb-storage
--
withReverseHeaderStream
    :: BlockHeaderDb
    -> MaxRank
    -> (S.Stream (S.Of BlockHeader) IO () -> IO a)
    -> IO a
withReverseHeaderStream db mar inner = withTableIterator headerTbl $ \it -> do

    iterSeek it $
        RankedBlockHash (BlockHeight $ int $ _getMaxRank mar + 1) nullBlockHash
    iterPrev it

    inner $ iterToReverseValueStream it
        & S.map _getRankedBlockHeader
  where
    headerTbl = _chainDbCas db

withWebReverseHeaderStream
    :: WebBlockHeaderDb
    -> MaxRank
    -> (S.Stream (S.Of BlockHeader) IO () -> IO a)
    -> IO a
withWebReverseHeaderStream wbhdb mar inner = do
    runContT (forM (_webBlockHeaderDb wbhdb) (\db -> ContT $ withReverseHeaderStream db mar))
        $ \streamPerChain -> inner $ mergeN (\bh -> (Down (view blockHeight bh), view chainId bh)) $ toList streamPerChain
