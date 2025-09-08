{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Lens (view, (^?!), iforM_, (%~))
import Control.Monad
import Control.Monad.Catch

import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T

import GHC.Generics
import GHC.Stack

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

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
import Data.LogMessage
import Chainweb.WebBlockHeaderDB
import Chainweb.Cut
import Chainweb.CutDB
import Control.Monad.Cont
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Void (Void)

-- -------------------------------------------------------------------------- --
-- Chain Database Pruning

safeDepth :: BlockHeight
safeDepth = 10000

data DoPrune = DoPrune | ReDoPrune | DoNotPrune
    deriving (Show, Eq, Ord)

pruneForksJob
    :: HasVersion
    => Logger logger
    => logger
    -> CutDb
    -> DoPrune
    -> Natural
    -> IO Void
pruneForksJob logger cdb doPrune depth = do
    runForeverThrottled (logFunction logger) "prune_forks" 1 (1_000_000 * 60 * 60) $
        void $ pruneForks logger cdb doPrune depth

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
    -> CutDb
    -> DoPrune
    -> Natural
        -- ^ The depth at which pruning starts. Block at this depth are used as
        -- pivots and actual deletion starts at (depth - 1).
        --
        -- Note, that the max rank isn't necessarly included in the current best
        -- cut. So one, should choose a depth for which one is confident that
        -- all forks are resolved.

    -> IO Int
pruneForks logger cdb doPrune depth = do
    startCut <- _cut cdb
    let highestSafePruneTarget = _cutMinHeight startCut - min (int depth) (_cutMinHeight startCut)
    (resumingFrom, startedFrom) <- tableLookup (_webCurrentPruneJob wbhdb) () >>= \case
        Just j | doPrune /= ReDoPrune -> return j
        _ -> return (0, highestSafePruneTarget)
    -- the lower bound is different from the job start point because some
    -- heights have already been pruned
    highestPruned <- tableLookup (_webHighestPruned wbhdb) () >>= \case
        Just highestPruned | doPrune /= ReDoPrune -> return highestPruned
        _ -> return 0

    if
        | int (_cutMinHeight startCut) <= depth -> do
            logFunctionText logger Info
                $ "Skipping database pruning because the maximum block height "
                <> sshow (_cutMinHeight startCut) <> " is not larger than then requested depth "
                <> sshow depth
            return 0
        | otherwise -> do
            numPruned <- pruneForks_ logger wbhdb doPrune (PruneJob resumingFrom startedFrom highestPruned)
            tableInsert (_webHighestPruned wbhdb) () highestPruned
            tableDelete (_webCurrentPruneJob wbhdb) ()
            return numPruned
  where
    wbhdb = view cutDbWebBlockHeaderDb cdb

data PruneForksException
    = PruneForksDbInvariantViolation BlockHeight [BlockHeight] T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception PruneForksException

data PruneState = PruneState
    { pivots :: HashSet BlockHash
    , prevHeight :: BlockHeight
    , numPruned :: Int
    , pendingDeletes :: ChainMap [RankedBlockHash]
    , pendingDeleteCount :: Int
    }

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
-- pruneForks_ logger _ (PruneJob (Max mar)) _
--     | mar <= 1 = 0 <$ logFunctionText logger Warn ("Skipping database pruning for max bound of " <> sshow mar)
pruneForks_ logger wbhdb doPrune pruneJob = do
    logFunctionText logger Debug $ "Pruning block header database job "
        <> sshow pruneJob

    -- parent hashes of all blocks at height max rank @mar@.
    -- it's fine if we miss some chains here, because we never remove chains
    -- from the chain graph.
    --
    !pivots <- webEntries_ wbhdb (Just $ MinRank $ Min $ _getMaxRank mar) (Just mar)
        $ S.foldMap_ HashSet.singleton . S.map (unwrapParent . view blockParent)
            -- the set of pivots is expected to be very small. In fact it is
            -- almost always a singleton set.

    if null pivots
    then do
        logFunctionText logger Warn
            $ "Skipping database pruning because of an empty set of block headers at upper pruning bound " <> sshow mar
        return 0
    else do
        r <- fmap numPruned
            $ withWebReverseHeaderStream wbhdb (mar - 1) mir
            $ pruneBlocks pivots
        tableDelete (_webCurrentPruneJob wbhdb) ()
        return r

  where
    mir = MinRank $ int $ lowerBound pruneJob
    mar = MaxRank $ int $ resumptionPoint pruneJob

    executePendingDeletes prevHeight pendingDeletes = do
        iforM_ pendingDeletes $ \cid pendingDeletesForCid -> do
            let cdb = _webBlockHeaderDb wbhdb ^?! atChain cid
            case doPrune of
                DoNotPrune -> return ()
                _ -> do
                    tableDeleteBatch (_chainDbCas cdb) pendingDeletesForCid
                    tableDeleteBatch (_chainDbRankTable cdb) (_ranked <$> pendingDeletesForCid)
        logFunctionText logger Debug $ "pruned block headers " <> sshow pendingDeletes <> ", at height " <> sshow prevHeight
        tableInsert (_webCurrentPruneJob wbhdb) () (prevHeight, startedFrom pruneJob)

    pruneBlocks pivots =
        go initialState
        where
        initialState = PruneState
            { pivots
            , prevHeight = BlockHeight $ int (_getMaxRank mar)
            , numPruned = 0
            , pendingDeletes = onAllChains []
            , pendingDeleteCount = 0
            }
        go state strm = S.uncons strm >>= \case
            Nothing -> return state
            Just (block, strm') -> do
                pruneBlock state block >>= \case
                    (False, state') -> do
                        executePendingDeletes (prevHeight state) (pendingDeletes state')
                        return state' { pendingDeletes = onAllChains [], pendingDeleteCount = 0 }
                    (True, state') -> do
                        go state' strm'

    pruneBlock :: PruneState -> BlockHeader -> IO (Bool, PruneState)
    pruneBlock PruneState {pivots, prevHeight} _ | HashSet.null pivots =
        throwM $ InternalInvariantViolation
            $ "PruneForks.pruneForks_: no pivots left at height " <> sshow prevHeight
    pruneBlock pruneState@PruneState{..} cur
        -- This checks some structural consistency. It's not a comprehensive
        -- check. Comprehensive checks on various levels are available through
        -- callbacks that are offered in the module "Chainweb.Chainweb.PruneChainDatabase"
        | prevHeight /= curHeight && prevHeight /= curHeight + 1 =
            throwM $ InternalInvariantViolation
                $ "PruneForks.pruneForks_: detected a corrupted database. Some block headers are missing"
                <> ". Current pivots: " <> encodeToText pivots
                <> ". Current header: " <> encodeToText (ObjectEncoded cur)
                <> ". Previous height: " <> sshow prevHeight
        -- if we have only one pivots and are at or beyond the lower bound,
        -- then we have no more pending fork tips to delete in the range
        | curHeight <= int (_getMinRank mir) && length pivots == 1 = do
            return (False, pruneState)
        -- the current block is a pivot, thus we've seen its child block(s)
        -- and must keep it in the database
        | curHash `elem` pivots = do
            let !pivots' = HashSet.insert (view (blockParent . _Parent) cur) $ HashSet.delete curHash pivots
            return (True, PruneState
                { pivots = pivots'
                , prevHeight = curHeight
                , numPruned
                , pendingDeletes
                , pendingDeleteCount
                })
        -- the current block is not a pivot, so we haven't seen its child and can safely delete it
        | otherwise = do
            let newDelete = onChain cid [view rankedBlockHash cur]
            !(pendingDeletes', !pendingDeleteCount') <-
                if pendingDeleteCount > 1000
                then do
                    executePendingDeletes prevHeight pendingDeletes
                    return (newDelete, 1)
                else
                    return (chainZip (++) newDelete pendingDeletes, pendingDeleteCount + 1)
            return (True, PruneState
                { pivots
                , prevHeight = curHeight
                , numPruned = numPruned + 1
                , pendingDeletes = pendingDeletes'
                , pendingDeleteCount = pendingDeleteCount'
                })
        where
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
    -> MinRank
    -> (S.Stream (S.Of BlockHeader) IO () -> IO a)
    -> IO a
withReverseHeaderStream db mar mir inner = withTableIterator headerTbl $ \it -> do
    iterSeek it $ RankedBlockHash (BlockHeight $ int $ _getMaxRank mar + 1) nullBlockHash
    iterPrev it
    inner $ iterToReverseValueStream it
        & S.map _getRankedBlockHeader
        & S.takeWhile (\a -> int (view blockHeight a) >= mir)
  where
    headerTbl = _chainDbCas db

withWebReverseHeaderStream
    :: WebBlockHeaderDb
    -> MaxRank
    -> MinRank
    -> (S.Stream (S.Of BlockHeader) IO () -> IO a)
    -> IO a
withWebReverseHeaderStream wbhdb mar mir inner = do
    runContT (forM (_webBlockHeaderDb wbhdb) (\db -> ContT $ withReverseHeaderStream db mar mir)) $ \streamPerChain ->
        inner $ foldr1 (\x t -> () <$ S.mergeOn (view blockHeight) x t) streamPerChain

{-# INLINE withReverseHeaderStream #-}
