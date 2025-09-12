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
import Control.Exception (evaluate, throw)
import Control.Lens (ix, view, (^?!), iforM_, (%~))
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
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Ord
import Control.Concurrent

-- -------------------------------------------------------------------------- --
-- Chain Database Pruning

safeDepth :: BlockHeight
safeDepth = 10000

data DoPrune = Prune | ForcePrune | PruneDryRun
    deriving (Show, Eq, Ord)

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
    let highestSafePruneTarget = _cutMinHeight initialCut - min (int depth) (_cutMinHeight initialCut)
    (resumptionPoint, startedFrom) <- tableLookup (_webCurrentPruneJob wbhdb) () >>= \case
        Just j | doPrune /= ForcePrune -> return j
        _ -> return (highestSafePruneTarget, highestSafePruneTarget)
    -- the lower bound is different from the job start point because some
    -- heights have already been pruned
    lowerBound <- tableLookup (_webHighestPruned wbhdb) () >>= \case
        Just highestPruned | doPrune /= ForcePrune -> return highestPruned
        _ -> return 0
    tableInsert (_webHighestPruned wbhdb) () lowerBound
    if
        | int (_cutMinHeight initialCut) <= depth -> do
            logFunctionText logger Info
                $ "Skipping database pruning because the maximum block height "
                <> sshow (_cutMinHeight initialCut) <> " is not larger than then requested depth "
                <> sshow depth
            return 0
        | otherwise -> do
            pruned <- pruneForks_ logger wbhdb doPrune PruneJob
                { resumptionPoint
                , startedFrom
                , lowerBound
                }
            tableInsert (_webHighestPruned wbhdb) () startedFrom
            tableDelete (_webCurrentPruneJob wbhdb) ()
            logFunctionText logger Info
                $ "Pruned "
                <> sshow pruned
                <> " blocks, now pruned up to "
                <> sshow startedFrom
            return pruned

data PruneForksException
    = PruneForksDbInvariantViolation BlockHeight [BlockHeight] T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception PruneForksException

data PruneState = PruneState
    { liveSet :: HashSet BlockHash
    , pendingForkTips :: HashSet BlockHash
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
    !initialLiveSet <- webEntries_ wbhdb (Just $ MinRank $ Min $ _getMaxRank mar) (Just mar)
        $ S.foldMap_ (HashSet.singleton . unwrapParent . view blockParent)
            -- the initial live set is expected to be very small. In fact it is
            -- almost always a singleton set on each chain.
    let initialPruneState = PruneState
            { liveSet = initialLiveSet
            , pendingForkTips = HashSet.empty
            , prevHeight = resumptionPoint pruneJob
            , prevRecordedHeight = resumptionPoint pruneJob
            , numPruned = 0
            , pendingDeletes = noDeletes
            , pendingDeleteCount = 0
            }

    if null initialLiveSet
    then do
        logFunctionText logger Warn
            $ "Skipping database pruning because of an empty set of block headers at upper pruning bound " <> sshow mar
        return 0
    else do
        r <- withWebReverseHeaderStream wbhdb (max 1 mar - 1)
            $ pruneBlocks initialPruneState
        tableDelete (_webCurrentPruneJob wbhdb) ()
        return r

  where
    !noDeletes = onAllChains []
    mar = MaxRank $ int $ resumptionPoint pruneJob
    !action = case doPrune of
        PruneDryRun -> "would have pruned "
        _ -> "pruned "

    executePendingDeletes prevHeight pendingDeletes pendingDeleteCount = do
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

    deleteBatchSize = 1000
    checkpointSize = 200

    pruneBlocks initialState =
        go initialState
        where
        go state strm =
            if pendingDeleteCount state >= deleteBatchSize
            then do
                executePendingDeletes (prevHeight state) (pendingDeletes state) (pendingDeleteCount state)
                tableInsert (_webCurrentPruneJob wbhdb) () (prevHeight state, startedFrom pruneJob)
                go state { prevRecordedHeight = prevHeight state, pendingDeletes = noDeletes, pendingDeleteCount = 0 } strm
            else if prevRecordedHeight state - prevHeight state > checkpointSize
            then do
                tableInsert (_webCurrentPruneJob wbhdb) () (prevHeight state, startedFrom pruneJob)
                go state { prevRecordedHeight = prevHeight state } strm
            else do
                S.uncons strm >>= \case
                    Nothing -> do
                        executePendingDeletes (prevHeight state) (pendingDeletes state) (pendingDeleteCount state)
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
                    $ "PruneForks.pruneForks_: detected a corrupted database. Some block headers are missing"
                    <> ". Current live set: " <> encodeToText liveSet
                    <> ". Current header: " <> encodeToText (ObjectEncoded cur)
                    <> ". Previous height: " <> sshow prevHeight
            -- if we are at or beyond the lower bound,
            -- then we have no more pending fork tips to delete in the range
            | curHeight <= lowerBound pruneJob
            , curHeight < prevHeight
            , HashSet.null pendingForkTips = do
                when (pendingDeleteCount > 0) $
                    executePendingDeletes prevHeight pendingDeletes pendingDeleteCount
                return numPruned
            -- the current block is a pivot, thus we've seen its child block(s)
            -- and must keep it in the database.
            -- its parent takes its place as a live block.
            | curHash `elem` liveSet = do
                let !liveSet' =
                        HashSet.insert curParent $
                        HashSet.delete curHash liveSet
                let !pendingForkTips' =
                        HashSet.delete curParent pendingForkTips
                go
                    PruneState
                        { liveSet = liveSet'
                        , pendingForkTips = pendingForkTips'
                        , prevHeight = curHeight
                        , prevRecordedHeight
                        , numPruned
                        , pendingDeletes
                        , pendingDeleteCount
                        } strm
            -- the current block is not a pivot, so we haven't seen its child and can safely delete it
            | otherwise = do
                let !newDelete = view rankedBlockHash cur
                let !(!pendingDeletes', !pendingDeleteCount') =
                        (pendingDeletes & ix cid %~ (newDelete :)
                        , pendingDeleteCount + 1)
                let !numPruned' = numPruned + 1
                let pendingForkTips' = HashSet.delete curHash $
                        -- don't add a *new* pending fork tip if we're below the lower bound already
                        if not (HashSet.member curParent liveSet) &&
                            (curHeight > lowerBound pruneJob || HashSet.member curHash pendingForkTips)
                        then HashSet.insert curParent pendingForkTips
                        else pendingForkTips
                go
                    PruneState
                        { liveSet
                        , pendingForkTips = pendingForkTips'
                        , prevHeight = curHeight
                        , prevRecordedHeight
                        , numPruned = numPruned'
                        , pendingDeletes = pendingDeletes'
                        , pendingDeleteCount = pendingDeleteCount'
                        } strm
            where
            curParent = view (blockParent . _Parent) cur
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
    iterSeek it $ RankedBlockHash (BlockHeight $ int $ _getMaxRank mar + 1) nullBlockHash
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
    runContT (forM (_webBlockHeaderDb wbhdb) (\db -> ContT $ withReverseHeaderStream db mar)) $ \streamPerChain ->
        inner $ mergeN (Down . view blockHeight) $ toList streamPerChain
