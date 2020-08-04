{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
( pruneForksLogg
, pruneForks
, pruneForks_
) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec)

import Data.CAS
import Data.CAS.RocksDB
import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Chain Database Pruning

pruneForksLogg
    :: Logger logger
    => logger
    -> BlockHeaderDb
    -> Natural
        -- ^ The depth at which deletion starts. Note, that the max rank isn't
        -- necessarly included in the current best cut. So one, should choose a
        -- depth for which one is confident that all forks are resolved.

    -> (Bool -> BlockHeader -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more.

    -> (Bool -> BlockPayloadHash -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more.
    -> IO Int
pruneForksLogg logger = pruneForks logg
  where
    logg = logFunctionText (setComponent "ChainDatabasePrunning" logger)

-- | Prunes most block headers and block payloads from forks that are older than
-- the given number of blocks.
--
-- This function guarantees that the predecessors of all remaining nodes also
-- remain in the database and that there are are no tangling references.
--
-- This function doesn't guarantee to delete all blocks on forks. A small number
-- of fork blocks may not get deleted.
--
-- The function takes callbacks that are invoked on each block header and
-- paylaod hash. The callback takes a parameter that indicates whether the
-- related block is pruned or not.
--
pruneForks
    :: LogFunctionText
    -> BlockHeaderDb
    -> Natural
        -- ^ The depth at which deletion starts. Note, that the max rank isn't
        -- necessarly included in the current best cut. So one, should choose a
        -- depth for which one is confident that all forks are resolved.

    -> (Bool -> BlockHeader -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more.

    -> (Bool -> BlockPayloadHash -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more.
    -> IO Int
pruneForks logg cdb depth headerCallback payloadCallback = do
    hdr <- maxEntry cdb
    let mar = MaxRank $ Max $ int (_blockHeight hdr) - depth
    pruneForks_ logg cdb mar (MinRank $ Min 0) headerCallback payloadCallback

-- | TODO add option to also validate the block headers
--
pruneForks_
    :: LogFunctionText
    -> BlockHeaderDb
    -> MaxRank
    -> MinRank
    -> (Bool -> BlockHeader -> IO ())
    -> (Bool -> BlockPayloadHash -> IO ())
    -> IO Int
pruneForks_ logg cdb mar mir hdrCallback payloadCallback = do

    !pivots <- entries cdb Nothing Nothing
        (Just $ MinRank $ Min $ _getMaxRank mar)
        (Just mar)
        S.toList_

    when (null pivots) $ do
        logg Warn
            $ "Skipping database pruning because of an empty set of block headers at upper pruning bound " <> sshow mar
            <> ". This would otherwise delete the complete database."

    withReverseHeaderStream cdb (mar - 1) mir $
        S.foldM_ go (return (pivots, [], 0)) (\(_, _, n) -> return n)
  where

    go
        :: ([BlockHeader], [BlockPayloadHash], Int)
        -> BlockHeader
        -> IO ([BlockHeader], [BlockPayloadHash], Int)

    go ([], _, _) _ = error "impossible" -- FIXME

    -- Note that almost always `pivots` is a singleton list and `bs` is empty.
    -- Also `payloads` almost always empty.
    --
    go (!pivots, !payloads, !n) cur = do

        -- Sanity Check: make sure didn't miss the pivot:
        --
        when (_blockHeight cur + 1 < maximum (fmap _blockHeight pivots)) $ do
            let pivot = head pivots
            throwM
                $ TreeDbAncestorMissing @BlockHeaderDb pivot (int (_blockHeight cur))
                $ "Corrupted chain database for chain " <> toText (_chainId cdb)
                <> ". The chain db must be deleted and re-resynchronized."

            -- FIXME: try to repair the database by fetching the missing
            -- block from remote peers?
            --
            -- It's probably the best to write an independent repair
            -- program or module

        case L.partition (\p -> _blockHash cur == _blockParent p) pivots of

            -- Delete element
            ([], _) -> do
                deleteHdr cur
                hdrCallback True cur
                return (pivots, _blockPayloadHash cur : payloads, n+1)

            -- We've got a new pivot. This case happens almost always.
            --
            (_, bs) -> do
                -- TODO: add intrinsic and inductive valiation?

                let newPivots = cur : bs

                -- When after adding this pivot all pivots have the same block
                -- height we can delete the pending payloads, since we've seen
                -- the payloads of all pivots down to the current height.
                --
                -- This check is fast when bs is empty.
                --
                when (all (((==) `on` _blockHeight) cur) bs) $ do
                    let pivotPayloads = _blockPayloadHash <$> newPivots
                    forM_ (_blockPayloadHash cur : payloads) $ \p -> do
                        let del = not $ elem p pivotPayloads
                        when del $ logg Debug
                            $ "call payload callback for deleted payload: " <> encodeToText p
                        payloadCallback del p

                hdrCallback False cur
                return (newPivots, [], n)

    deleteHdr k = do
        -- TODO: make this atomic (create boilerplate to combine queries for
        -- different tables)
        casDelete (_chainDbCas cdb) (casKey $ RankedBlockHeader k)
        tableDelete (_chainDbRankTable cdb) (_blockHash k)
        logg Debug
            $ "pruned block header " <> encodeToText (_blockHash k)
            <> " at height " <> sshow (_blockHeight k)

-- -------------------------------------------------------------------------- --
-- Utils

-- TODO: provide this function in chainweb-storage:
--
-- Returns the stream of key-value pairs of an 'RocksDbTableIter' in reverse
-- order.
--
-- The iterator must be released after the stream is consumed. Releasing the
-- iterator to early while the stream is still in use results in a runtime
-- error. Not releasing the iterator after the processing of the stream has
-- finished results in a memory leak.
--
iterToReverseValueStream :: RocksDbTableIter k v -> S.Stream (S.Of v) IO ()
iterToReverseValueStream it = liftIO (tableIterValue it) >>= \case
    Nothing -> return ()
    Just x -> S.yield x >> liftIO (tableIterPrev it) >> iterToReverseValueStream it
{-# INLINE iterToReverseValueStream #-}

withReverseHeaderStream
    :: BlockHeaderDb
    -> MaxRank
    -> MinRank
    -> (S.Stream (S.Of BlockHeader) IO () -> IO a)
    -> IO a
withReverseHeaderStream db mar mir inner = withTableIter headerTbl $ \it -> do
    tableIterSeek it $ RankedBlockHash (BlockHeight $ int $ _getMaxRank mar + 1) nullBlockHash
    tableIterPrev it
    inner $ iterToReverseValueStream it
        & S.map _getRankedBlockHeader
        & S.takeWhile (\a -> int (_blockHeight a) >= mir)
    where
    headerTbl = _chainDbCas db
{-# INLINE withReverseHeaderStream #-}

