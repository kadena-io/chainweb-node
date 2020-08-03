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

-- * Mark and sweep GC for payloads
, mkFilter
, markPayload
) where

import Chainweb.MerkleLogHash

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.ByteArray as BA
import Data.Coerce
import Data.Cuckoo
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup

import Foreign.Ptr

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
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
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

    -> (BlockHeader -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more.

    -> (BlockPayloadHash -> IO ())
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
-- The function takes a callback that is invoked on each deleted block header.
-- The callback takes a parameter that indicates whether the block payload hash
-- is shared with any non-deleted block header. There is a small rate of false
-- positives of block payload hashes that are marked in use.
--
-- This doesn't update the the cut db or the payload db.
--
pruneForks
    :: LogFunctionText
    -> BlockHeaderDb
    -> Natural
        -- ^ The depth at which deletion starts. Note, that the max rank isn't
        -- necessarly included in the current best cut. So one, should choose a
        -- depth for which one is confident that all forks are resolved.

    -> (BlockHeader -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more.

    -> (BlockPayloadHash -> IO ())
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
    -> (BlockHeader -> IO ())
    -> (BlockPayloadHash -> IO ())
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
                when (all (((==) `on` _blockHeight) cur) bs) $
                    mapM_ deletePayload (payloads L.\\ fmap _blockPayloadHash newPivots)

                return (newPivots, [], n)

    deleteHdr k = do
        -- TODO: make this atomic (create boilerplate to combine queries for
        -- different tables)
        casDelete (_chainDbCas cdb) (casKey $ RankedBlockHeader k)
        tableDelete (_chainDbRankTable cdb) (_blockHash k)
        logg Debug
            $ "pruned block header " <> encodeToText (_blockHash k)
            <> " at height " <> sshow (_blockHeight k)
        hdrCallback k

    deletePayload p = do
        logg Debug $ "call payload pruning callback for hash: " <> encodeToText p
        payloadCallback p

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

-- -------------------------------------------------------------------------- --
-- Mark and sweep GC for Payloads
--

newtype GcHash = GcHash MerkleLogHash
    deriving newtype (Show, ToJSON)

instance CuckooFilterHash GcHash where
    cuckooHash (Salt s) (GcHash a) = fnv1a_bytes s a  --  $ BA.takeView a 32
    cuckooFingerprint (Salt s) (GcHash a) = sip_bytes s a  -- s $ BA.takeView a 32
    {-# INLINE cuckooHash #-}
    {-# INLINE cuckooFingerprint #-}

gcHash :: Coercible a MerkleLogHash => a -> GcHash
gcHash = coerce
{-# INLINE gcHash #-}

type Filter = CuckooFilterIO 4 8 GcHash

mkFilter :: IO Filter
mkFilter = do
    newCuckooFilter 0 80000000
        -- 100 items per block (as of summer 2020)
        -- TODO: should depend on current block height

markPayload
    :: HasCasLookupConstraint cas BlockPayload
    => PayloadDb cas
    -> Filter
    -> BlockPayloadHash
    -> IO ()
markPayload db cf h = do
    casLookup pdb h >>= \case
        Nothing -> error "corrupted database: payload not found"
        Just payload -> do
            tryInsert "payload hash" (gcHash $ _blockPayloadPayloadHash payload)
            tryInsert "transactions hash" (gcHash $ _blockPayloadTransactionsHash payload)
            tryInsert "outputs hash" (gcHash $ _blockPayloadOutputsHash payload)
  where
    tryInsert k a = do
        -- inserting a large number of equal elements causes the filter to fail.
        m <- member cf a
        if m
          then
            print $ "member found for " <> k <> ": " <> encodeToText a
          else
            unlessM (insert cf a) $
                error "failed to insert item in cuckoo filter: increase the size of the filter and try again"

    pdb = _transactionDbBlockPayloads $ _transactionDb db

    -- Tables
    --
    -- BlockPayloadStore - BlockPayload:
    --     *BlockPayloadHash, BlockTransactionsHash, BlockOutputsHash
    --
    -- BlockTransactionStore - BlockTransactions:
    --     *BlockTransactionsHash, Vector Transactions, MinerData
    --
    -- BlockOutputsStore - BlockOutputs:
    --     *BlockOutputsHash, Vector TransactionOutput, CoinbaseOutput
    --
    -- TransactionTreeStore - TransactionTree:
    --     *BlockTransactionsHash, MerkleTree
    --
    -- OutputTreeStore - OutputTree
    --     *BlockOutputsHash, MerkleTree
    --


    -- 1. Delete payloads hashes
    -- 2. do payload mark and sweep gc after pruning all chain databases
