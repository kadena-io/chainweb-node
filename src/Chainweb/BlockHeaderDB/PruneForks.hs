{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
) where

import Control.Monad
import Control.Monad.ST

import qualified Data.BloomFilter.Easy as BF (suggestSizing)
import qualified Data.BloomFilter.Hash as BF
import qualified Data.BloomFilter.Mutable as BF
import Data.Function
import qualified Data.HashSet as HS
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
import Chainweb.Logger
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec)

import Data.CAS
import Data.CAS.RocksDB
import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Prune Old Forks

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
    :: Logger logger
    => logger
    -> BlockHeaderDb
    -> Natural
        -- ^ The depth at which the roots are collected for marking block
        -- headers that are kept. Any fork that was active that the given depth
        -- is kept.
        --
        -- The depth is computed based on the entry of maximum rank. This entry
        -- is not necessarily included in the overall consensus of the chainweb.
        -- In particular the the higest ranking entry is not necessarily the
        -- entry of largest POW weight.
        --
        -- Usually this number would be defined in terms of the chainweb
        -- diameter and/or the difficulty adjustment window.
    -> (BlockHeader -> Bool -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more. The Boolean argument indicates whether the payload
        -- of the block is shared with any block header that isn't marked for
        -- deletion.
    -> IO Int
pruneForks logger = pruneForks_ logg
  where
    logg = logFunctionText (setComponent "pact-tx-replay" logger)


pruneForks_
    :: LogFunctionText
    -> BlockHeaderDb
    -> Natural
    -> (BlockHeader -> Bool -> IO ())
    -> IO Int
pruneForks_ logg cdb limit callback = do

    -- find all roots at \(maxEntry - limit\)
    --
    m <- maxRank cdb
    let rootHeight = m - min m limit

    !roots <- keys cdb Nothing Nothing
        (Just $ MinRank $ Min rootHeight)
        Nothing
            -- include all block headers in the root set, so that all headers
            -- are included in the marking phase. PayloadBlockHashes can be
            -- shared between blocks at different height, so we must make sure
            -- that they are retained if a root depends on them.
        streamToHashSet_

    -- Bloom filter for marking block headers
    --
    let s = max (int rootHeight + 10 * HS.size roots) (int rootHeight * 2)
    let (size, hashNum) = BF.suggestSizing s 0.0001
    !marked <- stToIO $ BF.new (BF.cheapHashes hashNum) size

    -- Bloom filter for marking block payload hashes
    --
    -- Payloads can be shared between different blocks. Thus, to be on the safe
    -- side, we are not deleting any marked payload.
    --
    -- Note that we could use cuckoo hashes or counting bloom filters to do
    -- reference counting and collect more unreferenced payloads.
    --
    -- TODO: would it be better to use a single shared filter?
    --
    let (psize, phashNum) = BF.suggestSizing s 0.0001
    !markedPayloads <- stToIO $ BF.new (BF.cheapHashes phashNum) psize

    -- Iterate backwards and mark all predecessors of the roots
    --
    void $ branchEntries cdb Nothing Nothing Nothing Nothing mempty (HS.map UpperBound roots)
        $ S.mapM_ $ \h -> stToIO $ do
            BF.insert marked $ runPut $ encodeBlockHash $ _blockHash h
            BF.insert markedPayloads
                $ runPut $ encodeBlockPayloadHash $ _blockPayloadHash h

    -- Iterate forward and delete all non-marked block headers that are not a
    -- predecessor of a block header that is kept.
    --
    entries cdb Nothing Nothing Nothing (Just $ MaxRank $ Max rootHeight)
        $ S.foldM_ (go marked markedPayloads) (return (mempty, 0)) (return . snd)

  where

    -- The falsePositiveSet collects all deleted nodes that are known to be
    -- false positives.
    --
    -- We know that a node is false positive when it is in the filter but any of
    -- its ancestors is neither in the filter nor in the falsePositiveSet.
    -- (Because for a true positive all ancestors are in the filter.) We also
    -- know that a node is false positive if it's payload isn't marked, because
    -- we mark the payload of all nodes that are true positives.
    --
    -- Note that this doesn't capture all false positives, but only those that
    -- are not connected to the main chain.
    --
    -- Nodes that are known to be false positives are safe to remove, if also
    -- all of its successors are removed.
    --
    -- Nodes that are known to be false postives must be removed, if any of their
    -- predecessors got removed or if their payload got removed.
    --
    go marked markedPayloads (!falsePositiveSet, !i) !h = do
        let k = runPut $ encodeBlockHash $ _blockHash h
        let p = runPut $ encodeBlockHash $ _blockParent h
        isMarked <- stToIO $ BF.elem k marked

        let payloadHashBytes = runPut $ encodeBlockPayloadHash $ _blockPayloadHash h
        isPayloadMarked <- stToIO $ BF.elem payloadHashBytes markedPayloads

        -- Delete nodes not in the filter
        if not isMarked
          then do
            deleteKey h isPayloadMarked
            return (falsePositiveSet, succ i)
          else do
            -- Delete nodes which parent isn't in the filter or is in the
            -- falsePositiveSet
            parentIsMarked <- stToIO $ BF.elem p marked
            if not (isGenesisBlockHeader h) && (not parentIsMarked || HS.member p falsePositiveSet || not isPayloadMarked)
              then do
                -- We know that this a false positive. We keep track of this for
                -- future reference.
                --
                -- TODO: consider using cuckoo filters because entries can be
                -- deleted from them. So we wouldn't need to keep track of
                -- deleted falsePositives. However, with cuckoo filters we'd
                -- have to re-hash or keep track of failing inserts.
                deleteKey h isPayloadMarked
                return (HS.insert k falsePositiveSet, succ i)
              else
                -- The key is either
                -- 1. in the chain or
                -- 2. is a false positive that has a payload and is connected to
                --    the chain (i.e. has all of its predecessors).
                --
                -- We accept a small number of nodes of the second case.
                --
                return (falsePositiveSet, i)

    deleteKey h isPayloadMarked = do
        -- TODO: make this atomic (create boilerplate to combine queries for
        -- different tables)
        casDelete (_chainDbCas cdb) (casKey $ RankedBlockHeader h)
        tableDelete (_chainDbRankTable cdb) (_blockHash h)
        logg Debug $ "deleted block header at height " <> sshow (_blockHeight h) <> " with payload mark " <> sshow isPayloadMarked
        callback h isPayloadMarked

