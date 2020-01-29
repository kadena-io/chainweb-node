{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- due to module import cycle-breaking with pact: pact wants a BlockHeaderDB,
-- but the TreeDB instance wants to know about genesis blocks, which requires
-- validation, which requires pact
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.BlockHeaderDB
(
-- * Chain Database Handle
  Configuration(..)
, BlockHeaderDb
, initBlockHeaderDb
, closeBlockHeaderDb
, withBlockHeaderDb
, pruneForks

-- internal
, seekTreeDb
) where

import Control.Arrow
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.Trans.Maybe

import qualified Data.BloomFilter.Easy as BF (suggestSizing)
import qualified Data.BloomFilter.Hash as BF
import qualified Data.BloomFilter.Mutable as BF
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup
import qualified Data.Text.Encoding as T

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.TreeDB
import Chainweb.TreeDB.Validation
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.CAS
import Data.CAS.RocksDB

-- -------------------------------------------------------------------------- --
-- Internal

type E = BlockHeader

encodeRankedBlockHeader :: MonadPut m => RankedBlockHeader -> m ()
encodeRankedBlockHeader = encodeBlockHeader . _getRankedBlockHeader
{-# INLINE encodeRankedBlockHeader #-}

decodeRankedBlockHeader :: MonadGet m => m RankedBlockHeader
decodeRankedBlockHeader = RankedBlockHeader <$!> decodeBlockHeader
{-# INLINE decodeRankedBlockHeader #-}

encodeRankedBlockHash :: MonadPut m => RankedBlockHash -> m ()
encodeRankedBlockHash (RankedBlockHash r bh) = do
    encodeBlockHeightBe r
    encodeBlockHash bh
{-# INLINE encodeRankedBlockHash #-}

decodeRankedBlockHash :: MonadGet m => m RankedBlockHash
decodeRankedBlockHash = RankedBlockHash
    <$!> decodeBlockHeightBe
    <*> decodeBlockHash
{-# INLINE decodeRankedBlockHash #-}

-- -------------------------------------------------------------------------- --
-- | Configuration of the chain DB.
--
data Configuration = Configuration
    { _configRoot :: !BlockHeader
    , _configRocksDb :: !RocksDb
    }

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
pruneForks logger cdb limit callback = do

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
        $ S.mapM_ $ \h -> do
            stToIO $ do
                BF.insert marked $ runPut $ encodeBlockHash $ _blockHash h
                BF.insert markedPayloads
                    $ runPut $ encodeBlockPayloadHash $ _blockPayloadHash h

    -- Iterate forward and delete all non-marked block headers that are not a
    -- predecessor of a block header that is kept.
    --
    entries cdb Nothing Nothing Nothing (Just $ MaxRank $ Max rootHeight)
        $ S.foldM_ (go marked markedPayloads) (return (mempty, 0)) (return . snd)

  where
    logg = logFunctionText (setComponent "pact-tx-replay" logger)

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
    go marked markedPayloads !(!falsePositiveSet, !i) !h = do
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

-- -------------------------------------------------------------------------- --
-- Insert
--
-- Only functions in this section are allowed to modify values of the Db type.

-- | A a new entry to the database
--
-- Updates all indices.
--
dbAddChecked :: BlockHeaderDb -> BlockHeader -> IO ()
dbAddChecked db e = unlessM (casMember (_chainDbCas db) ek) dbAddCheckedInternal
  where
    r = int $ rank e
    ek = RankedBlockHash r (_blockHash e)

    -- Internal helper methods

    -- | Unchecked addition
    --
    -- ASSUMES that
    --
    -- * Item is not yet in database
    --
    dbAddCheckedInternal :: IO ()
    dbAddCheckedInternal = case parent e of
        Nothing -> add
        Just p -> casLookup (_chainDbCas db) (RankedBlockHash (r - 1) p)  >>= \case
            Nothing -> throwM $ TreeDbParentMissing @BlockHeaderDb e
            Just (RankedBlockHeader pe) -> do
                unless (rank e == rank pe + 1)
                    $ throwM $ TreeDbInvalidRank @BlockHeaderDb e
                add
      where

    add = updateBatch
            [ RocksDbInsert (_chainDbCas db) (casKey rbh) rbh
            , RocksDbInsert (_chainDbRankTable db) (_blockHash e) (_blockHeight e)
            ]
      where
        rbh = RankedBlockHeader e

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Initialize a database handle
--
initBlockHeaderDb :: Configuration -> IO BlockHeaderDb
initBlockHeaderDb config = do
    dbAddChecked db rootEntry
    return db
  where
    rootEntry = _configRoot config
    cid = _chainId rootEntry
    cidNs = T.encodeUtf8 (toText cid)

    headerTable = newTable
        (_configRocksDb config)
        (Codec (runPut . encodeRankedBlockHeader) (runGet decodeRankedBlockHeader))
        (Codec (runPut . encodeRankedBlockHash) (runGet decodeRankedBlockHash))
        ["BlockHeader", cidNs, "header"]

    rankTable = newTable
        (_configRocksDb config)
        (Codec (runPut . encodeBlockHeight) (runGet decodeBlockHeight))
        (Codec (runPut . encodeBlockHash) (runGet decodeBlockHash))
        ["BlockHeader", cidNs, "rank"]

    !db = BlockHeaderDb cid
        (_chainwebVersion rootEntry)
        headerTable
        rankTable

-- | Close a database handle and release all resources
--
closeBlockHeaderDb :: BlockHeaderDb -> IO ()
closeBlockHeaderDb _ = return ()

withBlockHeaderDb
    :: RocksDb
    -> ChainwebVersion
    -> ChainId
    -> (BlockHeaderDb -> IO b)
    -> IO b
withBlockHeaderDb db v cid = bracket start closeBlockHeaderDb
  where
    start = initBlockHeaderDb Configuration
        { _configRoot = genesisBlockHeader v cid
        , _configRocksDb = db
        }

-- -------------------------------------------------------------------------- --
-- TreeDB instance

-- | TODO provide more efficient branchEntries implementation that uses
-- iterators.
--
instance TreeDb BlockHeaderDb where
    type DbEntry BlockHeaderDb = BlockHeader

    lookup db h = runMaybeT $ do
        -- lookup rank
        r <- MaybeT $ tableLookup (_chainDbRankTable db) h

        -- lookup header
        rh <- MaybeT $ casLookup (_chainDbCas db) $ RankedBlockHash r h

        return $! _getRankedBlockHeader rh
    {-# INLINEABLE lookup #-}

    entries db k l mir mar f = withSeekTreeDb db k mir $ \it -> f $ do
        iterToValueStream it
            & S.map _getRankedBlockHeader
            & maybe id (\x -> S.takeWhile (\a -> int (_blockHeight a) <= x)) mar
            & limitStream l
    {-# INLINEABLE entries #-}

    branchEntries = chainBranchEntries
    {-# INLINEABLE branchEntries #-}

    keys db k l mir mar f = withSeekTreeDb db k mir $ \it -> f $ do
        iterToKeyStream it
            & maybe id (\x -> S.takeWhile (\a -> int (_rankedBlockHashHeight a) <= x)) mar
            & S.map _rankedBlockHash
            & limitStream l
    {-# INLINEABLE keys #-}

    insert db e = liftIO $ insertBlockHeaderDb db [e]
    {-# INLINEABLE insert #-}

    maxEntry db = withTableIter (_chainDbCas db) $ \it -> do
        tableIterLast it
        tableIterValue it >>= \case
            Just (RankedBlockHeader !r) -> return r
            Nothing -> throwM
                $ InternalInvariantViolation "BlockHeaderDb.maxEntry: empty block header db"
    {-# INLINEABLE maxEntry #-}

    maxRank db = withTableIter (_chainDbCas db) $ \it -> do
        tableIterLast it
        tableIterKey it >>= \case
            Just (RankedBlockHash !r _) -> return $! int r
            Nothing -> throwM
                $ InternalInvariantViolation "BlockHeaderDb.maxRank: empty block header db"
    {-# INLINEABLE maxRank #-}

withSeekTreeDb
    :: BlockHeaderDb
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> (RocksDbTableIter RankedBlockHash RankedBlockHeader -> IO a)
    -> IO a
withSeekTreeDb db k mir = bracket (seekTreeDb db k mir) releaseTableIter
{-# INLINE withSeekTreeDb #-}

-- | If @k@ is not 'Nothing', @seekTreeDb d k mir@ seeks key @k@ in @db@. If the
-- key doesn't exist it throws @TreeDbKeyNotFound@. Otherwise if @k@ was
-- exclusive, it advance the iterator to the next item. If @minr@ is not
-- 'Nothing' and the iterator is at a position smaller than @minr@ an invalid
-- iterator is returned. Otherwise the iterator is returned.
--
-- If @k@ is 'Nothing' and @minr@ is not 'Nothing' it seeks up to @minr@ and
-- returns the iterator.
--
-- If both @k@ and @minr@ are 'Nothing' it returns an iterator that points to
-- the first entry in @d@.
--
seekTreeDb
    :: BlockHeaderDb
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> IO (RocksDbTableIter RankedBlockHash RankedBlockHeader)
seekTreeDb db k mir = do
    !it <- createTableIter (_chainDbCas db)
    case k of
        Nothing -> case mir of
            Nothing -> return ()
            Just r -> tableIterSeek it
                $ RankedBlockHash (BlockHeight $ int $ _getMinRank r) nullBlockHash

        Just a -> do

            -- Seek to cursor
            let x = _getNextItem a
            r <- tableLookup (_chainDbRankTable db) x >>= \case
                Nothing -> throwM $ TreeDbKeyNotFound @BlockHeaderDb x
                (Just !b) -> return b
            tableIterSeek it (RankedBlockHash r x)

            -- if we don't find the cursor, throw exception
            tableIterKey it >>= \case
                Just (RankedBlockHash _ b) | b == x -> return ()
                _ -> throwM $ TreeDbKeyNotFound @BlockHeaderDb x

            -- If the cursor is exclusive, then advance the iterator
            when (isExclusive a) $ tableIterNext it

            -- Check minimum rank. Return invalid iter if cursor is below
            -- minimum rank.
            tableIterKey it >>= \case
                Just (RankedBlockHash r' _) | Just m <- mir, int r' < m -> invalidIter it
                _ -> return ()
    return it
  where
    invalidIter it = tableIterLast it >> tableIterNext it

-- -------------------------------------------------------------------------- --
-- Insertions

insertBlockHeaderDb :: BlockHeaderDb -> [E] -> IO ()
insertBlockHeaderDb db es = do

    -- Validate set of additions
    validateAdditionsDbM db $ HM.fromList $ (key &&& id) <$!> es

    -- add set of additions to database
    mapM_ (dbAddChecked db) rankedAdditions
  where
    rankedAdditions = L.sortOn rank es
