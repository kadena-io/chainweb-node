{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Chainweb.PruneChainDatabase
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- There is a block header database for each, but only a single shared payload
-- database.
--
-- Since BlockTransactions can be shared between blocks, even across chains,
-- we'll have to take a global approach at garabage collecting payloads.
--
-- There's also the advantage of guaranteeing that there are no concurrent
-- writes during the garbage collections, because the chain databases aren't
-- initialized yet.
--
-- 1. Concurrently for each chain:
--     * initialize temporary block header db,
--     * prune the block header db, and
--     * use the callbacks to delete those parts of the payloads that are
--       per chain, and also mark all payloads and all transactions.
-- 2. traverse payloads and delete all unused entries.
-- 3. traverse transactions and delete all unused entries.
--
-- /Notes on the GC scope:/
--
-- The second step is optional. It shouldn't be needed in most cases, but should
-- be performed on the first run of this function and after that from time to
-- time to cleanup garbage that could result from crashes.
--
-- The third step is optional, too. By skipping it, some garbage accumulates,
-- but it's a relatively small amount and skipping make pruning much more
-- efficient. It should be performed from time to time.
--
-- Garabage collecting all other payload tables isn't currently supported --
-- we assume that the overhead due to gargabe in those tables (which can
-- accumulate during crashes) is smaller than the false positive rate of the
-- member ship query sets. We may add support for offline GC in the future.
-- In order to support all tables we would also have to mark BlockOutputHashes.
--
-- /Implementation:/
--
-- For marking Cuckoo filters are used. Cuckoo filters have the disadvantage
-- that they don't support concurrent insertion. Also merging cuckoo filters is
-- slow. So we rather use seperate filters for each chain and query them
-- individually. We may explore other techniques to represent set membership in
-- the future.
--
-- /TODO:/
--
-- * implement incremental pruning (which can't be used with mark and sweep,
--   though)
--
-- * Consider changing the database format to store the transactions with the
--   block payloads and eliminate sharing. Sharing is benefitial only when most
--   blocks are empty and the there's only a small number of pools. However, in
--   that case storage requirements are moderate anyways. When most block
--   contain transactions the benefits of sharing become marginal, but the
--   saving during GC (in particular incremental GC) become larger.
--
--
module Chainweb.Chainweb.PruneChainDatabase
( pruneAllChains
, fullGc
) where

import Chainweb.BlockHeader

import Control.Concurrent.Async
import Control.Monad

import Data.Aeson hiding (Error)
import qualified Data.ByteArray as BA
import Data.CAS
import Data.CAS.RocksDB
import Data.Cuckoo
import Data.Foldable

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.LogLevel
import System.Random

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.PruneForks
import Chainweb.BlockHeight
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Data.LogMessage

-- | Prune all chains and cleanup BlockPayloads and BlockOutputs.
--
-- This doesn't cleanup BlockTransactions in the payload database.
--
-- Note: this assumes that the payload db is already initialized, i.e. that
-- genesis headers have been injected.
--
pruneAllChains
    :: Logger logger
    => logger
    -> RocksDb
    -> ChainwebVersion
    -> IO ()
pruneAllChains logger rdb v = do
    forConcurrently_ (toList $ chainIds v) pruneChain
  where
    pdb = newPayloadDb rdb
    diam = diameter $ chainGraphAt v (maxBound @BlockHeight)
    pruneChain cid = withBlockHeaderDb rdb v cid $ \cdb -> do
        let chainLogger = addLabel ("chain", toText cid) logger
            chainLogg = logFunctionText chainLogger
        chainLogg Info "start pruning block header database"
        x <- pruneForksLogg chainLogger cdb (diam * 3)
            (\_ _ -> return ())
            (payloadGcCallback chainLogg pdb Nothing Nothing)
        chainLogg Info $ "finished pruning block header database. Deleted " <> sshow x <> " block headers."

-- | Prune all chains and cleanup BlockPayloads and BlockOutputs.
--
-- This also cleans up BlockTransactions in the payload database.
--
-- It currently doesn't garbage collect entries that aren't reachable from the
-- BlockPayload table. This can happen when a node crashes, but is expected to
-- be rare. Since, we take an probablistic take an garbage collection we don't
-- care.
--
-- Note: this assumes that the payload db is already initialized, i.e. that
-- genesis headers have been injected.
--
fullGc
    :: Logger logger
    => logger
    -> RocksDb
    -> ChainwebVersion
    -> IO ()
fullGc logger rdb v = do
    logg Info $ "Starting chain database garbage collection"

    -- 1. Concurrently prune chain dbs (TODO use a pool to limit concurrency)
    (markedPayloads, markedTrans) <- unzip <$> forConcurrently (toList $ chainIds v) pruneChain
    logg Info $ "Finished mark phase"

    -- TODO: paralleize he the sweep phases to use all available cors.

    -- 2. Traverse BlockPayloads table
    --
    let sweepPayloads = do
            logg Info $ "Sweeping BlockPayloads and Outputs"
            c0 <- withTableIter payloadsTable $ \it -> do
                iterToValueStream it
                    & S.filterM (fmap not . checkMark markedPayloads . _blockPayloadPayloadHash)
                    & S.mapM (deleteBlockPayload logg db)
                    & S.length_
            logg Info $ "Swept entries for " <> sshow c0 <> " block payload hashes"

    -- 3. Traverse BlockTransactions table
    --
    let sweepTransactions = do
            logg Info $ "Sweeping BlockTransactions"
            c1 <- withTableIter transactionsTable $ \it -> do
                iterToValueStream it
                    & S.map _blockTransactionsHash
                    & S.filterM (fmap not . checkMark markedTrans)
                    & S.mapM (deleteBlockTransactions logg db)
                    & S.length_
            logg Info $ "Swept entries for " <> sshow c1 <> " block transactions hashes"

    concurrently_ sweepPayloads sweepTransactions
    logg Info $ "Finished chain database garbage collection"

  where

    logg = logFunctionText logger
    diam = diameter $ chainGraphAt v (maxBound @BlockHeight)
    depth = diam * 3

    db = newPayloadDb rdb

    -- Extract RocksDB Tables from Payload Db
    payloadsTable :: RocksDbTable BlockPayloadHash BlockPayload
    payloadsTable = _getRocksDbCas t
      where
        BlockPayloadStore t = _transactionDbBlockPayloads $ _transactionDb db

    transactionsTable :: RocksDbTable BlockTransactionsHash BlockTransactions
    transactionsTable = _getRocksDbCas t
      where
        BlockTransactionsStore t = _transactionDbBlockTransactions $ _transactionDb db


    -- Prune a single chain and return the sets of marked payloads and
    -- transactions.
    --
    pruneChain cid = withBlockHeaderDb rdb v cid $ \cdb -> do
        let chainLogger = addLabel ("chain", toText cid) logger
            chainLogg = logFunctionText chainLogger

        m <- maxRank cdb
        markedTrans <- mkFilter (round $ int @_ @Double m * 1.1)
        markedPayloads <- mkFilter (round $ int @_ @Double m * 1.1)

        logg Info $ "Allocated "
            <> sshow ((sizeInAllocatedBytes markedTrans + sizeInAllocatedBytes markedPayloads) `div` (1024 * 1024))
            <> "MB for marking database entries"

        -- TODO mark all entries above a depth of depth, so it doesn't get GCed
        void $ entries cdb Nothing Nothing (Just $ int $ depth + 1) Nothing $ \s -> s
            & S.map _blockPayloadHash
            & S.mapM_ (payloadGcCallback chainLogg db (Just markedPayloads) (Just markedTrans) False)

        chainLogg Info "start pruning block header database"
        x <- pruneForksLogg chainLogger cdb depth
            (loggHdrDelete chainLogg)
            (payloadGcCallback chainLogg db (Just markedPayloads) (Just markedTrans))

        chainLogg Info $ "finished pruning block header database. Deleted " <> sshow x <> " block headers."
        return (markedPayloads, markedTrans)

    -- TODO: consider using bloom fiters instead that can be merged. Alternatively,
    -- implement concurrent insertion for cuckoo filters, where the hashing is done
    -- concurrently and a lock is used only for the actual modification of the
    -- underlying buffer. Or do fine grained locking on the filter.
    --
    checkMark :: BA.ByteArrayAccess a => [(Filter a)] -> a -> IO Bool
    checkMark [] _ = return False
    checkMark (h : t) a = member h (GcHash a) >>= \case
        True -> return True
        False -> checkMark t a


    loggHdrDelete _ False _ = return ()
    loggHdrDelete l True h = l Info
        $ "pruned header " <> toText (_blockHash h)
        <> " at height " <> sshow (_blockHeight h)

-- -------------------------------------------------------------------------- --
-- Utils for Mark and sweep GC for Payloads
--

-- | Wraps any types wrapper around a MerkleLogHash for usage with a Cuckoo
-- filter.
--
newtype GcHash a = GcHash a
    deriving newtype (Show, ToJSON)

instance BA.ByteArrayAccess a => CuckooFilterHash (GcHash a) where
    cuckooHash (Salt s) (GcHash a) = fnv1a_bytes s $ BA.takeView a 8
    cuckooFingerprint (Salt s) (GcHash a) = sip_bytes s $ BA.takeView a 8
    {-# INLINE cuckooHash #-}
    {-# INLINE cuckooFingerprint #-}

gcHash :: a -> GcHash a
gcHash = GcHash
{-# INLINE gcHash #-}

type Filter a = CuckooFilterIO 4 10 (GcHash a)

mkFilter :: Natural -> IO (Filter a)
mkFilter n = do
    s <- randomIO
    newCuckooFilter (Salt s) n

-- | 'BlockPayloadHash'es and 'BlockOutputsHash'es are unique up to orphans
-- (and genesis blocks). They can therefore be garbage collected during database
-- pruning.
--
-- 'BlockTransactionHash's are not unique for block payload hashes. We use
-- mark and sweep garbage collection to remove them.
--
payloadGcCallback
    :: CasConstraint cas BlockPayload
    => CasConstraint cas BlockOutputs
    => CasConstraint cas OutputTree
    => LogFunctionText
    -> PayloadDb cas
    -> (Maybe (Filter BlockPayloadHash))
        -- ^ Set of marked payloads
    -> (Maybe (Filter BlockTransactionsHash))
        -- ^ Set of marked block transactions
        --
        -- This are shared globally (across chains) and it is possible that a
        -- payload isn't marked but the block transactions of the payload are
        -- marked.
        --
    -> Bool
    -> BlockPayloadHash
    -> IO ()
payloadGcCallback logg db markedPayloads markedTrans isDelete h = do
    casLookup pdb h >>= \case
        Nothing -> logg Error
            $ "While pruning database: payload not found for " <> encodeToText h
            <> "; block deleted: " <> sshow isDelete
            <> ". The database may be corrupted. In case of doubt it is recommended to delete the database and synchronize a new database"
        Just payload -> if isDelete
          then deleteBlockPayload logg db payload

          else do
             -- mark PayloadTransactionsHash as being used
            forM_ markedPayloads $ \cf ->
                tryInsert cf "payload hash" (gcHash $ _blockPayloadPayloadHash payload)
            forM_ markedTrans $ \cf ->
                tryInsert cf "transactions hash" (gcHash $ _blockPayloadTransactionsHash payload)
  where
    tryInsert cf k a = do
        -- inserting a somewhat larger number (I think, it's actually 7) of
        -- equal elements causes the filter to fail.
        unlessM (member cf a) $
            unlessM (insert cf a) $ error
                $ "failed to insert item " <> k <> " in cuckoo filter"
                <> ": while very rare this can happen. Usually it is resolve by retrying."

    pdb = _transactionDbBlockPayloads $ _transactionDb db

-- -------------------------------------------------------------------------- --
-- Delete Payload
--
-- Payload Components are deleted in a way such that it is guaranteed that there
-- are no dangling references.
--
-- /Tables:/
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

deleteBlockPayload
    :: CasConstraint cas BlockPayload
    => CasConstraint cas BlockOutputs
    => CasConstraint cas OutputTree
    => LogFunctionText
    -> PayloadDb cas
    -> BlockPayload
    -> IO ()
deleteBlockPayload logg db p = do
    logg Debug $ "Delete PayloadHash and OutputsHash for " <> encodeToText (_blockPayloadPayloadHash p)
    -- delete BlockPayload, BlockOutputs, and OutputTree
    casDelete pdb (_blockPayloadPayloadHash p)
    casDelete odb (_blockPayloadOutputsHash p)
    casDelete otdb (_blockPayloadOutputsHash p)
  where
    pdb = _transactionDbBlockPayloads $ _transactionDb db
    odb = _payloadCacheBlockOutputs $ _payloadCache db
    otdb = _payloadCacheOutputTrees $ _payloadCache db

deleteBlockTransactions
    :: CasConstraint cas BlockTransactions
    => CasConstraint cas TransactionTree
    => LogFunctionText
    -> PayloadDb cas
    -> BlockTransactionsHash
    -> IO ()
deleteBlockTransactions logg db p = do
    logg Debug $ "Delete BlockTransactionsHash for " <> encodeToText p
    -- delete BlockTransactions and TransactionsTree
    casDelete tdb p
    casDelete ttdb p
  where
    tdb = _transactionDbBlockTransactions $ _transactionDb db
    ttdb = _payloadCacheTransactionTrees $ _payloadCache db

