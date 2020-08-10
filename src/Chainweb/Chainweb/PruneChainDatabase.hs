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

-- -------------------------------------------------------------------------- --
-- Fork Pruning

-- | Prune all chains. This doesn't clean up Payloads.
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
    diam = diameter $ chainGraphAt v (maxBound @BlockHeight)
    pruneChain cid = withBlockHeaderDb rdb v cid $ \cdb -> do
        let chainLogger = addLabel ("chain", toText cid) logger
            chainLogg = logFunctionText chainLogger
        chainLogg Info "start pruning block header database"
        x <- pruneForksLogg chainLogger cdb (diam * 3) (\_ _ -> return ())
        chainLogg Info $ "finished pruning block header database. Deleted " <> sshow x <> " block headers."

-- -------------------------------------------------------------------------- --
-- Full GC

-- | Prune all chains and clean up payloads.
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
    markedPayloads <- forConcurrently (toList $ chainIds v) pruneChain
    logg Info $ "Finished pruning block headers"

    -- TODO: parallelize he the sweep payload phase to use all available cores.

    -- Sweep Payloads and mark transactions
    (markedTrans, markedOutputs) <- sweepPayloads logg db markedPayloads

    -- Sweep transactions
    -- (these are reasonably fast, prossibly because they only iterate over the keys)
    concurrently_
        (sweepTransactions logg db markedTrans)
        (sweepOutputs logg db markedOutputs)

    logg Info $ "Finished chain database garbage collection"

  where
    logg = logFunctionText logger
    diam = diameter $ chainGraphAt v (maxBound @BlockHeight)
    depth = diam * 3

    db = newPayloadDb rdb


    -- Prune a single chain and return the sets of marked payloads
    --
    pruneChain cid = withBlockHeaderDb rdb v cid $ \cdb -> do
        let chainLogger = addLabel ("chain", toText cid) logger
            chainLogg = logFunctionText chainLogger

        m <- maxRank cdb
        markedPayloads <- mkFilter (round $ (1024 + int @_ @Double m) * 1.1)

        chainLogg Info $ "Allocated "
            <> sshow (sizeInAllocatedBytes markedPayloads `div` (1024 * 1024))
            <> "MB for marking database entries"

        -- Mark all entries above depth, so they don't get GCed
        --
        void $ entries cdb Nothing Nothing (Just $ int $ depth + 1) Nothing
            $ S.mapM_ (markPayload markedPayloads)

        chainLogg Info "start pruning block header database"
        x <- pruneForksLogg chainLogger cdb depth $ \isDeleted hdr -> case isDeleted of
            True -> chainLogg Debug
                $ "pruned header " <> toText (_blockHash hdr)
                <> " at height " <> sshow (_blockHeight hdr)
            False -> markPayload markedPayloads hdr

        chainLogg Info $ "finished pruning block header database. Deleted " <> sshow x <> " block headers."
        return markedPayloads

-- -------------------------------------------------------------------------- --
-- Payload Mark and Sweep

-- | Mark Payloads of non-deleted block headers.
--
markPayload :: Filter BlockPayloadHash -> BlockHeader -> IO ()
markPayload f = tryInsert f "payload hash" . _blockPayloadHash
{-# INLINE markPayload #-}

-- | Mark Payload Transactions
--
markTransactions :: Filter BlockTransactionsHash -> BlockPayload -> IO ()
markTransactions f
    = tryInsert f "transactions hash" . _blockPayloadTransactionsHash
{-# INLINE markTransactions #-}

-- | Mark Payload Outputs
--
markOutputs :: Filter BlockOutputsHash -> BlockPayload -> IO ()
markOutputs f
    = tryInsert f "outputs hash" . _blockPayloadOutputsHash
{-# INLINE markOutputs #-}

-- | Sweep payload and mark all transactions that are kept
--
sweepPayloads
    :: LogFunctionText
    -> PayloadDb RocksDbCas
    -> [Filter BlockPayloadHash]
    -> IO (Filter BlockTransactionsHash, Filter BlockOutputsHash)
sweepPayloads logg db markedPayloads = do
    logg Info $ "Sweeping BlockPayloads"

    -- create filter with sufficient capacity
    m <- sum <$> mapM itemCount markedPayloads
    markedTrans <- mkFilter (round $ int @_ @Double m * 1.1)

    logg Info $ "Allocated "
        <> sshow (sizeInAllocatedBytes markedTrans `div` (1024 * 1024))
        <> "MB for marking transaction hashes entries"

    markedOutputs <- mkFilter (round $ int @_ @Double m * 1.1)
    logg Info $ "Allocated "
        <> sshow (sizeInAllocatedBytes markedOutputs `div` (1024 * 1024))
        <> "MB for marking outputs hashes entries"

    -- traverse all payloads
    c0 <- withTableIter payloadsTable
        $ S.sum_ @_ @Int . S.mapM (go markedTrans markedOutputs) . iterToValueStream
    logg Info $ "Swept entries for " <> sshow c0 <> " block payload hashes"
    return (markedTrans, markedOutputs)
  where
    go mt mo x = checkMark markedPayloads (_blockPayloadPayloadHash x) >>= \case
        True -> 0 <$ markTransactions mt x <* markOutputs mo x
        False -> 1 <$ deleteBlockPayload logg db x

    -- Extract RocksDB Tables from Payload Db
    payloadsTable :: RocksDbTable BlockPayloadHash BlockPayload
    payloadsTable = _getRocksDbCas t
      where
        BlockPayloadStore t = _transactionDbBlockPayloads $ _transactionDb db

-- | Sweep Transations
--
sweepTransactions
    :: LogFunctionText
    -> PayloadDb RocksDbCas
    -> Filter BlockTransactionsHash
    -> IO ()
sweepTransactions logg db marked = do
    logg Info $ "Sweeping BlockTransactions"
    c1 <- withTableIter table $ S.sum_ @_ @Int . S.mapM go . iterToKeyStream
    logg Info $ "Swept " <> sshow c1 <> " block transactions hashes"
  where
    go x = member marked (GcHash x) >>= \case
        True -> return 0
        False -> 1 <$ deleteBlockTransactions logg db x

    -- Extract RocksDB Tables from Payload Db
    table :: RocksDbTable BlockTransactionsHash BlockTransactions
    table = _getRocksDbCas t
      where
        BlockTransactionsStore t = _transactionDbBlockTransactions $ _transactionDb db

-- | Sweep Outputs
--
sweepOutputs
    :: LogFunctionText
    -> PayloadDb RocksDbCas
    -> Filter BlockOutputsHash
    -> IO ()
sweepOutputs logg db marked = do
    logg Info $ "Sweeping BlockOutputss"
    c1 <- withTableIter table $ S.sum_ @_ @Int . S.mapM go . iterToKeyStream
    logg Info $ "Swept " <> sshow c1 <> " block output hashes"
  where
    go x = member marked (GcHash x) >>= \case
        True -> return 0
        False -> 1 <$ deleteBlockOutputs logg db x

    -- Extract RocksDB Tables from Payload Db
    table :: RocksDbTable BlockOutputsHash BlockOutputs
    table = _getRocksDbCas t
      where
        BlockOutputsStore t = _payloadCacheBlockOutputs $ _payloadCache db

-- -------------------------------------------------------------------------- --
-- Utils for Mark and sweep GC for Payloads
--

-- | Wraps a MerkleLogHash for usage with a Cuckoo filter.
--
newtype GcHash a = GcHash a
    deriving newtype (Show, ToJSON)

instance BA.ByteArrayAccess a => CuckooFilterHash (GcHash a) where
    cuckooHash (Salt s) (GcHash a) = fnv1a_bytes s $ BA.takeView a 8
    cuckooFingerprint (Salt s) (GcHash a) = sip_bytes s $ BA.takeView a 8
    {-# INLINE cuckooHash #-}
    {-# INLINE cuckooFingerprint #-}

type Filter a = CuckooFilterIO 4 10 (GcHash a)

mkFilter :: Natural -> IO (Filter a)
mkFilter n = do
    s <- randomIO
    newCuckooFilter (Salt s) n

-- | inserting a somewhat larger number (I think, it's actually 7) of
-- equal elements causes the filter to fail.
--
tryInsert :: BA.ByteArrayAccess a => Filter a -> [Char] -> a -> IO ()
tryInsert cf k a = unlessM (member cf $ GcHash a) $
    unlessM (insert cf $ GcHash a) $ error
        $ "failed to insert item " <> k <> " in cuckoo filter"
        <> ": while very rare this can happen. Usually it is resolve by retrying."
{-# INLINE tryInsert #-}

-- TODO: consider using bloom fiters instead that can be merged. Alternatively,
-- implement concurrent insertion for cuckoo filters, where the hashing is done
-- concurrently and a lock is used only for the actual modification of the
-- underlying buffer. Or do fine grained locking on the filter.
--
checkMark :: BA.ByteArrayAccess a => [Filter a] -> a -> IO Bool
checkMark fs a = go fs
  where
    go [] = return False
    go (h : t) = member h (GcHash a) >>= \case
        True -> return True
        False -> go t
    {-# INLINE go #-}

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

-- | delete BlockPayload
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
    casDelete pdb (_blockPayloadPayloadHash p)
  where
    pdb = _transactionDbBlockPayloads $ _transactionDb db

-- | Delete BlockOutputs and OutputTree
--
deleteBlockOutputs
    :: CasConstraint cas BlockOutputs
    => CasConstraint cas OutputTree
    => LogFunctionText
    -> PayloadDb cas
    -> BlockOutputsHash
    -> IO ()
deleteBlockOutputs logg db p = do
    logg Debug $ "Delete BlockOutputs for " <> encodeToText p
    casDelete odb p
    casDelete otdb p
  where
    odb = _payloadCacheBlockOutputs $ _payloadCache db
    otdb = _payloadCacheOutputTrees $ _payloadCache db

-- | Delete BlockTransactions and TransactionsTree
--
deleteBlockTransactions
    :: CasConstraint cas BlockTransactions
    => CasConstraint cas TransactionTree
    => LogFunctionText
    -> PayloadDb cas
    -> BlockTransactionsHash
    -> IO ()
deleteBlockTransactions logg db p = do
    logg Debug $ "Delete BlockTransactions for " <> encodeToText p
    casDelete tdb p
    casDelete ttdb p
  where
    tdb = _transactionDbBlockTransactions $ _transactionDb db
    ttdb = _payloadCacheTransactionTrees $ _payloadCache db
