{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- There are four modes for pruning the database:
--
-- * `none`: pruning is skipped
-- * `headers`: only block headers of stale forks below are certain depth are
--   pruned. The payload db is left unchanged.
-- * `headers-checked`: like `headers` but block headers are also validated and
--   the existence and consistency of the respective paylaods is checked.
-- * `full`: like `headers` but additionally garbage collection is performed
--   on payload database.
--
-- The `headers-checked` mode does a verification of the complete Merkle tree of
-- the chain.
--
-- There is a block header database for each chain, but only a single shared
-- payload database. Payloads or parts thereof can be shared between blocks
-- on the same chain and on different chains. Therefore garbage collection is
-- implemented as mark and sweep, using a probabilistic set representation
-- for marking.
--
-- This is an offline implementation. There must be no concurrent writes during
-- database pruning. Although, if needed it could (relatively easily) be
-- extended for online garbage collection.
--
-- /Implementation:/
--
-- The algorithm is probabilistic. Some small percentage of unreachable data
-- remains in the database. The datastructure for marking used entries is uses a
-- random seed, such that repeated pruning will delete an increasing amount of
-- items.
--
-- For marking Cuckoo filters are used. Cuckoo filters have the disadvantage
-- that they don't support concurrent insertion. Also merging cuckoo filters is
-- slow. We use seperate filters for each chain and query them individually. We
-- may explore other techniques to represent set membership in the future.
--
-- The algorithm works top down on the hiearchical structure of the data. It
-- guarantees that the database is consistent even if an exception occurs during
-- a sweep phase. In that case some extra garbage would remain in the database,
-- but there will be no dangling references.
--
-- /NOTE: READ BEFORE MAKING A CHANGE TO THE ALGORITHM:/
--
-- For the algorithm to maintain database "deep" consistency (no dangling
-- references) it is madatory that GC proceedes in stages. For instance, it is
-- tempting to merge all marking phases into a single traversal of the
-- BlockPaylaod store. However, due to the probabilistic nature of marking, some
-- BlockPayloads are marked that are not reachable from a block header and will
-- thus remain in the database. Hence, marking of the lower-level BlockOutputs
-- and BlockTransactions must be based on what is /actually/ kept in the store
-- and not just on what is reachable from a block header.
--
-- /TODO:/
--
-- * implement incremental pruning (which can't be used with mark and sweep,
--   though)
--
-- * implement online garbage collection
--
-- * Consider changing the database format to store the transactions with the
--   block payloads and eliminate sharing. Sharing is benefitial only when most
--   blocks are empty and the there's only a small number of pools. However, in
--   that case storage requirements are moderate anyways. When most block
--   contain transactions the benefits of sharing become marginal, but the
--   saving during GC (in particular incremental GC) become larger.
--
module Chainweb.Chainweb.PruneChainDatabase
( PruningChecks(..)
, pruneAllChains
, fullGc
, DatabaseCheckException(..)
) where

import Chainweb.BlockHeader

import Control.Concurrent.Async
import Control.Lens (view)
import Control.Monad
import Control.Monad.Catch

import Data.Aeson hiding (Error)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Cuckoo
import Data.Foldable

import GHC.Generics

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.LogLevel
import System.Random

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.PruneForks
import Chainweb.BlockHeight
import Chainweb.ChainValue
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Fork Pruning

-- | Different consistency checks that can be piggybacked onto database pruning.
--
-- The following runtimes were measured on a mac book pro at a block height of
-- about 820,000:
--
-- * `[]`: 10s
-- * `[CheckInductive]`: 2min
-- * `[CheckIntrinsic, CheckPayloadsExist]`: 4min
-- * `[CheckFull, CheckPayloads]`: 8min
-- * `[CheckFull, CheckPayloadsExist]`: 8min
--
data PruningChecks
    = CheckIntrinsic
        -- ^ Performs intrinsic validation on all block headers.
    | CheckInductive
        -- ^ Performs all intrinsic and inductive block header validations.
    | CheckFull
        -- ^ Performs full block header validation. This includes intrinsic,
        -- inductive, and braiding validation.
    | CheckPayloads
        -- ^ checks that all payload components exist in the payload and can be
        -- decoded and the Merkle Trees are consitent.
    | CheckPayloadsExist
        -- ^ only checks the existence of the payload hash in the
        -- BlockPayloadStore, which is faster than fully checking payloads.
    deriving (Show, Eq, Ord, Enum, Bounded)

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
    -> [PruningChecks]
    -> IO ()
pruneAllChains logger rdb v checks = do
    now <- getCurrentTimeIntegral
    wdb <- initWebBlockHeaderDb rdb v
    forConcurrently_ (toList $ chainIds v) (pruneChain now wdb)
  where
    diam = diameter $ chainGraphAt v (maxBound @BlockHeight)
    pruneChain now wdb cid = do
        let chainLogger = addLabel ("chain", toText cid) logger
            chainLogg = logFunctionText chainLogger
        cdb <- getWebBlockHeaderDb wdb cid
        chainLogg Info "start pruning block header database"
        x <- pruneForksLogg chainLogger cdb (1 + diam * 3) (callback now wdb cdb)
        chainLogg Info $ "finished pruning block header database. Deleted " <> sshow x <> " block headers."

    pdb = newPayloadDb rdb

    callback now wdb cdb d h = mapM_ (\c -> run c d h) checks
      where
        run CheckIntrinsic = checkIntrinsic now
        run CheckInductive = checkInductive now cdb
        run CheckPayloadsExist = checkPayloadsExist pdb
        run CheckPayloads = checkPayloads pdb
        run CheckFull = checkFull now wdb

-- -------------------------------------------------------------------------- --
-- Fork Pruning callbacks

data DatabaseCheckException
    = MissingPayloadException BlockHeader
    | InconsistentPaylaod BlockHeader PayloadWithOutputs
    deriving (Show, Generic)

instance Exception DatabaseCheckException

-- | Verify existance and consistency of payloads for all block headers that are
-- not deleted.
--
-- Adds about 4 minutes of overhead at block height 800,000 on a mac bock pro.
--
checkPayloads :: PayloadDb RocksDbTable -> Bool -> BlockHeader -> IO ()
checkPayloads _ True _ = return ()
checkPayloads pdb False h = lookupPayloadWithHeight pdb (Just $ view blockHeight h) (view blockPayloadHash h) >>= \case
    Just p
        | verifyPayloadWithOutputs p -> return ()
        | otherwise -> throwM $ InconsistentPaylaod h p
    Nothing -> throwM $ MissingPayloadException h
{-# INLINE checkPayloads #-}

-- | Just check the existence of the Payload in the Database but don't check
-- that it can actually be decoded and is consistent.
--
-- This is faster than 'checkPayload' because only the top-level 'PayloadData'
-- structure is queried -- and immediately discarded.
--
checkPayloadsExist :: PayloadDb RocksDbTable -> Bool -> BlockHeader -> IO ()
checkPayloadsExist _ True _ = return ()
checkPayloadsExist pdb False h = do
    lookupPayloadDataWithHeight pdb (Just $ view blockHeight h) (view blockPayloadHash h) >>= \case
        Just _ -> return ()
        Nothing -> throwM $ MissingPayloadException h
{-# INLINE checkPayloadsExist #-}

-- | Intrinsically validate all block headers that are not deleted.
--
-- Adds less than 1 minute of overhead at block height 800,000 on a mac bock pro.
--
checkIntrinsic :: Time Micros -> Bool -> BlockHeader -> IO ()
checkIntrinsic _ True _ = return ()
checkIntrinsic now False h = validateIntrinsicM now h
{-# INLINE checkIntrinsic #-}

-- | Intrinsically validate all block headers that are not deleted.
--
checkInductive :: Time Micros -> BlockHeaderDb -> Bool -> BlockHeader -> IO ()
checkInductive _ _ True _ = return ()
checkInductive now cdb False h = do
    validateInductiveChainM (tableLookup cdb) h
    validateIntrinsicM now h
{-# INLINE checkInductive #-}

-- | Perform complete block header validation for all block that are not deleted.
--
-- Adds about 5 minutes of overhead at block height 800,000 on a mac book pro
--
checkFull :: Time Micros -> WebBlockHeaderDb -> Bool -> BlockHeader -> IO ()
checkFull _ _ True = const $ return ()
checkFull now wdb False = void . validateBlockHeaderM now ctx
  where
    ctx :: ChainValue BlockHash -> IO (Maybe BlockHeader)
    ctx cv = fmap _chainValueValue <$> tableLookup wdb cv
{-# INLINE checkFull #-}

-- -------------------------------------------------------------------------- --
-- Full GC

-- | Prune all chains and clean up payloads.
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

        -- Mark all entries down to the requested depth, so they don't get GCed
        --
        -- Blocks at (maxRank - depth) are used as pivots and pruning and marking starts at
        -- (depth - 1)
        --
        let keptBound = MinRank $ int $ m - min m depth
        void $ entries cdb Nothing Nothing (Just keptBound) Nothing
            $ S.mapM_ (markPayload markedPayloads)

        chainLogg Info "start pruning block header database"
        x <- pruneForksLogg chainLogger cdb depth $ \isDeleted hdr -> case isDeleted of
            True -> chainLogg Debug
                $ "pruned header " <> toText (view blockHash hdr)
                <> " at height " <> sshow (view blockHeight hdr)
            False -> markPayload markedPayloads hdr

        chainLogg Info $ "finished pruning block header database. Deleted " <> sshow x <> " block headers."
        return markedPayloads

-- -------------------------------------------------------------------------- --
-- Payload Mark and Sweep

-- | Mark Payloads of non-deleted block headers.
--
markPayload :: Filter BlockPayloadHash -> BlockHeader -> IO ()
markPayload f = tryInsert f "payload hash" . view blockPayloadHash
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
    -> PayloadDb RocksDbTable
    -> [Filter BlockPayloadHash]
    -> IO (Filter BlockTransactionsHash, Filter BlockOutputsHash)
sweepPayloads logg db markedPayloads = do
    logg Info "Sweeping BlockPayloads"

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

    -- traverse all payloads, both old and new tables
    c0 <- withTableIterator newPayloadsTable
        $ S.sum_ @_ @Int . S.mapM (go markedTrans markedOutputs) . iterToValueStream
    logg Info $ "Swept entries for " <> sshow c0 <> " NEW block payload hashes"

    c1 <- withTableIterator oldPayloadsTable
        $ S.sum_ @_ @Int . S.mapM (go markedTrans markedOutputs) . iterToValueStream
    logg Info $ "Swept entries for " <> sshow c1 <> " OLD block payload hashes"

    return (markedTrans, markedOutputs)
  where
    go mt mo x = checkMark markedPayloads (_blockPayloadPayloadHash x) >>= \case
        True -> 0 <$ markTransactions mt x <* markOutputs mo x
        False -> 1 <$ deleteBlockPayload logg db x

    -- Extract RocksDB Tables from Payload Db
    newPayloadsTable :: RocksDbTable (BlockHeight, BlockPayloadHash) BlockPayload
    newPayloadsTable = _newTransactionDbBlockPayloadsTbl $ _transactionDb db

    oldPayloadsTable :: RocksDbTable BlockPayloadHash BlockPayload
    oldPayloadsTable = unCasify (_oldTransactionDbBlockPayloadsTbl $ _transactionDb db)

-- | Sweep Transations
--
sweepTransactions
    :: LogFunctionText
    -> PayloadDb RocksDbTable
    -> Filter BlockTransactionsHash
    -> IO ()
sweepTransactions logg db marked = do
    logg Info "Sweeping BlockTransactions"
    c1 <- withTableIterator (_newTransactionDbBlockTransactionsTbl $ _transactionDb db) $
        S.sum_ @_ @Int . S.mapM (\(bh, hsh) -> go (Just bh, hsh)) . iterToKeyStream
    logg Info $ "Swept " <> sshow c1 <> " block transactions hashes (new table)"
    c2 <- withTableIterator (_oldTransactionDbBlockTransactionsTbl $ _transactionDb db) $
        S.sum_ @_ @Int . S.mapM (\hsh -> go (Nothing, hsh)) . iterToKeyStream
    logg Info $ "Swept " <> sshow c2 <> " block transactions hashes (old table)"
  where
    go (h, x) = member marked (GcHash x) >>= \case
        True -> return 0
        False -> 1 <$ deleteBlockTransactions logg db h x

-- | Sweep Outputs
--
sweepOutputs
    :: LogFunctionText
    -> PayloadDb RocksDbTable
    -> Filter BlockOutputsHash
    -> IO ()
sweepOutputs logg db marked = do
    logg Info "Sweeping BlockOutputss"
    c1 <- withTableIterator (_newBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache db) $
        S.sum_ @_ @Int . S.mapM (\(bh, hsh) -> go (Just bh, hsh)) . iterToKeyStream
    logg Info $ "Swept " <> sshow c1 <> " block output hashes (new table)"
    c2 <- withTableIterator (_oldBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache db) $
        S.sum_ @_ @Int . S.mapM (curry go Nothing) . iterToKeyStream
    logg Info $ "Swept " <> sshow c2 <> " block output hashes (new table)"
  where
    go (h, x) = member marked (GcHash x) >>= \case
        True -> return 0
        False -> 1 <$ deleteBlockOutputs logg db h x

-- -------------------------------------------------------------------------- --
-- Utils for Mark and sweep GC for Payloads
--

-- | Wraps a MerkleLogHash for usage with a Cuckoo filter.
--
newtype GcHash a = GcHash a
    deriving newtype (Show, ToJSON)

instance BA.ByteArrayAccess a => CuckooFilterHash (GcHash a) where
    cuckooHash (Salt s) (GcHash a) =
        saltedFnv1aByteString s (B.take 8 $ BA.convert a)
    cuckooFingerprint (Salt s) (GcHash a) =
        saltedSipHashByteString s (B.take 8 $ BA.convert a)
    {-# INLINE cuckooHash #-}
    {-# INLINE cuckooFingerprint #-}

type Filter a = CuckooFilterIO 4 10 (GcHash a)

mkFilter :: Natural -> IO (Filter a)
mkFilter n = do
    s <- randomIO
    newCuckooFilter (Salt s) $ max 128 n

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
    :: CanPayloadCas tbl
    => LogFunctionText
    -> PayloadDb tbl
    -> BlockPayload
    -> IO ()
deleteBlockPayload logg db p = do
    logg Debug $ "Delete PayloadHash for " <> encodeToText (_blockPayloadPayloadHash p)
    deletePayload db p

-- | Delete BlockOutputs and OutputTree
--
deleteBlockOutputs
    :: CanPayloadCas tbl
    => LogFunctionText
    -> PayloadDb tbl
    -> Maybe BlockHeight
    -> BlockOutputsHash
    -> IO ()
deleteBlockOutputs logg db mh p = do
    logg Debug $ "Delete BlockOutputs for " <> encodeToText p
    case mh of
        Just h -> do
            flip tableDelete (h, p) $
                _newOutputTreeStoreTbl $ _payloadCacheOutputTrees $ _payloadCache db
            flip tableDelete (h, p) $
                _newBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache db
        Nothing -> do
            flip tableDelete p $
                _oldOutputTreeStoreTbl $ _payloadCacheOutputTrees $ _payloadCache db
            flip tableDelete p $
                _oldBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache db

-- | Delete BlockTransactions and TransactionsTree
--
deleteBlockTransactions
    :: CanPayloadCas tbl
    => LogFunctionText
    -> PayloadDb tbl
    -> Maybe BlockHeight
    -> BlockTransactionsHash
    -> IO ()
deleteBlockTransactions logg db mh p = do
    logg Debug $ "Delete BlockTransactions for " <> encodeToText p
    case mh of
        Just h -> do
            flip tableDelete (h, p) $
                _newTransactionDbBlockTransactionsTbl $ _transactionDb db
            flip tableDelete (h, p) $
                _newTransactionTreeStoreTbl $ _payloadCacheTransactionTrees $ _payloadCache db
        Nothing -> do
            flip tableDelete p $
                _oldTransactionDbBlockTransactionsTbl $ _transactionDb db
            flip tableDelete p $
                _oldTransactionTreeStoreTbl $ _payloadCacheTransactionTrees $ _payloadCache db
