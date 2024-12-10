{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.Payload.PayloadStore
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Payload.PayloadStore
(
-- * Exceptions
  PayloadNotFoundException
, PayloadNotFoundException_(..)

-- * Transaction Database
, TransactionDb
, TransactionDb_(..)
, transactionDbBlockPayloadHeightsTbl
, oldTransactionDbBlockTransactionsTbl
, newTransactionDbBlockTransactionsTbl
, oldTransactionDbBlockPayloadsTbl
, newTransactionDbBlockPayloadsTbl

-- * Caches
, OutputTreeStore
, OutputTreeStore_(..)
, TransactionTreeStore
, TransactionTreeStore_(..)
, BlockOutputsStore
, BlockOutputsStore_(..)
, PayloadCache
, PayloadCache_(..)
, payloadCacheBlockOutputs
, payloadCacheOutputTrees
, payloadCacheTransactionTrees


-- * Payload Database
, PayloadDb
, PayloadDb_(..)
, CanPayloadCas
, CanReadablePayloadCas
, payloadCache
, transactionDb
, lookupPayloadWithHeight
, lookupPayloadWithHeightBatch
, lookupPayloadDataWithHeight
, lookupPayloadDataWithHeightBatch

-- ** Initialize Payload Database with Genesis Payloads
, initializePayloadDb

-- **  insert new payload
, addPayload
, addNewPayload
-- ** delete old payload
, deletePayload
) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.Hashable
import Data.Foldable

import GHC.Generics

-- internal modules

import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.Version

import Chainweb.Storage.Table
import Chainweb.BlockHeight

-- -------------------------------------------------------------------------- --
-- Exceptions

type PayloadNotFoundException = PayloadNotFoundException_ ChainwebMerkleHashAlgorithm

newtype PayloadNotFoundException_ a = PayloadNotFoundException (BlockPayloadHash_ a)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)

instance Exception PayloadNotFoundException

-- -------------------------------------------------------------------------- --
-- Transaction Database

type TransactionDb tbl = TransactionDb_ ChainwebMerkleHashAlgorithm tbl

-- | The authoritative CAS stores for a block chain.
--
data TransactionDb_ a tbl = TransactionDb
    { _transactionDbBlockPayloadHeightsTbl :: !(tbl (BlockPayloadHash_ a) BlockHeight)
        -- ^ Map of a hash of a block payload to the height of the block that
        -- contains the payload.

    , _newTransactionDbBlockPayloadsTbl :: !(tbl (BlockHeight, BlockPayloadHash_ a) (BlockPayload_ a))
        -- ^ While the content of this store can be computed from the block
        -- transactions, it is needed as an index into the block transaction
        -- store. If it would be lost one would have to recompute all of it in
        -- order to look up the data for a single transation.

    , _newTransactionDbBlockTransactionsTbl :: !(tbl (BlockHeight, BlockTransactionsHash_ a) (BlockTransactions_ a))
        -- ^ The block transactions of the block chain. This data is strictly
        -- needed to rebuild the payload data. Note: stored in a binary format.
        --
        -- While the content of this store can be computed from the block
        -- transactions, it is needed as an index into the block transaction
        -- store. If it would be lost one would have to recompute all of it in
        -- order to look up the data for a single transation.

    , _oldTransactionDbBlockPayloadsTbl :: !(Casify tbl (BlockPayload_ a))
        -- ^ Old table containing block payloads. This data is strictly needed
        -- to rebuild the payload data. Note: stored encoded as base64.
        --
        -- NOTE: DO NOT WRITE TO THIS TABLE. All writes should instead go to
        -- `_newTransactionDbBlockPayloadsTbl`. instead, which is indexed.

    , _oldTransactionDbBlockTransactionsTbl :: !(Casify tbl (BlockTransactions_ a))
        -- ^ The block transactions of the block chain. This data is strictly
        -- needed to rebuild the payload data. Note: stored encoded as base64.
        --
        -- NOTE: DO NOT WRITE TO THIS TABLE. All writes should instead go to
        -- `_newTransactionDbBlockTransactionsTbl`. instead, which is indexed.
    }

-- | Store of the 'BlockOutputs' for all blocks.
--
type BlockOutputsStore tbl = BlockOutputsStore_ ChainwebMerkleHashAlgorithm tbl
data BlockOutputsStore_ a tbl = BlockOutputsStore
    { _oldBlockOutputsTbl :: !(Casify tbl (BlockOutputs_ a))
    , _newBlockOutputsTbl :: !(tbl (BlockHeight, BlockOutputsHash_ a) (BlockOutputs_ a))
    }

-- | Store of the 'TransactionTree' Merkle trees for all blocks.
--
type TransactionTreeStore tbl = TransactionTreeStore_ ChainwebMerkleHashAlgorithm tbl
data TransactionTreeStore_ a tbl = TransactionTreeStore
    { _oldTransactionTreeStoreTbl :: !(Casify tbl (TransactionTree_ a))
    , _newTransactionTreeStoreTbl :: !(tbl (BlockHeight, BlockTransactionsHash_ a) (TransactionTree_ a))
    }

-- | Store of the 'OutputTree' Merkle trees for all blocks.
--
type OutputTreeStore tbl = OutputTreeStore_ ChainwebMerkleHashAlgorithm tbl
data OutputTreeStore_ a tbl = OutputTreeStore
    { _oldOutputTreeStoreTbl :: !(Casify tbl (OutputTree_ a))
    , _newOutputTreeStoreTbl :: !(tbl (BlockHeight, BlockOutputsHash_ a) (OutputTree_ a))
    }

-- | The CAS caches for a block chain.
--
-- If an entry is missing it can be rebuild from the PayloadDb
--
type PayloadCache tbl = PayloadCache_ ChainwebMerkleHashAlgorithm tbl
data PayloadCache_ a tbl = PayloadCache
    { _payloadCacheBlockOutputs :: !(BlockOutputsStore_ a tbl)
        -- ^ This is relatively expensive to rebuild.

    , _payloadCacheTransactionTrees :: !(TransactionTreeStore_ a tbl)
        -- ^ This are relatively cheap to rebuild.
        -- (tens of thousands of blocks per second)

    , _payloadCacheOutputTrees :: !(OutputTreeStore_ a tbl)
        -- ^ This are relatively cheap to rebuild.
        -- (tens of thousands blocks per second)
    }

type PayloadDb tbl = PayloadDb_ ChainwebMerkleHashAlgorithm tbl

data PayloadDb_ a tbl = PayloadDb
    { _transactionDb :: !(TransactionDb_ a tbl)
    , _payloadCache :: !(PayloadCache_ a tbl)
    }

type HeightedCas c t v = c (t (BlockHeight, CasKeyType v) v) (BlockHeight, CasKeyType v) v
type CanReadablePayloadCas tbl = CanReadablePayloadCas_ ChainwebMerkleHashAlgorithm tbl
type CanReadablePayloadCas_ a tbl =
    ( CanReadableCas tbl (BlockOutputs_ a)
    , HeightedCas ReadableTable tbl (BlockOutputs_ a)
    , CanReadableCas tbl (TransactionTree_ a)
    , HeightedCas ReadableTable tbl (TransactionTree_ a)
    , CanReadableCas tbl (OutputTree_ a)
    , HeightedCas ReadableTable tbl (OutputTree_ a)
    , CanReadableCas tbl (BlockTransactions_ a)
    , HeightedCas ReadableTable tbl (BlockTransactions_ a)
    , CanReadableCas tbl (BlockPayload_ a)
    , HeightedCas ReadableTable tbl (BlockPayload_ a)
    , ReadableTable (tbl (BlockPayloadHash_ a) BlockHeight) (BlockPayloadHash_ a) BlockHeight
    )

type CanPayloadCas tbl = CanPayloadCas_ ChainwebMerkleHashAlgorithm tbl
type CanPayloadCas_ a tbl =
    ( CanCas tbl (BlockOutputs_ a)
    , HeightedCas Table tbl (BlockOutputs_ a)
    , CanCas tbl (TransactionTree_ a)
    , HeightedCas Table tbl (TransactionTree_ a)
    , CanCas tbl (OutputTree_ a)
    , HeightedCas Table tbl (OutputTree_ a)
    , CanCas tbl (BlockTransactions_ a)
    , HeightedCas Table tbl (BlockTransactions_ a)
    , CanCas tbl (BlockPayload_ a)
    , HeightedCas Table tbl (BlockPayload_ a)
    , Table (tbl (BlockPayloadHash_ a) BlockHeight) (BlockPayloadHash_ a) BlockHeight
    )

-- -------------------------------------------------------------------------- --
-- Caches

type CanCas tbl a = Cas (tbl (CasKeyType a) a) a
type CanReadableCas tbl a = ReadableCas (tbl (CasKeyType a) a) a

type CanReadableTransactionDbCas_ a tbl =
    ( CanReadableCas tbl (BlockTransactions_ a)
    , ReadableTable (tbl (BlockHeight, BlockTransactionsHash_ a) (BlockTransactions_ a)) (BlockHeight, BlockTransactionsHash_ a) (BlockTransactions_ a)
    , CanReadableCas tbl (BlockPayload_ a)
    , ReadableTable (tbl (BlockHeight, BlockPayloadHash_ a) (BlockPayload_ a)) (BlockHeight, BlockPayloadHash_ a) (BlockPayload_ a)
    , ReadableTable (tbl (BlockPayloadHash_ a) BlockHeight) (BlockPayloadHash_ a) BlockHeight
    )

tableLookupMT :: ReadableTable t k v => t -> k -> MaybeT IO v
tableLookupMT t k = MaybeT (tableLookup t k)

-- non-exported; only used with a weaker set of constraints in tableLookup below
lookupPayloadDataWithHeight'
  :: CanReadableTransactionDbCas_ a tbl
  => TransactionDb_ a tbl
  -> Maybe BlockHeight
  -> BlockPayloadHash_ a
  -> IO (Maybe (PayloadData_ a))
lookupPayloadDataWithHeight' tdb height k = runMaybeT $ do
    let
      lookupOldPayload    = tableLookupMT (_oldTransactionDbBlockPayloadsTbl tdb) k
      lookupNewPayload h  = tableLookupMT (_newTransactionDbBlockPayloadsTbl tdb) (h, k)

    pd <- (lookupNewPayload =<< hoistMaybe height) <|> lookupOldPayload

    let txsHash = _blockPayloadTransactionsHash pd
    let
        lookupOldTxn = tableLookupMT (_oldTransactionDbBlockTransactionsTbl tdb) txsHash
        lookupNewTxn h = tableLookupMT (_newTransactionDbBlockTransactionsTbl tdb) (h, txsHash)
    txs <- (lookupNewTxn =<< hoistMaybe height) <|> lookupOldTxn
    return $ payloadData txs pd

lookupHeight
    :: ReadableTable (tbl (BlockPayloadHash_ a) BlockHeight) (BlockPayloadHash_ a) BlockHeight
    => TransactionDb_ a tbl -> BlockPayloadHash_ a -> MaybeT IO BlockHeight
lookupHeight tdb k = tableLookupMT (_transactionDbBlockPayloadHeightsTbl tdb) k

lookupPayloadDataWithHeight
  :: CanReadablePayloadCas_ a tbl
  => PayloadDb_ a tbl
  -> Maybe BlockHeight
  -> BlockPayloadHash_ a
  -> IO (Maybe (PayloadData_ a))
lookupPayloadDataWithHeight db mh k = do
    mh' <- runMaybeT $
        hoistMaybe mh <|> lookupHeight (_transactionDb db) k
    lookupPayloadDataWithHeight' (_transactionDb db) mh' k

lookupPayloadDataWithHeightBatch
  :: CanReadablePayloadCas_ a tbl
  => PayloadDb_ a tbl
  -> [(Maybe BlockHeight, BlockPayloadHash_ a)]
  -> IO [Maybe (PayloadData_ a)]
lookupPayloadDataWithHeightBatch db = traverse (uncurry $ lookupPayloadDataWithHeight db)

lookupPayloadWithHeight
  :: CanReadablePayloadCas_ a tbl
  => PayloadDb_ a tbl
  -> Maybe BlockHeight
  -> BlockPayloadHash_ a
  -> IO (Maybe (PayloadWithOutputs_ a))
lookupPayloadWithHeight db mh k = runMaybeT $ do
    mh' <- liftIO $ runMaybeT $
        hoistMaybe mh <|> lookupHeight (_transactionDb db) k
    pd <- MaybeT (lookupPayloadDataWithHeight db mh' k)
    let outsHash = view payloadDataOutputsHash pd
    let lookupNew h = tableLookupMT (_newBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache db) (h, outsHash)
    let lookupOld = tableLookupMT (_oldBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache db) outsHash

    outs <- (hoistMaybe mh' >>= lookupNew) <|> lookupOld
    liftIO . evaluate $
        payloadWithOutputs pd (_blockCoinbaseOutput outs) (_blockOutputs outs)

lookupPayloadWithHeightBatch
  :: CanReadablePayloadCas_ a tbl
  => PayloadDb_ a tbl
  -> [(Maybe BlockHeight, BlockPayloadHash_ a)]
  -> IO [Maybe (PayloadWithOutputs_ a)]
lookupPayloadWithHeightBatch db = traverse (uncurry $ lookupPayloadWithHeight db)

instance (pk ~ CasKeyType (PayloadData_ a), CanReadableTransactionDbCas_ a tbl) => ReadableTable (TransactionDb_ a tbl) pk (PayloadData_ a) where
    tableLookup db = lookupPayloadDataWithHeight' db Nothing

-- -------------------------------------------------------------------------- --
-- Initialize a PayloadDb with Genesis Payloads

-- | Initialize a PayloadDb with genesis payloads for the given chainweb
-- version.
--
initializePayloadDb
    :: CanPayloadCas tbl
    => ChainwebVersion
    -> PayloadDb tbl
    -> IO ()
initializePayloadDb v db = traverse_ initForChain $ chainIds v
  where
    initForChain cid =
        addNewPayload db (genesisBlockHeight v cid) $ v ^?! versionGenesis . genesisBlockPayload . atChain cid

-- -------------------------------------------------------------------------- --
-- Insert new Payload

-- | Insert block payload data in to the database.
--
addPayload
    :: MerkleHashAlgorithm a
    => CanPayloadCas_ a tbl
    => PayloadDb_ a tbl
    -> BlockHeight
    -> BlockTransactions_ a
    -> TransactionTree_ a
    -> BlockOutputs_ a
    -> OutputTree_ a
    -> IO ()
addPayload db height txs txTree outs outTree = do
    tableInsert (_newTransactionDbBlockPayloadsTbl $ _transactionDb db) (height, casKey payload) payload
    tableInsert (_transactionDbBlockPayloadHeightsTbl $ _transactionDb db) (casKey payload) height
    tableInsert (_newTransactionDbBlockTransactionsTbl $ _transactionDb db) (height, casKey txs) txs
    tableInsert (_newBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache db) (height, casKey outs) outs
    tableInsert (_newTransactionTreeStoreTbl $ _payloadCacheTransactionTrees $ _payloadCache db) (height, casKey txTree) txTree
    tableInsert (_newOutputTreeStoreTbl $ _payloadCacheOutputTrees $ _payloadCache db) (height, casKey outTree) outTree
  where
    payload = blockPayload txs outs

-- | Create block payload data from a sequence of transaction and insert it into
-- the database.
--
addNewPayload
    :: MerkleHashAlgorithm a
    => CanPayloadCas_ a tbl
    => PayloadDb_ a tbl
    -> BlockHeight
    -> PayloadWithOutputs_ a
    -> IO ()
addNewPayload db height s = addPayload db height txs txTree outs outTree
  where
    (bts, bos) = payloadWithOutputsToBlockObjects s
    (txTree, txs) = newBlockTransactions (_blockMinerData bts) (_blockTransactions bts)
    (outTree, outs) = newBlockOutputs (_blockCoinbaseOutput bos) (_blockOutputs bos)

deletePayload
    :: CanPayloadCas_ a tbl
    => PayloadDb_ a tbl
    -> BlockPayload_ a
    -> IO ()
deletePayload db p = do
    let
      tdb = _transactionDb db
      pdb = _newTransactionDbBlockPayloadsTbl $ _transactionDb db
    height <- tableLookup (_transactionDbBlockPayloadHeightsTbl tdb) (casKey p)
    case height of
        Nothing -> tableDelete (_oldTransactionDbBlockPayloadsTbl tdb) (_blockPayloadPayloadHash p)
        Just h  -> do
            tableDelete (_transactionDbBlockPayloadHeightsTbl tdb) (casKey p)
            tableDelete pdb (h, _blockPayloadPayloadHash p)

-- lens exports

-- XXX TODO (aseipp): delete these, since they're probably useless and have no callers
makeLenses ''PayloadDb_
makeLenses ''PayloadCache_
makeLenses ''TransactionDb_
