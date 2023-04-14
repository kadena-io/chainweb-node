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
, transactionDbBlockTransactions
, transactionDbBlockPayloads

-- * Caches
, OutputTreeStore
, OutputTreeStore_
, TransactionTreeStore
, TransactionTreeStore_
, BlockOutputsStore
, BlockOutputsStore_
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

-- ** Initialize Payload Database with Genesis Payloads
, initializePayloadDb

-- **  insert new payload
, addPayload
, addNewPayload
) where

import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad.Trans.Maybe

import Data.Hashable
import Data.Foldable
import qualified Data.Vector as V

import GHC.Generics


-- internal modules

import Chainweb.BlockHeader.Genesis (genesisBlockPayload)
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.Version

import Chainweb.Storage.Table

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
    { _transactionDbBlockTransactions :: !(Casify tbl (BlockTransactions_ a))
        -- ^ The block transactions of the block chain. This data is strictly
        -- needed to rebuild the payload data.

    , _transactionDbBlockPayloads :: !(Casify tbl (BlockPayload_ a))
        -- ^ While the content of this store can be computed from the block
        -- transactions, it is needed as an index into the block transaction
        -- store. If it would be lost one would have to recompute all of it in
        -- order to look up the data for a single transation.
    }

makeLenses ''TransactionDb_

type CanCas tbl a = Cas (tbl (CasKeyType a) a) a
type CanReadableCas tbl a = ReadableCas (tbl (CasKeyType a) a) a

type CanReadableTransactionDbCas_ a tbl =
    ( CanReadableCas tbl (BlockTransactions_ a)
    , CanReadableCas tbl (BlockPayload_ a)
    )

instance (pk ~ CasKeyType (PayloadData_ a), CanReadableTransactionDbCas_ a tbl) => ReadableTable (TransactionDb_ a tbl) pk (PayloadData_ a) where
    tableLookup db k = runMaybeT $ do
        pd <- MaybeT $ tableLookup (_transactionDbBlockPayloads db) k 
        let txsHash = _blockPayloadTransactionsHash pd
        let outsHash = _blockPayloadOutputsHash pd
        txs <- MaybeT $ tableLookup (_transactionDbBlockTransactions db) txsHash 
        return $ PayloadData
            { _payloadDataTransactions = _blockTransactions txs
            , _payloadDataMiner = _blockMinerData txs
            , _payloadDataPayloadHash = k
            , _payloadDataTransactionsHash = txsHash
            , _payloadDataOutputsHash = outsHash
            }
    {-# INLINE tableLookup #-}

-- -------------------------------------------------------------------------- --
-- Caches

-- | Store of the 'BlockOutputs' for all blocks.
--
type BlockOutputsStore tbl = BlockOutputsStore_ ChainwebMerkleHashAlgorithm tbl
type BlockOutputsStore_ a tbl = Casify tbl (BlockOutputs_ a)

-- | Store of the 'TransactionTree' Merkle trees for all blocks.
--
type TransactionTreeStore tbl = TransactionTreeStore_ ChainwebMerkleHashAlgorithm tbl
type TransactionTreeStore_ a tbl = Casify tbl (TransactionTree_ a)

-- | Store of the 'OutputTree' Merkle trees for all blocks.
--
type OutputTreeStore tbl = OutputTreeStore_ ChainwebMerkleHashAlgorithm tbl
type OutputTreeStore_ a tbl = Casify tbl (OutputTree_ a)

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

makeLenses ''PayloadCache_

-- -------------------------------------------------------------------------- --
-- Payload Database

type PayloadDb tbl = PayloadDb_ ChainwebMerkleHashAlgorithm tbl

data PayloadDb_ a tbl = PayloadDb
    { _transactionDb :: !(TransactionDb_ a tbl)
    , _payloadCache :: !(PayloadCache_ a tbl)
    }

makeLenses ''PayloadDb_

type CanReadablePayloadCas tbl = CanReadablePayloadCas_ ChainwebMerkleHashAlgorithm tbl
type CanReadablePayloadCas_ a tbl =
    ( CanReadableCas tbl (BlockOutputs_ a)
    , CanReadableCas tbl (TransactionTree_ a)
    , CanReadableCas tbl (OutputTree_ a)
    , CanReadableCas tbl (BlockTransactions_ a)
    , CanReadableCas tbl (BlockPayload_ a)
    )

type CanPayloadCas tbl = CanPayloadCas_ ChainwebMerkleHashAlgorithm tbl
type CanPayloadCas_ a tbl =
    ( CanCas tbl (BlockOutputs_ a)
    , CanCas tbl (TransactionTree_ a)
    , CanCas tbl (OutputTree_ a)
    , CanCas tbl (BlockTransactions_ a)
    , CanCas tbl (BlockPayload_ a)
    )

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
        addNewPayload db $ genesisBlockPayload v cid

-- -------------------------------------------------------------------------- --
-- Insert new Payload

-- | Insert block payload data in to the database.
--
addPayload
    :: MerkleHashAlgorithm a
    => CanPayloadCas_ a tbl
    => PayloadDb_ a tbl
    -> BlockTransactions_ a
    -> TransactionTree_ a
    -> BlockOutputs_ a
    -> OutputTree_ a
    -> IO ()
addPayload db txs txTree outs outTree = do
    casInsert (_transactionDbBlockPayloads $ _transactionDb db) payload 
    casInsert (_transactionDbBlockTransactions $ _transactionDb db) txs 
    casInsert (_payloadCacheBlockOutputs $ _payloadCache db) outs 
    casInsert (_payloadCacheTransactionTrees $ _payloadCache db) txTree 
    casInsert (_payloadCacheOutputTrees $ _payloadCache db) outTree 
  where
    payload = blockPayload txs outs

-- | Create block payload data from a sequence of transaction and insert it into
-- the database.
--
addNewPayload
    :: MerkleHashAlgorithm a
    => CanPayloadCas_ a tbl
    => PayloadDb_ a tbl
    -> PayloadWithOutputs_ a
    -> IO ()
addNewPayload db s = addPayload db txs txTree outs outTree
  where
    (bts, bos) = payloadWithOutputsToBlockObjects s
    (txTree, txs) = newBlockTransactions (_blockMinerData bts) (_blockTransactions bts)
    (outTree, outs) = newBlockOutputs (_blockCoinbaseOutput bos) (_blockOutputs bos)

-- -------------------------------------------------------------------------- --
-- IsCas instance for PayloadDb

-- | Combine all Payload related stores into a single content addressed
-- store. We want the invariant that if a key is present in the store also all
-- of its dependencies are present. For that we must be careful about the order
-- of insertion and deletions.
--
instance (pk ~ CasKeyType (PayloadWithOutputs_ a), CanReadablePayloadCas_ a tbl) => ReadableTable (PayloadDb_ a tbl) pk (PayloadWithOutputs_ a) where
    tableLookup db k = runMaybeT $ do
        pd <- MaybeT $ tableLookup
            (_transactionDbBlockPayloads $ _transactionDb db)
            k
        let txsHash = _blockPayloadTransactionsHash pd
        let outsHash = _blockPayloadOutputsHash pd
        txs <- MaybeT $ tableLookup
            (_transactionDbBlockTransactions $ _transactionDb db)
            txsHash
        outs <- MaybeT $ tableLookup
            (_payloadCacheBlockOutputs $ _payloadCache db)
            outsHash
        return $ PayloadWithOutputs
            { _payloadWithOutputsTransactions = V.zip (_blockTransactions txs) (_blockOutputs outs)
            , _payloadWithOutputsMiner = _blockMinerData txs
            , _payloadWithOutputsCoinbase = _blockCoinbaseOutput outs
            , _payloadWithOutputsPayloadHash = k
            , _payloadWithOutputsTransactionsHash = txsHash
            , _payloadWithOutputsOutputsHash = outsHash
            }
    {-# INLINE tableLookup #-}


-- | Combine all Payload related stores into a single content addressed
-- store. We want the invariant that if a key is present in the store also all
-- of its dependencies are present. For that we must be careful about the order
-- of insertion and deletions.
--
instance (pk ~ CasKeyType (PayloadWithOutputs_ a), MerkleHashAlgorithm a, CanPayloadCas_ a tbl) => Table (PayloadDb_ a tbl) pk (PayloadWithOutputs_ a) where
    tableInsert db _ v = addNewPayload db v
    {-# INLINE tableInsert #-}

    tableDelete db k = 
        tableLookup (_transactionDbBlockPayloads $ _transactionDb db) k >>= \case
            Just pd -> do
                tableDelete
                    (_transactionDbBlockPayloads $ _transactionDb db)
                    k
                tableDelete
                    (_transactionDbBlockTransactions $ _transactionDb db)
                    (_blockPayloadTransactionsHash pd)
                tableDelete
                    (_payloadCacheBlockOutputs $ _payloadCache db)
                    (_blockPayloadOutputsHash pd)
            Nothing -> return ()
    {-# INLINE tableDelete #-}
