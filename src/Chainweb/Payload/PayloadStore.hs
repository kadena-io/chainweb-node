{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
, BlockTransactionsStore
, BlockTransactionsStore_(..)
, BlockPayloadStore
, BlockPayloadStore_(..)
, TransactionDb
, TransactionDb_(..)
, TransactionDbCasLookup
, TransactionDbCas
, transactionDbBlockTransactions
, transactionDbBlockPayloads

-- * Caches
, OutputTreeStore
, OutputTreeStore_(..)
, TransactionTreeStore
, TransactionTreeStore_(..)
, BlockOutputsStore
, BlockOutputsStore_(..)
, PayloadCache
, PayloadCache_(..)
, PayloadCacheCasLookup
, PayloadCacheCas
, payloadCacheBlockOutputs
, payloadCacheOutputTrees
, payloadCacheTransactionTrees


-- * Payload Database
, PayloadDb
, PayloadDb_(..)
, PayloadCasLookup
, PayloadCas
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

import Data.CAS

-- -------------------------------------------------------------------------- --
-- Exceptions

type PayloadNotFoundException = PayloadNotFoundException_ ChainwebMerkleHashAlgorithm

newtype PayloadNotFoundException_ a = PayloadNotFoundException (BlockPayloadHash_ a)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)

instance Exception PayloadNotFoundException

-- -------------------------------------------------------------------------- --
-- Transaction Database


-- | Store of the 'BlockPayloads' for all blocks
--
-- Primary Key: '_blockPayloadHash'
--
type BlockPayloadStore cas = BlockPayloadStore_ ChainwebMerkleHashAlgorithm cas
newtype BlockPayloadStore_ a cas = BlockPayloadStore (cas (BlockPayload_ a))
deriving newtype instance HasCasLookup (cas (BlockPayload_ a)) => HasCasLookup (BlockPayloadStore_ a cas)
deriving newtype instance IsCas (cas (BlockPayload_ a)) => IsCas (BlockPayloadStore_ a cas)

-- | Store of the 'BlockTransactions' for all blocks.
--
-- Primary Key: '_blockTransactionsHash'
--
type BlockTransactionsStore cas = BlockTransactionsStore_ ChainwebMerkleHashAlgorithm cas
newtype BlockTransactionsStore_ a cas = BlockTransactionsStore (cas (BlockTransactions_ a))
deriving newtype instance HasCasLookup (cas (BlockTransactions_ a)) => HasCasLookup (BlockTransactionsStore_ a cas)
deriving newtype instance IsCas (cas (BlockTransactions_ a)) => IsCas (BlockTransactionsStore_ a cas)

type TransactionDb cas = TransactionDb_ ChainwebMerkleHashAlgorithm cas

-- | The authoritative CAS stores for a block chain.
--
data TransactionDb_ a cas = TransactionDb
    { _transactionDbBlockTransactions :: !(BlockTransactionsStore_ a cas)
        -- ^ The block transactions of the block chain. This data is strictly
        -- needed to rebuild the payload data.

    , _transactionDbBlockPayloads :: !(BlockPayloadStore_ a cas)
        -- ^ While the content of this store can be computed from the block
        -- transactions, it is needed as an index into the block transaction
        -- store. If it would be lost one would have to recompute all of it in
        -- order to look up the data for a single transation.
    }

makeLenses ''TransactionDb_

type TransactionDbCasLookup cas = TransactionDbCasLookup_ ChainwebMerkleHashAlgorithm cas
type TransactionDbCasLookup_ a cas =
    ( HasCasLookupConstraint cas (BlockPayload_ a)
    , HasCasLookupConstraint cas (BlockTransactions_ a)
    )

type TransactionDbCas cas = TransactionDbCas_ ChainwebMerkleHashAlgorithm cas
type TransactionDbCas_ a cas =
    ( CasConstraint cas (BlockPayload_ a)
    , CasConstraint cas (BlockTransactions_ a)
    )

instance TransactionDbCasLookup_ a cas => HasCasLookup (TransactionDb_ a cas) where
    type CasValueType (TransactionDb_ a cas) = PayloadData_ a

    casLookup db k = runMaybeT $ do
        pd <- MaybeT $ casLookup (_transactionDbBlockPayloads db) k
        let txsHash = _blockPayloadTransactionsHash pd
        let outsHash = _blockPayloadOutputsHash pd
        txs <- MaybeT $ casLookup (_transactionDbBlockTransactions db) txsHash
        return $ PayloadData
            { _payloadDataTransactions = _blockTransactions txs
            , _payloadDataMiner = _blockMinerData txs
            , _payloadDataPayloadHash = k
            , _payloadDataTransactionsHash = txsHash
            , _payloadDataOutputsHash = outsHash
            }
    {-# INLINE casLookup #-}

-- -------------------------------------------------------------------------- --
-- Caches

-- | Store of the 'BlockOutputs' for all blocks.
--
type BlockOutputsStore cas = BlockOutputsStore_ ChainwebMerkleHashAlgorithm cas
newtype BlockOutputsStore_ a cas = BlockOutputsStore (cas (BlockOutputs_ a))
deriving newtype instance HasCasLookupConstraint cas (BlockOutputs_ a) => HasCasLookup (BlockOutputsStore_ a cas)
deriving newtype instance CasConstraint cas (BlockOutputs_ a) => IsCas (BlockOutputsStore_ a cas)

-- | Store of the 'TransactionTree' Merkle trees for all blocks.
--
type TransactionTreeStore cas = TransactionTreeStore_ ChainwebMerkleHashAlgorithm cas
newtype TransactionTreeStore_ a cas = TransactionTreeStore (cas (TransactionTree_ a))
deriving newtype instance HasCasLookupConstraint cas (TransactionTree_ a) => HasCasLookup (TransactionTreeStore_ a cas)
deriving newtype instance CasConstraint cas (TransactionTree_ a) => IsCas (TransactionTreeStore_ a cas)

-- | Store of the 'OutputTree' Merkle trees for all blocks.
--
type OutputTreeStore cas = OutputTreeStore_ ChainwebMerkleHashAlgorithm cas
newtype OutputTreeStore_ a cas = OutputTreeStore (cas (OutputTree_ a))
deriving newtype instance HasCasLookupConstraint cas (OutputTree_ a) => HasCasLookup (OutputTreeStore_ a cas)
deriving newtype instance CasConstraint cas (OutputTree_ a) => IsCas (OutputTreeStore_ a cas)

-- | The CAS caches for a block chain.
--
-- If an entry is missing it can be rebuild from the PayloadDb
--
type PayloadCache cas = PayloadCache_ ChainwebMerkleHashAlgorithm cas
data PayloadCache_ a cas = PayloadCache
    { _payloadCacheBlockOutputs :: !(BlockOutputsStore_ a cas)
        -- ^ This is relatively expensive to rebuild.

    , _payloadCacheTransactionTrees :: !(TransactionTreeStore_ a cas)
        -- ^ This are relatively cheap to rebuild.
        -- (tens of thousands of blocks per second)

    , _payloadCacheOutputTrees :: !(OutputTreeStore_ a cas)
        -- ^ This are relatively cheap to rebuild.
        -- (tens of thousands blocks per second)
    }

makeLenses ''PayloadCache_

type PayloadCacheCasLookup cas = PayloadCacheCasLookup_ ChainwebMerkleHashAlgorithm cas
type PayloadCacheCasLookup_ a cas =
    ( HasCasLookupConstraint cas (BlockOutputs_ a)
    , HasCasLookupConstraint cas (TransactionTree_ a)
    , HasCasLookupConstraint cas (OutputTree_ a)
    )

type PayloadCacheCas cas = PayloadCacheCas_ ChainwebMerkleHashAlgorithm cas
type PayloadCacheCas_ a cas =
    ( CasConstraint cas (BlockOutputs_ a)
    , CasConstraint cas (TransactionTree_ a)
    , CasConstraint cas (OutputTree_ a)
    )

-- -------------------------------------------------------------------------- --
-- Payload Database

type PayloadDb cas = PayloadDb_ ChainwebMerkleHashAlgorithm cas

data PayloadDb_ a cas = PayloadDb
    { _transactionDb :: !(TransactionDb_ a cas)
    , _payloadCache :: !(PayloadCache_ a cas)
    }

makeLenses ''PayloadDb_

type PayloadCasLookup cas = PayloadCasLookup_ ChainwebMerkleHashAlgorithm cas
type PayloadCasLookup_ a cas =
    ( HasCasLookupConstraint cas (BlockOutputs_ a)
    , HasCasLookupConstraint cas (TransactionTree_ a)
    , HasCasLookupConstraint cas (OutputTree_ a)
    , HasCasLookupConstraint cas (BlockTransactions_ a)
    , HasCasLookupConstraint cas (BlockPayload_ a)
    )

type PayloadCas cas = PayloadCas_ ChainwebMerkleHashAlgorithm cas
type PayloadCas_ a cas =
    ( CasConstraint cas (BlockOutputs_ a)
    , CasConstraint cas (TransactionTree_ a)
    , CasConstraint cas (OutputTree_ a)
    , CasConstraint cas (BlockTransactions_ a)
    , CasConstraint cas (BlockPayload_ a)
    )

-- -------------------------------------------------------------------------- --
-- Initialize a PayloadDb with Genesis Payloads

-- | Initialize a PayloadDb with genesis payloads for the given chainweb
-- version.
--
initializePayloadDb
    :: PayloadCas cas
    => ChainwebVersion
    -> PayloadDb cas
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
    => PayloadCas_ a cas
    => PayloadDb_ a cas
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
    => PayloadCas_ a cas
    => PayloadDb_ a cas
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
instance PayloadCasLookup_ a cas => HasCasLookup (PayloadDb_ a cas) where
    type CasValueType (PayloadDb_ a cas) = PayloadWithOutputs_ a

    casLookup db k = runMaybeT $ do
        pd <- MaybeT $ casLookup
            (_transactionDbBlockPayloads $ _transactionDb db)
            k
        let txsHash = _blockPayloadTransactionsHash pd
        let outsHash = _blockPayloadOutputsHash pd
        txs <- MaybeT $ casLookup
            (_transactionDbBlockTransactions $ _transactionDb db)
            txsHash
        outs <- MaybeT $ casLookup
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
    {-# INLINE casLookup #-}


-- | Combine all Payload related stores into a single content addressed
-- store. We want the invariant that if a key is present in the store also all
-- of its dependencies are present. For that we must be careful about the order
-- of insertion and deletions.
--
instance (MerkleHashAlgorithm a, PayloadCas_ a cas) => IsCas (PayloadDb_ a cas) where
    casInsert = addNewPayload
    {-# INLINE casInsert #-}

    casDelete db k =
        casLookup (_transactionDbBlockPayloads $ _transactionDb db) k >>= \case
            Just pd -> do
                casDelete
                    (_transactionDbBlockPayloads $ _transactionDb db)
                    k
                casDelete
                    (_transactionDbBlockTransactions $ _transactionDb db)
                    (_blockPayloadTransactionsHash pd)
                casDelete
                    (_payloadCacheBlockOutputs $ _payloadCache db)
                    (_blockPayloadOutputsHash pd)
            Nothing -> return ()
    {-# INLINE casDelete #-}
