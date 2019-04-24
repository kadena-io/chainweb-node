{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.Payload.PayloadStore
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Payload.PayloadStore
(
-- * Transaction Database

  BlockTransactionsStore(..)
, BlockPayloadStore(..)
, TransactionDb(..)
, TransactionDbCas
, transactionDbBlockTransactions
, transactionDbBlockPayloads

-- * Caches

, OutputTreeStore(..)
, TransactionTreeStore(..)
, BlockOutputsStore(..)
, PayloadCache(..)
, PayloadCacheCas
, payloadCacheBlockOutputs
, payloadCacheOutputTrees
, payloadCacheTransactionTrees

-- * Payload Database

, PayloadDb(..)
, PayloadCas
, payloadCache
, transactionDb

-- ** Initialize Payload Database with Genesis Payloads

, initializePayloadDb

-- **  insert new payload

, addPayload
, addNewPayload
) where

import Control.Lens
import Control.Monad.Trans.Maybe

import Data.Foldable
import qualified Data.Sequence as S

-- internal modules

import Chainweb.BlockHeader.Genesis (genesisBlockPayload)
import Chainweb.Payload
import Chainweb.Version

import Data.CAS

type CasConstraint cas x = (IsCas (cas x), CasValueType (cas x) ~ x)

-- -------------------------------------------------------------------------- --
-- Transaction Database

-- | Store of the 'BlockPayloads' for all blocks
--
-- Primary Key: '_blockPayloadHash'
--
newtype BlockPayloadStore cas = BlockPayloadStore (cas BlockPayload)
deriving newtype instance IsCas (cas BlockPayload) => IsCas (BlockPayloadStore cas)

-- | Store of the 'BlockTransactions' for all blocks.
--
-- Primary Key: '_blockTransactionsHash'
--
newtype BlockTransactionsStore cas = BlockTransactionsStore (cas BlockTransactions)
deriving newtype instance IsCas (cas BlockTransactions) => IsCas (BlockTransactionsStore cas)

-- | The authoritative CAS stores for a block chain.
--
data TransactionDb cas = TransactionDb
    { _transactionDbBlockTransactions :: !(BlockTransactionsStore cas)
        -- ^ The block transactions of the block chain. This data is strictly
        -- needed to rebuild the payload data.

    , _transactionDbBlockPayloads :: !(BlockPayloadStore cas)
        -- ^ While the content of this store can be computed from the block
        -- transactions, it is needed as an index into the block transaction
        -- store. If it would be lost one would have to recompute all of it in
        -- order to look up the data for a single transation.
    }

makeLenses ''TransactionDb

type TransactionDbCas cas =
    ( CasConstraint cas BlockPayload
    , CasConstraint cas BlockTransactions
    )

-- -------------------------------------------------------------------------- --
-- Caches

-- | Store of the 'BlockOutputs' for all blocks.
--
newtype BlockOutputsStore cas = BlockOutputsStore (cas BlockOutputs)
deriving newtype instance CasConstraint cas BlockOutputs => IsCas (BlockOutputsStore cas)

-- | Store of the 'TransactionTree' Merkle trees for all blocks.
--
newtype TransactionTreeStore cas = TransactionTreeStore (cas TransactionTree)
deriving newtype instance CasConstraint cas TransactionTree => IsCas (TransactionTreeStore cas)

-- | Store of the 'OutputTree' Merkle trees for all blocks.
--
newtype OutputTreeStore cas = OutputTreeStore (cas OutputTree)
deriving newtype instance CasConstraint cas OutputTree => IsCas (OutputTreeStore cas)

-- | The CAS caches for a block chain.
--
-- If an entry is missing it can be rebuild from the PayloadDb
--
data PayloadCache cas = PayloadCache
    { _payloadCacheBlockOutputs :: !(BlockOutputsStore cas)
        -- ^ This is relatively expensive to rebuild.

    , _payloadCacheTransactionTrees :: !(TransactionTreeStore cas)
        -- ^ This are relatively cheap to rebuild.
        -- (tens of thousands of blocks per second)

    , _payloadCacheOutputTrees :: !(OutputTreeStore cas)
        -- ^ This are relatively cheap to rebuild.
        -- (tens of thousands blocks per second)
    }

makeLenses ''PayloadCache

type PayloadCacheCas cas =
    ( CasConstraint cas BlockOutputs
    , CasConstraint cas TransactionTree
    , CasConstraint cas OutputTree
    )

-- -------------------------------------------------------------------------- --
-- Payload Database

data PayloadDb cas = PayloadDb
    { _transactionDb :: !(TransactionDb cas)
    , _payloadCache :: !(PayloadCache cas)
    }

makeLenses ''PayloadDb

type PayloadCas cas =
    ( CasConstraint cas BlockOutputs
    , CasConstraint cas TransactionTree
    , CasConstraint cas OutputTree
    , CasConstraint cas BlockTransactions
    , CasConstraint cas BlockPayload
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
    initForChain cid = do
        addNewPayload db $ genesisBlockPayload v cid

-- -------------------------------------------------------------------------- --
-- Insert new Payload

-- | Insert block payload data in to the database.
--
addPayload
    :: PayloadCas cas
    => PayloadDb cas
    -> BlockTransactions
    -> TransactionTree
    -> BlockOutputs
    -> OutputTree
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
    :: PayloadCas cas
    => PayloadDb cas
    -> PayloadWithOutputs
    -> IO ()
addNewPayload db s = addPayload db txs txTree outs outTree
  where
    (bts,bos) = payloadWithOutputsToBlockObjects s
    (txTree, txs) = newBlockTransactions (_blockMinerData bts) (_blockTransactions bts)
    (outTree, outs) = newBlockOutputs (_blockCoinbaseOutput bos) (_blockOutputs bos)

-- -------------------------------------------------------------------------- --
-- IsCas instance for PayloadDb

-- | Combine all Payload related stores into a single content addressed
-- store. We want the invariant that if a key is present in the store also all
-- of its dependencies are present. For that we must be careful about the order
-- of insertion and deletions.
--
instance PayloadCas cas => IsCas (PayloadDb cas) where
    type CasValueType (PayloadDb cas) = PayloadWithOutputs
    casInsert = addNewPayload
    {-# INLINE casInsert #-}

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
            { _payloadWithOutputsTransactions = S.zip (_blockTransactions txs) (_blockOutputs outs)
            , _payloadWithOutputsMiner = _blockMinerData txs
            , _payloadWithOutputsCoinbase = _blockCoinbaseOutput outs
            , _payloadWithOutputsPayloadHash = k
            , _payloadWithOutputsTransactionsHash = txsHash
            , _payloadWithOutputsOutputsHash = outsHash
            }
    {-# INLINE casLookup #-}

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
