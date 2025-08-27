{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Payload.PayloadStore.InMemory
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- An in-memory block payload store.
--
-- TODO: move to tests
module Chainweb.Payload.PayloadStore.InMemory
( newTransactionDb
, newPayloadDb
) where

-- internal modules

import Chainweb.BlockHeight
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Storage.Table
import Chainweb.Storage.Table.HashMap(HashMapTable)
import qualified Chainweb.Storage.Table.HashMap as HashMapTable
import Chainweb.Storage.Table.RocksDB (Codec(..))
import Chainweb.Utils.Serialization

-- -------------------------------------------------------------------------- --
-- HashMap CAS

newTransactionDb :: IO (TransactionDb HashMapTable)
newTransactionDb = TransactionDb
    <$> newBlockPayloadHeightIndex
    <*> newBlockPayloadStore
    <*> newBlockTransactionsStore
    <*> oldBlockPayloadStore
    <*> oldBlockTransactionsStore
  where
    newBlockPayloadHeightIndex :: IO (HashMapTable BlockPayloadHash BlockHeight)
    newBlockPayloadHeightIndex = HashMapTable.emptyTable (Codec (runPutS . encodeBlockHeight) (runGetS decodeBlockHeight))

    newBlockPayloadStore :: IO (HashMapTable (BlockHeight, CasKeyType BlockPayload) BlockPayload)
    newBlockPayloadStore = HashMapTable.emptyTable (Codec encodeBlockPayloads decodeBlockPayloads)

    newBlockTransactionsStore :: IO (HashMapTable (BlockHeight, CasKeyType BlockTransactions) BlockTransactions)
    newBlockTransactionsStore = HashMapTable.emptyTable (Codec encodeBlockTransactions decodeBlockTransactions)

    oldBlockPayloadStore :: IO (Casify HashMapTable BlockPayload)
    oldBlockPayloadStore = Casify <$> HashMapTable.emptyTable (Codec encodeBlockPayloads decodeBlockPayloads)

    oldBlockTransactionsStore :: IO (Casify HashMapTable BlockTransactions)
    oldBlockTransactionsStore = Casify <$> HashMapTable.emptyTable (Codec encodeBlockTransactions decodeBlockTransactions)

newPayloadDb :: IO (PayloadDb HashMapTable)
newPayloadDb = PayloadDb <$> newTransactionDb <*> newPayloadCache
  where
    newPayloadCache :: IO (PayloadCache HashMapTable)
    newPayloadCache = PayloadCache
        <$> newBlockOutputsStore
        <*> newTransactionTreeStore
        <*> newOutputTreeStore

    newBlockOutputsStore :: IO (BlockOutputsStore HashMapTable)
    newBlockOutputsStore = BlockOutputsStore
        <$> (Casify <$> HashMapTable.emptyTable (Codec encodeBlockOutputs decodeBlockOutputs))
        <*> HashMapTable.emptyTable (Codec encodeBlockOutputs decodeBlockOutputs)

    newTransactionTreeStore :: IO (TransactionTreeStore HashMapTable)
    newTransactionTreeStore = TransactionTreeStore
        <$> (Casify <$> HashMapTable.emptyTable (Codec encodeTransactionTree decodeTransactionTree))
        <*> HashMapTable.emptyTable (Codec encodeTransactionTree decodeTransactionTree)

    newOutputTreeStore :: IO (OutputTreeStore HashMapTable)
    newOutputTreeStore = OutputTreeStore
        <$> (Casify <$> HashMapTable.emptyTable (Codec encodeOutputTree decodeOutputTree))
        <*> HashMapTable.emptyTable (Codec encodeOutputTree decodeOutputTree)
