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
    newBlockPayloadHeightIndex :: IO (HashMapTable (BlockPayloadHash_ a) BlockHeight)
    newBlockPayloadHeightIndex = HashMapTable.emptyTable

    newBlockPayloadStore :: IO (HashMapTable (BlockHeight, CasKeyType (BlockPayload_ a)) (BlockPayload_ a))
    newBlockPayloadStore = HashMapTable.emptyTable

    newBlockTransactionsStore :: IO (HashMapTable (BlockHeight, CasKeyType (BlockTransactions_ a)) (BlockTransactions_ a))
    newBlockTransactionsStore = HashMapTable.emptyTable

    oldBlockPayloadStore :: IO (Casify HashMapTable BlockPayload)
    oldBlockPayloadStore = Casify <$> HashMapTable.emptyTable

    oldBlockTransactionsStore :: IO (Casify HashMapTable BlockTransactions)
    oldBlockTransactionsStore = Casify <$> HashMapTable.emptyTable

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
        <$> (Casify <$> HashMapTable.emptyTable)
        <*> HashMapTable.emptyTable

    newTransactionTreeStore :: IO (TransactionTreeStore HashMapTable)
    newTransactionTreeStore = TransactionTreeStore
        <$> (Casify <$> HashMapTable.emptyTable)
        <*> HashMapTable.emptyTable

    newOutputTreeStore :: IO (OutputTreeStore HashMapTable)
    newOutputTreeStore = OutputTreeStore
        <$> (Casify <$> HashMapTable.emptyTable)
        <*> HashMapTable.emptyTable
