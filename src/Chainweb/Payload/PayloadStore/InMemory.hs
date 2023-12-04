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
( newPayloadDb

-- * Internal
, newBlockPayloadStore
, newBlockTransactionsStore
, newTransactionDb
, newBlockOutputsStore
, newTransactionTreeStore
, newOutputTreeStore
, newPayloadCache
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

newBlockPayloadStore :: IO (HashMapTable (BlockHeight, BlockPayloadHash_ a) (BlockPayload_ a))
newBlockPayloadStore = HashMapTable.emptyTable

newBlockTransactionsStore :: IO (Casify HashMapTable BlockTransactions)
newBlockTransactionsStore = Casify <$> HashMapTable.emptyTable

newBlockPayloadHeightIndex :: IO (HashMapTable (BlockPayloadHash_ a) BlockHeight)
newBlockPayloadHeightIndex = HashMapTable.emptyTable

newTransactionDb :: IO (TransactionDb HashMapTable)
newTransactionDb = TransactionDb
    <$> newBlockTransactionsStore
    <*> newBlockPayloadStore
    <*> newBlockPayloadHeightIndex

newBlockOutputsStore :: IO (BlockOutputsStore HashMapTable)
newBlockOutputsStore = Casify <$> HashMapTable.emptyTable

newTransactionTreeStore :: IO (TransactionTreeStore HashMapTable)
newTransactionTreeStore = Casify <$> HashMapTable.emptyTable

newOutputTreeStore :: IO (OutputTreeStore HashMapTable)
newOutputTreeStore = Casify <$> HashMapTable.emptyTable

newPayloadCache :: IO (PayloadCache HashMapTable)
newPayloadCache = PayloadCache <$> newBlockOutputsStore
    <*> newTransactionTreeStore
    <*> newOutputTreeStore

newPayloadDb :: IO (PayloadDb HashMapTable)
newPayloadDb = PayloadDb <$> newTransactionDb <*> newPayloadCache
