{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Payload.PayloadStore.RocksDB
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Content addressable block payload store that uses RocksDB as storage backend.
--
module Chainweb.Payload.PayloadStore.RocksDB
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

import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Serialization

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

-- -------------------------------------------------------------------------- --
-- RocksDbCas

newBlockPayloadStore :: RocksDb -> Casify RocksDbTable BlockPayload
newBlockPayloadStore db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockPayloadHash) (runGetS decodeBlockPayloadHash))
    ["BlockPayload"]

newBlockTransactionsStore :: RocksDb -> Casify RocksDbTable BlockTransactions 
newBlockTransactionsStore db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockTransactionsHash) (runGetS decodeBlockTransactionsHash))
    ["BlockTransactions"]

newTransactionDb :: RocksDb -> TransactionDb RocksDbTable
newTransactionDb db = TransactionDb
    (newBlockTransactionsStore db)
    (newBlockPayloadStore db)

newBlockOutputsStore :: RocksDb -> BlockOutputsStore RocksDbTable
newBlockOutputsStore db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockOutputsHash) (runGetS decodeBlockOutputsHash))
    ["BlockOutputs"]

newTransactionTreeStore :: RocksDb -> TransactionTreeStore RocksDbTable
newTransactionTreeStore db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockTransactionsHash) (runGetS decodeBlockTransactionsHash))
    ["TransactionTree"]

newOutputTreeStore :: RocksDb -> OutputTreeStore RocksDbTable
newOutputTreeStore db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockOutputsHash) (runGetS decodeBlockOutputsHash))
    ["OutputTree"]

newPayloadCache :: RocksDb -> PayloadCache RocksDbTable
newPayloadCache db = PayloadCache
    (newBlockOutputsStore db)
    (newTransactionTreeStore db)
    (newOutputTreeStore db)

newPayloadDb :: RocksDb -> PayloadDb RocksDbTable
newPayloadDb db = PayloadDb
    (newTransactionDb db)
    (newPayloadCache db)

