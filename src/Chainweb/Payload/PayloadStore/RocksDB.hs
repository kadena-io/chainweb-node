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
, oldBlockPayloadStore
, newBlockPayloadStore
, newBlockPayloadHeightsStore
, newBlockTransactionsStore
, newTransactionDb
, blockOutputsStore
, transactionTreeStore
, outputTreeStore
, newPayloadCache
) where

-- internal modules

import Chainweb.BlockHeight
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Serialization

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

-- -------------------------------------------------------------------------- --
-- RocksDbCas

oldBlockPayloadStore :: RocksDb -> Casify RocksDbTable BlockPayload
oldBlockPayloadStore db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockPayloadHash) (runGetS decodeBlockPayloadHash))
    ["BlockPayload"]

newBlockPayloadStore :: RocksDb -> RocksDbTable (BlockHeight, BlockPayloadHash) BlockPayload
newBlockPayloadStore db = newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (\(bh, bp) -> runPutS (encodeBlockHeight bh >> encodeBlockPayloadHash bp)) (runGetS ((,) <$> decodeBlockHeight <*> decodeBlockPayloadHash)))
    ["BlockPayload2"]

oldBlockTransactionsStore :: RocksDb -> Casify RocksDbTable BlockTransactions
oldBlockTransactionsStore db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockTransactionsHash) (runGetS decodeBlockTransactionsHash))
    ["BlockTransactions"]

newBlockTransactionsStore :: RocksDb -> RocksDbTable (BlockHeight, BlockTransactionsHash) BlockTransactions
newBlockTransactionsStore db = newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (\(h, hsh) -> runPutS $ encodeBlockHeight h >> encodeBlockTransactionsHash hsh) (runGetS ((,) <$> decodeBlockHeight <*> decodeBlockTransactionsHash)))
    ["BlockTransactions2"]

newBlockPayloadHeightsStore :: RocksDb -> RocksDbTable BlockPayloadHash BlockHeight
newBlockPayloadHeightsStore db = newTable db
    (Codec (runPutS . encodeBlockHeight) (runGetS decodeBlockHeight))
    (Codec (runPutS . encodeBlockPayloadHash) (runGetS decodeBlockPayloadHash))
    ["BlockPayloadIndex"]

newTransactionDb :: RocksDb -> TransactionDb RocksDbTable
newTransactionDb db = TransactionDb
    (oldBlockTransactionsStore db)
    (newBlockTransactionsStore db)
    (newBlockPayloadStore db)
    (newBlockPayloadHeightsStore db)
    (oldBlockPayloadStore db)

blockOutputsStore :: RocksDb -> BlockOutputsStore RocksDbTable
blockOutputsStore db =
    BlockOutputsStore (oldBlockOutputsTbl db) (newBlockOutputsTbl db)

oldBlockOutputsTbl :: RocksDb -> Casify RocksDbTable BlockOutputs
oldBlockOutputsTbl db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockOutputsHash) (runGetS decodeBlockOutputsHash))
    ["BlockOutputs"]

newBlockOutputsTbl :: RocksDb -> RocksDbTable (BlockHeight, BlockOutputsHash) BlockOutputs
newBlockOutputsTbl db = newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec
        (\(h, hsh) -> runPutS $ encodeBlockHeight h >> encodeBlockOutputsHash hsh)
        (runGetS $ (,) <$> decodeBlockHeight <*> decodeBlockOutputsHash))
    ["BlockOutputs2"]

oldTransactionTreeTbl :: RocksDb -> Casify RocksDbTable TransactionTree
oldTransactionTreeTbl db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockTransactionsHash) (runGetS decodeBlockTransactionsHash))
    ["TransactionTree"]

newTransactionTreeTbl
    :: RocksDb -> RocksDbTable (BlockHeight, BlockTransactionsHash) TransactionTree
newTransactionTreeTbl db = newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec
        (\(h, hsh) -> runPutS $ encodeBlockHeight h >> encodeBlockTransactionsHash hsh)
        (runGetS $ (,) <$> decodeBlockHeight <*> decodeBlockTransactionsHash))
    ["TransactionTree2"]

transactionTreeStore :: RocksDb -> TransactionTreeStore RocksDbTable
transactionTreeStore db =
    TransactionTreeStore (oldTransactionTreeTbl db) (newTransactionTreeTbl db)

oldOutputTreeTbl :: RocksDb -> Casify RocksDbTable OutputTree
oldOutputTreeTbl db = Casify $ newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPutS . encodeBlockOutputsHash) (runGetS decodeBlockOutputsHash))
    ["OutputTree"]

newOutputTreeTbl :: RocksDb -> RocksDbTable (BlockHeight, BlockOutputsHash) OutputTree
newOutputTreeTbl db = newTable db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec
        (\(h, hsh) -> runPutS $ encodeBlockHeight h >> encodeBlockOutputsHash hsh)
        (runGetS $ (,) <$> decodeBlockHeight <*> decodeBlockOutputsHash))
    ["OutputTree2"]

outputTreeStore :: RocksDb -> OutputTreeStore RocksDbTable
outputTreeStore db = OutputTreeStore (oldOutputTreeTbl db) (newOutputTreeTbl db)

newPayloadCache :: RocksDb -> PayloadCache RocksDbTable
newPayloadCache db = PayloadCache
    (blockOutputsStore db)
    (transactionTreeStore db)
    (outputTreeStore db)

newPayloadDb :: RocksDb -> PayloadDb RocksDbTable
newPayloadDb db = PayloadDb
    (newTransactionDb db)
    (newPayloadCache db)
