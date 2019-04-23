{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Payload.PayloadStore.RocksDB
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Content addressable block payload store that uses RocksDB as storage backend.
--
module Chainweb.Payload.PayloadStore.RocksDB
( newPayloadDb
, openPayloadDb

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

import Data.CAS.RocksDB

-- -------------------------------------------------------------------------- --
-- RocksDbCas

newBlockPayloadStore :: RocksDb -> BlockPayloadStore RocksDbCas
newBlockPayloadStore db = BlockPayloadStore $ newCas db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPut . encodeBlockPayloadHash) (runGet decodeBlockPayloadHash))
    ["BlockPayload"]

newBlockTransactionsStore :: RocksDb -> BlockTransactionsStore RocksDbCas
newBlockTransactionsStore db = BlockTransactionsStore $ newCas db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPut . encodeBlockTransactionsHash) (runGet decodeBlockTransactionsHash))
    ["BlockTransactions"]

newTransactionDb :: RocksDb -> TransactionDb RocksDbCas
newTransactionDb db = TransactionDb
    (newBlockTransactionsStore db)
    (newBlockPayloadStore db)

newBlockOutputsStore :: RocksDb -> BlockOutputsStore RocksDbCas
newBlockOutputsStore db = BlockOutputsStore $ newCas db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPut . encodeBlockOutputsHash) (runGet decodeBlockOutputsHash))
    ["BlockOutputs"]

newTransactionTreeStore :: RocksDb -> TransactionTreeStore RocksDbCas
newTransactionTreeStore db = TransactionTreeStore $ newCas db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPut . encodeBlockTransactionsHash) (runGet decodeBlockTransactionsHash))
    ["TransactionTree"]

newOutputTreeStore :: RocksDb -> OutputTreeStore RocksDbCas
newOutputTreeStore db = OutputTreeStore $ newCas db
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPut . encodeBlockOutputsHash) (runGet decodeBlockOutputsHash))
    ["OutputTree"]

newPayloadCache :: RocksDb -> PayloadCache RocksDbCas
newPayloadCache db = PayloadCache
    (newBlockOutputsStore db)
    (newTransactionTreeStore db)
    (newOutputTreeStore db)

newPayloadDb :: RocksDb -> PayloadDb RocksDbCas
newPayloadDb db = PayloadDb
    (newTransactionDb db)
    (newPayloadCache db)

openPayloadDb :: FilePath -> IO (PayloadDb RocksDbCas)
openPayloadDb path = newPayloadDb <$> openRocksDb path

