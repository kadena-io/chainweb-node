-- Module: Chainweb.Payload
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Block payloads in the format as they are stored in the Chainweb Merkle tree.
--
-- The format abstracts from the smart contract language. I.e. it does not
-- depend on any Pact data structure. The reason for this is to allow changes to
-- Pact without breaking the Merkle tree.
--
module Chainweb.Payload
(
-- * Block Chain Data

  Transaction(..)
, TransactionOutput(..)

-- * Hashes

, BlockPayloadHash
, BlockPayloadHash_(..)
, encodeBlockPayloadHash
, decodeBlockPayloadHash

, BlockTransactionsHash
, BlockTransactionsHash_(..)
, encodeBlockTransactionsHash
, decodeBlockTransactionsHash

, BlockOutputsHash
, BlockOutputsHash_(..)
, encodeBlockOutputsHash
, decodeBlockOutputsHash

-- * Authoritative Data

, BlockPayload
, BlockPayload_(..)
, BlockTransactions
, BlockTransactions_(..)
, verifyBlockPayload

-- * Redundant Data / Caches

, BlockOutputs
, BlockOutputs_(..)
, TransactionTree
, TransactionTree_(..)
, verifyTransactionTree
, OutputTree
, OutputTree_(..)
, verifyOutputTree

-- * Create Data
, BlockTransactionsLog
, newTransactionLog
, newBlockTransactions
, transactionLog
, verifyBlockTransactions

, MinerData(..)
, CoinbaseOutput(..)
, noCoinbaseOutput

, BlockOutputsLog
, newBlockOutputLog
, newBlockOutputs
, blockOutputLog
, verifyBlockOutputs

, blockPayload
, newBlockPayload

-- * API Payload Data
, PayloadData
, PayloadData_
, payloadData
, payloadDataTransactions
, payloadDataMiner
, payloadDataPayloadHash
, payloadDataTransactionsHash
, payloadDataOutputsHash
, newPayloadData
, PayloadDataCas
, verifyPayloadData

-- * All Payload Data in a Single Structure
, PayloadWithOutputs
, PayloadWithOutputs_(..)
, payloadWithOutputs
, newPayloadWithOutputs
, payloadWithOutputsToBlockObjects
, payloadWithOutputsToPayloadData
, verifyPayloadWithOutputs
) where

import Chainweb.Payload.Internal
