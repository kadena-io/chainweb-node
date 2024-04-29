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

-- * Binary encodings

, encodeBlockPayloads
, decodeBlockPayloads
, encodeBlockTransactions
, decodeBlockTransactions
, encodeBlockOutputs
, decodeBlockOutputs
, encodeTransactionTree
, decodeTransactionTree
, encodeOutputTree
, decodeOutputTree

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
, payloadDataToBlockPayload
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

, CheckablePayload(..)
, checkablePayloadToPayloadData
) where

import Chainweb.Payload.Internal
