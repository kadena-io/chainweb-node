{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.Receipt
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM.Receipt
( LogTopic(..)
, LogData(..)
, LogEntry(..)
, RpcLogEntry(..)
, fromRpcLogEntry
, TxStatus(..)
, Receipt(..)
, ReceiptsRoot(..)
, TransactionIndex(..)
, RpcReceipt(..)
, fromRpcReceipt
, encodeReceipt
, receiptTrieProof
, rpcReceiptTrieProof
) where

import Control.Applicative
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.PayloadProvider.EVM.Utils
import Chainweb.Utils
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Bifunctor
import Data.ByteString qualified as B
import Ethereum.Misc
import Ethereum.RLP
import Ethereum.Trie
import Ethereum.Utils hiding (int)
import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- Log Entry

newtype LogTopic = LogTopic (BytesN 32)
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

newtype LogData = LogData B.ByteString
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes B.ByteString)
    deriving FromJSON via (HexBytes B.ByteString)

-- | LogEntry
--
-- https://github.com/ethereum/py-evm/blob/main/eth/rlp/logs.py:
--
-- @
-- fields = [
--     ("address", address),
--     ("topics", CountableList(uint32)),
--     ("data", binary)
-- ]
-- @
--
data LogEntry = LogEntry
    { _logEntryAddress :: !Address
    , _logEntryTopics :: ![LogTopic]
    , _logEntryData :: !LogData
    }
    deriving (Show, Eq)

instance RLP LogEntry where
    putRlp a = putRlpL
        [ putRlp $! _logEntryAddress a
        , putRlpL $! putRlp <$> _logEntryTopics a
        , putRlp $! _logEntryData a
        ]
    getRlp = label "LogEntry" $ getRlpL $ LogEntry
        <$> label "logEntryAddress" getRlp
        <*> label "logEntryTopics" getRlp
        <*> label "logEntryData" getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance ToJSON LogEntry where
    toEncoding = pairs . mconcat . logEntryProperties
    toJSON = object . logEntryProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON LogEntry where
    parseJSON = withObject "LogEntry" $ \o -> LogEntry
        <$> o .: "address"
        <*> o .: "topics"
        <*> o .: "data"
    {-# INLINE parseJSON #-}

logEntryProperties :: KeyValue e kv => LogEntry -> [kv]
logEntryProperties r =
    [ "address" .= _logEntryAddress r
    , "data" .= _logEntryData r
    , "topics" .= _logEntryTopics r
    ]
{-# INLINE logEntryProperties #-}
{-# SPECIALIZE logEntryProperties :: LogEntry -> [Series] #-}
{-# SPECIALIZE logEntryProperties :: LogEntry -> [Pair] #-}

-- -------------------------------------------------------------------------- --
-- JSON RPC Log Entries

newtype TransactionIndex = TransactionIndex Natural
    deriving (Show, Eq, Ord)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

data RpcLogEntry = RpcLogEntry
    { _rpcLogEntryAddress :: !Address
        -- ^ 20 Bytes - address from which this log originated.
    , _rpcLogEntryTopics :: ![LogTopic]
        -- ^ Array of 0 to 4 32 Bytes of indexed log arguments. (In solidity: The first topic is the
        -- hash of the signature of the event (e.g. Deposit(address,bytes32,uint256)), except you
        -- declared the event with the anonymous specifier.)
    , _rpcLogEntryData :: !LogData
        -- ^ contains one or more 32 Bytes non-indexed arguments of the log.
    , _rpcLogEntryBlockHash :: !BlockHash
        -- ^ 32 Bytes - hash of the block where this log was in. null when its pending. null when
        -- its pending log.
    , _rpcLogEntryBlockNumber :: !BlockNumber
        -- ^ the block number where this log was in. null when its pending. null when its pending
        -- log.
    , _rpcLogEntryLogIndex :: !TransactionIndex
        -- ^ integer of the log index position in the block. null when its pending log.
    , _rpcLogEntryRemoved :: !Bool
        -- ^ true when the log was removed, due to a chain reorganization. false if it's a valid
        -- log.
    , _rpcLogEntryTransactionHash :: !TransactionHash
        -- ^ 32 Bytes - hash of the transactions this log was created from. null when its pending
        -- log.
    , _rpcLogEntryTransactionIndex :: !TransactionIndex
        -- ^ integer of the transactions index position log was created from. null when its pending
        -- log.
    }
    deriving (Eq, Show)

fromRpcLogEntry :: RpcLogEntry -> LogEntry
fromRpcLogEntry rpc = LogEntry
    { _logEntryAddress = _rpcLogEntryAddress rpc
    , _logEntryTopics = _rpcLogEntryTopics rpc
    , _logEntryData = _rpcLogEntryData rpc
    }

instance ToJSON RpcLogEntry where
    toEncoding = pairs . mconcat . rpcLogEntryProperties
    toJSON = object . rpcLogEntryProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON RpcLogEntry where
    parseJSON = withObject "RpcLogEntry" $ \o -> RpcLogEntry
        <$> o .: "address"
        <*> o .: "topics"
        <*> o .: "data"
        <*> o .: "blockHash"
        <*> o .: "blockNumber"
        <*> o .: "logIndex"
        <*> o .: "removed"
        <*> o .: "transactionHash"
        <*> o .: "transactionIndex"
    {-# INLINE parseJSON #-}

rpcLogEntryProperties :: KeyValue e kv => RpcLogEntry -> [kv]
rpcLogEntryProperties r =
    [ "address" .= _rpcLogEntryAddress r
    , "blockHash" .= _rpcLogEntryBlockHash r
    , "blockNumber" .= _rpcLogEntryBlockNumber r
    , "data" .= _rpcLogEntryData r
    , "logIndex" .= _rpcLogEntryLogIndex r
    , "removed" .= _rpcLogEntryRemoved r
    , "topics" .= _rpcLogEntryTopics r
    , "transactionHash" .= _rpcLogEntryTransactionHash r
    , "transactionIndex" .= _rpcLogEntryTransactionIndex r
    ]
{-# INLINE rpcLogEntryProperties #-}
{-# SPECIALIZE rpcLogEntryProperties :: RpcLogEntry -> [Series] #-}
{-# SPECIALIZE rpcLogEntryProperties :: RpcLogEntry -> [Pair] #-}

-- -------------------------------------------------------------------------- --
-- Tx Status

newtype TxStatus = TxStatus Natural
    deriving (Show, Eq)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

-- | This is the instance used in RLP encodings for Receipts in the Receipt
-- Merkle tree for computing the receipt root in Consensus Headers.
--
-- The Yellow paper doesn't specify how the tx status is encoded in the
-- RLP encoding of receipts. This encoding is derived from
-- <https://github.com/ethereum/go-ethereum/blob/cf856ea1ad96ac39ea477087822479b63417036a/core/types/receipt.go#L36>
--
instance RLP TxStatus where
    putRlp (TxStatus 1) = putRlp @B.ByteString "\x01"
    putRlp (TxStatus 0) = putRlp @B.ByteString ""
    putRlp x = error $ "unsupported tx status: " <> show x

    getRlp = label "TxStatus" $ getRlp @B.ByteString >>= \case
        "\x01" -> return $ TxStatus 1
        "" -> return $ TxStatus 0
        x -> fail $ "unsupported tx status: " <> show x

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- -------------------------------------------------------------------------- --
-- Transaction Type

-- | Ethereum Transaction Types:
--
data TransactionType
    = LegacyTransaction
        -- ^ Legacy Transactions.
        --
        -- The RLP encoding for legacy receipts is @rlp([status,
        -- cumulativeGasUsed, logsBloom, logs])@. This binary format is used as
        -- value in the trie for the receipt root hash.
        --
        -- cf.  https://eips.ethereum.org/EIPS/eip-2718
    | Eip2930Transaction
        -- ^ EIP-2930: 0x01
        --
        --   The EIP-2718 ReceiptPayload for this transaction is @rlp([status,
        --   cumulativeGasUsed, logsBloom, logs])@.
        --
        -- cf. https://eips.ethereum.org/EIPS/eip-2930
    | Eip1559Transaction
        -- ^ EIP-1559: 0x02
        --
        --   The EIP-2718 ReceiptPayload for this transaction is @rlp([status,
        --   cumulative_transaction_gas_used, logs_bloom, logs])@.
        --
        -- cf. https://eips.ethereum.org/EIPS/eip-1559
    | Eip4844Transaction
        -- ^ EIP-4844: 0x03
        --
        --   The EIP-2718 ReceiptPayload for this transaction is @rlp([status,
        --   cumulative_transaction_gas_used, logs_bloom, logs])@.
        --
        -- cf. https://eips.ethereum.org/EIPS/eip-4844
    deriving (Show, Eq, Ord, Enum, Bounded)

instance ToJSON TransactionType where
    toJSON = toJSON . HexQuantity . int @_ @Natural . fromEnum
    toEncoding = toEncoding . HexQuantity . int @_ @Natural . fromEnum
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON TransactionType where
    parseJSON v = do
        HexQuantity x <- parseJSON v
        case (x :: Natural) of
            0x00 -> return LegacyTransaction
            0x01 -> return Eip2930Transaction
            0x02 -> return Eip1559Transaction
            0x03 -> return Eip4844Transaction
            _ -> fail $ "invalid transaction type: " <> show x
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Receipt

-- | Receipt
--
-- The encoding that is used in the trie for computing the receipt root hash is
-- as follows:
--
-- https://eips.ethereum.org/EIPS/eip-2718:
--
-- @
-- (TransactionType || ReceiptPayload) or LegacyReceipt
-- @
--
-- where
--
-- - TransactionType is a positive unsigned 8-bit number between 0 and 0x7f that
--   represents the type of the transaction
-- - ReceiptPayload is an opaque byte array whose interpretation is dependent on
--   the TransactionType and defined in future EIPs
-- - LegacyReceipt is @rlp([status, cumulativeGasUsed, logsBloom, logs])@

-- -------------------------------------------------------------------------- --
-- Receipt Payload

-- | According to EIP-2718 a ReceiptPayload is an opaque byte array whose
-- interpretation is dependent on the TransactionType and defined in future
-- EIPs.
--
-- However, up to the Cancun fork the ReceiptPayload are the same for all
-- transaction types.
--
data ReceiptPayload = ReceiptPayload
    { _receiptPayloadStatus :: !TxStatus
        -- ^ Status code of the transaction
        --
        -- A non-negative integer

    , _receiptPayloadGasUsed :: !GasUsed
        -- ^ Gas used in block up to and including this tx.
        --
        -- A non-negative integer value

    , _receiptPayloadBloom :: !Bloom
        -- ^ Bloomfilter of the logs
        --
        -- A 2048 bit (256 bytes) hash value

    , _receiptPayloadLogs :: ![LogEntry]
        -- ^ Logs that are created during execution of the tx
        --
        -- The sequence Rl is a series of log entries
    }
    deriving (Show, Eq)

-- The RLP encoding for Receipt Payloads accodring to EIP-2718, EIP-2930,
-- EIP-1559 and EIP-4844.
--
-- @
-- rlp([status, cumulativeGasUsed, logsBloom, logs])
-- @
--
-- cf. https://eips.ethereum.org/EIPS/eip-2718:
--
instance RLP ReceiptPayload where
    putRlp r = putRlpL
        [ putRlp $! _receiptPayloadStatus r
        , putRlp $! _receiptPayloadGasUsed r
        , putRlp $! _receiptPayloadBloom r
        , putRlp $! _receiptPayloadLogs r
        ]

    getRlp = label "ReceiptPayload" $ getRlpL $ ReceiptPayload
        <$> getRlp -- status
        <*> getRlp -- gas used
        <*> getRlp -- bloom
        <*> getRlp -- logs
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- | Receipt
--
data Receipt
    = Receipt
        { _receiptType :: !TransactionType
            -- ^ Transaction type
            --
        , _receiptPayload :: !ReceiptPayload
            -- ^ As of the Cancun for receipts payloads are the same for all
            -- transaction types. If this changes in the future, the Receipt
            -- type should probably use different constructors for different
            -- transaction types.
        }
    deriving (Show, Eq)

-- | Status code of the transaction
--
-- A non-negative integer
_receiptStatus :: Receipt -> TxStatus
_receiptStatus = _receiptPayloadStatus . _receiptPayload

-- | Gas used in block up to and including this tx.
--
-- A non-negative integer value
_receiptGasUsed :: Receipt -> GasUsed
_receiptGasUsed = _receiptPayloadGasUsed . _receiptPayload

-- | Bloomfilter of the logs
--
-- A 2048 bit (256 bytes) hash value
_receiptBloom :: Receipt -> Bloom
_receiptBloom = _receiptPayloadBloom . _receiptPayload

-- | Logs that are created during execution of the tx
--
-- The sequence Rl is a series of log entries
_receiptLogs :: Receipt -> [LogEntry]
_receiptLogs = _receiptPayloadLogs . _receiptPayload

-- | RLP encodings of receipts are used inconsistently in different contexts
-- based on the transaction type.
--
instance RLP Receipt where
    putRlp r = case _receiptType r of
        LegacyTransaction -> putRlp (_receiptPayload r)
        t -> putRlp
            $ B.singleton (int $ fromEnum t)
            <> putByteString (putRlp (_receiptPayload r))

    -- This is sound because the RLP encodings for a list (legacy txs) and
    -- a bytestring (EIP-2718 txs) differe at the first byte. This also means
    -- the first branch of the alternative fails fast.
    getRlp = label "Receipt" $ getLegacyTx <|> getTx
      where
        getLegacyTx = label "LegacyTransaction" $ Receipt
            <$> pure LegacyTransaction
            <*> getRlp @ReceiptPayload
        getTx = do
            b <- getRlp @B.ByteString
            case B.uncons b of
                Just (0x01, r) -> label "Eip2930Transaction" $ Receipt
                    <$> pure Eip2930Transaction
                    <*> case get (getRlp @ReceiptPayload) r of
                        Left e -> fail $ "Eip2930Transaction: " <> show e
                        Right r' -> return r'
                Just (0x02, r) -> label "Eip1559Transaction" $ Receipt
                    <$> pure Eip1559Transaction
                    <*> case get (getRlp @ReceiptPayload) r of
                        Left e -> fail $ "Eip2930Transaction: " <> show e
                        Right r' -> return r'
                Just (0x03, r) -> label "Eip4844Transaction" $ Receipt
                    <$> pure Eip4844Transaction
                    <*> case get (getRlp @ReceiptPayload) r of
                        Left e -> fail $ "Eip2930Transaction: " <> show e
                        Right r' -> return r'
                Just (x, _) -> fail $
                    "invalid transaction type: expected 0x01, 0x02 or 0x03, but got: " <> sshow x
                Nothing -> fail "invalid receipt encoding"
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

encodeReceipt :: Receipt -> B.ByteString
encodeReceipt r = case _receiptType r of
    LegacyTransaction -> putByteString $ putRlp (_receiptPayload r)
    t -> B.singleton (int $ fromEnum t) <> putByteString (putRlp (_receiptPayload r))

-- instance ToJSON Receipt where
--     toEncoding = pairs . mconcat . receiptProperties
--     toJSON = object . receiptProperties
--     {-# INLINE toEncoding #-}
--     {-# INLINE toJSON #-}
--
-- instance FromJSON Receipt where
--     parseJSON = withObject "Receipt" $ \o -> Receipt
--         <$> o .: "status"
--         <*> o .: "cumulativeGasUsed"
--         <*> o .: "bloom"
--         <*> o .: "logs"
--     {-# INLINE parseJSON #-}
--
-- receiptProperties :: KeyValue e kv => Receipt -> [kv]
-- receiptProperties o =
--     [ "status" .= _receiptStatus o
--     , "cumulativeGasUsed" .= _receiptGasUsed o
--     , "bloom" .= _receiptBloom o
--     , "logs" .= _receiptLogs o
--     ]
-- {-# INLINE receiptProperties #-}
-- {-# SPECIALIZE receiptProperties :: Receipt -> [Series] #-}
-- {-# SPECIALIZE receiptProperties :: Receipt -> [Pair] #-}

deriving via (RlpMerkleLogEntry 'EthReceiptsRootTag ReceiptsRoot)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ReceiptsRoot

deriving via (RlpMerkleLogEntry 'EthReceiptTag Receipt)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Receipt

-- -------------------------------------------------------------------------- --
-- JSON RPC API Receipts

data RpcReceipt = RpcReceipt
    { _rpcReceiptType :: !TransactionType
        -- ^ Transaction type
    , _rpcReceiptGasUsed :: !GasUsed
        -- ^ the amount of gas used by this specific transaction alone.
    , _rpcReceiptBloom :: !Bloom
        -- ^ 256 Bytes - Bloom filter for light clients to quickly retrieve related logs.
    , _rpcReceiptLogs :: ![RpcLogEntry]
        -- ^ Array - Array of log objects, which this transaction generated.
    , _rpcReceiptStatus :: !TxStatus
        -- ^ Status code of the transaction, either 1 (success) or 0 (failure)
        --
        -- For pre Byzantium this is "root", 32 bytes of post-transaction stateroot

    , _rpcReceiptBlockHash :: !BlockHash
        -- ^ 32 Bytes - hash of the block where this transaction was in.
    , _rpcReceiptBlockNumber :: !BlockNumber
        -- ^ block number where this transaction was in.
    , _rpcReceiptContractAddress :: !(Maybe Address)
        -- ^ 20 Bytes - the contract address created, if the transaction was a contract creation, otherwise - null.
    , _rpcReceiptCumulativeGasUsed :: !GasUsed
        -- ^ the total amount of gas used when this transaction was executed in the block.
    , _rpcReceiptFrom :: !Address
        -- ^ 20 Bytes - address of the sender.
    , _rpcReceiptTo :: !(Maybe Address)
        -- ^ 20 Bytes - address of the receiver. Null when the transaction is a contract creation transaction.
    , _rpcReceiptTransactionHash :: !TransactionHash
        -- ^ 32 Bytes - hash of the transaction.
    , _rpcReceiptTransactionIndex :: !TransactionIndex
        -- ^ integer of the transactions index position in the block.
    }
    deriving (Eq, Show)

fromRpcReceipt :: RpcReceipt -> Receipt
fromRpcReceipt rpc = Receipt
    { _receiptType = _rpcReceiptType rpc
    , _receiptPayload = ReceiptPayload
        { _receiptPayloadStatus = _rpcReceiptStatus rpc
        -- , _receiptPayloadGasUsed = _rpcReceiptGasUsed rpc
        , _receiptPayloadGasUsed = _rpcReceiptCumulativeGasUsed rpc
            -- this comes as a surprise, but seems to be required for computing the correct
            -- receipt root in the consensus header.
        , _receiptPayloadBloom = _rpcReceiptBloom rpc
        , _receiptPayloadLogs = fromRpcLogEntry <$> _rpcReceiptLogs rpc
        }
    }

instance ToJSON RpcReceipt where
    toEncoding = pairs . mconcat . rpcReceiptProperties
    toJSON = object . rpcReceiptProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON RpcReceipt where
    parseJSON = withObject "RpcReceipt" $ \o -> RpcReceipt
        <$> (o .:? "type" .!= LegacyTransaction)
        <*> o .: "gasUsed"
        <*> o .: "logsBloom"
        <*> o .: "logs"
        <*> o .: "status"
        <*> o .: "blockHash"
        <*> o .: "blockNumber"
        <*> o .: "contractAddress"
        <*> o .: "cumulativeGasUsed"
        <*> o .: "from"
        <*> o .: "to"
        <*> o .: "transactionHash"
        <*> o .: "transactionIndex"
    {-# INLINE parseJSON #-}

rpcReceiptProperties :: KeyValue e kv => RpcReceipt -> [kv]
rpcReceiptProperties r =
    [ "type" .= _rpcReceiptType r
    , "blockHash" .= _rpcReceiptBlockHash r
    , "blockNumber" .= _rpcReceiptBlockNumber r
    , "contractAddress" .= _rpcReceiptContractAddress r
    , "cumulativeGasUsed" .= _rpcReceiptCumulativeGasUsed r
    , "from" .= _rpcReceiptFrom r
    , "gasUsed" .= _rpcReceiptGasUsed r
    , "logs" .= _rpcReceiptLogs r
    , "logsBloom" .= _rpcReceiptBloom r
    , "status" .= _rpcReceiptStatus r
    , "to" .= _rpcReceiptTo r
    , "transactionHash" .= _rpcReceiptTransactionHash r
    , "transactionIndex" .= _rpcReceiptTransactionIndex r
    ]
{-# INLINE rpcReceiptProperties #-}
{-# SPECIALIZE rpcReceiptProperties :: RpcReceipt -> [Series] #-}
{-# SPECIALIZE rpcReceiptProperties :: RpcReceipt -> [Pair] #-}

-- -------------------------------------------------------------------------- --
--

rpcReceiptTrieProof
    :: [RpcReceipt]
    -> TransactionIndex
    -> Proof
rpcReceiptTrieProof rs = receiptTrieProof kv
  where
    kv = (\x -> (_rpcReceiptTransactionIndex x, fromRpcReceipt x)) <$> rs

receiptTrieProof
    :: [(TransactionIndex, Receipt)]
        -- ^ block receipts
    -> TransactionIndex
    -> Proof
receiptTrieProof receipts idx = createProof kv (putRlpByteString idx)
  where
    kv = bimap putRlpByteString encodeReceipt <$> receipts
