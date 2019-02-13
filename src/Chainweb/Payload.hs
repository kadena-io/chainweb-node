{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Payload
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Payload
(
-- * Block Chain Data

  Transaction(..)
, TransactionOutput(..)

-- * Hashes

, BlockPayloadHash(..)
, encodeBlockPayloadHash
, decodeBlockPayloadHash
, BlockTransactionsHash(..)
, encodeBlockTransactionsHash
, decodeBlockTransactionsHash
, BlockOutputsHash(..)
, encodeBlockOutputsHash
, decodeBlockOutputsHash

-- * Authorative Data

, BlockPayload(..)
, BlockTransactions(..)

-- * Redundate Data / Caches

, BlockOutputs(..)
, TransactionTree(..)
, OutputTree(..)

-- * Create Data
, BlockTransactionsLog
, newTransactionLog
, newBlockTransactions
, transactionLog

, BlockOutputsLog
, newBlockOutputLog
, newBlockOutputs
, blockOutputLog

, blockPayload

-- * API Payload Data
, PayloadData(..)
) where

import Control.DeepSeq
import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Aeson
import qualified Data.ByteArray as BA
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import Data.Hashable
import Data.MerkleLog
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Void

import GHC.Generics

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Utils

import Data.CAS

-- -------------------------------------------------------------------------- --
-- Block Transactions Hash

newtype BlockTransactionsHash = BlockTransactionsHash MerkleLogHash
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess)
    deriving newtype (Hashable, ToJSON, FromJSON)

encodeBlockTransactionsHash :: MonadPut m => BlockTransactionsHash -> m ()
encodeBlockTransactionsHash (BlockTransactionsHash w) = encodeMerkleLogHash w

decodeBlockTransactionsHash :: MonadGet m => m BlockTransactionsHash
decodeBlockTransactionsHash = BlockTransactionsHash <$> decodeMerkleLogHash

instance IsMerkleLogEntry ChainwebHashTag BlockTransactionsHash where
    type Tag BlockTransactionsHash = 'BlockTransactionsHashTag
    toMerkleNode = encodeMerkleInputNode encodeBlockTransactionsHash
    fromMerkleNode = decodeMerkleInputNode decodeBlockTransactionsHash
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- -------------------------------------------------------------------------- --
-- Block Outputs Hash

newtype BlockOutputsHash = BlockOutputsHash MerkleLogHash
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess)
    deriving newtype (Hashable, ToJSON, FromJSON)

encodeBlockOutputsHash :: MonadPut m => BlockOutputsHash -> m ()
encodeBlockOutputsHash (BlockOutputsHash w) = encodeMerkleLogHash w

decodeBlockOutputsHash :: MonadGet m => m BlockOutputsHash
decodeBlockOutputsHash = BlockOutputsHash <$> decodeMerkleLogHash

instance IsMerkleLogEntry ChainwebHashTag BlockOutputsHash where
    type Tag BlockOutputsHash = 'BlockOutputsHashTag
    toMerkleNode = encodeMerkleInputNode encodeBlockOutputsHash
    fromMerkleNode = decodeMerkleInputNode decodeBlockOutputsHash
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- -------------------------------------------------------------------------- --
-- BlockPayloadHash

newtype BlockPayloadHash = BlockPayloadHash MerkleLogHash
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess)
    deriving newtype (Hashable, ToJSON, FromJSON)

encodeBlockPayloadHash :: MonadPut m => BlockPayloadHash -> m ()
encodeBlockPayloadHash (BlockPayloadHash w) = encodeMerkleLogHash w

decodeBlockPayloadHash :: MonadGet m => m BlockPayloadHash
decodeBlockPayloadHash = BlockPayloadHash <$> decodeMerkleLogHash

instance IsMerkleLogEntry ChainwebHashTag BlockPayloadHash where
    type Tag BlockPayloadHash = 'BlockPayloadHashTag
    toMerkleNode = encodeMerkleInputNode encodeBlockPayloadHash
    fromMerkleNode = decodeMerkleInputNode decodeBlockPayloadHash
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- -------------------------------------------------------------------------- --
-- Transaction

-- | An encoded transaction, including all of its inputs.
--
-- We don't care about the encoding of an transaction. The semantics of a
-- transaction is only known to pact.
--
newtype Transaction = Transaction { _transactionBytes :: B.ByteString }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess, Hashable)

instance Show Transaction where
    show = T.unpack . encodeToText
    {-# INLINE show #-}

instance ToJSON Transaction where
    toJSON = toJSON . encodeB64UrlNoPaddingText . _transactionBytes
    {-# INLINE toJSON #-}

instance FromJSON Transaction where
    parseJSON = parseJsonFromText "Transaction"
    {-# INLINE parseJSON #-}

instance IsMerkleLogEntry ChainwebHashTag Transaction where
    type Tag Transaction = 'TransactionTag
    toMerkleNode = InputNode . _transactionBytes
    fromMerkleNode (InputNode bytes) = Right $ Transaction bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

transactionToText :: Transaction -> T.Text
transactionToText = encodeB64UrlNoPaddingText . _transactionBytes
{-# INLINE transactionToText #-}

transactionFromText :: MonadThrow m => T.Text -> m Transaction
transactionFromText t = either (throwM . TextFormatException . sshow) return
    $ Transaction <$> decodeB64UrlNoPaddingText t
{-# INLINE transactionFromText #-}

instance HasTextRepresentation Transaction where
    toText = transactionToText
    {-# INLINE toText #-}
    fromText = transactionFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Transaction Output

-- | Encoded output of a sinle transaction.
--
-- We don't care about the encoding of the output. The semantics of a output is
-- only known to pact.
--
newtype TransactionOutput = TransactionOutput
    { _transactionOutputBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (BA.ByteArrayAccess)

instance IsMerkleLogEntry ChainwebHashTag TransactionOutput where
    type Tag TransactionOutput = 'TransactionOutputTag
    toMerkleNode = InputNode . _transactionOutputBytes
    fromMerkleNode (InputNode bytes) = Right $ TransactionOutput bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

transactionOutputToText :: TransactionOutput -> T.Text
transactionOutputToText = encodeB64UrlNoPaddingText . _transactionOutputBytes
{-# INLINE transactionOutputToText #-}

transactionOutputFromText :: MonadThrow m => T.Text -> m TransactionOutput
transactionOutputFromText t = either (throwM . TextFormatException . sshow) return
    $ TransactionOutput <$> decodeB64UrlNoPaddingText t
{-# INLINE transactionOutputFromText #-}

instance HasTextRepresentation TransactionOutput where
    toText = transactionOutputToText
    {-# INLINE toText #-}
    fromText = transactionOutputFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Block Payloads

-- | The Payload of a block.
--
-- The transactions of a block at a given height in the chain are discovered by
-- @_blockPayloadTransactionsHash . _blockHeaderPayloadHash@.
--
-- NOTES:
--
-- The block and it's output is uniquely determined by the transactions.
-- However, we want to include the output hash in the payload hash as a checksum
-- and for inclusion proofs. In order to validate the payload hash without
-- recomputing the outputs we need this extra level of indirection between the
-- BlockHeaders and the Transactions.
--
-- This structure could be recomputed from the block transactions only if it
-- would be possible to discover the order of blocks from the block transactions
-- structure which isn't the case. Instead the order of the blocks is discovered
-- from the 'BlockHeader' chain and block transactions for a given block height
-- are looked up by @_blockPayloadTransactionsHash . _blockPayloadHash@.
--
data BlockPayload = BlockPayload
    { _blockPayloadHash :: !BlockPayloadHash
        -- ^ Hash of '_blockPayloadTransactionsHash' and '_blockPayloadTransactionsHash'.
        -- Primary key of 'BlockPayloadStore'.

    , _blockPayloadTransactionsHash :: !BlockTransactionsHash
        -- ^ Root of 'TransactionTree' of the block. Foreign key into
        -- 'BlockTransactionsStore' and 'TransactionTreeStore'.

    , _blockPayloadOutputsHash :: !BlockOutputsHash
        -- ^ Root of 'OutputsTree' of the block. Foreign key into
        -- 'BlockOutputsStore' and 'OutputTreeStore'.
    }
    deriving (Show)

instance IsCasValue BlockPayload where
    type CasKeyType BlockPayload = BlockPayloadHash
    casKey = _blockPayloadHash

instance HasMerkleLog ChainwebHashTag BlockPayload where
    type MerkleLogHeader BlockPayload = '[BlockTransactionsHash, BlockOutputsHash]
    type MerkleLogBody BlockPayload = Void

    toLog a = merkleLog root entries
      where
        BlockPayloadHash (MerkleLogHash root) = _blockPayloadHash a
        entries = _blockPayloadTransactionsHash a
            :+: _blockPayloadOutputsHash a
            :+: emptyBody

    fromLog l = BlockPayload
        { _blockPayloadHash = BlockPayloadHash $ MerkleLogHash $ _merkleLogRoot l
        , _blockPayloadTransactionsHash = txHash
        , _blockPayloadOutputsHash = outHash
        }
      where
        (txHash :+: outHash :+: _) = _merkleLogEntries l

-- -------------------------------------------------------------------------- --
-- Block Transactions

-- | The block transactions
--
data BlockTransactions = BlockTransactions
    { _blockTransactionsHash :: !BlockTransactionsHash
        -- ^ Root of 'TransactionTree' of the block. Primary key of
        -- 'BlockTransactionsStore'. Foreign key into 'TransactionTreeStore'.

    , _blockTransactions :: !(S.Seq Transaction)
        -- ^ Ordered list of all transactions of the block.
    }
    deriving (Show)

instance IsCasValue BlockTransactions where
    type CasKeyType BlockTransactions = BlockTransactionsHash
    casKey = _blockTransactionsHash

instance HasMerkleLog ChainwebHashTag BlockTransactions where
    type MerkleLogHeader BlockTransactions = '[]
    type MerkleLogBody BlockTransactions = Transaction

    toLog a = merkleLog root entries
      where
        BlockTransactionsHash (MerkleLogHash root) = _blockTransactionsHash a
        entries = MerkleLogBody (_blockTransactions a)

    fromLog l = BlockTransactions
        { _blockTransactionsHash = BlockTransactionsHash $ MerkleLogHash $ _merkleLogRoot l
        , _blockTransactions = txs
        }
      where
        MerkleLogBody txs = _merkleLogEntries l

type BlockTransactionsLog = MkLogType ChainwebHashTag BlockTransactions

-- -------------------------------------------------------------------------- --
-- Redundant / Caches
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Block Outputs

-- | All outputs of the transactions of a block.
--
-- NOTE: the block outputs are associated with the respective block in the
-- 'BlockPayload' structure for the block.
--
data BlockOutputs = BlockOutputs
    { _blockOutputsHash :: !BlockOutputsHash
        -- ^ Root of 'OutputTree' of the block. Primary key of
        -- 'BlockOutputsStore'. Foreign key into 'OutputTreeStore'.

    , _blockOutputs :: !(S.Seq TransactionOutput)
        -- ^ Output of all transactions of a block in the order of the
        -- transactions in the block.
    }
    deriving (Show)

instance IsCasValue BlockOutputs where
    type CasKeyType BlockOutputs = BlockOutputsHash
    casKey = _blockOutputsHash

instance HasMerkleLog ChainwebHashTag BlockOutputs where
    type MerkleLogHeader BlockOutputs = '[]
    type MerkleLogBody BlockOutputs = TransactionOutput

    toLog a = merkleLog root entries
      where
        BlockOutputsHash (MerkleLogHash root) = _blockOutputsHash a
        entries = MerkleLogBody (_blockOutputs a)

    fromLog l = BlockOutputs
        { _blockOutputsHash = BlockOutputsHash $ MerkleLogHash $ _merkleLogRoot l
        , _blockOutputs = outs
        }
      where
        MerkleLogBody outs = _merkleLogEntries l

type BlockOutputsLog = MkLogType ChainwebHashTag BlockOutputs

-- -------------------------------------------------------------------------- --
-- Transaction Merkle Tree

-- | Merkle tree for the transactions in a block.
--
data TransactionTree = TransactionTree
    { _transactionTreeHash :: !BlockTransactionsHash
        -- ^ Root of '_transactionTree'. Primary key of 'TransactionTreeStore.
        -- Foreign key into 'BlockTransactionsStore'.

    , _transactionTree :: !(MerkleTree SHA512t_256)
    }
    deriving (Show)

instance IsCasValue TransactionTree where
    type CasKeyType TransactionTree = BlockTransactionsHash
    casKey = _transactionTreeHash

-- -------------------------------------------------------------------------- --
-- Output Merkle Tree

-- | Merkle Tree for transaction outputs of a block.
--
data OutputTree = OutputTree
    { _outputTreeHash :: !BlockOutputsHash
        -- ^ Root of '_outputTree'. Primary key of 'OutputTreeStore. Foreign key
        -- into 'BlockOutputsStore'.

    , _outputTree :: !(MerkleTree SHA512t_256)
    }
    deriving (Show)

instance IsCasValue OutputTree where
    type CasKeyType OutputTree = BlockOutputsHash
    casKey = _outputTreeHash

-- -------------------------------------------------------------------------- --
-- Data Creation
-- -------------------------------------------------------------------------- --

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newTransactionLog :: S.Seq Transaction -> BlockTransactionsLog
newTransactionLog = newMerkleLog . MerkleLogBody

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockTransactions :: S.Seq Transaction -> (TransactionTree, BlockTransactions)
newBlockTransactions txs = (tree, blockTxs)
  where
    mlog = newTransactionLog txs
    blockTxs = fromLog mlog
    tree = TransactionTree
        { _transactionTreeHash = _blockTransactionsHash blockTxs
        , _transactionTree = _merkleLogTree mlog
        }

-- | Create a MerkleLog from a 'BlockTransactions' and a cached
-- 'TransactionTree'.
--
-- It is an error if the 'TransactionTree' doesn't match the
-- 'BlockTransactions'.
--
-- If you only have the 'BlockTransactions' you can use 'toLog' in which case
-- the '_merkleLogTree' will be instantiated lazily.
--
transactionLog
    :: BlockTransactions
    -> TransactionTree
    -> BlockTransactionsLog
transactionLog txs tree
    | _blockTransactionsHash txs == _transactionTreeHash tree
        = (toLog txs) { _merkleLogTree = _transactionTree tree }
    | otherwise = error "Transaction tree and block transactions don't match"

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockOutputLog :: S.Seq TransactionOutput -> BlockOutputsLog
newBlockOutputLog = newMerkleLog . MerkleLogBody

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockOutputs :: S.Seq TransactionOutput -> (OutputTree, BlockOutputs)
newBlockOutputs outs = (tree, blkOuts)
  where
    mlog = newBlockOutputLog outs
    blkOuts = fromLog mlog
    tree = OutputTree
        { _outputTreeHash = _blockOutputsHash blkOuts
        , _outputTree = _merkleLogTree mlog
        }

-- | Create a MerkleLog from 'BlockOutputs' and a cached 'OutputTree'.
--
-- It is an error if the 'OutputTree' doesn't match the 'BlockOutputs'.
--
-- If you only have the 'BlockOutputs' you can use 'toLog' in which case the
-- '_merkleLogTree' will be instantiated lazily.
--
blockOutputLog
    :: BlockOutputs
    -> OutputTree
    -> BlockOutputsLog
blockOutputLog outs tree
    | _blockOutputsHash outs == _outputTreeHash tree
        = (toLog outs) { _merkleLogTree = _outputTree tree }
    | otherwise = error "Output tree and block outputs don't match"

-- |
--
-- Note, that forcing the 'MerkleTree' is cheap for 'BlockPayload'.
--
blockPayload :: BlockTransactions -> BlockOutputs -> BlockPayload
blockPayload txs outs
    = fromLog $ newMerkleLog @ChainwebHashTag
        $ _blockTransactionsHash txs
        :+: _blockOutputsHash outs
        :+: emptyBody

-- -------------------------------------------------------------------------- --
-- Payload Data

data PayloadData = PayloadData
    { _payloadDataTransactions :: !(S.Seq Transaction)
    , _payloadDataPayloadHash :: !BlockPayloadHash
    , _payloadDataTransactionsHash :: !BlockTransactionsHash
    , _payloadDataOutputsHash :: !BlockOutputsHash
    }
    deriving (Show, Generic)

instance ToJSON PayloadData where
    toJSON o = object
        [ "transactions" .= _payloadDataTransactions o
        , "payloadHash" .= _payloadDataPayloadHash o
        , "transactionsHash" .= _payloadDataTransactionsHash o
        , "outputsHash" .= _payloadDataOutputsHash o
        ]

instance FromJSON PayloadData where
    parseJSON = withObject "PayloadData" $ \o -> PayloadData
        <$> o .: "transactions"
        <*> o .: "payloadHash"
        <*> o .: "transactionsHash"
        <*> o .: "outputsHash"

