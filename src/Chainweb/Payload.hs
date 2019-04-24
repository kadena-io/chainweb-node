{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO KeySet NFData in Pact
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- * Authoritative Data

, BlockPayload(..)
, BlockTransactions(..)

-- * Redundant Data / Caches

, BlockOutputs(..)
, TransactionTree(..)
, OutputTree(..)

-- * Create Data
, BlockTransactionsLog
, newTransactionLog
, newBlockTransactions
, transactionLog

, MinerData(..)
, CoinbaseOutput(..)

, BlockOutputsLog
, newBlockOutputLog
, newBlockOutputs
, blockOutputLog

, blockPayload
, newBlockPayload

-- * API Payload Data
, PayloadData(..)
, payloadData
, newPayloadData

-- * All Payload Data in a Single Structure
, PayloadWithOutputs(..)
, payloadWithOutputs
, newPayloadWithOutputs
, payloadWithOutputsToBlockObjects
, payloadWithOutputsToPayloadData
) where

import Control.DeepSeq
import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Aeson
import qualified Data.Aeson.Types as A
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
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
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
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
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
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- -------------------------------------------------------------------------- --
-- Transaction

-- | An encoded transaction, including all of its inputs.
--
-- We don't care about the encoding of a transaction. The semantics of a
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

-- | Encoded output of a single transaction.
--
-- We don't care about the encoding of the output. The semantics of a output is
-- only known to pact.
--
newtype TransactionOutput = TransactionOutput
    { _transactionOutputBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (BA.ByteArrayAccess)

instance ToJSON TransactionOutput where
    toJSON = toJSON . encodeB64UrlNoPaddingText . _transactionOutputBytes
    {-# INLINE toJSON #-}

instance FromJSON TransactionOutput where
    parseJSON = parseJsonFromText "TransactionOutput"
    {-# INLINE parseJSON #-}

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
-- The block and its output are uniquely determined by the transactions.
-- However, we want to include the output hash in the payload hash as a checksum
-- and for inclusion proofs. In order to validate the payload hash without
-- recomputing the outputs we need this extra level of indirection between the
-- BlockHeaders and the Transactions.
--
-- This structure could be recomputed from the block transactions only if it
-- would be possible to discover the order of blocks from the block transactions
-- structure which isn't the case. Instead the order of the blocks is discovered
-- from the 'BlockHeader' chain and block transactions for a given block height
-- are looked up by @_blockPayloadTransactionsHash . _blockPayloadPayloadHash@.
--
data BlockPayload = BlockPayload
    { _blockPayloadPayloadHash :: !BlockPayloadHash
        -- ^ Hash of '_blockPayloadTransactionsHash' and '_blockPayloadTransactionsHash'.
        -- Primary key of 'BlockPayloadStore'.

    , _blockPayloadTransactionsHash :: !BlockTransactionsHash
        -- ^ Root of 'TransactionTree' of the block. Foreign key into
        -- 'BlockTransactionsStore' and 'TransactionTreeStore'.

    , _blockPayloadOutputsHash :: !BlockOutputsHash
        -- ^ Root of 'OutputsTree' of the block. Foreign key into
        -- 'BlockOutputsStore' and 'OutputTreeStore'.
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON BlockPayload where
    toJSON o = object
        [ "payloadHash" .= _blockPayloadPayloadHash o
        , "transactionsHash" .= _blockPayloadTransactionsHash o
        , "outputsHash" .= _blockPayloadOutputsHash o
        ]

instance FromJSON BlockPayload where
    parseJSON = withObject "BlockPayload" $ \o -> BlockPayload
        <$> o .: "payloadHash"
        <*> o .: "transactionsHash"
        <*> o .: "outputsHash"

instance IsCasValue BlockPayload where
    type CasKeyType BlockPayload = BlockPayloadHash
    casKey = _blockPayloadPayloadHash

instance HasMerkleLog ChainwebHashTag BlockPayload where
    type MerkleLogHeader BlockPayload = '[BlockTransactionsHash, BlockOutputsHash]
    type MerkleLogBody BlockPayload = Void

    toLog a = merkleLog root entries
      where
        BlockPayloadHash (MerkleLogHash root) = _blockPayloadPayloadHash a
        entries = _blockPayloadTransactionsHash a
            :+: _blockPayloadOutputsHash a
            :+: emptyBody

    fromLog l = BlockPayload
        { _blockPayloadPayloadHash = BlockPayloadHash $ MerkleLogHash $ _merkleLogRoot l
        , _blockPayloadTransactionsHash = txHash
        , _blockPayloadOutputsHash = outHash
        }
      where
        (txHash :+: outHash :+: _) = _merkleLogEntries l


-- -------------------------------------------------------------------------- --
-- Miner data as an opaque Pact-specific bytestring

newtype MinerData = MinerData { _minerData :: B.ByteString }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess, Hashable)

instance Show MinerData where
    show = T.unpack . encodeToText
    {-# INLINE show #-}

instance ToJSON MinerData where
    toJSON = toJSON . encodeB64UrlNoPaddingText . _minerData
    {-# INLINE toJSON #-}

instance FromJSON MinerData where
    parseJSON = parseJsonFromText "MinerData"
    {-# INLINE parseJSON #-}

instance IsMerkleLogEntry ChainwebHashTag MinerData where
    type Tag MinerData = 'MinerDataTag
    toMerkleNode = InputNode . _minerData
    fromMerkleNode (InputNode bytes) = Right $ MinerData bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

minerDataToText :: MinerData -> T.Text
minerDataToText = encodeB64UrlNoPaddingText . _minerData
{-# INLINE minerDataToText #-}

minerDataFromText :: MonadThrow m => T.Text -> m MinerData
minerDataFromText t = either (throwM . TextFormatException . sshow) return
    $ MinerData <$> decodeB64UrlNoPaddingText t
{-# INLINE minerDataFromText #-}

instance HasTextRepresentation MinerData where
    toText = minerDataToText
    {-# INLINE toText #-}
    fromText = minerDataFromText
    {-# INLINE fromText #-}

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

    , _blockMinerData :: !MinerData
        -- ^ Miner data for rewards
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON BlockTransactions where
    toJSON o = object
        [ "transactionHash" .= _blockTransactionsHash o
        , "transaction" .= _blockTransactions o
        , "minerData" .= _blockMinerData o
        ]

instance FromJSON BlockTransactions where
    parseJSON = withObject "BlockTransactions" $ \o -> BlockTransactions
        <$> o .: "transactionHash"
        <*> o .: "transaction"
        <*> o .: "minerData"

instance IsCasValue BlockTransactions where
    type CasKeyType BlockTransactions = BlockTransactionsHash
    casKey = _blockTransactionsHash

instance HasMerkleLog ChainwebHashTag BlockTransactions where
    type MerkleLogHeader BlockTransactions = '[MinerData]
    type MerkleLogBody BlockTransactions = Transaction

    toLog a = merkleLog root entries
      where
        BlockTransactionsHash (MerkleLogHash root) = _blockTransactionsHash a
        entries = _blockMinerData a :+: MerkleLogBody (_blockTransactions a)

    fromLog l = BlockTransactions
        { _blockTransactionsHash = BlockTransactionsHash $ MerkleLogHash $ _merkleLogRoot l
        , _blockTransactions = txs
        , _blockMinerData = mi
        }
      where
        (mi :+: MerkleLogBody txs) = _merkleLogEntries l

type BlockTransactionsLog = MkLogType ChainwebHashTag BlockTransactions

-- -------------------------------------------------------------------------- --
-- Redundant / Caches
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Coinbase transaction output

newtype CoinbaseOutput = CoinbaseOutput { _coinbaseOutput :: B.ByteString }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess, Hashable)

instance Show CoinbaseOutput where
    show = T.unpack . encodeToText
    {-# INLINE show #-}

instance ToJSON CoinbaseOutput where
    toJSON = toJSON . encodeB64UrlNoPaddingText . _coinbaseOutput
    {-# INLINE toJSON #-}

instance FromJSON CoinbaseOutput where
    parseJSON = parseJsonFromText "CoinbaseOutput"
    {-# INLINE parseJSON #-}

instance IsMerkleLogEntry ChainwebHashTag CoinbaseOutput where
    type Tag CoinbaseOutput = 'CoinbaseOutputTag
    toMerkleNode = InputNode . _coinbaseOutput
    fromMerkleNode (InputNode bytes) = Right $ CoinbaseOutput bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

coinbaseOutputToText :: CoinbaseOutput -> T.Text
coinbaseOutputToText = encodeB64UrlNoPaddingText . _coinbaseOutput
{-# INLINE coinbaseOutputToText #-}

coinbaseOutputFromText :: MonadThrow m => T.Text -> m CoinbaseOutput
coinbaseOutputFromText t = either (throwM . TextFormatException . sshow) return
    $ CoinbaseOutput <$> decodeB64UrlNoPaddingText t
{-# INLINE coinbaseOutputFromText #-}

instance HasTextRepresentation CoinbaseOutput where
    toText = coinbaseOutputToText
    {-# INLINE toText #-}
    fromText = coinbaseOutputFromText
    {-# INLINE fromText #-}


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

    , _blockCoinbaseOutput :: !CoinbaseOutput
        -- ^ Output of coinbase transaction.
    }
    deriving (Show)

instance ToJSON BlockOutputs where
    toJSON o = object
        [ "outputsHash" .= _blockOutputsHash o
        , "outputs" .= _blockOutputs o
        , "coinbaseOutput" .= _blockCoinbaseOutput o
        ]

instance FromJSON BlockOutputs where
    parseJSON = withObject "BlockOutputs" $ \o -> BlockOutputs
        <$> o .: "outputsHash"
        <*> o .: "outputs"
        <*> o .: "coinbaseOutput"

instance IsCasValue BlockOutputs where
    type CasKeyType BlockOutputs = BlockOutputsHash
    casKey = _blockOutputsHash

instance HasMerkleLog ChainwebHashTag BlockOutputs where
    type MerkleLogHeader BlockOutputs = '[CoinbaseOutput]
    type MerkleLogBody BlockOutputs = TransactionOutput

    toLog a = merkleLog root entries
      where
        BlockOutputsHash (MerkleLogHash root) = _blockOutputsHash a
        entries = _blockCoinbaseOutput a :+: MerkleLogBody (_blockOutputs a)

    fromLog l = BlockOutputs
        { _blockOutputsHash = BlockOutputsHash $ MerkleLogHash $ _merkleLogRoot l
        , _blockOutputs = outs
        , _blockCoinbaseOutput = co
        }
      where
        (co :+: MerkleLogBody outs) = _merkleLogEntries l

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

instance ToJSON TransactionTree where
    toJSON o = object
        [ "hash" .= _transactionTreeHash o
        , "tree" .= merkleTreeToJson (_transactionTree o)
        ]

instance FromJSON TransactionTree where
    parseJSON = withObject "TransactionTree" $ \o -> TransactionTree
        <$> o .: "hash"
        <*> (o .: "tree" >>= merkleTreeFromJson)

merkleTreeToJson :: MerkleTree a -> Value
merkleTreeToJson = toJSON . encodeB64UrlNoPaddingText . encodeMerkleTree

merkleTreeFromJson :: HashAlgorithm a => Value -> A.Parser (MerkleTree a)
merkleTreeFromJson = withText "MerkleTree" $ \t -> either (fail . sshow) return
    $ decodeB64UrlNoPaddingText t >>= decodeMerkleTree

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

instance ToJSON OutputTree where
    toJSON o = object
        [ "hash" .= _outputTreeHash o
        , "tree" .= merkleTreeToJson (_outputTree o)
        ]

instance FromJSON OutputTree where
    parseJSON = withObject "OutputTree" $ \o -> OutputTree
        <$> o .: "hash"
        <*> (o .: "tree" >>= merkleTreeFromJson)

-- -------------------------------------------------------------------------- --
-- Data Creation
-- -------------------------------------------------------------------------- --

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newTransactionLog :: MinerData -> S.Seq Transaction -> BlockTransactionsLog
newTransactionLog md txs =
  newMerkleLog $ md :+: MerkleLogBody txs

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockTransactions :: MinerData -> S.Seq Transaction -> (TransactionTree, BlockTransactions)
newBlockTransactions mi txs = (tree, blockTxs)
  where
    mlog = newTransactionLog mi txs
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
newBlockOutputLog :: CoinbaseOutput -> S.Seq TransactionOutput -> BlockOutputsLog
newBlockOutputLog co tos = newMerkleLog $ co :+: MerkleLogBody tos

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockOutputs :: CoinbaseOutput -> S.Seq TransactionOutput -> (OutputTree, BlockOutputs)
newBlockOutputs co outs = (tree, blkOuts)
  where
    mlog = newBlockOutputLog co outs
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

newBlockPayload
    :: MinerData
    -> CoinbaseOutput
    -> S.Seq (Transaction, TransactionOutput)
    -> BlockPayload
newBlockPayload mi co s = blockPayload txs outs
  where
    (_, txs) = newBlockTransactions mi (fst <$> s)
    (_, outs) = newBlockOutputs co (snd <$> s)

-- -------------------------------------------------------------------------- --
-- Payload Data

data PayloadData = PayloadData
    { _payloadDataTransactions :: !(S.Seq Transaction)
    , _payloadDataMiner :: !MinerData
    , _payloadDataPayloadHash :: !BlockPayloadHash
    , _payloadDataTransactionsHash :: !BlockTransactionsHash
    , _payloadDataOutputsHash :: !BlockOutputsHash
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (NFData)

instance ToJSON PayloadData where
    toJSON o = object
        [ "transactions" .= _payloadDataTransactions o
        , "minerData" .= _payloadDataMiner o
        , "payloadHash" .= _payloadDataPayloadHash o
        , "transactionsHash" .= _payloadDataTransactionsHash o
        , "outputsHash" .= _payloadDataOutputsHash o
        ]

instance FromJSON PayloadData where
    parseJSON = withObject "PayloadData" $ \o -> PayloadData
        <$> o .: "transactions"
        <*> o .: "minerData"
        <*> o .: "payloadHash"
        <*> o .: "transactionsHash"
        <*> o .: "outputsHash"

instance IsCasValue PayloadData where
    type CasKeyType PayloadData = BlockPayloadHash
    casKey = _payloadDataPayloadHash
    {-# INLINE casKey #-}

payloadData :: BlockTransactions -> BlockPayload -> PayloadData
payloadData txs payload = PayloadData
    { _payloadDataTransactions = _blockTransactions txs
    , _payloadDataMiner = _blockMinerData txs
    , _payloadDataPayloadHash = _blockPayloadPayloadHash payload
    , _payloadDataTransactionsHash = _blockPayloadTransactionsHash payload
    , _payloadDataOutputsHash = _blockPayloadOutputsHash payload
    }

newPayloadData :: BlockTransactions -> BlockOutputs -> PayloadData
newPayloadData txs outputs = payloadData txs $ blockPayload txs outputs

-- -------------------------------------------------------------------------- --
-- All Payload Data in a Single Structure

data PayloadWithOutputs = PayloadWithOutputs
    { _payloadWithOutputsTransactions :: !(S.Seq (Transaction, TransactionOutput))
    , _payloadWithOutputsMiner :: !MinerData
    , _payloadWithOutputsCoinbase :: !CoinbaseOutput
    , _payloadWithOutputsPayloadHash :: !BlockPayloadHash
    , _payloadWithOutputsTransactionsHash :: !BlockTransactionsHash
    , _payloadWithOutputsOutputsHash :: !BlockOutputsHash
    } deriving (Show)

instance IsCasValue PayloadWithOutputs where
    type CasKeyType PayloadWithOutputs = BlockPayloadHash
    casKey = _payloadWithOutputsPayloadHash
    {-# INLINE casKey #-}

payloadWithOutputs
    :: PayloadData
    -> CoinbaseOutput
    -> S.Seq TransactionOutput
    -> PayloadWithOutputs
payloadWithOutputs d co outputs = PayloadWithOutputs
    { _payloadWithOutputsTransactions = S.zip (_payloadDataTransactions d) outputs
    , _payloadWithOutputsMiner = _payloadDataMiner d
    , _payloadWithOutputsCoinbase = co
    , _payloadWithOutputsPayloadHash = _payloadDataPayloadHash d
    , _payloadWithOutputsTransactionsHash = _payloadDataTransactionsHash d
    , _payloadWithOutputsOutputsHash = _payloadDataOutputsHash d
    }

newPayloadWithOutputs
    :: MinerData
    -> CoinbaseOutput
    -> S.Seq (Transaction, TransactionOutput)
    -> PayloadWithOutputs
newPayloadWithOutputs mi co s = PayloadWithOutputs
    { _payloadWithOutputsTransactions = s
    , _payloadWithOutputsMiner = mi
    , _payloadWithOutputsCoinbase = co
    , _payloadWithOutputsPayloadHash = _blockPayloadPayloadHash p
    , _payloadWithOutputsTransactionsHash = _blockPayloadTransactionsHash p
    , _payloadWithOutputsOutputsHash = _blockPayloadOutputsHash p
    }
  where
    p = newBlockPayload mi co s

instance ToJSON PayloadWithOutputs where
    toJSON o = object
        [ "transactions" .= _payloadWithOutputsTransactions o
        , "minerData" .= _payloadWithOutputsMiner o
        , "coinbase" .= _payloadWithOutputsCoinbase o
        , "payloadHash" .= _payloadWithOutputsPayloadHash o
        , "transactionsHash" .= _payloadWithOutputsTransactionsHash o
        , "outputsHash" .= _payloadWithOutputsOutputsHash o
        ]

instance FromJSON PayloadWithOutputs where
    parseJSON = withObject "PayloadWithOutputs" $ \o -> PayloadWithOutputs
        <$> o .: "transactions"
        <*> o .: "minerData"
        <*> o .: "coinbase"
        <*> o .: "payloadHash"
        <*> o .: "transactionsHash"
        <*> o .: "outputsHash"

payloadWithOutputsToBlockObjects :: PayloadWithOutputs -> (BlockTransactions, BlockOutputs)
payloadWithOutputsToBlockObjects PayloadWithOutputs {..} =
    ( BlockTransactions _payloadWithOutputsTransactionsHash ins _payloadWithOutputsMiner
    , BlockOutputs _payloadWithOutputsOutputsHash outs _payloadWithOutputsCoinbase
    )
  where
    (ins,outs) = S.unzip $ _payloadWithOutputsTransactions

payloadWithOutputsToPayloadData :: PayloadWithOutputs -> PayloadData
payloadWithOutputsToPayloadData o = PayloadData
    { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions o
    , _payloadDataMiner = _payloadWithOutputsMiner o
    , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash o
    , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash o
    , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash o
    }

