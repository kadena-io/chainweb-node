{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
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

-- * Binary encodings
, PayloadDataList(..)
, PayloadWithOutputsList(..)
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
, encodePayloadData
, decodePayloadData
, encodePayloadWithOutputs
, decodePayloadWithOutputs
, encodePayloadDataList
, decodePayloadDataList
, encodePayloadWithOutputsList
, decodePayloadWithOutputsList

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
, newPayloadData
, payloadDataToBlockPayload
, PayloadDataCas
, verifyPayloadData
-- * Payload Data Lenses
, payloadDataTransactions
, payloadDataMiner
, payloadDataPayloadHash
, payloadDataTransactionsHash
, payloadDataOutputsHash

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

import Control.DeepSeq
import Control.Lens (Getter, to)
import Control.Monad
import Control.Monad.Catch

import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString, pair)
import qualified Data.Aeson.Types as A
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Hashable
import Data.MerkleLog
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void

import GHC.Generics (Generic)
import GHC.Stack

-- internal modules

import Chainweb.BlockPayloadHash
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse

import Chainweb.Storage.Table

import Chainweb.Utils
import Chainweb.Utils.Serialization
import Crypto.Hash.Algorithms

-- -------------------------------------------------------------------------- --
-- Block Transactions Hash

type BlockTransactionsHash = BlockTransactionsHash_ ChainwebMerkleHashAlgorithm

newtype BlockTransactionsHash_ a = BlockTransactionsHash (MerkleLogHash a)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess)
    deriving newtype (Hashable, ToJSON, FromJSON)

encodeBlockTransactionsHash :: BlockTransactionsHash_ a -> Put
encodeBlockTransactionsHash (BlockTransactionsHash w) = encodeMerkleLogHash w

decodeBlockTransactionsHash
    :: MerkleHashAlgorithm a
    => Get (BlockTransactionsHash_ a)
decodeBlockTransactionsHash = BlockTransactionsHash <$!> decodeMerkleLogHash

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag (BlockTransactionsHash_ a) where
    type Tag (BlockTransactionsHash_ a) = 'BlockTransactionsHashTag
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

instance HasTextRepresentation BlockTransactionsHash where
  toText (BlockTransactionsHash h) = toText h
  fromText = fmap BlockTransactionsHash . fromText
  {-# INLINE toText #-}
  {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Block Outputs Hash

type BlockOutputsHash = BlockOutputsHash_ ChainwebMerkleHashAlgorithm

newtype BlockOutputsHash_ a = BlockOutputsHash (MerkleLogHash a)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess)
    deriving newtype (Hashable, ToJSON, FromJSON)

encodeBlockOutputsHash :: BlockOutputsHash_ a -> Put
encodeBlockOutputsHash (BlockOutputsHash w) = encodeMerkleLogHash w

decodeBlockOutputsHash
    :: MerkleHashAlgorithm a
    => Get (BlockOutputsHash_ a)
decodeBlockOutputsHash = BlockOutputsHash <$!> decodeMerkleLogHash

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag (BlockOutputsHash_ a) where
    type Tag (BlockOutputsHash_ a) = 'BlockOutputsHashTag
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

instance HasTextRepresentation BlockOutputsHash where
  toText (BlockOutputsHash h) = toText h
  fromText = fmap BlockOutputsHash . fromText
  {-# INLINE toText #-}
  {-# INLINE fromText #-}

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
    toEncoding = b64UrlNoPaddingTextEncoding . _transactionBytes
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Transaction where
    parseJSON = parseJsonFromText "Transaction"
    {-# INLINE parseJSON #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Transaction where
    type Tag Transaction = 'TransactionTag
    toMerkleNode = InputNode . _transactionBytes
    fromMerkleNode (InputNode bytes) = Right $ Transaction bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

transactionToText :: Transaction -> T.Text
transactionToText = encodeB64UrlNoPaddingText . _transactionBytes
{-# INLINE transactionToText #-}

transactionFromText :: MonadThrow m => T.Text -> m Transaction
transactionFromText t = either (throwM . TextFormatException . sshow) return
    $ Transaction <$!> decodeB64UrlNoPaddingText t
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
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess)

instance Show TransactionOutput where
    show = T.unpack . encodeToText
    {-# INLINE show #-}

instance ToJSON TransactionOutput where
    toJSON = toJSON . encodeB64UrlNoPaddingText . _transactionOutputBytes
    toEncoding = b64UrlNoPaddingTextEncoding . _transactionOutputBytes
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON TransactionOutput where
    parseJSON = parseJsonFromText "TransactionOutput"
    {-# INLINE parseJSON #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag TransactionOutput where
    type Tag TransactionOutput = 'TransactionOutputTag
    toMerkleNode = InputNode . _transactionOutputBytes
    fromMerkleNode (InputNode bytes) = Right $ TransactionOutput bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

transactionOutputToText :: TransactionOutput -> T.Text
transactionOutputToText = encodeB64UrlNoPaddingText . _transactionOutputBytes
{-# INLINE transactionOutputToText #-}

transactionOutputFromText :: MonadThrow m => T.Text -> m TransactionOutput
transactionOutputFromText t = either (throwM . TextFormatException . sshow) return
    $ TransactionOutput <$!> decodeB64UrlNoPaddingText t
{-# INLINE transactionOutputFromText #-}

instance HasTextRepresentation TransactionOutput where
    toText = transactionOutputToText
    {-# INLINE toText #-}
    fromText = transactionOutputFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Block Payloads

type BlockPayload = BlockPayload_ ChainwebMerkleHashAlgorithm

-- | The Payload of a block.
--
-- The transactions of a block at a given height in the chain are discovered by
-- @_blockPayloadTransactionsHash . view blockPayloadHash@.
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
data BlockPayload_ a = BlockPayload
    { _blockPayloadPayloadHash :: !(BlockPayloadHash_ a)
        -- ^ Hash of '_blockPayloadTransactionsHash' and '_blockPayloadTransactionsHash'.
        -- Primary key of 'BlockPayloadStore'.

    , _blockPayloadTransactionsHash :: !(BlockTransactionsHash_ a)
        -- ^ Root of 'TransactionTree' of the block. Foreign key into
        -- 'BlockTransactionsStore' and 'TransactionTreeStore'.

    , _blockPayloadOutputsHash :: !(BlockOutputsHash_ a)
        -- ^ Root of 'OutputsTree' of the block. Foreign key into
        -- 'BlockOutputsStore' and 'OutputTreeStore'.
    }
    deriving (Show, Eq, Ord, Generic)

instance MerkleHashAlgorithm a => ToJSON (BlockPayload_ a) where
    toJSON o = object
        [ "payloadHash" .= _blockPayloadPayloadHash o
        , "transactionsHash" .= _blockPayloadTransactionsHash o
        , "outputsHash" .= _blockPayloadOutputsHash o
        ]

instance MerkleHashAlgorithm a => FromJSON (BlockPayload_ a) where
    parseJSON = withObject "BlockPayload" $ \o -> BlockPayload
        <$!> o .: "payloadHash"
        <*> o .: "transactionsHash"
        <*> o .: "outputsHash"

instance IsCasValue (BlockPayload_ a) where
    type CasKeyType (BlockPayload_ a) = BlockPayloadHash_ a
    casKey = _blockPayloadPayloadHash

instance MerkleHashAlgorithm a => HasMerkleLog a ChainwebHashTag (BlockPayload_ a) where
    type MerkleLogHeader (BlockPayload_ a) = '[BlockTransactionsHash_ a, BlockOutputsHash_ a]
    type MerkleLogBody (BlockPayload_ a) = Void

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

-- | Verify the consistency of the MerkleTree of a 'BlockPayload' value.
--
verifyBlockPayload :: MerkleHashAlgorithm a => BlockPayload_ a -> Bool
verifyBlockPayload p
    = BlockPayloadHash (MerkleLogHash (computeMerkleLogRoot p)) == _blockPayloadPayloadHash p

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
    toEncoding = b64UrlNoPaddingTextEncoding . _minerData
    {-# INLINE toJSON #-}

instance FromJSON MinerData where
    parseJSON = parseJsonFromText "MinerData"
    {-# INLINE parseJSON #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag MinerData where
    type Tag MinerData = 'MinerDataTag
    toMerkleNode = InputNode . _minerData
    fromMerkleNode (InputNode bytes) = Right $ MinerData bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

minerDataToText :: MinerData -> T.Text
minerDataToText = encodeB64UrlNoPaddingText . _minerData
{-# INLINE minerDataToText #-}

minerDataFromText :: MonadThrow m => T.Text -> m MinerData
minerDataFromText t = either (throwM . TextFormatException . sshow) return
    $ MinerData <$!> decodeB64UrlNoPaddingText t
{-# INLINE minerDataFromText #-}

instance HasTextRepresentation MinerData where
    toText = minerDataToText
    {-# INLINE toText #-}
    fromText = minerDataFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Block Transactions

type BlockTransactions = BlockTransactions_ ChainwebMerkleHashAlgorithm

-- | The block transactions
--
data BlockTransactions_ a = BlockTransactions
    { _blockTransactionsHash :: !(BlockTransactionsHash_ a)
        -- ^ Root of 'TransactionTree' of the block. Primary key of
        -- 'BlockTransactionsStore'. Foreign key into 'TransactionTreeStore'.

    , _blockTransactions :: !(V.Vector Transaction)
        -- ^ Ordered list of all transactions of the block.

    , _blockMinerData :: !MinerData
        -- ^ Miner data for rewards
    }
    deriving (Show, Eq, Ord, Generic)

-- -------------------------------------------------------------------------- --
-- Type-wrappers for some REST API endpoints

-- We want to use application/octet-stream as the content type for types
-- like [PayloadData], but doing that requires encoding the list specifically
-- with a specific binary instance. write some newtype wrappers to do this
-- with a specific encoding function, so we can then later write MimeRender
-- and MimeUnrender instances

newtype PayloadDataList = PayloadDataList { _payloadDataList :: [PayloadData] }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON)

newtype PayloadWithOutputsList = PayloadWithOutputsList { _payloadWithOutputsList :: [PayloadWithOutputs] }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON)

-- -------------------------------------------------------------------------- --

encodeBlockPayloads :: BlockPayload_ a -> B.ByteString
encodeBlockPayloads BlockPayload{..} = runPutS $ do
    encodeBlockPayloadHash _blockPayloadPayloadHash
    encodeBlockTransactionsHash _blockPayloadTransactionsHash
    encodeBlockOutputsHash _blockPayloadOutputsHash

decodeBlockPayloads :: (MonadThrow m, MerkleHashAlgorithm a) => B.ByteString -> m (BlockPayload_ a)
decodeBlockPayloads = runGetS $ BlockPayload
    <$> decodeBlockPayloadHash
    <*> decodeBlockTransactionsHash
    <*> decodeBlockOutputsHash

encodeBlockTransactions :: BlockTransactions_ a -> B.ByteString
encodeBlockTransactions txs = runPutS $ do
    encodeBlockTransactionsHash (_blockTransactionsHash txs)
    putWord64be (fromIntegral $ V.length (_blockTransactions txs))
    forM_ (_blockTransactions txs) $ \tx -> do
        putWord64be (fromIntegral $ B.length (_transactionBytes tx))
        putByteString (_transactionBytes tx)
    putWord64be (fromIntegral $ B.length $ _minerData $ _blockMinerData txs)
    putByteString (_minerData $ _blockMinerData txs)

decodeBlockTransactions :: (MonadThrow m, HashAlgorithm a) => B.ByteString -> m (BlockTransactions_ a)
decodeBlockTransactions = runGetS $ do
    hsh <- decodeBlockTransactionsHash
    txsCount <- fromIntegral <$> getWord64be
    txs <- replicateM txsCount $ do
        txSz <- fromIntegral <$> getWord64be
        txData <- getByteString txSz
        pure $ Transaction txData
    minerDataSz <- fromIntegral <$> getWord64be
    minerData <- MinerData <$> getByteString minerDataSz
    return BlockTransactions
        { _blockTransactionsHash = hsh
        , _blockTransactions = V.fromList txs
        , _blockMinerData = minerData
        }

encodeBlockOutputs :: BlockOutputs_ a -> B.ByteString
encodeBlockOutputs bo = runPutS $ do
    encodeBlockOutputsHash (_blockOutputsHash bo)
    putWord64be (fromIntegral $ V.length (_blockOutputs bo))
    forM_ (_blockOutputs bo) $ \tx -> do
        putWord64be (fromIntegral $ B.length (_transactionOutputBytes tx))
        putByteString (_transactionOutputBytes tx)
    putWord64be (fromIntegral $ B.length $ _coinbaseOutput $ _blockCoinbaseOutput bo)
    putByteString $ _coinbaseOutput $ _blockCoinbaseOutput bo

decodeBlockOutputs :: (MonadThrow m, HashAlgorithm a) => B.ByteString -> m (BlockOutputs_ a)
decodeBlockOutputs = runGetS $ do
    hsh <- decodeBlockOutputsHash
    txsCount <- fromIntegral <$> getWord64be
    txs <- replicateM txsCount $ do
        txSz <- fromIntegral <$> getWord64be
        txData <- getByteString txSz
        pure $ TransactionOutput txData
    coinbaseSz <- fromIntegral <$> getWord64be
    coinbaseData <- getByteString coinbaseSz
    return BlockOutputs
        { _blockOutputsHash = hsh
        , _blockOutputs = V.fromList txs
        , _blockCoinbaseOutput = CoinbaseOutput coinbaseData
        }

encodeTransactionTree :: TransactionTree_ a -> B.ByteString
encodeTransactionTree tt = runPutS $ do
    encodeBlockTransactionsHash (_transactionTreeHash tt)
    let bs = encodeMerkleTree (_transactionTree tt)
    putWord64be (fromIntegral $ B.length bs)
    putByteString bs

decodeTransactionTree :: (MonadThrow m, HashAlgorithm a) => B.ByteString -> m (TransactionTree_ a)
decodeTransactionTree = runGetS $ do
    hsh <- decodeBlockTransactionsHash
    sz <- fromIntegral <$> getWord64be
    bs <- getByteString sz
    mt <- decodeMerkleTree bs
    return TransactionTree
        { _transactionTreeHash = hsh
        , _transactionTree = mt
        }

encodeOutputTree :: OutputTree_ a -> B.ByteString
encodeOutputTree ot = runPutS $ do
    encodeBlockOutputsHash (_outputTreeHash ot)
    let bs = encodeMerkleTree (_outputTree ot)
    putWord64be (fromIntegral $ B.length bs)
    putByteString bs

decodeOutputTree :: (MonadThrow m, HashAlgorithm a) => B.ByteString -> m (OutputTree_ a)
decodeOutputTree = runGetS $ do
    hsh <- decodeBlockOutputsHash
    sz <- fromIntegral <$> getWord64be
    bs <- getByteString sz
    mt <- decodeMerkleTree bs
    return OutputTree
        { _outputTreeHash = hsh
        , _outputTree = mt
        }

encodeMinerData :: MinerData -> Put
encodeMinerData (MinerData md) = do
    putWord64be (fromIntegral $ B.length md)
    putByteString md

decodeMinerData :: Get MinerData
decodeMinerData = do
    sz <- fromIntegral <$> getWord64be
    MinerData <$> getByteString sz

putPayloadData :: PayloadData_ a -> Put
putPayloadData pd = do
    -- first encode _payloadDataTransactions: Vector Transaction
    putWord64be (fromIntegral $ V.length (_payloadDataTransactions pd))
    forM_ (_payloadDataTransactions pd) $ \tx -> do
        putWord64be (fromIntegral $ B.length (_transactionBytes tx))
        putByteString (_transactionBytes tx)
    encodeMinerData (_payloadDataMiner pd)
    encodeBlockPayloadHash (_payloadDataPayloadHash pd)
    encodeBlockTransactionsHash (_payloadDataTransactionsHash pd)
    encodeBlockOutputsHash (_payloadDataOutputsHash pd)

getPayloadData :: HashAlgorithm a => Get (PayloadData_ a)
getPayloadData = do
    txsCount <- fromIntegral <$> getWord64be
    txs <- replicateM txsCount $ do
        txSz <- fromIntegral <$> getWord64be
        txData <- getByteString txSz
        pure $ Transaction txData
    minerData <- decodeMinerData
    payloadHash <- decodeBlockPayloadHash
    txHash <- decodeBlockTransactionsHash
    outHash <- decodeBlockOutputsHash
    return PayloadData
        { _payloadDataTransactions = V.fromList txs
        , _payloadDataMiner = minerData
        , _payloadDataPayloadHash = payloadHash
        , _payloadDataTransactionsHash = txHash
        , _payloadDataOutputsHash = outHash
        }

decodePayloadData :: (MonadThrow m, HashAlgorithm a) => B.ByteString -> m (PayloadData_ a)
decodePayloadData = runGetS getPayloadData

encodePayloadData :: PayloadData_ a -> B.ByteString
encodePayloadData = runPutS . putPayloadData

encodeCoinbaseOutput :: CoinbaseOutput -> Put
encodeCoinbaseOutput (CoinbaseOutput co) = do
    putWord64be (fromIntegral $ B.length co)
    putByteString co

decodeCoinbaseOutput :: Get CoinbaseOutput
decodeCoinbaseOutput = do
    sz <- fromIntegral <$> getWord64be
    CoinbaseOutput <$> getByteString sz

putPayloadWithOutputs :: PayloadWithOutputs_ a -> Put
putPayloadWithOutputs pwo = do
    putWord64be (fromIntegral $ V.length (_payloadWithOutputsTransactions pwo))
    forM_ (_payloadWithOutputsTransactions pwo) $ \(tx, txo) -> do
        putWord64be (fromIntegral $ B.length (_transactionBytes tx))
        putByteString (_transactionBytes tx)
        putWord64be (fromIntegral $ B.length (_transactionOutputBytes txo))
        putByteString (_transactionOutputBytes txo)
    encodeMinerData (_payloadWithOutputsMiner pwo)
    encodeCoinbaseOutput (_payloadWithOutputsCoinbase pwo)
    encodeBlockPayloadHash (_payloadWithOutputsPayloadHash pwo)
    encodeBlockTransactionsHash (_payloadWithOutputsTransactionsHash pwo)
    encodeBlockOutputsHash (_payloadWithOutputsOutputsHash pwo)

encodePayloadWithOutputs :: PayloadWithOutputs_ a -> B.ByteString
encodePayloadWithOutputs = runPutS . putPayloadWithOutputs

getPayloadWithOutputs :: HashAlgorithm a => Get (PayloadWithOutputs_ a)
getPayloadWithOutputs = do
    txsCount <- fromIntegral <$> getWord64be
    txs <- replicateM txsCount $ do
        txSz <- fromIntegral <$> getWord64be
        txData <- getByteString txSz
        txoSz <- fromIntegral <$> getWord64be
        txoData <- getByteString txoSz
        pure (Transaction txData, TransactionOutput txoData)
    minerData <- decodeMinerData
    coinbaseOutput <- decodeCoinbaseOutput
    payloadHash <- decodeBlockPayloadHash
    txHash <- decodeBlockTransactionsHash
    outHash <- decodeBlockOutputsHash
    return PayloadWithOutputs
        { _payloadWithOutputsTransactions = V.fromList txs
        , _payloadWithOutputsMiner = minerData
        , _payloadWithOutputsCoinbase = coinbaseOutput
        , _payloadWithOutputsPayloadHash = payloadHash
        , _payloadWithOutputsTransactionsHash = txHash
        , _payloadWithOutputsOutputsHash = outHash
        }

decodePayloadWithOutputs :: (MonadThrow m, HashAlgorithm a) => B.ByteString -> m (PayloadWithOutputs_ a)
decodePayloadWithOutputs = runGetS getPayloadWithOutputs

encodePayloadDataList :: PayloadDataList -> B.ByteString
encodePayloadDataList (PayloadDataList xs) = runPutS $ do
    putWord64be (fromIntegral $ length xs)
    forM_ xs putPayloadData

decodePayloadDataList :: (MonadThrow m) => B.ByteString -> m PayloadDataList
decodePayloadDataList = runGetS $ do
    xsCount <- fromIntegral <$> getWord64be
    PayloadDataList <$> replicateM xsCount getPayloadData

encodePayloadWithOutputsList :: PayloadWithOutputsList -> B.ByteString
encodePayloadWithOutputsList (PayloadWithOutputsList xs) = runPutS $ do
    putWord64be (fromIntegral $ length xs)
    forM_ xs putPayloadWithOutputs

decodePayloadWithOutputsList :: (MonadThrow m) => B.ByteString -> m PayloadWithOutputsList
decodePayloadWithOutputsList = runGetS $ do
    xsCount <- fromIntegral <$> getWord64be
    PayloadWithOutputsList <$> replicateM xsCount getPayloadWithOutputs

-- -------------------------------------------------------------------------- --

blockTransactionsProperties
    :: MerkleHashAlgorithm a
    => A.KeyValue e kv
    => BlockTransactions_ a
    -> [kv]
blockTransactionsProperties o =
    [ "transactionHash" .= _blockTransactionsHash o
    , "transaction" .= _blockTransactions o
    , "minerData" .= _blockMinerData o
    ]
{-# INLINE blockTransactionsProperties #-}

instance MerkleHashAlgorithm a => ToJSON (BlockTransactions_ a) where
    toJSON = object . blockTransactionsProperties
    toEncoding = pairs . mconcat . blockTransactionsProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => FromJSON (BlockTransactions_ a) where
    parseJSON = withObject "BlockTransactions" $ \o -> BlockTransactions
        <$!> o .: "transactionHash"
        <*> o .: "transaction"
        <*> o .: "minerData"

instance IsCasValue (BlockTransactions_ a) where
    type CasKeyType (BlockTransactions_ a) = BlockTransactionsHash_ a
    casKey = _blockTransactionsHash

instance MerkleHashAlgorithm a => HasMerkleLog a ChainwebHashTag (BlockTransactions_ a) where
    type MerkleLogHeader (BlockTransactions_ a) = '[MinerData]
    type MerkleLogBody (BlockTransactions_ a) = Transaction

    toLog a = merkleLog root entries
      where
        BlockTransactionsHash (MerkleLogHash (!root)) = _blockTransactionsHash a
        !entries = _blockMinerData a :+: MerkleLogBody (_blockTransactions a)

    fromLog l = BlockTransactions
        { _blockTransactionsHash = BlockTransactionsHash $! MerkleLogHash $! _merkleLogRoot l
        , _blockTransactions = txs
        , _blockMinerData = mi
        }
      where
        (mi :+: MerkleLogBody txs) = _merkleLogEntries l

type BlockTransactionsLog a = MkLogType a ChainwebHashTag BlockTransactions

-- | Verify the consistency of the MerkleTree of a 'BlockTransactions' value.
--
-- This forces the MerkleTree which can be (somewhat) expensive for large input
-- values.
--
verifyBlockTransactions :: BlockTransactions -> Bool
verifyBlockTransactions p
    = BlockTransactionsHash (MerkleLogHash $ computeMerkleLogRoot p) == _blockTransactionsHash p

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
    toEncoding = b64UrlNoPaddingTextEncoding . _coinbaseOutput
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON CoinbaseOutput where
    parseJSON = parseJsonFromText "CoinbaseOutput"
    {-# INLINE parseJSON #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag CoinbaseOutput where
    type Tag CoinbaseOutput = 'CoinbaseOutputTag
    toMerkleNode = InputNode . _coinbaseOutput
    fromMerkleNode (InputNode bytes) = Right $ CoinbaseOutput bytes
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

coinbaseOutputToText :: CoinbaseOutput -> T.Text
coinbaseOutputToText = encodeB64UrlNoPaddingText . _coinbaseOutput
{-# INLINE coinbaseOutputToText #-}

coinbaseOutputFromText :: MonadThrow m => T.Text -> m CoinbaseOutput
coinbaseOutputFromText t = either (throwM . TextFormatException . sshow) return
    $ CoinbaseOutput <$!> decodeB64UrlNoPaddingText t
{-# INLINE coinbaseOutputFromText #-}

-- | No-op coinbase payload
--
noCoinbaseOutput :: CoinbaseOutput
noCoinbaseOutput = CoinbaseOutput $ BL.toStrict $ encodingToLazyByteString $ pairs $ mconcat
    [ "gas" .= (0 :: Int)
    , pair "result" $ pairs $ mconcat
        [ "status" .= ("success" :: String)
        , "data" .= ("NO_COINBASE" :: String)
        ]
    , "reqKey" .= ("DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g" :: String)
        -- this is the unique hash value define in @Pact.Types.Hash.initialHash@
    , "logs" .= Null
    , "metaData" .= Null
    , "continuation" .= Null
    , "txId" .= Null
    ]
{-# NOINLINE noCoinbaseOutput #-}

instance HasTextRepresentation CoinbaseOutput where
    toText = coinbaseOutputToText
    {-# INLINE toText #-}
    fromText = coinbaseOutputFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Block Outputs

type BlockOutputs = BlockOutputs_ ChainwebMerkleHashAlgorithm

-- | All outputs of the transactions of a block.
--
-- NOTE: the block outputs are associated with the respective block in the
-- 'BlockPayload' structure for the block.
--
data BlockOutputs_ a = BlockOutputs
    { _blockOutputsHash :: !(BlockOutputsHash_ a)
        -- ^ Root of 'OutputTree' of the block. Primary key of
        -- 'BlockOutputsStore'. Foreign key into 'OutputTreeStore'.

    , _blockOutputs :: !(V.Vector TransactionOutput)
        -- ^ Output of all transactions of a block in the order of the
        -- transactions in the block.

    , _blockCoinbaseOutput :: !CoinbaseOutput
        -- ^ Output of coinbase transaction.
    }
    deriving (Show, Eq)

blockOutputsProperties
    :: MerkleHashAlgorithm a
    => A.KeyValue e kv
    => BlockOutputs_ a
    -> [kv]
blockOutputsProperties o =
    [ "outputsHash" .= _blockOutputsHash o
    , "outputs" .= _blockOutputs o
    , "coinbaseOutput" .= _blockCoinbaseOutput o
    ]
{-# INLINE blockOutputsProperties #-}

instance MerkleHashAlgorithm a => ToJSON (BlockOutputs_ a) where
    toJSON = object . blockOutputsProperties
    toEncoding = pairs . mconcat . blockOutputsProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => FromJSON (BlockOutputs_ a) where
    parseJSON = withObject "BlockOutputs" $ \o -> BlockOutputs
        <$!> o .: "outputsHash"
        <*> o .: "outputs"
        <*> o .: "coinbaseOutput"

instance IsCasValue (BlockOutputs_ a) where
    type CasKeyType (BlockOutputs_ a) = BlockOutputsHash_ a
    casKey = _blockOutputsHash

instance MerkleHashAlgorithm a => HasMerkleLog a ChainwebHashTag (BlockOutputs_ a) where
    type MerkleLogHeader (BlockOutputs_ a) = '[CoinbaseOutput]
    type MerkleLogBody (BlockOutputs_ a) = TransactionOutput

    toLog a = merkleLog root entries
      where
        BlockOutputsHash (MerkleLogHash (!root)) = _blockOutputsHash a
        !entries = _blockCoinbaseOutput a :+: MerkleLogBody (_blockOutputs a)

    fromLog l = BlockOutputs
        { _blockOutputsHash = BlockOutputsHash $! MerkleLogHash $! _merkleLogRoot l
        , _blockOutputs = outs
        , _blockCoinbaseOutput = co
        }
      where
        (co :+: MerkleLogBody outs) = _merkleLogEntries l

type BlockOutputsLog a = MkLogType a ChainwebHashTag BlockOutputs

-- | Verify the consistency of the MerkleTree of a 'BlockOutputs' value.
--
-- This forces the MerkleTree which can be (somewhat) expensive for large input
-- values.
--
verifyBlockOutputs :: BlockOutputs -> Bool
verifyBlockOutputs p
    = BlockOutputsHash (MerkleLogHash $ computeMerkleLogRoot p) == _blockOutputsHash p

-- -------------------------------------------------------------------------- --
-- Transaction Merkle Tree

type TransactionTree = TransactionTree_ ChainwebMerkleHashAlgorithm

-- | Merkle tree for the transactions in a block.
--
data TransactionTree_ a = TransactionTree
    { _transactionTreeHash :: !(BlockTransactionsHash_ a)
        -- ^ Root of '_transactionTree'. Primary key of 'TransactionTreeStore.
        -- Foreign key into 'BlockTransactionsStore'.

    , _transactionTree :: !(MerkleTree a)
    }
    deriving (Show, Eq)

instance IsCasValue (TransactionTree_ a) where
    type CasKeyType (TransactionTree_ a) = BlockTransactionsHash_ a
    casKey = _transactionTreeHash

transactionTreeProperties
    :: MerkleHashAlgorithm a
    => A.KeyValue e kv
    => TransactionTree_ a
    -> [kv]
transactionTreeProperties o =
    [ "hash" .= _transactionTreeHash o
    , "tree" .= merkleTreeToJson (_transactionTree o)
    ]
{-# INLINE transactionTreeProperties #-}

instance MerkleHashAlgorithm a => ToJSON (TransactionTree_ a) where
    toJSON = object . transactionTreeProperties
    toEncoding = pairs . mconcat . transactionTreeProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => FromJSON (TransactionTree_ a) where
    parseJSON = withObject "TransactionTree" $ \o -> TransactionTree
        <$!> o .: "hash"
        <*> (o .: "tree" >>= merkleTreeFromJson)

merkleTreeToJson :: MerkleTree a -> Value
merkleTreeToJson = toJSON . encodeB64UrlNoPaddingText . encodeMerkleTree

merkleTreeFromJson :: MerkleHashAlgorithm a => Value -> A.Parser (MerkleTree a)
merkleTreeFromJson = withText "MerkleTree" $ \t -> either (fail . sshow) return
    $ decodeB64UrlNoPaddingText t >>= decodeMerkleTree

-- | Verify the consistency of the MerkleTree of a 'TransactionTree' value.
--
-- This forces the MerkleTree which can be (somewhat) expensive for large input
-- values.
--
verifyTransactionTree :: MerkleHashAlgorithm a => TransactionTree_ a -> Bool
verifyTransactionTree p = _transactionTreeHash p
    == BlockTransactionsHash (MerkleLogHash $ merkleRoot $ _transactionTree p)

-- -------------------------------------------------------------------------- --
-- Output Merkle Tree

type OutputTree = OutputTree_ ChainwebMerkleHashAlgorithm

-- | Merkle Tree for transaction outputs of a block.
--
data OutputTree_ a = OutputTree
    { _outputTreeHash :: !(BlockOutputsHash_ a)
        -- ^ Root of '_outputTree'. Primary key of 'OutputTreeStore. Foreign key
        -- into 'BlockOutputsStore'.

    , _outputTree :: !(MerkleTree a)
    }
    deriving (Show, Eq)

instance IsCasValue (OutputTree_ a) where
    type CasKeyType (OutputTree_ a) = BlockOutputsHash_ a
    casKey = _outputTreeHash

outputTreeProperties
    :: MerkleHashAlgorithm a
    => A.KeyValue e kv
    => OutputTree_ a
    -> [kv]
outputTreeProperties o =
    [ "hash" .= _outputTreeHash o
    , "tree" .= merkleTreeToJson (_outputTree o)
    ]
{-# INLINE outputTreeProperties #-}

instance MerkleHashAlgorithm a => ToJSON (OutputTree_ a) where
    toJSON = object . outputTreeProperties
    toEncoding = pairs . mconcat . outputTreeProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => FromJSON (OutputTree_ a) where
    parseJSON = withObject "OutputTree" $ \o -> OutputTree
        <$!> o .: "hash"
        <*> (o .: "tree" >>= merkleTreeFromJson)

-- | Verify the consistency of the MerkleTree of a 'OuputTree' value.
--
-- This forces the MerkleTree which can be (somewhat) expensive for large input
-- values.
--
verifyOutputTree :: MerkleHashAlgorithm a => OutputTree_ a -> Bool
verifyOutputTree p = _outputTreeHash p
    == BlockOutputsHash (MerkleLogHash $ merkleRoot $ _outputTree p)

-- -------------------------------------------------------------------------- --
-- Data Creation
-- -------------------------------------------------------------------------- --

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newTransactionLog
    :: MerkleHashAlgorithm a
    => MinerData
    -> V.Vector Transaction
    -> BlockTransactionsLog a
newTransactionLog md txs =
  newMerkleLog $ md :+: MerkleLogBody txs

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockTransactions
    :: MerkleHashAlgorithm a
    => MinerData
    -> V.Vector Transaction
    -> (TransactionTree_ a, BlockTransactions_ a)
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
    :: MerkleHashAlgorithm a
    => BlockTransactions_ a
    -> TransactionTree_ a
    -> BlockTransactionsLog a
transactionLog txs tree
    | _blockTransactionsHash txs == _transactionTreeHash tree
        = (toLog txs) { _merkleLogTree = _transactionTree tree }
    | otherwise = error "Transaction tree and block transactions don't match"

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockOutputLog
    :: MerkleHashAlgorithm a
    => CoinbaseOutput
    -> V.Vector TransactionOutput
    -> BlockOutputsLog a
newBlockOutputLog co tos = newMerkleLog $ co :+: MerkleLogBody tos

-- | This forces the 'MerkleTree' which can be an expensive operation.
--
newBlockOutputs
    :: MerkleHashAlgorithm a
    => CoinbaseOutput
    -> V.Vector TransactionOutput
    -> (OutputTree_ a, BlockOutputs_ a)
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
    :: MerkleHashAlgorithm a
    => BlockOutputs_ a
    -> OutputTree_ a
    -> BlockOutputsLog a
blockOutputLog outs tree
    | _blockOutputsHash outs == _outputTreeHash tree
        = (toLog outs) { _merkleLogTree = _outputTree tree }
    | otherwise = error "Output tree and block outputs don't match"

-- | Create a BlockPayload from 'BlockTransactions' and 'BlockOutputs'
--
-- This doesn't force the Merkle trees of the input structures.
--
blockPayload
    :: forall a
    . MerkleHashAlgorithm a
    => BlockTransactions_ a
    -> BlockOutputs_ a
    -> BlockPayload_ a
blockPayload txs outs
    = fromLog $! newMerkleLog @a @ChainwebHashTag
        $ _blockTransactionsHash txs
        :+: _blockOutputsHash outs
        :+: emptyBody

-- | Compute BlockPayload from transactions and outputs.
--
-- This forces the MerkleTrees of all payload components. The returned
-- '_blockPayloadPayloadHash' value can be trusted.
--
newBlockPayload
    :: MerkleHashAlgorithm a
    => MinerData
    -> CoinbaseOutput
    -> V.Vector (Transaction, TransactionOutput)
    -> BlockPayload_ a
newBlockPayload mi co s = blockPayload txs outs
  where
    (_, !txs) = newBlockTransactions mi (fst <$!> s)
    (_, !outs) = newBlockOutputs co (snd <$!> s)

-- -------------------------------------------------------------------------- --
-- Payload Data

type PayloadData = PayloadData_ ChainwebMerkleHashAlgorithm

-- | This contains all non-redundant payload data for a block. It doesn't
-- contain any data that can be recomputed.
--
-- This data structure is used mainly to transfer payloads over the wire.
--
data PayloadData_ a = PayloadData
    { _payloadDataTransactions :: !(V.Vector Transaction)
    , _payloadDataMiner :: !MinerData
    , _payloadDataPayloadHash :: !(BlockPayloadHash_ a)
    , _payloadDataTransactionsHash :: !(BlockTransactionsHash_ a)
    , _payloadDataOutputsHash :: !(BlockOutputsHash_ a)
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (NFData)

payloadDataTransactions :: Getter (PayloadData_ a) (V.Vector Transaction)
payloadDataTransactions = to _payloadDataTransactions

payloadDataMiner :: Getter (PayloadData_ a) MinerData
payloadDataMiner = to _payloadDataMiner

payloadDataPayloadHash :: Getter (PayloadData_ a) (BlockPayloadHash_ a)
payloadDataPayloadHash = to _payloadDataPayloadHash

payloadDataTransactionsHash :: Getter (PayloadData_ a) (BlockTransactionsHash_ a)
payloadDataTransactionsHash = to _payloadDataTransactionsHash

payloadDataOutputsHash :: Getter (PayloadData_ a) (BlockOutputsHash_ a)
payloadDataOutputsHash = to _payloadDataOutputsHash

payloadDataProperties
    :: MerkleHashAlgorithm a
    => A.KeyValue e kv
    => PayloadData_ a
    -> [kv]
payloadDataProperties o =
    [ "transactions" .= _payloadDataTransactions o
    , "minerData" .= _payloadDataMiner o
    , "transactionsHash" .= _payloadDataTransactionsHash o
    , "outputsHash" .= _payloadDataOutputsHash o
    , "payloadHash" .= _payloadDataPayloadHash o
    ]
{-# INLINE payloadDataProperties #-}

instance MerkleHashAlgorithm a => ToJSON (PayloadData_ a) where
    toJSON = object . payloadDataProperties
    toEncoding = pairs . mconcat . payloadDataProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => FromJSON (PayloadData_ a) where
    parseJSON = withObject "PayloadData" $ \o -> PayloadData
        <$!> o .: "transactions"
        <*> o .: "minerData"
        <*> o .: "payloadHash"
        <*> o .: "transactionsHash"
        <*> o .: "outputsHash"

instance IsCasValue (PayloadData_ a) where
    type CasKeyType (PayloadData_ a) = BlockPayloadHash_ a
    casKey = _payloadDataPayloadHash
    {-# INLINE casKey #-}

payloadData :: BlockTransactions_ a -> BlockPayload_ a -> PayloadData_ a
payloadData txs payload = PayloadData
    { _payloadDataTransactions = _blockTransactions txs
    , _payloadDataMiner = _blockMinerData txs
    , _payloadDataPayloadHash = _blockPayloadPayloadHash payload
    , _payloadDataTransactionsHash = _blockPayloadTransactionsHash payload
    , _payloadDataOutputsHash = _blockPayloadOutputsHash payload
    }

payloadDataToBlockPayload :: PayloadData_ a -> BlockPayload_ a
payloadDataToBlockPayload p = BlockPayload
    { _blockPayloadPayloadHash = _payloadDataPayloadHash p
    , _blockPayloadTransactionsHash = _payloadDataTransactionsHash p
    , _blockPayloadOutputsHash = _payloadDataOutputsHash p
    }

newPayloadData
    :: MerkleHashAlgorithm a
    => BlockTransactions_ a
    -> BlockOutputs_ a
    -> PayloadData_ a
newPayloadData txs outputs = payloadData txs $ blockPayload txs outputs

type PayloadDataCas tbl = Cas tbl PayloadData

-- | Verify the consistency of the MerkleTree of a 'PayloadData' value.
--
-- This doesn't verify the MerkleTree for the outputs because those are not
-- available in the 'PayloadData'.
--
-- This forces the MerkleTree which can be (somewhat) expensive for large input
-- values.
--
verifyPayloadData :: forall a . MerkleHashAlgorithm a => PayloadData_ a -> Bool
verifyPayloadData p
    = _payloadDataTransactionsHash p == _blockTransactionsHash txs
    && _payloadDataPayloadHash p == _blockPayloadPayloadHash bp
  where
    -- forces the transactions Merkle Tree
    txs :: BlockTransactions_ a
    txs = fromLog @a $ newTransactionLog
        (_payloadDataMiner p)
        (_payloadDataTransactions p)

    -- forces the BlockPayload Merkle Tree
    bp = fromLog @a $ newMerkleLog
        $ _payloadDataTransactionsHash p
        :+: _payloadDataOutputsHash p
        :+: emptyBody

-- -------------------------------------------------------------------------- --
-- All Payload Data in a Single Structure

type PayloadWithOutputs = PayloadWithOutputs_ ChainwebMerkleHashAlgorithm

data PayloadWithOutputs_ a = PayloadWithOutputs
    { _payloadWithOutputsTransactions :: !(V.Vector (Transaction, TransactionOutput))
    , _payloadWithOutputsMiner :: !MinerData
    , _payloadWithOutputsCoinbase :: !CoinbaseOutput
    , _payloadWithOutputsPayloadHash :: !(BlockPayloadHash_ a)
    , _payloadWithOutputsTransactionsHash :: !(BlockTransactionsHash_ a)
    , _payloadWithOutputsOutputsHash :: !(BlockOutputsHash_ a)
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (NFData)

instance IsCasValue (PayloadWithOutputs_ a) where
    type CasKeyType (PayloadWithOutputs_ a) = BlockPayloadHash_ a
    casKey = _payloadWithOutputsPayloadHash
    {-# INLINE casKey #-}

-- | Smart constructor for 'PayloadWithOutputs'.
--
-- Precondition: the vector of transaction output has the same length (and is
-- in the same order, i.e. the two vectors will be zipped) as the list of input
-- transactions inside the 'PayloadData'.
--
-- NOTE: The resulting structure is consistent only if the input 'PayloadData'
-- is consistent. Use 'newPayloadWithOutputs' and compare
-- '_payloadWithOutputsPayloadHash' to verify consistency.
--
payloadWithOutputs
    :: HasCallStack
    => PayloadData_ a
    -> CoinbaseOutput
    -> V.Vector TransactionOutput
    -> PayloadWithOutputs_ a
payloadWithOutputs d co outputs =
  if V.length (_payloadDataTransactions d) /= V.length outputs
    then let msg = concat [
               "Internal code invariant violation: ",
               "PAYLOAD ERROR: MISMATCHED # OF TRANSACTIONS AND OUTPUTS: \n",
               "PayloadData=",
               show d,
               "\nTransactionOutputs=",
               show outputs
               ]
         in error msg
    else PayloadWithOutputs
           { _payloadWithOutputsTransactions = V.zip (_payloadDataTransactions d) outputs
           , _payloadWithOutputsMiner = _payloadDataMiner d
           , _payloadWithOutputsCoinbase = co
           , _payloadWithOutputsPayloadHash = _payloadDataPayloadHash d
           , _payloadWithOutputsTransactionsHash = _payloadDataTransactionsHash d
           , _payloadWithOutputsOutputsHash = _payloadDataOutputsHash d
           }

newPayloadWithOutputs
    :: MerkleHashAlgorithm a
    => MinerData
    -> CoinbaseOutput
    -> V.Vector (Transaction, TransactionOutput)
    -> PayloadWithOutputs_ a
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

payloadWithOutputsProperties
    :: MerkleHashAlgorithm a
    => A.KeyValue e kv
    => PayloadWithOutputs_ a
    -> [kv]
payloadWithOutputsProperties o =
    [ "transactions" .= _payloadWithOutputsTransactions o
    , "minerData" .= _payloadWithOutputsMiner o
    , "transactionsHash" .= _payloadWithOutputsTransactionsHash o
    , "outputsHash" .= _payloadWithOutputsOutputsHash o
    , "payloadHash" .= _payloadWithOutputsPayloadHash o
    , "coinbase" .= _payloadWithOutputsCoinbase o
    ]
{-# INLINE payloadWithOutputsProperties #-}

instance MerkleHashAlgorithm a => ToJSON (PayloadWithOutputs_ a) where
    toJSON = object . payloadWithOutputsProperties
    toEncoding = pairs . mconcat . payloadWithOutputsProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

-- | This instance trusts the content of the JSON structure. It doesn't
-- guarantee that the result is consistent (it doesn't rebuild the Merkle tree).
--
-- Use 'newPayloadWithOutputs' and compare '_payloadWithOutputsPayloadHash' to
-- verify consistency.
--
instance MerkleHashAlgorithm a => FromJSON (PayloadWithOutputs_ a) where
    parseJSON = withObject "PayloadWithOutputs" $ \o -> PayloadWithOutputs
        <$!> o .: "transactions"
        <*> o .: "minerData"
        <*> o .: "coinbase"
        <*> o .: "payloadHash"
        <*> o .: "transactionsHash"
        <*> o .: "outputsHash"

payloadWithOutputsToBlockObjects
    :: PayloadWithOutputs_ a
    -> (BlockTransactions_ a, BlockOutputs_ a)
payloadWithOutputsToBlockObjects PayloadWithOutputs {..} =
    ( BlockTransactions _payloadWithOutputsTransactionsHash ins _payloadWithOutputsMiner
    , BlockOutputs _payloadWithOutputsOutputsHash outs _payloadWithOutputsCoinbase
    )
  where
    (ins,outs) = V.unzip _payloadWithOutputsTransactions

payloadWithOutputsToPayloadData :: PayloadWithOutputs_ a -> PayloadData_ a
payloadWithOutputsToPayloadData o = PayloadData
    { _payloadDataTransactions = fst <$!> _payloadWithOutputsTransactions o
    , _payloadDataMiner = _payloadWithOutputsMiner o
    , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash o
    , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash o
    , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash o
    }

-- | Verify the consistency of the MerkleTree of a 'PayloadData' value.
--
-- This doesn't verify the MerkleTree for the outputs because those are not
-- available in the 'PayloadData'.
--
-- This forces the MerkleTree which can be (somewhat) expensive for large input
-- values.
--
verifyPayloadWithOutputs :: MerkleHashAlgorithm a => PayloadWithOutputs_ a -> Bool
verifyPayloadWithOutputs p
    = _payloadWithOutputsPayloadHash p == _blockPayloadPayloadHash p'
  where
    -- recreate the structure and force all MerkleTrees
    --
    p' = newBlockPayload
        (_payloadWithOutputsMiner p)
        (_payloadWithOutputsCoinbase p)
        (_payloadWithOutputsTransactions p)

-- if this payload was produced locally we have the outputs from when we
-- produced it originally. we can't just *use them* - ValidateBlock still has
-- to run the payload to check if it's a valid block - but they are great
-- for comparative error messages if the block is invalid.
data CheckablePayload
  = CheckablePayloadWithOutputs !PayloadWithOutputs
  | CheckablePayload !PayloadData
  deriving (Show)

checkablePayloadToPayloadData :: CheckablePayload -> PayloadData
checkablePayloadToPayloadData = \case
  CheckablePayload pd -> pd
  CheckablePayloadWithOutputs pwo -> payloadWithOutputsToPayloadData pwo
