{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.EngineAPI
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Ethereum Engine API:
--
-- https://ethereum.org/en/developers/docs/apis/json-rpc/
-- https://github.com/ethereum/execution-apis
-- https://ethereum.github.io/execution-apis/api-documentation/
--
-- There are currently four versions of the Engine API:
--
-- - Paris: First version that is used during the "merge". It covers several
--   details of the transition from PoW to PoS that are not relevant in the
--   context of Chainweb.
--
-- - Shanghai: Introduces support for Withdrawals and adds
--   - WithdrawalsV1
--   - ExecutionPayloadBodyV1
--   - engine_getPayloadBodiesByHashV1
--   - engine_getPayloadBodiesByRangeV1
--
-- - Cancun: Introduces support for Blobs and adds
--   - BlobsBundleV1
--   - BlobAndProofV1
--   - engine_getBlobsV1
--
-- - Prague: Introduces support for ExecutionRequests
--
-- The implementation does not cover all features from all versions.
--
-- IMPORTANT NOTE:
--
-- Consensus Layer client software MUST wait for a specified timeout before
-- aborting the call. In such an event, the Consensus Layer client software
-- SHOULD retry the call when it is needed to keep progressing.
--
-- Consensus Layer client software MAY wait for response longer than it is
-- specified by the timeout parameter.
--
module Chainweb.PayloadProvider.EVM.EngineAPI
( WithdrawalV1(..)
, withdrawlsRoot
, ForkchoiceStateV1(..)
, DefaultBlockParameter(..)
, EngineServerErrors(..)
, EngineErrors(..)
, PayloadId(..)
, PayloadStatusV1(..)
, TransactionBytes(..)
, transactionsRoot

-- * Payload Attributes
, PayloadAttributesV1(..)
, PayloadAttributesV2(..)
, PayloadAttributesV3(..)
, PayloadStatusStatus(..)

-- * Forkchoice Update
, ForkchoiceUpdatedV3Request(..)
, ForkchoiceUpdatedV1Response(..)

-- * Execution Payload
, ExecutionPayloadV1(..)
, ExecutionPayloadV2(..)
, ExecutionPayloadV3(..)
, NewPayloadV4Request(..)

-- * Get Payload Response
, Blob(..)
, KzgProof(..)
, KzgCommitment(..)
, BlobsBundleV1(..)
, VersionedHash(..)
, versionedHashes
, BlockValue(..)
, GetPayloadV2Response(..)
, GetPayloadV3Response(..)
, GetPayloadV4Response(..)

-- * Authentication and Client Context
, JwtSecret(..)
, jwtToken
, getJwtToken
, mkSimpleEngineCtx
, mkEngineCtx

-- * Engine API Methods
, type Engine_GetPayloadV2
, type Engine_GetPayloadV3
, type Engine_GetPayloadV4
, type Engine_ForkchoiceUpdatedV3
, type Engine_NewPayloadV4
) where

import Chainweb.PayloadProvider.EVM.Header
import Chainweb.PayloadProvider.EVM.JsonRPC
import Chainweb.PayloadProvider.EVM.Utils
import Chainweb.Utils
import Control.Monad.Catch
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC
import Data.Aeson
import Data.Bifunctor
import Data.ByteArray qualified as BA
import Data.ByteString.Short qualified as BS
import Data.Foldable
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Word
import Ethereum.Misc qualified as E
import Ethereum.RLP (RLP (..), putRlpByteString, getRlpL, putRlpL, label)
import Ethereum.Trie
import Ethereum.Utils hiding (int, natVal_)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GHC.TypeLits
import Network.HTTP.Client qualified as HTTP
import Network.URI
import Network.URI.Static (uri)
import System.IO.Unsafe (unsafePerformIO)
import Data.Hash.SHA2
import Data.Coerce

-- -------------------------------------------------------------------------- --
-- Forkchoice State V1

-- | Forkchoice state object V1
--
-- This structure encapsulates the fork choice state.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/paris.md#forkchoicestatev1
--
data ForkchoiceStateV1 = ForkchoiceStateV1
    { _forkchoiceHeadBlockHash :: !E.BlockHash
        -- ^ headBlockHash: DATA, 32 Bytes - block hash of the head of the
        -- canonical chain
    , _forkchoiceSafeBlockHash :: !E.BlockHash
        -- ^ safeBlockHash: DATA, 32 Bytes - the "safe" block hash of the
        -- canonical chain under certain synchrony and honesty assumptions. This
        -- value MUST be either equal to or an ancestor of headBlockHash
    , _forkchoiceFinalizedBlockHash :: !E.BlockHash
        -- ^ finalizedBlockHash: DATA, 32 Bytes - block hash of the most recent
        -- finalized block
    }
    deriving (Show, Eq, Generic)

instance ToJSON ForkchoiceStateV1 where
    toEncoding o = pairs
        $ "headBlockHash" .= _forkchoiceHeadBlockHash o
        <> "safeBlockHash" .= _forkchoiceSafeBlockHash o
        <> "finalizedBlockHash" .= _forkchoiceFinalizedBlockHash o
    {-# INLINE toEncoding #-}

    toJSON o = object
        [ "headBlockHash" .= _forkchoiceHeadBlockHash o
        , "safeBlockHash" .= _forkchoiceSafeBlockHash o
        , "finalizedBlockHash" .= _forkchoiceFinalizedBlockHash o
        ]
    {-# INLINE toJSON #-}

instance FromJSON ForkchoiceStateV1 where
    parseJSON = withObject "ForkchoiceStateV1" $ \o -> ForkchoiceStateV1
        <$> o .: "headBlockHash"
        <*> o .: "safeBlockHash"
        <*> o .: "finalizedBlockHash"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Payload Status Status

-- | Payload validation status
--
-- Set of possible values is restricted to
-- "VALID" | "INVALID" | "SYNCING" | "ACCEPTED" | "INVALID_BLOCK_HASH"
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/paris.md#payloadstatusv1
--
data PayloadStatusStatus
    = Valid
    | Invalid
    | Syncing
    | Accepted
    | InvalidBlockHash
    deriving (Show, Eq, Generic, Enum, Bounded)
    deriving (ToJSON, FromJSON) via (JsonTextRepresentation "PayloadStatusStatus" PayloadStatusStatus)

instance HasTextRepresentation PayloadStatusStatus where
    toText Valid = "VALID"
    toText Invalid = "INVALID"
    toText Syncing = "SYNCING"
    toText Accepted = "ACCEPTED"
    toText InvalidBlockHash = "INVALID_BLOCK_HASH"
    {-# INLINE toText #-}

    fromText "VALID" = return Valid
    fromText "INVALID" = return Invalid
    fromText "SYNCING" = return Syncing
    fromText "ACCEPTED" = return Accepted
    fromText "INVALID_BLOCK_HASH" = return InvalidBlockHash
    fromText x = throwM $ TextFormatException $ "Invalid PayloadStatusStatus: " <> x
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Payload Status V1

-- | Payload Status V1
--
-- This structure contains the result of processing a payload.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/paris.md#payloadstatusv1
--
data PayloadStatusV1 = PayloadStatusV1
    { _payloadStatusV1Status :: !PayloadStatusStatus
        -- ^ Status of the payload
    , _payloadStatusV1LatestValidHash :: !(Maybe E.BlockHash)
        -- ^ DATA|null, 32 Bytes - the hash of the most recent valid block in
        -- the branch defined by payload and its ancestors
    , _payloadStatusV1ValidationError :: !(Maybe T.Text)
        -- ^ String|null - a message providing additional details on the
        -- validation error if the payload is classified as INVALID or
        -- INVALID_BLOCK_HASH.
    }
    deriving (Eq, Show, Generic)

payloadStatusV1Properties
    :: KeyValue e kv
    => PayloadStatusV1
    -> [kv]
payloadStatusV1Properties o =
    [ "status" .= _payloadStatusV1Status o
    , "latestValidHash" .= _payloadStatusV1LatestValidHash o
    , "validationError" .= _payloadStatusV1ValidationError o
    ]
{-# INLINE payloadStatusV1Properties #-}

instance ToJSON PayloadStatusV1 where
    toEncoding = pairs . mconcat . payloadStatusV1Properties
    toJSON = object . payloadStatusV1Properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON PayloadStatusV1 where
    parseJSON = withObject "PayloadStatusV1" $ \o -> PayloadStatusV1
        <$> o .: "status"
        <*> o .: "latestValidHash"
        <*> o .: "validationError"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Withdrawal V1

-- | Withdrawal object V1
--
-- This structure maps onto the validator withdrawal object from the beacon
-- chain spec.
--
-- Note: the amount value is represented on the beacon chain as a little-endian
-- value in units of Gwei, whereas the amount in this structure MUST be
-- converted to a big-endian value in units of Gwei.
--
-- https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#withdrawalv1
--
data WithdrawalV1 = WithdrawalV1
    { _withdrawalIndex :: !Word64
        -- ^ index: QUANTITY, 64 Bits
    , _withdrawalValidatorIndex :: !Word64
        -- ^ validatorIndex: QUANTITY, 64 Bits
    , _withdrawalAddress :: !E.Address
        -- ^ address: DATA, 20 Bytes
    , _withdrawalAmount :: !Word64
        -- ^ amount: QUANTITY, 64 Bits
    }
    deriving (Show, Eq, Generic)

instance ToJSON WithdrawalV1 where
    toEncoding o = pairs
        $ "index" .= HexQuantity (_withdrawalIndex o)
        <> "validatorIndex" .= HexQuantity (_withdrawalValidatorIndex o)
        <> "address" .= _withdrawalAddress o
        <> "amount" .= HexQuantity (_withdrawalAmount o)
    {-# INLINE toEncoding #-}

    toJSON o = object
        [ "index" .= HexQuantity (_withdrawalIndex o)
        , "validatorIndex" .= HexQuantity (_withdrawalValidatorIndex o)
        , "address" .= _withdrawalAddress o
        , "amount" .= HexQuantity (_withdrawalAmount o)
        ]
    {-# INLINE toJSON #-}

instance FromJSON WithdrawalV1 where
    parseJSON = withObject "Withdrawal" $ \o -> WithdrawalV1
        <$> fmap fromHexQuanity (o .: "index")
        <*> fmap fromHexQuanity (o .: "validatorIndex")
        <*> o .: "address"
        <*> fmap fromHexQuanity (o .: "amount")
    {-# INLINE parseJSON #-}

instance RLP WithdrawalV1 where
    putRlp w = putRlpL
        [ putRlp $ _withdrawalIndex w
        , putRlp $ _withdrawalValidatorIndex w
        , putRlp $ _withdrawalAddress w
        , putRlp $ _withdrawalAmount w
        ]

    getRlp = label "WithdrawalV1" $ getRlpL $
        WithdrawalV1
            <$> getRlp -- index
            <*> getRlp -- validator index
            <*> getRlp -- address
            <*> getRlp -- amount

withdrawlsRoot :: [WithdrawalV1] -> WithdrawalsRoot
withdrawlsRoot l = unsafePerformIO $ do
    store <- mkHashMapStore
    Trie (E.Keccak256Hash !t) <- trie (_trieStoreAdd store)
        $ bimap putRlpByteString putRlpByteString <$> zip [0::Natural ..] l
    return (WithdrawalsRoot t)

-- -------------------------------------------------------------------------- --
-- Blobs Bundle V1

type BYTES_PER_FIELD_ELEMENT :: Natural
type BYTES_PER_FIELD_ELEMENT = 32

type FIELD_ELEMENTS_PER_BLOB :: Natural
type FIELD_ELEMENTS_PER_BLOB = 4096

type BLOB_SIZE :: Natural
type BLOB_SIZE = FIELD_ELEMENTS_PER_BLOB GHC.TypeLits.* BYTES_PER_FIELD_ELEMENT

versionedHashVersionKzg :: Word8
versionedHashVersionKzg = 0x01

-- | EIP-4844 Versioned Hash
--
newtype VersionedHash = VersionedHash { _versionedHash :: E.BytesN 32 }
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, E.Bytes, Storable, Hashable)
    deriving (ToJSON, FromJSON) via (HexBytes (E.BytesN 32))

-- | EIP-4844 KZG Commitment
--
newtype KzgCommitment = KzgCommitment { _kzgCommitment :: E.BytesN 48 }
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, E.Bytes, Storable, Hashable)
    deriving (ToJSON, FromJSON) via (HexBytes (E.BytesN 48))

-- | EIP-4844 KZG Proof
--
newtype KzgProof = KzgProof { _kzgProof :: E.BytesN 48 }
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, E.Bytes, Storable, Hashable)
    deriving (ToJSON, FromJSON) via (HexBytes (E.BytesN 48))

-- | EIP-4844 Blob
--
newtype Blob = Blob { _blob :: E.BytesN BLOB_SIZE }
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, E.Bytes, Storable, Hashable)
    deriving (ToJSON, FromJSON) via (HexBytes (E.BytesN BLOB_SIZE))

-- | Blobs Bundle V1
--
-- * commitments: Array of DATA - Array of KZGCommitment as defined in EIP-4844,
--   48 bytes each (DATA).
-- * proofs: Array of DATA - Array of KZGProof as defined in EIP-4844, 48 bytes
--   each (DATA).
-- * blobs: Array of DATA - Array of blobs, each blob is FIELD_ELEMENTS_PER_BLOB
--   * BYTES_PER_FIELD_ELEMENT = 4096 * 32 = 131072 bytes (DATA) representing a
--   SSZ-encoded Blob as defined in EIP-4844
--
-- All of the above three arrays MUST be of same length.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/cancun.md#blobsbundlev1
--
data BlobsBundleV1 = BlobsBundleV1
    { _blobsBundleV1Commitments :: ![KzgCommitment]
        -- ^ commitments: Array of DATA - Array of KZGCommitment as defined in
        -- EIP-4844, 48 bytes each (DATA).
    , _blobsBundleV1Proofs :: ![KzgProof]
        -- ^ proofs: Array of DATA - Array of KZGProof as defined in EIP-4844,
        -- 48 bytes each (DATA).
    , _blobsBundleV1Blobs :: ![Blob]
        -- ^ blobs: Array of DATA - Array of blobs, each blob is
        -- FIELD_ELEMENTS_PER_BLOB * BYTES_PER_FIELD_ELEMENT = 4096 * 32 = 131072
        -- bytes (DATA) representing a SSZ-encoded Blob as defined in EIP-4844
    }
    deriving (Show, Eq, Generic)

blobsBundleV1Properties
    :: KeyValue e kv
    => BlobsBundleV1
    -> [kv]
blobsBundleV1Properties o =
    [ "commitments" .= _blobsBundleV1Commitments o
    , "proofs" .= _blobsBundleV1Proofs o
    , "blobs" .= _blobsBundleV1Blobs o
    ]

instance ToJSON BlobsBundleV1 where
    toEncoding = pairs . mconcat . blobsBundleV1Properties
    toJSON = object . blobsBundleV1Properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON BlobsBundleV1 where
    parseJSON = withObject "BlobsBundleV1" $ \o -> BlobsBundleV1
        <$> o .: "commitments"
        <*> o .: "proofs"
        <*> o .: "blobs"
    {-# INLINE parseJSON #-}

-- | Compute versioned hash for a blob
--
-- cf. https://eips.ethereum.org/EIPS/eip-4844#helpers
--
-- VERSIONED_HASH_VERSION_KZG + sha256(commitment)[1:]
--
versionedHash :: KzgCommitment -> VersionedHash
versionedHash (KzgCommitment c) = VersionedHash
    $ E.unsafeBytesN @32
    $ BS.singleton versionedHashVersionKzg
    <> BS.drop 1 (coerce $ hashShortByteString_ @Sha2_256 $ E._getBytesN c)

versionedHashes :: BlobsBundleV1 -> [VersionedHash]
versionedHashes (BlobsBundleV1 cs _ _) = map versionedHash cs

-- -------------------------------------------------------------------------- --
-- Execution Payload V1

newtype TransactionBytes = TransactionBytes
    { _transactionBytes :: BS.ShortByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable, E.Bytes, RLP)

instance ToJSON TransactionBytes where
    toEncoding (TransactionBytes a) = toEncoding (HexBytes a)
    toJSON (TransactionBytes a) = toJSON (HexBytes a)
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON TransactionBytes where
    parseJSON v = TransactionBytes <$> do
        HexBytes b <- parseJSON v
        return b
    {-# INLINE parseJSON #-}

transactionsRoot :: [TransactionBytes] -> E.TransactionsRoot
transactionsRoot l = unsafePerformIO $ do
    store <- mkHashMapStore
    Trie !t <- trie (_trieStoreAdd store)
        $ bimap putRlpByteString E.bytes <$> zip [0::Natural ..] l
    return (E.TransactionsRoot t)

-- | Execution Payload V1
--
-- This structure maps on the ExecutionPayload structure of the beacon chain
-- spec.
--
-- * parentHash: DATA, 32 Bytes
-- * feeRecipient:  DATA, 20 Bytes
-- * stateRoot: DATA, 32 Bytes
-- * receiptsRoot: DATA, 32 Bytes
-- * logsBloom: DATA, 256 Bytes
-- * prevRandao: DATA, 32 Bytes
-- * blockNumber: QUANTITY, 64 Bits
-- * gasLimit: QUANTITY, 64 Bits
-- * gasUsed: QUANTITY, 64 Bits
-- * timestamp: QUANTITY, 64 Bits
-- * extraData: DATA, 0 to 32 Bytes
-- * baseFeePerGas: QUANTITY, 256 Bits
-- * blockHash: DATA, 32 Bytes
-- * transactions: Array of DATA - Array of transaction objects, each object is
--   a byte list (DATA) representing TransactionType || TransactionPayload or
--   LegacyTransaction as defined in EIP-2718
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/paris.md#executionpayloadv1
--
data ExecutionPayloadV1 = ExecutionPayloadV1
    { _executionPayloadV1ParentHash :: !E.ParentHash
        -- ^ parentHash: DATA, 32 Bytes
    , _executionPayloadV1FeeRecipient :: !E.Beneficiary
        -- ^ feeRecipient:  DATA, 20 Bytes
    , _executionPayloadV1StateRoot :: !E.StateRoot
        -- ^ stateRoot: DATA, 32 Bytes
    , _executionPayloadV1ReceiptsRoot :: !E.ReceiptsRoot
        -- ^ receiptsRoot: DATA, 32 Bytes
    , _executionPayloadV1LogsBloom :: !E.Bloom
        -- ^ logsBloom: DATA, 256 Bytes
    , _executionPayloadV1PrevRandao :: !Randao
        -- ^ prevRandao: DATA, 32 Bytes
    , _executionPayloadV1BlockNumber :: !E.BlockNumber
        -- ^ blockNumber: QUANTITY, 64 Bits
    , _executionPayloadV1GasLimit :: !E.GasLimit
        -- ^ gasLimit: QUANTITY, 64 Bits
    , _executionPayloadV1GasUsed :: !E.GasUsed
        -- ^ gasUsed: QUANTITY, 64 Bits
    , _executionPayloadV1Timestamp :: !E.Timestamp
        -- ^ timestamp: QUANTITY, 64 Bits
    , _executionPayloadV1ExtraData :: !E.ExtraData
        -- ^ extraData: DATA, 0 to 32 Bytes
    , _executionPayloadV1BaseFeePerGas :: !BaseFeePerGas
        -- ^ baseFeePerGas: QUANTITY, 256 Bits
    , _executionPayloadV1BlockHash :: !E.BlockHash
        -- ^ blockHash: DATA, 32 Bytes
    , _executionPayloadV1Transactions :: ![TransactionBytes]
        -- ^ transactions: Array of DATA - Array of transaction objects, each
        -- object is a byte list (DATA) representing TransactionType ||
        -- TransactionPayload or LegacyTransaction as defined in EIP-2718
    }
    deriving (Show, Eq, Generic)

executionPayloadV1Properties
    :: KeyValue e kv
    => ExecutionPayloadV1
    -> [kv]
executionPayloadV1Properties o =
    [ "parentHash" .= _executionPayloadV1ParentHash o
    , "feeRecipient" .= _executionPayloadV1FeeRecipient o
    , "stateRoot" .= _executionPayloadV1StateRoot o
    , "receiptsRoot" .= _executionPayloadV1ReceiptsRoot o
    , "logsBloom" .= _executionPayloadV1LogsBloom o
    , "prevRandao" .= _executionPayloadV1PrevRandao o
    , "blockNumber" .= _executionPayloadV1BlockNumber o
    , "gasLimit" .= _executionPayloadV1GasLimit o
    , "gasUsed" .= _executionPayloadV1GasUsed o
    , "timestamp" .= _executionPayloadV1Timestamp o
    , "extraData" .= _executionPayloadV1ExtraData o
    , "baseFeePerGas" .= _executionPayloadV1BaseFeePerGas o
    , "blockHash" .= _executionPayloadV1BlockHash o
    , "transactions" .= _executionPayloadV1Transactions o
    ]

instance ToJSON ExecutionPayloadV1 where
    toEncoding = pairs . mconcat . executionPayloadV1Properties
    toJSON = object . executionPayloadV1Properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON ExecutionPayloadV1 where
    parseJSON = withObject "ExecutionPayloadV2" $ \o -> ExecutionPayloadV1
        <$> o .: "parentHash"
        <*> o .: "feeRecipient"
        <*> o .: "stateRoot"
        <*> o .: "receiptsRoot"
        <*> o .: "logsBloom"
        <*> o .: "prevRandao"
        <*> o .: "blockNumber"
        <*> o .: "gasLimit"
        <*> o .: "gasUsed"
        <*> o .: "timestamp"
        <*> o .: "extraData"
        <*> o .: "baseFeePerGas"
        <*> o .: "blockHash"
        <*> o .: "transactions"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Execution Payload V2

-- | Execution Payload V2
--
-- This structure has the syntax of ExecutionPayloadV1 and appends a single
-- field: withdrawals.
--
-- * withdrawals: Array of WithdrawalV1 - Array of withdrawals, each object is
--   an OBJECT containing the fields of a WithdrawalV1 structure.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#executionpayloadv2
--
data ExecutionPayloadV2 = ExecutionPayloadV2
    { _executionPayloadV1 :: !ExecutionPayloadV1
    , _executionPayloadV2Withdrawals :: ![WithdrawalV1]
        -- ^ withdrawals: Array of WithdrawalV1 - Array of withdrawals, each
        -- object is an OBJECT containing the fields of a WithdrawalV1 structure.
    }
    deriving (Show, Eq, Generic)

executionPayloadV2Properties
    :: KeyValue e kv
    => ExecutionPayloadV2
    -> [kv]
executionPayloadV2Properties o =
    executionPayloadV1Properties (_executionPayloadV1 o) <>
    [ "withdrawals" .= _executionPayloadV2Withdrawals o
    ]
{-# INLINABLE executionPayloadV2Properties #-}

instance ToJSON ExecutionPayloadV2 where
    toEncoding = pairs . mconcat . executionPayloadV2Properties
    toJSON = object . executionPayloadV2Properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON ExecutionPayloadV2 where
    parseJSON = withObject "ExecutionPayloadV2" $ \o -> ExecutionPayloadV2
        <$> parseJSON (Object o)
        <*> o .: "withdrawals"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Execution Payload V3

-- | Execution Payload V3
--
-- This structure has the syntax of ExecutionPayloadV2 and appends the new
-- fields: blobGasUsed and excessBlobGas.
--
-- * blobGasUsed: QUANTITY, 64 Bits
-- * excessBlobGas: QUANTITY, 64 Bits
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/cancun.md#executionpayloadv3
--
data ExecutionPayloadV3 = ExecutionPayloadV3
    { _executionPayloadV2 :: !ExecutionPayloadV2
    , _executionPayloadV3BlobGasUsed :: !BlobGasUsed
        -- ^ QUANTITY, 64 Bits
    , _executionPayloadV3ExcessBlobGas :: !ExcessBlobGas
        -- ^ QUANTITY, 64 Bits
    }
    deriving (Show, Eq, Generic)

executionPayloadV3Properties
    :: KeyValue e kv
    => ExecutionPayloadV3
    -> [kv]
executionPayloadV3Properties o =
    executionPayloadV2Properties (_executionPayloadV2 o) <>
    [ "blobGasUsed" .= _executionPayloadV3BlobGasUsed o
    , "excessBlobGas" .= _executionPayloadV3ExcessBlobGas o
    ]
{-# INLINABLE executionPayloadV3Properties #-}

instance ToJSON ExecutionPayloadV3 where
    toEncoding = pairs . mconcat . executionPayloadV3Properties
    toJSON = object . executionPayloadV3Properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON ExecutionPayloadV3 where
    parseJSON = withObject "ExecutionPayloadV3" $ \o -> ExecutionPayloadV3
        <$> parseJSON (Object o)
        <*> o .: "blobGasUsed"
        <*> o .: "excessBlobGas"

-- -------------------------------------------------------------------------- --
-- Full Execution Payload

-- | This is the "full" execution payload that is used in the engine_newPayloadV4
-- requests.
--
-- Method parameter list is extended with executionRequests.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/prague.md#request
--
data NewPayloadV4Request = NewPayloadV4Request
    { _newPayloadV4RequestExecutionPayloadV3 :: !ExecutionPayloadV3
    , _newPayloadV4RequestExpectedBlobVersionedHashes :: ![VersionedHash]
        -- ^ Array of DATA, 32 Bytes - Array of expected blob versioned hashes
        -- to validate.
    , _newPayloadV4RequestParentBeaconBlockRoot :: !ParentBeaconBlockRoot
        -- ^ DATA, 32 Bytes - Root of the parent beacon block.
    , _newPayloadV4RequestRequests :: ![ExecutionRequest]
        -- ^ Array of DATA - List of execution layer triggered requests. Each
        -- list element is a requests byte array as defined by EIP-7685. The
        -- first byte of each element is the request_type and the remaining
        -- bytes are the request_data. Elements of the list MUST be ordered by
        -- request_type in ascending order. Elements with empty request_data
        -- MUST be excluded from the list. If the list has no elements, the
        -- expected array MUST be []. If any element is out of order, has a
        -- length of 1-byte or shorter, or more than one element has the same
        -- type byte, or the param is null, client software MUST return -32602:
        -- Invalid params error.
    }
    deriving (Eq, Show, Generic)

instance ToJSON NewPayloadV4Request where
    toEncoding o = toEncoding
        ( _newPayloadV4RequestExecutionPayloadV3 o
        , _newPayloadV4RequestExpectedBlobVersionedHashes o
        , _newPayloadV4RequestParentBeaconBlockRoot o
        , _newPayloadV4RequestRequests o
        )
    toJSON o = toJSON
        ( _newPayloadV4RequestExecutionPayloadV3 o
        , _newPayloadV4RequestExpectedBlobVersionedHashes o
        , _newPayloadV4RequestParentBeaconBlockRoot o
        , _newPayloadV4RequestRequests o
        )
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON NewPayloadV4Request where
    parseJSON = withArray "NewPayloadV4Request" $ \v -> do
        case toList v of
            [a, b, c, d] -> NewPayloadV4Request
                <$> parseJSON a
                <*> parseJSON b
                <*> parseJSON c
                <*> parseJSON d
            l -> fail $ "invalid NewPayloadV4Request: expected 4 parameters but got " <> show (length l)
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Payload Attributes V1

-- | Payload Attributes V1
--
-- This structure contains the attributes required to initiate a payload build
-- process in the context of an engine_forkchoiceUpdated call.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/paris.md#payloadattributesv1
--
data PayloadAttributesV1 = PayloadAttributesV1
    { _payloadAttributesV1Timestamp :: !E.Timestamp
        -- ^ timestamp: QUANTITY, 64 Bits - value for the timestamp field of the
        -- new payload
    , _payloadAttributesV1PrevRandao :: !Randao
        -- ^ prevRandao: DATA, 32 Bytes - value for the prevRandao field of the
        -- new payload.
    , _payloadAttributesV1SuggestedFeeRecipient :: !E.Address
        -- ^ suggestedFeeRecipient: DATA, 20 Bytes - suggested value for the
        -- feeRecipient field of the new payload
    }
    deriving (Show, Eq, Generic)

payloadAttributesV1Properties
    :: KeyValue e kv
    => PayloadAttributesV1
    -> [kv]
payloadAttributesV1Properties o =
    [ "timestamp" .= _payloadAttributesV1Timestamp o
    , "prevRandao" .= _payloadAttributesV1PrevRandao o
    , "suggestedFeeRecipient" .= _payloadAttributesV1SuggestedFeeRecipient o
    ]

instance ToJSON PayloadAttributesV1 where
    toEncoding = pairs . mconcat . payloadAttributesV1Properties
    toJSON = object . payloadAttributesV1Properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON PayloadAttributesV1 where
    parseJSON = withObject "PayloadAttributesV1" $ \o -> PayloadAttributesV1
        <$> o .: "timestamp"
        <*> o .: "prevRandao"
        <*> o .: "suggestedFeeRecipient"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Payload Atributes V2

-- | Payload Attributes V2
--
-- This structure has the syntax of PayloadAttributesV1 and appends a single
-- field: withdrawals
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#payloadattributesv2
--
data PayloadAttributesV2 = PayloadAttributesV2
    { _payloadAttributesV1 :: !PayloadAttributesV1
    , _payloadAttributesV2Withdrawals :: ![WithdrawalV1]
        -- ^ withdrawals: Array of WithdrawalV1 - Array of withdrawals, each
        -- object is an OBJECT containing the fields of a WithdrawalV1
        -- structure.
    }
    deriving (Show, Eq, Generic)

payloadAttributesV2Properties
    :: KeyValue e kv
    => PayloadAttributesV2
    -> [kv]
payloadAttributesV2Properties o =
    payloadAttributesV1Properties (_payloadAttributesV1 o) <>
    [ "withdrawals" .= _payloadAttributesV2Withdrawals o
    ]

instance ToJSON PayloadAttributesV2 where
    toEncoding = pairs . mconcat . payloadAttributesV2Properties
    toJSON = object . payloadAttributesV2Properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON PayloadAttributesV2 where
    parseJSON = withObject "PayloadAttributesV2" $ \o -> PayloadAttributesV2
        <$> parseJSON (Object o)
        <*> o .: "withdrawals"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Payload Attributes V3

-- | Payload Attributes Object V3
--
-- This structure has the syntax of PayloadAttributesV2 and appends a single
-- field: parentBeaconBlockRoot.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/cancun.md#payloadattributesv3
--
data PayloadAttributesV3 = PayloadAttributesV3
    { _payloadAttributesV2 :: PayloadAttributesV2
    , _payloadAttributesV3parentBeaconBlockRoot :: !ParentBeaconBlockRoot
        -- ^ parentBeaconBlockRoot: DATA, 32 Bytes - Root of the parent beacon
        -- block.
    }
    deriving (Show, Eq, Generic)

payloadAttributeV3Properties
    :: KeyValue e kv
    => PayloadAttributesV3
    -> [kv]
payloadAttributeV3Properties o =
    payloadAttributesV2Properties (_payloadAttributesV2 o) <>
    [ "parentBeaconBlockRoot" .= _payloadAttributesV3parentBeaconBlockRoot o
    ]

instance ToJSON PayloadAttributesV3 where
    toEncoding = pairs . mconcat . payloadAttributeV3Properties
    toJSON = object . payloadAttributeV3Properties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON PayloadAttributesV3 where
    parseJSON = withObject "PayloadAttributesV3" $ \o -> PayloadAttributesV3
        <$> parseJSON (Object o)
        <*> o .: "parentBeaconBlockRoot"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Payload Id

-- | Payload Id
--
-- payloadId: DATA|null, 8 Bytes - identifier of the payload build process or
-- null
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#request-2
--
newtype PayloadId = PayloadId { _payloadId :: E.BytesN 8 }
    deriving (Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON) via JsonTextRepresentation "PayloadId" PayloadId

instance HasTextRepresentation PayloadId where
    toText = toText . HexBytes . _payloadId
    fromText = fmap (PayloadId . fromHexBytes) . fromText

-- -------------------------------------------------------------------------- --
-- Errors

-- | Engine Errors
--
-- * -32700 | Parse error | Invalid JSON was received by the server.
-- * -32600 | Invalid Request | The JSON sent is not a valid Request object.
-- * -32601 | Method not found | The method does not exist / is not available.
-- * -32602 | Invalid params | Invalid method parameter(s).
-- * -32603 | Internal error | Internal JSON-RPC error.
-- * -32000 | Server error | Generic client error while processing request.
-- * -38001 | Unknown payload | Payload does not exist / is not available.
-- * -38002 | Invalid forkchoice state | Forkchoice state is invalid / inconsistent.
-- * -38003 | Invalid payload attributes | Payload attributes are invalid / inconsistent.
-- * -38004 | Too large request | Number of requested entities is too large.
-- * -38005 | Unsupported fork | Payload belongs to a fork that is not supported.
--
-- Each error returns a null data value, except -32000 which returns the data
-- object with a err member that explains the error encountered.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/common.md#errors
--
data EngineErrors
    = UnknownPayload
    | InvalidForkChoiceState
    | InvalidPayloadAttributes
    | TooLargeRequest
    | UnsupportedFork
    deriving (Show, Eq, Generic, Enum, Bounded)

instance HasErrorCode EngineErrors where
    toErrorCode UnknownPayload = -38001
    toErrorCode InvalidForkChoiceState = -38002
    toErrorCode InvalidPayloadAttributes = -38003
    toErrorCode TooLargeRequest = -38004
    toErrorCode UnsupportedFork = -38005

    fromErrorCode (-38001) = pure UnknownPayload
    fromErrorCode (-38002) = pure InvalidForkChoiceState
    fromErrorCode (-38003) = pure InvalidPayloadAttributes
    fromErrorCode (-38004) = pure TooLargeRequest
    fromErrorCode (-38005) = pure UnsupportedFork
    fromErrorCode n = throwM $ UnknownErrorCodeException n

data EngineServerErrors
    = EngineServerError
    deriving (Show, Eq, Generic, Enum, Bounded)

instance HasErrorCode EngineServerErrors where
    toErrorCode EngineServerError = -32000
    fromErrorCode (-32000) = pure EngineServerError
    fromErrorCode n = throwM $ UnknownErrorCodeException n

-- -------------------------------------------------------------------------- --
-- Forkchoice Update V2 (Shanghai)

-- | Engine Forkchoice Updated V2
--
-- timeout: 8s
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#engine_forkchoiceupdatedv2
--
instance JsonRpcMethod "engine_forkchoiceUpdatedV2" where
    type MethodRequest "engine_forkchoiceUpdatedV2" = ForkchoiceUpdatedV2Request
    type MethodResponse "engine_forkchoiceUpdatedV2" = ForkchoiceUpdatedV1Response
    type ServerErrors "engine_forkchoiceUpdatedV2" = EngineServerErrors
    type ApplicationErrors "engine_forkchoiceUpdatedV2" = EngineErrors
    responseTimeoutMs = Just 8000
    methodErrors =
        [ ApplicationError InvalidForkChoiceState
        , ApplicationError InvalidPayloadAttributes
        ]

-- | Engine ForkchoiceUpdatedV2 Request
--
-- timeout: 8s
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#request-1
--
data ForkchoiceUpdatedV2Request = ForkchoiceUpdatedV2Request
    { _forkchoiceUpdatedV2RequestState :: !ForkchoiceStateV1
        -- ^ forkchoiceState: Object - instance of ForkchoiceStateV1
    , _forkchoiceUpdatedV2RequestPayloadAttributes :: !(Maybe PayloadAttributesV2)
        -- ^ payloadAttributes: Object|null - PayloadAttributesV2 or null.
        --
        -- Client software MUST return -32602: Invalid params error if the wrong
        -- version of the structure is used in the method call.
    }
    deriving (Show, Eq, Generic)

instance ToJSON ForkchoiceUpdatedV2Request where
    toEncoding o = toEncoding
        ( _forkchoiceUpdatedV2RequestState o
        , _forkchoiceUpdatedV2RequestPayloadAttributes o
        )
    toJSON o = toJSON
        ( _forkchoiceUpdatedV2RequestState o
        , _forkchoiceUpdatedV2RequestPayloadAttributes o
        )
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON ForkchoiceUpdatedV2Request where
    parseJSON = withArray "ForkchoiceUpdatedV2Request" $ \v -> do
        case toList v of
            [a, b] -> ForkchoiceUpdatedV2Request <$> parseJSON a <*> parseJSON b
            l -> fail $ "invalid ForkchoiceUpdatedV2Request: expected 2 parameters but got " <> show (length l)
    {-# INLINE parseJSON #-}


-- | Engine ForkchoiceUpdatedV1 response
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/paris.md#response-1
--
-- error: code and message set in case an exception happens while the validating
-- payload, updating the forkchoice or initiating the payload build process.
--
data ForkchoiceUpdatedV1Response = ForkchoiceUpdateV1Response
    { _forkchoiceUpdatedV1ResponsePayloadStatus :: !PayloadStatusV1
        -- ^ payloadStatus: PayloadStatusV1; values of the status field in the
        -- context of this method are restricted to the following subset:
        -- "VALID" "INVALID" "SYNCING"
    , _forkchoiceUpdatedV1ResponsePayloadId :: !(Maybe PayloadId)
        -- ^ payloadId: DATA|null, 8 Bytes - identifier of the payload build
        -- process or null
    }
    deriving (Show, Eq, Generic)

instance ToJSON ForkchoiceUpdatedV1Response where
    toEncoding o = pairs
        $ "payloadStatus" .= _forkchoiceUpdatedV1ResponsePayloadStatus o
        <> "payloadId" .= _forkchoiceUpdatedV1ResponsePayloadId o
    {-# INLINE toEncoding #-}

    toJSON o = object
        [ "payloadStatus" .= _forkchoiceUpdatedV1ResponsePayloadStatus o
        , "payloadId" .= _forkchoiceUpdatedV1ResponsePayloadId o
        ]
    {-# INLINE toJSON #-}

instance FromJSON ForkchoiceUpdatedV1Response where
    parseJSON = withObject "ForkchoiceUpdatedV1Response" $ \o -> ForkchoiceUpdateV1Response
        <$> o .: "payloadStatus"
        <*> o .: "payloadId"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Forkchoice Update V3 (Cancun)

type Engine_ForkchoiceUpdatedV3 = "engine_forkchoiceUpdatedV3"

-- | Engine Forkchoice Updated V3
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/cancun.md#engine_forkchoiceupdatedv3
--
-- Client software MUST verify that forkchoiceState matches the
-- ForkchoiceStateV1 structure and return -32602: Invalid params on failure.
--
instance JsonRpcMethod "engine_forkchoiceUpdatedV3" where
    type MethodRequest "engine_forkchoiceUpdatedV3" = ForkchoiceUpdatedV3Request
    type MethodResponse "engine_forkchoiceUpdatedV3" = ForkchoiceUpdatedV1Response
    type ServerErrors "engine_forkchoiceUpdatedV3" = EngineServerErrors
    type ApplicationErrors "engine_forkchoiceUpdatedV3" = EngineErrors
    responseTimeoutMs = Just 8000
    methodErrors =
        [ InvalidParams
        , ApplicationError InvalidForkChoiceState
        , ApplicationError InvalidPayloadAttributes
        , ApplicationError UnsupportedFork
        ]

-- | Engine ForkchoiceUpdatedV3 Request
--
-- timeout: 8s
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/cancun.md#request-1
--
data ForkchoiceUpdatedV3Request = ForkchoiceUpdatedV3Request
    { _forkchoiceUpdatedV3RequestState :: !ForkchoiceStateV1
        -- ^ forkchoiceState: ForkchoiceStateV1.
    , _forkchoiceUpdatedV3RequestPayloadAttributes :: !(Maybe PayloadAttributesV3)
        -- ^ payloadAttributes: Object|null - Instance of PayloadAttributesV3 or null.
    }
    deriving (Show, Eq, Generic)

instance ToJSON ForkchoiceUpdatedV3Request where
    toEncoding o = toEncoding
        ( _forkchoiceUpdatedV3RequestState o
        , _forkchoiceUpdatedV3RequestPayloadAttributes o
        )
    toJSON o = toJSON
        ( _forkchoiceUpdatedV3RequestState o
        , _forkchoiceUpdatedV3RequestPayloadAttributes o
        )

instance FromJSON ForkchoiceUpdatedV3Request where
    parseJSON = withArray "ForkchoiceUpdatedV3Request" $ \v -> do
        case toList v of
            [a, b] -> ForkchoiceUpdatedV3Request <$> parseJSON a <*> parseJSON b
            l -> fail $ "invalid ForkchoiceUpdatedV2Request: expected 2 parameters but got " <> show (length l)

-- -------------------------------------------------------------------------- --
-- Engine Get Payload V2 (Shanghai)

type Engine_GetPayloadV2 = "engine_getPayloadV2"

-- | Engine Get Payload V2
--
-- timeout: 1s
--
-- error: code and message set in case an exception happens while getting the
-- payload.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#engine_getpayloadv2
--
instance JsonRpcMethod "engine_getPayloadV2" where
    type MethodRequest "engine_getPayloadV2" = PayloadId
    type MethodResponse "engine_getPayloadV2" = GetPayloadV2Response
    type ServerErrors "engine_getPayloadV2" = EngineServerErrors
    type ApplicationErrors "engine_getPayloadV2" = EngineErrors
    responseTimeoutMs = Just 1000
    methodErrors =
        [ ApplicationError UnknownPayload
        ]

-- | Engine Get Payload V2 Response
--
-- error: code and message set in case an exception happens while getting the
-- payload.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#response-2
--
data GetPayloadV2Response = GetPayloadV2Response
    { _getPayloadV2ResponseExecutionPayload :: !ExecutionPayloadV2
        -- ^ executionPayload: ExecutionPayloadV2
    , _getPayloadV2ResponseBlockValue :: !BlockValue
        -- ^ blockValue : QUANTITY, 256 Bits - The expected value to be received
        -- by the feeRecipient in wei
    }
    deriving (Show, Eq, Generic)

getPayloadV2ResponseProperties
    :: KeyValue e kv
    => GetPayloadV2Response
    -> [kv]
getPayloadV2ResponseProperties o =
    [ "executionPayload" .= _getPayloadV2ResponseExecutionPayload o
    , "blockValue" .= _getPayloadV2ResponseBlockValue o
    ]

instance ToJSON GetPayloadV2Response where
    toEncoding = pairs . mconcat . getPayloadV2ResponseProperties
    toJSON = object . getPayloadV2ResponseProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON GetPayloadV2Response where
    parseJSON = withObject "GetPayloadV2Response" $ \o -> GetPayloadV2Response
        <$> o .: "executionPayload"
        <*> o .: "blockValue"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Engine Get Payload V3 (Cancun)

type Engine_GetPayloadV3 = "engine_getPayloadV3"

-- | Engine Get Payload V3
--
-- The response of this method is extended with BlobsBundleV1 containing the
-- blobs, their respective KZG commitments and proofs corresponding to the
-- versioned_hashes included in the blob transactions of the execution payload.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/cancun.md#engine_getpayloadv3
--
instance JsonRpcMethod "engine_getPayloadV3" where
    type MethodRequest "engine_getPayloadV3" = [PayloadId]
    type MethodResponse "engine_getPayloadV3" = GetPayloadV3Response
    type ServerErrors "engine_getPayloadV3" = EngineServerErrors
    type ApplicationErrors "engine_getPayloadV3" = EngineErrors
    responseTimeoutMs = Just 1000
    methodErrors =
        [ ApplicationError UnknownPayload
        , ApplicationError UnsupportedFork
        ]

-- | Engine Get Payload V3 Response
--
-- error: code and message set in case an exception happens while getting the
-- payload.
--
-- * blobsBundle: BlobsBundleV1 - Bundle with data corresponding to blob
--   transactions included into executionPayload
-- * shouldOverrideBuilder : BOOLEAN - Suggestion from the execution layer to
--   use this executionPayload instead of an externally provided one
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/shanghai.md#response-2
--
data GetPayloadV3Response = GetPayloadV3Response
    { _getPayloadV3ResponseExecutionPayload :: !ExecutionPayloadV3
        -- ^ executionPayload: ExecutionPayloadV2
    , _getPayloadV3ResponseBlockValue :: !BlockValue
        -- ^ blockValue : QUANTITY, 256 Bits - The expected value to be received
        -- by the feeRecipient in wei
    , _getPayloadV3ResponseBlobsBundle :: !BlobsBundleV1
        -- ^ blobsBundle: BlobsBundleV1 - Bundle with data corresponding to blob
        -- transactions included into executionPayload
    , _getPayloadV3ResponseShouldOverrideBuilder :: !Bool
        -- ^ shouldOverrideBuilder : BOOLEAN - Suggestion from the execution layer to
        -- use this executionPayload instead of an externally provided one
    }
    deriving (Show, Eq, Generic)

getPayloadV3ResponseProperties
    :: KeyValue e kv
    => GetPayloadV3Response
    -> [kv]
getPayloadV3ResponseProperties o =
    [ "executionPayload" .= _getPayloadV3ResponseExecutionPayload o
    , "blockValue" .= _getPayloadV3ResponseBlockValue o
    , "blobsBundle" .= _getPayloadV3ResponseBlobsBundle o
    , "shouldOverrideBuilder" .= _getPayloadV3ResponseShouldOverrideBuilder o
    ]

instance ToJSON GetPayloadV3Response where
    toEncoding = pairs . mconcat . getPayloadV3ResponseProperties
    toJSON = object . getPayloadV3ResponseProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON GetPayloadV3Response where
    parseJSON = withObject "GetPayloadV3Response" $ \o -> GetPayloadV3Response
        <$> o .: "executionPayload"
        <*> o .: "blockValue"
        <*> o .: "blobsBundle"
        <*> o .: "shouldOverrideBuilder"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Engine Get Payload V4 (Prague / Pectra)

type Engine_GetPayloadV4 = "engine_getPayloadV4"

-- | Engine Get Payload V4
--
-- The response of this method is extended with the executionRequests field.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/prague.md#engine_getpayloadv4
--
instance JsonRpcMethod "engine_getPayloadV4" where
    type MethodRequest "engine_getPayloadV4" = [PayloadId]
    type MethodResponse "engine_getPayloadV4" = GetPayloadV4Response
    type ServerErrors "engine_getPayloadV4" = EngineServerErrors
    type ApplicationErrors "engine_getPayloadV4" = EngineErrors
    responseTimeoutMs = Just 1000
    methodErrors =
        [ ApplicationError UnknownPayload
        , ApplicationError UnsupportedFork
        ]

-- | Engine Get Payload V4 Response
--
-- New fields:
--
-- * executionRequests
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/prague.md#response-1
--
data GetPayloadV4Response = GetPayloadV4Response
    { _getPayloadV4ResponseExecutionPayload :: !ExecutionPayloadV3
        -- ^ executionPayload: ExecutionPayloadV2
    , _getPayloadV4ResponseBlockValue :: !BlockValue
        -- ^ blockValue : QUANTITY, 256 Bits - The expected value to be received
        -- by the feeRecipient in wei
    , _getPayloadV4ResponseBlobsBundle :: !BlobsBundleV1
        -- ^ blobsBundle: BlobsBundleV1 - Bundle with data corresponding to blob
        -- transactions included into executionPayload
    , _getPayloadV4ResponseShouldOverrideBuilder :: !Bool
        -- ^ shouldOverrideBuilder : BOOLEAN - Suggestion from the execution layer
        -- to use this executionPayload instead of an externally provided one
    , _getPayloadV4ResponseExecutionRequests :: ![ExecutionRequest]
        -- ^ executionRequests: Array of DATA - Execution layer triggered requests
        -- obtained from the executionPayload transaction execution.
    }
    deriving (Show, Eq, Generic)

getPayloadV4ResponseProperties
    :: KeyValue e kv
    => GetPayloadV4Response
    -> [kv]
getPayloadV4ResponseProperties o =
    [ "executionPayload" .= _getPayloadV4ResponseExecutionPayload o
    , "blockValue" .= _getPayloadV4ResponseBlockValue o
    , "blobsBundle" .= _getPayloadV4ResponseBlobsBundle o
    , "shouldOverrideBuilder" .= _getPayloadV4ResponseShouldOverrideBuilder o
    , "executionRequests" .= _getPayloadV4ResponseExecutionRequests o
    ]

instance ToJSON GetPayloadV4Response where
    toEncoding = pairs . mconcat . getPayloadV4ResponseProperties
    toJSON = object . getPayloadV4ResponseProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON GetPayloadV4Response where
    parseJSON = withObject "GetPayloadV4Response" $ \o -> GetPayloadV4Response
        <$> o .: "executionPayload"
        <*> o .: "blockValue"
        <*> o .: "blobsBundle"
        <*> o .: "shouldOverrideBuilder"
        <*> o .: "executionRequests"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Engine New Payload V1 (Paris)

-- | Engine New Payload V1
--
-- The purpose of this method to inform the execution layer about a new payload
-- without demanding validation in case that it is not extending the current
-- head of the canonical chain. In any case, as long as the hash is valid and
-- ancestors are present, the payload is accepted and cached for possible future
-- validation.
--
-- timeout: 8s

-- -------------------------------------------------------------------------- --
-- Engine New Payload V4 (Prague / Pectra)

type Engine_NewPayloadV4 = "engine_newPayloadV4"

-- | Engine New Payload V4 Response
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/prague.md#response
--
-- This is the same type as PayloadStatusV1, with:
--
-- result: PayloadStatusV1, values of the status field are restricted in the
-- following way: INVALID_BLOCK_HASH status value is supplanted by INVALID.
--
-- error: code and message set in case an exception happens while processing the
-- payload.
--
instance JsonRpcMethod "engine_newPayloadV4" where
    type MethodRequest "engine_newPayloadV4" = NewPayloadV4Request
    type MethodResponse "engine_newPayloadV4" = PayloadStatusV1
    type ServerErrors "engine_newPayloadV4" = EngineServerErrors
    type ApplicationErrors "engine_newPayloadV4" = EngineErrors
    responseTimeoutMs = Just 8000
    methodErrors =
        [ InvalidParams
        , ApplicationError UnsupportedFork
        ]

-- -------------------------------------------------------------------------- --
-- Authentication

-- | JSON Web Token (JWT) HMAC+SHA256 (HS256) Secret
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/authentication.md#jwt-specifications
--
newtype JwtSecret = JwtSecret { _jwtSecret :: E.BytesN 32 }
    deriving (Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON) via (JsonTextRepresentation "JwtSecret" JwtSecret)

instance HasTextRepresentation JwtSecret where
    toText = T.drop 2 . toText . HexBytes . _jwtSecret
    fromText = fmap (JwtSecret . fromHexBytes) . fromText . ("0x" <>)

-- | JSON Web Token (JWT) used by the Ethereum Engine API
--
-- The only mandatory claim is the "iat" claim.
--
-- cf. https://github.com/ethereum/execution-apis/blob/main/src/engine/authentication.md#jwt-claims
--
getJwtToken :: JwtSecret -> IO T.Text
getJwtToken secret = jwtToken secret <$> getPOSIXTime

jwtToken :: JwtSecret -> POSIXTime -> T.Text
jwtToken (JwtSecret secret) t =
    T.intercalate "." [header, claim, signature]
  where
    header = b64 "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
    claim = b64 $ "{\"iat\":" <> sshow (round @_ @Natural t) <> "}"
    signature = b64
        $ BA.convert
        $ hmac @_ @_ @SHA256 (E.bytes secret)
        $ T.encodeUtf8
        $ T.intercalate "." [header, claim]
    b64 = encodeB64UrlNoPaddingText

-- | Default Engine Context
--
mkSimpleEngineCtx :: JwtSecret -> IO JsonRpcHttpCtx
mkSimpleEngineCtx secret = do
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    return $ JsonRpcHttpCtx
        { _jsonRpcHttpCtxManager = mgr
        , _jsonRpcHttpCtxURI = [uri|http://localhost:8551|]
        , _jsonRpcHttpCtxMakeBearerToken = Just (getJwtToken secret)
        }

-- | Create a new Engine Context
--
-- This initializes a new connection manager for this context. Hence, there
-- should only ever be a single context used with a given URI.
--
mkEngineCtx :: JwtSecret -> URI -> IO JsonRpcHttpCtx
mkEngineCtx secret u = do
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    return $ JsonRpcHttpCtx
        { _jsonRpcHttpCtxManager = mgr
        , _jsonRpcHttpCtxURI = u
        , _jsonRpcHttpCtxMakeBearerToken = Just (getJwtToken secret)
        }

-- -------------------------------------------------------------------------- --
-- Example
--
-- >>> Just s = fromText @JwtSecret "10b45e8907ab12dd750f688733e73cf433afadfd2f270e5b75a6b8fff22dd352"
-- >>> ctx <- mkSimpleEngineCtx s
-- >>> Just hdr <- callMethodHttp @"eth_getBlockByNumber" ctx (DefaultBlockLatest, False)
-- >>> let b = blockHash hdr
-- >>> callMethodHttp @"engine_forkchoiceUpdatedV3" ctx $ ForkchoiceUpdatedV3Request (ForkchoiceStateV1 b b b) Nothing
--
