{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.ExecutionPayload
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM.ExecutionPayload
( ExecutionPayloadData(..)
, executionPayloadDataTransactions
, executionPayloadDataWithdrawals
, executionPayloadDataExpectedBlobVersionedHashes
, executionPayloadDataRequests
, Payload(..)
, payloadHeader
, payloadData
, payloadToNewPayloadV4Request
, getPayloadV4ResponseToPayload
, executionPayloadV3ToHeader

-- * Getters
, _pldParentHash
, pldParentHash
, _pldOmmersHash
, pldOmmersHash
, _pldBeneficiary
, pldBeneficiary
, _pldStateRoot
, pldStateRoot
, _pldTransactionsRoot
, pldTransactionsRoot
, _pldReceiptsRoot
, pldReceiptsRoot
, _pldLogsBloom
, pldLogsBloom
, _pldDifficulty
, pldDifficulty
, _pldNumber
, pldNumber
, _pldHeight
, pldHeight
, _pldGasLimit
, pldGasLimit
, _pldGasUsed
, pldGasUsed
, _pldTimestamp
, pldTimestamp
, _pldExtraData
, pldExtraData
, _pldPrevRandao
, pldPrevRandao
, _pldNonce
, pldNonce
, _pldBaseFeePerGas
, pldBaseFeePerGas
, _pldWithdrawalsRoot
, pldWithdrawalsRoot
, _pldBlobGasUsed
, pldBlobGasUsed
, _pldExcessBlobGas
, pldExcessBlobGas
, _pldParentBeaconBlockRoot
, pldParentBeaconBlockRoot
, _pldRequestsHash
, pldRequestsHash
, _pldHash
, pldHash
, _pldPayloadHash
, pldPayloadHash
, _pldTransactions
, pldTransactions
, _pldWithdrawals
, pldWithdrawals
, _pldExpectedBlobVersionedHashes
, pldExpectedBlobVersionedHashes
, _pldRequests
, pldRequests
, _pldRankedBlockPayloadHash
, pldRankedBlockPayloadHash
) where

import Chainweb.PayloadProvider.EVM.EngineAPI
import Chainweb.BlockHeight as Chainweb
import Chainweb.BlockPayloadHash
import Chainweb.PayloadProvider.EVM.Header qualified as EVM
import Chainweb.PayloadProvider.EVM.Utils qualified as EVM
import Chainweb.PayloadProvider.EVM.Utils qualified as Utils
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Function (on)
import Data.Hashable
import Data.Word
import Ethereum.Misc
import Ethereum.RLP
import GHC.Generics (Generic)
import Chainweb.Utils (int)

-- -------------------------------------------------------------------------- --
-- | Execution Payload Data
--
-- This data structure contains the execution payload data as it is not already
-- included in the EVM Header. Together with the EVM Header this provides all
-- information needed to make a new payload request to the EVM Engine API.
--
-- The executiojn payload equals the execution payload header except for the
-- transactions and withdrawals fields, for which the header only contains the
-- respective Merkle roots. Internally we store
--
-- Deneb Execution Payload:
--
-- cf. https://github.com/ethereum/consensus-specs/blob/dev/specs/deneb/beacon-chain.md#executionpayload
--
-- We also include the fields of the newPayloadV4 engine API call from the
-- Pectra (Prague/Electra) fork.
--
-- cf. https://github.com/ethereum/consensus-specs/blob/dev/specs/electra/beacon-chain.md#modified-newpayloadrequest
--
data ExecutionPayloadData = ExecutionPayloadData
    { _executionPayloadDataTransactions :: ![TransactionBytes]
        -- ^ transactions: Array of DATA - Array of transaction objects, each
        -- object is a byte list (DATA) representing TransactionType ||
        -- TransactionPayload or LegacyTransaction as defined in EIP-2718
    , _executionPayloadDataWithdrawals :: ![WithdrawalV1]
        -- ^ withdrawals: Array of WithdrawalV1 - Array of withdrawals, each
        -- object is an OBJECT containing the fields of a WithdrawalV1 structure.
    , _executionPayloadDataExpectedBlobVersionedHashes :: ![VersionedHash]
        -- ^ Array of DATA, 32 Bytes - Array of expected blob versioned hashes
        -- to validate.
        --
        -- The actual blob data is not part of the execution paylaod data
        -- (Unlike transactions and withdrawals).
    , _executionPayloadDataRequests :: ![EVM.ExecutionRequest]
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
        --
        -- This field is new in the Pectra (Prague/Electra) fork.
    }
    deriving (Show, Eq, Generic)

makeLenses ''ExecutionPayloadData

instance RLP ExecutionPayloadData where
    putRlp o = putRlpL
        [ putRlp $ _executionPayloadDataTransactions o
        , putRlp $ _executionPayloadDataWithdrawals o
        , putRlp $ _executionPayloadDataExpectedBlobVersionedHashes o
        , putRlp $ _executionPayloadDataRequests o
        ]
    getRlp = label "ExcutionPayloadData" $ getRlpL $
        ExecutionPayloadData
            <$> getRlp -- transactions
            <*> getRlp -- withdrawals
            <*> getRlp -- expectedBlobVersionedHashes
            <*> getRlp -- requests
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

executionPayloadDataProperties
    :: KeyValue e kv
    => ExecutionPayloadData
    -> [kv]
executionPayloadDataProperties o =
    [ "transactions" .= _executionPayloadDataTransactions o
    , "withdrawals" .= _executionPayloadDataWithdrawals o
    , "expectedBlobVersionedHashes" .= _executionPayloadDataExpectedBlobVersionedHashes o
    , "executionRequests" .= _executionPayloadDataRequests o
    ]

instance ToJSON ExecutionPayloadData where
    toEncoding = pairs . mconcat . executionPayloadDataProperties
    toJSON = object . executionPayloadDataProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON ExecutionPayloadData where
    parseJSON = withObject "ExecutionPayloadData" $ \o -> ExecutionPayloadData
        <$> o .: "transactions"
        <*> o .: "withdrawals"
        <*> o .: "expectedBlobVersionedHashes"
        <*> o .: "executionRequests"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Payload

data Payload = Payload
    { _payloadHeader :: !EVM.Header
    , _payloadData :: !(Maybe ExecutionPayloadData)
    }
    deriving (Show, Generic)

makeLenses ''Payload

instance Eq Payload where
    (==) = (==) `on` _payloadHeader

instance Ord Payload where
    compare = compare `on` _payloadHeader

instance Hashable Payload where
    hashWithSalt s = hashWithSalt s . _payloadHeader

instance RLP Payload where
    putRlp (Payload hdr Nothing) = putRlpL
        [ putRlp @Word8 0
        , putRlp hdr
        ]
    putRlp (Payload hdr (Just pld)) = putRlpL
        [ putRlp @Word8 1
        , putRlp hdr
        , putRlp pld
        ]
    getRlp = label "Payload" $ getRlpL $ do
        getRlp @Word8 >>= \case
            0 -> Payload
                <$> getRlp -- header
                <*> pure Nothing
            1 -> Payload
                <$> getRlp -- header
                <*> (Just <$> getRlp) -- data
            _ -> fail "Chainweb.PayloadProvider.EVM.ExecutionPayload.getRlp: invalid payload type"
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

payloadProperties
    :: KeyValue e kv
    => Payload
    -> [kv]
payloadProperties o =
    [ "header" .= _payloadHeader o
    , "data" .= _payloadData o
    ]

instance ToJSON Payload where
    toEncoding = pairs . mconcat . payloadProperties
    toJSON = object . payloadProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Payload where
    parseJSON = withObject "Payload" $ \o -> Payload
        <$> o .: "header"
        <*> o .:? "data"
    {-# INLINE parseJSON #-}

payloadToNewPayloadV4Request
    :: Payload
    -> Maybe NewPayloadV4Request
payloadToNewPayloadV4Request (Payload _ Nothing) = Nothing
payloadToNewPayloadV4Request (Payload hdr (Just d)) = Just $ NewPayloadV4Request
    { _newPayloadV4RequestExecutionPayloadV3 = ExecutionPayloadV3
        { _executionPayloadV2 = ExecutionPayloadV2
            { _executionPayloadV1 = ExecutionPayloadV1
                { _executionPayloadV1ParentHash = EVM._hdrParentHash hdr
                , _executionPayloadV1FeeRecipient = EVM._hdrBeneficiary hdr
                , _executionPayloadV1StateRoot = EVM._hdrStateRoot hdr
                , _executionPayloadV1ReceiptsRoot = EVM._hdrReceiptsRoot hdr
                , _executionPayloadV1LogsBloom = EVM._hdrLogsBloom hdr
                , _executionPayloadV1BlockNumber = EVM._hdrNumber hdr
                , _executionPayloadV1GasLimit = EVM._hdrGasLimit hdr
                , _executionPayloadV1GasUsed = EVM._hdrGasUsed hdr
                , _executionPayloadV1Timestamp = EVM._hdrTimestamp hdr
                , _executionPayloadV1ExtraData = EVM._hdrExtraData hdr
                , _executionPayloadV1PrevRandao = EVM._hdrPrevRandao hdr
                , _executionPayloadV1BaseFeePerGas = EVM._hdrBaseFeePerGas hdr
                , _executionPayloadV1BlockHash = EVM._hdrHash hdr
                , _executionPayloadV1Transactions = _executionPayloadDataTransactions d
                }
            , _executionPayloadV2Withdrawals = _executionPayloadDataWithdrawals d
            }
        , _executionPayloadV3BlobGasUsed = EVM._hdrBlobGasUsed hdr
        , _executionPayloadV3ExcessBlobGas = EVM._hdrExcessBlobGas hdr
        }
    , _newPayloadV4RequestExpectedBlobVersionedHashes
        = _executionPayloadDataExpectedBlobVersionedHashes d
    , _newPayloadV4RequestParentBeaconBlockRoot =
        EVM._hdrParentBeaconBlockRoot hdr
    , _newPayloadV4RequestRequests = _executionPayloadDataRequests d
    }

getPayloadV4ResponseToPayload
    :: EVM.ParentBeaconBlockRoot
    -> GetPayloadV4Response
    -> Payload
getPayloadV4ResponseToPayload pbh resp = Payload
    { _payloadHeader = executionPayloadV3ToHeader pbh
        (_getPayloadV4ResponseExecutionPayload resp)
        (_getPayloadV4ResponseExecutionRequests resp)
    , _payloadData = Just $ ExecutionPayloadData
        { _executionPayloadDataTransactions = _executionPayloadV1Transactions v1
        , _executionPayloadDataWithdrawals = _executionPayloadV2Withdrawals v2
        , _executionPayloadDataExpectedBlobVersionedHashes =
            versionedHashes (_getPayloadV4ResponseBlobsBundle resp)
        , _executionPayloadDataRequests = _getPayloadV4ResponseExecutionRequests resp
        }
    }
  where
    v1 = _executionPayloadV1 v2
    v2 = _executionPayloadV2 v3
    v3 = _getPayloadV4ResponseExecutionPayload resp

executionPayloadV3ToHeader
    :: EVM.ParentBeaconBlockRoot
    -> ExecutionPayloadV3
    -> [Utils.ExecutionRequest]
    -> EVM.Header
executionPayloadV3ToHeader phdr v3 reqs = hdr
    { EVM._hdrHash = EVM.computeBlockHash hdr
    , EVM._hdrPayloadHash = EVM.computeBlockPayloadHash hdr
    }
  where
    v2 = _executionPayloadV2 v3
    v1 = _executionPayloadV1 v2
    hdr = EVM.Header
        { EVM._hdrParentHash = _executionPayloadV1ParentHash v1
        , EVM._hdrOmmersHash = EVM.ommersHash
        , EVM._hdrBeneficiary = _executionPayloadV1FeeRecipient v1
        , EVM._hdrStateRoot = _executionPayloadV1StateRoot v1
        , EVM._hdrTransactionsRoot = transactionsRoot (_executionPayloadV1Transactions v1)
        , EVM._hdrReceiptsRoot = _executionPayloadV1ReceiptsRoot v1
        , EVM._hdrLogsBloom = _executionPayloadV1LogsBloom v1
        , EVM._hdrDifficulty = EVM.difficulty
        , EVM._hdrNumber = _executionPayloadV1BlockNumber v1
        , EVM._hdrGasLimit = _executionPayloadV1GasLimit v1
        , EVM._hdrGasUsed = _executionPayloadV1GasUsed v1
        , EVM._hdrTimestamp = _executionPayloadV1Timestamp v1
        , EVM._hdrExtraData = _executionPayloadV1ExtraData v1
        , EVM._hdrPrevRandao = _executionPayloadV1PrevRandao v1
        , EVM._hdrNonce = EVM.nonce
        , EVM._hdrBaseFeePerGas = _executionPayloadV1BaseFeePerGas v1
        , EVM._hdrWithdrawalsRoot = withdrawlsRoot (_executionPayloadV2Withdrawals v2)
        , EVM._hdrBlobGasUsed = _executionPayloadV3BlobGasUsed v3
        , EVM._hdrExcessBlobGas = _executionPayloadV3ExcessBlobGas v3
        , EVM._hdrParentBeaconBlockRoot = phdr
        , EVM._hdrRequestsHash = EVM.requestsHash reqs
        , EVM._hdrHash = error "Chainweb.PayloadProvider.EVM.executionPayloadV3ToHeader: _hdrHash"
        , EVM._hdrPayloadHash = error "Chainweb.PayloadProvider.executionPayloadV3ToHeader: _hdrPayloadHash"
        }

-- -------------------------------------------------------------------------- --
-- Getters

_pldParentHash :: Payload -> ParentHash
_pldParentHash = EVM._hdrParentHash . _payloadHeader
{-# INLINE _pldParentHash #-}

pldParentHash :: Getter Payload ParentHash
pldParentHash = payloadHeader . EVM.hdrParentHash
{-# INLINE pldParentHash #-}

_pldOmmersHash :: Payload -> OmmersHash
_pldOmmersHash = EVM._hdrOmmersHash . _payloadHeader
{-# INLINE _pldOmmersHash #-}

pldOmmersHash :: Getter Payload OmmersHash
pldOmmersHash = payloadHeader . EVM.hdrOmmersHash
{-# INLINE pldOmmersHash #-}

_pldBeneficiary :: Payload -> Beneficiary
_pldBeneficiary = EVM._hdrBeneficiary . _payloadHeader
{-# INLINE _pldBeneficiary #-}

pldBeneficiary :: Getter Payload Beneficiary
pldBeneficiary = payloadHeader . EVM.hdrBeneficiary
{-# INLINE pldBeneficiary #-}

_pldStateRoot :: Payload -> StateRoot
_pldStateRoot = EVM._hdrStateRoot . _payloadHeader
{-# INLINE _pldStateRoot #-}

pldStateRoot :: Getter Payload StateRoot
pldStateRoot = payloadHeader . EVM.hdrStateRoot
{-# INLINE pldStateRoot #-}

_pldTransactionsRoot :: Payload -> TransactionsRoot
_pldTransactionsRoot = EVM._hdrTransactionsRoot . _payloadHeader
{-# INLINE _pldTransactionsRoot #-}

pldTransactionsRoot :: Getter Payload TransactionsRoot
pldTransactionsRoot = payloadHeader . EVM.hdrTransactionsRoot
{-# INLINE pldTransactionsRoot #-}

_pldReceiptsRoot :: Payload -> ReceiptsRoot
_pldReceiptsRoot = EVM._hdrReceiptsRoot . _payloadHeader
{-# INLINE _pldReceiptsRoot #-}

pldReceiptsRoot :: Getter Payload ReceiptsRoot
pldReceiptsRoot = payloadHeader . EVM.hdrReceiptsRoot
{-# INLINE pldReceiptsRoot #-}

_pldLogsBloom :: Payload -> Bloom
_pldLogsBloom = EVM._hdrLogsBloom . _payloadHeader
{-# INLINE _pldLogsBloom #-}

pldLogsBloom :: Getter Payload Bloom
pldLogsBloom = payloadHeader . EVM.hdrLogsBloom
{-# INLINE pldLogsBloom #-}

_pldDifficulty :: Payload -> Difficulty
_pldDifficulty = EVM._hdrDifficulty . _payloadHeader
{-# INLINE _pldDifficulty #-}

pldDifficulty :: Getter Payload Difficulty
pldDifficulty = payloadHeader . EVM.hdrDifficulty
{-# INLINE pldDifficulty #-}

_pldNumber :: Payload -> BlockNumber
_pldNumber = EVM._hdrNumber . _payloadHeader
{-# INLINE _pldNumber #-}

pldNumber :: Getter Payload BlockNumber
pldNumber = payloadHeader . EVM.hdrNumber
{-# INLINE pldNumber #-}

_pldHeight :: Payload -> Chainweb.BlockHeight
_pldHeight = EVM._hdrHeight . _payloadHeader
{-# INLINE _pldHeight #-}

pldHeight :: Getter Payload Chainweb.BlockHeight
pldHeight = payloadHeader . EVM.hdrHeight
{-# INLINE pldHeight #-}

_pldGasLimit :: Payload -> GasLimit
_pldGasLimit = EVM._hdrGasLimit . _payloadHeader
{-# INLINE _pldGasLimit #-}

pldGasLimit :: Getter Payload GasLimit
pldGasLimit = payloadHeader . EVM.hdrGasLimit
{-# INLINE pldGasLimit #-}

_pldGasUsed :: Payload -> GasUsed
_pldGasUsed = EVM._hdrGasUsed . _payloadHeader
{-# INLINE _pldGasUsed #-}

pldGasUsed :: Getter Payload GasUsed
pldGasUsed = payloadHeader . EVM.hdrGasUsed
{-# INLINE pldGasUsed #-}

_pldTimestamp :: Payload -> Timestamp
_pldTimestamp = EVM._hdrTimestamp . _payloadHeader
{-# INLINE _pldTimestamp #-}

pldTimestamp :: Getter Payload Timestamp
pldTimestamp = payloadHeader . EVM.hdrTimestamp
{-# INLINE pldTimestamp #-}

_pldExtraData :: Payload -> ExtraData
_pldExtraData = EVM._hdrExtraData . _payloadHeader
{-# INLINE _pldExtraData #-}

pldExtraData :: Getter Payload ExtraData
pldExtraData = payloadHeader . EVM.hdrExtraData
{-# INLINE pldExtraData #-}

_pldPrevRandao :: Payload -> EVM.Randao
_pldPrevRandao = EVM._hdrPrevRandao . _payloadHeader
{-# INLINE _pldPrevRandao #-}

pldPrevRandao :: Getter Payload EVM.Randao
pldPrevRandao = payloadHeader . EVM.hdrPrevRandao
{-# INLINE pldPrevRandao #-}

_pldNonce :: Payload -> Nonce
_pldNonce = EVM._hdrNonce . _payloadHeader
{-# INLINE _pldNonce #-}

pldNonce :: Getter Payload Nonce
pldNonce = payloadHeader . EVM.hdrNonce
{-# INLINE pldNonce #-}

_pldBaseFeePerGas :: Payload -> EVM.BaseFeePerGas
_pldBaseFeePerGas = EVM._hdrBaseFeePerGas . _payloadHeader
{-# INLINE _pldBaseFeePerGas #-}

pldBaseFeePerGas :: Getter Payload EVM.BaseFeePerGas
pldBaseFeePerGas = payloadHeader . EVM.hdrBaseFeePerGas
{-# INLINE pldBaseFeePerGas #-}

_pldWithdrawalsRoot :: Payload -> EVM.WithdrawalsRoot
_pldWithdrawalsRoot = EVM._hdrWithdrawalsRoot . _payloadHeader
{-# INLINE _pldWithdrawalsRoot #-}

pldWithdrawalsRoot :: Getter Payload EVM.WithdrawalsRoot
pldWithdrawalsRoot = payloadHeader . EVM.hdrWithdrawalsRoot
{-# INLINE pldWithdrawalsRoot #-}

_pldBlobGasUsed :: Payload -> EVM.BlobGasUsed
_pldBlobGasUsed = EVM._hdrBlobGasUsed . _payloadHeader
{-# INLINE _pldBlobGasUsed #-}

pldBlobGasUsed :: Getter Payload EVM.BlobGasUsed
pldBlobGasUsed = payloadHeader . EVM.hdrBlobGasUsed
{-# INLINE pldBlobGasUsed #-}

_pldExcessBlobGas :: Payload -> EVM.ExcessBlobGas
_pldExcessBlobGas = EVM._hdrExcessBlobGas . _payloadHeader
{-# INLINE _pldExcessBlobGas #-}

pldExcessBlobGas :: Getter Payload EVM.ExcessBlobGas
pldExcessBlobGas = payloadHeader . EVM.hdrExcessBlobGas
{-# INLINE pldExcessBlobGas #-}

_pldParentBeaconBlockRoot :: Payload -> EVM.ParentBeaconBlockRoot
_pldParentBeaconBlockRoot = EVM._hdrParentBeaconBlockRoot . _payloadHeader
{-# INLINE _pldParentBeaconBlockRoot #-}

pldParentBeaconBlockRoot :: Getter Payload EVM.ParentBeaconBlockRoot
pldParentBeaconBlockRoot = payloadHeader . EVM.hdrParentBeaconBlockRoot
{-# INLINE pldParentBeaconBlockRoot #-}

_pldRequestsHash :: Payload -> EVM.RequestsHash
_pldRequestsHash = EVM._hdrRequestsHash . _payloadHeader
{-# INLINE _pldRequestsHash #-}

pldRequestsHash :: Getter Payload EVM.RequestsHash
pldRequestsHash = payloadHeader . EVM.hdrRequestsHash
{-# INLINE pldRequestsHash #-}

_pldHash :: Payload -> BlockHash
_pldHash = EVM._hdrHash . _payloadHeader
{-# INLINE _pldHash #-}

pldHash :: Getter Payload BlockHash
pldHash = payloadHeader . EVM.hdrHash
{-# INLINE pldHash #-}

_pldPayloadHash :: Payload -> BlockPayloadHash
_pldPayloadHash = EVM._hdrPayloadHash . _payloadHeader
{-# INLINE _pldPayloadHash #-}

pldPayloadHash :: Getter Payload BlockPayloadHash
pldPayloadHash = payloadHeader . EVM.hdrPayloadHash
{-# INLINE pldPayloadHash #-}

_pldTransactions :: Payload -> Maybe [TransactionBytes]
_pldTransactions = fmap _executionPayloadDataTransactions . _payloadData
{-# INLINE _pldTransactions #-}

pldTransactions :: Getter Payload (Maybe [TransactionBytes])
pldTransactions = to _pldTransactions
{-# INLINE pldTransactions #-}

_pldWithdrawals :: Payload -> Maybe [WithdrawalV1]
_pldWithdrawals = fmap _executionPayloadDataWithdrawals . _payloadData
{-# INLINE _pldWithdrawals #-}

pldWithdrawals :: Getter Payload (Maybe [WithdrawalV1])
pldWithdrawals = to _pldWithdrawals
{-# INLINE pldWithdrawals #-}

_pldExpectedBlobVersionedHashes :: Payload -> Maybe [VersionedHash]
_pldExpectedBlobVersionedHashes = fmap _executionPayloadDataExpectedBlobVersionedHashes . _payloadData
{-# INLINE _pldExpectedBlobVersionedHashes #-}

pldExpectedBlobVersionedHashes :: Getter Payload (Maybe [VersionedHash])
pldExpectedBlobVersionedHashes = to _pldExpectedBlobVersionedHashes
{-# INLINE pldExpectedBlobVersionedHashes #-}

_pldRequests :: Payload -> Maybe [EVM.ExecutionRequest]
_pldRequests = fmap _executionPayloadDataRequests . _payloadData
{-# INLINE _pldRequests #-}

pldRequests :: Getter Payload (Maybe [EVM.ExecutionRequest])
pldRequests = to _pldRequests
{-# INLINE pldRequests #-}

_pldRankedBlockPayloadHash :: Payload -> RankedBlockPayloadHash
_pldRankedBlockPayloadHash pld = RankedBlockPayloadHash
    { _rankedBlockPayloadHashHeight = int $ EVM._hdrNumber $ _payloadHeader pld
    , _rankedBlockPayloadHashHash = EVM._hdrPayloadHash $ _payloadHeader pld
    }
{-# INLINE _pldRankedBlockPayloadHash #-}

pldRankedBlockPayloadHash :: Getter Payload RankedBlockPayloadHash
pldRankedBlockPayloadHash = to _pldRankedBlockPayloadHash
{-# INLINE pldRankedBlockPayloadHash #-}
