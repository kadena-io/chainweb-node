{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.MerkleUniverse
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The Merkle Universe for the Chainweb Merkle Tree. One must define a
-- 'ChainwebHashTag' for every type that appears as a node in the Chainweb
-- Merkle Tree.
--
module Chainweb.MerkleUniverse
( ChainwebHashTag(..)
, ChainwebMerkleHashAlgorithm

-- * Merkle Root Types
, MerkleRootType(..)
, merkleRootTypeToText
, merkleRootTypeFromText
, MerkleRootMismatch(..)

, fromSomeTagVal
, fromTagVal
, Sing (..)
, pattern STagVal
, sTagVal
) where

import Control.DeepSeq
import Control.Monad.Catch

import Data.Aeson
import Data.Hash.SHA2
import Data.Kind
import Data.Singletons
import Data.Text qualified as T
import Data.Void

import GHC.Generics
import GHC.Stack

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.Utils
import Control.Monad
import Data.Word
import GHC.TypeNats
import Data.Type.Equality
import Unsafe.Coerce

-- -------------------------------------------------------------------------- --
-- Chainweb Merkle Hash Algorithm

type ChainwebMerkleHashAlgorithm = Sha2_512_256

-- -------------------------------------------------------------------------- --
-- Chainweb Merkle Universe

-- | The closed kind of tags for Leaf Nodes in the Chainweb Merkle Tree.
--
-- IMPORTANT NOTE:
--
-- A tag MUST uniquely identify each particular use of a type in the Merkle
-- Tree. NEVER EVER reuse a tag at a different place in the tree.
--
-- Merkle Proofs for the Chainweb Merkle tree witness the existence of a given
-- tagged value anywhere in the tree. If the same tagged value is used in
-- in different roles in multiple places in the tree, the proof will be
-- ambiguous.
--
-- Particular care must be taken for types that are used as roots of the tree
-- (MerkleRoot :: MerkleNotType). It is generally assumed that the preimage of
-- of the respective hashes are computed from leafs that themselfs are tagged.
-- If an attack can control the preimage of a root hash, they can create valid
-- proofs for arbtirary values.
--
data ChainwebHashTag
    = VoidTag
    | MerkleRootTag
    | ChainIdTag
    | BlockHeightTag
    | BlockWeightTag
    | BlockPayloadHashTag
    | BlockNonceTag
    | BlockCreationTimeTag
    | ChainwebVersionTag
    | PowHashTag
    | BlockHashTag
    | HashTargetTag
    | TransactionTag
    | TransactionOutputTag
    | BlockTransactionsHashTag
    | BlockOutputsHashTag
    | MinerDataTag
    | CoinbaseOutputTag
    | EpochStartTimeTag
    | FeatureFlagsTag

    -- Event Proofs
    | OutputEventsTag
    | BlockEventsHashTag
    | RequestKeyTag
    | PactEventTag

    -- Minimal Payload Provider
    | MinimalPayloadTag

    -- Ethereum EL
    | EthParentHashTag
    | EthOmmersHashTag
    | EthBeneficiaryTag
    | EthStateRootTag
    | EthTransactionsRootTag
    | EthReceiptsRootTag
    | EthBloomTag
    | EthDifficultyTag
    | EthBlockNumberTag
    | EthGasLimitTag
    | EthGasUsedTag
    | EthTimestampTag
    | EthExtraDataTag
    | EthRandaoTag
    | EthNonceTag
    | EthBaseFeePerGasTag
    | EthWithdrawalsRootTag
    | EthBlobGasUsedTag
    | EthExcessBlobGasTag
    | EthParentBeaconBlockRootTag
    | EthReceiptTag
    | EthRequestsHashTag
    deriving (Show, Eq, Bounded, Enum)

instance MerkleUniverse ChainwebHashTag where
    type MerkleTagVal ChainwebHashTag 'VoidTag = 0x0000
    type MerkleTagVal ChainwebHashTag 'MerkleRootTag = 0x0001
    type MerkleTagVal ChainwebHashTag 'ChainIdTag = 0x0002
    type MerkleTagVal ChainwebHashTag 'BlockHeightTag = 0x0003
    type MerkleTagVal ChainwebHashTag 'BlockWeightTag = 0x0004
    type MerkleTagVal ChainwebHashTag 'BlockPayloadHashTag = 0x0005
    type MerkleTagVal ChainwebHashTag 'FeatureFlagsTag = 0x0006
    type MerkleTagVal ChainwebHashTag 'BlockCreationTimeTag = 0x0007
    type MerkleTagVal ChainwebHashTag 'ChainwebVersionTag = 0x0008
    type MerkleTagVal ChainwebHashTag 'PowHashTag = 0x0009
    type MerkleTagVal ChainwebHashTag 'BlockHashTag = 0x0010
    type MerkleTagVal ChainwebHashTag 'HashTargetTag = 0x0011
    type MerkleTagVal ChainwebHashTag 'TransactionTag = 0x0013
    type MerkleTagVal ChainwebHashTag 'TransactionOutputTag = 0x0014
    type MerkleTagVal ChainwebHashTag 'BlockTransactionsHashTag = 0x0015
    type MerkleTagVal ChainwebHashTag 'BlockOutputsHashTag = 0x0016
    type MerkleTagVal ChainwebHashTag 'MinerDataTag = 0x0017
    type MerkleTagVal ChainwebHashTag 'CoinbaseOutputTag = 0x0018
    type MerkleTagVal ChainwebHashTag 'EpochStartTimeTag = 0x0019
    type MerkleTagVal ChainwebHashTag 'BlockNonceTag = 0x0020

    -- Event Proofs
    type MerkleTagVal ChainwebHashTag 'OutputEventsTag = 0x0030
    type MerkleTagVal ChainwebHashTag 'BlockEventsHashTag = 0x0031
    type MerkleTagVal ChainwebHashTag 'RequestKeyTag = 0x0032
    type MerkleTagVal ChainwebHashTag 'PactEventTag = 0x0034

    -- Minimal Payload Provider
    type MerkleTagVal ChainwebHashTag 'MinimalPayloadTag = 0x0035

    -- Ethereum EL
    type MerkleTagVal ChainwebHashTag 'EthParentHashTag = 0x0040
    type MerkleTagVal ChainwebHashTag 'EthOmmersHashTag = 0x0041
    type MerkleTagVal ChainwebHashTag 'EthBeneficiaryTag = 0x0042
    type MerkleTagVal ChainwebHashTag 'EthStateRootTag = 0x0043
    type MerkleTagVal ChainwebHashTag 'EthTransactionsRootTag = 0x0044
    type MerkleTagVal ChainwebHashTag 'EthReceiptsRootTag = 0x0045
    type MerkleTagVal ChainwebHashTag 'EthBloomTag = 0x0046
    type MerkleTagVal ChainwebHashTag 'EthDifficultyTag = 0x0047
    type MerkleTagVal ChainwebHashTag 'EthBlockNumberTag = 0x0048
    type MerkleTagVal ChainwebHashTag 'EthGasLimitTag = 0x0049
    type MerkleTagVal ChainwebHashTag 'EthGasUsedTag = 0x004a
    type MerkleTagVal ChainwebHashTag 'EthTimestampTag = 0x004b
    type MerkleTagVal ChainwebHashTag 'EthExtraDataTag = 0x004c
    type MerkleTagVal ChainwebHashTag 'EthRandaoTag = 0x004d
    type MerkleTagVal ChainwebHashTag 'EthNonceTag = 0x004e
    type MerkleTagVal ChainwebHashTag 'EthBaseFeePerGasTag = 0x004f
    type MerkleTagVal ChainwebHashTag 'EthWithdrawalsRootTag = 0x0050
    type MerkleTagVal ChainwebHashTag 'EthBlobGasUsedTag = 0x0051
    type MerkleTagVal ChainwebHashTag 'EthExcessBlobGasTag = 0x0052
    type MerkleTagVal ChainwebHashTag 'EthParentBeaconBlockRootTag = 0x0053
    type MerkleTagVal ChainwebHashTag 'EthReceiptTag = 0x0054
    type MerkleTagVal ChainwebHashTag 'EthRequestsHashTag = 0x0055

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Void where
    type Tag Void = 'VoidTag
    toMerkleNode = \case
    fromMerkleNode _ = throwM
        $ MerkleLogDecodeException "can't deserialize value of type Void"

-- -------------------------------------------------------------------------- --
-- Merkle Root Types

data MerkleRootType
    = RootBlock
    | RootBlockPayload
    | RootBlockEvents
    deriving (Show, Eq, Ord, Generic, NFData)

merkleRootTypeToText :: HasCallStack => MerkleRootType -> T.Text
merkleRootTypeToText RootBlock = "block"
merkleRootTypeToText RootBlockPayload = "blockPayload"
merkleRootTypeToText RootBlockEvents = "blockEvents"

merkleRootTypeFromText :: MonadThrow m => T.Text -> m MerkleRootType
merkleRootTypeFromText "block" = pure RootBlock
merkleRootTypeFromText "blockPayload" = pure RootBlockPayload
merkleRootTypeFromText "blockEvents" = pure RootBlockEvents
merkleRootTypeFromText t = throwM . TextFormatException $ "Unknown merkle root type " <> t

instance HasTextRepresentation MerkleRootType where
    toText = merkleRootTypeToText
    {-# INLINE toText #-}
    fromText = merkleRootTypeFromText
    {-# INLINE fromText #-}

instance ToJSON MerkleRootType where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON MerkleRootType where
    parseJSON = parseJsonFromText "MerkleRootType"
    {-# INLINE parseJSON #-}

data MerkleRootMismatch = MerkleRootMismatch
    { _merkleRootMismatchExpected :: !(Expected MerkleRootType)
    , _merkleRootMismatchActual :: !(Actual MerkleRootType)
    }
    deriving (Show, Eq, Ord, Generic, NFData)

instance Exception MerkleRootMismatch

-- -------------------------------------------------------------------------- --
-- Singletons

data instance Sing :: ChainwebHashTag -> Type where
    SVoidTag
        :: SNat (MerkleTagVal ChainwebHashTag VoidTag)
        -> Sing 'VoidTag
    SMerkleRootTag
        :: SNat (MerkleTagVal ChainwebHashTag MerkleRootTag)
        -> Sing 'MerkleRootTag
    SChainIdTag
        :: SNat (MerkleTagVal ChainwebHashTag ChainIdTag)
        -> Sing 'ChainIdTag
    SBlockHeightTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockHeightTag)
        -> Sing 'BlockHeightTag
    SBlockWeightTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockWeightTag)
        -> Sing 'BlockWeightTag
    SBlockPayloadHashTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockPayloadHashTag)
        -> Sing 'BlockPayloadHashTag
    SFeatureFlagsTag
        :: SNat (MerkleTagVal ChainwebHashTag FeatureFlagsTag)
        -> Sing 'FeatureFlagsTag
    SBlockCreationTimeTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockCreationTimeTag)
        -> Sing 'BlockCreationTimeTag
    SChainwebVersionTag
        :: SNat (MerkleTagVal ChainwebHashTag ChainwebVersionTag)
        -> Sing 'ChainwebVersionTag
    SPowHashTag
        :: SNat (MerkleTagVal ChainwebHashTag PowHashTag)
        -> Sing 'PowHashTag
    SBlockHashTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockHashTag)
        -> Sing 'BlockHashTag
    SHashTargetTag
        :: SNat (MerkleTagVal ChainwebHashTag HashTargetTag)
        -> Sing 'HashTargetTag
    STransactionTag
        :: SNat (MerkleTagVal ChainwebHashTag TransactionTag)
        -> Sing 'TransactionTag
    STransactionOutputTag
        :: SNat (MerkleTagVal ChainwebHashTag TransactionOutputTag)
        -> Sing 'TransactionOutputTag
    SBlockTransactionsHashTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockTransactionsHashTag)
        -> Sing 'BlockTransactionsHashTag
    SBlockOutputsHashTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockOutputsHashTag)
        -> Sing 'BlockOutputsHashTag
    SMinerDataTag
        :: SNat (MerkleTagVal ChainwebHashTag MinerDataTag)
        -> Sing 'MinerDataTag
    SCoinbaseOutputTag
        :: SNat (MerkleTagVal ChainwebHashTag CoinbaseOutputTag)
        -> Sing 'CoinbaseOutputTag
    SEpochStartTimeTag
        :: SNat (MerkleTagVal ChainwebHashTag EpochStartTimeTag)
        -> Sing 'EpochStartTimeTag
    SBlockNonceTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockNonceTag)
        -> Sing 'BlockNonceTag
    SOutputEventsTag
        :: SNat (MerkleTagVal ChainwebHashTag OutputEventsTag)
        -> Sing 'OutputEventsTag
    SBlockEventsHashTag
        :: SNat (MerkleTagVal ChainwebHashTag BlockEventsHashTag)
        -> Sing 'BlockEventsHashTag
    SRequestKeyTag
        :: SNat (MerkleTagVal ChainwebHashTag RequestKeyTag)
        -> Sing 'RequestKeyTag
    SPactEventTag
        :: SNat (MerkleTagVal ChainwebHashTag PactEventTag)
        -> Sing 'PactEventTag
    SMinimalPayloadTag
        :: SNat (MerkleTagVal ChainwebHashTag MinimalPayloadTag)
        -> Sing 'MinimalPayloadTag
    SEthParentHashTag
        :: SNat (MerkleTagVal ChainwebHashTag EthParentHashTag)
        -> Sing 'EthParentHashTag
    SEthOmmersHashTag
        :: SNat (MerkleTagVal ChainwebHashTag EthOmmersHashTag)
        -> Sing 'EthOmmersHashTag
    SEthBeneficiaryTag
        :: SNat (MerkleTagVal ChainwebHashTag EthBeneficiaryTag)
        -> Sing 'EthBeneficiaryTag
    SEthStateRootTag
        :: SNat (MerkleTagVal ChainwebHashTag EthStateRootTag)
        -> Sing 'EthStateRootTag
    SEthTransactionsRootTag
        :: SNat (MerkleTagVal ChainwebHashTag EthTransactionsRootTag)
        -> Sing 'EthTransactionsRootTag
    SEthReceiptsRootTag
        :: SNat (MerkleTagVal ChainwebHashTag EthReceiptsRootTag)
        -> Sing 'EthReceiptsRootTag
    SEthBloomTag
        :: SNat (MerkleTagVal ChainwebHashTag EthBloomTag)
        -> Sing 'EthBloomTag
    SEthDifficultyTag
        :: SNat (MerkleTagVal ChainwebHashTag EthDifficultyTag)
        -> Sing 'EthDifficultyTag
    SEthBlockNumberTag
        :: SNat (MerkleTagVal ChainwebHashTag EthBlockNumberTag)
        -> Sing 'EthBlockNumberTag
    SEthGasLimitTag
        :: SNat (MerkleTagVal ChainwebHashTag EthGasLimitTag)
        -> Sing 'EthGasLimitTag
    SEthGasUsedTag
        :: SNat (MerkleTagVal ChainwebHashTag EthGasUsedTag)
        -> Sing 'EthGasUsedTag
    SEthTimestampTag
        :: SNat (MerkleTagVal ChainwebHashTag EthTimestampTag)
        -> Sing 'EthTimestampTag
    SEthExtraDataTag
        :: SNat (MerkleTagVal ChainwebHashTag EthExtraDataTag)
        -> Sing 'EthExtraDataTag
    SEthRandaoTag
        :: SNat (MerkleTagVal ChainwebHashTag EthRandaoTag)
        -> Sing 'EthRandaoTag
    SEthNonceTag
        :: SNat (MerkleTagVal ChainwebHashTag EthNonceTag)
        -> Sing 'EthNonceTag
    SEthBaseFeePerGasTag
        :: SNat (MerkleTagVal ChainwebHashTag EthBaseFeePerGasTag)
        -> Sing 'EthBaseFeePerGasTag
    SEthWithdrawalsRootTag
        :: SNat (MerkleTagVal ChainwebHashTag EthWithdrawalsRootTag)
        -> Sing 'EthWithdrawalsRootTag
    SEthBlobGasUsedTag
        :: SNat (MerkleTagVal ChainwebHashTag EthBlobGasUsedTag)
        -> Sing 'EthBlobGasUsedTag
    SEthExcessBlobGasTag
        :: SNat (MerkleTagVal ChainwebHashTag EthExcessBlobGasTag)
        -> Sing 'EthExcessBlobGasTag
    SEthParentBeaconBlockRootTag
        :: SNat (MerkleTagVal ChainwebHashTag EthParentBeaconBlockRootTag)
        -> Sing 'EthParentBeaconBlockRootTag
    SEthReceiptTag
        :: SNat (MerkleTagVal ChainwebHashTag EthReceiptTag)
        -> Sing 'EthReceiptTag
    SEthRequestsHashTag
        :: SNat (MerkleTagVal ChainwebHashTag EthRequestsHashTag)
        -> Sing 'EthRequestsHashTag

deriving instance Show (Sing (a :: ChainwebHashTag))

instance Eq (Sing (a :: ChainwebHashTag)) where
    _ == _ = True

instance TestEquality (Sing @ChainwebHashTag) where
    testEquality a b = case testEquality (sTagVal a) (sTagVal b) of
        Just Refl -> Just $ unsafeCoerce Refl
            -- This is justified by the injectivity of 'MerkleTagVal'
        Nothing -> Nothing

sTagVal :: Sing (a :: ChainwebHashTag) -> SNat (MerkleTagVal ChainwebHashTag a)
sTagVal (SVoidTag n) = n
sTagVal (SMerkleRootTag n) = n
sTagVal (SChainIdTag n) = n
sTagVal (SBlockHeightTag n) = n
sTagVal (SBlockWeightTag n) = n
sTagVal (SBlockPayloadHashTag n) = n
sTagVal (SFeatureFlagsTag n) = n
sTagVal (SBlockCreationTimeTag n) = n
sTagVal (SChainwebVersionTag n) = n
sTagVal (SPowHashTag n) = n
sTagVal (SBlockHashTag n) = n
sTagVal (SHashTargetTag n) = n
sTagVal (STransactionTag n) = n
sTagVal (STransactionOutputTag n) = n
sTagVal (SBlockTransactionsHashTag n) = n
sTagVal (SBlockOutputsHashTag n) = n
sTagVal (SMinerDataTag n) = n
sTagVal (SCoinbaseOutputTag n) = n
sTagVal (SEpochStartTimeTag n) = n
sTagVal (SBlockNonceTag n) = n
sTagVal (SOutputEventsTag n) = n
sTagVal (SBlockEventsHashTag n) = n
sTagVal (SRequestKeyTag n) = n
sTagVal (SPactEventTag n) = n
sTagVal (SMinimalPayloadTag n) = n
sTagVal (SEthParentHashTag n) = n
sTagVal (SEthOmmersHashTag n) = n
sTagVal (SEthBeneficiaryTag n) = n
sTagVal (SEthStateRootTag n) = n
sTagVal (SEthTransactionsRootTag n) = n
sTagVal (SEthReceiptsRootTag n) = n
sTagVal (SEthBloomTag n) = n
sTagVal (SEthDifficultyTag n) = n
sTagVal (SEthBlockNumberTag n) = n
sTagVal (SEthGasLimitTag n) = n
sTagVal (SEthGasUsedTag n) = n
sTagVal (SEthTimestampTag n) = n
sTagVal (SEthExtraDataTag n) = n
sTagVal (SEthRandaoTag n) = n
sTagVal (SEthNonceTag n) = n
sTagVal (SEthBaseFeePerGasTag n) = n
sTagVal (SEthWithdrawalsRootTag n) = n
sTagVal (SEthBlobGasUsedTag n) = n
sTagVal (SEthExcessBlobGasTag n) = n
sTagVal (SEthParentBeaconBlockRootTag n) = n
sTagVal (SEthReceiptTag n) = n
sTagVal (SEthRequestsHashTag n) = n

pattern STagVal
    :: forall (a :: ChainwebHashTag)
    . SNat (MerkleTagVal ChainwebHashTag a)
    -> Sing a
pattern STagVal n <- (sTagVal -> n)
{-# COMPLETE STagVal #-}

instance SingI 'VoidTag where sing = SVoidTag SNat
instance SingI 'MerkleRootTag where sing = SMerkleRootTag SNat
instance SingI 'ChainIdTag where sing = SChainIdTag SNat
instance SingI 'BlockHeightTag where sing = SBlockHeightTag SNat
instance SingI 'BlockWeightTag where sing = SBlockWeightTag SNat
instance SingI 'BlockPayloadHashTag where sing = SBlockPayloadHashTag SNat
instance SingI 'FeatureFlagsTag where sing = SFeatureFlagsTag SNat
instance SingI 'BlockCreationTimeTag where sing = SBlockCreationTimeTag SNat
instance SingI 'ChainwebVersionTag where sing = SChainwebVersionTag SNat
instance SingI 'PowHashTag where sing = SPowHashTag SNat
instance SingI 'BlockHashTag where sing = SBlockHashTag SNat
instance SingI 'HashTargetTag where sing = SHashTargetTag SNat
instance SingI 'TransactionTag where sing = STransactionTag SNat
instance SingI 'TransactionOutputTag where sing = STransactionOutputTag SNat
instance SingI 'BlockTransactionsHashTag where sing = SBlockTransactionsHashTag SNat
instance SingI 'BlockOutputsHashTag where sing = SBlockOutputsHashTag SNat
instance SingI 'MinerDataTag where sing = SMinerDataTag SNat
instance SingI 'CoinbaseOutputTag where sing = SCoinbaseOutputTag SNat
instance SingI 'EpochStartTimeTag where sing = SEpochStartTimeTag SNat
instance SingI 'BlockNonceTag where sing = SBlockNonceTag SNat
instance SingI 'OutputEventsTag where sing = SOutputEventsTag SNat
instance SingI 'BlockEventsHashTag where sing = SBlockEventsHashTag SNat
instance SingI 'RequestKeyTag where sing = SRequestKeyTag SNat
instance SingI 'PactEventTag where sing = SPactEventTag SNat
instance SingI 'MinimalPayloadTag where sing = SMinimalPayloadTag SNat
instance SingI 'EthParentHashTag where sing = SEthParentHashTag SNat
instance SingI 'EthOmmersHashTag where sing = SEthOmmersHashTag SNat
instance SingI 'EthBeneficiaryTag where sing = SEthBeneficiaryTag SNat
instance SingI 'EthStateRootTag where sing = SEthStateRootTag SNat
instance SingI 'EthTransactionsRootTag where sing = SEthTransactionsRootTag SNat
instance SingI 'EthReceiptsRootTag where sing = SEthReceiptsRootTag SNat
instance SingI 'EthBloomTag where sing = SEthBloomTag SNat
instance SingI 'EthDifficultyTag where sing = SEthDifficultyTag SNat
instance SingI 'EthBlockNumberTag where sing = SEthBlockNumberTag SNat
instance SingI 'EthGasLimitTag where sing = SEthGasLimitTag SNat
instance SingI 'EthGasUsedTag where sing = SEthGasUsedTag SNat
instance SingI 'EthTimestampTag where sing = SEthTimestampTag SNat
instance SingI 'EthExtraDataTag where sing = SEthExtraDataTag SNat
instance SingI 'EthRandaoTag where sing = SEthRandaoTag SNat
instance SingI 'EthNonceTag where sing = SEthNonceTag SNat
instance SingI 'EthBaseFeePerGasTag where sing = SEthBaseFeePerGasTag SNat
instance SingI 'EthWithdrawalsRootTag where sing = SEthWithdrawalsRootTag SNat
instance SingI 'EthBlobGasUsedTag where sing = SEthBlobGasUsedTag SNat
instance SingI 'EthExcessBlobGasTag where sing = SEthExcessBlobGasTag SNat
instance SingI 'EthParentBeaconBlockRootTag where sing = SEthParentBeaconBlockRootTag SNat
instance SingI 'EthReceiptTag where sing = SEthReceiptTag SNat
instance SingI 'EthRequestsHashTag where sing = SEthRequestsHashTag SNat

instance SingKind ChainwebHashTag where
    type Demote ChainwebHashTag = ChainwebHashTag
    fromSing (SVoidTag SNat) = VoidTag
    fromSing (SMerkleRootTag SNat) = MerkleRootTag
    fromSing (SChainIdTag SNat) = ChainIdTag
    fromSing (SBlockHeightTag SNat) = BlockHeightTag
    fromSing (SBlockWeightTag SNat) = BlockWeightTag
    fromSing (SBlockPayloadHashTag SNat) = BlockPayloadHashTag
    fromSing (SFeatureFlagsTag SNat) = FeatureFlagsTag
    fromSing (SBlockCreationTimeTag SNat) = BlockCreationTimeTag
    fromSing (SChainwebVersionTag SNat) = ChainwebVersionTag
    fromSing (SPowHashTag SNat) = PowHashTag
    fromSing (SBlockHashTag SNat) = BlockHashTag
    fromSing (SHashTargetTag SNat) = HashTargetTag
    fromSing (STransactionTag SNat) = TransactionTag
    fromSing (STransactionOutputTag SNat) = TransactionOutputTag
    fromSing (SBlockTransactionsHashTag SNat) = BlockTransactionsHashTag
    fromSing (SBlockOutputsHashTag SNat) = BlockOutputsHashTag
    fromSing (SMinerDataTag SNat) = MinerDataTag
    fromSing (SCoinbaseOutputTag SNat) = CoinbaseOutputTag
    fromSing (SEpochStartTimeTag SNat) = EpochStartTimeTag
    fromSing (SBlockNonceTag SNat) = BlockNonceTag
    fromSing (SOutputEventsTag SNat) = OutputEventsTag
    fromSing (SBlockEventsHashTag SNat) = BlockEventsHashTag
    fromSing (SRequestKeyTag SNat) = RequestKeyTag
    fromSing (SPactEventTag SNat) = PactEventTag
    fromSing (SMinimalPayloadTag SNat) = MinimalPayloadTag
    fromSing (SEthParentHashTag SNat) = EthParentHashTag
    fromSing (SEthOmmersHashTag SNat) = EthOmmersHashTag
    fromSing (SEthBeneficiaryTag SNat) = EthBeneficiaryTag
    fromSing (SEthStateRootTag SNat) = EthStateRootTag
    fromSing (SEthTransactionsRootTag SNat) = EthTransactionsRootTag
    fromSing (SEthReceiptsRootTag SNat) = EthReceiptsRootTag
    fromSing (SEthBloomTag SNat) = EthBloomTag
    fromSing (SEthDifficultyTag SNat) = EthDifficultyTag
    fromSing (SEthBlockNumberTag SNat) = EthBlockNumberTag
    fromSing (SEthGasLimitTag SNat) = EthGasLimitTag
    fromSing (SEthGasUsedTag SNat) = EthGasUsedTag
    fromSing (SEthTimestampTag SNat) = EthTimestampTag
    fromSing (SEthExtraDataTag SNat) = EthExtraDataTag
    fromSing (SEthRandaoTag SNat) = EthRandaoTag
    fromSing (SEthNonceTag SNat) = EthNonceTag
    fromSing (SEthBaseFeePerGasTag SNat) = EthBaseFeePerGasTag
    fromSing (SEthWithdrawalsRootTag SNat) = EthWithdrawalsRootTag
    fromSing (SEthBlobGasUsedTag SNat) = EthBlobGasUsedTag
    fromSing (SEthExcessBlobGasTag SNat) = EthExcessBlobGasTag
    fromSing (SEthParentBeaconBlockRootTag SNat) = EthParentBeaconBlockRootTag
    fromSing (SEthReceiptTag SNat) = EthReceiptTag
    fromSing (SEthRequestsHashTag SNat) = EthRequestsHashTag

    toSing VoidTag = SomeSing (SVoidTag SNat)
    toSing MerkleRootTag = SomeSing (SMerkleRootTag SNat)
    toSing ChainIdTag = SomeSing (SChainIdTag SNat)
    toSing BlockHeightTag = SomeSing (SBlockHeightTag SNat)
    toSing BlockWeightTag = SomeSing (SBlockWeightTag SNat)
    toSing BlockPayloadHashTag = SomeSing (SBlockPayloadHashTag SNat)
    toSing FeatureFlagsTag = SomeSing (SFeatureFlagsTag SNat)
    toSing BlockCreationTimeTag = SomeSing (SBlockCreationTimeTag SNat)
    toSing ChainwebVersionTag = SomeSing (SChainwebVersionTag SNat)
    toSing PowHashTag = SomeSing (SPowHashTag SNat)
    toSing BlockHashTag = SomeSing (SBlockHashTag SNat)
    toSing HashTargetTag = SomeSing (SHashTargetTag SNat)
    toSing TransactionTag = SomeSing (STransactionTag SNat)
    toSing TransactionOutputTag = SomeSing (STransactionOutputTag SNat)
    toSing BlockTransactionsHashTag = SomeSing (SBlockTransactionsHashTag SNat)
    toSing BlockOutputsHashTag = SomeSing (SBlockOutputsHashTag SNat)
    toSing MinerDataTag = SomeSing (SMinerDataTag SNat)
    toSing CoinbaseOutputTag = SomeSing (SCoinbaseOutputTag SNat)
    toSing EpochStartTimeTag = SomeSing (SEpochStartTimeTag SNat)
    toSing BlockNonceTag = SomeSing (SBlockNonceTag SNat)
    toSing OutputEventsTag = SomeSing (SOutputEventsTag SNat)
    toSing BlockEventsHashTag = SomeSing (SBlockEventsHashTag SNat)
    toSing RequestKeyTag = SomeSing (SRequestKeyTag SNat)
    toSing PactEventTag = SomeSing (SPactEventTag SNat)
    toSing MinimalPayloadTag = SomeSing (SMinimalPayloadTag SNat)
    toSing EthParentHashTag = SomeSing (SEthParentHashTag SNat)
    toSing EthOmmersHashTag = SomeSing (SEthOmmersHashTag SNat)
    toSing EthBeneficiaryTag = SomeSing (SEthBeneficiaryTag SNat)
    toSing EthStateRootTag = SomeSing (SEthStateRootTag SNat)
    toSing EthTransactionsRootTag = SomeSing (SEthTransactionsRootTag SNat)
    toSing EthReceiptsRootTag = SomeSing (SEthReceiptsRootTag SNat)
    toSing EthBloomTag = SomeSing (SEthBloomTag SNat)
    toSing EthDifficultyTag = SomeSing (SEthDifficultyTag SNat)
    toSing EthBlockNumberTag = SomeSing (SEthBlockNumberTag SNat)
    toSing EthGasLimitTag = SomeSing (SEthGasLimitTag SNat)
    toSing EthGasUsedTag = SomeSing (SEthGasUsedTag SNat)
    toSing EthTimestampTag = SomeSing (SEthTimestampTag SNat)
    toSing EthExtraDataTag = SomeSing (SEthExtraDataTag SNat)
    toSing EthRandaoTag = SomeSing (SEthRandaoTag SNat)
    toSing EthNonceTag = SomeSing (SEthNonceTag SNat)
    toSing EthBaseFeePerGasTag = SomeSing (SEthBaseFeePerGasTag SNat)
    toSing EthWithdrawalsRootTag = SomeSing (SEthWithdrawalsRootTag SNat)
    toSing EthBlobGasUsedTag = SomeSing (SEthBlobGasUsedTag SNat)
    toSing EthExcessBlobGasTag = SomeSing (SEthExcessBlobGasTag SNat)
    toSing EthParentBeaconBlockRootTag = SomeSing (SEthParentBeaconBlockRootTag SNat)
    toSing EthReceiptTag = SomeSing (SEthReceiptTag SNat)
    toSing EthRequestsHashTag = SomeSing (SEthRequestsHashTag SNat)

tagList :: [ChainwebHashTag]
tagList = [minBound .. maxBound]

fromTagVal :: forall m . MonadThrow m => Word16 -> m ChainwebHashTag
fromTagVal x = case msum (f <$> tagList) of
    Just t -> return t
    Nothing -> throwM $ DecodeException $ "unknown tag value: " <> sshow x
  where
    f :: ChainwebHashTag -> Maybe ChainwebHashTag
    f t = case (toSing @ChainwebHashTag t) of
        (SomeSing (STagVal (SNat @s))) -> do
            guard (natVal_ @s == int x)
            Just t

fromSomeTagVal :: forall m . MonadThrow m => Word16 -> m (SomeSing ChainwebHashTag)
fromSomeTagVal x = case msum (f <$> tagList) of
    Just t -> return t
    Nothing -> throwM $ DecodeException $ "unknown tag value: " <> sshow x
  where
    f :: ChainwebHashTag -> Maybe (SomeSing ChainwebHashTag)
    f t = case (toSing @ChainwebHashTag t) of
        r@(SomeSing (STagVal (SNat @s))) -> do
            guard (natVal_ @s == int x)
            Just r

