{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
) where

import Control.DeepSeq
import Control.Monad.Catch

import Data.Hash.SHA2

import Data.Aeson
import qualified Data.Text as T
import Data.Void

import GHC.Generics
import GHC.Stack

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Chainweb Merkle Hash Algorithm

type ChainwebMerkleHashAlgorithm = Sha2_512_256

-- -------------------------------------------------------------------------- --
-- Chainweb Merkle Universe

-- | Tags for Leaf Nodes in the Chainweb Merkle Tree
--
-- IMPORTANT NOTE:
--
-- A tag MUST uniquely identify the each particular use of a type in the Merkle
-- Tree. NEVER EVER reuse a tag at a different place in the tree.
--
-- Merkle Proofs for the Chainweb Merkle tree witness the existence of a given
-- tagged value anywhere in the tree. If the same tagged value is used in
-- in different roles in multiple places in the tree, the proof will be
-- ambiguous.
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
    deriving (Show, Eq)

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
