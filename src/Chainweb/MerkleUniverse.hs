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

import Crypto.Hash.Algorithms

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

type ChainwebMerkleHashAlgorithm = SHA512t_256

-- -------------------------------------------------------------------------- --
-- Chainweb Merkle Universe

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
    type MerkleTagVal ChainwebHashTag 'BlockNonceTag = 0x00020

    -- Event Proofs
    type MerkleTagVal ChainwebHashTag 'OutputEventsTag = 0x00030
    type MerkleTagVal ChainwebHashTag 'BlockEventsHashTag = 0x00031
    type MerkleTagVal ChainwebHashTag 'RequestKeyTag = 0x00032
    type MerkleTagVal ChainwebHashTag 'PactEventTag = 0x00034

instance HashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Void where
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

