{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.BlockPayloadHash
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.BlockPayloadHash
( BlockPayloadHash
, BlockPayloadHash_(..)
, encodeBlockPayloadHash
, decodeBlockPayloadHash
, nullBlockPayloadHash

-- * Ranked Block Payload Hash
, type RankedBlockPayloadHash
, pattern RankedBlockPayloadHash
, _rankedBlockPayloadHashHash
, _rankedBlockPayloadHashHeight
, encodeRankedBlockPayloadHash
, decodeRankedBlockPayloadHash
) where

import Control.DeepSeq
import Control.Monad

import Data.Aeson
import Data.ByteArray qualified as BA
import Data.Hashable

import GHC.Generics (Generic)

-- internal modules

import Chainweb.BlockHeight
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Ranked
import Chainweb.Utils
import Chainweb.Utils.Serialization

-- -------------------------------------------------------------------------- --
-- BlockPayloadHash

-- | The Merkle root of a block payload evaluation
--
-- NOTE: for historic reasons this is called `BlockPayloadHash`. A more accurate
-- name would be `PayloadEvaluationHash`.
--
-- This is computed by payload provider of the respective block payload. It is
-- treated by Chainweb consensus as the root of a Chainweb Merkle (sub-) tree.
-- It is the responsibility of the payload provider that this interpretation is
-- cryptographically sound.
--
-- Semantically, the hash must completely authenticate the block payload and
-- payload evaluation results, including all updates to the internal state of
-- the payload provider (but not complete state itself).
--
-- It is not required to authenticate the complete internal state of the payload
-- provider. (Although it is strongly recommended that payload providers support
-- this by including a state root into the payload Merkle tree. Pact currently
-- does not support this.)
--
-- Beside of unambiguously authenticating the evaluation of the payload, it is
-- up to the respective payload provider to decide what cryptographic protocol
-- is used to compute this value and what can be proven about the payload.
--
-- Binary format: 32 bytes.
--
type BlockPayloadHash = BlockPayloadHash_ ChainwebMerkleHashAlgorithm

newtype BlockPayloadHash_ a = BlockPayloadHash (MerkleLogHash a)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (BA.ByteArrayAccess)
    deriving newtype (Hashable, ToJSON, FromJSON)
    deriving newtype (ToJSONKey, FromJSONKey)

encodeBlockPayloadHash :: BlockPayloadHash_ a -> Put
encodeBlockPayloadHash (BlockPayloadHash w) = encodeMerkleLogHash w

decodeBlockPayloadHash
    :: MerkleHashAlgorithm a
    => Get (BlockPayloadHash_ a)
decodeBlockPayloadHash = BlockPayloadHash <$!> decodeMerkleLogHash

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag (BlockPayloadHash_ a) where
    type Tag (BlockPayloadHash_ a) = 'BlockPayloadHashTag
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

instance HasTextRepresentation BlockPayloadHash where
  toText (BlockPayloadHash h) = toText h
  fromText = fmap BlockPayloadHash . fromText
  {-# INLINE toText #-}
  {-# INLINE fromText #-}

nullBlockPayloadHash :: MerkleHashAlgorithm a => BlockPayloadHash_ a
nullBlockPayloadHash = BlockPayloadHash nullHashBytes
{-# INLINE nullBlockPayloadHash #-}

-- -------------------------------------------------------------------------- --
-- Ranked Block Payload Hash

type RankedBlockPayloadHash = Ranked BlockPayloadHash

pattern RankedBlockPayloadHash
    :: BlockHeight
    -> BlockPayloadHash
    -> RankedBlockPayloadHash
pattern RankedBlockPayloadHash
    { _rankedBlockPayloadHashHeight
    , _rankedBlockPayloadHashHash
    }
    = Ranked _rankedBlockPayloadHashHeight _rankedBlockPayloadHashHash
{-# COMPLETE RankedBlockPayloadHash #-}

encodeRankedBlockPayloadHash :: RankedBlockPayloadHash -> Put
encodeRankedBlockPayloadHash = encodeRanked encodeBlockPayloadHash

decodeRankedBlockPayloadHash :: Get RankedBlockPayloadHash
decodeRankedBlockPayloadHash = decodeRanked decodeBlockPayloadHash

