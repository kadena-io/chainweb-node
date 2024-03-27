-- Module: Chainweb.BlockHeader
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
module Chainweb.BlockHeader
(
-- * Newtype wrappers for function parameters
  ParentHeader(..)
, parentHeader
, ParentCreationTime(..)

-- * Block Payload Hash
, BlockPayloadHash
, BlockPayloadHash_(..)
, encodeBlockPayloadHash
, decodeBlockPayloadHash

-- * Nonce
, Nonce(..)
, encodeNonce
, encodeNonceToWord64
, decodeNonce

-- * EpochStartTime
, EpochStartTime(..)
, encodeEpochStartTime
, decodeEpochStartTime
, epochStart

-- * FeatureFlags
, FeatureFlags
, mkFeatureFlags
, encodeFeatureFlags
, decodeFeatureFlags

-- * POW Target
, powTarget

-- * BlockHeader
, BlockHeader
, blockNonce
, blockChainId
, blockHeight
, blockWeight
, blockChainwebVersion
, blockAdjacentHashes
, blockCreationTime
, blockHash
, blockParent
, blockPayloadHash
, blockTarget
, blockEpochStart
, blockFlags
, blockPow
, blockAdjacentChainIds
, encodeBlockHeader
, encodeBlockHeaderWithoutHash
, decodeBlockHeader
, decodeBlockHeaderWithoutHash
, decodeBlockHeaderChecked
, decodeBlockHeaderCheckedChainId
, ObjectEncoded(..)

, timeBetween
, getAdjacentHash
, computeBlockHash
, adjacentChainIds
, absBlockHeightDiff

-- * IsBlockHeader
, IsBlockHeader(..)

-- * Genesis BlockHeader
, isGenesisBlockHeader
, genesisParentBlockHash
, genesisBlockHeader
, genesisBlockHeaders
, genesisBlockHeadersAtHeight
, genesisHeight
, headerSizes
, headerSizeBytes
, workSizeBytes

-- * Create a new BlockHeader
, newBlockHeader

-- * CAS Constraint
, BlockHeaderCas

)where

import Chainweb.BlockHeader.Internal
