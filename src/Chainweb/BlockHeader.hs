{-# LANGUAGE ImportQualifiedPost #-}

-- Use this module to get read-only access to a 'BlockHeader' via 'Getter's.
--
-- Editing or manually constructing 'BlockHeader's outside of tests is dangerous
-- and likely to result in invalid headers, whether through invalid block hashes
-- or invalid adjacent hash records.
--
-- If you need to manually construct or overwrite a BlockHeader record or
-- 'Setter', again only in tests, use 'Chainweb.BlockHeader.Internal' instead.
module Chainweb.BlockHeader
(
-- * Newtype wrappers for function parameters
  I.ParentHeader(..)
, I._ParentHeader
, I.parentHeader
, I.ParentCreationTime(..)

-- * Block Payload Hash
, I.BlockPayloadHash
, I.BlockPayloadHash_(..)
, I.encodeBlockPayloadHash
, I.decodeBlockPayloadHash

-- * Nonce
, I.Nonce(..)
, I.encodeNonce
, I.encodeNonceToWord64
, I.decodeNonce

-- * EpochStartTime
, I.EpochStartTime(..)
, I.encodeEpochStartTime
, I.decodeEpochStartTime
, I.epochStart

-- * FeatureFlags
, I.FeatureFlags
, I.mkFeatureFlags
, I.encodeFeatureFlags
, I.decodeFeatureFlags

-- * POW Target
, I.powTarget

-- * BlockHeader
, I.BlockHeader
-- ** Getters
, blockFlags
, blockCreationTime
, blockParent
, blockAdjacentHashes
, blockTarget
, blockPayloadHash
, blockChainId
, blockWeight
, blockHeight
, blockChainwebVersion
, blockEpochStart
, blockNonce
, blockHash
-- ** Utilities
, I._blockPow
, I.blockPow
, I._blockAdjacentChainIds
, I.blockAdjacentChainIds
, I.encodeBlockHeader
, I.encodeBlockHeaderWithoutHash
, I.decodeBlockHeader
, I.decodeBlockHeaderWithoutHash
, I.decodeBlockHeaderChecked
, I.decodeBlockHeaderCheckedChainId
, I.blockHeaderShortDescription
, I.ObjectEncoded(..)

, I.timeBetween
, I.getAdjacentHash
, I.computeBlockHash
, I.adjacentChainIds
, I.absBlockHeightDiff

, I.guardBlockHeader

-- * IsBlockHeader
, I.IsBlockHeader(..)

-- * Genesis BlockHeader
, I.isGenesisBlockHeader
, I.genesisParentBlockHash
, I.genesisBlockHeader
, I.genesisBlockHeaders
, I.genesisBlockHeadersAtHeight
, I.genesisHeight
, I.headerSizes
, I.headerSizeBytes
, I.workSizeBytes

-- * Create a new BlockHeader
, I.newBlockHeader

-- * CAS Constraint
, I.BlockHeaderCas
)
where

import Chainweb.ChainId (ChainId)
import Chainweb.BlockWeight (BlockWeight)
import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Version (ChainwebVersionCode)
import Chainweb.Payload (BlockPayloadHash)
import Chainweb.Difficulty (HashTarget)
import Chainweb.BlockHash (BlockHash, BlockHashRecord)
import Chainweb.BlockHeader.Internal qualified as I
import Chainweb.BlockCreationTime (BlockCreationTime)
import Control.Lens (Getter)

blockFlags :: Getter I.BlockHeader I.FeatureFlags
blockFlags = I.blockFlags

blockCreationTime :: Getter I.BlockHeader BlockCreationTime
blockCreationTime = I.blockCreationTime

blockParent :: Getter I.BlockHeader BlockHash
blockParent = I.blockParent

blockAdjacentHashes :: Getter I.BlockHeader BlockHashRecord
blockAdjacentHashes = I.blockAdjacentHashes

blockTarget :: Getter I.BlockHeader HashTarget
blockTarget = I.blockTarget

blockPayloadHash :: Getter I.BlockHeader BlockPayloadHash
blockPayloadHash = I.blockPayloadHash

blockChainId :: Getter I.BlockHeader ChainId
blockChainId = I.blockChainId

blockWeight :: Getter I.BlockHeader BlockWeight
blockWeight = I.blockWeight

blockHeight :: Getter I.BlockHeader BlockHeight
blockHeight = I.blockHeight

blockChainwebVersion :: Getter I.BlockHeader ChainwebVersionCode
blockChainwebVersion = I.blockChainwebVersion

blockEpochStart :: Getter I.BlockHeader I.EpochStartTime
blockEpochStart = I.blockEpochStart

blockNonce :: Getter I.BlockHeader I.Nonce
blockNonce = I.blockNonce

blockHash :: Getter I.BlockHeader BlockHash
blockHash = I.blockHash
