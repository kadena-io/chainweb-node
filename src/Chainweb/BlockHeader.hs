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
, I.parentHeader
, I.parentHeaderHash
, I._rankedParentHash
, I.rankedParentHash
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
, blockForkState
, blockForkVotes
, blockForkNumber

-- ** Utilities
, I._blockPow
, I.blockPow
, I.rankedBlockHash
, I._rankedBlockHash
, I.rankedBlockPayloadHash
, I._rankedBlockPayloadHash
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
, I.ExtendedObjectEncoded(..)

, I.timeBetween
, I.getAdjacentHash
, I.computeBlockHash
, I.adjacentChainIds
, I.absBlockHeightDiff

-- ** Fork State
, I.isForkEpochStart
, I.forkEpochLength
, I.isForkCountBlock
, I.isForkVoteBlock
, I.newForkState
, I.genesisForkState

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

import Chainweb.ForkState (ForkState, ForkVotes, ForkNumber)
import Chainweb.BlockCreationTime (BlockCreationTime)
import Chainweb.BlockHash (BlockHash, BlockHashRecord)
import Chainweb.BlockHeader.Internal qualified as I
import Chainweb.BlockHeight (BlockHeight)
import Chainweb.BlockWeight (BlockWeight)
import Chainweb.ChainId (ChainId)
import Chainweb.Difficulty (HashTarget)
import Chainweb.Payload (BlockPayloadHash)
import Chainweb.Version (ChainwebVersionCode)
import Control.Lens (Getter)

blockFlags :: Getter I.BlockHeader ForkState
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

blockForkState :: Getter I.BlockHeader ForkState
blockForkState = I.blockForkState

blockForkVotes :: Getter I.BlockHeader ForkVotes
blockForkVotes = I.blockForkVotes

blockForkNumber :: Getter I.BlockHeader ForkNumber
blockForkNumber = I.blockForkNumber
