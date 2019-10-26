{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.BlockHeader.Genesis
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Hard-coded Genesis blocks for various versions of Chainweb.
--
module Chainweb.BlockHeader.Genesis
  ( -- * Genesis Blocks
    -- ** Creation
    genesisBlockHeader
  , genesisBlockHeader'
  , genesisBlockHeaders
    -- ** Querying
  , genesisBlockPayload
  , genesisParentBlockHash
  , genesisBlockTarget
  , genesisTime
  ) where

import Control.Arrow ((&&&))

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.MerkleLog hiding (Actual, Expected, MerkleHash)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import qualified Chainweb.BlockHeader.Genesis.Development0Payload as DN0
import qualified Chainweb.BlockHeader.Genesis.Development1Payload as DN1
import qualified Chainweb.BlockHeader.Genesis.Development2Payload as DN2
import qualified Chainweb.BlockHeader.Genesis.Development3Payload as DN3
import qualified Chainweb.BlockHeader.Genesis.Development4Payload as DN4
import qualified Chainweb.BlockHeader.Genesis.Development5Payload as DN5
import qualified Chainweb.BlockHeader.Genesis.Development6Payload as DN6
import qualified Chainweb.BlockHeader.Genesis.Development7Payload as DN7
import qualified Chainweb.BlockHeader.Genesis.Development8Payload as DN8
import qualified Chainweb.BlockHeader.Genesis.Development9Payload as DN9
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM0Payload as TN0
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM1Payload as TN1
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM2Payload as TN2
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM3Payload as TN3
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM4Payload as TN4
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM5Payload as TN5
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM6Payload as TN6
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM7Payload as TN7
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM8Payload as TN8
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM9Payload as TN9
import qualified Chainweb.BlockHeader.Genesis.Mainnet0Payload as MN0
import qualified Chainweb.BlockHeader.Genesis.Mainnet1Payload as MN1
import qualified Chainweb.BlockHeader.Genesis.Mainnet2Payload as MN2
import qualified Chainweb.BlockHeader.Genesis.Mainnet3Payload as MN3
import qualified Chainweb.BlockHeader.Genesis.Mainnet4Payload as MN4
import qualified Chainweb.BlockHeader.Genesis.Mainnet5Payload as MN5
import qualified Chainweb.BlockHeader.Genesis.Mainnet6Payload as MN6
import qualified Chainweb.BlockHeader.Genesis.Mainnet7Payload as MN7
import qualified Chainweb.BlockHeader.Genesis.Mainnet8Payload as MN8
import qualified Chainweb.BlockHeader.Genesis.Mainnet9Payload as MN9
import qualified Chainweb.BlockHeader.Genesis.Testnet0Payload as PN0
import qualified Chainweb.BlockHeader.Genesis.Testnet1Payload as PN1
import qualified Chainweb.BlockHeader.Genesis.Testnet2Payload as PN2
import qualified Chainweb.BlockHeader.Genesis.Testnet3Payload as PN3
import qualified Chainweb.BlockHeader.Genesis.Testnet4Payload as PN4
import qualified Chainweb.BlockHeader.Genesis.Testnet5Payload as PN5
import qualified Chainweb.BlockHeader.Genesis.Testnet6Payload as PN6
import qualified Chainweb.BlockHeader.Genesis.Testnet7Payload as PN7
import qualified Chainweb.BlockHeader.Genesis.Testnet8Payload as PN8
import qualified Chainweb.BlockHeader.Genesis.Testnet9Payload as PN9
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty (HashTarget, maxTarget)
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Pact.Types (emptyPayload)
import Chainweb.Payload
import Chainweb.Time
import Chainweb.Version

---

-- -------------------------------------------------------------------------- --
-- Genesis BlockHeader

-- | The genesis block hash includes the Chainweb version and the 'ChainId'
-- within the Chainweb version.
--
-- It is the '_blockParent' of the genesis block
--
genesisParentBlockHash :: HasChainId p => ChainwebVersion -> p -> BlockHash
genesisParentBlockHash v p = BlockHash $ MerkleLogHash
    $ merkleRoot $ merkleTree @(HashAlg ChainwebHashTag)
        [ InputNode "CHAINWEB_GENESIS"
        , encodeMerkleInputNode encodeChainwebVersion v
        , encodeMerkleInputNode encodeChainId (_chainId p)
        ]

-- | By definition, Genesis Blocks are "mined" on the easiest difficulty. No
-- subsequent block mining can have a `HashTarget` easier (re: higher) than
-- this. Equivalent to `maxTarget`.
--
genesisBlockTarget :: HashTarget
genesisBlockTarget = maxTarget

-- | The moment of creation of a Genesis Block. For test chains, this is the
-- Linux Epoch. Production chains are otherwise fixed to a specific timestamp.
--
genesisTime :: ChainwebVersion -> BlockCreationTime
genesisTime Test{} = BlockCreationTime epoch
genesisTime TimedConsensus{} = BlockCreationTime epoch
genesisTime PowConsensus{} = BlockCreationTime epoch
genesisTime TimedCPM{} = BlockCreationTime epoch
genesisTime FastTimedCPM{} = BlockCreationTime epoch
genesisTime Development = BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
genesisTime Testnet02 = BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
genesisTime Mainnet01 = BlockCreationTime [timeMicrosQQ| 2019-10-24T21:51:26.444848 |]

genesisBlockPayloadHash :: ChainwebVersion -> ChainId -> BlockPayloadHash
genesisBlockPayloadHash v = _payloadWithOutputsPayloadHash . genesisBlockPayload v

-- TODO when Payload DB is finally loading genesis post-sync and post-pact,
-- the genesis block payload should be PayloadData, and PayloadWithOutputs
-- should have the TransactionTree and OutputTree to avoid recreating those
-- in PayloadStore.
genesisBlockPayload :: ChainwebVersion -> ChainId -> PayloadWithOutputs
-- Test Instances
genesisBlockPayload Test{} _ = emptyPayload
genesisBlockPayload TimedConsensus{} _ = emptyPayload
genesisBlockPayload PowConsensus{} _ = emptyPayload
genesisBlockPayload TimedCPM{} cid = case chainIdInt @Int cid of
    0 -> TN0.payloadBlock
    1 -> TN1.payloadBlock
    2 -> TN2.payloadBlock
    3 -> TN3.payloadBlock
    4 -> TN4.payloadBlock
    5 -> TN5.payloadBlock
    6 -> TN6.payloadBlock
    7 -> TN7.payloadBlock
    8 -> TN8.payloadBlock
    9 -> TN9.payloadBlock
    _ -> error "peterson graph only supports a maximum of 10 chains - please review"

genesisBlockPayload FastTimedCPM{} cid = case chainIdInt @Int cid of
    0 -> TN0.payloadBlock
    1 -> TN1.payloadBlock
    2 -> TN2.payloadBlock
    3 -> TN3.payloadBlock
    4 -> TN4.payloadBlock
    5 -> TN5.payloadBlock
    6 -> TN6.payloadBlock
    7 -> TN7.payloadBlock
    8 -> TN8.payloadBlock
    9 -> TN9.payloadBlock
    _ -> error "peterson graph only supports a maximum of 10 chains - please review"

-- Development Instances
genesisBlockPayload Development cid = case chainIdInt @Int cid of
    0 -> DN0.payloadBlock
    1 -> DN1.payloadBlock
    2 -> DN2.payloadBlock
    3 -> DN3.payloadBlock
    4 -> DN4.payloadBlock
    5 -> DN5.payloadBlock
    6 -> DN6.payloadBlock
    7 -> DN7.payloadBlock
    8 -> DN8.payloadBlock
    9 -> DN9.payloadBlock
    _ -> error "peterson graph only supports a maximum of 10 chains - please review"

-- Production Instances
genesisBlockPayload Testnet02 cid = case chainIdInt @Int cid of
    0 -> PN0.payloadBlock
    1 -> PN1.payloadBlock
    2 -> PN2.payloadBlock
    3 -> PN3.payloadBlock
    4 -> PN4.payloadBlock
    5 -> PN5.payloadBlock
    6 -> PN6.payloadBlock
    7 -> PN7.payloadBlock
    8 -> PN8.payloadBlock
    9 -> PN9.payloadBlock
    _ -> error "peterson graph only supports a maximum of 10 chains - please review"

genesisBlockPayload Mainnet01 cid = case chainIdInt @Int cid of
    0 -> MN0.payloadBlock
    1 -> MN1.payloadBlock
    2 -> MN2.payloadBlock
    3 -> MN3.payloadBlock
    4 -> MN4.payloadBlock
    5 -> MN5.payloadBlock
    6 -> MN6.payloadBlock
    7 -> MN7.payloadBlock
    8 -> MN8.payloadBlock
    9 -> MN9.payloadBlock
    _ -> error "peterson graph only supports a maximum of 10 chains - please review"

-- | A block chain is globally uniquely identified by its genesis hash.
-- Internally, we use the 'ChainwebVersion' value and the 'ChainId'
-- as identifiers. We thus include the 'ChainwebVersion' value and the
-- 'ChainId' into the genesis block hash.
--
-- We assume that there is always only a single 'ChainwebVersion' in
-- scope and identify chains only by their internal 'ChainId'.
--
--
genesisBlockHeader :: HasChainId p => ChainwebVersion -> p -> BlockHeader
genesisBlockHeader v p = genesisBlockHeader' v p (genesisTime v) (Nonce 0)

-- | Like `genesisBlockHeader`, but with slightly more control.
--
genesisBlockHeader'
    :: HasChainId p
    => ChainwebVersion
    -> p
    -> BlockCreationTime
    -> Nonce
    -> BlockHeader
genesisBlockHeader' v p ct@(BlockCreationTime t) n = fromLog mlog
  where
    g = _chainGraph v
    cid = _chainId p

    mlog = newMerkleLog
        $ n
        :+: ct
        :+: genesisParentBlockHash v cid
        :+: genesisBlockTarget
        :+: genesisBlockPayloadHash v cid
        :+: cid
        :+: BlockWeight 0
        :+: BlockHeight 0
        :+: v
        :+: EpochStartTime t
        :+: FeatureFlags 0
        :+: MerkleLogBody (blockHashRecordToVector adjParents)
    adjParents = BlockHashRecord $ HM.fromList $
        (\c -> (c, genesisParentBlockHash v c)) <$> HS.toList (adjacentChainIds g p)

-- | This is an expensive call, try not to repeat it.
genesisBlockHeaders :: ChainwebVersion -> HM.HashMap ChainId BlockHeader
genesisBlockHeaders v = HM.fromList
    . fmap (id &&& genesisBlockHeader v)
    . toList
    $ chainIds v
