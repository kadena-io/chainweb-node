{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.BlockHeader.Genesis
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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
  , genesisBlockHeadersAtHeight
    -- ** Querying
  , genesisBlockPayload
  , genesisParentBlockHash
  , genesisBlockTarget
  , genesisTime
    -- * No-op payloads
  , emptyPayload
  ) where

import Control.Arrow ((&&&))

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.MerkleLog hiding (Actual, Expected, MerkleHash)

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import qualified Chainweb.BlockHeader.Genesis.Development0Payload as DN0
import qualified Chainweb.BlockHeader.Genesis.DevelopmentNPayload as DNN
import qualified Chainweb.BlockHeader.Genesis.DevelopmentKADPayload as DNKAD
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM0Payload as TN0
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPMNPayload as TNN
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
import qualified Chainweb.BlockHeader.Genesis.MainnetKADPayload as MNKAD
import qualified Chainweb.BlockHeader.Genesis.Testnet0Payload as PN0
import qualified Chainweb.BlockHeader.Genesis.TestnetNPayload as PNN
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty (HashTarget, maxTarget)
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version


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

-- | Empty payload marking no-op transaction payloads for deprecated
-- versions.
--
emptyPayload :: PayloadWithOutputs
emptyPayload = PayloadWithOutputs mempty miner coinbase h i o
  where
    (BlockPayload h i o) = newBlockPayload miner coinbase mempty
    miner = MinerData $ encodeToByteString noMiner
    coinbase = noCoinbaseOutput

-- | The moment of creation of a Genesis Block. For test chains, this is the
-- Linux Epoch. Production chains are otherwise fixed to a specific timestamp.
--
genesisTime :: ChainwebVersion -> ChainId -> BlockCreationTime
genesisTime Test{} _ = BlockCreationTime epoch
genesisTime TimedConsensus{} _ = BlockCreationTime epoch
genesisTime PowConsensus{} _ = BlockCreationTime epoch
genesisTime TimedCPM{} _ = BlockCreationTime epoch
genesisTime FastTimedCPM{} _ = BlockCreationTime epoch
genesisTime Development _ = BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
genesisTime Testnet04 _ = BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
genesisTime Mainnet01 _ = BlockCreationTime [timeMicrosQQ| 2019-10-30T00:01:00.0 |]

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
    _ -> TNN.payloadBlock

genesisBlockPayload FastTimedCPM{} cid = case chainIdInt @Int cid of
    0 -> TN0.payloadBlock
    _ -> TNN.payloadBlock

-- Development Instances
genesisBlockPayload Development cid = case chainIdInt @Int cid of
    0 -> DN0.payloadBlock
    c | c >= 1, c <= 9 -> DNN.payloadBlock
    c | c >= 10, c <= 19 -> DNKAD.payloadBlock
    _ -> error "chainweb graph only supports a maximum of 20 chains - please review"

-- Production Instances
genesisBlockPayload Testnet04 cid = case chainIdInt @Int cid of
    0 -> PN0.payloadBlock
    _ -> PNN.payloadBlock

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
    c | c >= 10, c <= 19 -> MNKAD.payloadBlock
    _ -> error "chainweb graph only supports a maximum of 20 chains - please review"

-- | A block chain is globally uniquely identified by its genesis hash.
-- Internally, we use the 'ChainwebVersion' value and the 'ChainId'
-- as identifiers. We thus include the 'ChainwebVersion' value and the
-- 'ChainId' into the genesis block hash.
--
-- We assume that there is always only a single 'ChainwebVersion' in
-- scope and identify chains only by their internal 'ChainId'.
--
genesisBlockHeader :: HasChainId p => ChainwebVersion -> p -> BlockHeader
genesisBlockHeader v p = genesisBlockHeader' v p (genesisTime v (_chainId p)) (Nonce 0)

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
    g = genesisGraph v p
    cid = _chainId p

    mlog = newMerkleLog
        $ mkFeatureFlags
        :+: ct
        :+: genesisParentBlockHash v cid
        :+: genesisBlockTarget
        :+: genesisBlockPayloadHash v cid
        :+: cid
        :+: BlockWeight 0
        :+: genesisHeight v cid -- because of chain graph changes (new chains) not all chains start at 0
        :+: v
        :+: EpochStartTime t
        :+: n
        :+: MerkleLogBody (blockHashRecordToVector adjParents)
    adjParents = BlockHashRecord $ HM.fromList $
        (\c -> (c, genesisParentBlockHash v c)) <$> HS.toList (adjacentChainIds g p)

-- | This is an expensive call, try not to repeat it.
--
genesisBlockHeaders :: ChainwebVersion -> HM.HashMap ChainId BlockHeader
genesisBlockHeaders v = HM.fromList
    . fmap (id &&& genesisBlockHeader v)
    . toList
    $ chainIds v


genesisBlockHeadersAtHeight
    :: ChainwebVersion
    -> BlockHeight
    -> HM.HashMap ChainId BlockHeader
genesisBlockHeadersAtHeight v h = HM.filter
    (\hdr -> _blockHeight hdr <= h)
    $ genesisBlockHeaders v
