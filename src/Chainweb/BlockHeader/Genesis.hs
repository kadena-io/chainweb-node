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
import qualified Chainweb.BlockHeader.Genesis.DevelopmentPayload as D
import qualified Chainweb.BlockHeader.Genesis.Testnet00Payload as TN0
import qualified Chainweb.BlockHeader.Genesis.Testnet01Payload as TN1
import qualified Chainweb.BlockHeader.Genesis.Testnet01Payload as TN2
import Chainweb.ChainId (ChainId, HasChainId(..), encodeChainId)
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty (HashTarget, maxTarget)
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.NodeId (ChainNodeId(..))
import Chainweb.Pact.Types (emptyPayload)
import Chainweb.Payload
import Chainweb.Time (Time(..), TimeSpan(..), epoch)
import Chainweb.Version (ChainwebVersion(..), chainIds, encodeChainwebVersion)

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
-- Thursday, 2019 July 17, 11:28 AM
genesisTime Development = BlockCreationTime . Time $ TimeSpan 1563388117613832
-- Tuesday, 2019 February 26, 10:55 AM
genesisTime Testnet00 = BlockCreationTime . Time $ TimeSpan 1551207336601038
-- Thursday, 2019 April 18, 11:52 AM
genesisTime Testnet01 = BlockCreationTime . Time $ TimeSpan 1555613536726767
-- Thursday, 2019 July 17, 11:28 AM
genesisTime Testnet02 = BlockCreationTime . Time $ TimeSpan 1563388117613832

-- TODO: Base the `ChainNodeId` off a Pact public key that is significant to Kadena.
-- In other words, 0 is a meaningless hard-coding.
genesisMiner :: HasChainId p => ChainwebVersion -> p -> ChainNodeId
-- Test Instances
genesisMiner Test{} p = ChainNodeId (_chainId p) 0
genesisMiner TimedConsensus{} p = ChainNodeId (_chainId p) 0
genesisMiner PowConsensus{} p = ChainNodeId (_chainId p) 0
genesisMiner TimedCPM{} p = ChainNodeId (_chainId p) 0
-- Development Instances
genesisMiner Development p = ChainNodeId (_chainId p) 0
-- Production Instances
genesisMiner Testnet00 p = ChainNodeId (_chainId p) 0
genesisMiner Testnet01 p = ChainNodeId (_chainId p) 0
genesisMiner Testnet02 p = ChainNodeId (_chainId p) 0

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
genesisBlockPayload TimedCPM{} _ = TN0.payloadBlock
-- Development Instances
genesisBlockPayload Development _ = D.payloadBlock
-- Production Instances
genesisBlockPayload Testnet00 _ = TN0.payloadBlock
genesisBlockPayload Testnet01 _ = TN1.payloadBlock
genesisBlockPayload Testnet02 _ = TN2.payloadBlock

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
genesisBlockHeader' v p ct n = fromLog mlog
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
        :+: genesisMiner v cid
        :+: MerkleLogBody (blockHashRecordToSequence adjParents)
    adjParents = BlockHashRecord $ HM.fromList $
        (\c -> (c, genesisParentBlockHash v c)) <$> HS.toList (adjacentChainIds g p)

genesisBlockHeaders :: ChainwebVersion -> HM.HashMap ChainId BlockHeader
genesisBlockHeaders v = HM.fromList
    . fmap (id &&& genesisBlockHeader v)
    . toList
    $ chainIds v
