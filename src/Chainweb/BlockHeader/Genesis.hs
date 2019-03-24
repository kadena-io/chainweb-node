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

    -- * Hard-coded Blocks
    -- | === Laws
    --
    --   * The contents of hard-coded genesis blocks must be visually
    --     verifiable by people.
    --   * The output of @mine-genesis@ must be deterministic, and never produce
    --     blocks that differ from what is hard-coded in a live MainNet.
    --   * Changes in the structure of the BlockHeader type must not prevent
    --     earlier, MainNet-live BlockHeaders from being read / verified.
    --   * Changes in the structure of the BlockHeader type must not prevent
    --     the contents of MainNet-live genesis block files from compiling.

    -- ** Testnet00
  , testnet00Geneses
  ) where

import Control.Arrow ((&&&))

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.MerkleLog hiding (Actual, Expected, MerkleHash)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis.Testnet00
import Chainweb.BlockHeader.Genesis.TestWithTime
import qualified Chainweb.BlockHeader.Genesis.Testnet00Payload as TN0 (payloadBlock)
import qualified Chainweb.BlockHeader.Genesis.TestWithTimePayload as TWT (payloadBlock)
import Chainweb.ChainId (ChainId, HasChainId(..), encodeChainId)
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty (HashTarget, maxTarget)
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.NodeId (ChainNodeId(..))
import Chainweb.Pact.Types (noMiner,toCoinbaseOutput,noCoinbase)
import Chainweb.Payload
import Chainweb.Time (Time(..), TimeSpan(..), epoche)
import Chainweb.Version (ChainwebVersion(..), encodeChainwebVersion)
import Chainweb.Utils

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
genesisBlockTarget :: ChainwebVersion -> HashTarget
genesisBlockTarget = maxTarget

-- | The moment of creation of a Genesis Block. For test chains, this is the
-- Linux Epoch. Production chains are otherwise fixed to a specific timestamp.
--
genesisTime :: ChainwebVersion -> BlockCreationTime
genesisTime Test{} = BlockCreationTime epoche
-- TODO fix timespan
genesisTime TestWithTime{} = BlockCreationTime . Time $ TimeSpan 1551207336601039
genesisTime TestWithPow{} = BlockCreationTime epoche
genesisTime Simulation{} = BlockCreationTime epoche
-- Tuesday, 2019 February 26, 10:55 AM
genesisTime Testnet00 = BlockCreationTime . Time $ TimeSpan 1551207336601038

genesisMiner :: HasChainId p => ChainwebVersion -> p -> ChainNodeId
genesisMiner Test{} p = ChainNodeId (_chainId p) 0
genesisMiner TestWithTime{} p = ChainNodeId (_chainId p) 0
genesisMiner TestWithPow{} p = ChainNodeId (_chainId p) 0
genesisMiner Simulation{} p = ChainNodeId (_chainId p) 0
-- TODO: Base the `ChainNodeId` off a Pact public key that is significant to Kadena.
-- In other words, 0 is a meaningless hard-coding.
genesisMiner Testnet00 p = ChainNodeId (_chainId p) 0

genesisBlockPayloadHash :: ChainwebVersion -> ChainId -> BlockPayloadHash
genesisBlockPayloadHash v = _payloadWithOutputsPayloadHash . genesisBlockPayload v

-- TODO when Payload DB is finally loading genesis post-sync and post-pact,
-- the genesis block payload should be PayloadData, and PayloadWithOutputs
-- should have the TransactionTree and OutputTree to avoid recreating those
-- in PayloadStore.
genesisBlockPayload :: ChainwebVersion -> ChainId -> PayloadWithOutputs
genesisBlockPayload Test{} _ = emptyPayload
genesisBlockPayload TestWithTime{} _ = TWT.payloadBlock
genesisBlockPayload TestWithPow{} _ = emptyPayload
genesisBlockPayload Simulation{} _ =
    error "genesisBlockPayload isn't yet defined for Simulation"
genesisBlockPayload Testnet00 _ = TN0.payloadBlock

emptyPayload :: PayloadWithOutputs
emptyPayload = PayloadWithOutputs mempty miner coinbase h i o
  where (BlockPayload h i o) = newBlockPayload miner coinbase mempty
        miner = MinerData $ encodeToByteString noMiner
        coinbase = toCoinbaseOutput noCoinbase

-- | A block chain is globally uniquely identified by its genesis hash.
-- Internally, we use the 'ChainwebVersion' value and the 'ChainId'
-- as identifiers. We thus include the 'ChainwebVersion' value and the
-- 'ChainId' into the genesis block hash.
--
-- We assume that there is always only a single 'ChainwebVersion' in
-- scope and identify chains only by there internal 'ChainId'.
--
-- For production Chainwebs, this function dispatches to hard-coded blocks.
-- Otherwise, the blocks are deterministically generated.
--
genesisBlockHeader :: HasChainId p => ChainwebVersion -> p -> BlockHeader
genesisBlockHeader Testnet00 p =
    case HM.lookup (_chainId p) testnet00Geneses of
        Nothing -> error $ "Testnet00: No genesis block exists for " <> show (_chainId p)
        Just gb -> gb
genesisBlockHeader (TestWithTime _chainGraph) p =
    case HM.lookup (_chainId p) testWithTimeGeneses of
        Nothing -> error $ "TestWithTime: No genesis block exists for " <> show (_chainId p)
        Just gb -> gb
genesisBlockHeader v p =
    genesisBlockHeader' v p (genesisTime v) (Nonce 0)

-- | Like `genesisBlockHeader`, but with slightly more control.
-- __Will not dispatch to hard-coded `BlockHeader`s!__
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
        $ genesisParentBlockHash v cid
        :+: genesisBlockTarget v
        :+: genesisBlockPayloadHash v cid
        :+: ct
        :+: n
        :+: cid
        :+: BlockWeight 0
        :+: BlockHeight 0
        :+: v
        :+: genesisMiner v cid
        :+: MerkleLogBody (blockHashRecordToSequence adjParents)
    adjParents = BlockHashRecord $ HM.fromList $
        (\c -> (c, genesisParentBlockHash v c)) <$> HS.toList (adjacentChainIds g p)

genesisBlockHeaders
    :: ChainwebVersion
    -> HM.HashMap ChainId BlockHeader
genesisBlockHeaders v = HM.fromList
    . fmap (id &&& genesisBlockHeader v)
    . toList
    . chainIds_
    . _chainGraph
    $ v

-- -------------------------------------------------------------------------- --
-- Testnet00

-- | Ten Genesis Blocks for `Testnet00`.
testnet00Geneses :: HM.HashMap ChainId BlockHeader
testnet00Geneses = HM.fromList $ map (_chainId &&& id) bs
  where
    bs = [ testnet00C0
         , testnet00C1
         , testnet00C2
         , testnet00C3
         , testnet00C4
         , testnet00C5
         , testnet00C6
         , testnet00C7
         , testnet00C8
         , testnet00C9 ]
{-# NOINLINE testnet00Geneses #-}

-- -------------------------------------------------------------------------- --
-- TestWithTime

-- | Ten Genesis Blocks for `TestWithTime`.
testWithTimeGeneses :: HM.HashMap ChainId BlockHeader
testWithTimeGeneses = HM.fromList $ map (_chainId &&& id) bs
  where
    bs = [ testWithTimeC0
         , testWithTimeC1
         , testWithTimeC2
         , testWithTimeC3
         , testWithTimeC4
         , testWithTimeC5
         , testWithTimeC6
         , testWithTimeC7
         , testWithTimeC8
         , testWithTimeC9 ]
{-# NOINLINE testWithTimeGeneses #-}
