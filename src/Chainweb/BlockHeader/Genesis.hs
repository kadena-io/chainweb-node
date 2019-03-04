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

import Data.Bytes.Put (putByteString, runPutS)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import Data.MerkleLog hiding (Actual, Expected, MerkleHash)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml

import NeatInterpolation (text)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId (ChainId, HasChainId(..), encodeChainId)
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty (HashTarget, maxTarget)
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.NodeId (ChainNodeId(..))
import Chainweb.Payload
import Chainweb.Time (Time(..), TimeSpan(..), epoche)
import Chainweb.Version (ChainwebVersion(..), encodeChainwebVersion)

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
genesisTime :: ChainwebVersion -> ChainId -> BlockCreationTime
genesisTime Test{} _ = BlockCreationTime epoche
genesisTime TestWithTime{} _ = BlockCreationTime epoche
genesisTime TestWithPow{} _ = BlockCreationTime epoche
genesisTime Simulation{} _ = BlockCreationTime epoche
-- Tuesday, 2019 February 26, 10:55 AM
genesisTime Testnet00 _ = BlockCreationTime . Time $ TimeSpan 1551207336601038

genesisMiner :: HasChainId p => ChainwebVersion -> p -> ChainNodeId
genesisMiner Test{} p = ChainNodeId (_chainId p) 0
genesisMiner TestWithTime{} p = ChainNodeId (_chainId p) 0
genesisMiner TestWithPow{} p = ChainNodeId (_chainId p) 0
genesisMiner Simulation{} p = ChainNodeId (_chainId p) 0
-- TODO: Base the `ChainNodeId` off a Pact public key that is significant to Kadena.
-- In other words, 0 is a meaningless hard-coding.
genesisMiner Testnet00 p = ChainNodeId (_chainId p) 0

-- TODO: characterize genesis block payload. Should this be the value of
-- chainId instead of empty string?
genesisBlockPayloadHash :: ChainwebVersion -> ChainId -> BlockPayloadHash
genesisBlockPayloadHash v@Test{} c
    = _blockPayloadPayloadHash $ uncurry blockPayload $ genesisBlockPayload v c
genesisBlockPayloadHash v@TestWithTime{} c
    = _blockPayloadPayloadHash $ uncurry blockPayload $ genesisBlockPayload v c
genesisBlockPayloadHash v c = hashPayload v c $ runPutS $ do
    putByteString "GENESIS:"
    encodeChainwebVersion v
    encodeChainId c

genesisBlockPayload :: ChainwebVersion -> ChainId -> (BlockTransactions, BlockOutputs)
genesisBlockPayload Test{} _ = (txs, outs)
  where
    (_, outs) = newBlockOutputs mempty
    (_, txs) = newBlockTransactions mempty
genesisBlockPayload TestWithTime{} _ = (txs, outs)
  where
    (_, outs) = newBlockOutputs mempty
    (_, txs) = newBlockTransactions mempty
genesisBlockPayload TestWithPow{} _ = (txs, outs)
  where
    (_, outs) = newBlockOutputs mempty
    (_, txs) = newBlockTransactions mempty
genesisBlockPayload Simulation{} _ =
    error "genesisBlockPayload isn't yet defined for Simulation"
genesisBlockPayload Testnet00 _ =
    error "genesisBlockPayload isn't yet defined for Testnet00"

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
genesisBlockHeader v p =
    genesisBlockHeader' v p (genesisTime v $ _chainId p) (Nonce 0)

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

unsafeFromYamlText :: Text -> BlockHeader
unsafeFromYamlText = _objectEncoded . fromJust . Yaml.decodeThrow . T.encodeUtf8

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

testnet00C0 :: BlockHeader
testnet00C0 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
height: 0
hash: Rtt5MNX1AWQos8DOHn3MRv-d4TbyJx4FGQgOZu6jEd4
miner:
  _chainNodeIdChain: 0
  _chainNodeIdId: 0
chainId: 0
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
payloadHash: NKHeUOuw64OabnpPUfZRIcc5Fvu_IA90F4DwIFjIRuU
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 783
    |]

testnet00C1 :: BlockHeader
testnet00C1 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
height: 0
hash: ZcoJy_e7Rcf7VNTF5bRUR6mLqilgnccSdFjd95FJ8cw
miner:
  _chainNodeIdChain: 1
  _chainNodeIdId: 0
chainId: 1
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
payloadHash: WqIppoHOtat38DHUSwrso8mbgQ4opPb7VrkNOwJdlyA
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 269
    |]

testnet00C2 :: BlockHeader
testnet00C2 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
height: 0
hash: 3W3KoX9q_GDHwAbLCA9AN5ASu-s5an3KSm8Q49XbUzM
miner:
  _chainNodeIdChain: 2
  _chainNodeIdId: 0
chainId: 2
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
payloadHash: djlccWNhufSi0mJsOjm9pRFTjyFpE8IWt7VzIQb00d4
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 380
    |]

testnet00C3 :: BlockHeader
testnet00C3 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
height: 0
hash: VBEdYJpLTNmHWpxfVNM0WhqHvlnTdDiGYvM6tJJ6uio
miner:
  _chainNodeIdChain: 3
  _chainNodeIdId: 0
chainId: 3
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
payloadHash: bH16cViQSjqd-j7cOD_SIZsnpjoY_e3xpvzgxPlreEE
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 113
    |]

testnet00C4 :: BlockHeader
testnet00C4 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
height: 0
hash: 8-2Zaxx9f4nNBnbQ0p-2p6mZ-OECdW6nwboPxoVVubk
miner:
  _chainNodeIdChain: 4
  _chainNodeIdId: 0
chainId: 4
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: 1gMCMjFoi5jK3LTo8E-CSl82SonSRplSVjYOz2GHliA
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 1205
    |]

testnet00C5 :: BlockHeader
testnet00C5 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
height: 0
hash: GWc0Y6KBMideblr-hNtxS-4mwvDkrtrsyFTp6_bPEso
miner:
  _chainNodeIdChain: 5
  _chainNodeIdId: 0
chainId: 5
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: xw-9DsQgnuFnnWJje_htsHAm6W2pEy7392jsvpADk0Q
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 183
    |]

testnet00C6 :: BlockHeader
testnet00C6 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
height: 0
hash: vTTkWTR_ZeMPI7cbAJD9TG-PVn1RbP_H2avLpGIsUD0
miner:
  _chainNodeIdChain: 6
  _chainNodeIdId: 0
chainId: 6
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
payloadHash: TNxIJweDgew0u7rsgrre6G9Q8XuwXO2yiCVratLJbrM
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 1966
    |]

testnet00C7 :: BlockHeader
testnet00C7 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
height: 0
hash: P54DBJutxTUE3cOE5ribGmQQwR7FGeTooMqw_ij_EHo
miner:
  _chainNodeIdChain: 7
  _chainNodeIdId: 0
chainId: 7
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
payloadHash: HzYMZFvuzkDSPHun8IbS_JYVtpE7_I_ZNSLzndArZa8
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 439
    |]

testnet00C8 :: BlockHeader
testnet00C8 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
height: 0
hash: AmJeCOJ4EWT6f2TtFCrrKZfzwAtjaOMv2P-PqRJd0iM
miner:
  _chainNodeIdChain: 8
  _chainNodeIdId: 0
chainId: 8
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: MTOsiPQ8qe8Jqo51-207n6ybASAnI6OZyBQLiXX3M3Q
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 307
    |]

testnet00C9 :: BlockHeader
testnet00C9 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
height: 0
hash: 4H7twgEl0BdNajK8wYBr6s49ooRpNJ785wQE4QM38g8
miner:
  _chainNodeIdChain: 9
  _chainNodeIdId: 0
chainId: 9
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
payloadHash: 3P8FfZRC6A7yNrm76SrnrXrIMGv5yRzO0dbv-FEbVCM
chainwebVersion: testnet00
target: ________________________________________fwA
nonce: 364
    |]
