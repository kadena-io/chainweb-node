{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
  ( -- * Testnet00
    testnet00C0
  , testnet00C1
  , testnet00C2
  , testnet00C3
  , testnet00C4
  , testnet00C5
  , testnet00C6
  , testnet00C7
  , testnet00C8
  , testnet00C9
  ) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml

import NeatInterpolation (text)

-- internal modules

import Chainweb.BlockHeader (BlockHeader, ObjectEncoded(..))

---

-- -------------------------------------------------------------------------- --
-- Testnet00

{-

=== LAWS ===

- The contents of hard-coded genesis blocks must be visually verifiable by
  people.

- The output of `mine-genesis` must be deterministic, and never produce blocks
  that differ from what is hard-coded in a live MainNet.

- Changes in the structure of the BlockHeader type must not prevent earlier,
  MainNet-live BlockHeaders from being read / verified.

- Changes in the structure of the BlockHeader type must not prevent the contents
  of MainNet-live genesis block files from compiling.

=== CONSIDERATIONS ===

If we hard-code using constructors (as below) and the definition of
`BlockHeader` changes in future, these all break.

If we instead reconstruct the blocks via Merkle Logs (as they are originally
through `genesisBlockHeader`), then they're weak to changes in the `merkle-log`
sublibrary.

If we "stringly-type" the genesis and save here it as inlined JSON, then it's
weak to changes in the JSON codec.

If we save the genesis blocks as encoded bytes in external files, then their
contents are hard to verify visually.

If we save the genesis blocks as encoded JSON or YAML in external files, then
we're again at the mercy of the codecs and/or runtime decoding failures.

Headers saved instead as json/yaml/bytes *might* be resistent to changes in the
structure of `BlockHeader`. This would require some idea of backwards
compatibility, and potentially some official extension semantics like GIF or
Protobuf.

Using Protobuf directly seems attractive, however contents can't be verified
visually (as mentioned above).

-}

unsafeFromYamlText :: Text -> BlockHeader
unsafeFromYamlText = _objectEncoded . fromJust . Yaml.decodeThrow . T.encodeUtf8

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
