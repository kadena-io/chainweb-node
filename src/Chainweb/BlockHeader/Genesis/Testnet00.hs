{-# LANGUAGE QuasiQuotes #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.BlockHeader.Genesis.Testnet00 where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)

import GHC.Stack (HasCallStack)

import NeatInterpolation (text)

import Chainweb.BlockHeader
import Chainweb.Utils (fromJuste)

unsafeFromYamlText :: HasCallStack => Text -> BlockHeader
unsafeFromYamlText = _objectEncoded . fromJuste . decodeThrow . encodeUtf8

testnet00C0 :: BlockHeader
testnet00C0 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
height: 0
hash: 29qj7Ea7NGgVNrqf22cdo2_6swzYHxvMnTR0git2_7c
miner: 0/0
chainId: 0
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '3106'

    |]

testnet00C1 :: BlockHeader
testnet00C1 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
height: 0
hash: hKB25cm5_hPZDSJ8ZzyBOcYeNyz7GYiz6OWrzAnYiqI
miner: 0/1
chainId: 1
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '19200'

    |]

testnet00C2 :: BlockHeader
testnet00C2 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
height: 0
hash: JwqfM4UEFdIp1QNBN6z9XWKCX-ufPcwSty9RKlrH910
miner: 0/2
chainId: 2
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '11130'

    |]

testnet00C3 :: BlockHeader
testnet00C3 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
height: 0
hash: jMgF2OeDXATYsMp8iSALk11v7-QI2iIT1wkwOpwTf9A
miner: 0/3
chainId: 3
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '2328'

    |]

testnet00C4 :: BlockHeader
testnet00C4 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
height: 0
hash: 50gVoZWh_JmCZ6uPP5kS8upob96_0nrEOmEWaWjyq38
miner: 0/4
chainId: 4
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '12652'

    |]

testnet00C5 :: BlockHeader
testnet00C5 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
height: 0
hash: MKTbTAsZZ3q-d3vuNvYkIhi8-R8EMNcaQhv_86S2ZdM
miner: 0/5
chainId: 5
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '3521'

    |]

testnet00C6 :: BlockHeader
testnet00C6 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
height: 0
hash: OhNjtuPDfGY8hPzqUKm56xOkjTUlEpXx0aO5kSIz7OI
miner: 0/6
chainId: 6
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '10359'

    |]

testnet00C7 :: BlockHeader
testnet00C7 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
height: 0
hash: HH-oGa99OUEemrN5X9AlME__6XfC2fOR4pOGtoGv_no
miner: 0/7
chainId: 7
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '2192'

    |]

testnet00C8 :: BlockHeader
testnet00C8 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
height: 0
hash: XL1DeCuYDAgrkZGrt_fQYnOjNAp-J7adtmqpK7TTn3o
miner: 0/8
chainId: 8
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '804'

    |]

testnet00C9 :: BlockHeader
testnet00C9 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
height: 0
hash: LARhevb-Tc9pFsimCgsy0Qr0YEaz18lBAKyB_NfABO8
miner: 0/9
chainId: 9
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
payloadHash: z-TEwU2t7DU5kPrNEnXEiQie_Wu8ew4DgUrDwGm3_y4
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '25740'

    |]

