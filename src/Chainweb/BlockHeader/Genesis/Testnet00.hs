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
hash: ibkjqOeUqGPk-Xw4ZGXsOyS66P_8jRV5hJmv1dVY78o
miner: 0/0
chainId: 0
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '461'

    |]

testnet00C1 :: BlockHeader
testnet00C1 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
height: 0
hash: _qKLz8pl59Wa6e2xbSg6ZmepLrJqY2Ax--Y_-KzXeC8
miner: 0/1
chainId: 1
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '15620'

    |]

testnet00C2 :: BlockHeader
testnet00C2 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
height: 0
hash: IdIsejZmvNqeBdWIzXzBTMKrYB2GDGnjCb45IFF74KU
miner: 0/2
chainId: 2
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '46478'

    |]

testnet00C3 :: BlockHeader
testnet00C3 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
height: 0
hash: w4xpYDj8SyRd9tApJshku9aoybnRBKLNl-lfjt-F978
miner: 0/3
chainId: 3
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '1297'

    |]

testnet00C4 :: BlockHeader
testnet00C4 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
height: 0
hash: W1s8dn4giYc7LxLA8knxcBlzNfSB2WAgeA-muAliS-s
miner: 0/4
chainId: 4
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '11735'

    |]

testnet00C5 :: BlockHeader
testnet00C5 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
height: 0
hash: ak_aEzJet4dcjshMcg5g7xd5iOktKhveAh5jLMfincQ
miner: 0/5
chainId: 5
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '0': hkY3tAJOaRSSTG5DUYBEMRjNlZr2jEyA_8d0_NJ76ow
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '20432'

    |]

testnet00C6 :: BlockHeader
testnet00C6 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
height: 0
hash: qVrf28Nt1l6BqQa23VKPT35Z0kx0ThUzeajwKlAzTsE
miner: 0/6
chainId: 6
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '1': gSDXx0M9qJg03BU2zi1jDGo0n8lHhcojup27cl5bVtM
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '28882'

    |]

testnet00C7 :: BlockHeader
testnet00C7 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
height: 0
hash: B84irZ8XflnlHMeWetJc2SM40KViqU4W_GeGkQf1ICc
miner: 0/7
chainId: 7
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '2': zBD6jyT5Irr5QIcNoDw48_aN8TcPI7-HgHJBYm_ra18
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
  '6': xSXQP0riuw-DDRLz-BEdw7Vn7C8c8ICwlQK_DwhE18Q
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '5948'

    |]

testnet00C8 :: BlockHeader
testnet00C8 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
height: 0
hash: A-7OwHtdVHL3YJnV1gzQo5bNlrQxwjj1AxLPqr0kfKg
miner: 0/8
chainId: 8
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': wHYmEOiBrC3l7ZaZdrr2Nr1ClvsA6WdS3Tps20HfvjY
  '3': iliOelarez9K7DNE1Je8V_TczJAgJh4dB9Pm3WgKbMQ
  '9': CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '11789'

    |]

testnet00C9 :: BlockHeader
testnet00C9 = unsafeFromYamlText
    [text|
creationTime: 1551207336601038
parent: CY9Uo83VT4g_RJar_lLItK_MpWvl4e4yHsY1i2KXuBk
height: 0
hash: PQVjMaV61f4JDLRpCD6IgzhXy4FS8P89YmIuCe0TYnA
miner: 0/9
chainId: 9
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '4': E8g1sAZD7xvIRLiqF-TmQ6YwIh1lUxmIpSzaJb9F8WM
  '5': jRBryPOLRqBKjceQXsRuLp6Q9mMqrZmCW3vQ3XgDtts
  '8': -acx5PNzURsOtqhJKm08Zf9FchU7FDs64cKVqA5Vm0A
payloadHash: TRbQB-adV5io-6dmd8cUI6rWt3N4gFsZ619J4OCmlcA
chainwebVersion: testnet00
target: ________________________________________AwA
nonce: '8876'

    |]

