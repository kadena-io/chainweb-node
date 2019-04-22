{-# LANGUAGE QuasiQuotes #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.BlockHeader.Genesis.Testnet01 where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeThrow)

import GHC.Stack (HasCallStack)

import NeatInterpolation (text)

import Chainweb.BlockHeader
import Chainweb.Utils (fromJuste)

unsafeFromYamlText :: HasCallStack => Text -> BlockHeader
unsafeFromYamlText = _objectEncoded . fromJuste . decodeThrow . encodeUtf8

testnet01C0 :: BlockHeader
testnet01C0 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: hmqOBLvsVKKexErkDjVULKsAJtmBltU2s6iOMkqLb64
height: 0
hash: K8v5H56DYLMNJuoxlWu-B3Z8355IWG7YtVydICbghkw
miner: 0/12
chainId: 12
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '13': n85aWmf6P37Gzx0-huLjdw79xVXYxXC43jj60_3c2IA
  '2': g4Z62-7kUVsBOs0x058-K_hPiTyfdzXKvbh2zLgp0JM
  '11': 3ADtOzWTTd-nmDFnF4Ut1GX1nXQZL1SI8cl_0xGh_nQ
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '8772'

    |]

testnet01C1 :: BlockHeader
testnet01C1 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: n85aWmf6P37Gzx0-huLjdw79xVXYxXC43jj60_3c2IA
height: 0
hash: KN-hqfPSDHb-nwb0P_TIN2tmMmzLeAnzSHSwSM51VGE
miner: 0/13
chainId: 13
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '14': Kqq4p-2aFjbePbks3KyEr3zZLM5x3UavC2FKoiCU46U
  '12': hmqOBLvsVKKexErkDjVULKsAJtmBltU2s6iOMkqLb64
  '3': 8SmViUqycWoRDwYuaaxjGfU9jiaTSTdCTB0n6e8HR2E
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '4138'

    |]

testnet01C2 :: BlockHeader
testnet01C2 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: Kqq4p-2aFjbePbks3KyEr3zZLM5x3UavC2FKoiCU46U
height: 0
hash: xwQBHcIIT3hYR-LdkJrKnTpyGbgLriqOlE0BFpf8StU
miner: 0/14
chainId: 14
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '13': n85aWmf6P37Gzx0-huLjdw79xVXYxXC43jj60_3c2IA
  '4': N7XxTzQ6QtRFBHfbmlLuXrFY-dvhzQEe0luZcptdQck
  '10': 5LZI89qDeaNKFruAh5GWT7f18R7IaegwOJNYhJPSdww
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '17720'

    |]

testnet01C3 :: BlockHeader
testnet01C3 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: z-F0gIwaZWVZg5l7tLm314q3LtDjujDEbFow-N9VbF4
height: 0
hash: MXZcEF-72ni98oqimbmkwlS-f7_QOwdFvE_ksiwWi2U
miner: 0/15
chainId: 15
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '0': 0_wArwHesJQAwgCTtQkgwFBQHumPheK5MnzjX7qbYEw
  '19': iUbSRS_0oFLsPsbsicoKgiZmjmYIivUFzLz2E-Upvrs
  '16': jUJ3W59bqY4Jt5YBp2Q7UrIa-c1FDyn5lxGSOzkLf60
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '22403'

    |]

testnet01C4 :: BlockHeader
testnet01C4 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: saf6HdNU8Fwj389l2-AK5G5Y4AVRAWSwsEFjF6e6VzM
height: 0
hash: 4NC9ppddRm7lk_50gus09DvAVQR9YYsezFFTKuVRBsQ
miner: 0/8
chainId: 8
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '5': 45SO7SREvayqCbt0CmlEsqcayMj2-4Pbm4JNtW8lYyo
  '3': 8SmViUqycWoRDwYuaaxjGfU9jiaTSTdCTB0n6e8HR2E
  '6': 3FFkRp4Phb9EtFsRj_CV5BG2CIh2hFE9WZ8EK-C3a8M
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '48122'

    |]

testnet01C5 :: BlockHeader
testnet01C5 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: DXl1TWg1zAfXp0_HPbJWdvTg-2-OhfHwLsJOGGz7_1A
height: 0
hash: hELD-SrFo6VeEOY3asAm1iAGMWLttvJYeeEU9mwu6bw
miner: 0/9
chainId: 9
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': co_zMtp4jgoPM83U84_vdr1xVHrWv8GO15_cjHOar24
  '4': N7XxTzQ6QtRFBHfbmlLuXrFY-dvhzQEe0luZcptdQck
  '6': 3FFkRp4Phb9EtFsRj_CV5BG2CIh2hFE9WZ8EK-C3a8M
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '2603'

    |]

testnet01C6 :: BlockHeader
testnet01C6 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: 5LZI89qDeaNKFruAh5GWT7f18R7IaegwOJNYhJPSdww
height: 0
hash: HQbH5cxxUiOLJh03ceSkM9RCCyYjSM8UHfmOxqZsqnA
miner: 0/10
chainId: 10
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '14': Kqq4p-2aFjbePbks3KyEr3zZLM5x3UavC2FKoiCU46U
  '0': 0_wArwHesJQAwgCTtQkgwFBQHumPheK5MnzjX7qbYEw
  '11': 3ADtOzWTTd-nmDFnF4Ut1GX1nXQZL1SI8cl_0xGh_nQ
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '10432'

    |]

testnet01C7 :: BlockHeader
testnet01C7 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: 3ADtOzWTTd-nmDFnF4Ut1GX1nXQZL1SI8cl_0xGh_nQ
height: 0
hash: xdzunw1IJuJ-N3qqPG9Ev7bHElbeZ2QkUUBd2pilhGw
miner: 0/11
chainId: 11
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '12': hmqOBLvsVKKexErkDjVULKsAJtmBltU2s6iOMkqLb64
  '1': oWNuEdDDqX9mP570wNNf8MmHEGIWUl4H25nY5mdGJrE
  '10': 5LZI89qDeaNKFruAh5GWT7f18R7IaegwOJNYhJPSdww
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '4384'

    |]

testnet01C8 :: BlockHeader
testnet01C8 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: N7XxTzQ6QtRFBHfbmlLuXrFY-dvhzQEe0luZcptdQck
height: 0
hash: v_3Dj_4MjjM16PrjpVzarSxy4N688S9LTOhHrIey0fY
miner: 0/4
chainId: 4
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '14': Kqq4p-2aFjbePbks3KyEr3zZLM5x3UavC2FKoiCU46U
  '19': iUbSRS_0oFLsPsbsicoKgiZmjmYIivUFzLz2E-Upvrs
  '9': DXl1TWg1zAfXp0_HPbJWdvTg-2-OhfHwLsJOGGz7_1A
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '60072'

    |]

testnet01C9 :: BlockHeader
testnet01C9 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: 45SO7SREvayqCbt0CmlEsqcayMj2-4Pbm4JNtW8lYyo
height: 0
hash: _tKM35yGvTfh83-leQSKNugspCSgcgWIIVc-T6biiII
miner: 0/5
chainId: 5
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': co_zMtp4jgoPM83U84_vdr1xVHrWv8GO15_cjHOar24
  '0': 0_wArwHesJQAwgCTtQkgwFBQHumPheK5MnzjX7qbYEw
  '8': saf6HdNU8Fwj389l2-AK5G5Y4AVRAWSwsEFjF6e6VzM
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '24476'

    |]

testnet01C10 :: BlockHeader
testnet01C10 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: 3FFkRp4Phb9EtFsRj_CV5BG2CIh2hFE9WZ8EK-C3a8M
height: 0
hash: 3FefQ7BT-23UVR2g76sf1LYRAdpY9qd1AoyG7ml8zLE
miner: 0/6
chainId: 6
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '1': oWNuEdDDqX9mP570wNNf8MmHEGIWUl4H25nY5mdGJrE
  '8': saf6HdNU8Fwj389l2-AK5G5Y4AVRAWSwsEFjF6e6VzM
  '9': DXl1TWg1zAfXp0_HPbJWdvTg-2-OhfHwLsJOGGz7_1A
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '6419'

    |]

testnet01C11 :: BlockHeader
testnet01C11 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: co_zMtp4jgoPM83U84_vdr1xVHrWv8GO15_cjHOar24
height: 0
hash: U_h61_6oVqi6DpawgbrcHNdVTgOirXEdfD7oct7yNM4
miner: 0/7
chainId: 7
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '2': g4Z62-7kUVsBOs0x058-K_hPiTyfdzXKvbh2zLgp0JM
  '5': 45SO7SREvayqCbt0CmlEsqcayMj2-4Pbm4JNtW8lYyo
  '9': DXl1TWg1zAfXp0_HPbJWdvTg-2-OhfHwLsJOGGz7_1A
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '23551'

    |]

testnet01C12 :: BlockHeader
testnet01C12 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: 0_wArwHesJQAwgCTtQkgwFBQHumPheK5MnzjX7qbYEw
height: 0
hash: ThDhtVe9VLTv778WvnmCBsFuYaNPW-KsaoqllptjxQw
miner: 0/0
chainId: 0
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '15': z-F0gIwaZWVZg5l7tLm314q3LtDjujDEbFow-N9VbF4
  '5': 45SO7SREvayqCbt0CmlEsqcayMj2-4Pbm4JNtW8lYyo
  '10': 5LZI89qDeaNKFruAh5GWT7f18R7IaegwOJNYhJPSdww
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '12094'

    |]

testnet01C13 :: BlockHeader
testnet01C13 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: jUJ3W59bqY4Jt5YBp2Q7UrIa-c1FDyn5lxGSOzkLf60
height: 0
hash: LiX1aHI-iiZEARXas11HhVxERifdpnbSsHs5eSm1UF4
miner: 0/16
chainId: 16
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '15': z-F0gIwaZWVZg5l7tLm314q3LtDjujDEbFow-N9VbF4
  '17': 882y7Q6CWFtfpt_rSR4-Z1JjCYCAgSau9ZovTUzDd8E
  '1': oWNuEdDDqX9mP570wNNf8MmHEGIWUl4H25nY5mdGJrE
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '9855'

    |]

testnet01C14 :: BlockHeader
testnet01C14 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: oWNuEdDDqX9mP570wNNf8MmHEGIWUl4H25nY5mdGJrE
height: 0
hash: 7EIoMGnT1vy-dWEx4rwJke6thsrZd77IK_6T1gQYmDY
miner: 0/1
chainId: 1
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '16': jUJ3W59bqY4Jt5YBp2Q7UrIa-c1FDyn5lxGSOzkLf60
  '11': 3ADtOzWTTd-nmDFnF4Ut1GX1nXQZL1SI8cl_0xGh_nQ
  '6': 3FFkRp4Phb9EtFsRj_CV5BG2CIh2hFE9WZ8EK-C3a8M
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '8913'

    |]

testnet01C15 :: BlockHeader
testnet01C15 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: 882y7Q6CWFtfpt_rSR4-Z1JjCYCAgSau9ZovTUzDd8E
height: 0
hash: orkmQefOem9dYDWmV_OWrnLTzaXwm8f6KFv8PYfja5k
miner: 0/17
chainId: 17
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '18': v6fXQcA74C4q5YYmlz3tRVx63-nfx-6cwSDit1VSpjE
  '16': jUJ3W59bqY4Jt5YBp2Q7UrIa-c1FDyn5lxGSOzkLf60
  '2': g4Z62-7kUVsBOs0x058-K_hPiTyfdzXKvbh2zLgp0JM
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '4099'

    |]

testnet01C16 :: BlockHeader
testnet01C16 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: g4Z62-7kUVsBOs0x058-K_hPiTyfdzXKvbh2zLgp0JM
height: 0
hash: bJWfUjFbrzCEWOWkMsvUCtK1Oo3bHbnNZKOgj-krCsE
miner: 0/2
chainId: 2
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '7': co_zMtp4jgoPM83U84_vdr1xVHrWv8GO15_cjHOar24
  '12': hmqOBLvsVKKexErkDjVULKsAJtmBltU2s6iOMkqLb64
  '17': 882y7Q6CWFtfpt_rSR4-Z1JjCYCAgSau9ZovTUzDd8E
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '3532'

    |]

testnet01C17 :: BlockHeader
testnet01C17 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: v6fXQcA74C4q5YYmlz3tRVx63-nfx-6cwSDit1VSpjE
height: 0
hash: RpMrOJqx4SgAw26S0w--saoqACs_jA74wNi47MuXXdw
miner: 0/18
chainId: 18
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '19': iUbSRS_0oFLsPsbsicoKgiZmjmYIivUFzLz2E-Upvrs
  '17': 882y7Q6CWFtfpt_rSR4-Z1JjCYCAgSau9ZovTUzDd8E
  '3': 8SmViUqycWoRDwYuaaxjGfU9jiaTSTdCTB0n6e8HR2E
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '1916'

    |]

testnet01C18 :: BlockHeader
testnet01C18 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: 8SmViUqycWoRDwYuaaxjGfU9jiaTSTdCTB0n6e8HR2E
height: 0
hash: m6w3iDy7ynCCUypeI3b27Cuv8o2Bdzvhe8V4J1QrLak
miner: 0/3
chainId: 3
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '13': n85aWmf6P37Gzx0-huLjdw79xVXYxXC43jj60_3c2IA
  '18': v6fXQcA74C4q5YYmlz3tRVx63-nfx-6cwSDit1VSpjE
  '8': saf6HdNU8Fwj389l2-AK5G5Y4AVRAWSwsEFjF6e6VzM
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '8232'

    |]

testnet01C19 :: BlockHeader
testnet01C19 = unsafeFromYamlText
    [text|
creationTime: 1555613536726767
parent: iUbSRS_0oFLsPsbsicoKgiZmjmYIivUFzLz2E-Upvrs
height: 0
hash: MkFc5qEI_eR98JaDNlmlIE8iDivZyoNZnmagWyp8v_U
miner: 0/19
chainId: 19
weight: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
adjacents:
  '15': z-F0gIwaZWVZg5l7tLm314q3LtDjujDEbFow-N9VbF4
  '18': v6fXQcA74C4q5YYmlz3tRVx63-nfx-6cwSDit1VSpjE
  '4': N7XxTzQ6QtRFBHfbmlLuXrFY-dvhzQEe0luZcptdQck
payloadHash: 61j8anj3geGTgnWGPfnxN8X5lpwphWMTWFc_sdTfZlw
chainwebVersion: testnet01
target: ________________________________________AwA
nonce: '2066'

    |]

