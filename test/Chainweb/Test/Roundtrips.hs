{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.Roundtrips
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Unit tests for various kinds of isomorphims and encoding roundtrips
--
module Chainweb.Test.Roundtrips
( tests
) where

import qualified Data.Text as T

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck.Instances ()

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.NodeId
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration

import Chainweb.Test.Orphans.Internal ()

-- -------------------------------------------------------------------------- --
-- Roundrip Tests

tests :: TestTree
tests = testGroup "roundtrip tests"
    [ encodeDecodeTests
    , showReadTests
    , base64RoundtripTests
    , hasTextRepresentationTests
    ]

-- -------------------------------------------------------------------------- --
-- Binary Encoding rountrips

encodeDecodeTests :: TestTree
encodeDecodeTests = testGroup "Encode-Decode roundtrips"
    [ testProperty "ChainwebVersion"
        $ prop_encodeDecodeRoundtrip decodeChainwebVersion encodeChainwebVersion
    , testProperty "ChainId"
        $ prop_encodeDecodeRoundtrip decodeChainId encodeChainId
    , testProperty "NodeId"
        $ prop_encodeDecodeRoundtrip decodeNodeId encodeNodeId
    , testProperty "BlockHashBytes"
        $ prop_encodeDecodeRoundtrip decodeBlockHashBytes encodeBlockHashBytes
    , testProperty "BlockHash"
        $ prop_encodeDecodeRoundtrip decodeBlockHash encodeBlockHash
    , testProperty "BlockHeight"
        $ prop_encodeDecodeRoundtrip decodeBlockHeight encodeBlockHeight
    , testProperty "BlockHashNat"
        $ prop_encodeDecodeRoundtrip decodeBlockHashNat encodeBlockHashNat
    , testProperty "HashDifficulty"
        $ prop_encodeDecodeRoundtrip decodeHashDifficulty encodeHashDifficulty
    , testProperty "HashTarget"
        $ prop_encodeDecodeRoundtrip decodeHashTarget encodeHashTarget
    , testProperty "BlockWeight"
        $ prop_encodeDecodeRoundtrip decodeBlockWeight encodeBlockWeight

    , testProperty "BlockHashRecord"
        $ prop_encodeDecodeRoundtrip decodeBlockHashRecord encodeBlockHashRecord
    , testProperty "BlockHeader"
        $ prop_encodeDecodeRoundtrip decodeBlockHeader encodeBlockHeader
    , testProperty "BlockPayloadHash"
        $ prop_encodeDecodeRoundtrip decodeBlockPayloadHash encodeBlockPayloadHash
    , testProperty "Nonce"
       $ prop_encodeDecodeRoundtrip decodeNonce encodeNonce
   , testProperty "Time"
       $ prop_encodeDecodeRoundtrip decodeTime encodeTime
   , testProperty "TimeSpan"
       $ prop_encodeDecodeRoundtrip decodeTimeSpan encodeTimeSpan

    -- The following doesn't hold:
    -- , testProperty "target difficulty"
    --     $ prop_iso difficultyToTarget targetToDifficulty
    ]

-- -------------------------------------------------------------------------- --
-- Show-Read

showReadTestCases
    :: (forall a . Arbitrary a => Show a => Eq a => Read a => a -> Property)
    -> [TestTree]
showReadTestCases f =
    [ testProperty "Int" $ f @Int
    , testProperty "()" $ f @()
    , testProperty "Double" $ f @Double
    , testProperty "String" $ f @String
    , testProperty "[Int]" $ f @[Int]
    , testProperty "Maybe Int" $ f @(Maybe Int)
    , testProperty "Either String Int" $ f @(Either String Int)
    , testProperty "Text" $ f @T.Text
    , testProperty "ChainId" $ f @ChainId
    , testProperty "NodeId" $ f @NodeId
    ]

showReadTests :: TestTree
showReadTests = testGroup "Show-Read roundtrips"
    [ testGroup "tread . sshow"
        $ showReadTestCases (prop_iso' tread sshow)
    , testGroup "treadM . sshow"
        $ showReadTestCases (prop_iso' treadM sshow)
    , testGroup "read . sshow"
        $ showReadTestCases (prop_iso' (Right @() . read) sshow)
    ]

-- -------------------------------------------------------------------------- --
-- Base64

base64RoundtripTests :: TestTree
base64RoundtripTests = testGroup "Base64 encoding roundtrips"
    [ testProperty "decodeB64Text . encodeB64Text"
        $ prop_iso' decodeB64Text encodeB64Text
    , testProperty "decodeB64UrlText . encodeB64UrlText"
        $ prop_iso' decodeB64UrlText encodeB64UrlText
    , testProperty "decodeB64UrlNoPaddingText . encodeB64UrlNoPaddingText"
        $ prop_iso' decodeB64UrlText encodeB64UrlText
    ]

-- -------------------------------------------------------------------------- --
-- HasTextReprestation

hasTextRepresentationTests :: TestTree
hasTextRepresentationTests = testGroup "HasTextRepresentation roundtrips"
    [ testProperty "ChainwebVersion" $ prop_iso' @_ @ChainwebVersion fromText toText
    , testProperty "ChainId" $ prop_iso' @_ @ChainId fromText toText
    , testProperty "NodeId" $ prop_iso' @_ @NodeId fromText toText
    , testProperty "BlockHash" $ prop_iso' @_ @BlockHash fromText toText
    , testProperty "Seconds" $ prop_iso' @_ @Seconds fromText toText
    , testProperty "Hostname" $ prop_iso' @_ @Hostname fromText toText
    , testProperty "Port" $ prop_iso' @_ @Port fromText toText
    , testProperty "HostAddress" $ prop_iso' @_ @HostAddress fromText toText
    , testProperty "T.Text" $ prop_iso' @_ @T.Text fromText toText
    , testProperty "[Char]" $ prop_iso' @_ @[Char] fromText toText
    , testProperty "PeerId" $ prop_iso' @_ @PeerId fromText toText
    , testProperty "P2pNetworkId" $ prop_iso' @_ @P2pNetworkId fromText toText
    ]

