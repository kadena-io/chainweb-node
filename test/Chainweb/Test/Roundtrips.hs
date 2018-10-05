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
import Chainweb.NodeId
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Chainweb.Test.Orphans.Internal ()

-- -------------------------------------------------------------------------- --
-- Encode-Decode Roundrip Tests
--
-- * [x] encodeB64Text
-- * [x] encodeB64UrlText
-- * [x] encodeBlockHash
-- * [x] encodeBlockHashBytes
-- * [x] encodeBlockHashNat
-- * [x] encodeBlockHashRecord
-- * [x] encodeBlockHeader
-- * [x] encodeBlockHeight
-- * [x] encodeBlockPayloadHash
-- * [x] encodeBlockWeight
-- * [x] encodeChainId
-- * [x] encodeChainwebVersion
-- * [x] encodeHashDifficulty
-- * [x] encodeHashTarget
-- * [x] encodeNodeId
-- * [x] encodeNonce
-- * [x] encodeTime
-- * [x] encodeTimeSpan
-- * [ ] encodeKey
-- * [ ] encodeEntry

tests :: TestTree
tests = testGroup "roundtrip tests"
    [ testGroup "tread . sshow"
        $ showReadTests (prop_iso' tread sshow)
    , testGroup "read . sshow"
        $ showReadTests (prop_iso' (Right @() . read) sshow)
    , testProperty "decodeB64Text . encodeB64Text"
        $ prop_iso' decodeB64Text encodeB64Text
    , testProperty "decodeB64UrlText . encodeB64UrlText"
        $ prop_iso' decodeB64UrlText encodeB64UrlText
    , testProperty "pretty ChainId"
        $ prop_iso' readPrettyChainId prettyChainId
    , testProperty "ChainwebVersion"
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

showReadTests
    :: (forall a . Arbitrary a => Show a => Eq a => Read a => a -> Property)
    -> [TestTree]
showReadTests f =
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

