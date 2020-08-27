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

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck.Instances ()

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Cut.Create
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.MerkleLogHash
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.RestAPI.NetworkID
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import P2P.Node
import P2P.Node.Configuration
import P2P.Peer
import P2P.Test.Orphans ()

-- -------------------------------------------------------------------------- --
-- Roundrip Tests

tests :: TestTree
tests = testGroup "roundtrip tests"
    [ encodeDecodeTests
    , showReadTests
    , base64RoundtripTests
    , hasTextRepresentationTests
    , jsonRoundtripTests
    , jsonKeyRoundtripTests
    ]

-- -------------------------------------------------------------------------- --
-- Binary Encoding rountrips

encodeDecodeTests :: TestTree
encodeDecodeTests = testGroup "Encode-Decode roundtrips"
    [ testProperty "ChainwebVersion"
        $ prop_encodeDecode decodeChainwebVersion encodeChainwebVersion
    , testProperty "ChainId"
        $ prop_encodeDecode decodeChainId encodeChainId
    , testProperty "MerkleLogHash"
        $ prop_encodeDecode decodeMerkleLogHash encodeMerkleLogHash
    , testProperty "BlockHash"
        $ prop_encodeDecode decodeBlockHash encodeBlockHash
    , testProperty "BlockHeight"
        $ prop_encodeDecode decodeBlockHeight encodeBlockHeight
    , testProperty "CutHeight"
        $ prop_encodeDecode decodeCutHeight encodeCutHeight
    , testProperty "PowHash"
        $ prop_encodeDecode decodePowHash encodePowHash
    , testProperty "PowHashNat"
        $ prop_encodeDecode decodePowHashNat encodePowHashNat
    , testProperty "HashDifficulty"
        $ prop_encodeDecode decodeHashDifficulty encodeHashDifficulty
    , testProperty "HashTarget"
        $ prop_encodeDecode decodeHashTarget encodeHashTarget
    , testProperty "BlockWeight"
        $ prop_encodeDecode decodeBlockWeight encodeBlockWeight

    , testProperty "BlockHashRecord"
        $ prop_encodeDecode decodeBlockHashRecord encodeBlockHashRecord
    , testProperty "BlockHeader"
        $ prop_encodeDecode decodeBlockHeader encodeBlockHeader
    , testProperty "Nonce"
       $ prop_encodeDecode decodeNonce encodeNonce
   , testProperty "Time"
       $ prop_encodeDecode decodeTime encodeTime
   , testProperty "TimeSpan"
       $ prop_encodeDecode decodeTimeSpan encodeTimeSpan

    , testProperty "BlockPayloadHash"
        $ prop_encodeDecode decodeBlockPayloadHash encodeBlockPayloadHash
    , testProperty "BlockTransactionsHash"
        $ prop_encodeDecode decodeBlockTransactionsHash encodeBlockTransactionsHash
    , testProperty "BlockTransactionsHash"
        $ prop_encodeDecode decodeBlockTransactionsHash encodeBlockTransactionsHash

    , testProperty "SolvedWork"
        $ prop_encodeDecode decodeSolvedWork encodeSolvedWork

    -- FIXME: decoding depends on version and block height (which is something
    -- that we should fix)
    -- , testProperty "WorkHeader"
    --    $ prop_encodeDecode decodeWorkHeader encodeWorkHeader

    -- TODO Fix this!
    -- The following doesn't hold:
    -- , testProperty "target difficulty"
    --     $ prop_iso difficultyToTarget targetToDifficulty
    ]

-- -------------------------------------------------------------------------- --
-- JSON

jsonTestCases
    :: (forall a . Arbitrary a => Show a => ToJSON a => FromJSON a => Eq a => a -> Property)
    -> [TestTree]
jsonTestCases f =
    [ testProperty "Time Micros" $ f @(Time Micros)
    , testProperty "TimeSpan Micros" $ f @(TimeSpan Micros)
    , testProperty "Seconds" $ f @Seconds
    , testProperty "ChainId" $ f @ChainId
    , testProperty "ChainwebVersion" $ f @ChainwebVersion
    , testProperty "Nonce" $ f @Nonce
    , testProperty "HashDifficulty" $ f @HashDifficulty
    , testProperty "HashTarget" $ f @HashTarget
    , testProperty "MerkleLogHash" $ f @MerkleLogHash
    , testProperty "PowHash" $ f @PowHash
    , testProperty "PowHashNat" $ f @PowHashNat
    , testProperty "BlockHash" $ f @BlockHash
    , testProperty "BlockHashRecord" $ f @BlockHashRecord
    , testProperty "BlockHeader" $ f @BlockHeader
    , testProperty "BlockWeight" $ f @BlockWeight
    , testProperty "P2pNodeStats" $ f @P2pNodeStats
    , testProperty "P2pSessionResult" $ f @P2pSessionResult
    , testProperty "P2pSessionInfo" $ f @P2pSessionInfo
    , testProperty "P2pConfiguration" $ f @P2pConfiguration
    , testProperty "Hostname" $ f @Hostname
    , testProperty "Port" $ f @Port
    , testProperty "HostAddress" $ f @HostAddress
    , testProperty "PeerConfig" $ f @PeerConfig
    , testProperty "PeerId" $ f @PeerId
    , testProperty "PeerInfo" $ f @PeerInfo
    , testProperty "NetworkId" $ f @NetworkId
    , testProperty "ChainDatabaseGcConfig" $ f @ChainDatabaseGcConfig

    , testProperty "BlockPayloadHash" $ f @BlockPayloadHash
    , testProperty "BlockTransactionsHash" $ f @BlockTransactionsHash
    , testProperty "BlockOutputsHash" $ f @BlockOutputsHash
    , testProperty "Transaction" $ f @Transaction
    , testProperty "TransactionOutput" $ f @TransactionOutput
    , testProperty "PayloadData" $ f @PayloadData
    , testProperty "BlockTransactions" $ f @BlockTransactions
    , testProperty "MinerData" $ f @MinerData
    ]

jsonRoundtripTests :: TestTree
jsonRoundtripTests = testGroup "JSON roundtrips"
    [ testGroup "decodeOrThrow . encode"
        $ jsonTestCases (prop_iso' decodeOrThrow encode)
    , testGroup "decodeOrThrow' . encode"
        $ jsonTestCases (prop_iso' decodeOrThrow' encode)
    , testGroup "decodeStrictOrThrow . encode"
        $ jsonTestCases (prop_iso' decodeStrictOrThrow (BL.toStrict . encode))
    , testGroup "decodeStrictOrThrow' . encode"
        $ jsonTestCases (prop_iso' decodeStrictOrThrow' (BL.toStrict . encode))
    ]

jsonKeyTestCases
    :: (forall a . Arbitrary a => Show a => ToJSON a => FromJSON a => Eq a => a -> Property)
    -> [TestTree]
jsonKeyTestCases f =
    [ testProperty "HashMap ChainId ()" $ f @(HM.HashMap ChainId ())
    , testProperty "HashMap BlockHash ()" $ f @(HM.HashMap BlockHash ())
    , testProperty "HashMap BlockWeight ()" $ f @(HM.HashMap BlockWeight ())
    , testProperty "HashMap HashDifficulty ()" $ f @(HM.HashMap HashDifficulty ())
    ]

jsonKeyRoundtripTests :: TestTree
jsonKeyRoundtripTests = testGroup "JSON Key roundtrips"
    [ testGroup "decodeOrThrow . encode"
        $ jsonKeyTestCases (prop_iso' decodeOrThrow encode)
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
    , testProperty "ChainwebVersion" $ prop_iso' @_ @ChainwebVersion eitherFromText toText
    , testProperty "ChainId" $ prop_iso' @_ @ChainId fromText toText
    , testProperty "BlockHash" $ prop_iso' @_ @BlockHash fromText toText
    , testProperty "Seconds" $ prop_iso' @_ @Seconds fromText toText
    , testProperty "Hostname" $ prop_iso' @_ @Hostname fromText toText
    , testProperty "Port" $ prop_iso' @_ @Port fromText toText
    , testProperty "HostAddress" $ prop_iso' @_ @HostAddress fromText toText
    , testProperty "T.Text" $ prop_iso' @_ @T.Text fromText toText
    , testProperty "[Char]" $ prop_iso' @_ @[Char] fromText toText
    , testProperty "PeerId" $ prop_iso' @_ @PeerId fromText toText
    , testProperty "Int" $ prop_iso' @_ @Int fromText toText
    , testProperty "P2pNetworkId" $ prop_iso' @_ @NetworkId fromText toText
    , testProperty "Transaction" $ prop_iso' @_ @Transaction fromText toText
    , testProperty "TransactionOutput" $ prop_iso' @_ @TransactionOutput fromText toText
    , testProperty "ChainDatabaseGcConfig" $ prop_iso' @_ @ChainDatabaseGcConfig fromText toText
    ]
