{-# LANGUAGE FlexibleContexts #-}
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

import Crypto.Hash.Algorithms

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Cut.CutHashes
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Mempool.RestAPI
import Chainweb.MerkleLogHash
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.RestAPI.NetworkID
import Chainweb.SPV
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import P2P.Node
import P2P.Node.Configuration
import P2P.Peer

import Utils.Logging.Config

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
    , configRoundtripTests
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
    , testProperty "ChainNodeId"
        $ prop_encodeDecodeRoundtrip decodeChainNodeId encodeChainNodeId
    , testProperty "MerkleLogHash"
        $ prop_encodeDecodeRoundtrip decodeMerkleLogHash encodeMerkleLogHash
    , testProperty "BlockHash"
        $ prop_encodeDecodeRoundtrip decodeBlockHash encodeBlockHash
    , testProperty "BlockHeight"
        $ prop_encodeDecodeRoundtrip decodeBlockHeight encodeBlockHeight
    , testProperty "PowHash"
        $ prop_encodeDecodeRoundtrip decodePowHash encodePowHash
    , testProperty "PowHashNat"
        $ prop_encodeDecodeRoundtrip decodePowHashNat encodePowHashNat
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
    , testProperty "Nonce"
       $ prop_encodeDecodeRoundtrip decodeNonce encodeNonce
   , testProperty "Time"
       $ prop_encodeDecodeRoundtrip decodeTime encodeTime
   , testProperty "TimeSpan"
       $ prop_encodeDecodeRoundtrip decodeTimeSpan encodeTimeSpan

    , testProperty "BlockPayloadHash"
        $ prop_encodeDecodeRoundtrip decodeBlockPayloadHash encodeBlockPayloadHash
    , testProperty "BlockTransactionsHash"
        $ prop_encodeDecodeRoundtrip decodeBlockTransactionsHash encodeBlockTransactionsHash
    , testProperty "BlockTransactionsHash"
        $ prop_encodeDecodeRoundtrip decodeBlockTransactionsHash encodeBlockTransactionsHash

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
    [ testProperty "BackendConfig" $ f @BackendConfig
    , testProperty "BlockHash" $ f @BlockHash
    , testProperty "BlockHashRecord" $ f @BlockHashRecord
    , testProperty "BlockHeader" $ f @BlockHeader
    , testProperty "BlockOutputs" $ f @BlockOutputs
    , testProperty "BlockOutputsHash" $ f @BlockOutputsHash
    , testProperty "BlockPayload" $ f @BlockPayload
    , testProperty "BlockPayloadHash" $ f @BlockPayloadHash
    , testProperty "BlockTransactions" $ f @BlockTransactions
    , testProperty "BlockTransactionsHash" $ f @BlockTransactionsHash
    , testProperty "BlockWeight" $ f @BlockWeight
    , testProperty "ChainId" $ f @ChainId
    , testProperty "ChainNodeId" $ f @ChainNodeId
    , testProperty "ChainwebVersion" $ f @ChainwebVersion
    , testProperty "ClusterId" $ f @ClusterId
    , testProperty "CutId" $ f @CutId
    , testProperty "CoinbaseOutput" $ f @CoinbaseOutput
    , testProperty "CutHashes" $ f @CutHashes
    , testProperty "EnableConfig ()" $ f @(EnableConfig ())
    , testProperty "HandleConfig" $ f @HandleConfig
    , testProperty "HashDifficulty" $ f @HashDifficulty
    , testProperty "HashTarget" $ f @HashTarget
    , testProperty "HostAddress" $ f @HostAddress
    , testProperty "Hostname" $ f @Hostname
    , testProperty "LogFormat" $ f @LogFormat
    , testProperty "LogConfig" $ f @LogConfig
    , testProperty "MempoolP2pConfig" $ f @MempoolP2pConfig
    , testProperty "MerkleLogHash" $ f @MerkleLogHash
    , testProperty "MinerConfig" $ f @MinerConfig
    , testProperty "MinerData" $ f @MinerData
    , testProperty "MinerInfo" $ f @MinerInfo
    , testProperty "NetworkId" $ f @NetworkId
    , testProperty "NextItem Int" $ f @(NextItem Int)
    , testProperty "NodeId" $ f @NodeId
    , testProperty "Nonce" $ f @Nonce
    , testProperty "ObjectEncoded BlockHeader" $ f @(ObjectEncoded BlockHeader)
    , testProperty "OutputTree" $ f @OutputTree
    , testProperty "P2pConfiguration" $ f @P2pConfiguration
    , testProperty "P2pNodeStats" $ f @P2pNodeStats
    , testProperty "P2pSessionInfo" $ f @P2pSessionInfo
    , testProperty "P2pSessionResult" $ f @P2pSessionResult
    , testProperty "PayloadData" $ f @PayloadData
    , testProperty "PayloadWithOutputs" $ f @PayloadWithOutputs
    , testProperty "Peer" $ f @Peer
    , testProperty "PeerConfig" $ f @PeerConfig
    , testProperty "PeerId" $ f @PeerId
    , testProperty "PeerInfo" $ f @PeerInfo
    , testProperty "PendingTransactions" $ f @PendingTransactions
    , testProperty "Port" $ f @Port
    , testProperty "PowHash" $ f @PowHash
    , testProperty "PowHashNat" $ f @PowHashNat
    , testProperty "Seconds" $ f @Seconds
    , testProperty "TimeSpan Micros" $ f @(TimeSpan Micros)
    , testProperty "Transaction" $ f @Transaction
    , testProperty "TransactionHash" $ f @TransactionHash
    , testProperty "TransactionOutput" $ f @TransactionOutput
    , testProperty "TransactionOutputProof SHA512t_256" $ f @(TransactionOutputProof SHA512t_256)
    , testProperty "TransactionProof SHA512t_256" $ f @(TransactionProof SHA512t_256)
    , testProperty "TransactionTree" $ f @TransactionTree
    , testProperty "Time Micros" $ f @(Time Micros)
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
    , testProperty "HashMap MerkleLogHash ()" $ f @(HM.HashMap MerkleLogHash ())
    , testProperty "HashMap BlockPayloadHash ()" $ f @(HM.HashMap BlockPayloadHash ())
    , testProperty "HashMap PowHashNat ()" $ f @(HM.HashMap PowHashNat ())
    ]

jsonKeyRoundtripTests :: TestTree
jsonKeyRoundtripTests = testGroup "JSON Key roundtrips"
    [ testGroup "decodeOrThrow . encode"
        $ jsonKeyTestCases (prop_iso' decodeOrThrow encode)
    ]

-- -------------------------------------------------------------------------- --
-- Configuration JSON roundtrips

configRoundtripTests :: TestTree
configRoundtripTests = testGroup "Configuration JSON roundtrips"
    [ testGroup "decodeOrThrow . encode"
        $ configTestCases (\a -> prop_iso' (\x -> decodeOrThrow x <*> pure a) encode a)
    ]

configTestCases
    :: (forall a . Show a => ToJSON a => FromJSON (a -> a) => Eq a => a -> Property)
    -> [TestTree]
configTestCases f =
    [ testProperty "ChainwebConfiguration Development" $ f $ defaultChainwebConfiguration Development
    , testProperty "ChainwebConfiguration Testnet01" $ f $ defaultChainwebConfiguration Testnet01
    , testProperty "EnableConfig PeerConfig" $ f (defaultEnableConfig defaultPeerConfig)
    , testProperty "LogConfig" $ f defaultLogConfig
    , testProperty "MempoolP2pConfig" $ f defaultMempoolP2pConfig
    , testProperty "MinerConfig" $ f defaultMinerConfig
    , testProperty "P2pConfiguration" $ f defaultP2pConfiguration
    , testProperty "PeerConfig" $ f defaultPeerConfig
    , testProperty "BackendConfig" $ f defaultBackendConfig
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
    , testProperty "ChainNodeId" $ f @ChainNodeId
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
    , testProperty "BlockHash" $ prop_iso' @_ @BlockHash fromText toText
    , testProperty "ChainId" $ prop_iso' @_ @ChainId fromText toText
    , testProperty "ChainNodeId" $ prop_iso' @_ @ChainNodeId fromText toText
    , testProperty "ClusterId" $ prop_iso' @_ @ClusterId fromText toText
    , testProperty "CoinbaseOutput" $ prop_iso' @_ @CoinbaseOutput fromText toText
    , testProperty "CutId" $ prop_iso' @_ @CutId fromText toText
    , testProperty "HandleConfig" $ prop_iso' @_ @HandleConfig fromText toText
    , testProperty "HostAddress" $ prop_iso' @_ @HostAddress fromText toText
    , testProperty "Hostname" $ prop_iso' @_ @Hostname fromText toText
    , testProperty "HostPreference" $ prop_iso' @_ @HostPreference fromText toText
    , testProperty "Int" $ prop_iso' @_ @Int fromText toText
    , testProperty "LogFormat" $ prop_iso' @_ @LogFormat fromText toText
    , testProperty "MerkleLogHash" $ prop_iso' @_ @MerkleLogHash fromText toText
    , testProperty "MinerData" $ prop_iso' @_ @MinerData fromText toText
    , testProperty "NetworkId" $ prop_iso' @_ @NetworkId fromText toText
    , testProperty "NodeId" $ prop_iso' @_ @NodeId fromText toText
    , testProperty "P2pNetworkId" $ prop_iso' @_ @NetworkId fromText toText
    , testProperty "PeerId" $ prop_iso' @_ @PeerId fromText toText
    , testProperty "PeerInfo" $ prop_iso' @_ @PeerInfo fromText toText
    , testProperty "Port" $ prop_iso' @_ @Port fromText toText
    , testProperty "Seconds" $ prop_iso' @_ @Seconds fromText toText
    , testProperty "T.Text" $ prop_iso' @_ @T.Text fromText toText
    , testProperty "Transaction" $ prop_iso' @_ @Transaction fromText toText
    , testProperty "TransactionOutput" $ prop_iso' @_ @TransactionOutput fromText toText
    , testProperty "[Char]" $ prop_iso' @_ @[Char] fromText toText
    ]

