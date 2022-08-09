{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Chainweb.Pact.RestAPI.SPV

import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Aeson
import Data.Aeson.Encoding
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Int
import qualified Data.Text as T

import Pact.Parse

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.Configuration
import Chainweb.Cut.Create
import Chainweb.Cut.CutHashes
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Miner.Config
import Chainweb.Miner.Pact
import Chainweb.NodeVersion
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.NodeInfo
import Chainweb.SPV
import Chainweb.SPV.EventProof
import Chainweb.SPV.PayloadProof
import Chainweb.Test.Orphans.Internal (EventPactValue(..))
import Chainweb.Test.SPV.EventProof hiding (tests)
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Network.X509.SelfSigned

import P2P.Node
import P2P.Node.Configuration
import P2P.Peer
import P2P.Test.Orphans ()

import Utils.Logging

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
    , timeSpanTests
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
        $ prop_encodeDecode decodeMerkleLogHash (encodeMerkleLogHash @ChainwebMerkleHashAlgorithm)
    , testProperty "BlockHash"
        $ prop_encodeDecode decodeBlockHash (encodeBlockHash @ChainwebMerkleHashAlgorithm)
    , testProperty "BlockHash_ Keccak_256"
        $ prop_encodeDecode decodeBlockHash (encodeBlockHash @Keccak_256)
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

    , testGroup "ChainwebMerkleHashAlgorithm"
        [ testProperty "BlockPayloadHash"
            $ prop_encodeDecode decodeBlockPayloadHash (encodeBlockPayloadHash @ChainwebMerkleHashAlgorithm)
        , testProperty "BlockTransactionsHash"
            $ prop_encodeDecode decodeBlockTransactionsHash (encodeBlockTransactionsHash @ChainwebMerkleHashAlgorithm)
        , testProperty "BlockTransactionsHash"
            $ prop_encodeDecode decodeBlockTransactionsHash (encodeBlockTransactionsHash @ChainwebMerkleHashAlgorithm)
        , testProperty "BlockEventsHash"
            $ prop_encodeDecode decodeBlockEventsHash (encodeBlockEventsHash @ChainwebMerkleHashAlgorithm)
        ]

    , testGroup "Keccak_256"
        [ testProperty "BlockPayloadHash"
            $ prop_encodeDecode decodeBlockPayloadHash (encodeBlockPayloadHash @Keccak_256)
        , testProperty "BlockTransactionsHash"
            $ prop_encodeDecode decodeBlockTransactionsHash (encodeBlockTransactionsHash @Keccak_256)
        , testProperty "BlockTransactionsHash"
            $ prop_encodeDecode decodeBlockTransactionsHash (encodeBlockTransactionsHash @Keccak_256)
        , testProperty "BlockEventsHash"
            $ prop_encodeDecode decodeBlockEventsHash (encodeBlockEventsHash @Keccak_256)
        ]

    -- SPV
    , testGroup "SPV"
        [ testProperty "Int256 LE"
            $ prop_encodeDecode getInt256Le putInt256Le
        , testProperty "Int256 BE"
            $ prop_encodeDecode getInt256Be putInt256Be
        , testProperty "Bytes"
            $ prop_encodeDecode decodeBytes encodeBytes
        , testProperty "String"
            $ prop_encodeDecode decodeString encodeString
        , testProperty "ModuleName"
            $ prop_encodeDecode decodeModuleName encodeModuleName
        -- FIXME limit to empty spec and info
        , testProperty "ModRef"
            $ prop_encodeDecode (PactEventModRef <$> decodeModRef) (encodeModRef . _getPactEventModRef)
        , testProperty "Integer"
            $ prop_encodeDecode decodeInteger encodeInteger
        , testProperty "Decimal"
            $ prop_encodeDecode (PactEventDecimal <$> decodeDecimal) (encodeDecimal . _getPactEventDecimal)
        , testProperty "Hash"
            $ prop_encodeDecode decodeHash encodeHash
        , testProperty "Array[Int256]"
            $ prop_encodeDecode (decodeArray decodeInteger) (`encodeArray` encodeInteger)
        , testProperty "Array[Bytes]"
            $ prop_encodeDecode (decodeArray decodeBytes) (`encodeArray` encodeBytes)

        -- FIXME "too few bytes"
        , testProperty "PactEvent"
            $ prop_encodeDecode decodePactEvent encodePactEvent
        -- FIXME "pending bytes"
        , testProperty "PactParam"
            $ prop_encodeDecode (EventPactValue <$> decodeParam) (encodeParam . getEventPactValue)

        -- FIXME "pending bytes"
        , testProperty "OutputEvents"
            $ prop_encodeDecode decodeOutputEvents encodeOutputEvents
        ]

    -- Mining
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
    , testProperty "Micros" $ f @Micros
    , testProperty "ChainId" $ f @ChainId
    , testProperty "ChainwebVersion" $ f @ChainwebVersion
    , testProperty "Nonce" $ f @Nonce
    , testProperty "HashDifficulty" $ f @HashDifficulty
    , testProperty "HashTarget" $ f @HashTarget
    , testProperty "MerkleRootType" $ f @MerkleRootType
    , testProperty "MerkleLogHash" $ f @(MerkleLogHash ChainwebMerkleHashAlgorithm)
    , testProperty "MerkleLogHash Keccak_256" $ f @(MerkleLogHash Keccak_256)
    , testProperty "PowHash" $ f @PowHash
    , testProperty "PowHashNat" $ f @PowHashNat
    , testProperty "BlockHash" $ f @BlockHash
    , testProperty "BlockHash_ Keccak_256" $ f @(BlockHash_ Keccak_256)
    , testProperty "BlockHashRecord" $ f @BlockHashRecord
    , testProperty "BlockHeader" $ f @BlockHeader
    , testProperty "HeaderUpdate" $ f @HeaderUpdate
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
    , testProperty "Peer" $ f @Peer
    , testProperty "NetworkId" $ f @NetworkId
    , testProperty "ChainDatabaseGcConfig" $ f @ChainDatabaseGcConfig
    , testProperty "MerkleRootType" $ f @MerkleRootType
    , testProperty "ChainwebConfiguration" $ f @ChainwebConfiguration
    , testProperty "Probability" $ f @Probability
    , testProperty "LogFilterRule" $ f @LogFilterRule
    , testProperty "LogFilter" $ f @LogFilter
    , testProperty "BlockHashWithHeight" $ f @BlockHashWithHeight
    , testProperty "CutId" $ f @CutId
    , testProperty "CutHashes" $ f @CutHashes
    , testProperty "NodeVersion" $ f @NodeVersion
    , testProperty "NodeInfo" $ f @NodeInfo
    , testProperty "EnableConfig MiningConfig" $ f @(EnableConfig MiningConfig)
    , testProperty "NextItem Int" $ f @(NextItem Int)
    , testProperty "Page BlockHash BlockHeader" $ f @(Page BlockHash BlockHeader)
    , testProperty "X509CertPem" $ f @X509CertPem
    , testProperty "X509CertChainPem" $ f @X509CertChainPem
    , testProperty "X509KeyPem" $ f @X509KeyPem

    , testGroup "SPV"
        [ testProperty "SpvAlgorithm" $ f @SpvAlgorithm
        , testProperty "SpvSubjectType" $ f @SpvAlgorithm
        , testProperty "SpvSubjectIdentifier" $ f @SpvSubjectIdentifier
        , testProperty "SpvRequest" $ f @SpvRequest
        , testProperty "Spv2Request" $ f @Spv2Request
        , testProperty "TransactionProof" $ f @(TransactionProof ChainwebMerkleHashAlgorithm)
        , testProperty "TransactionOutputProof" $ f @(TransactionOutputProof ChainwebMerkleHashAlgorithm)
        , testProperty "PayloadProof" $ f @(PayloadProof ChainwebMerkleHashAlgorithm)
        , testProperty "SomePayloadProof" $ f @(SomePayloadProof)
        ]

    , testGroup "Miner"
        [ testProperty "MinerId" $ f @Miner
        , testProperty "MinerKeys" $ f @Miner
        , testProperty "Miner" $ f @Miner
        ]

    , testGroup "Mempool"
        [ testProperty "TransactionHash" $ f @TransactionHash
        , testProperty "PendingTransactions" $ f @PendingTransactions
        , testProperty "LookupResult Text" $ f @(LookupResult T.Text)
        , testProperty "TransactionMetadata" $ f @TransactionMetadata
        , testProperty "ValidatedTransaction" $ f @(ValidatedTransaction T.Text)
        , testProperty "MockTx" $ f @MockTx
        , testProperty "GasLimit" $ f @GasLimit
        , testProperty "GasPrice" $ f @GasPrice
        , testProperty "ParsedDecimal" $ f @ParsedDecimal
        , testProperty "ParsedInteger" $ f @ParsedInteger
        ]

    -- Chainweb.Payload
    , testGroup "Payload types"
        [ testProperty "Transaction" $ f @Transaction
        , testProperty "MinerData" $ f @MinerData
        , testProperty "CoinbaseOutput" $ f @CoinbaseOutput
        , testProperty "TransactionOutput" $ f @TransactionOutput
        , testGroup "ChainwebMerkleHashAlgorithm"
            [ testProperty "BlockPayloadHash" $ f @BlockPayloadHash
            , testProperty "BlockTransactionsHash" $ f @BlockTransactionsHash
            , testProperty "BlockOutputsHash" $ f @BlockOutputsHash
            , testProperty "PayloadData" $ f @PayloadData
            , testProperty "BlockTransactions" $ f @BlockTransactions
            , testProperty "BlockPayload" $ f @BlockPayload
            , testProperty "BlockOutputs" $ f @BlockOutputs
            , testProperty "TransactionTree" $ f @TransactionTree
            , testProperty "OutputTree" $ f @OutputTree
            , testProperty "PayloadData" $ f @PayloadData
            , testProperty "PayloadWithOutputs" $ f @PayloadWithOutputs
            , testProperty "PayloadOutputProof" $ f @(PayloadProof ChainwebMerkleHashAlgorithm)
            , testProperty "BlockEventsHash" $ f @(BlockEventsHash_ ChainwebMerkleHashAlgorithm)
            ]

        , testGroup "Keccak_256"
            [ testProperty "BlockPayloadHash" $ f @(BlockPayloadHash_ Keccak_256)
            , testProperty "BlockTransactionsHash" $ f @(BlockTransactionsHash_ Keccak_256)
            , testProperty "BlockOutputsHash" $ f @(BlockOutputsHash_ Keccak_256)
            , testProperty "PayloadData" $ f @(PayloadData_ Keccak_256)
            , testProperty "BlockTransactions" $ f @(BlockTransactions_ Keccak_256)
            , testProperty "BlockPayload" $ f @(BlockPayload_ Keccak_256)
            , testProperty "BlockOutputs" $ f @(BlockOutputs_ Keccak_256)
            , testProperty "TransactionTree" $ f @(TransactionTree_ Keccak_256)
            , testProperty "OutputTree" $ f @(OutputTree_ Keccak_256)
            , testProperty "PayloadData" $ f @(PayloadData_ Keccak_256)
            , testProperty "PayloadWithOutputs" $ f @(PayloadWithOutputs_ Keccak_256)
            , testProperty "PayloadOutputProof" $ f @(PayloadProof Keccak_256)
            , testProperty "BlockEventsHash" $ f @(BlockEventsHash_ Keccak_256)
            ]
        ]
    ]
    -- Types with ToJSON but without FromJSON instance
    -- * CRLogPair
    -- * RemoteNodeInfo
    -- * JsonSockAddr
    -- * Trace

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
    , testGroup "decodeOrThrow . encode . toJSON"
        $ jsonTestCases (prop_iso' decodeOrThrow (encode . toJSON))
    , testGroup "decodeOrThrow . encodingToLazyByteString . toEncoding"
        $ jsonTestCases (prop_iso' decodeOrThrow (encodingToLazyByteString . toEncoding))
    , testGroup "toEncoding - toJSON consistency"
        $ jsonTestCases (\(x :: a) ->
            (first show . decodeOrThrow @(Either SomeException) @a . encodingToLazyByteString . toEncoding $ x)
            ===
            (first show . decodeOrThrow @(Either SomeException) @a . encode . toJSON $ x)
        )
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
    , testProperty "Micros" $ prop_iso' @_ @Micros fromText toText
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
    , testProperty "MerkleRootType" $ prop_iso' @_ @MerkleRootType fromText toText
    ]

-- -------------------------------------------------------------------------- --
-- Time

timeSpanTests :: TestTree
timeSpanTests = testGroup "TimeSpan roundtrips"
    [ testProperty "timeSpanToMicros Int" $ prop_iso @(TimeSpan Int) microsToTimeSpan timeSpanToMicros
    , testProperty "timeSpanToMicros Int64" $ prop_iso @(TimeSpan Int64) microsToTimeSpan timeSpanToMicros
    , testProperty "timeSpanToMicros Micros" $ prop_iso @(TimeSpan Micros) microsToTimeSpan timeSpanToMicros

    , testProperty "microsToTimeSpan Int" $ prop_iso @_ @(TimeSpan Int) timeSpanToMicros microsToTimeSpan
    , testProperty "microsToTimeSpan Int64" $ prop_iso @_ @(TimeSpan Int64) timeSpanToMicros microsToTimeSpan
    , testProperty "microsToTimeSpan Micros" $ prop_iso @_ @(TimeSpan Micros) timeSpanToMicros microsToTimeSpan

    , testProperty "secondsToTimeSpan Int" $ prop_iso @_ @(TimeSpan Int) timeSpanToSeconds secondsToTimeSpan
    , testProperty "secondsToTimeSpan Int64" $ prop_iso @_ @(TimeSpan Int64) timeSpanToSeconds secondsToTimeSpan
    , testProperty "secondsToTimeSpan Seconds" $ prop_iso @_ @(TimeSpan Micros) timeSpanToSeconds secondsToTimeSpan
    ]
