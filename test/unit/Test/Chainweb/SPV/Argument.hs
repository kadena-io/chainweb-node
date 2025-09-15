{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Test.Chainweb.SPV.Argument
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Chainweb.SPV.Argument
( tests
) where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Pact.Payload
import Chainweb.PayloadProvider.EVM.Genesis qualified as EVM
import Chainweb.PayloadProvider.EVM.Header qualified as EVM
import Chainweb.PayloadProvider.EVM.Receipt qualified as EVM
import Chainweb.PayloadProvider.Pact.Genesis qualified as Pact
import Chainweb.SPV.Argument
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.EvmDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Registry
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString qualified as B
import Data.Coerce
import Data.Hash.SHA2
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.MerkleLog qualified as ML
import Data.MerkleLog.V1 qualified as MLV1
import Data.Text qualified as T
import Data.Typeable
import Data.Vector qualified as V
import Ethereum.Block qualified as E
import Ethereum.Misc qualified as E
import Numeric.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "Chainweb.SPV.Argument"
    [ testCase "Pact Output Argument" test_pactOutputArgument
    , testCase "Legacy Pact Output Argument" test_legacyPactOutputArgument
    , testCase "EVM JSON Roundtrips" test_jsonRoundtrips
    , testCase "EVM Header Arguments" test_evmHeaderArguments
    ]

-- -------------------------------------------------------------------------- --
-- JSON Roundrip Tests for Test Data

test_jsonRoundtrip
    :: Typeable a
    => Show a
    => Eq a
    => ToJSON a
    => FromJSON a
    => a
    -> IO ()
test_jsonRoundtrip a = assertEqual
    ("json roundtrip for type " <> show (typeOf a))
    (Right a)
    (eitherDecode (encode a))

test_jsonRoundtrips :: IO ()
test_jsonRoundtrips = withVersion evmDevnet $ do
    test_jsonRoundtrip testHeader
    test_jsonRoundtrip testPayload
    test_jsonRoundtrip testRpcReceipt

-- -------------------------------------------------------------------------- --
-- Test Data

pactMainnetGenesisPayload :: ChainId -> Maybe PayloadWithOutputs
pactMainnetGenesisPayload = withVersion Mainnet01 Pact.genesisPayload

genesisData :: HasVersion => ChainId -> IO (BlockHeader, PayloadWithOutputs)
genesisData cid = do
    let hdr = genesisBlockHeader cid
    pwo <- case pactMainnetGenesisPayload cid of
        Just x -> return x
        Nothing -> error "genesis block for chain 0 not found"
    assertEqual "block payload hash of header and payload match"
        (view blockPayloadHash hdr)
        (_payloadWithOutputsPayloadHash pwo)
    return (hdr, pwo)

-- evmGenesisData :: ChainId -> IO (BlockHeader, PayloadWithOutputs)
-- evmGenesisData cid = do
--     let hdr = EVM.genesisBlocks EvmDevelopment cid
--     EVM.hdr hdr
--     -- pwo <- case preview (ixg cid) (genesisPayload EvmDevelopment) of
--     --     Just x -> return x
--     --     Nothing -> error "genesis block for chain 0 not found"
--     -- assertEqual "block payload hash of header and payload match"
--     --     (view blockPayloadHash hdr)
--     --     (_payloadWithOutputsPayloadHash pwo)
--     -- return (hdr, pwo)

-- -------------------------------------------------------------------------- --
-- Tests

test_pactOutputArgument :: IO ()
test_pactOutputArgument = withVersion mainnet $ do
    (hdr, pwo) <- genesisData (unsafeChainId 0)
    hdrArg <- test_hdr hdr

    let txCount = length $ _payloadWithOutputsTransactions pwo

    -- run test for each tx in the block
    forM_ [0 .. txCount - 1] $ \txPos -> do

        pwoArg <- test_txOutput txPos pwo

        -- compose pact output and header arguments
        arg <- compose pwoArg hdrArg
        argRoot <- runArg arg
        assertEqual "composed argument gives correct root hash"
            (view blockHash hdr)
            argRoot

        claim <- argumentClaim arg
        assertEqual "argument claim is correct"
            (snd (_payloadWithOutputsTransactions pwo V.! txPos))
            claim

test_legacyPactOutputArgument :: IO ()
test_legacyPactOutputArgument = withVersion mainnet $ do
    (hdr, pwo) <- genesisData (unsafeChainId 0)

    -- create tx output proof for tx 0
    let outs = snd $ payloadWithOutputsToBlockObjects pwo
    let txCount = length (_blockOutputs outs)

    let pld = payloadDataToBlockPayload (payloadWithOutputsToPayloadData pwo)
    let pldTree = headerTree_ @BlockOutputsHash @ChainwebMerkleHashAlgorithm pld
    let hdrTree = headerTree_ @BlockPayloadHash @ChainwebMerkleHashAlgorithm hdr

    -- run test for each tx in the block
    forM_ [0 .. txCount - 1] $ \txPos -> do
        let (outs0, outs0Pos, outs0Tree) =
                bodyTree @ChainwebMerkleHashAlgorithm outs txPos

        -- create block payload proof for for outputs
        proof <- MLV1.merkleTreeProof_ outs0 $
            (outs0Pos, outs0Tree) NE.:| [ pldTree, hdrTree ]

        root <- MLV1.runMerkleProof proof
        assertEqual "body proof of tx output gives correct root hash"
            (view blockHash hdr)
            (coerce root)

        -- create legacy pact output argument
        let arg = PactLegacyProof proof
        argRoot <- runArg arg
        assertEqual "pact output argument gives correct root hash"
            (view blockHash hdr)
            argRoot

        claim <- argumentClaim arg
        assertEqual "argument claim is correct"
            (snd (_payloadWithOutputsTransactions pwo V.! txPos))
            claim

test_hdr
    :: HasVersion
    => BlockHeader
    -> IO (Argument BlockPayloadHash BlockHash)
test_hdr hdr = do
    hdrProof <- headerProofV2 @BlockPayloadHash @ChainwebMerkleHashAlgorithm hdr
    assertEqual "header proof gives correct root hash"
        (view blockHash hdr)
        (coerce $ ML.runProof hdrProof)

    -- create header argument
    let hdrArg = BlockPayloadHashArgument hdrProof
    hdrArgRoot <- runArg hdrArg
    assertEqual "header argument gives correct root hash"
        (view blockHash hdr)
        hdrArgRoot

    return hdrArg

test_txOutput
    :: Int
    -> PayloadWithOutputs
    -> IO (Argument TransactionOutput BlockPayloadHash)
test_txOutput txPos pwo = do
    -- create tx output proof for tx 0
    let outs = snd $ payloadWithOutputsToBlockObjects pwo
    outs0Proof <- bodyProofV2 @ChainwebMerkleHashAlgorithm outs txPos
    assertEqual "body proof of tx output gives correct root hash"
        (_payloadWithOutputsOutputsHash pwo)
        (coerce $ ML.runProof outs0Proof)

    -- create block payload proof for for outputs
    let pld = payloadDataToBlockPayload (payloadWithOutputsToPayloadData pwo)
    pldProof <- headerProofV2 @BlockOutputsHash @ChainwebMerkleHashAlgorithm pld
    assertEqual "block payload proof gives correct root hash"
        (_payloadWithOutputsPayloadHash pwo)
        (coerce $ ML.runProof pldProof)

    -- compose proofs into pact output proof
    pwoProof <- ML.composeProofs outs0Proof pldProof
    assertEqual "composed pact output proof gives correct root hash"
        (_payloadWithOutputsPayloadHash pwo)
        (coerce $ ML.runProof pwoProof)

    -- create pact output argument
    let pwoArg = PactOutputArgument @BlockPayloadHash pwoProof
    pwoArgRoot <- runArg pwoArg
    assertEqual "pact output argument gives correct root hash"
        (_payloadWithOutputsPayloadHash pwo)
        pwoArgRoot

    return pwoArg

-- -------------------------------------------------------------------------- --
-- EVM Receipt Tests

data ReceiptTestData = ReceiptTestData
    { _receiptTestDataHeader :: BlockHeader
    , _receiptTestDataPayload :: EVM.Header
    , _receiptTestDataRpcReceipts :: [EVM.RpcReceipt]
    }
    deriving (Show, Eq)

simpleTestData :: HasVersion => ReceiptTestData
simpleTestData = ReceiptTestData
    { _receiptTestDataHeader = testHeader
    , _receiptTestDataPayload = testPayload
    , _receiptTestDataRpcReceipts = [testRpcReceipt]
    }

test_evmHeaderArguments :: IO ()
test_evmHeaderArguments =
    withVersion evmDevnet $ test_evmHeaderArgument simpleTestData 0

test_evmHeaderArgument :: HasVersion => ReceiptTestData -> Natural -> IO ()
test_evmHeaderArgument d idx = do
    let trieProof = EVM.rpcReceiptTrieProof rs (EVM.TransactionIndex 0)
    trieProofRoot <- validateTrieProof trieProof
    assertEqual "receipt proof gives correct root hash"
        (EVM._hdrReceiptsRoot pld)
        (EVM.ReceiptsRoot trieProofRoot)

    -- create EthReceiptArgument
    let evmReceiptArg = EthReceiptArgument trieProof
    evmReceiptArgRoot <- runArg evmReceiptArg
    assertEqual "receipt argument gives correct root hash"
        (EVM._hdrReceiptsRoot pld)
        evmReceiptArgRoot

    -- create EVM Payload Proof
    evmPayloadProof <- headerProofV2 @EVM.ReceiptsRoot @ChainwebMerkleHashAlgorithm
        pld
    assertEqual "EVM payload proof gives correct root hash"
        (EVM._hdrPayloadHash pld)
        (coerce $ ML.runProof evmPayloadProof)

    -- create EVM BlockHeaderArgument
    let evmHdrArg = EthHeaderArgument evmPayloadProof
    evmHdrArgRoot <- runArg evmHdrArg
    assertEqual "header argument gives correct root hash"
        (EVM._hdrPayloadHash pld)
        evmHdrArgRoot

    -- compose EVM header and receipt arguments
    payloadArg <- compose evmReceiptArg evmHdrArg
    payloadArgRoot <- runArg payloadArg
    assertEqual "composed argument gives correct root hash"
        (EVM._hdrPayloadHash pld)
        payloadArgRoot

    -- create Header Proof
    hdrProof <- headerProofV2 @BlockPayloadHash @ChainwebMerkleHashAlgorithm hdr
    assertEqual "header proof gives correct root hash"
        (view blockHash hdr)
        (coerce $ ML.runProof hdrProof)

    let hdrArgument = BlockPayloadHashArgument hdrProof
    hdrArgumentRoot <- runArg hdrArgument
    assertEqual "header argument gives correct root hash"
        (view blockHash hdr)
        hdrArgumentRoot

    -- compose payload argument and header argument
    arg <- compose payloadArg hdrArgument >>= \a -> case a of
        ComposeArgument EthReceiptArgument{} EthHeaderArgument{} -> return a
        _ -> assertFailure "Argument is not composition of EthReceiptArgument and EthHeaderArgument" :: IO (Argument EVM.Receipt BlockHash)
    argRoot <- runArg arg
    assertEqual "composed argument gives correct root hash"
        (view blockHash hdr)
        argRoot

  where
    rs = _receiptTestDataRpcReceipts d
    pld = _receiptTestDataPayload d
    hdr = _receiptTestDataHeader d

-- -------------------------------------------------------------------------- --
-- EVM Test Data

testReceipt :: EVM.Receipt
testReceipt = EVM.fromRpcReceipt testRpcReceipt

testRpcReceipt :: EVM.RpcReceipt
testRpcReceipt = case eitherDecodeStrictText jsonReceiptStr of
    Left err -> error $ "failed to decode receipt: " <> show err
    Right x -> x
  where
    jsonReceiptStr :: T.Text
    jsonReceiptStr = [r|
        {
        "type": "0x2",
        "status": "0x1",
        "cumulativeGasUsed": "0x9dcf",
        "logs": [
            {
            "address": "0x0826f2fc8902ca5afe82570560ba752a7f53675c",
            "topics": [
                "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
                "0x000000000000000000000000fb8fb7f9bdc8951040a6d195764905138f7462ed",
                "0x0000000000000000000000000000000000000000000000000000000000000000"
            ],
            "data": "0x0000000000000000000000000000000000000000000000056bc75e2d63100000",
            "blockHash": "0x47c1944c382527f82044276c04a9e0d642da58316d1b963e7a623e03ac085d4f",
            "blockNumber": "0x162",
            "blockTimestamp": "0x67ef2c65",
            "transactionHash": "0x3247ba77980ba47c6a59afc8d73bf5d70eff69e7f129b680b15451e45c3b4d5d",
            "transactionIndex": "0x0",
            "logIndex": "0x0",
            "removed": false
            },
            {
            "address": "0x0826f2fc8902ca5afe82570560ba752a7f53675c",
            "topics": [
                "0x9d2528c24edd576da7816ca2bdaa28765177c54b32fb18e2ca18567fbc2a9550",
                "0x0000000000000000000000000000000000000000000000000000000000000001",
                "0x000000000000000000000000ead0610198d4d802f9d14ec7e8d85d30233ccc6b",
                "0x0000000000000000000000000000000000000000000000000000000000000001"
            ],
            "data": "0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000004000000000000000000000000028f2d8ef4e0fe6b2e945cf5c33a0118a30a623540000000000000000000000000000000000000000000000056bc75e2d63100000",
            "blockHash": "0x47c1944c382527f82044276c04a9e0d642da58316d1b963e7a623e03ac085d4f",
            "blockNumber": "0x162",
            "blockTimestamp": "0x67ef2c65",
            "transactionHash": "0x3247ba77980ba47c6a59afc8d73bf5d70eff69e7f129b680b15451e45c3b4d5d",
            "transactionIndex": "0x0",
            "logIndex": "0x1",
            "removed": false
            }
        ],
        "logsBloom": "0x00000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000800000000000000000000040100000000000000000000000008000000000000000000040000000000000000000000000000020000000000000000000800000000000000000000002010000000000000000000000000000100000000004000000000000000000001000000000000000000000000000000000000000000000000000000008000000000000000002000000002000000000000000000000000000000000000000040000000000060004000000000000000000000000000000000000200000000000000000000000000",
        "transactionHash": "0x3247ba77980ba47c6a59afc8d73bf5d70eff69e7f129b680b15451e45c3b4d5d",
        "transactionIndex": "0x0",
        "blockHash": "0x47c1944c382527f82044276c04a9e0d642da58316d1b963e7a623e03ac085d4f",
        "blockNumber": "0x162",
        "gasUsed": "0x9dcf",
        "effectiveGasPrice": "0x3b9aca07",
        "from": "0xfb8fb7f9bdc8951040a6d195764905138f7462ed",
        "to": "0x0826f2fc8902ca5afe82570560ba752a7f53675c",
        "contractAddress": null
        }
    |]

testPayload :: EVM.Header
testPayload = case eitherDecodeStrictText payloadStr of
    Left err -> error $ "failed to decode payload: " <> show err
    Right x -> x
  where
    -- TODO: the below is not a valid Pectra header, so the test fails
    payloadStr :: T.Text
    payloadStr = [r|
        {
        "parentHash": "0x692d784f999068270a78491f50404073f988a324598799866f740e2a52857a6a",
        "sha3Uncles": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
        "miner": "0xd42d71cdc2a0a78fe7fbe7236c19925f62c442ba",
        "stateRoot": "0x05f700ac2df8e2985433b63e3641e308976dd1a0dc0d29b600f71cc20939cba9",
        "transactionsRoot": "0x8748b3f2e8d6a4a78a6f4b4b39e608cd3765505fd317020ba4c4a1aae4a6018c",
        "receiptsRoot": "0x37cb5c601f1a535769d6a36c35f34c22fea9e9a1a011144381b8e3c7bb31657c",
        "logsBloom": "0x00000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000800000000000000000000040100000000000000000000000008000000000000000000040000000000000000000000000000020000000000000000000800000000000000000000002010000000000000000000000000000100000000004000000000000000000001000000000000000000000000000000000000000000000000000000008000000000000000002000000002000000000000000000000000000000000000000040000000000060004000000000000000000000000000000000000200000000000000000000000000",
        "difficulty": "0x0",
        "number": "0x162",
        "gasLimit": "0x1c9c380",
        "gasUsed": "0x9dcf",
        "timestamp": "0x67ef2c65",
        "extraData": "0x726574682f76312e312e352f6c696e7578",
        "mixHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
        "nonce": "0x0000000000000000",
        "baseFeePerGas": "0x7",
        "withdrawalsRoot": "0x2f1a2b19c3801a44641db3f88532b818b585754dc389fd18ab6a0299e2ebdbc2",
        "blobGasUsed": "0x0",
        "excessBlobGas": "0x0",
        "parentBeaconBlockRoot": "0x80af8b91b32cae2e3c3b17ef0b6ce9124e01176bf953f192633a6cc718f129ba",
        "hash": "0x47c1944c382527f82044276c04a9e0d642da58316d1b963e7a623e03ac085d4f",
        }
    |]

testHeader :: HasVersion => BlockHeader
testHeader = case eitherDecodeStrictText headerStr of
    Left err -> error $ "failed to decode header: " <> show err
    Right (ObjectEncoded x) -> x
  where
    headerStr :: T.Text
    headerStr = [r|
        {
        "nonce": "0",
        "creationTime": 1743727718036200,
        "parent": "gK-LkbMsri48OxfvC2zpEk4BF2v5U_GSYzpsxxjxKbo",
        "adjacents": {
            "10": "RJuqDTjhoclukSr7qPOkg0IOAcEJ1bIeltehw_-OHTo",
            "5": "xOqpgPjJl0VehoCDX4se2i-cK1Ql_R75YHa1pAuSuWo",
            "15": "B0iB0OTRlVtQgOyCpdomGfJTyXF92R8pcs06upXTOrY"
        },
        "target": "PKUF-3dimGnG6F3iN6MvPUA1hTNt8eECAAAAAAAAAAA",
        "payloadHash": "RXrRaAvhF74-xJcQzVT8ZHJqW7FtgP4y0WWQCy13w9U",
        "chainId": 0,
        "weight": "9puuQnDNZ7WAKQAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "height": 354,
        "chainwebVersion": "evm-development",
        "epochStart": 1743727581567100,
        "featureFlags": 0,
        "hash": "9NDFjt_VVE2_fJD54eLcybRfbp_BAOR2T2HxavWe7ho"
        }
    |]


-- EVM Development block (chain 0, height 354)
-- {
--   "receipts": [
--     {
--       "type": "0x2",
--       "status": "0x1",
--       "cumulativeGasUsed": "0x9dcf",
--       "logs": [
--         {
--           "address": "0x0826f2fc8902ca5afe82570560ba752a7f53675c",
--           "topics": [
--             "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
--             "0x000000000000000000000000fb8fb7f9bdc8951040a6d195764905138f7462ed",
--             "0x0000000000000000000000000000000000000000000000000000000000000000"
--           ],
--           "data": "0x0000000000000000000000000000000000000000000000056bc75e2d63100000",
--           "blockHash": "0x47c1944c382527f82044276c04a9e0d642da58316d1b963e7a623e03ac085d4f",
--           "blockNumber": "0x162",
--           "blockTimestamp": "0x67ef2c65",
--           "transactionHash": "0x3247ba77980ba47c6a59afc8d73bf5d70eff69e7f129b680b15451e45c3b4d5d",
--           "transactionIndex": "0x0",
--           "logIndex": "0x0",
--           "removed": false
--         },
--         {
--           "address": "0x0826f2fc8902ca5afe82570560ba752a7f53675c",
--           "topics": [
--             "0x9d2528c24edd576da7816ca2bdaa28765177c54b32fb18e2ca18567fbc2a9550",
--             "0x0000000000000000000000000000000000000000000000000000000000000001",
--             "0x000000000000000000000000ead0610198d4d802f9d14ec7e8d85d30233ccc6b",
--             "0x0000000000000000000000000000000000000000000000000000000000000001"
--           ],
--           "data": "0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000004000000000000000000000000028f2d8ef4e0fe6b2e945cf5c33a0118a30a623540000000000000000000000000000000000000000000000056bc75e2d63100000",
--           "blockHash": "0x47c1944c382527f82044276c04a9e0d642da58316d1b963e7a623e03ac085d4f",
--           "blockNumber": "0x162",
--           "blockTimestamp": "0x67ef2c65",
--           "transactionHash": "0x3247ba77980ba47c6a59afc8d73bf5d70eff69e7f129b680b15451e45c3b4d5d",
--           "transactionIndex": "0x0",
--           "logIndex": "0x1",
--           "removed": false
--         }
--       ],
--       "logsBloom": "0x00000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000800000000000000000000040100000000000000000000000008000000000000000000040000000000000000000000000000020000000000000000000800000000000000000000002010000000000000000000000000000100000000004000000000000000000001000000000000000000000000000000000000000000000000000000008000000000000000002000000002000000000000000000000000000000000000000040000000000060004000000000000000000000000000000000000200000000000000000000000000",
--       "transactionHash": "0x3247ba77980ba47c6a59afc8d73bf5d70eff69e7f129b680b15451e45c3b4d5d",
--       "transactionIndex": "0x0",
--       "blockHash": "0x47c1944c382527f82044276c04a9e0d642da58316d1b963e7a623e03ac085d4f",
--       "blockNumber": "0x162",
--       "gasUsed": "0x9dcf",
--       "effectiveGasPrice": "0x3b9aca07",
--       "from": "0xfb8fb7f9bdc8951040a6d195764905138f7462ed",
--       "to": "0x0826f2fc8902ca5afe82570560ba752a7f53675c",
--       "contractAddress": null
--     }
--   ],
--   "payload": {
--     "parentHash": "0x692d784f999068270a78491f50404073f988a324598799866f740e2a52857a6a",
--     "sha3Uncles": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
--     "miner": "0xd42d71cdc2a0a78fe7fbe7236c19925f62c442ba",
--     "stateRoot": "0x05f700ac2df8e2985433b63e3641e308976dd1a0dc0d29b600f71cc20939cba9",
--     "transactionsRoot": "0x8748b3f2e8d6a4a78a6f4b4b39e608cd3765505fd317020ba4c4a1aae4a6018c",
--     "receiptsRoot": "0x37cb5c601f1a535769d6a36c35f34c22fea9e9a1a011144381b8e3c7bb31657c",
--     "logsBloom": "0x00000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000800000000000000000000040100000000000000000000000008000000000000000000040000000000000000000000000000020000000000000000000800000000000000000000002010000000000000000000000000000100000000004000000000000000000001000000000000000000000000000000000000000000000000000000008000000000000000002000000002000000000000000000000000000000000000000040000000000060004000000000000000000000000000000000000200000000000000000000000000",
--     "difficulty": "0x0",
--     "number": "0x162",
--     "gasLimit": "0x1c9c380",
--     "gasUsed": "0x9dcf",
--     "timestamp": "0x67ef2c65",
--     "extraData": "0x726574682f76312e312e352f6c696e7578",
--     "mixHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
--     "nonce": "0x0000000000000000",
--     "baseFeePerGas": "0x7",
--     "withdrawalsRoot": "0x2f1a2b19c3801a44641db3f88532b818b585754dc389fd18ab6a0299e2ebdbc2",
--     "blobGasUsed": "0x0",
--     "excessBlobGas": "0x0",
--     "parentBeaconBlockRoot": "0x80af8b91b32cae2e3c3b17ef0b6ce9124e01176bf953f192633a6cc718f129ba",
--     "hash": "0x47c1944c382527f82044276c04a9e0d642da58316d1b963e7a623e03ac085d4f"
--   },
--   "nonce": "0",
--   "creationTime": 1743727718036200,
--   "parent": "gK-LkbMsri48OxfvC2zpEk4BF2v5U_GSYzpsxxjxKbo",
--   "adjacents": {
--     "10": "RJuqDTjhoclukSr7qPOkg0IOAcEJ1bIeltehw_-OHTo",
--     "5": "xOqpgPjJl0VehoCDX4se2i-cK1Ql_R75YHa1pAuSuWo",
--     "15": "B0iB0OTRlVtQgOyCpdomGfJTyXF92R8pcs06upXTOrY"
--   },
--   "target": "PKUF-3dimGnG6F3iN6MvPUA1hTNt8eECAAAAAAAAAAA",
--   "payloadHash": "RXrRaAvhF74-xJcQzVT8ZHJqW7FtgP4y0WWQCy13w9U",
--   "chainId": 0,
--   "weight": "9puuQnDNZ7WAKQAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
--   "height": 354,
--   "chainwebVersion": "evm-development",
--   "epochStart": 1743727581567100,
--   "featureFlags": 0,
--   "hash": "9NDFjt_VVE2_fJD54eLcybRfbp_BAOR2T2HxavWe7ho"
-- }
