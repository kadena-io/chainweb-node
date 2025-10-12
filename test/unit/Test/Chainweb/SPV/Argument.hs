{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

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
import Chainweb.PayloadProvider.EVM.Header qualified as EVM
import Chainweb.PayloadProvider.EVM.Receipt qualified as EVM
import Chainweb.PayloadProvider.Pact.Genesis qualified as Pact
import Chainweb.SPV.Argument
import Chainweb.Version
import Chainweb.Version.EvmTestnet
import Chainweb.Version.Mainnet
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Coerce
import Data.List.NonEmpty qualified as NE
import Data.MerkleLog qualified as ML
import Data.MerkleLog.V1 qualified as MLV1
import Data.Text qualified as T
import Data.Typeable
import Data.Vector qualified as V
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
test_jsonRoundtrips = withVersion evmTestnet $ do
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
    withVersion evmTestnet $ test_evmHeaderArgument simpleTestData 0

test_evmHeaderArgument :: HasVersion => ReceiptTestData -> Natural -> IO ()
test_evmHeaderArgument d _idx = do
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

-- testReceipt :: EVM.Receipt
-- testReceipt = EVM.fromRpcReceipt testRpcReceipt

testRpcReceipt :: EVM.RpcReceipt
testRpcReceipt = case eitherDecodeStrictText jsonReceiptStr of
    Left err -> error $ "failed to decode receipt: " <> show err
    Right x -> x
  where
    jsonReceiptStr :: T.Text
    jsonReceiptStr = [r|
        {
          "status": "0x1",
          "cumulativeGasUsed": "0xb1d6",
          "logs": [
            {
              "address": "0x7f81bfb73cb29c3725779ec2c060787daa2f97d4",
              "topics": [
                "0xb4beca5a896c70c0f896e51f0c7c91592fe79893272eb4d5f41aa376c3c2aa95",
                "0x000000000000000000000000a525fcb70404336c591833a85abb1663b779cddf"
              ],
              "data": "0x0000000000000000000000000000000000000000000000000000000068e6c67d00000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000004f91",
              "blockHash": "0x78bc1a838d8e69c3f980d3c69ad79bf9b1dbc934ca9f9cb572408fac3259d161",
              "blockNumber": "0x45426",
              "blockTimestamp": "0x68e6c67d",
              "transactionHash": "0x807bd8e1999cc975bd14826033e998c92d1eb604342ae0dcdc3e2dec015466c3",
              "transactionIndex": "0x0",
              "logIndex": "0x0",
              "removed": false
            }
          ],
          "logsBloom": "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000002000000001200000000000000000000000000000000001000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000002000",
          "type": "0x0",
          "transactionHash": "0x807bd8e1999cc975bd14826033e998c92d1eb604342ae0dcdc3e2dec015466c3",
          "transactionIndex": "0x0",
          "blockHash": "0x78bc1a838d8e69c3f980d3c69ad79bf9b1dbc934ca9f9cb572408fac3259d161",
          "blockNumber": "0x45426",
          "gasUsed": "0xb1d6",
          "effectiveGasPrice": "0x47868c08",
          "from": "0xa525fcb70404336c591833a85abb1663b779cddf",
          "to": "0x7f81bfb73cb29c3725779ec2c060787daa2f97d4",
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
          "hash": "0x78bc1a838d8e69c3f980d3c69ad79bf9b1dbc934ca9f9cb572408fac3259d161",
          "parentHash": "0x2afaa2d34fb53b08aa150f9e8b0f707753ed7778d0f8ee6b22601d8a6fadcd01",
          "sha3Uncles": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
          "miner": "0x8cb33ecc40c31b79ae414a3d958b1b094b8993ce",
          "stateRoot": "0x6ffe7d5fe11439a683a9de6a59b9d9b2576772298d4c077ab0b262c311aa0139",
          "transactionsRoot": "0xed97db66cd791a681eb8ef982285003fa51aa9bd1eba11df2a616b0b2d479a8c",
          "receiptsRoot": "0xb2c672dbd3f5c41fe156dddcca90b1ebc847da3a5f338bc66b697f3a1151f582",
          "logsBloom": "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000002000000001200000000000000000000000000000000001000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000002000",
          "difficulty": "0x0",
          "number": "0x45426",
          "gasLimit": "0x1c9c380",
          "gasUsed": "0xb1d6",
          "timestamp": "0x68e6c67d",
          "extraData": "0x",
          "mixHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
          "nonce": "0x0000000000000000",
          "baseFeePerGas": "0x7",
          "withdrawalsRoot": "0x7e36212b14efdeab5312650a1c64bfd672db373e97d00d4aa9e6ec1397d94b82",
          "blobGasUsed": "0x0",
          "excessBlobGas": "0x0",
          "parentBeaconBlockRoot": "0xb2613c38880572d2467803b8a236eee297f20a1ec0bb20b6ee3ba69f85aa168c",
          "requestsHash": "0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          "size": "0x2f6",
          "uncles": [],
          "transactions": [
            {
              "type": "0x0",
              "chainId": "0x1720",
              "nonce": "0x1b",
              "gasPrice": "0x47868c08",
              "gas": "0x10cfd",
              "to": "0x7f81bfb73cb29c3725779ec2c060787daa2f97d4",
              "value": "0x0",
              "input": "0xef5fb05b",
              "r": "0xb51a0cefb88f0a0ab6bc82720a1da7c9ba05116e76ac519740736a1f225524e9",
              "s": "0x1447924901426ae0a0df648f700eb7f4760cd3b572360c6d1981077248eeeda6",
              "v": "0x2e63",
              "hash": "0x807bd8e1999cc975bd14826033e998c92d1eb604342ae0dcdc3e2dec015466c3",
              "blockHash": "0x78bc1a838d8e69c3f980d3c69ad79bf9b1dbc934ca9f9cb572408fac3259d161",
              "blockNumber": "0x45426",
              "transactionIndex": "0x0",
              "from": "0xa525fcb70404336c591833a85abb1663b779cddf"
            }
          ],
          "withdrawals": [
            {
              "index": "0x45426",
              "validatorIndex": "0x0",
              "address": "0x8cb33ecc40c31b79ae414a3d958b1b094b8993ce",
              "amount": "0xde53c1b"
            }
          ]
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
          "featureFlags": 0,
          "creationTime": 1759954586876631,
          "parent": "smE8OIgFctJGeAO4ojbu4pfyCh7AuyC27jumn4WqFow",
          "adjacents": {
            "41": "bRgmeumz8u8L1V_HYKgL-rwUNSqKlXB-ap_7ssmpptI",
            "69": "ZacCSYKvKIUoVcRLD8Y_J0SLjyHBL9lotdeWn3kFFJ4",
            "79": "sefhr1YuvwtsnqOoCftjW28lytACU_5I0UmHH2EK_rM",
            "86": "taX_Ekq_vacCT4G6AR6oMo0VRVt2RgAJ2sPmUiMtYDY"
          },
          "target": "BgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
          "payloadHash": "JUXtOVmUmUCCR8O0F4qNzLdp8WU7Xr4CGpLte70Ef9E",
          "chainId": 20,
          "weight": "ziBRVAzQZq_oNsjiWK0LSFSwS3WbffvI28fVKKlcqFY",
          "height": 283686,
          "chainwebVersion": "evm-testnet",
          "epochStart": 1759954428697267,
          "nonce": "0x0000000000000000",
          "hash": "aV9bLh5Pfb-ZnM-jFlsarg6kNZ31HFL-0hjlV1BC4DQ"
        }
    |]

    -- FIXME: this is actually an evm-testnet header


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
