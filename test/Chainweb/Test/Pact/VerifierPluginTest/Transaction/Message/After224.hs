{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact.VerifierPluginTest.Transaction.Message.After224 (tests) where

import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Default
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.PactValue
import Pact.Types.Term
import Pact.Types.Runtime
import Pact.Types.Verifier hiding (verifierName)

import Chainweb.Miner.Pact
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.Types
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.VerifierPlugin.Hyperlane.Binary
import Chainweb.VerifierPlugin.Hyperlane.Utils

import Chainweb.Test.Pact.VerifierPluginTest.Transaction.Utils

tests :: TestTree
tests = testGroup "After224"
  [ test generousConfig getGasModel "verifySuccess" hyperlaneVerifySuccess
  , test generousConfig getGasModel "verifyEmptyRecoveredSignaturesSuccess" hyperlaneVerifyEmptyRecoveredSignaturesSuccess
  , test generousConfig getGasModel "verifyWrongSignersFailure" hyperlaneVerifyWrongSignersFailure
  , test generousConfig getGasModel "verifyNotEnoughRecoveredSignaturesFailure" hyperlaneVerifyNotEnoughRecoveredSignaturesFailure
  , test generousConfig getGasModel "verifyNotEnoughCapabilitySignaturesFailure" hyperlaneVerifyNotEnoughCapabilitySignaturesFailure
  , test generousConfig getGasModel "verifyIncorretProofFailure" hyperlaneVerifyMerkleIncorrectProofFailure
  ]
  where
    -- This is way more than what is used in production, but during testing
    -- we can be generous.
    generousConfig = testPactServiceConfig { _pactBlockGasLimit = 300_000 }

    test pactConfig gasmodel tname f =
      withDelegateMempool $ \dmpio -> testCaseSteps tname $ \step ->
        withTestBlockDb testVersion $ \bdb -> do
          (iompa,mpa) <- dmpio
          let logger = hunitDummyLogger step
          withWebPactExecutionService logger testVersion pactConfig bdb mpa gasmodel $ \(pact,_) ->
            runReaderT f $
            SingleEnv bdb pact (return iompa) noMiner cid

-- hyperlane message tests

-- | Hyperlane test message TokenMessageERC20 encoded in base64:
--
--  { "amount": 0.000000000000000123
--  , "chainId": "4"
--  , "recipient": { "test-keys" : {"pred": "keys-all", "keys": ["da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6"]} }
--  }
--
hyperlaneTokenMessageBase64 :: T.Text
hyperlaneTokenMessageBase64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAewAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGF7InByZWQiOiAia2V5cy1hbGwiLCAia2V5cyI6WyJkYTFhMzM5YmQ4MmQyYzJlOTE4MDYyNmEwMGRjMDQzMjc1ZGViM2FiYWJiMjdiNTczOGFiZjZiOWRjZWU4ZGI2Il19AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

-- | Hyperlane test message encoded in base64
hyperlaneMessageBase64 :: T.Text
hyperlaneMessageBase64 = encodeB64UrlNoPaddingText $ runPutS $ putHyperlaneMessage $
  HyperlaneMessage
    { hmVersion = 3
    , hmNonce = 0
    , hmOriginDomain = 31337
    , hmSender = decodeHexUnsafe "0x7fa9385be102ac3eac297483dd6233d62b3e1496"
    , hmDestinationDomain = 626
    , hmRecipient = "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    , hmMessageBody = either (error . show) id $ decodeB64UrlNoPaddingText hyperlaneTokenMessageBase64
    }

-- | Hyperlane test MerkleTree Metadata encoded in base64
-- Test data was generated using the following test in the hyperlane codebase
-- https://github.com/hyperlane-xyz/hyperlane-monorepo/blob/b14f997810ebd7dbdff2ac6622a149ae77010ae3/solidity/test/isms/MultisigIsm.t.sol#L35
mkHyperlaneMerkleTreeMetadataBase64 :: B.ByteString -> [B.ByteString] -> T.Text
mkHyperlaneMerkleTreeMetadataBase64 proof signatures = encodeB64UrlNoPaddingText $ runPutS $ putMerkleRootMultisigIsmMetadata $
  MerkleRootMultisigIsmMetadata
    { mrmimOriginMerkleTreeAddress = decodeHexUnsafe "0x2e234dae75c793f67a35089c9d99245e1c58470b"
    , mrmimMessageIdIndex = 0
    , mrmimSignedCheckpointMessageId = decodeHexUnsafe "0x6f370c453c86ad681e936741683cceca8f13c46f2a49b1c9f8c6a23b5bb97aae"
    , mrmimMerkleProof = proof
    , mrmimSignedCheckpointIndex = 0
    , mrmimSignatures = signatures
    }

hyperlaneMerkleTreeCorrectProof :: B.ByteString
hyperlaneMerkleTreeCorrectProof = decodeHexUnsafe "0x0000000000000000000000000000000000000000000000000000000000000000ad3228b676f7d3cd4284a5443f17f1962b36e491b30a40b2405849e597ba5fb5b4c11951957c6f8f642c4af61cd6b24640fec6dc7fc607ee8206a99e92410d3021ddb9a356815c3fac1026b6dec5df3124afbadb485c9ba5a3e3398a04b7ba85e58769b32a1beaf1ea27375a44095a0d1fb664ce2dd358e7fcbfb78c26a193440eb01ebfc9ed27500cd4dfc979272d1f0913cc9f66540d7e8005811109e1cf2d887c22bd8750d34016ac3c66b5ff102dacdd73f6b014e710b51e8022af9a1968ffd70157e48063fc33c97a050f7f640233bf646cc98d9524c6b92bcf3ab56f839867cc5f7f196b93bae1e27e6320742445d290f2263827498b54fec539f756afcefad4e508c098b9a7e1d8feb19955fb02ba9675585078710969d3440f5054e0f9dc3e7fe016e050eff260334f18a5d4fe391d82092319f5964f2e2eb7c1c3a5f8b13a49e282f609c317a833fb8d976d11517c571d1221a265d25af778ecf8923490c6ceeb450aecdc82e28293031d10c7d73bf85e57bf041a97360aa2c5d99cc1df82d9c4b87413eae2ef048f94b4d3554cea73d92b0f7af96e0271c691e2bb5c67add7c6caf302256adedf7ab114da0acfe870d449a3a489f781d659e8beccda7bce9f4e8618b6bd2f4132ce798cdc7a60e7e1460a7299e3c6342a579626d22733e50f526ec2fa19a22b31e8ed50f23cd1fdf94c9154ed3a7609a2f1ff981fe1d3b5c807b281e4683cc6d6315cf95b9ade8641defcb32372f1c126e398ef7a5a2dce0a8a7f68bb74560f8f71837c2c2ebbcbf7fffb42ae1896f13f7c7479a0b46a28b6f55540f89444f63de0378e3d121be09e06cc9ded1c20e65876d36aa0c65e9645644786b620e2dd2ad648ddfcbf4a7e5b1a3a4ecfe7f64667a3f0b7e2f4418588ed35a2458cffeb39b93d26f18d2ab13bdce6aee58e7b99359ec2dfd95a9c16dc00d6ef18b7933a6f8dc65ccb55667138776f7dea101070dc8796e3774df84f40ae0c8229d0d6069e5c8f39a7c299677a09d367fc7b05e3bc380ee652cdc72595f74c7b1043d0e1ffbab734648c838dfb0527d971b602bc216c9619ef0abf5ac974a1ed57f4050aa510dd9c74f508277b39d7973bb2dfccc5eeb0618db8cd74046ff337f0a7bf2c8e03e10f642c1886798d71806ab1e888d9e5ee87d0838c5655cb21c6cb83313b5a631175dff4963772cce9108188b34ac87c81c41e662ee4dd2dd7b2bc707961b1e646c4047669dcb6584f0d8d770daf5d7e7deb2e388ab20e2573d171a88108e79d820e98f26c0b84aa8b2f4aa4968dbb818ea32293237c50ba75ee485f4c22adf2f741400bdf8d6a9cc7df7ecae576221665d7358448818bb4ae4562849e949e17ac16e0be16688e156b5cf15e098c627c0056a9"

hyperlaneMerkleTreeIncorrectProof :: B.ByteString
hyperlaneMerkleTreeIncorrectProof
  = B.take 31 hyperlaneMerkleTreeCorrectProof
  <> "\x19"
  <> B.drop 32 hyperlaneMerkleTreeCorrectProof

-- | Deploys a contract with a valid signer
deployContractWithValidSigner :: PactTxTest
deployContractWithValidSigner =
  PactTxTest
    (buildBasicGas 70000
    $ mkExec' $ mconcat
      [ "(namespace 'free)"
      , "(module m G"
      , "(defschema hyperlane_message"
      , "    version:integer"
      , "    nonce:integer"
      , "    originDomain:integer"
      , "    destinationDomain:integer"
      , "    sender:string"
      , "    recipient:string"
      , "    messageBody:string"
      , ")"
      , "(defcap G () true)"
      , "(defcap K (messageId:string message:object{hyperlane_message} signers:[string])"
      , "  (enforce-verifier 'hyperlane_v3_message)"
      , "  (enforce (= messageId \"0x6f370c453c86ad681e936741683cceca8f13c46f2a49b1c9f8c6a23b5bb97aae\") \"invalid messageId\")"
      , "  (enforce (= signers [\"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"]) \"invalid signers\")"
      , "  (enforce (= (at \"recipient\" message) \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\") \"invalid recipient\")"
      , "  (bind (hyperlane-decode-token-message (at \"messageBody\" message)) "
      , "    { \"amount\" := amount, "
      , "      \"chainId\" := chain-id, "
      , "      \"recipient\" := recipient-guard }"
      , "    (enforce (= amount 0.000000000000000123) \"invalid amount\")"
      , "    (enforce (= (create-principal recipient-guard) \"k:da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6\") \"invalid recipient guard\")"
      , "  )"
      , ")"
      , "(defun x () (with-capability (K "
      , "\"0x6f370c453c86ad681e936741683cceca8f13c46f2a49b1c9f8c6a23b5bb97aae\""
      , " {"
      , "  \"version\": 3,"
      , "  \"nonce\": 0,"
      , "  \"originDomain\": 31337,"
      , "  \"sender\": \"0x7fa9385be102ac3eac297483dd6233d62b3e1496\","
      , "  \"destinationDomain\": 626,"
      , "  \"recipient\": \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\","
      , "  \"messageBody\": \"" <> hyperlaneTokenMessageBase64 <> "\""
      , "}"
      , " [\"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"]"
      , ")"
      , " \"succeeded\")))"
      ])
    (assertTxSuccess
      "Should deploy module"
      (pString "Loaded module free.m, hash lJnbz74rDJQFwjIeA2ZdK0WmXJTScxGUGem-nre4TV4"))

-- | Deploys a contract with an invalid signer
deployContractWithInvalidSigner :: PactTxTest
deployContractWithInvalidSigner =
  PactTxTest
    (buildBasicGas 70000
    $ mkExec' $ mconcat
      [ "(namespace 'free)"
      , "(module m G"
      , "(defschema hyperlane_message"
      , "    version:integer"
      , "    nonce:integer"
      , "    originDomain:integer"
      , "    destinationDomain:integer"
      , "    sender:string"
      , "    recipient:string"
      , "    messageBody:string"
      , ")"
      , "(defcap G () true)"
      , "(defcap K (messageId:string message:object{hyperlane_message} signers:[string])"
      , "  (enforce-verifier 'hyperlane_v3_message)"
      , "  (enforce (= messageId \"0x6f370c453c86ad681e936741683cceca8f13c46f2a49b1c9f8c6a23b5bb97aae\") \"invalid messageId\")"
      , "  (enforce (= signers [\"wrongValidator\"]) \"invalid signers\")"
      , "  (enforce (= (at \"recipient\" message) \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\") \"invalid recipient\")"
      , "  (bind (hyperlane-decode-token-message (at \"messageBody\" message)) "
      , "    { \"amount\" := amount, "
      , "      \"chainId\" := chain-id, "
      , "      \"recipient\" := recipient-guard }"
      , "    (enforce (= amount 0.000000000000000123) \"invalid amount\")"
      , "    (enforce (= (create-principal recipient-guard) \"k:da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6\") \"invalid recipient guard\")"
      , "  )"
      , ")"
      , "(defun x () (with-capability (K "
      , "\"0x6f370c453c86ad681e936741683cceca8f13c46f2a49b1c9f8c6a23b5bb97aae\""
      , " {"
      , "  \"version\": 3,"
      , "  \"nonce\": 0,"
      , "  \"originDomain\": 31337,"
      , "  \"sender\": \"0x7fa9385be102ac3eac297483dd6233d62b3e1496\","
      , "  \"destinationDomain\": 626,"
      , "  \"recipient\": \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\","
      , "  \"messageBody\": \"" <> hyperlaneTokenMessageBase64 <> "\""
      , "}"
      , " []"
      , ")"
      , " \"succeeded\")))"
      ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash Yq0Z121-F0lVinDfjCtIwUZLZjz0ffIShYZfsO75WFk"))

-- | Calls '(free.m.x)' from 'deployContractWithValidSigner'
mkMerkleMetadataCallWithGas :: GasLimit -> B.ByteString -> [T.Text] -> [T.Text] -> MempoolCmdBuilder
mkMerkleMetadataCallWithGas gas merkleProof signatures signersText = buildBasic'
  (set cbGasLimit gas . set cbVerifiers
    [Verifier
      (VerifierName "hyperlane_v3_message")
      (ParsedVerifierProof $
        PList $ V.fromList
          [ pString hyperlaneMessageBase64

          , pString $ mkHyperlaneMerkleTreeMetadataBase64 merkleProof $ map decodeHexUnsafe signatures
          ]
        )
      [cap]])
      (mkExec' "(free.m.x)")
  where
    messageId = pString "0x6f370c453c86ad681e936741683cceca8f13c46f2a49b1c9f8c6a23b5bb97aae"
    message = PObject . ObjectMap . M.fromList $
      [ ("version", PLiteral $ LInteger 3)
      , ("nonce", PLiteral $ LInteger 0)
      , ("originDomain", PLiteral $ LInteger 31337)
      , ("destinationDomain", PLiteral $ LInteger 626)
      , ("sender", pString "0x7fa9385be102ac3eac297483dd6233d62b3e1496")
      , ("recipient", pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV")
      , ("messageBody", pString hyperlaneTokenMessageBase64)
      ]

    signers = PList $ V.fromList $ map pString signersText
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
            [messageId, message, signers]

mkMerkleMetadataCall :: B.ByteString -> [T.Text] -> [T.Text] -> MempoolCmdBuilder
mkMerkleMetadataCall = mkMerkleMetadataCallWithGas 20000

hyperlaneVerifySuccess :: PactTestM ()
hyperlaneVerifySuccess = do
  runToHeight 127

  runBlockTest
    [ deployContractWithValidSigner
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall
        hyperlaneMerkleTreeCorrectProof
        ["0xb3df841e9e3036f0858b0376280ef6692be293b53da3fba3384c82d6ca86704619cd7700147e3439fb66a985bdb310a4273204a0b5e5337deec0f00dfc8a5a171c"]
        ["0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e"]
        )
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pString "succeeded") cr
        assertEqual "gas should have been charged" 16578 (_crGas cr))
    ]

hyperlaneVerifyEmptyRecoveredSignaturesSuccess :: PactTestM ()
hyperlaneVerifyEmptyRecoveredSignaturesSuccess = do
  runToHeight 127

  runBlockTest
    [ PactTxTest
    (buildBasicGas 70000
    $ mkExec' $ mconcat
      [ "(namespace 'free)"
      , "(module m G"
      , "(defcap G () true)"
      , "(defcap K (messageId:string message signers:[string])"
      , "  (enforce-verifier 'hyperlane_v3_message)"
      , "  (enforce (= signers []) \"invalid signers\")"
      , ")"
      , "(defun x () (with-capability (K "
      , "\"0x6f370c453c86ad681e936741683cceca8f13c46f2a49b1c9f8c6a23b5bb97aae\""
      , " {"
      , "  \"version\": 3,"
      , "  \"nonce\": 0,"
      , "  \"originDomain\": 31337,"
      , "  \"sender\": \"0x7fa9385be102ac3eac297483dd6233d62b3e1496\","
      , "  \"destinationDomain\": 626,"
      , "  \"recipient\": \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\","
      , "  \"messageBody\": \"" <> hyperlaneTokenMessageBase64 <> "\""
      , "}"
      , " []"
      , ")"
      , " \"succeeded\")))"
      ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash dOUmd1arhUC2DGhDP1dLGad_JBXpLsvMhs98wP_ovJo"))
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [] [])
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pString "succeeded") cr
        assertEqual "gas should have been charged" 299 (_crGas cr))
    ]

hyperlaneVerifyWrongSignersFailure :: PactTestM ()
hyperlaneVerifyWrongSignersFailure = do
  runToHeight 127

  runBlockTest
    [ deployContractWithInvalidSigner
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall
        hyperlaneMerkleTreeCorrectProof
        ["0xb3df841e9e3036f0858b0376280ef6692be293b53da3fba3384c82d6ca86704619cd7700147e3439fb66a985bdb310a4273204a0b5e5337deec0f00dfc8a5a171c"]
        ["wrongValidator"])
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"})] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyNotEnoughRecoveredSignaturesFailure :: PactTestM ()
hyperlaneVerifyNotEnoughRecoveredSignaturesFailure = do
  runToHeight 127

  runBlockTest
    [ deployContractWithInvalidSigner
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall
        hyperlaneMerkleTreeCorrectProof [] ["wrongValidator"])
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyNotEnoughCapabilitySignaturesFailure :: PactTestM ()
hyperlaneVerifyNotEnoughCapabilitySignaturesFailure = do
  runToHeight 127

  runBlockTest
    [ deployContractWithValidSigner
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCallWithGas 40000
        hyperlaneMerkleTreeCorrectProof
        [ "0xb3df841e9e3036f0858b0376280ef6692be293b53da3fba3384c82d6ca86704619cd7700147e3439fb66a985bdb310a4273204a0b5e5337deec0f00dfc8a5a171c"
        , "0xb3df841e9e3036f0858b0376280ef6692be293b53da3fba3384c82d6ca86704619cd7700147e3439fb66a985bdb310a4273204a0b5e5337deec0f00dfc8a5a171c"
        ]
        ["0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e"]
        )
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"}),PLiteral (LString {_lString = \"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"})] but got PList [PLiteral (LString {_lString = \"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 40000 (_crGas cr))
    ]

hyperlaneVerifyMerkleIncorrectProofFailure :: PactTestM ()
hyperlaneVerifyMerkleIncorrectProofFailure = do
  runToHeight 127

  runBlockTest
    [ deployContractWithValidSigner
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall
        hyperlaneMerkleTreeIncorrectProof
        ["0xb3df841e9e3036f0858b0376280ef6692be293b53da3fba3384c82d6ca86704619cd7700147e3439fb66a985bdb310a4273204a0b5e5337deec0f00dfc8a5a171c"]
        ["0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e"]
      )
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x6d49eb3534b546856da70706a745038e4f0fd88a\"})] but got PList [PLiteral (LString {_lString = \"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]
