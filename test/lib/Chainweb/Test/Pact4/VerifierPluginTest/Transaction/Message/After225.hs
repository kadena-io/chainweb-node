{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact4.VerifierPluginTest.Transaction.Message.After225 (tests) where

import Control.Lens hiding ((.=))
import Control.Monad.Reader
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
import Chainweb.Pact.Types
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.Utils
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.VerifierPlugin.Hyperlane.Binary
import Chainweb.VerifierPlugin.Hyperlane.Utils

import Chainweb.Test.Pact4.VerifierPluginTest.Transaction.Utils
import Chainweb.Version
import Data.IORef

tests :: RocksDb -> TestTree
tests rdb = testGroup "After225"
  [ test generousConfig "verifySuccess" hyperlaneVerifySuccess
  , test generousConfig "verifyMoreValidatorsSuccess" hyperlaneVerifyMoreValidatorsSuccess
  , test generousConfig "verifyThresholdZeroError" hyperlaneVerifyThresholdZeroError
  , test generousConfig "verifyWrongSignersFailure" hyperlaneVerifyWrongSignersFailure
  , test generousConfig "verifyNotEnoughRecoveredSignaturesFailure" hyperlaneVerifyNotEnoughRecoveredSignaturesFailure
  , test generousConfig "verifyNotEnoughCapabilitySignaturesFailure" hyperlaneVerifyNotEnoughCapabilitySignaturesFailure
  , test generousConfig "verifyIncorrectProofFailure" hyperlaneVerifyMerkleIncorrectProofFailure
  , test generousConfig "verifyFailureNotEnoughSignaturesToPassThreshold" hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold
  ]
  where
    -- This is way more than what is used in production, but during testing
    -- we can be generous.
    generousConfig = testPactServiceConfig { _pactNewBlockGasLimit = 300_000 }

    test pactConfig tname f = withResourceT (mkTestBlockDb testVersion rdb) $ \bdbIO ->
      testCaseSteps tname $ \step -> do
        bdb <- bdbIO
        let logger = hunitDummyLogger step
        mempools <- onAllChains testVersion $ \_ -> do
          mempoolRef <- newIORef mempty
          return (mempoolRef, delegateMemPoolAccess mempoolRef)
        withWebPactExecutionService logger testVersion pactConfig bdb (snd <$> mempools) $ \(pact,_) ->
          runReaderT f $
          SingleEnv bdb pact (mempools ^?! atChain cid . _1) noMiner cid

-- hyperlane message tests

-- | Hyperlane test message TokenMessageERC20 encoded in base64:
--
--  { "amount": 0.000000000000000123
--  , "chainId": "4"
--  , "recipient": { "test-keys" : {"pred": "keys-all", "keys": ["da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6"]} }
--  }
--
hyperlaneTokenMessageBase64 :: T.Text
hyperlaneTokenMessageBase64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHsABHsicHJlZCI6ICJrZXlzLWFsbCIsICJrZXlzIjpbImRhMWEzMzliZDgyZDJjMmU5MTgwNjI2YTAwZGMwNDMyNzVkZWIzYWJhYmIyN2I1NzM4YWJmNmI5ZGNlZThkYjYiXX0"

-- | Hyperlane test message encoded in base64
hyperlaneMessageBase64 :: T.Text
hyperlaneMessageBase64 = encodeB64UrlNoPaddingText $ runPutS $ putHyperlaneMessage $
  HyperlaneMessage
    { hmVersion = 3
    , hmNonce = 0
    , hmOriginDomain = 31337
    , hmSender = either (error . show) id $ decodeB64UrlNoPaddingText "AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY"
    , hmDestinationDomain = 626
    , hmRecipient = either (error . show) id $ decodeB64UrlNoPaddingText "AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU"
    , hmMessageBody = either (error . show) id $ decodeB64UrlNoPaddingText hyperlaneTokenMessageBase64
    }

hyperlaneMessageId :: T.Text
hyperlaneMessageId = encodeB64UrlNoPaddingText $ keccak256ByteString $ either (error . show) id $ decodeB64UrlNoPaddingText $ hyperlaneMessageBase64

-- | Hyperlane test MerkleTree Metadata encoded in base64
-- Test data was generated using the following test in the hyperlane codebase
-- https://github.com/hyperlane-xyz/hyperlane-monorepo/blob/b14f997810ebd7dbdff2ac6622a149ae77010ae3/solidity/test/isms/MultisigIsm.t.sol#L35
mkHyperlaneMerkleTreeMetadataBase64 :: B.ByteString -> [B.ByteString] -> T.Text
mkHyperlaneMerkleTreeMetadataBase64 proof signatures = encodeB64UrlNoPaddingText $ runPutS $ putMerkleRootMultisigIsmMetadata $
  MerkleRootMultisigIsmMetadata
    { mrmimOriginMerkleTreeAddress = decodeHexUnsafe "0x2e234dae75c793f67a35089c9d99245e1c58470b"
    , mrmimMessageIdIndex = 0
    , mrmimSignedCheckpointMessageId = either (error . show) id $ decodeB64UrlNoPaddingText hyperlaneMessageId
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

validSigner :: T.Text
validSigner = "0x2bd2e3ba4861fae19d87cd77f8557bbad8e92d23"

validSignature :: T.Text
validSignature = "0xfabe80dd5bf4440e5e7fbc3cdf12325df9c00beb1281c5ddf12e77177046790c49f531ccebb29ba9c9664a581ed1870873850e0cf0c231b779e21f48a1d0dcea1b"

-- | Deploys a contract with a valid signer
deployContractWith :: [T.Text] -> Integer -> T.Text -> PactTxTest
deployContractWith signers threshold moduleHash =
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
      , "(defcap K (messageId:string message:object{hyperlane_message} signers:[string] threshold:integer)"
      , "  (enforce-verifier 'hyperlane_v3_message)"
      , "  (enforce (= messageId \"" <> hyperlaneMessageId <> "\") \"invalid messageId\")"
      , "  (enforce (= messageId (hyperlane-message-id message)) \"invalid calculated messageId\")"
      , "  (enforce (= signers [" <> (T.intercalate "," $ map (\s -> "\"" <> s <> "\"") signers) <> "]) \"invalid signers\")"
      , "  (enforce (= (at \"sender\" message) \"AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY\") \"invalid sender\")"
      , "  (enforce (= (at \"recipient\" message) \"AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU\") \"invalid recipient\")"
      , "  (bind (hyperlane-decode-token-message (at \"messageBody\" message)) "
      , "    { \"amount\" := amount, "
      , "      \"chainId\" := chain-id, "
      , "      \"recipient\" := recipient-guard }"
      , "    (enforce (= amount 0.000000000000000123) \"invalid amount\")"
      , "    (enforce (= (create-principal recipient-guard) \"k:da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6\") \"invalid recipient guard\")"
      , "    (enforce (= (hyperlane-encode-token-message {\"amount\": 123.0, \"chainId\": chain-id, \"recipient\": \"eyJwcmVkIjogImtleXMtYWxsIiwgImtleXMiOlsiZGExYTMzOWJkODJkMmMyZTkxODA2MjZhMDBkYzA0MzI3NWRlYjNhYmFiYjI3YjU3MzhhYmY2YjlkY2VlOGRiNiJdfQ\""
      , "}) (at \"messageBody\" message)) \"invalid encoded message\")"
      , "  )"
      , ")"
      , "(defun x () (with-capability (K "
      , "\"" <> hyperlaneMessageId <> "\""
      , " {"
      , "  \"version\": 3,"
      , "  \"nonce\": 0,"
      , "  \"originDomain\": 31337,"
      , "  \"sender\": \"AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY\","
      , "  \"destinationDomain\": 626,"
      , "  \"recipient\": \"AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU\","
      , "  \"messageBody\": \"" <> hyperlaneTokenMessageBase64 <> "\""
      , "}"
      , " [" <> (T.intercalate "," $ map (\s -> "\"" <> s <> "\"") signers) <> "]"
      , " " <> sshow threshold
      , ")"
      , " \"succeeded\")))"
      ])
    (assertTxSuccess
      "Should deploy module"
      (pString $ "Loaded module free.m, hash " <> moduleHash))

-- | Calls '(free.m.x)' from 'deployContractWithValidSigner'
mkMerkleMetadataCallWithGas :: GasLimit -> B.ByteString -> [T.Text] -> [T.Text] -> Integer -> MempoolCmdBuilder
mkMerkleMetadataCallWithGas gas merkleProof signatures signersText threshold = buildBasic'
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
    messageId = pString hyperlaneMessageId
    message = PObject . ObjectMap . M.fromList $
      [ ("version", PLiteral $ LInteger 3)
      , ("nonce", PLiteral $ LInteger 0)
      , ("originDomain", PLiteral $ LInteger 31337)
      , ("destinationDomain", PLiteral $ LInteger 626)
      , ("sender", pString "AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY")
      , ("recipient", pString "AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU")
      , ("messageBody", pString hyperlaneTokenMessageBase64)
      ]

    signers = PList $ V.fromList $ map pString signersText
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" noInfo)
            [messageId, message, signers, pInteger threshold]

mkMerkleMetadataCall :: B.ByteString -> [T.Text] -> [T.Text] -> Integer -> MempoolCmdBuilder
mkMerkleMetadataCall = mkMerkleMetadataCallWithGas 20000

hyperlaneVerifySuccess :: PactTestM ()
hyperlaneVerifySuccess = do
  runToHeight 131

  let threshold = 1
  runBlockTest
    [ deployContractWith [validSigner] threshold "QpXqxeT4QKDK98PZaxQCexpxg8bRWDkF4fmB0IhoLR0"
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [validSignature] [validSigner] threshold)
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pString "succeeded") cr
        assertEqual "gas should have been charged" 16582 (_crGas cr))
    ]

hyperlaneVerifyMoreValidatorsSuccess :: PactTestM ()
hyperlaneVerifyMoreValidatorsSuccess = do
  runToHeight 131

  let threshold = 1
  let signers = ["wrongSigner", validSigner]
  runBlockTest
    [ deployContractWith signers threshold "8hkRC85XbTyMUBEbpwLNXP-KH7mVezY51uKnuznGAAo"
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [validSignature] signers threshold)
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pString "succeeded") cr
        assertEqual "gas should have been charged" 16583 (_crGas cr))
    ]

hyperlaneVerifyThresholdZeroError :: PactTestM ()
hyperlaneVerifyThresholdZeroError = do
  runToHeight 131

  runBlockTest
    [ PactTxTest
    (buildBasicGas 70000
    $ mkExec' $ mconcat
      [ "(namespace 'free)"
      , "(module m G"
      , "(defcap G () true)"
      , "(defcap K (messageId:string message signers:[string] threshold:integer)"
      , "  (enforce-verifier 'hyperlane_v3_message)"
      , "  (enforce (= signers []) \"invalid signers\")"
      , ")"
      , "(defun x () (with-capability (K "
      , "\"" <> hyperlaneMessageId <> "\""
      , " {"
      , "  \"version\": 3,"
      , "  \"nonce\": 0,"
      , "  \"originDomain\": 31337,"
      , "  \"sender\": \"AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY\","
      , "  \"destinationDomain\": 626,"
      , "  \"recipient\": \"AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU\","
      , "  \"messageBody\": \"" <> hyperlaneTokenMessageBase64 <> "\""
      , "}"
      , " [] 0"
      , ")"
      , " \"succeeded\")))"
      ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash dSzwI4Kg9RYSrEqzdqQ9-bhsqn3NrefVXvtPQF7RF80"))
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [] [] 0)
      (\cr -> liftIO $ do
        assertTxFailure "Verification should fail" "Tx verifier error: Threshold should be greater than 0" cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyWrongSignersFailure :: PactTestM ()
hyperlaneVerifyWrongSignersFailure = do
  runToHeight 131

  let threshold = 1
  runBlockTest
    [ deployContractWith ["wrongSigner"] threshold "LqQ1Y2N2UXxOnWRBaXQ9lNcit5dESFtetuB3oxQyNEg"
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [validSignature] ["wrongSigner"] threshold)
      (\cr -> liftIO $ do
        assertTxFailure "Verification should fail" "Tx verifier error: Verification failed" cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyNotEnoughRecoveredSignaturesFailure :: PactTestM ()
hyperlaneVerifyNotEnoughRecoveredSignaturesFailure = do
  runToHeight 131

  let threshold = 1
  runBlockTest
    [ deployContractWith ["wrongSigner"] threshold "LqQ1Y2N2UXxOnWRBaXQ9lNcit5dESFtetuB3oxQyNEg"
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall
        hyperlaneMerkleTreeCorrectProof [] ["wrongSigner"] threshold)
      (\cr -> liftIO $ do
        assertTxFailure "Verification should fail with not enough recovered addresses" "Tx verifier error: The number of signatures can't be less than threshold" cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyNotEnoughCapabilitySignaturesFailure :: PactTestM ()
hyperlaneVerifyNotEnoughCapabilitySignaturesFailure = do
  runToHeight 131

  let threshold = 2
  runBlockTest
    [ deployContractWith [validSigner] threshold "7z1p9sgWQ_RC-iaNSwQU8I7K1pFL2AOFtI-ZyxRdzkg"
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCallWithGas 40000 hyperlaneMerkleTreeCorrectProof [validSignature, validSignature] [validSigner] threshold)
      (\cr -> liftIO $ do
        assertTxFailure "Verification should fail" "Tx verifier error: Verification failed" cr
        assertEqual "gas should have been charged" 40000 (_crGas cr))
    ]

hyperlaneVerifyMerkleIncorrectProofFailure :: PactTestM ()
hyperlaneVerifyMerkleIncorrectProofFailure = do
  runToHeight 131

  let threshold = 1
  runBlockTest
    [ deployContractWith [validSigner] threshold "QpXqxeT4QKDK98PZaxQCexpxg8bRWDkF4fmB0IhoLR0"
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCall hyperlaneMerkleTreeIncorrectProof [validSignature] [validSigner] threshold)
      (\cr -> liftIO $ do
        assertTxFailure "Verification should fail" "Tx verifier error: Verification failed" cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

-- | We pass 2 signatures, 1st one matches to the correct validator,
-- but there is no second valid validator for the 2nd signature, and the verification fails.
hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold :: PactTestM ()
hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold = do
  runToHeight 131

  let threshold = 2
  let signers = ["wrongSigner", validSigner, "wrongSigner"]
  runBlockTest
    [ deployContractWith [validSigner] threshold "7z1p9sgWQ_RC-iaNSwQU8I7K1pFL2AOFtI-ZyxRdzkg"
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadataCallWithGas 40000 hyperlaneMerkleTreeCorrectProof [validSignature, validSignature] signers threshold)
      (\cr -> liftIO $ do
        assertTxFailure "Verification should fail" "Tx verifier error: Verification failed" cr
        assertEqual "gas should have been charged" 40000 (_crGas cr))
    ]
