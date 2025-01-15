{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact4.VerifierPluginTest.Transaction.Message.Before225 (tests) where

import Control.Lens hiding ((.=))
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Info (noInfo)
import Pact.Types.PactValue
import Pact.Types.Term
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
import Data.IORef
import Chainweb.Version

tests :: RocksDb -> TestTree
tests rdb = testGroup "Before225"
  [ testGroup "MessageId metadata tests"
    [ test generousConfig "verifySuccess" hyperlaneVerifyMessageIdSuccess
    , test generousConfig "verifyEmptyRecoveredSignaturesSuccess" hyperlaneVerifyMessageIdEmptyRecoveredSignaturesSuccess
    , test generousConfig "verifyWrongSignersFailure" hyperlaneVerifyMessageIdWrongSignersFailure
    , test generousConfig "verifyNotEnoughRecoveredSignaturesFailure" hyperlaneVerifyMessageIdNotEnoughRecoveredSignaturesFailure
    , test generousConfig "verifyNotEnoughCapabilitySignaturesFailure" hyperlaneVerifyMessageIdNotEnoughCapabilitySignaturesFailure
    ]

  , testGroup "MerkleTree metadata tests"
    [ test generousConfig "verifyNotEnabledFailure" hyperlaneVerifyMerkleNotEnabledFailure
    ]
  ]
  where
    -- This is way more than what is used in production, but during testing
    -- we can be generous.
    generousConfig = testPactServiceConfig { _pactNewBlockGasLimit = 300_000 }

    test pactConfig tname f =
      withResourceT (mkTestBlockDb testVersion rdb) $ \bdbIO ->
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
    , hmSender = padLeft $ decodeHexUnsafe "0x7fa9385be102ac3eac297483dd6233d62b3e1496"
    , hmDestinationDomain = 626
    , hmRecipient = padLeft $ "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    , hmMessageBody = either (error . show) id $ decodeB64UrlNoPaddingText hyperlaneTokenMessageBase64
    }


-- =========================================================
--
-- MessageId metadata tests
--
-- =========================================================

-- | Hyperlane test MessageId Metadata encoded in base64
mkHyperlaneMessageIdMetadataBase64 :: [B.ByteString] -> T.Text
mkHyperlaneMessageIdMetadataBase64 signatures = encodeB64UrlNoPaddingText $ runPutS $ putMessageIdMultisigIsmMetadata $
  MessageIdMultisigIsmMetadata
    { mmimOriginMerkleTreeAddress = decodeHexUnsafe "0x2e234dae75c793f67a35089c9d99245e1c58470b"
    , mmimSignedCheckpointRoot = decodeHexUnsafe "0x6d1257af3b899a1ffd71849d9f5534753accbe25f85983aac343807a9184bd10"
    , mmimSignedCheckpointIndex = 0
    , mmimSignatures = signatures
    }

hyperlaneVerifyMessageIdSuccess :: PactTestM ()
hyperlaneVerifyMessageIdSuccess = do
  runToHeight 127
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0x909de0a3579891470db562c178905bf17a0e2cc1" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" noInfo)
            [pString hyperlaneTokenMessageBase64, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (messageBody:string recipient:string signers:[string])"
        , "  (enforce-verifier '" <> verifierName <> ")"
        , "  (enforce (= signers [\"0x909de0a3579891470db562c178905bf17a0e2cc1\"]) \"invalid signers\")"
        , "  (enforce (= recipient \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\") \"invalid recipient\")"
        , "  (bind (hyperlane-decode-token-message \"" <> hyperlaneTokenMessageBase64 <> "\") "
        , "    { \"amount\" := amount, "
        , "      \"chainId\" := chain-id, "
        , "      \"recipient\" := recipient-guard }"
        , "    (enforce (= amount 0.000000000000000123) \"invalid amount\")"
        , "    (enforce (= (create-principal recipient-guard) \"k:da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6\") \"invalid recipient guard\")"
        , "  )"
        , ")"
        , "(defun x () (with-capability (K "
        , "\"" <> hyperlaneTokenMessageBase64 <> "\""
        , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\""
        , " [\"0x909de0a3579891470db562c178905bf17a0e2cc1\"]"
        , ")"
        , " \"succeeded\")))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash CbPVttnlGVDHCi2MFqsbkVjNgscz5SwMInZUA5lmJCk"))
    , checkVerifierNotInTx verifierName
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName verifierName)
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString hyperlaneMessageBase64

                -- metadata with one valid signature
                , pString $ mkHyperlaneMessageIdMetadataBase64
                  [ decodeHexUnsafe "0x60ab9a1a8c880698ad56cc32210ba75f3f73599afca28e85e3935d9c3252c7f353fec4452218367116ae5cb0df978a21b39a4701887651fff1d6058d629521641c"]
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pString "succeeded") cr
        assertEqual "gas should have been charged" 16524 (_crGas cr))
    ]


hyperlaneVerifyMessageIdEmptyRecoveredSignaturesSuccess :: PactTestM ()
hyperlaneVerifyMessageIdEmptyRecoveredSignaturesSuccess = do
  runToHeight 127
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList []
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" noInfo)
            [pString hyperlaneTokenMessageBase64, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (messageBody:string recipient:string signers:[string]) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K"
        , " \"" <> hyperlaneTokenMessageBase64 <> "\""
        , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" []) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash qimic9pNaQ9A-hdJEQz8XQx91sPXlrGrn22Jg7GrkGg"))
    , checkVerifierNotInTx verifierName
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName verifierName)
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString hyperlaneMessageBase64

                -- metadata without signatures
                , pString $ mkHyperlaneMessageIdMetadataBase64 []
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        assertEqual "gas should have been charged" 249 (_crGas cr))
    ]


hyperlaneVerifyMessageIdWrongSignersFailure :: PactTestM ()
hyperlaneVerifyMessageIdWrongSignersFailure = do
  runToHeight 127
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    -- incorrect validator
    signers = PList $ V.fromList [ pString "wrongValidator" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" noInfo)
            [pString hyperlaneTokenMessageBase64, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (messageBody:string recipient:string signers:[string]) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K"
        , " \"" <> hyperlaneTokenMessageBase64 <> "\""
        , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"wrongValidator\"]) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash 8-eX_a7n5D_ETZ7FAW6p_WGTHjDK4DC_RD_gbQl7AVY"))
    , checkVerifierNotInTx verifierName
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName verifierName)
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString hyperlaneMessageBase64

                -- metadata with one valid signature
                , pString $ mkHyperlaneMessageIdMetadataBase64
                  [ decodeHexUnsafe "0x60ab9a1a8c880698ad56cc32210ba75f3f73599afca28e85e3935d9c3252c7f353fec4452218367116ae5cb0df978a21b39a4701887651fff1d6058d629521641c"]
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x909de0a3579891470db562c178905bf17a0e2cc1\"})] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyMessageIdNotEnoughRecoveredSignaturesFailure :: PactTestM ()
hyperlaneVerifyMessageIdNotEnoughRecoveredSignaturesFailure = do
  runToHeight 127
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "wrongValidator" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" noInfo)
            [pString hyperlaneTokenMessageBase64, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (messageBody:string recipient:string signers:[string]) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K"
        , " \"" <> hyperlaneTokenMessageBase64 <> "\""
        , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"wrongValidator\"]) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash 8-eX_a7n5D_ETZ7FAW6p_WGTHjDK4DC_RD_gbQl7AVY"))
    , checkVerifierNotInTx verifierName
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName verifierName)
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString hyperlaneMessageBase64

                -- metadata without signatures
                , pString $ mkHyperlaneMessageIdMetadataBase64 []
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyMessageIdNotEnoughCapabilitySignaturesFailure :: PactTestM ()
hyperlaneVerifyMessageIdNotEnoughCapabilitySignaturesFailure = do
  runToHeight 127
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0x909de0a3579891470db562c178905bf17a0e2cc1" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" noInfo)
            [pString hyperlaneTokenMessageBase64, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (messageBody:string recipient:string signers:[string]) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K"
        , " \"" <> hyperlaneTokenMessageBase64 <> "\""
        , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"0x909de0a3579891470db562c178905bf17a0e2cc1\"]) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash fpbofdp-l4YpUMhAcJGrGzu1H-l1Y1mA6JTjJi41ADk"))
    , checkVerifierNotInTx verifierName
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 40000 . set cbVerifiers
          [Verifier
            (VerifierName verifierName)
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString hyperlaneMessageBase64

                  -- metadata with one valid signature repeated twice
                , pString $ mkHyperlaneMessageIdMetadataBase64
                  [ decodeHexUnsafe "0x60ab9a1a8c880698ad56cc32210ba75f3f73599afca28e85e3935d9c3252c7f353fec4452218367116ae5cb0df978a21b39a4701887651fff1d6058d629521641c"
                  , decodeHexUnsafe "0x60ab9a1a8c880698ad56cc32210ba75f3f73599afca28e85e3935d9c3252c7f353fec4452218367116ae5cb0df978a21b39a4701887651fff1d6058d629521641c"
                  ]
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x909de0a3579891470db562c178905bf17a0e2cc1\"}),PLiteral (LString {_lString = \"0x909de0a3579891470db562c178905bf17a0e2cc1\"})] but got PList [PLiteral (LString {_lString = \"0x909de0a3579891470db562c178905bf17a0e2cc1\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 40000 (_crGas cr))
    ]

-- =========================================================
--
-- MerkleTree metadata tests
--
-- =========================================================

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

deployContractForMerkleTests :: PactTxTest
deployContractForMerkleTests =
  PactTxTest
    (buildBasicGas 70000
    $ mkExec' $ mconcat
      [ "(namespace 'free)"
      , "(module m G"
      , "(defcap G () true)"
      , "(defcap K (messageBody:string recipient:string signers:[string])"
      , "  (enforce-verifier 'hyperlane_v3_message)"
      , "  (enforce (= signers [\"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"]) \"invalid signers\")"
      , "  (enforce (= recipient \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\") \"invalid recipient\")"
      , "  (bind (hyperlane-decode-token-message \"" <> hyperlaneTokenMessageBase64 <> "\") "
      , "    { \"amount\" := amount, "
      , "      \"chainId\" := chain-id, "
      , "      \"recipient\" := recipient-guard }"
      , "    (enforce (= amount 0.000000000000000123) \"invalid amount\")"
      , "    (enforce (= (create-principal recipient-guard) \"k:da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6\") \"invalid recipient guard\")"
      , "  )"
      , ")"
      , "(defun x () (with-capability (K "
      , "\"" <> hyperlaneTokenMessageBase64 <> "\""
      , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\""
      , " [\"0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e\"]"
      , ")"
      , " \"succeeded\")))"
      ])
    (assertTxSuccess
      "Should deploy module"
      (pString "Loaded module free.m, hash kU7y3IbldFn1zeYRrrkT0BvLwjtBltyMsMR_E_SL5wU"))

-- | Calls '(free.m.x)' from 'deployContractForMerkleTests'
mkMerkleMetadatWithOneSignatureCall :: B.ByteString -> MempoolCmdBuilder
mkMerkleMetadatWithOneSignatureCall merkleProof = buildBasic'
  (set cbGasLimit 20000 . set cbVerifiers
    [Verifier
      (VerifierName "hyperlane_v3_message")
      (ParsedVerifierProof $
        PList $ V.fromList
          [ pString hyperlaneMessageBase64

          , pString $ mkHyperlaneMerkleTreeMetadataBase64 merkleProof
            [ decodeHexUnsafe "0xb3df841e9e3036f0858b0376280ef6692be293b53da3fba3384c82d6ca86704619cd7700147e3439fb66a985bdb310a4273204a0b5e5337deec0f00dfc8a5a171c" ]
          ]
        )
      [cap]])
      (mkExec' "(free.m.x)")
  where
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0x4bd34992e0994e9d3c53c1ccfe5c2e38d907338e" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" noInfo)
            [pString hyperlaneTokenMessageBase64, recipient, signers]

hyperlaneVerifyMerkleNotEnabledFailure :: PactTestM ()
hyperlaneVerifyMerkleNotEnabledFailure = do
  runToHeight 127

  runBlockTest
    [ deployContractForMerkleTests
    , checkVerifierNotInTx "hyperlane_v3_message"
    , PactTxTest (mkMerkleMetadatWithOneSignatureCall hyperlaneMerkleTreeCorrectProof)
      (\cr -> liftIO $ do
        assertTxFailure "should have failed with uncaught exception" "Tx verifier error: Uncaught exception in verifier" cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]
