{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact.VerifierPluginTest.Transaction
( tests
) where

import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Reader
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Hash
import qualified Pact.JSON.Encode as PactJSON
import Pact.Types.PactError
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.RPC
import Pact.Types.Term
import Pact.Types.Verifier hiding (verifierName)

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Chainweb.VerifierPlugin.Hyperlane.Binary
import Chainweb.VerifierPlugin.Hyperlane.Utils
import Chainweb.Payload.PayloadStore (lookupPayloadWithHeight)

testVersion :: ChainwebVersion
testVersion = slowForkingCpmTestVersion peterson

cid :: ChainId
cid = unsafeChainId 9
    -- several tests in this file expect chain 9

data MultiEnv = MultiEnv
    { _menvBdb :: !TestBlockDb
    , _menvPact :: !WebPactExecutionService
    , _menvMpa :: !(IO (IORef MemPoolAccess))
    , _menvMiner :: !Miner
    , _menvChainId :: !ChainId
    }

makeLenses ''MultiEnv

type PactTestM = ReaderT MultiEnv IO

data MempoolInput = MempoolInput
    { _miBlockFill :: BlockFill
    , _miBlockHeader :: BlockHeader
    }

newtype MempoolCmdBuilder = MempoolCmdBuilder
    { _mempoolCmdBuilder :: MempoolInput -> CmdBuilder
    }

-- | Block filler. A 'Nothing' result means "skip this filler".
newtype MempoolBlock = MempoolBlock
    { _mempoolBlock :: MempoolInput -> Maybe [MempoolCmdBuilder]
    }

-- | Mempool with an ordered list of fillers.
newtype PactMempool = PactMempool
  { _pactMempool :: [MempoolBlock]
  }
  deriving (Semigroup,Monoid)


-- | Pair a builder with a test
data PactTxTest = PactTxTest
    { _pttBuilder :: MempoolCmdBuilder
    , _pttTest :: CommandResult Hash -> Assertion
    }

tests :: TestTree
tests = testGroup testName
  [ test generousConfig getGasModel "verifierTest" verifierTest

  , test generousConfig getGasModel "recoverValidatorAnnouncementSuccess" hyperlaneRecoverValidatorAnnouncementSuccess
  , test generousConfig getGasModel "recoverValidatorAnnouncementIncorrectSignatureFailure"
    hyperlaneRecoverValidatorAnnouncementIncorrectSignatureFailure
  , test generousConfig getGasModel "recoverValidatorAnnouncementDifferentSignerFailure"
      hyperlaneRecoverValidatorAnnouncementDifferentSignerFailure

  , test generousConfig getGasModel "verifySuccess" hyperlaneVerifySuccess
  , test generousConfig getGasModel "verifyEmptyRecoveredSignaturesSuccess" hyperlaneVerifyEmptyRecoveredSignaturesSuccess

  , test generousConfig getGasModel "verifyWrongSignersFailure" hyperlaneVerifyWrongSignersFailure
  , test generousConfig getGasModel "verifyNotEnoughRecoveredSignaturesFailure" hyperlaneVerifyNotEnoughRecoveredSignaturesFailure
  , test generousConfig getGasModel "verifyNotEnoughCapabilitySignaturesFailure" hyperlaneVerifyNotEnoughCapabilitySignaturesFailure
  ]
  where
    testName = "Chainweb.Test.Pact.VerifierPluginTest.Transaction"
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
            MultiEnv bdb pact (return iompa) noMiner cid

verifierTest :: PactTestM ()
verifierTest = do
  runToHeight 118

  let cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "G" def) []

  runBlockTest
    [ PactTxTest
        (buildBasic (mkExec' "(enforce-verifier 'allow)"))
        (assertTxFailure "Should not resolve enforce-verifier" "Cannot resolve enforce-verifier")
    , PactTxTest
        (buildBasic'
          (set cbVerifiers
            [Verifier
              (VerifierName "missing")
              (ParsedVerifierProof $ pString "")
              [cap]])
          (mkExec' "1"))
        (assertTxSuccess
          "Should not run verifiers before they're enabled" (pDecimal 1))
    ]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () (enforce-verifier 'allow))"
        , "(defun x () (with-capability (G) 1)))"
        ]
      )
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash QNTlTCp-KMPkT52CEo_0zGaLJ_PnAxsenyhUck1njcc")
      )
    , PactTxTest
      (buildBasicGas 10000 (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure allow: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr
      )
    , PactTxTest
      (buildBasic'
        (set cbVerifiers
          [Verifier
            (VerifierName "allow")
            (ParsedVerifierProof $ pString (PactJSON.encodeText cap))
            [cap]
          ]
        )
        (mkExec' "(free.m.x)")
      )
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        -- The **Allow** verifier costs 100 gas flat
        assertEqual "gas should have been charged" 344 (_crGas cr)
      )
    , PactTxTest
      (buildBasic'
        (set cbVerifiers
          [Verifier
            (VerifierName "missing")
            (ParsedVerifierProof $ pString (PactJSON.encodeText cap))
            [cap]
          ]
        )
        (mkExec' "(free.m.x)")
      )
      (assertTxFailure
        "should have failed, missing verifier"
        "Tx verifier error: verifier does not exist: missing")
    ]

-- hyperlane validator announcement tests

hyperlaneRecoverValidatorAnnouncementSuccess :: PactTestM ()
hyperlaneRecoverValidatorAnnouncementSuccess = do
  runToHeight 119
  let verifierName = "hyperlane_v3_announcement"

  let cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
              [pString "storagelocation", pString "0x6c414e7a15088023e28af44ad0e1d593671e4b15", pString "kb-mailbox"]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (location:string signer:string mailbox:string) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K \"storagelocation\" \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\" \"kb-mailbox\") 1)))"])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash JbKuarvFOs3FGnpQ3R2exafA2gKonqa5Ls1-l3rIh8I"))
    , checkVerifierNotInTx verifierName
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName verifierName)
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "storagelocation"
                -- TODO: generate instead of using the precomputed value
                , pString "U7oftiGhn7rpWJydP6t0FKStdcRd223a8uSTqKjs8K8nJW7U84tzBOgPZTtGKnncwiu8l1185vB38c7-Ov7avBw"
                , pString "kb-mailbox"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        assertEqual "gas should have been charged" 16501 (_crGas cr))
    ]

hyperlaneRecoverValidatorAnnouncementIncorrectSignatureFailure :: PactTestM ()
hyperlaneRecoverValidatorAnnouncementIncorrectSignatureFailure = do
  runToHeight 119
  let verifierName = "hyperlane_v3_announcement"

  let cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
              [pString "storagelocation", pString "0x6c414e7a15088023e28af44ad0e1d593671e4b15", pString "kb-mailbox"]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (location:string signer:string mailbox:string) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K \"storagelocation\" \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\" \"kb-mailbox\") 1)))"])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash JbKuarvFOs3FGnpQ3R2exafA2gKonqa5Ls1-l3rIh8I"))
    , checkVerifierNotInTx verifierName
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName verifierName)
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "storagelocation"

                -- bad signature (same as from the previous test but the different first symbol)
                , pString "Q7oftiGhn7rpWJydP6t0FKStdcRd223a8uSTqKjs8K8nJW7U84tzBOgPZTtGKnncwiu8l1185vB38c7-Ov7avBw"

                , pString "kb-mailbox"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure "should have failed with incorrect signature" "Tx verifier error: Failed to recover the address from the signature" cr
        assertTxGas "verifier errors charge all gas" 20000 cr)
    ]

hyperlaneRecoverValidatorAnnouncementDifferentSignerFailure :: PactTestM ()
hyperlaneRecoverValidatorAnnouncementDifferentSignerFailure = do
  runToHeight 119
  let verifierName = "hyperlane_v3_announcement"

  let cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
              -- bad signer (same as from the previous test but the different first symbol)
              [pString "storagelocation", pString "0x5c414e7a15088023e28af44ad0e1d593671e4b15", pString "kb-mailbox"]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (location:string signer:string mailbox:string) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K \"storagelocation\" \"0x5c414e7a15088023e28af44ad0e1d593671e4b15\" \"kb-mailbox\") 1)))"])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash jZR2sS2FjdR-3udU9-DNL1RfsjKISk4AuhNm43yueOA"))
    , checkVerifierNotInTx verifierName
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName verifierName)
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "storagelocation"
                , pString "U7oftiGhn7rpWJydP6t0FKStdcRd223a8uSTqKjs8K8nJW7U84tzBOgPZTtGKnncwiu8l1185vB38c7-Ov7avBw"
                , pString "kb-mailbox"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Incorrect signer. Expected: PLiteral (LString {_lString = \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\"}) but got PLiteral (LString {_lString = \"0x5c414e7a15088023e28af44ad0e1d593671e4b15\"})"
        assertTxFailure "should have failed with incorrect signer" errMsg cr
        assertTxGas "verifier errors charge all gas" 20000 cr)
    ]


-- hyperlane message tests

-- | Hyperlane test message TokenMessageERC20 encoded in base64:
--
--  { tmRecipient = "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c"
--  , tmAmount = decimalToWord 249
--  , tmChainId = 0
--  }
--
-- TODO: replace with pact native
hyperlaneTokenMessageBase64 :: T.Text
hyperlaneTokenMessageBase64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA1_kbS90EQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJrOjk0YzM1YWIxYmQ3MDI0M2VjNjcwNDk1MDc3Zjc4NDYzNzNiNGRjNWU5Nzc5ZDdhNjczMmI1Y2ViNmZkZTA1OWMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

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

-- | Hyperlane test Metadata encoded in base64
mkHyperlaneMetadataBase64 :: [B.ByteString] -> T.Text
mkHyperlaneMetadataBase64 signatures = encodeB64UrlNoPaddingText $ runPutS $ putMessageIdMultisigIsmMetadata $
  MessageIdMultisigIsmMetadata
    { mmimOriginMerkleTreeAddress = decodeHexUnsafe "0x2e234dae75c793f67a35089c9d99245e1c58470b"
    , mmimSignedCheckpointRoot = decodeHexUnsafe "0x6d1257af3b899a1ffd71849d9f5534753accbe25f85983aac343807a9184bd10"
    , mmimSignedCheckpointIndex = 0
    , mmimSignatures = signatures
    }

hyperlaneVerifySuccess :: PactTestM ()
hyperlaneVerifySuccess = do
  runToHeight 119
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
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
        , "  (enforce (= signers [\"0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6\"]) \"invalid signers\")"
        , "  (enforce (= recipient \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\") \"invalid recipient\")"
        , ")"
        , "(defun x () (with-capability (K "
        , "\"" <> hyperlaneTokenMessageBase64 <> "\""
        , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\""
        , " [\"0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6\"]"
        , ")"
        , " \"succeeded\")))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash A7lPMvlHngX3fuN3qaZRvF2ih5wUBgjHyWWX9l39p-I"))
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
                , pString $ mkHyperlaneMetadataBase64
                  [ decodeHexUnsafe "0x60ab9a1a8c880698ad56cc32210ba75f3f73599afca28e85e3935d9c3252c7f353fec4452218367116ae5cb0df978a21b39a4701887651fff1d6058d629521641c"]
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pString "succeeded") cr
        assertEqual "gas should have been charged" 16520 (_crGas cr))
    ]


hyperlaneVerifyEmptyRecoveredSignaturesSuccess :: PactTestM ()
hyperlaneVerifyEmptyRecoveredSignaturesSuccess = do
  runToHeight 119
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList []
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
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
        (pString "Loaded module free.m, hash s-EsSFcP7MMfVIDXZ7F9Lw6yOeGaeawdfWWC0pxvIbA"))
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
                , pString $ mkHyperlaneMetadataBase64 []
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        assertEqual "gas should have been charged" 263 (_crGas cr))
    ]

hyperlaneVerifyWrongSignersFailure :: PactTestM ()
hyperlaneVerifyWrongSignersFailure = do
  runToHeight 119
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    -- incorrect validator
    signers = PList $ V.fromList [ pString "wrongValidator" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
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
        (pString "Loaded module free.m, hash GCYphpEUm2AqaTg5af31pdUu3Lre1K9BDTpN7Q74gq8"))
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
                , pString $ mkHyperlaneMetadataBase64
                  [ decodeHexUnsafe "0x60ab9a1a8c880698ad56cc32210ba75f3f73599afca28e85e3935d9c3252c7f353fec4452218367116ae5cb0df978a21b39a4701887651fff1d6058d629521641c"]
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6\"})] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyNotEnoughRecoveredSignaturesFailure :: PactTestM ()
hyperlaneVerifyNotEnoughRecoveredSignaturesFailure = do
  runToHeight 119
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "wrongValidator" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
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
        (pString "Loaded module free.m, hash GCYphpEUm2AqaTg5af31pdUu3Lre1K9BDTpN7Q74gq8"))
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
                , pString $ mkHyperlaneMetadataBase64 []
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyNotEnoughCapabilitySignaturesFailure :: PactTestM ()
hyperlaneVerifyNotEnoughCapabilitySignaturesFailure = do
  runToHeight 119
  let verifierName = "hyperlane_v3_message"

  let
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
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
        , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6\"]) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash q3ZufEV8GtQOabbLFWUmRbXIkIozl_ceslXPCKaYzKo"))
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
                , pString $ mkHyperlaneMetadataBase64
                  [ decodeHexUnsafe "0x60ab9a1a8c880698ad56cc32210ba75f3f73599afca28e85e3935d9c3252c7f353fec4452218367116ae5cb0df978a21b39a4701887651fff1d6058d629521641c"
                  , decodeHexUnsafe "0x60ab9a1a8c880698ad56cc32210ba75f3f73599afca28e85e3935d9c3252c7f353fec4452218367116ae5cb0df978a21b39a4701887651fff1d6058d629521641c"
                  ]
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6\"}),PLiteral (LString {_lString = \"0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6\"})] but got PList [PLiteral (LString {_lString = \"0xab36e79520d85f36fe5e2ca33c29cfe461eb48c6\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 40000 (_crGas cr))
    ]

-- =========================================================
--
-- Fixture
--
-- =========================================================

checkVerifierNotInTx :: T.Text -> PactTxTest
checkVerifierNotInTx v = PactTxTest
  (buildBasic (mkExec' "(free.m.x)"))
  (\cr -> liftIO $ do
    assertTxFailure
      "verifier not present"
      ("Verifier failure " <> pretty v <> ": not in transaction")
      cr
    assertTxGas "verifier errors charge all gas" 10000 cr)

-- | Sets mempool with block fillers. A matched filler
-- (returning a 'Just' result) is executed and removed from the list.
-- Fillers are tested in order.
setPactMempool :: PactMempool -> PactTestM ()
setPactMempool (PactMempool fs) = do
  mpa <- view menvMpa
  mpsRef <- liftIO $ newIORef fs
  setMempool mpa $ mempty {
    mpaGetBlock = go mpsRef
    }
  where
    go ref bf mempoolPreBlockCheck bHeight bHash blockHeader = do
      mps <- readIORef ref
      let mi = MempoolInput bf blockHeader
          runMps i = \case
            [] -> return mempty
            (mp:r) -> case _mempoolBlock mp mi of
              Just bs -> do
                writeIORef ref (take i mps ++ r)
                cmds <- fmap V.fromList $ forM bs $ \b ->
                  buildCwCmd (sshow blockHeader) testVersion $ _mempoolCmdBuilder b mi
                validationResults <- mempoolPreBlockCheck bHeight bHash cmds
                return $ fmap fst $ V.filter snd (V.zip cmds validationResults)
              Nothing -> runMps (succ i) r
      runMps 0 mps

filterBlock :: (MempoolInput -> Bool) -> MempoolBlock -> MempoolBlock
filterBlock f (MempoolBlock b) = MempoolBlock $ \mi ->
  if f mi then b mi else Nothing

blockForChain :: ChainId -> MempoolBlock -> MempoolBlock
blockForChain chid = filterBlock $ \(MempoolInput _ bh) ->
  _blockChainId bh == chid

runCut' :: PactTestM ()
runCut' = do
  pact <- view menvPact
  bdb <- view menvBdb
  miner <- view menvMiner
  liftIO $ runCut testVersion bdb pact (offsetBlockTime second) zeroNoncer miner

assertTxGas :: (HasCallStack, MonadIO m) => String -> Gas -> CommandResult Hash -> m ()
assertTxGas msg g = liftIO . assertEqual msg g . _crGas

assertTxSuccess
  :: HasCallStack
  => MonadIO m
  => String
  -> PactValue
  -> CommandResult Hash
  -> m ()
assertTxSuccess msg r tx = do
  liftIO $ assertEqual msg (Just r)
    (tx ^? crResult . to _pactResult . _Right)

-- | Exact match on error doc
assertTxFailure :: (HasCallStack, MonadIO m) => String -> Doc -> CommandResult Hash -> m ()
assertTxFailure msg d tx =
  liftIO $ assertEqual msg (Just d)
    (tx ^? crResult . to _pactResult . _Left . to peDoc)

-- | Run a single mempool block on current chain with tests for each tx.
-- Limitations: can only run a single-chain, single-refill test for
-- a given cut height.
runBlockTest :: HasCallStack => [PactTxTest] -> PactTestM ()
runBlockTest pts = do
  chid <- view menvChainId
  setPactMempool $ PactMempool [testsToBlock chid pts]
  runCut'
  runBlockTests pts

-- | Convert tests to block for specified chain.
testsToBlock :: ChainId -> [PactTxTest] -> MempoolBlock
testsToBlock chid pts = blockForChain chid $ MempoolBlock $ \_ ->
  pure $ map _pttBuilder pts

-- | Run tests on current cut and chain.
runBlockTests :: HasCallStack => [PactTxTest] -> PactTestM ()
runBlockTests pts = do
  rs <- txResults
  liftIO $ assertEqual "Result length should equal transaction length" (length pts) (length rs)
  zipWithM_ go pts (V.toList rs)
  where
    go :: PactTxTest -> CommandResult Hash -> PactTestM ()
    go (PactTxTest _ t) cr = liftIO $ t cr

-- | Run cuts to block height.
runToHeight :: BlockHeight -> PactTestM ()
runToHeight bhi = do
  chid <- view menvChainId
  bh <- getHeader chid
  when (_blockHeight bh < bhi) $ do
    runCut'
    runToHeight bhi

signSender00 :: CmdBuilder -> CmdBuilder
signSender00 = set cbSigners [mkEd25519Signer' sender00 []]

setFromHeader :: BlockHeader -> CmdBuilder -> CmdBuilder
setFromHeader bh =
  set cbChainId (_blockChainId bh)
  . set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)

buildBasic
    :: PactRPC T.Text
    -> MempoolCmdBuilder
buildBasic = buildBasic' id

buildBasicGas :: GasLimit -> PactRPC T.Text -> MempoolCmdBuilder
buildBasicGas g = buildBasic' (set cbGasLimit g)

-- | Build with specified setter to mutate defaults.
buildBasic'
    :: (CmdBuilder -> CmdBuilder)
    -> PactRPC T.Text
    -> MempoolCmdBuilder
buildBasic' f r = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
  f $ signSender00
  $ setFromHeader bh
  $ set cbRPC r
  $ defaultCmd

-- | Get output on latest cut for chain
getPWO :: ChainId -> PactTestM (PayloadWithOutputs,BlockHeader)
getPWO chid = do
  (TestBlockDb _ pdb _) <- view menvBdb
  h <- getHeader chid
  Just pwo <- liftIO $ lookupPayloadWithHeight pdb (Just $ _blockHeight h) (_blockPayloadHash h)
  return (pwo,h)

getHeader :: ChainId -> PactTestM BlockHeader
getHeader chid = do
  (TestBlockDb _ _ cmv) <- view menvBdb
  c <- liftIO $ readMVar cmv
  fromMaybeM (userError $ "chain lookup failed for " ++ show chid) $ HM.lookup chid (_cutMap c)

txResults :: HasCallStack => PactTestM (V.Vector (CommandResult Hash))
txResults = do
  chid <- view menvChainId
  (o,_h) <- getPWO chid
  forM (_payloadWithOutputsTransactions o) $ \(_,txo) ->
    decodeStrictOrThrow @_ @(CommandResult Hash) (_transactionOutputBytes txo)

