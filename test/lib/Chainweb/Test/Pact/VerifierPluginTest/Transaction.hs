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

import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Default
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Types.Capability
import Pact.Types.Command
import qualified Pact.JSON.Encode as PactJSON
import Pact.Types.PactValue
import Pact.Types.Term
import Pact.Types.Verifier hiding (verifierName)

import Chainweb.Miner.Pact
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.Types
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils

import qualified Chainweb.Test.Pact.VerifierPluginTest.Transaction.Message.After225 as After225
import qualified Chainweb.Test.Pact.VerifierPluginTest.Transaction.Message.Before225 as Before225
import Chainweb.Test.Pact.VerifierPluginTest.Transaction.Utils


tests :: TestTree
tests = testGroup testName
  [ test generousConfig getGasModel "verifierTest" verifierTest

  , test generousConfig getGasModel "recoverValidatorAnnouncementSuccess" hyperlaneRecoverValidatorAnnouncementSuccess
  , test generousConfig getGasModel "recoverValidatorAnnouncementIncorrectSignatureFailure"
    hyperlaneRecoverValidatorAnnouncementIncorrectSignatureFailure
  , test generousConfig getGasModel "recoverValidatorAnnouncementDifferentSignerFailure"
      hyperlaneRecoverValidatorAnnouncementDifferentSignerFailure

  , testGroup "Message"
    [ Before225.tests
    , After225.tests
    ]
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
            SingleEnv bdb pact (return iompa) noMiner cid

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

