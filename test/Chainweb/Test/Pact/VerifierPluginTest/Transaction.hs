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
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Hash
import qualified Pact.JSON.Encode as PactJSON
import Pact.Types.Exp
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
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table (casLookupM)

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

  runBlockTest
    [ PactTxTest
        (buildBasic (mkExec' $ "(enforce-verifier 'allow)"))
        (assertTxFailure "Should not resolve enforce-verifier" "Cannot resolve enforce-verifier")
    ]

  let cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "G" def) []

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () (enforce-verifier 'allow))"
        , "(defun x () (with-capability (G) 1)))"])
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash QNTlTCp-KMPkT52CEo_0zGaLJ_PnAxsenyhUck1njcc"))
    , checkVerifierNotInTx "allow"
    , PactTxTest
      (buildBasic'
        (set cbVerifiers
          [Verifier
            (VerifierName "allow")
            (ParsedVerifierProof $ pString (PactJSON.encodeText cap))
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        -- The **Allow** verifier costs 100 gas flat
        assertEqual "gas should have been charged" 344 (_crGas cr))
    ]

-- hyperlane validator announcement tests

hyperlaneRecoverValidatorAnnouncementSuccess :: PactTestM ()
hyperlaneRecoverValidatorAnnouncementSuccess = do
  runToHeight 119

  let cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def)
              [pString "storagelocation", pString "0x6c414e7a15088023e28af44ad0e1d593671e4b15", pString "kb-mailbox"]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (location:string signer:string mailbox:string) (enforce-verifier 'hyperlane_announcement))"
        , "(defun x () (with-capability (K \"storagelocation\" \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\" \"kb-mailbox\") 1)))"])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash 1JDzly5XUBLPnXoBQNlvPd91Rhsb6P0E5oZHjuKShjk"))
    , checkVerifierNotInTx "hyperlane_announcement"
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName "hyperlane_announcement")
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
  let verifierName = "hyperlane_announcement"

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
        (pString "Loaded module free.m, hash 1JDzly5XUBLPnXoBQNlvPd91Rhsb6P0E5oZHjuKShjk"))
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
  let verifierName = "hyperlane_announcement"

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
        (pString "Loaded module free.m, hash AcLq0Za-N65GsqlUV_6MzbhhilHV-9EoXPVgR9hNoeQ"))
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

-- | Hyperlane test message encoded in base64
hyperlaneMessageBase64 :: T.Text
hyperlaneMessageBase64 = "AwAAAAAAAHppAAAAAAAAAAAAAAAAdAsTPe23W9tY0AAFToc8rm_FZfsAAAJyNllLenFwRE5BVG1QaFVKemM1QTE3bUpiRlhILWRCa1YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAmKfZuDFMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJrOjk0YzM1YWIxYmQ3MDI0M2VjNjcwNDk1MDc3Zjc4NDYzNzNiNGRjNWU5Nzc5ZDdhNjczMmI1Y2ViNmZkZTA1OWMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

hyperlaneVerifySuccess :: PactTestM ()
hyperlaneVerifySuccess = do
  runToHeight 119
  let verifierName = "hyperlane_message_erc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0x71239e00ae942b394b3a91ab229e5264ad836f6f" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def) [tokenVal, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defschema token"
        , "  recipient:string"
        , "  amount:decimal"
        , ")"
        , "(defcap K (tokenVal:object{token} recipient:string signers:[string])"
        , "  (enforce-verifier '" <> verifierName <> ")"
        , "  (enforce (= signers [\"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"]) \"invalid signers\")"
        , "  (enforce (= recipient \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\") \"invalid recipient\")"
        , ")"
        , "(defun x () (with-capability (K "
        , " {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\", \"amount\": 44.0}"
        , " \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\""
        , " [\"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"]"
        , ")"
        , " \"succeeded\")))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash rweonocsxV_6gtOkhFU2gI4xqbmQ_dHkd1uQDhs4mrs"))
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
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAADMNePpLBoZeRCFBsZ8d2gEepmo1vV4Kf-4Ir_6qBwbsll-DdroT5RbBGBkMG1FwOg4VIXPt3fcsWqEiQcyRN_rGw"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pString "succeeded") cr
        assertEqual "gas should have been charged" 16517 (_crGas cr))
    ]


hyperlaneVerifyEmptyRecoveredSignaturesSuccess :: PactTestM ()
hyperlaneVerifyEmptyRecoveredSignaturesSuccess = do
  runToHeight 119
  let verifierName = "hyperlane_message_erc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList []
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def) [tokenVal, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defschema token"
        , "  recipient:string"
        , "  amount:decimal"
        , ")"
        , "(defcap K (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\","
        , " \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" []) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash yyTe5jtH55vPxqRptIj9HeD84BbkaH-AOEfN40Fo7GM"))
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
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAAA"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        assertEqual "gas should have been charged" 260 (_crGas cr))
    ]

hyperlaneVerifyWrongSignersFailure :: PactTestM ()
hyperlaneVerifyWrongSignersFailure = do
  runToHeight 119
  let verifierName = "hyperlane_message_erc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    -- incorrect validator
    signers = PList $ V.fromList [ pString "wrongValidator" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def) [tokenVal, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defschema token"
        , "  recipient:string"
        , "  amount:decimal"
        , ")"
        , "(defcap K (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\","
        , " \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"wrongValidator\"]) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash bSoS6nk6-HtHyeF9ydrLcfFa6ErI5G4iELCPY9oq_M8"))
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
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAADMNePpLBoZeRCFBsZ8d2gEepmo1vV4Kf-4Ir_6qBwbsll-DdroT5RbBGBkMG1FwOg4VIXPt3fcsWqEiQcyRN_rGw"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"})] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]"
        assertTxFailure "should have failed with signers don't match" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyNotEnoughRecoveredSignaturesFailure :: PactTestM ()
hyperlaneVerifyNotEnoughRecoveredSignaturesFailure = do
  runToHeight 119
  let verifierName = "hyperlane_message_erc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "wrongValidator" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def) [tokenVal, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defschema token"
        , "  recipient:string"
        , "  amount:decimal"
        , ")"
        , "(defcap K (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\","
        , " \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"wrongValidator\"]) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash bSoS6nk6-HtHyeF9ydrLcfFa6ErI5G4iELCPY9oq_M8"))
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
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAAA"
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
  let verifierName = "hyperlane_message_erc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0x71239e00ae942b394b3a91ab229e5264ad836f6f" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def) [tokenVal, recipient, signers]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defschema token"
        , "  recipient:string"
        , "  amount:decimal"
        , ")"
        , "(defcap K (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier '" <> verifierName <> "))"
        , "(defun x () (with-capability (K {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\","
        , " \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"]) 1)))"
        ])
      (assertTxSuccess
        "Should deploy module"
        (pString "Loaded module free.m, hash FbjeaPqjz0Zyfd8IrWHKGblVs45lOQ9wTJfrbX7AI_I"))
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
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAADMNePpLBoZeRCFBsZ8d2gEepmo1vV4Kf-4Ir_6qBwbsll-DdroT5RbBGBkMG1FwOg4VIXPt3fcsWqEiQcyRN_rG8w14-ksGhl5EIUGxnx3aAR6majW9Xgp_7giv_qoHBuyWX4N2uhPlFsEYGQwbUXA6DhUhc-3d9yxaoSJBzJE3-sb"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"}),PLiteral (LString {_lString = \"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"})] but got PList [PLiteral (LString {_lString = \"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"})]"
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
  pwo <- liftIO $ casLookupM pdb (_blockPayloadHash h)
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

