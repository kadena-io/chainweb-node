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
import Pact.Types.Verifier

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
  , test generousConfig getGasModel "recoverValidatorAnnouncementFailure" hyperlaneRecoverValidatorAnnouncementFailure

  , test generousConfig getGasModel "verifySuccess" hyperlaneVerifySuccess
  , test generousConfig getGasModel "verifySuccessEmptyRecoveredSignatures" hyperlaneVerifySuccessEmptyRecoveredSignatures

  , test generousConfig getGasModel "verifyFailureWrongValidator" hyperlaneVerifyFailureWrongValidator
  , test generousConfig getGasModel "verifyFailureNotEnoughRecoveredSignatures" hyperlaneVerifyFailureNotEnoughRecoveredSignatures
  , test generousConfig getGasModel "verifyFailureNotEnoughCapabilitySignatures" hyperlaneVerifyFailureNotEnoughCapabilitySignatures
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

  checkEnforceVerifer "allow"

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
    , PactTxTest
      (buildBasic (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure allow: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr)
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

hyperlaneRecoverValidatorAnnouncementSuccess :: PactTestM ()
hyperlaneRecoverValidatorAnnouncementSuccess = do
  runToHeight 118

  checkEnforceVerifer "hyperlane_announcement"

  let cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def) [pString "0x6c414e7a15088023e28af44ad0e1d593671e4b15"]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (address:string) (enforce-verifier 'hyperlane_announcement))"
        , "(defun x () (with-capability (K \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\") 1)))"])
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash 9UnPjHMwwrA5JvbavnZejwKIcmgLmZJ44uAq-9MkQxI"))
    , PactTxTest
      (buildBasic (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure hyperlane_announcement: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr)
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName "hyperlane_announcement")
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "storagelocation"
                , pString "U7oftiGhn7rpWJydP6t0FKStdcRd223a8uSTqKjs8K8nJW7U84tzBOgPZTtGKnncwiu8l1185vB38c7-Ov7avBw"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        assertEqual "gas should have been charged" 16501 (_crGas cr))
    ]

hyperlaneRecoverValidatorAnnouncementFailure :: PactTestM ()
hyperlaneRecoverValidatorAnnouncementFailure = do
  runToHeight 118

  checkEnforceVerifer "hyperlane_announcement"

  let cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "K" def) [pString "0x6c414e7a15088023e28af44ad0e1d593671e4b15"]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap K (address:string) (enforce-verifier 'hyperlane_announcement))"
        , "(defun x () (with-capability (K \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\") 1)))"])
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash 9UnPjHMwwrA5JvbavnZejwKIcmgLmZJ44uAq-9MkQxI"))
    , PactTxTest
      (buildBasic (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure hyperlane_announcement: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr)
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName "hyperlane_announcement")
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "storagelocation"
                , pString "Q7oftiGhn7rpWJydP6t0FKStdcRd223a8uSTqKjs8K8nJW7U84tzBOgPZTtGKnncwiu8l1185vB38c7-Ov7avBw"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure "should have failed" "Tx verifier error: Failed to recover the address from the signature" cr
        assertTxGas "verifier errors charge all gas" 20000 cr)
    ]

hyperlaneVerifySuccess :: PactTestM ()
hyperlaneVerifySuccess = do
  runToHeight 118

  checkEnforceVerifer "hyperlane_message_mrc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0x71239e00ae942b394b3a91ab229e5264ad836f6f" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "MRC20" def) [tokenVal, recipient, signers]

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
        , "(defcap MRC20 (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier 'hyperlane_message_mrc20))"
        , "(defun x () (with-capability (MRC20 {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\", \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"]) 1)))"
        ])
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash Sv_eCpI97oC_MEBVT7moFmZnCQm0Kw9N72IDyEJlaFc"))
    , PactTxTest
      (buildBasic (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure hyperlane_message_mrc20: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr)
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName "hyperlane_message_mrc20")
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "AwAAAAAAAHppAAAAAAAAAAAAAAAAdAsTPe23W9tY0AAFToc8rm_FZfsAAAJyNllLenFwRE5BVG1QaFVKemM1QTE3bUpiRlhILWRCa1YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAmKfZuDFMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJrOjk0YzM1YWIxYmQ3MDI0M2VjNjcwNDk1MDc3Zjc4NDYzNzNiNGRjNWU5Nzc5ZDdhNjczMmI1Y2ViNmZkZTA1OWMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAADMNePpLBoZeRCFBsZ8d2gEepmo1vV4Kf-4Ir_6qBwbsll-DdroT5RbBGBkMG1FwOg4VIXPt3fcsWqEiQcyRN_rGw"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        assertEqual "gas should have been charged" 16511 (_crGas cr))
    ]


hyperlaneVerifySuccessEmptyRecoveredSignatures :: PactTestM ()
hyperlaneVerifySuccessEmptyRecoveredSignatures = do
  runToHeight 118

  checkEnforceVerifer "hyperlane_message_mrc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList []
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "MRC20" def) [tokenVal, recipient, signers]

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
        , "(defcap MRC20 (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier 'hyperlane_message_mrc20))"
        , "(defun x () (with-capability (MRC20 {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\", \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" []) 1)))"
        ])
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash flDIg-O0Wh4MF3Dh-7b5V5R7dAGZS1bAB2x0ZFhT5qY"))
    , PactTxTest
      (buildBasic (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure hyperlane_message_mrc20: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr)
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName "hyperlane_message_mrc20")
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "AwAAAAAAAHppAAAAAAAAAAAAAAAAdAsTPe23W9tY0AAFToc8rm_FZfsAAAJyNllLenFwRE5BVG1QaFVKemM1QTE3bUpiRlhILWRCa1YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAmKfZuDFMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJrOjk0YzM1YWIxYmQ3MDI0M2VjNjcwNDk1MDc3Zjc4NDYzNzNiNGRjNWU5Nzc5ZDdhNjczMmI1Y2ViNmZkZTA1OWMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAAA"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxSuccess "should have succeeded" (pDecimal 1) cr
        assertEqual "gas should have been charged" 260 (_crGas cr))
    ]

hyperlaneVerifyFailureWrongValidator :: PactTestM ()
hyperlaneVerifyFailureWrongValidator = do
  runToHeight 118

  checkEnforceVerifer "hyperlane_message_mrc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "wrongValidator" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "MRC20" def) [tokenVal, recipient, signers]

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
        , "(defcap MRC20 (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier 'hyperlane_message_mrc20))"
        , "(defun x () (with-capability (MRC20 {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\", \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"wrongValidator\"]) 1)))"
        ])
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash B6evuZRGMsh6duuA9Na8cePF0sZTEUBZ33vyLM0c3VY"))
    , PactTxTest
      (buildBasic (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure hyperlane_message_mrc20: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr)
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName "hyperlane_message_mrc20")
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "AwAAAAAAAHppAAAAAAAAAAAAAAAAdAsTPe23W9tY0AAFToc8rm_FZfsAAAJyNllLenFwRE5BVG1QaFVKemM1QTE3bUpiRlhILWRCa1YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAmKfZuDFMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJrOjk0YzM1YWIxYmQ3MDI0M2VjNjcwNDk1MDc3Zjc4NDYzNzNiNGRjNWU5Nzc5ZDdhNjczMmI1Y2ViNmZkZTA1OWMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAADMNePpLBoZeRCFBsZ8d2gEepmo1vV4Kf-4Ir_6qBwbsll-DdroT5RbBGBkMG1FwOg4VIXPt3fcsWqEiQcyRN_rGw"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"})] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]"
        assertTxFailure "should have failed" errMsg cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyFailureNotEnoughRecoveredSignatures :: PactTestM ()
hyperlaneVerifyFailureNotEnoughRecoveredSignatures = do
  runToHeight 118

  checkEnforceVerifer "hyperlane_message_mrc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "wrongValidator" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "MRC20" def) [tokenVal, recipient, signers]

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
        , "(defcap MRC20 (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier 'hyperlane_message_mrc20))"
        , "(defun x () (with-capability (MRC20 {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\", \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"wrongValidator\"]) 1)))"
        ])
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash B6evuZRGMsh6duuA9Na8cePF0sZTEUBZ33vyLM0c3VY"))
    , PactTxTest
      (buildBasic (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure hyperlane_message_mrc20: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr)
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 20000 . set cbVerifiers
          [Verifier
            (VerifierName "hyperlane_message_mrc20")
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "AwAAAAAAAHppAAAAAAAAAAAAAAAAdAsTPe23W9tY0AAFToc8rm_FZfsAAAJyNllLenFwRE5BVG1QaFVKemM1QTE3bUpiRlhILWRCa1YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAmKfZuDFMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJrOjk0YzM1YWIxYmQ3MDI0M2VjNjcwNDk1MDc3Zjc4NDYzNzNiNGRjNWU5Nzc5ZDdhNjczMmI1Y2ViNmZkZTA1OWMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAAA"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure "should have failed" "Tx verifier error: Signers don't match. Expected: PList [] but got PList [PLiteral (LString {_lString = \"wrongValidator\"})]" cr
        assertEqual "gas should have been charged" 20000 (_crGas cr))
    ]

hyperlaneVerifyFailureNotEnoughCapabilitySignatures :: PactTestM ()
hyperlaneVerifyFailureNotEnoughCapabilitySignatures = do
  runToHeight 118

  checkEnforceVerifer "hyperlane_message_mrc20"

  let
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", pString "k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c")
        , ("amount", PLiteral $ LDecimal 44) ]
    recipient = pString "6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV"
    signers = PList $ V.fromList [ pString "0x71239e00ae942b394b3a91ab229e5264ad836f6f" ]
    cap = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "MRC20" def) [tokenVal, recipient, signers]

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
        , "(defcap MRC20 (tokenVal:object{token} recipient:string signers:[string]) (enforce-verifier 'hyperlane_message_mrc20))"
        , "(defun x () (with-capability (MRC20 {\"recipient\": \"k:94c35ab1bd70243ec670495077f7846373b4dc5e9779d7a6732b5ceb6fde059c\", \"amount\": 44.0} \"6YKzqpDNATmPhUJzc5A17mJbFXH-dBkV\" [\"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"]) 1)))"
        ])
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash Sv_eCpI97oC_MEBVT7moFmZnCQm0Kw9N72IDyEJlaFc"))
    , PactTxTest
      (buildBasic (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        assertTxFailure
          "verifier not present"
          "Verifier failure hyperlane_message_mrc20: not in transaction"
          cr
        assertTxGas "verifier errors charge all gas" 10000 cr)
    , PactTxTest
      (buildBasic'
        (set cbGasLimit 40000 . set cbVerifiers
          [Verifier
            (VerifierName "hyperlane_message_mrc20")
            (ParsedVerifierProof $
              PList $ V.fromList
                [ pString "AwAAAAAAAHppAAAAAAAAAAAAAAAAdAsTPe23W9tY0AAFToc8rm_FZfsAAAJyNllLenFwRE5BVG1QaFVKemM1QTE3bUpiRlhILWRCa1YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAmKfZuDFMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJrOjk0YzM1YWIxYmQ3MDI0M2VjNjcwNDk1MDc3Zjc4NDYzNzNiNGRjNWU5Nzc5ZDdhNjczMmI1Y2ViNmZkZTA1OWMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , pString "AAAAAAAAAAAAAAAAWvVWHDAXciof5CM4z1v8YV6seP8nGlCMb-CZnYe--Oj5XqAJdOHp36cJ9RYwxxw0jiAenwAAAADMNePpLBoZeRCFBsZ8d2gEepmo1vV4Kf-4Ir_6qBwbsll-DdroT5RbBGBkMG1FwOg4VIXPt3fcsWqEiQcyRN_rG8w14-ksGhl5EIUGxnx3aAR6majW9Xgp_7giv_qoHBuyWX4N2uhPlFsEYGQwbUXA6DhUhc-3d9yxaoSJBzJE3-sb"
                ]
              )
            [cap]])
            (mkExec' "(free.m.x)"))
      (\cr -> liftIO $ do
        let errMsg = "Tx verifier error: Signers don't match. Expected: PList [PLiteral (LString {_lString = \"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"}),PLiteral (LString {_lString = \"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"})] but got PList [PLiteral (LString {_lString = \"0x71239e00ae942b394b3a91ab229e5264ad836f6f\"})]"
        assertTxFailure "should have failed" errMsg cr
        assertEqual "gas should have been charged" 40000 (_crGas cr))
    ]

-- =========================================================
--
-- Fixture
--
-- =========================================================

checkEnforceVerifer :: T.Text -> PactTestM ()
checkEnforceVerifer v = do
  runBlockTest
    [ PactTxTest
        (buildBasic (mkExec' $ "(enforce-verifier '" <> v <> ")"))
        (assertTxFailure "Should not resolve enforce-verifier" "Cannot resolve enforce-verifier")
    ]

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

