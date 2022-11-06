{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact.PactMultiChainTest
( tests
) where

import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Base64.URL as B64U
import Data.CAS (casLookupM)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Continuation
import Pact.Types.Hash
import Pact.Types.PactError
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.RPC
import Pact.Types.Runtime (PactEvent)
import Pact.Types.SPV
import Pact.Types.Term

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
import Chainweb.Pact.TransactionExec (listErrMsg)
import Chainweb.Payload
import Chainweb.SPV.CreateProof
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebPactExecutionService

testVersion :: ChainwebVersion
testVersion = FastTimedCPM peterson

cid :: ChainId
cid = someChainId testVersion

data MultiEnv = MultiEnv
    { _menvBdb :: TestBlockDb
    , _menvPact :: WebPactExecutionService
    , _menvMpa :: IO (IORef MemPoolAccess)
    , _menvMiner :: Miner
    , _menvChainId :: ChainId
    }

makeLenses ''MultiEnv

type PactTestM = ReaderT MultiEnv IO

data MempoolInput = MempoolInput
    { _miBlockFill :: BlockFill
    , _miBlockHeader :: BlockHeader }

newtype MempoolCmdBuilder = MempoolCmdBuilder
    { _mempoolCmdBuilder :: MempoolInput -> CmdBuilder
    }

-- | Block filler. A 'Nothing' result means "skip this filler".
newtype MempoolBlock = MempoolBlock
    { _mempoolBlock :: MempoolInput -> Maybe [MempoolCmdBuilder]
    }

-- | Mempool with an ordered list of fillers.
newtype PactMempool = PactMempool
  { _pactMempool :: [MempoolBlock] }
  deriving (Semigroup,Monoid)


-- | Pair a builder with a test
data PactTxTest = PactTxTest
    { _pttBuilder :: MempoolCmdBuilder
    , _pttTest :: CommandResult Hash -> Assertion
    }

tests :: ScheduledTest
tests = ScheduledTest testName go
  where
    testName = "Chainweb.Test.Pact.PactMultiChainTest"
    go = testGroup testName
         [ multiChainTest freeGasModel "pact4coin3UpgradeTest" pact4coin3UpgradeTest
         , multiChainTest freeGasModel "pact420UpgradeTest" pact420UpgradeTest
         , multiChainTest freeGasModel "minerKeysetTest" minerKeysetTest
         , multiChainTest getGasModel "chainweb213Test" chainweb213Test
         , multiChainTest getGasModel "pact43UpgradeTest" pact43UpgradeTest
         , multiChainTest getGasModel "pact431UpgradeTest" pact431UpgradeTest
         , multiChainTest getGasModel "chainweb215Test" chainweb215Test
         , multiChainTest getGasModel "chainweb216Test" chainweb216Test
         ]
      where
        multiChainTest gasmodel tname f =
          withDelegateMempool $ \dmpio -> testCase tname $
            withTestBlockDb testVersion $ \bdb -> do
              (iompa,mpa) <- dmpio
              withWebPactExecutionService testVersion bdb mpa gasmodel $ \pact ->
                runReaderT f $
                MultiEnv bdb pact (return iompa) noMiner cid


minerKeysetTest :: PactTestM ()
minerKeysetTest = do

  -- run past genesis, upgrades
  runToHeight 24

  -- run block 4
  local (set menvMiner badMiner) $ do
    void runCut'

    -- run block 5 (fork for chainweb213)
    r <- try $ runCut'
    assertSatisfies "badMiner fails after fork" r $ \case
      Left (CoinbaseFailure t) -> "Invalid miner key" `T.isInfixOf` t
      _ -> False

  where

    badMiner = Miner (MinerId "miner") $ MinerKeys $ mkKeySet ["bad-bad-bad"] "keys-all"


chainweb213Test :: PactTestM ()
chainweb213Test = do

  -- run past genesis, upgrades
  runToHeight 24

  -- run block 25
  runBlockTest
      [ PactTxTest buildModCmd1 $
        assertTxGas "Old gas cost" 56
      , PactTxTest (buildSimpleCmd' "(list 1 2 3)") $
        assertTxFailure "list failure 1_1"
        listErrMsg
      , PactTxTest buildDbMod $
        assertTxSuccess "mod db installs" $
        pString "TableCreated"
      , PactTxTest (buildSimpleCmd' "(free.dbmod.fkeys)") $
        assertTxGas "fkeys gas cost 1" 214
      , PactTxTest (buildSimpleCmd' "(free.dbmod.ffolddb)") $
        assertTxGas "ffolddb gas cost 1" 215
      , PactTxTest (buildSimpleCmd' "(free.dbmod.fselect)") $
        assertTxGas "fselect gas cost 1" 215
      ]


  -- run block 26
  runBlockTest
      [ PactTxTest buildModCmd2 $
        assertTxGas "New gas cost" 60065
      , PactTxTest (buildSimpleCmd' "(list 1 2 3)") $
        assertTxFailure "list failure 2_1"
        "Gas limit (50000) exceeded: 1000013"
      , PactTxTest  (buildSimpleCmd' "(free.dbmod.fkeys)") $
        assertTxGas "fkeys gas cost 2" 40014
      , PactTxTest (buildSimpleCmd' "(free.dbmod.ffolddb)") $
        assertTxGas "ffolddb gas cost 2" 40015
      , PactTxTest (buildSimpleCmd' "(free.dbmod.fselect)") $
        assertTxGas "fselect gas cost 2" 40015
      ]


  where

    buildSimpleCmd' code = buildBasicGas 50000
        $ mkExec' code
    buildModCmd1 = buildBasic
        $ mkExec' $ mconcat ["(namespace 'free)", "(module mtest G (defcap G () true) (defun a () true))"]
    buildModCmd2 = buildBasicGas 70000
        $ mkExec' $ mconcat ["(namespace 'free)", "(module mtest2 G (defcap G () true) (defun a () false))"]
    buildDbMod = buildBasicGas 70000
        $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module dbmod G (defcap G () true)"
        , "  (defschema sch i:integer) (deftable tbl:{sch})"
        , "  (defun fkeys () (keys tbl))"
        , "  (defun ffolddb () (fold-db tbl (lambda (a b) true) (constantly true)))"
        , "  (defun fselect () (select tbl (constantly true))))"
        , "(create-table tbl)"
        ]

pact43UpgradeTest :: PactTestM ()
pact43UpgradeTest = do

  -- run past genesis, upgrades
  runToHeight 29

  runBlockTest
      [ PactTxTest buildMod $
        assertTxGas "Old gas cost" 120332
      , PactTxTest buildModPact $
        assertTxFailure
        "Should not resolve new pact native: continue"
        "Cannot resolve \"continue\""
      , PactTxTest (buildSimpleCmd "(create-principal (read-keyset 'k))") $
        assertTxFailure
        "Should not resolve new pact native: create-principal"
        "Cannot resolve create-principal"
      , PactTxTest (buildSimpleCmd "(validate-principal (read-keyset 'k) \"k:368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca\")") $
        assertTxFailure
        "Should not resolve new pact natives: validate-principal"
        "Cannot resolve validate-principal"
      , PactTxTest (buildXSend []) $
        assertTxSuccess "xsend success" $
        pObject [ ("amount",pDecimal 0.0123)
                , ("receiver",pString "sender00")
                , ("receiver-guard",pKeySet sender00Ks)
                ]
      ]

  tx30_4 <- txResult 4

  runBlockTest
      [ PactTxTest buildMod $
        assertTxGas "Old gas cost" 120296
      , PactTxTest buildModPact $
        assertTxSuccess
        "Should resolve continue in a module defn"
        (pString "Loaded module free.nestedMod, hash fDd0G7zvGar3ax2q0I0F9dISRq7Pjop5rUXOeokNIOU")
      , PactTxTest (buildSimpleCmd "(free.modB.chain)") $
        assertTxSuccess
        "Should resolve names properly post-fork"
      -- Note: returns LDecimal because of toPactValueLenient in interpret
        (pDecimal 11)
      , PactTxTest (buildSimpleCmd "(free.modB.get-test)") $
        assertTxSuccess
        "Should resolve names properly post-fork"
        (pString "hello")
      , PactTxTest (buildSimpleCmd "(create-principal (read-keyset 'k))") $
        assertTxSuccess
        "Should resolve create-principal properly post-fork"
        (pString "k:368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca")
      , PactTxTest (buildSimpleCmd "(validate-principal (read-keyset 'k) \"k:368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca\")") $
        assertTxSuccess
        "Should resolve validate-principal properly post-fork"
        (pBool True)
      ]

  resetMempool
  runToHeight 33

  xproof <- buildXProof cid 30 4 tx30_4

  withChain chain0 $ runBlockTest
      [ PactTxTest (buildXReceive xproof) $
        assertTxSuccess "xreceive success" (pString "Write succeeded")
      ]

  where

    buildSimpleCmd code = buildBasicGas 1000
        $ mkExec code
        $ mkKeySetData "k" [sender00]
    buildModPact = buildBasicGas 70000
        $ mkExec' (mconcat
        [ "(namespace 'free)"
        , "(module nestedMod G"
        , "  (defcap G () true)"
        , "  (defpact test:string () (step \"1\") (step \"2\") (step \"3\"))"
        , "  (defpact test-nested:string () (step (test)) (step (continue (test))) (step (continue (test))))"
        , ")"
        ])
    buildMod = buildBasicGas 130000
        $ mkExec (mconcat
        [ "(namespace 'free)"
        , "(module modA G"
        , "  (defcap G () true)"
        , "  (defun func:integer (x:integer) (+ 1 x))"
        , "  (defun func2:integer (x:integer) (+ (func x) (func x)))"
        , "  (defconst test:string \"hi\")"
        , ")"
        , "(module modB G"
        , "  (defcap G () true)"
        , "  (defun chain:integer () (modA.func 10))"
        , "  (defconst test:string \"hello\")"
        , "  (defun get-test() test)"
        , ")"
        ])
        $ mkKeySetData "k" [sender00]



chainweb215Test :: PactTestM ()
chainweb215Test = do

  -- run past genesis, upgrades
  runToHeight 30 -- 1->30

  -- execute pre-fork xchain transfer (blocc0)
  runBlockTest
      [ PactTxTest xsend $ \cr -> do
          evs <- mkSendEvents 0.0426 v4Hash
          assertTxEvents "Transfer events @ block 31" evs cr
      ]
  send0 <- txResult 0

  -- run past v5 upgrade, build proof of pre-fork xchain for tx31_0, save cut
  resetMempool
  runToHeight 34
  savedCut <- currentCut
  runToHeight 41

  xproof <- buildXProof cid 31 0 send0

  let blockSend42 =
        [ PactTxTest xsend $ \cr -> do
            evs <- mkSendEvents 0.0429 v5Hash
            assertTxEvents "Transfer events @ block 42 - post-fork send" evs cr
        ]
      blockRecv42 =
        [ PactTxTest (buildXReceive xproof) $ \cr -> do
            evs <- mkRecdEvents v5Hash v4Hash
            assertTxEvents "Transfer events @ block 42" evs cr
        ]

  setPactMempool $ PactMempool
      [ testsToBlock cid blockSend42
      , testsToBlock chain0 blockRecv42 ]
  runCut'
  withChain cid $ runBlockTests blockSend42
  withChain chain0 $ runBlockTests blockRecv42

  send1 <- withChain cid $ txResult 0

  currCut <- currentCut

    -- rewind to saved cut 43
  rewindTo savedCut
  runToHeight 43

  -- resume on original cut
  rewindTo currCut

  -- run until post-fork xchain proof exists
  resetMempool
  runToHeight 50
  savedCut1 <- currentCut
  runToHeight 52

  xproof1 <- buildXProof cid 42 0 send1

  withChain chain0 $ runBlockTest
    [ PactTxTest (buildXReceive xproof1) $ \cr -> do
        evs <- mkRecdEvents v5Hash v5Hash
        assertTxEvents "Transfer events @ block 53" evs cr
    ]

    -- rewind to saved cut 50
  rewindTo savedCut1
  runToHeight 53

  where
    xsend = buildXSend
        [ mkGasCap
        , mkXChainTransferCap "sender00" "sender00" 0.0123 "0"
        ]

    v4Hash = "BjZW0T2ac6qE_I5X8GE4fal6tTqjhLTC7my0ytQSxLU"
    v5Hash = "rE7DU8jlQL9x_MPYuniZJf5ICBTAEHAIFQCB4blofP4"

    mkSendEvents cbCost h = sequence
      [ mkTransferEvent "sender00" "NoMiner" cbCost "coin" h
      , mkTransferXChainEvent "sender00" "sender00" 0.0123 "coin" h "0"
      , mkTransferEvent "sender00" "" 0.0123 "coin" h
      , mkXYieldEvent "sender00" "sender00" 0.0123 sender00Ks "pact" h "0" "0"
      ]

    mkRecdEvents h h' = sequence
      [ mkTransferEvent "sender00" "NoMiner" 0.0258 "coin" h
      , mkTransferEvent "" "sender00" 0.0123 "coin" h
      , mkTransferXChainRecdEvent "" "sender00" 0.0123 "coin" h "8"
      , mkXResumeEvent "sender00" "sender00" 0.0123 sender00Ks "pact" h' "8" "0"
      ]



pact431UpgradeTest :: PactTestM ()
pact431UpgradeTest = do

  -- run past genesis, upgrades
  runToHeight 34

  -- run block 35, pre fork
  runBlockTest
    [ PactTxTest describeModule $
      assertTxSuccess "describe-module legacy success"
      $ pBool True
    , PactTxTest isPrincipal $
      assertTxFailure "Should not resolve new pact native: is-principal"
      "Cannot resolve is-principal"
    , PactTxTest typeOfPrincipal $
      assertTxFailure "Should not resolve new pact native: typeof-principal"
      "Cannot resolve typeof-principal"
    , PactTxTest enforcePactVersion $
      assertTxSuccess "Enforce pact version passes pre-fork"
      $ pBool True
    , PactTxTest pactVersion $
      assertTxSuccess "Pact version is 4.2.1 for compat pre-fork"
      $ pString "4.2.1"
    ]

  -- run block 36, post fork
  runBlockTest
    [ PactTxTest describeModule $
      assertTxFailure "Should fail to execute describe-module"
      "Operation only permitted in local execution mode"
    , PactTxTest isPrincipal $
      assertTxSuccess "Should resolve new pact native: is-principal"
      $ pBool True
    , PactTxTest typeOfPrincipal $
      assertTxSuccess "Should resolve new pact native: typeof-principal"
      $ pString "k:"
    , PactTxTest enforcePactVersion $
      assertTxFailure "Should fail to execute enforce-pact-version"
      "Operation only permitted in local execution mode"
    , PactTxTest pactVersion $
      assertTxFailure "Should fail to execute pact-version"
      "Operation only permitted in local execution mode"
    ]


  where
    isPrincipal =
      buildSimpleCmd "(is-principal (create-principal (read-keyset 'k)))"
    typeOfPrincipal =
      buildSimpleCmd "(typeof-principal (create-principal (read-keyset 'k)))"
    enforcePactVersion =
      buildSimpleCmd "(enforce-pact-version \"4.2.1\")"
    pactVersion =
      buildSimpleCmd "(pact-version)"
    buildSimpleCmd code = buildBasicGas 1000
        $ mkExec code
        $ mkKeySetData "k" [sender00]
    describeModule = buildBasicGas 100000
      $ mkExec' (mconcat
        [ "(namespace 'free)"
        , "(module mod G"
        , "  (defcap G () true)"
        , "  (defun f () true)"
        , ")"
        , "(describe-module \"free.mod\") true"
        ])


pact420UpgradeTest :: PactTestM ()
pact420UpgradeTest = do

  -- run past genesis, upgrades
  runToHeight 3

  -- run block 4
  runBlockTest
    [ PactTxTest buildNewNatives420FoldDbCmd $
      assertTxFailure
      "Should not resolve new pact natives"
      "Cannot resolve fold-db"
    , PactTxTest buildNewNatives420ZipCmd $
      assertTxFailure
      "Should not resolve new pact natives"
      "Cannot resolve zip"
    , PactTxTest buildFdbCmd $
      assertTxSuccess
      "Load fdb module"
      (pString "Write succeeded")
    ]

  cbResult >>= assertTxEvents "Coinbase events @ block 4" []

  -- run block 5

  runBlockTest
    [ PactTxTest buildNewNatives420FoldDbCmd $
      assertTxSuccess
      "Should resolve fold-db pact native" $
      pList [pObject [("a", pInteger 1),("b",pInteger 1)]
            ,pObject [("a", pInteger 2),("b",pInteger 2)]]
    , PactTxTest buildNewNatives420ZipCmd $
      assertTxSuccess
      "Should resolve zip pact native" $
      pList $ pInteger <$> [5,7,9]
    ]

  cbResult >>= assertTxEvents "Coinbase events @ block 5" []

  where

    buildFdbCmd = buildBasic
        $ mkExec' $ mconcat ["(namespace 'free)", moduleDeclaration, inserts]
      where
        moduleDeclaration =
          "(module fdb G (defcap G () true) (defschema fdb-test a:integer b:integer) (deftable fdb-tbl:{fdb-test}))"
        inserts =
          mconcat
            [
              "(create-table free.fdb.fdb-tbl)"
            , "(insert free.fdb.fdb-tbl 'b {'a:2, 'b:2})"
            , "(insert free.fdb.fdb-tbl 'd {'a:4, 'b:4})"
            , "(insert free.fdb.fdb-tbl 'c {'a:3, 'b:3})"
            , "(insert free.fdb.fdb-tbl 'a {'a:1, 'b:1})"
            ]

    buildNewNatives420FoldDbCmd = buildBasic
        $ mkExec'
        "(let* ((qry (lambda (k o) (<  k \"c\"))) (consume (lambda (k o) o))) (fold-db free.fdb.fdb-tbl (qry) (consume)))"


    buildNewNatives420ZipCmd = buildBasic
        $ mkExec' "(zip (+) [1 2 3] [4 5 6])"

chainweb216Test :: PactTestM ()
chainweb216Test = do
  -- This test should handles for format and try as well as
  -- keyset format changes and disallowances across fork boundaries.
  --
  -- Namely, to test keys properly, we should:
  --
  -- 1. Make sure keys defined before and after
  --    fork boundaries pass enforcement.
  --
  -- 2. Keys defined after the fork are only
  --    definable if a namespace is present.
  --

  -- run past genesis, upgrades
  runToHeight 52

  runBlockTest
      [ PactTxTest (buildSimpleCmd formatGas) $
        assertTxGas "Pre-fork format gas" 21
      , PactTxTest (buildSimpleCmd tryGas) $
        assertTxGas "Pre-fork try" 18
      , PactTxTest (buildSimpleCmd defineNonNamespacedPreFork) $
        assertTxSuccess
        "Should pass when defining a non-namespaced keyset"
        (pBool True)
      , PactTxTest (buildSimpleCmd defineNamespacedPreFork) $
      -- Note, keysets technically are not namespaced pre-fork, the new parser isn't applied
        assertTxSuccess
        "Should pass when defining a \"namespaced\" keyset pre fork"
        (pBool True)
      ]

  runBlockTest
      [ PactTxTest (buildSimpleCmd formatGas) $
        assertTxGas "Post-fork format gas increase" 48
      , PactTxTest (buildSimpleCmd tryGas) $
        assertTxGas "Post-fork try should charge a bit more gas" 19
      , PactTxTest (buildSimpleCmd defineNonNamespacedPostFork1) $
        assertTxFailure
        "Should fail when defining a non-namespaced keyset post fork"
        "Mismatching keyset namespace"
      , PactTxTest (buildSimpleCmd defineNamespacedPostFork) $
        assertTxSuccess
        "Pass when defining a namespaced keyset post fork"
        (pBool True)
      , PactTxTest (buildSimpleCmd enforceNamespacedFromPreFork) $
        assertTxSuccess
        "Should work in enforcing a namespaced keyset created prefork"
        (pBool True)
      , PactTxTest (buildSimpleCmd enforceNonNamespacedFromPreFork) $
        assertTxSuccess
        "Should work in enforcing a non-namespaced keyset created prefork"
        (pBool True)
      , PactTxTest (buildSimpleCmd defineNonNamespacedPostFork2) $
        assertTxFailure
        "Should fail in defining a keyset outside a namespace"
        "Cannot define a keyset outside of a namespace"
      , PactTxTest buildModCommand $
        assertTxSuccess
        "Should succeed in deploying a module guarded by a namespaced keyset"
        (pString "Loaded module free.m1, hash nOHaU-gPtmZTj6ZA3VArh-r7LEiwVUMN_RLJeW2hNv0")
      , PactTxTest (buildSimpleCmd rotateLegacyPostFork) $
        assertTxSuccess
        "Should succeed in rotating and enforcing a legacy keyset"
        (pBool True)
      , PactTxTest (buildSimpleCmd rotateNamespacedPostFork) $
        assertTxSuccess
        "Should succeed in rotating and enforcing a namespaced keyset"
        (pBool True)
      ]

  runBlockTest
      [ PactTxTest (buildSimpleCmd "(free.m1.f)") $
        assertTxSuccess
        "Should call a module with a namespaced keyset correctly"
        (pDecimal 1)
      , PactTxTest (buildSimpleCmd "(^ 15.034465284692086701747761395233132973944448512421004399685858401206740385711739229018307610943234609057822959334669087436253689423614206061665462283698768757790600552385430913941421707844383369633809803959413869974997415115322843838226312287673293352959835 3.466120406090666777582519661568003549307295836842780244500133445635634490670936927006970368136648330889718447039413255137656971927890831071689768359173260960739254160211017410322799793419223796996260056081828170546988461285168124170297427792046640116184356)") $
        assertTxSuccess
        "musl exponentiation regression"
        (pDecimal 12020.67042599064370733685791492462158203125)
      ]


  where
  defineNonNamespacedPreFork = mconcat
    [ "(define-keyset \'k123)"
    , "(enforce-guard (keyset-ref-guard \'k123))"
    ]
  defineNamespacedPreFork = mconcat
    [ "(define-keyset \"free.k123\")"
    , "(enforce-guard (keyset-ref-guard \"free.k123\"))"
    ]
  defineNonNamespacedPostFork1 = mconcat
    [ "(namespace 'free)"
    , "(define-keyset \'k456)"
    ]
  defineNonNamespacedPostFork2 = mconcat
    [ "(define-keyset \'k456)"
    ]
  defineNamespacedPostFork = mconcat
    [ "(namespace 'free)"
    , "(define-keyset \"free.k456\")"
    , "(enforce-guard (keyset-ref-guard \"free.k456\"))"
    ]
  rotateLegacyPostFork = mconcat
    [ "(namespace 'free)"
    , "(define-keyset \"k123\" (read-keyset 'k456))"
    , "(enforce-guard (keyset-ref-guard \"k123\"))"
    ]
  rotateNamespacedPostFork = mconcat
    [ "(namespace 'free)"
    , "(define-keyset \"free.k123\" (read-keyset 'k456))"
    , "(enforce-guard (keyset-ref-guard \"free.k123\"))"
    ]
  defineModulePostFork = mconcat
    [ "(namespace 'free)"
    , "(module m1 \"free.k456\" (defun f () 1))"
    ]
  enforceNamespacedFromPreFork = "(enforce-guard (keyset-ref-guard \"free.k123\"))"
  enforceNonNamespacedFromPreFork = "(enforce-guard (keyset-ref-guard \"k123\"))"
  tryGas = "(try (+ 1 1) (enforce false \"abc\"))"
  formatGas = "(format \"{}-{}\" [1234567, 890111213141516])"


  buildModCommand = buildBasicGas 70000
    $ mkExec' defineModulePostFork

  buildSimpleCmd code = buildBasicGas 10000
    $ mkExec code
    $ object
      [ "k123" .= map fst [sender00]
      , "k456" .= map fst [sender00]
      , "free.k123" .= map fst [sender00]
      , "free.k456" .= map fst [sender00]]

pact4coin3UpgradeTest :: PactTestM ()
pact4coin3UpgradeTest = do

  -- run past genesis, upgrades
  runToHeight 6

  -- run block 7
  runBlockTest
    [ PactTxTest buildHashCmd $ \cr -> do
        assertTxSuccess "Hash of coin @ block 7"
            (pString "ut_J_ZNkoyaPUEJhiwVeWnkSQn9JT9sQCWKdjjVVrWo")
            cr
        assertTxEvents "Events for tx 0 @ block 7" [] cr
    , PactTxTest (buildXSend []) $
      assertTxEvents "Events for tx 1 @ block 7" []
    , PactTxTest buildNewNatives40Cmd $
      assertTxFailure
      "Should not resolve new pact natives"
      "Cannot resolve distinct"
    , PactTxTest badKeyset $
      assertTxSuccess
      "Should allow bad keys" $
      pKeySet $ mkKeySet ["badkey"] "keys-all"
    ]

  assertTxEvents "Coinbase events @ block 7" [] =<< cbResult
  send0 <- txResult 1

  -- run past v3 upgrade, pact 4 switch
  resetMempool
  runToHeight 18
  savedCut <- currentCut
  runToHeight 21

  -- block 22
  -- get proof
  xproof <- buildXProof cid 7 1 send0

  let v3Hash = "1os_sLAUYvBzspn5jjawtRpJWiH1WPfhyNraeVvSIwU"
      block22 =
        [ PactTxTest buildHashCmd $ \cr -> do
            gasEv0 <- mkTransferEvent "sender00" "NoMiner" 0.0013 "coin" v3Hash
            assertTxSuccess "Hash of coin @ block 22" (pString v3Hash) cr
            assertTxEvents "Events for tx0 @ block 22" [gasEv0] cr
        , PactTxTest buildReleaseCommand $ \cr -> do
            gasEv1 <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
            allocTfr <- mkTransferEvent "" "allocation00" 1000000.0 "coin" v3Hash
            allocEv <- mkEvent "RELEASE_ALLOCATION" [pString "allocation00",pDecimal 1000000.0]
                       "coin" v3Hash
            assertTxEvents "Events for tx1 @ block 22" [gasEv1,allocEv,allocTfr] cr
        , PactTxTest (buildXSend []) $ \cr -> do
            gasEv2 <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
            sendTfr <- mkTransferEvent "sender00" "" 0.0123 "coin" v3Hash
            yieldEv <- mkXYieldEvent "sender00" "sender00" 0.0123 sender00Ks "pact" v3Hash "0" "0"
            assertTxEvents "Events for tx2 @ block 22" [gasEv2,sendTfr, yieldEv] cr
        , PactTxTest buildNewNatives40Cmd $
          assertTxSuccess
          "Should resolve enumerate pact native"
          (pList $ pInteger <$> [1..10])
        , PactTxTest badKeyset $
          assertTxFailure
          "Should not allow bad keys"
          "Invalid keyset"
        ]
      block22_0 =
        [ PactTxTest (buildXReceive xproof) $ \cr -> do
            -- test receive XChain events
            gasEvRcv <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
            rcvTfr <- mkTransferEvent "" "sender00" 0.0123 "coin" v3Hash
            assertTxEvents "Events for txRcv" [gasEvRcv,rcvTfr] cr
        ]

  setPactMempool $ PactMempool
      [ testsToBlock cid block22
      , testsToBlock chain0 block22_0
      ]
  runCut'
  withChain cid $ do
    runBlockTests block22
    cbEv <- mkTransferEvent "" "NoMiner" 2.304523 "coin" v3Hash
    assertTxEvents "Coinbase events @ block 22" [cbEv] =<< cbResult
  withChain chain0 $ runBlockTests block22_0

  -- rewind to savedCut (cut 18)
  rewindTo savedCut
  runToHeight 22

  where

    buildHashCmd = buildBasic
        $ mkExec' "(at 'hash (describe-module 'coin))"

    badKeyset = buildBasic
        $ mkExec "(read-keyset 'ks)" $ object ["ks" .= ["badkey"::T.Text]]

    buildNewNatives40Cmd = buildBasic
        $ mkExec' (mconcat expressions)
      where
        expressions =
          [
            "(distinct [1 1 2 2 3 3])"
          , "(concat [\"this\" \"is\" \"a\" \"test\"])"
          , "(str-to-list \"test\")"
          , "(enumerate 1 10)"
          ]

    buildReleaseCommand = buildBasic'
      (set cbSigners [ mkSigner' sender00 []
                     , mkSigner' allocation00KeyPair []])
      $ mkExec' "(coin.release-allocation 'allocation00)"


-- =========================================================
--
-- Fixture
--
-- =========================================================


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
    go ref bf _ _ _ bh = do
      mps <- readIORef ref
      let mi = MempoolInput bf bh
          runMps i = \case
            [] -> return mempty
            (mp:r) -> case _mempoolBlock mp mi of
              Just bs -> do
                writeIORef ref (take i mps ++ r)
                fmap V.fromList $ forM bs $ \b ->
                  buildCwCmd $ _mempoolCmdBuilder b mi
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

resetMempool :: PactTestM ()
resetMempool = view menvMpa >>= \r -> setMempool r mempty

currentCut :: PactTestM Cut
currentCut = view menvBdb >>= liftIO . readMVar . _bdbCut

rewindTo :: Cut -> PactTestM ()
rewindTo c = view menvBdb >>= \bdb -> void $ liftIO $ swapMVar (_bdbCut bdb) c

assertTxEvents :: (HasCallStack, MonadIO m) => String -> [PactEvent] -> CommandResult Hash -> m ()
assertTxEvents msg evs = liftIO . assertEqual msg evs . _crEvents

assertTxGas :: (HasCallStack, MonadIO m) => String -> Gas -> CommandResult Hash -> m ()
assertTxGas msg g = liftIO . assertEqual msg g . _crGas

assertTxSuccess :: (HasCallStack, MonadIO m) => String -> PactValue -> CommandResult Hash -> m ()
assertTxSuccess msg r tx = do
  liftIO $ assertEqual msg (Just r)
    (tx ^? crResult . to _pactResult . _Right)

assertTxFailure :: (HasCallStack, MonadIO m) => String -> Doc -> CommandResult Hash -> m ()
assertTxFailure msg d tx =
  liftIO $ assertEqual msg (Just d)
    (tx ^? crResult . to _pactResult . _Left . to peDoc)

-- | Run a single mempool block on current chain with tests for each tx.
-- Limitations: can only run a single-chain, single-refill test for
-- a given cut height.
runBlockTest :: [PactTxTest] -> PactTestM ()
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
runBlockTests :: [PactTxTest] -> PactTestM ()
runBlockTests pts = do
  txResults >>= zipWithM_ go pts . V.toList
  where
    go (PactTxTest _ t) cr = liftIO $ t cr

-- | Run cuts to block height.
runToHeight :: BlockHeight -> PactTestM ()
runToHeight bhi = do
  chid <- view menvChainId
  bh <- getHeader chid
  when (_blockHeight bh < bhi) $ do
    runCut'
    runToHeight bhi

buildXSend :: [SigCapability] -> MempoolCmdBuilder
buildXSend caps = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
  set cbSigners [mkSigner' sender00 caps]
  $ setFromHeader bh
  $ mkCmd (sshow bh)
  $ mkExec
    "(coin.transfer-crosschain 'sender00 'sender00 (read-keyset 'k) \"0\" 0.0123)" $
    mkKeySetData "k" [sender00]


chain0 :: ChainId
chain0 = unsafeChainId 0

withChain :: ChainId -> PactTestM a -> PactTestM a
withChain c = local (set menvChainId c)

buildXProof
    :: ChainId
    -> BlockHeight
    -> Int
    -> CommandResult l
    -> PactTestM (ContProof, PactId)
buildXProof scid bh i sendTx = do
    bdb <- view menvBdb
    proof <- liftIO $ ContProof . B64U.encode . encodeToByteString <$>
      createTransactionOutputProof_ (_bdbWebBlockHeaderDb bdb) (_bdbPayloadDb bdb) chain0 scid bh i
    pid <- fromMaybeM (userError "no continuation") $
      preview (crContinuation . _Just . pePactId) sendTx
    return (proof,pid)

buildXReceive
    :: (ContProof, PactId)
    -> MempoolCmdBuilder
buildXReceive (proof,pid) = buildBasic $
    mkCont ((mkContMsg pid 1) { _cmProof = Just proof })

signSender00 :: CmdBuilder -> CmdBuilder
signSender00 = set cbSigners [mkSigner' sender00 []]

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
buildBasic' s r = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
  s $ signSender00
  $ setFromHeader bh
  $ mkCmd (sshow bh) r


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

-- | Get tx at index from output
txResult :: HasCallStack => Int -> PactTestM (CommandResult Hash)
txResult i = txResults >>= \rs -> case preview (ix i) rs of
    Nothing ->
      liftIO $ assertFailure $ "no tx at " ++ show i
    Just r -> return r

txResults :: HasCallStack => PactTestM (V.Vector (CommandResult Hash))
txResults = do
  chid <- view menvChainId
  (o,_h) <- getPWO chid
  forM (_payloadWithOutputsTransactions o) $ \(_,txo) ->
    decodeStrictOrThrow @_ @(CommandResult Hash) (_transactionOutputBytes txo)

-- | Get coinbase from output
cbResult :: PactTestM (CommandResult Hash)
cbResult = do
  chid <- view menvChainId
  (o,_h) <- getPWO chid
  liftIO $
    decodeStrictOrThrow @_ @(CommandResult Hash) (_coinbaseOutput $ _payloadWithOutputsCoinbase o)
