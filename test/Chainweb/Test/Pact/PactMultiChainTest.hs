{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Data.Aeson (Value, object, (.=))
import Data.List(isPrefixOf)
import qualified Data.ByteString.Base64.URL as B64U
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
import Pact.Types.Lang(_LString)

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
import Chainweb.Pact.Types (TxTimeout(..))
import Chainweb.Payload
import Chainweb.SPV.CreateProof
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
    , _menvPacts :: !(HM.HashMap ChainId PactExecutionService)
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
  [ test generousConfig freeGasModel "pact4coin3UpgradeTest" pact4coin3UpgradeTest
  , test generousConfig freeGasModel "pact42UpgradeTest" pact42UpgradeTest
  , test generousConfig freeGasModel "minerKeysetTest" minerKeysetTest
  , test timeoutConfig freeGasModel "txTimeoutTest" txTimeoutTest
  , test generousConfig getGasModel "chainweb213Test" chainweb213Test
  , test generousConfig getGasModel "pact43UpgradeTest" pact43UpgradeTest
  , test generousConfig getGasModel "pact431UpgradeTest" pact431UpgradeTest
  , test generousConfig getGasModel "chainweb215Test" chainweb215Test
  , test generousConfig getGasModel "chainweb216Test" chainweb216Test
  , test generousConfig getGasModel "pact45UpgradeTest" pact45UpgradeTest
  , test generousConfig getGasModel "pact46UpgradeTest" pact46UpgradeTest
  , test generousConfig getGasModel "chainweb219UpgradeTest" chainweb219UpgradeTest
  , test generousConfig getGasModel "pactLocalDepthTest" pactLocalDepthTest
  , test generousConfig getGasModel "pact48UpgradeTest" pact48UpgradeTest
  , test generousConfig getGasModel "pact49UpgradeTest" pact49UpgradeTest
  , test generousConfig getGasModel "pact410UpgradeTest" pact410UpgradeTest
  ]
  where
    testName = "Chainweb.Test.Pact.PactMultiChainTest"
    -- This is way more than what is used in production, but during testing
    -- we can be generous.
    generousConfig = testPactServiceConfig { _pactBlockGasLimit = 300_000 }
    timeoutConfig = testPactServiceConfig { _pactBlockGasLimit = 100_000 }

    test pactConfig gasmodel tname f =
      withDelegateMempool $ \dmpio -> testCaseSteps tname $ \step ->
        withTestBlockDb testVersion $ \bdb -> do
          (iompa,mpa) <- dmpio
          let logger = hunitDummyLogger step
          withWebPactExecutionService logger testVersion pactConfig bdb mpa gasmodel $ \(pact,pacts) ->
            runReaderT f $
            MultiEnv bdb pact pacts (return iompa) noMiner cid

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

txTimeoutTest :: PactTestM ()
txTimeoutTest = do
  -- get access to `enumerate`
  runToHeight 20
  handle (\(TxTimeout _) -> return ()) $ do
    runBlockTest
      -- deliberately time out in newblock
      [PactTxTest (buildBasicGas 1000 $ mkExec' "(enumerate 0 999999999999)") (\_ -> assertFailure "tx succeeded")]
    liftIO $ assertFailure "block succeeded"
  runToHeight 26

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

pactLocalDepthTest :: PactTestM ()
pactLocalDepthTest = do
  runToHeight 53
  runBlockTest
    [ PactTxTest (buildCoinXfer "(coin.transfer 'sender00 'sender01 1.0)") $
        assertTxGas "Coin transfer pre-fork" 1583
    ]
  runBlockTest
    [ PactTxTest (buildCoinXfer "(coin.transfer 'sender00 'sender01 1.0)") $
        assertTxGas "Coin post-fork" 1583
    ]

  runLocalWithDepth (Just $ RewindDepth 0) cid getSender00Balance >>= \r ->
    checkLocalResult r $ assertTxSuccess "Should get the current balance" (pDecimal 99999997.6834)

  -- checking that `Just $ RewindDepth 0` has the same behaviour as `Nothing`
  runLocalWithDepth Nothing cid getSender00Balance >>= \r ->
    checkLocalResult r $ assertTxSuccess "Should get the current balance as well" (pDecimal 99999997.6834)

  runLocalWithDepth (Just $ RewindDepth 1) cid getSender00Balance >>= \r ->
    checkLocalResult r $ assertTxSuccess "Should get the balance one block before" (pDecimal 99999998.8417)

  runLocalWithDepth (Just $ RewindDepth 2) cid getSender00Balance >>= \r ->
    checkLocalResult r $ assertTxSuccess "Should get the balance two blocks before" (pDecimal 100000000)

  -- the negative depth turns into 18446744073709551611 and we expect the `LocalRewindLimitExceeded` exception
  -- since `Depth` is a wrapper around `Word64`
  handle
    (\case
      LocalRewindLimitExceeded _ _ -> return ()
      err -> liftIO $ assertFailure $ "Expected LocalRewindLimitExceeded, but got " ++ show err)
    (do
      runLocalWithDepth (Just $ RewindDepth (fromIntegral (-5 :: Int))) cid getSender00Balance >>= \_ ->
        liftIO $ assertFailure "Expected LocalRewindLimitExceeded, but block succeeded")

  -- the genesis depth
  runLocalWithDepth (Just $ RewindDepth 55) cid getSender00Balance >>= \r ->
    checkLocalResult r $ assertTxSuccess "Should get the balance at the genesis block" (pDecimal 100000000)

  -- depth that goes after the genesis block should trigger the `LocalRewindLimitExceeded` exception
  handle
    (\case
      LocalRewindGenesisExceeded -> return ()
      err -> liftIO $ assertFailure $ "Expected LocalRewindGenesisExceeded, but got " ++ show err)
    (do
      runLocalWithDepth (Just $ RewindDepth 56) cid getSender00Balance >>= \_ ->
        liftIO $ assertFailure "Expected LocalRewindGenesisExceeded, but block succeeded")

  where
  checkLocalResult r checkResult = case r of
    Right (LocalResultLegacy cr) -> checkResult cr
    res -> liftIO $ assertFailure $ "Expected LocalResultLegacy, but got: " ++ show res
  getSender00Balance = set cbGasLimit 700 $ mkCmd "nonce" $ mkExec' "(coin.get-balance \"sender00\")"
  buildCoinXfer code = buildBasic'
    (set cbSigners [mkEd25519Signer' sender00 coinCaps] . set cbGasLimit 3000)
    $ mkExec' code
    where
    coinCaps = [ mkGasCap, mkTransferCap "sender00" "sender01" 1.0 ]

pact45UpgradeTest :: PactTestM ()
pact45UpgradeTest = do
  runToHeight 53 -- 2 before fork
  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000 $ tblModule "tbl") $
      assertTxSuccess "mod53 table created" $ pString "TableCreated"
    ]
  runBlockTest
    [ PactTxTest (buildSimpleCmd "(enforce false 'hi)") $
        assertTxFailure "Should fail with the error from the enforce" "hi"
    , PactTxTest (buildSimpleCmd "(enforce true (format  \"{}-{}\" [12345, 657859]))") $
        assertTxGas "Enforce pre-fork evaluates the string with gas" 35
    , PactTxTest (buildSimpleCmd "(enumerate 0 10) (str-to-list 'hi) (make-list 10 'hi)") $
        assertTxGas "List functions pre-fork gas" 20
    , PactTxTest
      (buildBasicGas 70000 $ tblModule "Tbl") $
      assertTxSuccess "mod53 table update succeeds" $ pString "TableCreated"
    , PactTxTest (buildCoinXfer "(coin.transfer 'sender00 'sender01 1.0)") $
        assertTxGas "Coin transfer pre-fork" 1583
    ]
  -- chainweb217 fork
  runBlockTest
    [ PactTxTest (buildSimpleCmd "(+ 1 \'clearlyanerror)") $
      assertTxFailure "Should replace tx error with empty error" ""
    , PactTxTest (buildSimpleCmd "(enforce true (format  \"{}-{}\" [12345, 657859]))") $
        assertTxGas "Enforce post fork does not eval the string" (15 + coinTxBuyTransferGas)
    , PactTxTest (buildSimpleCmd "(enumerate 0 10) (str-to-list 'hi) (make-list 10 'hi)") $
        assertTxGas "List functions post-fork change gas" (40 + coinTxBuyTransferGas)
    , PactTxTest
      (buildBasicGas 70000 $ tblModule "tBl") $
      assertTxFailure "mod53 table update fails after fork" ""
    , PactTxTest (buildCoinXfer "(coin.transfer 'sender00 'sender01 1.0)") $
        assertTxGas "Coin post-fork" 709
    ]
  -- run local to check error
  lr <- runLocal cid $ set cbGasLimit 70000 $ mkCmd "nonce" (tblModule "tBl")
  assertLocalFailure "mod53 table update error"
    (pretty $ show $ PactDuplicateTableError "free.mod53_tBl")
    lr

  where
  tblModule tn = mkExec' $ T.replace "$TABLE$" tn
      " (namespace 'free) \
      \ (module mod53 G \
      \   (defcap G () true) \
      \   (defschema sch s:string) \
      \   (deftable $TABLE$:{sch})) \
      \ (create-table $TABLE$)"
  buildCoinXfer code = buildBasic'
    (set cbSigners [mkEd25519Signer' sender00 coinCaps] . set cbGasLimit 3000)
    $ mkExec' code
    where
    coinCaps = [ mkGasCap, mkTransferCap "sender00" "sender01" 1.0 ]
  coinTxBuyTransferGas = 216
  buildSimpleCmd code = buildBasicGas 3000
      $ mkExec' code

runLocal :: ChainId -> CmdBuilder -> PactTestM (Either PactException LocalResult)
runLocal cid' cmd = runLocalWithDepth Nothing cid' cmd

runLocalWithDepth :: Maybe RewindDepth -> ChainId -> CmdBuilder -> PactTestM (Either PactException LocalResult)
runLocalWithDepth depth cid' cmd = do
  pact <- getPactService cid'
  cwCmd <- buildCwCmd testVersion cmd
  liftIO $ _pactLocal pact Nothing Nothing depth cwCmd

getPactService :: ChainId -> PactTestM PactExecutionService
getPactService cid' = do
  HM.lookup cid' <$> view menvPacts >>= \case
    Just pact -> return pact
    Nothing -> liftIO $ assertFailure $ "No pact service found at chain id " ++ show cid'

assertLocalFailure
    :: (HasCallStack, MonadIO m)
    => String
    -> Doc
    -> Either PactException LocalResult
    -> m ()
assertLocalFailure s d lr =
  liftIO $ assertEqual s (Just d) $
    lr ^? _Right . _LocalResultLegacy . crResult . to _pactResult . _Left . to peDoc

assertLocalSuccess
    :: (HasCallStack, MonadIO m)
    => String
    -> PactValue
    -> Either PactException LocalResult
    -> m ()
assertLocalSuccess s pv lr =
  liftIO $ assertEqual s (Just pv) $
    lr ^? _Right . _LocalResultLegacy . crResult . to _pactResult . _Right

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
      , mkTransferXChainRecdEvent "" "sender00" 0.0123 "coin" h (toText cid)
      , mkXResumeEvent "sender00" "sender00" 0.0123 sender00Ks "pact" h' (toText cid) "0"
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


pact42UpgradeTest :: PactTestM ()
pact42UpgradeTest = do

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
        assertTxGas "Pre-fork try" 19
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
        assertTxGas "Post-fork try should charge a bit more gas" 20
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


pact46UpgradeTest :: PactTestM ()
pact46UpgradeTest = do

  -- run past genesis, upgrades
  runToHeight 58

  -- Note: no error messages on-chain, so the error message is empty
  runBlockTest
      [ PactTxTest pointAddTx $
        assertTxFailure
        "Should not resolve new pact native: point-add"
        ""
      , PactTxTest scalarMulTx $
        assertTxFailure
        "Should not resolve new pact native: scalar-mult"
        ""
      , PactTxTest pairingTx $
        assertTxFailure
        "Should not resolve new pact native: pairing-check"
        ""
      ]

  runBlockTest
      [ PactTxTest pointAddTx $
        assertTxSuccess
        "Should resolve point-add properly post-fork"
        (pObject [("x", pInteger  1368015179489954701390400359078579693043519447331113978918064868415326638035)
        , ("y", pInteger 9918110051302171585080402603319702774565515993150576347155970296011118125764)])
      , PactTxTest scalarMulTx $
        assertTxSuccess
         "Should resolve scalar-mult properly post-fork"
        (pObject [("x", pInteger 1)
        , ("y", pInteger 2)])
      , PactTxTest pairingTx $
        assertTxSuccess
         "Should resolve scalar-mult properly post-fork"
        (pBool True)
      ]
  where
    pointAddTx = buildBasicGas 10000
        $ mkExec' (mconcat
        [ "(point-add 'g1 {'x:1, 'y:2} {'x:1, 'y:2})"
        ])
    scalarMulTx = buildBasicGas 10000
        $ mkExec' (mconcat
        [ "(scalar-mult 'g1 {'x: 1, 'y: 2} 1)"
        ])
    pairingTx = buildBasicGas 30000
        $ mkExec' (mconcat
        [ "(pairing-check [{'x: 1, 'y: 2}] [{'x:[0, 0], 'y:[0, 0]}])"
        ])


chainweb219UpgradeTest :: PactTestM ()
chainweb219UpgradeTest = do

  -- run past genesis, upgrades
  runToHeight 69

  -- Block 70, pre-fork, no errors on chain
  runBlockTest
      [ PactTxTest addErrTx $
        assertTxFailure
        "Should fail on + with incorrect argument types"
        ""
      , PactTxTest mapErrTx $
        assertTxFailure
        "Should fail on map with the second argument not being a list"
        ""
      , PactTxTest nativeDetailsTx $
        assertTxSuccessWith
        "Should print native details on chain"
        (pString nativeDetailsMsg)
        (over (_PLiteral . _LString) (T.take (T.length nativeDetailsMsg)))
      , PactTxTest fnDetailsTx $
        assertTxSuccessWith
        "Should display function preview string"
        (pString fnDetailsMsg)
        (over (_PLiteral . _LString) (T.take (T.length fnDetailsMsg)))
      , PactTxTest decTx $
        assertTxFailure
        "Should not resolve new pact native: dec"
        -- Should be cannot resolve dec, but no errors pre-fork.
        ""
      , PactTxTest runIllTypedFunction $
        assertTxSuccess
        "User function return value types should not be checked before the fork"
        (pDecimal 1.0)
      , PactTxTest tryReadString $
        assertTxFailure
        "read-* errors are not recoverable before the fork"
        ""
      , PactTxTest tryReadInteger $
        assertTxFailure
        "read-* errors are not recoverable before the fork"
        ""
      , PactTxTest tryReadKeyset $
        assertTxFailure
        "read-* errors are not recoverable before the fork"
        ""
      , PactTxTest tryReadMsg $
        assertTxFailure
        "read-* errors are not recoverable before the fork"
        ""
      ]

  -- Block 71, post-fork, errors should return on-chain but different
  runBlockTest
      [ PactTxTest addErrTx $
        assertTxFailureWith
        "Should error with the argument types in +"
        (isPrefixOf "Invalid arguments in call to +, received arguments of type" . show)
      , PactTxTest mapErrTx $
        assertTxFailure
        "Should fail on map with the second argument not being a list"
        "map: expecting list, received argument of type: string"
      , PactTxTest nativeDetailsTx $
        assertTxFailure
        "Should print native details on chain"
        "Cannot display native function details in non-repl context"
      , PactTxTest fnDetailsTx $
        assertTxFailure
        "Should not display function preview string"
        "Cannot display function details in non-repl context"
      , PactTxTest decTx $
        assertTxSuccess
        "Should resolve new pact native: dec"
        (pDecimal 1)
      , PactTxTest runIllTypedFunction $
        assertTxSuccess
        "User function return value types should not be checked after the fork"
        (pDecimal 1.0)
      , PactTxTest tryReadString $
        assertTxSuccess
        "read-* errors are recoverable after the fork"
        (pDecimal 1.0)
      , PactTxTest tryReadInteger $
        assertTxSuccess
        "read-* errors are recoverable after the fork"
        (pDecimal 1.0)
      , PactTxTest tryReadKeyset $
        assertTxSuccess
        "read-* errors are recoverable after the fork"
        (pDecimal 1.0)
      , PactTxTest tryReadMsg $
        assertTxSuccess
        "read-* errors are recoverable after the fork"
        (pDecimal 1.0)
      ]

  -- run local on RTC check
  lr <- runLocal cid $ set cbGasLimit 70000 $ mkCmd "nonce" runIllTypedFunctionExec
  assertLocalSuccess
    "User function return value types should not be checked in local"
    (pDecimal 1.0)
    lr

  where
    addErrTx = buildBasicGas 10000
        $ mkExec' (mconcat
        [ "(+ 1 \"a\")"
        ])
    mapErrTx = buildBasicGas 10000
        $ mkExec' (mconcat
        [ "(map (+ 1) \"a\")"
        ])
    decTx = buildBasicGas 10000
       $ mkExec' "(dec 1)"
    nativeDetailsMsg = "native `=`  Compare alike terms for equality"
    nativeDetailsTx = buildBasicGas 10000
        $ mkExec' (mconcat
        [ "="
        ])
    fnDetailsMsg = "(defun coin.transfer:string"
    fnDetailsTx = buildBasicGas 10000
        $ mkExec' (mconcat
        [ "coin.transfer"
        ])
    runIllTypedFunction = buildBasicGas 70000 runIllTypedFunctionExec
    runIllTypedFunctionExec = mkExec' (mconcat
                  [ "(namespace 'free)"
                  , "(module m g (defcap g () true)"
                  , "  (defun foo:string () 1))"
                  , "(m.foo)"
                  ])
    tryReadInteger = buildBasicGas 1000
        $ mkExec' "(try 1 (read-integer \"somekey\"))"
    tryReadString = buildBasicGas 1000
        $ mkExec' "(try 1 (read-string \"somekey\"))"
    tryReadKeyset = buildBasicGas 1000
        $ mkExec' "(try 1 (read-keyset \"somekey\"))"
    tryReadMsg = buildBasicGas 1000
        $ mkExec' "(try 1 (read-msg \"somekey\"))"

pact48UpgradeTest :: PactTestM ()
pact48UpgradeTest = do
  runToHeight 83

  -- run block 84 (before the pact48 fork)
  runBlockTest
    [ PactTxTest runConcat $ assertTxGas "Old concat gas cost" 231
    , PactTxTest runFormat $ assertTxGas "Old format gas cost" 238
    , PactTxTest runReverse $ assertTxGas "Old reverse gas cost" 4232
    ]

  -- run block 85 (after the pact 48 fork)
  runBlockTest
    [ PactTxTest runConcat $ assertTxGas "New concat gas cost" 280
    , PactTxTest runFormat $ assertTxGas "New format gas cost" 233
    , PactTxTest runReverse $ assertTxGas "New reverse gas cost" 4272
    ]

  where
    runConcat = buildBasicGas 10000 $ mkExec' "(concat [\"hello\", \"world\"])"
    runFormat = buildBasicGas 10000 $ mkExec' "(format \"{}\" [1,2,3])"
    runReverse = buildBasicGas 10000 $ mkExec' "(reverse (enumerate 1 4000))"

pact49UpgradeTest :: PactTestM ()
pact49UpgradeTest = do
  runToHeight 97

  -- Run block 98.
  -- WebAuthn is not yet a valid PPK scheme, so this transaction
  -- is not valid for insertion into the mempool.
  expectInvalid
    "WebAuthn should not yet be supported"
    [ webAuthnSignedTransaction
    ]

  -- run block 99 (before the pact-4.9 fork)
  runBlockTest
    [ PactTxTest base64DecodeNonCanonical $
        assertTxSuccess
        "Non-canonical messages decode before pact-4.9"
        (pString "d")
    , PactTxTest base64DecodeBadPadding $ assertTxFailure "decoding illegally padded string" "Could not decode string: Base64URL decode failed: invalid padding near offset 16"
    ]

  -- run block 100 (after the pact-4.9 fork)
  runBlockTest
    [ PactTxTest base64DecodeNonCanonical $
        assertTxFailure "decoding non-canonical message" "Could not decode string: Could not base64-decode string"
    , PactTxTest base64DecodeBadPadding $ assertTxFailure "decoding illegally padded string" "Could not decode string: Could not base64-decode string"

    , PactTxTest webAuthnSignedTransaction $
      assertTxSuccess
      "WebAuthn signatures should be valid now"
      (pDecimal 3)

    ]

  where
    webAuthnSignedTransaction = buildBasicGasWebAuthnBareSigner 1000 $ mkExec' "(+ 1 2)"
    base64DecodeNonCanonical = buildBasicGas 10000 $ mkExec' "(base64-decode \"ZE==\")"
    base64DecodeBadPadding = buildBasicGas 10000 $ mkExec' "(base64-decode \"aGVsbG8gd29ybGQh%\")"

pact410UpgradeTest :: PactTestM ()
pact410UpgradeTest = do
  runToHeight 110

  expectInvalid
    "WebAuthn prefixed keys should not yet be supported in signatures"
    [ prefixedSigned
    ]
  runBlockTest [
    PactTxTest poseidonTx $
      assertTxFailure "Should not resolve new pact native: poseidon-hash-hack-a-chain"
      "Cannot resolve poseidon-hash-hack-a-chain"
    ]

  runToHeight 120
  runBlockTest
    [ PactTxTest prefixedSigned $
      assertTxSuccess
      "Prefixed WebAuthn signers should be legal"
      (pDecimal 1)

    , PactTxTest prefixedSignerPrefixedKey $
      assertTxSuccess
      "WebAuthn prefixed keys should be enforceable with prefixed signers"
      (pBool True)

    , PactTxTest bareSignerPrefixedKey $
      assertTxFailure
      "WebAuthn prefixed keys should not be enforceable with bare signers"
      "Keyset failure (keys-all): [WEBAUTHN...]"

    , PactTxTest definePrefixedKeySet $
      assertTxSuccess
      "WebAuthn prefixed keys should be enforceable after defining them"
      (pBool True)

    , PactTxTest prefixedSignerPrefixedKeyCreatePrincipal $
      assertTxSuccess
      "WebAuthn prefixed keys in a keyset should be possible to make into w: principals"
      (pString "w:XrscJ2X8aFxFF7oilzFyjQuA1mUN8jgwdxbAd8rt21M:keys-all")

    , PactTxTest prefixedSignerPrefixedKeyValidatePrincipal $
      assertTxSuccess
      "WebAuthn prefixed keys in a keyset should be possible to make into *valid* w: principals"
      (pBool True)

    , PactTxTest prefixedSignerBareKey $
      assertTxFailure
      "WebAuthn bare keys should throw an error when read"
      "Invalid keyset"

    , PactTxTest invalidPrefixedKey $
      assertTxFailure
      "Invalid WebAuthn prefixed keys should throw an error when read"
      "Invalid keyset"

    ,  PactTxTest poseidonTx $
      assertTxSuccess "Should resolve new pact native: poseidon-hash-hack-a-chain"
      (pDecimal 18586133768512220936620570745912940619677854269274689475585506675881198879027)
    ]

  where
    poseidonTx = buildBasic $ mkExec' "(poseidon-hash-hack-a-chain 1)"
    prefixedSigned = buildBasicGasWebAuthnPrefixedSigner 1000 $ mkExec' "1"

    prefixedSignerBareKey = buildBasicGasWebAuthnPrefixedSigner 1000 $ mkExec
      "(enforce-keyset (read-keyset 'k))"
      (mkKeyEnvData "a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf")

    bareSignerPrefixedKey = buildBasicGasWebAuthnBareSigner 1000 $ mkExec
      "(enforce-keyset (read-keyset 'k))"
      (mkKeyEnvData "WEBAUTHN-a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf")

    prefixedSignerPrefixedKey = buildBasicGasWebAuthnPrefixedSigner 1000 $ mkExec
      "(enforce-keyset (read-keyset 'k))"
      (mkKeyEnvData "WEBAUTHN-a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf")

    definePrefixedKeySet = buildBasicGasWebAuthnPrefixedSigner 1000 $ mkExec
      "(namespace 'free) (define-keyset \"free.edmund\" (read-keyset 'k)) (enforce-keyset \"free.edmund\")"
      (mkKeyEnvData "WEBAUTHN-a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf")

    prefixedSignerPrefixedKeyCreatePrincipal = buildBasicGasWebAuthnPrefixedSigner 1000 $ mkExec
      "(create-principal (read-keyset 'k))"
      (mkKeyEnvData "WEBAUTHN-a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf")

    prefixedSignerPrefixedKeyValidatePrincipal = buildBasicGasWebAuthnPrefixedSigner 1000 $ mkExec
      "(let ((ks (read-keyset 'k))) (validate-principal ks (create-principal ks)))"
      (mkKeyEnvData "WEBAUTHN-a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf")

    -- This hardcoded public key is the same as the valid one above, except that the first
    -- character is changed. CBOR parsing will fail.
    invalidPrefixedKey = buildBasicGas 1000 $ mkExec
      "(read-keyset 'k)"
      (mkKeyEnvData "WEBAUTHN-a401010327200add79710303bf")

    mkKeyEnvData :: String -> Value
    mkKeyEnvData key = object [ "k" .= [key] ]


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
  cont <- buildCont send0

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
            gasEv2 <- mkTransferEvent "sender00" "NoMiner" 0.0015 "coin" v3Hash
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
        , PactTxTest cont $
          assertTxFailure'
          "Attempt to continue xchain on same chain fails"
          "yield provenance"
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
      (set cbSigners [ mkEd25519Signer' sender00 []
                     , mkEd25519Signer' allocation00KeyPair []])
      $ mkExec' "(coin.release-allocation 'allocation00)"

    buildCont sendTx = do
      pid <- getPactId sendTx
      return $ buildBasic $ mkCont (mkContMsg pid 1)


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
    go ref bf mempoolPreBlockCheck bHeight bHash blockHeader = do
      mps <- readIORef ref
      let mi = MempoolInput bf blockHeader
          runMps i = \case
            [] -> return mempty
            (mp:r) -> case _mempoolBlock mp mi of
              Just bs -> do
                writeIORef ref (take i mps ++ r)
                cmds <- fmap V.fromList $ forM bs $ \b ->
                  buildCwCmd testVersion $ _mempoolCmdBuilder b mi
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

assertTxSuccessWith
  :: (HasCallStack, MonadIO m)
  => String
  -> PactValue
  -> (PactValue -> PactValue)
  -> CommandResult Hash
  -> m ()
assertTxSuccessWith msg r f tx = do
  liftIO $ assertEqual msg (Just r)
    (tx ^? crResult . to _pactResult . _Right . to f)

-- | Exact match on error doc
assertTxFailure :: (HasCallStack, MonadIO m) => String -> Doc -> CommandResult Hash -> m ()
assertTxFailure msg d tx =
  liftIO $ assertEqual msg (Just d)
    (tx ^? crResult . to _pactResult . _Left . to peDoc)

assertTxFailureWith
  :: (HasCallStack, MonadIO m)
  => String
  -> (Doc -> Bool)
  -> CommandResult Hash -> m ()
assertTxFailureWith msg f tx = do
  let mresult = tx ^? crResult . to _pactResult . _Left . to peDoc
  liftIO $ assertBool (msg <> ". Tx Result: " <>  show mresult) $ maybe False f mresult

-- | Partial match on show of error doc
assertTxFailure' :: (HasCallStack, MonadIO m) => String -> T.Text -> CommandResult Hash -> m ()
assertTxFailure' msg needle tx =
  liftIO $ assertSatisfies msg
    (tx ^? crResult . to _pactResult . _Left . to peDoc) $ \case
      Nothing -> False
      Just d -> T.isInfixOf needle (sshow d)


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

-- | No tests in this list should even be submitted to the mempool,
-- they should be rejected early.
expectInvalid :: String -> [MempoolCmdBuilder] -> PactTestM ()
expectInvalid msg pts = do
  chid <- view menvChainId
  setPactMempool $ PactMempool [blockForChain chid $ MempoolBlock $ \_ -> pure pts]
  _ <- runCut'
  rs <- txResults
  liftIO $ assertEqual msg mempty rs

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

buildXSend :: [SigCapability] -> MempoolCmdBuilder
buildXSend caps = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
  set cbSigners [mkEd25519Signer' sender00 caps]
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
    pid <- getPactId sendTx
    return (proof,pid)

getPactId :: CommandResult l -> PactTestM PactId
getPactId = fromMaybeM (userError "no continuation") .
            preview (crContinuation . _Just . pePactId)

buildXReceive
    :: (ContProof, PactId)
    -> MempoolCmdBuilder
buildXReceive (proof,pid) = buildBasic $
    mkCont ((mkContMsg pid 1) { _cmProof = Just proof })

signWebAuthn00Prefixed :: CmdBuilder -> CmdBuilder
signWebAuthn00Prefixed =
  set cbSigners [mkWebAuthnSigner' sender02WebAuthnPrefixed []
                ,mkEd25519Signer' sender00 []
                ]

signWebAuthn00 :: CmdBuilder -> CmdBuilder
signWebAuthn00 =
  set cbSigners [mkWebAuthnSigner' sender02WebAuthn []
                ,mkEd25519Signer' sender00 []
                ]

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
  $ mkCmd (sshow bh) r

buildBasicWebAuthnBareSigner'
    :: (CmdBuilder -> CmdBuilder)
    -> PactRPC T.Text
    -> MempoolCmdBuilder
buildBasicWebAuthnBareSigner' f r = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
  f $ signWebAuthn00
  $ setFromHeader bh
  $ mkCmd (sshow bh) r

buildBasicWebAuthnPrefixedSigner'
    :: (CmdBuilder -> CmdBuilder)
    -> PactRPC T.Text
    -> MempoolCmdBuilder
buildBasicWebAuthnPrefixedSigner' f r = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
  f $ signWebAuthn00Prefixed
  $ setFromHeader bh
  $ mkCmd (sshow bh) r

buildBasicGasWebAuthnPrefixedSigner :: GasLimit -> PactRPC T.Text -> MempoolCmdBuilder
buildBasicGasWebAuthnPrefixedSigner g = buildBasicWebAuthnPrefixedSigner' (set cbGasLimit g)

buildBasicGasWebAuthnBareSigner :: GasLimit -> PactRPC T.Text -> MempoolCmdBuilder
buildBasicGasWebAuthnBareSigner g = buildBasicWebAuthnBareSigner' (set cbGasLimit g)

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
