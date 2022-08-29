{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Test.PactInProcApi
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via (inprocess) API in Chainweb
--
module Chainweb.Test.Pact.PactInProcApi
( tests
) where

import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader


import Data.Aeson (object, (.=), Value(..), decode)
import qualified Data.ByteString.Base64.URL as B64U
import Data.CAS (casLookupM)
import Data.CAS.RocksDB
import Data.Default (def)
import Data.Either (isRight)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HU


-- internal modules

import Pact.Types.Continuation
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.PactError
import Pact.Types.Pretty
import Pact.Types.Runtime (PactEvent)
import Pact.Types.RPC
import Pact.Types.SPV
import Pact.Types.Term

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation hiding (local)
import Chainweb.Pact.Service.PactQueue (PactQueue)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact.TransactionExec (listErrMsg)
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.SPV.CreateProof
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebPactExecutionService
import Pact.Types.Capability

testVersion :: ChainwebVersion
testVersion = FastTimedCPM peterson

cid :: ChainId
cid = someChainId testVersion

genesisHeader :: BlockHeader
genesisHeader = genesisBlockHeader testVersion cid

data MultiEnv = MultiEnv
    { _menvBdb :: TestBlockDb
    , _menvPact :: WebPactExecutionService
    , _menvMpa :: IO (IORef MemPoolAccess)
    , _menvMiner :: Miner
    , _menvChainId :: ChainId
    }

makeLenses ''MultiEnv

_unused :: Lens' MultiEnv WebPactExecutionService
_unused = menvPact

type MultiM = ReaderT MultiEnv IO


type MultichainTest = IO (IORef MemPoolAccess) -> MultiM ()

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

-- | Sets mempool with block fillers. A matched filler
-- (returning a 'Just' result) is executed and removed from the list.
-- Fillers are tested in order.
setPactMempool :: PactMempool -> MultiM ()
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

-- | Pair a builder with a test
data PactTxTest = PactTxTest
    { _pttBuilder :: MempoolCmdBuilder
    , _pttTest :: CommandResult Hash -> Assertion
    }

-- | MonadIO friendly
assertEqual
    :: ( HasCallStack
       , MonadIO m
       , Eq a
       , Show a )
    => String
    -> a
    -> a
    -> m ()
assertEqual message intended actual = liftIO $ HU.assertEqual message intended actual


runCut' :: MultiM ()
runCut' = ask >>= \MultiEnv{..} ->
  liftIO $ runCut testVersion _menvBdb _menvPact (offsetBlockTime second) zeroNoncer _menvMiner


tests :: RocksDb -> ScheduledTest
tests rdb = ScheduledTest testName go
  where
    testName = "Chainweb.Test.Pact.PactInProcApi"
    go = testGroup testName
         [ test Warn $ goldenNewBlock "new-block-0" goldenMemPool
         , test Warn $ goldenNewBlock "empty-block-tests" mempty
         , test Warn $ newBlockAndValidate
         , test Warn $ newBlockRewindValidate
         , test Quiet $ getHistory
         , test Quiet $ testHistLookup1
         , test Quiet $ testHistLookup2
         , test Quiet $ testHistLookup3
         , test Quiet $ badlistNewBlockTest
         , test Warn $ mempoolCreationTimeTest
         , test Warn $ moduleNameFork
         , test Warn $ mempoolRefillTest
         , test Quiet $ blockGasLimitTest
         , multiChainTest freeGasModel "pact4coin3UpgradeTest" pact4coin3UpgradeTest
         , multiChainTest freeGasModel "pact420UpgradeTest" pact420UpgradeTest
         , multiChainTest freeGasModel "minerKeysetTest" minerKeysetTest
         , multiChainTest getGasModel "chainweb213Test" chainweb213Test
         , multiChainTest getGasModel "pact43UpgradeTest" pact43UpgradeTest
         , multiChainTest getGasModel "pact431UpgradeTest" pact431UpgradeTest
         , multiChainTest getGasModel "chainweb215Test" chainweb215Test
         , multiChainTest getGasModel "chainweb216Test" chainweb216Test
         ]
      where
        test logLevel f =
          withDelegateMempool $ \dm ->
          withPactTestBlockDb testVersion cid logLevel rdb (snd <$> dm) defaultPactServiceConfig $
          f (fst <$> dm)

        multiChainTest gasmodel tname f =
          withDelegateMempool $ \dmpio -> testCase tname $
            withTestBlockDb testVersion $ \bdb -> do
              (iompa,mpa) <- dmpio
              withWebPactExecutionService testVersion bdb mpa gasmodel $ \pact ->
                runReaderT (f (return iompa)) $
                MultiEnv bdb pact (return iompa) noMiner cid
        testHistLookup1 = getHistoricalLookupNoTxs "sender00"
          (assertSender00Bal 100_000_000 "check latest entry for sender00 after a no txs block")
        testHistLookup2 = getHistoricalLookupNoTxs "randomAccount"
          (assertEqual "Return Nothing if key absent after a no txs block" Nothing)
        testHistLookup3 = getHistoricalLookupWithTxs "sender00"
          (assertSender00Bal 9.999998051e7 "check latest entry for sender00 after block with txs")


forSuccess :: NFData a => String -> IO (MVar (Either PactException a)) -> IO a
forSuccess msg mvio = (`catchAllSynchronous` handler) $ do
  mv <- mvio
  takeMVar mv >>= \case
    Left e -> assertFailure $ msg ++ ": got failure result: " ++ show e
    Right v -> return v
  where
    handler e = assertFailure $ msg ++ ": exception thrown: " ++ show e

runBlock :: PactQueue -> TestBlockDb -> TimeSpan Micros -> String -> IO PayloadWithOutputs
runBlock q bdb timeOffset msg = do
  ph <- getParentTestBlockDb bdb cid
  let blockTime = add timeOffset $ _bct $ _blockCreationTime ph
  nb <- forSuccess (msg <> ": newblock") $
        newBlock noMiner (ParentHeader ph) q
  forM_ (chainIds testVersion) $ \c -> do
    let o | c == cid = nb
          | otherwise = emptyPayload
    addTestBlockDb bdb (Nonce 0) (\_ _ -> blockTime) c o
  nextH <- getParentTestBlockDb bdb cid
  forSuccess "newBlockAndValidate: validate" $
       validateBlock nextH (payloadWithOutputsToPayloadData nb) q

newBlockAndValidate :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockAndValidate refIO reqIO = testCase "newBlockAndValidate" $ do
  (q,bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool
  void $ runBlock q bdb second "newBlockAndValidate"


getHistory :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
getHistory refIO reqIO = testCase "getHistory" $ do
  (q,bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool
  void $ runBlock q bdb second "getHistory"
  h <- getParentTestBlockDb bdb cid
  mv <- pactBlockTxHistory h (Domain' (UserTables "coin_coin-table")) q

  (BlockTxHistory hist prevBals) <- forSuccess "getHistory" (return mv)
  -- just check first one here
  assertEqual "check first entry of history"
    (Just [TxLog "coin_coin-table" "sender00"
      (object
       [ "guard" .= object
         [ "pred" .= ("keys-all" :: T.Text)
         , "keys" .=
           ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text]
         ]
       , "balance" .= (Number 99999900.0)
       ])])
    (M.lookup 10 hist)
  -- and transaction txids
  assertEqual "check txids"
    [7,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42]
    (M.keys hist)
  -- and last tx log change for accounts touched in given block
  assertEqual "check previous balance"
    (M.fromList
     [(RowKey "sender00",
       (TxLog "coin_coin-table" "sender00"
        (object
         [ "guard" .= object
           [ "pred" .= ("keys-all" :: T.Text)
           , "keys" .=
             ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text]
           ]
         , "balance" .= (Number 100000000.0)
         ]
        )
       ))])
    prevBals

getHistoricalLookupNoTxs
    :: T.Text
    -> (Maybe (TxLog Value) -> IO ())
    -> IO (IORef MemPoolAccess)
    -> IO (PactQueue,TestBlockDb)
    -> TestTree
getHistoricalLookupNoTxs key assertF refIO reqIO = testCase msg $ do
  (q,bdb) <- reqIO
  setOneShotMempool refIO mempty
  void $ runBlock q bdb second msg
  h <- getParentTestBlockDb bdb cid
  histLookup q h key >>= assertF
  where msg = T.unpack $ "getHistoricalLookupNoTxs: " <> key

getHistoricalLookupWithTxs
    :: T.Text
    -> (Maybe (TxLog Value) -> IO ())
    -> IO (IORef MemPoolAccess)
    -> IO (PactQueue,TestBlockDb)
    -> TestTree
getHistoricalLookupWithTxs key assertF refIO reqIO = testCase msg $ do
  (q,bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool
  void $ runBlock q bdb second msg
  h <- getParentTestBlockDb bdb cid
  histLookup q h key >>= assertF
  where msg = T.unpack $ "getHistoricalLookupWithTxs: " <> key


histLookup :: PactQueue -> BlockHeader -> T.Text -> IO (Maybe (TxLog Value))
histLookup q bh k = do
  mv <- pactHistoricalLookup bh (Domain' (UserTables "coin_coin-table")) (RowKey k) q
  forSuccess "histLookup" (return mv)

assertSender00Bal :: Rational -> String -> Maybe (TxLog Value) -> Assertion
assertSender00Bal bal msg hist =
  assertEqual msg
    (Just (TxLog "coin_coin-table" "sender00"
      (object
        [ "guard" .= object
          [ "pred" .= ("keys-all" :: T.Text)
          , "keys" .=
            ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text]
          ]
        , "balance" .= Number (fromRational bal)
        ])))
    hist

newBlockRewindValidate :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockRewindValidate mpRefIO reqIO = testCase "newBlockRewindValidate" $ do
  (q,bdb) <- reqIO
  setOneShotMempool mpRefIO chainDataMemPool
  cut0 <- readMVar $ _bdbCut bdb -- genesis cut

  -- cut 1a
  void $ runBlock q bdb second "newBlockRewindValidate-1a"
  cut1a <- readMVar $ _bdbCut bdb

  -- rewind, cut 1b
  void $ swapMVar (_bdbCut bdb) cut0
  void $ runBlock q bdb second "newBlockRewindValidate-1b"

  -- rewind to cut 1a to trigger replay with chain data bug
  void $ swapMVar (_bdbCut bdb) cut1a
  void $ runBlock q bdb (secondsToTimeSpan 2) "newBlockRewindValidate-2"

  where

    chainDataMemPool = mempty {
      mpaGetBlock = \_ _ _ _ bh -> do
          fmap V.singleton $ buildBasicCmd bh $ mkExec' "(chain-data)"
      }

minerKeysetTest :: MultichainTest
minerKeysetTest _mpRefIO = do

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

chainweb213Test :: MultichainTest
chainweb213Test _mpRefIO = do

  -- run past genesis, upgrades
  runToHeight 24

  -- run block 25
  runBlockTest
      [ PactTxTest buildModCmd1 $
        assertTxGas "Old gas cost" 56
      , PactTxTest (buildSimpleCmd' "(list 1 2 3)") $
        assertTxFails' "list failure 1_1"
        listErrMsg
      , PactTxTest buildDbMod $
        assertTxSuccess' "mod db installs" $
        pString "TableCreated"
      , PactTxTest (buildSimpleCmd' "(free.dbmod.fkeys)") $
        assertTxGas "fkeys gas cost 1" 205
      , PactTxTest (buildSimpleCmd' "(free.dbmod.ffolddb)") $
        assertTxGas "ffolddb gas cost 1" 206
      , PactTxTest (buildSimpleCmd' "(free.dbmod.fselect)") $
        assertTxGas "fselect gas cost 1" 206
      ]


  -- run block 26
  runBlockTest
      [ PactTxTest buildModCmd2 $
        assertTxGas "New gas cost" 60065
      , PactTxTest (buildSimpleCmd' "(list 1 2 3)") $
        assertTxFails' "list failure 2_1"
        "Gas limit (50000) exceeded: 1000003"
      , PactTxTest  (buildSimpleCmd' "(free.dbmod.fkeys)") $
        assertTxGas "fkeys gas cost 2" 40005
      , PactTxTest (buildSimpleCmd' "(free.dbmod.ffolddb)") $
        assertTxGas "ffolddb gas cost 2" 40006
      , PactTxTest (buildSimpleCmd' "(free.dbmod.fselect)") $
        assertTxGas "fselect gas cost 2" 40006
      ]


  where

    buildSimpleCmd' code = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
        signSender00
        $ setFromHeader bh
        $ set cbGasLimit 50000
        $ mkCmd code
        $ mkExec' code
    buildModCmd1 = buildBasic
        $ mkExec' $ mconcat ["(namespace 'free)", "(module mtest G (defcap G () true) (defun a () true))"]
    buildModCmd2 = buildBasic' (set cbGasLimit 70000)
        $ mkExec' $ mconcat ["(namespace 'free)", "(module mtest2 G (defcap G () true) (defun a () false))"]
    buildDbMod = buildBasic' (set cbGasLimit 70000)
        $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module dbmod G (defcap G () true)"
        , "  (defschema sch i:integer) (deftable tbl:{sch})"
        , "  (defun fkeys () (keys tbl))"
        , "  (defun ffolddb () (fold-db tbl (lambda (a b) true) (constantly true)))"
        , "  (defun fselect () (select tbl (constantly true))))"
        , "(create-table tbl)"
        ]

buildVector :: MonadIO m => [m a] -> m (V.Vector a)
buildVector as = V.fromList <$> sequence as

resetMempool :: MultiM ()
resetMempool = view menvMpa >>= \r -> setMempool r mempty

pact43UpgradeTest :: MultichainTest
pact43UpgradeTest _mpRefIO = do

  -- run past genesis, upgrades
  runToHeight 29

  runBlockTest
      [ PactTxTest buildMod $
        assertTxGas "Old gas cost" 120332
      , PactTxTest buildModPact $
        assertTxFails'
        "Should not resolve new pact native: continue"
        "Cannot resolve \"continue\""
      , PactTxTest (buildSimpleCmd "(create-principal (read-keyset 'k))") $
        assertTxFails'
        "Should not resolve new pact native: create-principal"
        "Cannot resolve create-principal"
      , PactTxTest (buildSimpleCmd "(validate-principal (read-keyset 'k) \"k:368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca\")") $
        assertTxFails'
        "Should not resolve new pact natives: validate-principal"
        "Cannot resolve validate-principal"
      , PactTxTest (buildXSend1 []) $
        assertTxGas "xsend success" 378
      ]

  tx30_4 <- txResult 4


  runBlockTest
      [ PactTxTest buildMod $
        assertTxGas "Old gas cost" 120296
      , PactTxTest buildModPact $
        assertTxSuccess'
        "Should resolve continue in a module defn"
        (pString "Loaded module free.nestedMod, hash fDd0G7zvGar3ax2q0I0F9dISRq7Pjop5rUXOeokNIOU")
      , PactTxTest (buildSimpleCmd "(free.modB.chain)") $
        assertTxSuccess'
        "Should resolve names properly post-fork"
      -- Note: returns LDecimal because of toPactValueLenient in interpret
        (pDecimal 11)
      , PactTxTest (buildSimpleCmd "(free.modB.get-test)") $
        assertTxSuccess'
        "Should resolve names properly post-fork"
        (pString "hello")
      , PactTxTest (buildSimpleCmd "(create-principal (read-keyset 'k))") $
        assertTxSuccess'
        "Should resolve create-principal properly post-fork"
        (pString "k:368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca")
      , PactTxTest (buildSimpleCmd "(validate-principal (read-keyset 'k) \"k:368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca\")") $
        assertTxSuccess'
        "Should resolve validate-principal properly post-fork"
        (pBool True)
      ]

  resetMempool
  runToHeight 33

  xproof <- buildXProof cid 30 4 tx30_4

  withChain chain0 $ runBlockTest
      [ PactTxTest (buildXReceive' xproof) $
        assertTxSuccess' "xreceive success" (pString "Write succeeded")
      ]

  where

    buildSimpleCmd code = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
        signSender00
        $ setFromHeader bh
        $ set cbGasLimit 1000
        $ mkCmd code
        $ mkExec code
        $ mkKeySetData "k" [sender00]
    buildModPact = buildBasic' (set cbGasLimit 70000)
        $ mkExec' (mconcat
        [ "(namespace 'free)"
        , "(module nestedMod G"
        , "  (defcap G () true)"
        , "  (defpact test:string () (step \"1\") (step \"2\") (step \"3\"))"
        , "  (defpact test-nested:string () (step (test)) (step (continue (test))) (step (continue (test))))"
        , ")"
        ])
    buildMod = buildBasic' (set cbGasLimit 130000)
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



chainweb215Test :: MultichainTest
chainweb215Test _mpRefIO = do

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
        [ PactTxTest (buildXReceive' xproof) $ \cr -> do
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
    [ PactTxTest (buildXReceive' xproof1) $ \cr -> do
        evs <- mkRecdEvents v5Hash v5Hash
        assertTxEvents "Transfer events @ block 53" evs cr
    ]

    -- rewind to saved cut 50
  rewindTo savedCut1
  runToHeight 53

  where
    xsend = buildXSend1
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



currentCut :: MultiM Cut
currentCut = view menvBdb >>= liftIO . readMVar . _bdbCut

rewindTo :: Cut -> MultiM ()
rewindTo c = view menvBdb >>= \bdb -> void $ liftIO $ swapMVar (_bdbCut bdb) c

assertTxEvents :: (HasCallStack, MonadIO m) => String -> [PactEvent] -> CommandResult Hash -> m ()
assertTxEvents msg evs = assertEqual msg evs . _crEvents

assertTxGas :: (HasCallStack, MonadIO m) => String -> Gas -> CommandResult Hash -> m ()
assertTxGas msg g = assertEqual msg g . _crGas

assertTxSuccess :: HasCallStack => Int -> String -> PactValue -> MultiM ()
assertTxSuccess i msg r = do
  tx <- txResult i
  assertTxSuccess' msg r tx

assertTxSuccess' :: (HasCallStack, MonadIO m) => String -> PactValue -> CommandResult Hash -> m ()
assertTxSuccess' msg r tx = do
  assertEqual msg (Just r)
    (tx ^? crResult . to _pactResult . _Right)

assertTxFails :: HasCallStack => Int -> String -> Doc -> MultiM ()
assertTxFails i msg d = do
  tx <- txResult i
  assertTxFails' msg d tx

assertTxFails' :: (HasCallStack, MonadIO m) => String -> Doc -> CommandResult Hash -> m ()
assertTxFails' msg d tx =
  assertEqual msg (Just d)
    (tx ^? crResult . to _pactResult . _Left . to peDoc)


pact431UpgradeTest :: MultichainTest
pact431UpgradeTest _mpRefIO = do

  -- run past genesis, upgrades
  runToHeight 34

  -- run block 35, pre fork
  runBlockTest
    [ PactTxTest describeModule $
      assertTxSuccess' "describe-module legacy success"
      $ pBool True
    , PactTxTest isPrincipal $
      assertTxFails' "Should not resolve new pact native: is-principal"
      "Cannot resolve is-principal"
    , PactTxTest typeOfPrincipal $
      assertTxFails' "Should not resolve new pact native: typeof-principal"
      "Cannot resolve typeof-principal"
    , PactTxTest enforcePactVersion $
      assertTxSuccess' "Enforce pact version passes pre-fork"
      $ pBool True
    , PactTxTest pactVersion $
      assertTxSuccess' "Pact version is 4.2.1 for compat pre-fork"
      $ pString "4.2.1"
    ]

  -- run block 36, post fork
  runBlockTest
    [ PactTxTest describeModule $
      assertTxFails' "Should fail to execute describe-module"
      "Operation only permitted in local execution mode"
    , PactTxTest isPrincipal $
      assertTxSuccess' "Should resolve new pact native: is-principal"
      $ pBool True
    , PactTxTest typeOfPrincipal $
      assertTxSuccess' "Should resolve new pact native: typeof-principal"
      $ pString "k:"
    , PactTxTest enforcePactVersion $
      assertTxFails' "Should fail to execute enforce-pact-version"
      "Operation only permitted in local execution mode"
    , PactTxTest pactVersion $
      assertTxFails' "Should fail to execute pact-version"
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
    buildSimpleCmd code = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
        signSender00
        $ setFromHeader bh
        $ set cbGasLimit 1000
        $ mkCmd code
        $ mkExec code
        $ mkKeySetData "k" [sender00]
    describeModule = buildBasic' (set cbGasLimit 100000)
      $ mkExec (mconcat
        [ "(namespace 'free)"
        , "(module mod G"
        , "  (defcap G () true)"
        , "  (defun f () true)"
        , ")"
        , "(describe-module \"free.mod\") true"
        ])
        $ mkKeySetData "k" [sender00]

-- | Run a single mempool block on current chain with tests for each tx.
-- Limitations: can only run a single-chain, single-refill test for
-- a given cut height.
runBlockTest :: [PactTxTest] -> MultiM ()
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
runBlockTests :: [PactTxTest] -> MultiM ()
runBlockTests pts = do
  txResults >>= zipWithM_ go pts . V.toList
  where
    go (PactTxTest _ t) cr = liftIO $ t cr

-- | Run cuts to block height.
runToHeight :: BlockHeight -> MultiM ()
runToHeight bhi = do
  chid <- view menvChainId
  bh <- getHeader chid
  when (_blockHeight bh < bhi) $ do
    runCut'
    runToHeight bhi

pact420UpgradeTest :: MultichainTest
pact420UpgradeTest _mpRefIO = do

  -- run past genesis, upgrades
  runToHeight 3

  -- run block 4
  runBlockTest
    [ PactTxTest buildNewNatives420FoldDbCmd $
      assertTxFails'
      "Should not resolve new pact natives"
      "Cannot resolve fold-db"
    , PactTxTest buildNewNatives420ZipCmd $
      assertTxFails'
      "Should not resolve new pact natives"
      "Cannot resolve zip"
    , PactTxTest buildFdbCmd $
      assertTxSuccess'
      "Load fdb module"
      (pString "Write succeeded")
    ]

  cbResult >>= assertEqual "Coinbase events @ block 4" [] . _crEvents

  -- run block 5

  runBlockTest
    [ PactTxTest buildNewNatives420FoldDbCmd $
      assertTxSuccess'
      "Should resolve fold-db pact native" $
      pList [pObject [("a", pInteger 1),("b",pInteger 1)]
            ,pObject [("a", pInteger 2),("b",pInteger 2)]]
    , PactTxTest buildNewNatives420ZipCmd $
      assertTxSuccess'
      "Should resolve zip pact native" $
      pList $ pInteger <$> [5,7,9]
    ]

  cbResult >>= assertEqual "Coinbase events @ block 5" [] . _crEvents

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

chainweb216Test :: MultichainTest
chainweb216Test _mpRefIO = do
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
        assertTxGas "Pre-fork format gas" 11
      , PactTxTest (buildSimpleCmd tryGas) $
        assertTxGas "Pre-fork try" 9
      , PactTxTest (buildSimpleCmd defineNonNamespacedPreFork) $
        assertTxSuccess'
        "Should pass when defining a non-namespaced keyset"
        (pBool True)
      , PactTxTest (buildSimpleCmd defineNamespacedPreFork) $
      -- Note, keysets technically are not namespaced pre-fork, the new parser isn't applied
        assertTxSuccess'
        "Should pass when defining a \"namespaced\" keyset pre fork"
        (pBool True)
      ]

  runBlockTest
      [ PactTxTest (buildSimpleCmd formatGas) $
        assertTxGas "Post-fork format gas increase" 38
      , PactTxTest (buildSimpleCmd tryGas) $
        assertTxGas "Post-fork try should charge a bit more gas" 10
      , PactTxTest (buildSimpleCmd defineNonNamespacedPostFork1) $
        assertTxFails'
        "Should fail when defining a non-namespaced keyset post fork"
        "Mismatching keyset namespace"
      , PactTxTest (buildSimpleCmd defineNamespacedPostFork) $
        assertTxSuccess'
        "Pass when defining a namespaced keyset post fork"
        (pBool True)
      , PactTxTest (buildSimpleCmd enforceNamespacedFromPreFork) $
        assertTxSuccess'
        "Should work in enforcing a namespaced keyset created prefork"
        (pBool True)
      , PactTxTest (buildSimpleCmd enforceNonNamespacedFromPreFork) $
        assertTxSuccess'
        "Should work in enforcing a non-namespaced keyset created prefork"
        (pBool True)
      , PactTxTest (buildSimpleCmd defineNonNamespacedPostFork2) $
        assertTxFails'
        "Should fail in defining a keyset outside a namespace"
        "Cannot define a keyset outside of a namespace"
      , PactTxTest buildModCommand $
        assertTxSuccess'
        "Should succeed in deploying a module guarded by a namespaced keyset"
        (pString "Loaded module free.m1, hash nOHaU-gPtmZTj6ZA3VArh-r7LEiwVUMN_RLJeW2hNv0")
      , PactTxTest (buildSimpleCmd rotateLegacyPostFork) $
        assertTxSuccess'
        "Should succeed in rotating and enforcing a legacy keyset"
        (pBool True)
      , PactTxTest (buildSimpleCmd rotateNamespacedPostFork) $
        assertTxSuccess'
        "Should succeed in rotating and enforcing a namespaced keyset"
        (pBool True)
      ]

  runBlockTest
      [ PactTxTest (buildSimpleCmd "(free.m1.f)") $
        assertTxSuccess'
        "Should call a module with a namespaced keyset correctly"
        (pDecimal 1)
      , PactTxTest (buildSimpleCmd "(^ 15.034465284692086701747761395233132973944448512421004399685858401206740385711739229018307610943234609057822959334669087436253689423614206061665462283698768757790600552385430913941421707844383369633809803959413869974997415115322843838226312287673293352959835 3.466120406090666777582519661568003549307295836842780244500133445635634490670936927006970368136648330889718447039413255137656971927890831071689768359173260960739254160211017410322799793419223796996260056081828170546988461285168124170297427792046640116184356)") $
        assertTxSuccess'
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


  buildModCommand = buildBasic' (set cbGasLimit 70000)
    $ mkExec' defineModulePostFork

  buildSimpleCmd code = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
    signSender00
    $ setFromHeader bh
    $ set cbGasLimit 10000
    $ mkCmd code
    $ mkExec code
    $ object
      [ "k123" .= map fst [sender00]
      , "k456" .= map fst [sender00]
      , "free.k123" .= map fst [sender00]
      , "free.k456" .= map fst [sender00]]

pact4coin3UpgradeTest :: MultichainTest
pact4coin3UpgradeTest mpRefIO = do

  -- run past genesis, upgrades
  forM_ [(1::Int)..6] $ \_i -> runCut'

  -- run block 7
  setOneShotMempool mpRefIO getBlock7
  runCut'

  tx7_0 <- txResult 0
  assertEqual "Hash of coin @ block 7" (pHash "ut_J_ZNkoyaPUEJhiwVeWnkSQn9JT9sQCWKdjjVVrWo") (_crResult tx7_0)
  assertEqual "Events for tx 0 @ block 7" [] (_crEvents tx7_0)

  tx7_1 <- txResult 1
  assertEqual "Events for tx 1 @ block 7" [] (_crEvents tx7_1)

  assertTxFails 2
    "Should not resolve new pact natives"
    "Cannot resolve distinct"

  tx7_3 <- txResult 3
  assertSatisfies
    "Should allow bad keys"
    (_pactResult $ _crResult tx7_3) isRight

  cb7 <- cbResult
  assertEqual "Coinbase events @ block 7" [] (_crEvents cb7)

  -- run past v3 upgrade, pact 4 switch
  setOneShotMempool mpRefIO mempty
  cuts <- forM [(8::Int)..21] $ \_i -> do
      runCut'
      if _i == 18
          then Just <$> currentCut
          else return Nothing

  savedCut <- fromMaybeM (userError "A cut should exist here.") $ msum cuts

  -- block 22
  -- get proof
  xproof <- buildXProof cid 7 1 tx7_1

  -- run block 22
  setMempool mpRefIO =<< getOncePerChainMempool (getBlock22 xproof)
  runCut'
  let v3Hash = "1os_sLAUYvBzspn5jjawtRpJWiH1WPfhyNraeVvSIwU"

  cb22 <- cbResult
  cbEv <- mkTransferEvent "" "NoMiner" 2.304523 "coin" v3Hash
  assertEqual "Coinbase events @ block 22" [cbEv] (_crEvents cb22)

  tx22_0 <- txResult 0
  gasEv0 <- mkTransferEvent "sender00" "NoMiner" 0.0013 "coin" v3Hash
  assertEqual "Hash of coin @ block 22" (pHash v3Hash) (_crResult tx22_0)
  assertEqual "Events for tx0 @ block 22" [gasEv0] (_crEvents tx22_0)

  tx22_1 <- txResult 1
  gasEv1 <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
  allocTfr <- mkTransferEvent "" "allocation00" 1000000.0 "coin" v3Hash
  allocEv <- mkEvent "RELEASE_ALLOCATION" [pString "allocation00",pDecimal 1000000.0]
             "coin" v3Hash
  assertEqual "Events for tx1 @ block 22" [gasEv1,allocEv,allocTfr] (_crEvents tx22_1)

  -- test another sendXChain events
  tx22_2 <- txResult 2
  gasEv2 <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
  sendTfr <- mkTransferEvent "sender00" "" 0.0123 "coin" v3Hash
  let pguard = PGuard (GKeySet (KeySet {_ksKeys = S.fromList [PublicKey {_pubKey = "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"}], _ksPredFun = Name (BareName {_bnName = "keys-all", _bnInfo = def })}))
  yieldEv <- mkEvent "X_YIELD" [pString "0", pString "coin.transfer-crosschain", pList [pString "sender00", pString "sender00", pguard, pString "0", pDecimal 0.0123]] "pact" v3Hash
  assertEqual "Events for tx2 @ block 22" [gasEv2,sendTfr, yieldEv] (_crEvents tx22_2)

  assertTxSuccess 3
    "Should resolve enumerate pact native"
    (PList $ V.fromList $ pInteger <$> [1..10])

  assertTxFails 4
    "Should not allow bad keys"
    "Invalid keyset"


  -- test receive XChain events
  txRcv <- withChain chain0 $ txResult 0
  gasEvRcv <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
  rcvTfr <- mkTransferEvent "" "sender00" 0.0123 "coin" v3Hash
  assertEqual "Events for txRcv" [gasEvRcv,rcvTfr] (_crEvents txRcv)

  -- rewind to savedCut (cut 18)
  rewindTo savedCut
  forM_ [(18 :: Int) .. 21] $ const runCut'
  runCut'

  where

    getBlock7 = mempty {
      mpaGetBlock = \_ _ _ _ bh -> if _blockChainId bh == cid then buildVector
          [ buildHashCmd bh
          , buildXSend bh
          , buildNewNatives40Cmd bh
          , badKeyset bh
          ]
          else return mempty
      }

    getBlock22 xproof bh =
          let go | bid == cid = buildVector
                     [ buildHashCmd bh
                     , buildReleaseCommand bh
                     , buildXSend bh
                     , buildNewNatives40Cmd bh
                     , badKeyset bh
                     ]
                 | _blockChainId bh == chain0 = do
                     V.singleton <$> buildXReceive bh xproof
                 | otherwise = return mempty
              bid = _blockChainId bh
          in go


    buildHashCmd bh = buildBasicCmd bh
        $ mkExec' "(at 'hash (describe-module 'coin))"

    badKeyset bh = buildBasicCmd bh
        $ mkExec "(read-keyset 'ks)" $ object ["ks" .= ["badkey"::T.Text]]

    buildNewNatives40Cmd bh = buildBasicCmd bh
        $ mkExec' (mconcat expressions)
      where
        expressions =
          [
            "(distinct [1 1 2 2 3 3])"
          , "(concat [\"this\" \"is\" \"a\" \"test\"])"
          , "(str-to-list \"test\")"
          , "(enumerate 1 10)"
          ]


    buildReleaseCommand bh = buildCwCmd
        $ set cbSigners [mkSigner' sender00 [],mkSigner' allocation00KeyPair []]
        $ setFromHeader bh
        $ mkCmd (sshow bh)
        $ mkExec' "(coin.release-allocation 'allocation00)"

    pHash = PactResult . Right . pString

buildXSend :: BlockHeader -> IO ChainwebTransaction
buildXSend bh = buildXSend' (sshow bh) bh []

buildXSend' :: T.Text -> BlockHeader -> [SigCapability] -> IO ChainwebTransaction
buildXSend' nonce bh caps = buildCwCmd
    $ set cbSigners [mkSigner' sender00 caps]
    $ setFromHeader bh
    $ mkCmd nonce
    $ mkExec
      "(coin.transfer-crosschain 'sender00 'sender00 (read-keyset 'k) \"0\" 0.0123)" $
      mkKeySetData "k" [sender00]

buildXSend1 :: [SigCapability] -> MempoolCmdBuilder
buildXSend1 caps = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
  set cbSigners [mkSigner' sender00 caps]
  $ setFromHeader bh
  $ mkCmd (sshow bh)
  $ mkExec
    "(coin.transfer-crosschain 'sender00 'sender00 (read-keyset 'k) \"0\" 0.0123)" $
    mkKeySetData "k" [sender00]


chain0 :: ChainId
chain0 = unsafeChainId 0

withChain :: ChainId -> MultiM a -> MultiM a
withChain c = local (set menvChainId c)

buildXProof
    :: ChainId
    -> BlockHeight
    -> Int
    -> CommandResult l
    -> MultiM (ContProof, PactId)
buildXProof scid bh i sendTx = do
    bdb <- view menvBdb
    proof <- liftIO $ ContProof . B64U.encode . encodeToByteString <$>
      createTransactionOutputProof_ (_bdbWebBlockHeaderDb bdb) (_bdbPayloadDb bdb) chain0 scid bh i
    pid <- fromMaybeM (userError "no continuation") $
      preview (crContinuation . _Just . pePactId) sendTx
    return (proof,pid)

buildXReceive
    :: MonadIO m
    => MonadThrow m
    => BlockHeader
    -> (ContProof, PactId)
    -> m ChainwebTransaction
buildXReceive bh (proof,pid) = buildBasicCmd bh
    $ mkCont ((mkContMsg pid 1) { _cmProof = Just proof })

buildXReceive'
    :: (ContProof, PactId)
    -> MempoolCmdBuilder
buildXReceive' (proof,pid) = buildBasic $
    mkCont ((mkContMsg pid 1) { _cmProof = Just proof })

signSender00 :: CmdBuilder -> CmdBuilder
signSender00 = set cbSigners [mkSigner' sender00 []]

setFromHeader :: BlockHeader -> CmdBuilder -> CmdBuilder
setFromHeader bh =
  set cbChainId (_blockChainId bh)
  . set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)

buildBasicCmd
    :: MonadIO m
    => BlockHeader
    -> PactRPC T.Text
    -> m ChainwebTransaction
buildBasicCmd bh = buildBasicCmd' bh id

buildBasicCmd'
    :: MonadIO m
    => BlockHeader
    -> (CmdBuilder -> CmdBuilder)
    -> PactRPC T.Text
    -> m ChainwebTransaction
buildBasicCmd' bh s = liftIO . buildCwCmd
    . signSender00
    . setFromHeader bh
    . s
    . mkCmd (sshow bh)

buildBasic
    :: PactRPC T.Text
    -> MempoolCmdBuilder
buildBasic = buildBasic' id

buildBasic'
    :: (CmdBuilder -> CmdBuilder)
    -> PactRPC T.Text
    -> MempoolCmdBuilder
buildBasic' s r = MempoolCmdBuilder $ \(MempoolInput _ bh) ->
  signSender00
  $ setFromHeader bh
  $ s
  $ mkCmd (sshow bh) r

getOncePerChainMempool
    :: MonadIO m
    => (BlockHeader -> IO (V.Vector ChainwebTransaction))
    -> m MemPoolAccess
getOncePerChainMempool mp = do
  cids <- liftIO $ newIORef mempty
  return $ mempty {
    mpaGetBlock = \_ _ _ _ bh ->
      let enfChain f = do
            cids' <- readIORef cids
            if bid `elem` cids' then mempty else do
              writeIORef cids (bid:cids')
              f bh
          bid = _blockChainId bh
      in enfChain mp
    }

-- | Get output on latest cut for chain
getPWO :: ChainId -> MultiM (PayloadWithOutputs,BlockHeader)
getPWO chid = do
  (TestBlockDb _ pdb _) <- view menvBdb
  h <- getHeader chid
  pwo <- liftIO $ casLookupM pdb (_blockPayloadHash h)
  return (pwo,h)

getHeader :: ChainId -> MultiM BlockHeader
getHeader chid = do
  (TestBlockDb _ _ cmv) <- view menvBdb
  c <- liftIO $ readMVar cmv
  fromMaybeM (userError $ "chain lookup failed for " ++ show chid) $ HM.lookup chid (_cutMap c)

-- | Get tx at index from output
txResult :: Int -> MultiM (CommandResult Hash)
txResult i = do
  chid <- view menvChainId
  (o,h) <- getPWO chid
  case preview (ix i . _2) $ _payloadWithOutputsTransactions o of
    Nothing ->
      throwM $ userError $
      show (_blockHeight h) ++ ": no tx at " ++ show i
    Just txo -> decodeStrictOrThrow @_ @(CommandResult Hash) (_transactionOutputBytes txo)

txResults :: MultiM (V.Vector (CommandResult Hash))
txResults = do
  chid <- view menvChainId
  (o,_h) <- getPWO chid
  forM (_payloadWithOutputsTransactions o) $ \(_,txo) ->
    decodeStrictOrThrow @_ @(CommandResult Hash) (_transactionOutputBytes txo)

-- | Get coinbase from output
cbResult :: MultiM (CommandResult Hash)
cbResult = do
  chid <- view menvChainId
  (o,_h) <- getPWO chid
  liftIO $
    decodeStrictOrThrow @_ @(CommandResult Hash) (_coinbaseOutput $ _payloadWithOutputsCoinbase o)

pattern BlockGasLimitError :: forall b. Either PactException b
pattern BlockGasLimitError <-
  Left (PactInternalError (decode . BL.fromStrict . T.encodeUtf8 -> Just (BlockGasLimitExceeded _)))

-- this test relies on block gas errors being thrown before other Pact errors.
blockGasLimitTest :: IO (IORef MemPoolAccess) -> IO (PactQueue, TestBlockDb) -> TestTree
blockGasLimitTest _ reqIO = testCase "blockGasLimitTest" $ do
  (q,_) <- reqIO

  let
    useGas g = do
      bigTx <- buildCwCmd $ set cbGasLimit g $ signSender00 $ mkCmd "cmd" $ mkExec' "TESTING"
      let
        cr = CommandResult
          (RequestKey (Hash "0")) Nothing
          (PactResult $ Left $ PactError EvalError (Pact.Types.Info.Info $ Nothing) [] mempty)
          (fromIntegral g) Nothing Nothing Nothing []
        block = Transactions
          (V.singleton (bigTx, cr))
          (CommandResult (RequestKey (Hash "h")) Nothing
            (PactResult $ Right $ pString "output") 0 Nothing Nothing Nothing [])
        payload = toPayloadWithOutputs noMiner block
        bh = newBlockHeader
          mempty
          (_payloadWithOutputsPayloadHash payload)
          (Nonce 0)
          (BlockCreationTime $ Time $ TimeSpan 0)
          (ParentHeader $ genesisBlockHeader testVersion cid)
      validateBlock bh (payloadWithOutputsToPayloadData payload) q >>= takeMVar
  -- we consume slightly more than the maximum block gas limit and provoke an error.
  useGas 2_000_001 >>= \case
    BlockGasLimitError ->
      return ()
    r ->
      error $ "not a BlockGasLimitExceeded error: " <> sshow r
  -- we consume much more than the maximum block gas limit and expect an error.
  useGas 3_000_000 >>= \case
    BlockGasLimitError ->
      return ()
    r ->
      error $ "not a BlockGasLimitExceeded error: " <> sshow r
  -- we consume exactly the maximum block gas limit and expect no such error.
  useGas 2_000_000 >>= \case
    BlockGasLimitError ->
      error "consumed exactly block gas limit but errored"
    _ ->
      return ()
  -- we consume much less than the maximum block gas limit and expect no such error.
  useGas 1_000_000 >>= \case
    BlockGasLimitError ->
      error "consumed much less than block gas limit but errored"
    _ ->
      return ()
  -- we consume zero gas and expect no such error.
  useGas 0 >>= \case
    BlockGasLimitError ->
      error "consumed no gas but errored"
    _ ->
      return ()

mempoolRefillTest :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
mempoolRefillTest mpRefIO reqIO = testCase "mempoolRefillTest" $ do

  (q,bdb) <- reqIO
  supply <- newMVar (0 :: Int)

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [badTx] ) ]
  runBlock q bdb second "mempoolRefillTest-1" >>= checkCount 2

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [goodTx, badTx] ) ]
  runBlock q bdb second "mempoolRefillTest-2" >>= checkCount 3

  mp supply [ ( 0, [badTx, goodTx] ), ( 1, [goodTx, badTx] ) ]
  runBlock q bdb second "mempoolRefillTest-3" >>= checkCount 2

  mp supply [ ( 0, [badTx] ), ( 1, [goodTx, goodTx] ) ]
  runBlock q bdb second "mempoolRefillTest-3" >>= checkCount 2

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [badTx, badTx] ) ]
  runBlock q bdb second "mempoolRefillTest-3" >>= checkCount 2


  where

    checkCount n = assertEqual "tx return count" n . V.length . _payloadWithOutputsTransactions

    mp supply txRefillMap = setMempool mpRefIO $ mempty {
      mpaGetBlock = \BlockFill{..} _ _ _ bh -> case M.lookup _bfCount (M.fromList txRefillMap) of
          Nothing -> return mempty
          Just txs -> fmap V.fromList $ sequence $ map (next supply bh) txs
      }


    next supply bh f = do
      i <- modifyMVar supply $ return . (succ &&& id)
      f i bh

    goodTx i bh = buildCwCmd
        $ signSender00
        $ mkCmd' bh (sshow (i,bh))
        $ mkExec' "(+ 1 2)"

    badTx i bh = buildCwCmd
        $ signSender00
        $ set cbSender "bad"
        $ mkCmd' bh (sshow (i,bh))
        $ mkExec' "(+ 1 2)"


    mkCmd' :: BlockHeader -> T.Text -> PactRPC T.Text -> CmdBuilder
    mkCmd' bh nonce =
      setFromHeader bh
      . mkCmd nonce



moduleNameFork :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
moduleNameFork mpRefIO reqIO = testCase "moduleNameFork" $ do

  (q,bdb) <- reqIO

  -- install in free in block 1
  setOneShotMempool mpRefIO (moduleNameMempool "free" "test")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- install in user in block 2
  setOneShotMempool mpRefIO (moduleNameMempool "user" "test")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- do something else post-fork
  setOneShotMempool mpRefIO (moduleNameMempool "free" "test2")
  void $ runBlock q bdb second "moduleNameFork-1"
  setOneShotMempool mpRefIO (moduleNameMempool "user" "test2")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- TODO this test doesn't actually validate, I turn on Debug and make sure it
  -- goes well.

moduleNameMempool :: T.Text -> T.Text -> MemPoolAccess
moduleNameMempool ns mn = mempty
    { mpaGetBlock = getTestBlock
    }
  where
    getTestBlock _ _ _ _ bh = do
        let txs =
              [ "(namespace '" <> ns <> ") (module " <> mn <> " G (defcap G () (enforce false 'cannotupgrade)))"
              , ns <> "." <> mn <> ".G"
              ]
        fmap V.fromList $ forM (zip txs [0..]) $ \(code,n :: Int) ->
          buildCwCmd $
          signSender00 $
          set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh) $
          mkCmd ("1" <> sshow n) $
          mkExec' code


mempoolCreationTimeTest :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
mempoolCreationTimeTest mpRefIO reqIO = testCase "mempoolCreationTimeTest" $ do

  (q,bdb) <- reqIO

  let start@(Time startSpan) :: Time Micros = Time (TimeSpan (Micros 100_000_000))
      s30 = scaleTimeSpan (30 :: Int) second
      s15 = scaleTimeSpan (15 :: Int) second
  -- b1 block time is start
  void $ runBlock q bdb startSpan "mempoolCreationTimeTest-1"


  -- do pre-insert check with transaction at start + 15s
  tx <- makeTx "tx-now" (add s15 start)
  void $ forSuccess "mempoolCreationTimeTest: pre-insert tx" $
    pactPreInsertCheck (V.singleton tx) q

  setOneShotMempool mpRefIO $ mp tx
  -- b2 will be made at start + 30s
  void $ runBlock q bdb s30 "mempoolCreationTimeTest-2"

  where

    makeTx nonce t = buildCwCmd
        $ signSender00
        $ set cbChainId cid
        $ set cbCreationTime (toTxCreationTime $ t)
        $ set cbTTL 300
        $ mkCmd (sshow t <> nonce)
        $ mkExec' "1"
    mp tx = mempty {
      mpaGetBlock = \_ valid _ _ bh -> getBlock bh tx valid
      }

    getBlock bh tx valid = do
      let txs = V.singleton tx
      oks <- valid (_blockHeight bh) (_blockHash bh) txs
      unless (V.and oks) $ throwM $ userError "Insert failed"
      return txs


badlistNewBlockTest :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
badlistNewBlockTest mpRefIO reqIO = testCase "badlistNewBlockTest" $ do
  (reqQ,_) <- reqIO
  let hashToTxHashList = V.singleton . requestKeyToTransactionHash . RequestKey . toUntypedHash @'Blake2b_256
  badHashRef <- newIORef $ hashToTxHashList initialHash
  badTx <- buildCwCmd
    $ signSender00
    -- this should exceed the account balance
    $ set cbGasLimit 99999
    $ set cbGasPrice 1_000_000_000_000_000
    $ mkCmd "badListMPA"
    $ mkExec' "(+ 1 2)"
  setOneShotMempool mpRefIO (badlistMPA badTx badHashRef)
  resp <- forSuccess "badlistNewBlockTest" $ newBlock noMiner (ParentHeader genesisHeader) reqQ
  assertEqual "bad tx filtered from block" mempty (_payloadWithOutputsTransactions resp)
  badHash <- readIORef badHashRef
  assertEqual "Badlist should have badtx hash" (hashToTxHashList $ _cmdHash badTx) badHash
  where
    badlistMPA badTx badHashRef = mempty
      { mpaGetBlock = \_ _ _ _ _ -> return (V.singleton badTx)
      , mpaBadlistTx = \v -> writeIORef badHashRef v
      }


goldenNewBlock :: String -> MemPoolAccess -> IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
goldenNewBlock name mp mpRefIO reqIO = golden name $ do
    (reqQ,_) <- reqIO
    setOneShotMempool mpRefIO mp
    resp <- forSuccess ("goldenNewBlock:" ++ name) $
      newBlock noMiner (ParentHeader genesisHeader) reqQ
    -- ensure all golden txs succeed
    forM_ (_payloadWithOutputsTransactions resp) $ \(txIn,TransactionOutput out) -> do
      cr :: CommandResult Hash <- decodeStrictOrThrow out
      assertSatisfies ("golden tx succeeds, input: " ++ show txIn) (_crResult cr) (isRight . (\(PactResult r) -> r))
    goldenBytes resp
  where
    goldenBytes :: PayloadWithOutputs -> IO BL.ByteString
    goldenBytes a = return $ BL.fromStrict $ Y.encode $ object
      [ "test-group" .= ("new-block" :: T.Text)
      , "results" .= a
      ]

goldenMemPool :: MemPoolAccess
goldenMemPool = mempty
    { mpaGetBlock = getTestBlock
    }
  where
    getTestBlock _ validate bHeight bHash _parent = do
        moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
        let txs =
              [ (T.pack moduleStr)
              , "(create-table free.test1.accounts)"
              , "(free.test1.create-global-accounts)"
              , "(free.test1.transfer \"Acct1\" \"Acct2\" 1.00)"
              , "(at 'prev-block-hash (chain-data))"
              , "(at 'block-time (chain-data))"
              , "(at 'block-height (chain-data))"
              , "(at 'gas-limit (chain-data))"
              , "(at 'gas-price (chain-data))"
              , "(at 'chain-id (chain-data))"
              , "(at 'sender (chain-data))"
              ]
        outtxs <- mkTxs txs
        oks <- validate bHeight bHash outtxs
        unless (V.and oks) $ fail $ mconcat
            [ "tx failed validation! input list: \n"
            , show txs
            , "\n\nouttxs: "
            , show outtxs
            , "\n\noks: "
            , show oks ]
        return outtxs
    mkTxs txs =
        fmap V.fromList $ forM (zip txs [0..]) $ \(code,n :: Int) ->
          buildCwCmd $
          signSender00 $
          set cbGasPrice 0.01 $
          set cbTTL 1_000_000 $ -- match old goldens
          mkCmd ("1" <> sshow n) $
          mkExec code $
          mkKeySetData "test-admin-keyset" [sender00]
