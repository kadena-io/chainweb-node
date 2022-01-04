{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.DeepSeq
import Control.Concurrent.MVar
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad

import Data.Aeson (object, (.=), Value(..))
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
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import System.IO.Extra
import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Parse
import Pact.Types.ChainMeta
import Pact.Types.Continuation
import Pact.Types.Exp
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.PactError
import Pact.Types.SPV
import Pact.Types.Term

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue (PactQueue)
import Chainweb.Pact.Service.Types
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

testVersion :: ChainwebVersion
testVersion = FastTimedCPM peterson

cid :: ChainId
cid = someChainId testVersion

genesisHeader :: BlockHeader
genesisHeader = genesisBlockHeader testVersion cid

tests :: RocksDb -> ScheduledTest
tests rdb = ScheduledTest testName $ go
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
         , multiChainTest "pact4coin3UpgradeTest" pact4coin3UpgradeTest
         ]
      where
        test logLevel f =
          withDelegateMempool $ \dm ->
          withPactTestBlockDb testVersion cid logLevel rdb (snd <$> dm) defaultPactServiceConfig $
          f (fst <$> dm)

        multiChainTest tname f =
          withDelegateMempool $ \dmpio -> testCase tname $
            withTestBlockDb testVersion $ \bdb -> do
              (iompa,mpa) <- dmpio
              withWebPactExecutionService testVersion bdb mpa $ \pact ->
                f bdb (return iompa) pact
        testHistLookup1 = getHistoricalLookupNoTxs "sender00"
          (assertSender00Bal 100000000 "check latest entry for sender00 after a no txs block")
        testHistLookup2 = getHistoricalLookupNoTxs "randomAccount"
          (assertEqual "Return Nothing if key absent after a no txs block" Nothing)
        testHistLookup3 = getHistoricalLookupWithTxs "sender00"
          (assertSender00Bal 9.999998051e7 "check latest entry for sender00 after block with txs")


forSuccess :: NFData a => String -> IO (MVar (Either PactException a)) -> IO a
forSuccess msg mvio = (`catchAllSynchronous` handler) $ do
  mv <- mvio
  takeMVar mv >>= \r -> case r of
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
  setMempool refIO goldenMemPool
  void $ runBlock q bdb second "newBlockAndValidate"


getHistory :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
getHistory refIO reqIO = testCase "getHistory" $ do
  (q,bdb) <- reqIO
  setMempool refIO goldenMemPool
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
  setMempool refIO mempty
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
  setMempool refIO goldenMemPool
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
  setMempool mpRefIO chainDataMemPool
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
      mpaGetBlock = \_ _ _ bh -> do
          fmap V.singleton $ buildCwCmd
            $ set cbSigners [mkSigner' sender00 []]
            $ set cbChainId (_blockChainId bh)
            $ set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)
            $ mkCmd (sshow bh) -- nonce is block height, sufficiently unique
            $ mkExec' "(chain-data)"
      }


pact4coin3UpgradeTest :: TestBlockDb -> IO (IORef MemPoolAccess) -> WebPactExecutionService -> IO ()
pact4coin3UpgradeTest bdb mpRefIO pact = do

  -- run past genesis, upgrades
  forM_ [(1::Int)..6] $ \_i -> runCut'

  -- run block 7
  setMempool mpRefIO getBlock7
  runCut'
  pwo7 <- getPWO bdb cid

  tx7_0 <- txResult 0 pwo7
  assertEqual "Hash of coin @ block 7" (pHash "ut_J_ZNkoyaPUEJhiwVeWnkSQn9JT9sQCWKdjjVVrWo") (_crResult tx7_0)
  assertEqual "Events for tx 0 @ block 7" [] (_crEvents tx7_0)

  tx7_1 <- txResult 1 pwo7
  assertEqual "Events for tx 1 @ block 7" [] (_crEvents tx7_1)

  tx7_2 <- txResult 2 pwo7
  assertEqual
    "Should not resolve new pact natives"
    (Just "Cannot resolve distinct")
    (tx7_2 ^? crResult . to _pactResult . _Left . to peDoc)

  tx7_3 <- txResult 3 pwo7
  assertEqual
    "Should allow bad keys"
    Nothing
    (tx7_3 ^? crResult . to _pactResult . _Left . to peDoc)


  cb7 <- cbResult pwo7
  assertEqual "Coinbase events @ block 7" [] (_crEvents cb7)

  -- run past v3 upgrade, pact 4 switch
  setMempool mpRefIO mempty
  cuts <- forM [(8::Int)..21] $ \_i -> do
      runCut'
      if _i == 18
          then fmap Just (readMVar $ _bdbCut bdb)
          else return Nothing

  savedCut <- fromMaybeM (userError "A cut should exist here.") $ msum cuts

  -- block 22
  -- get proof
  proof <- ContProof . B64U.encode . encodeToByteString <$>
      createTransactionOutputProof_ (_bdbWebBlockHeaderDb bdb) (_bdbPayloadDb bdb) chain0 cid 7 1
  pid <- fromMaybeM (userError "no continuation") $
    preview (crContinuation . _Just . pePactId) tx7_1

  -- run block 22
  setMempool mpRefIO $ getBlock22 (Just proof) pid
  runCut'
  pwo22 <- getPWO bdb cid
  let v3Hash = "1os_sLAUYvBzspn5jjawtRpJWiH1WPfhyNraeVvSIwU"

  cb22 <- cbResult pwo22
  cbEv <- mkTransferEvent "" "NoMiner" 2.304523 "coin" v3Hash
  assertEqual "Coinbase events @ block 22" [cbEv] (_crEvents cb22)

  tx22_0 <- txResult 0 pwo22
  gasEv0 <- mkTransferEvent "sender00" "NoMiner" 0.0013 "coin" v3Hash
  assertEqual "Hash of coin @ block 22" (pHash v3Hash) (_crResult tx22_0)
  assertEqual "Events for tx0 @ block 22" [gasEv0] (_crEvents tx22_0)

  tx22_1 <- txResult 1 pwo22
  gasEv1 <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
  allocTfr <- mkTransferEvent "" "allocation00" 1000000.0 "coin" v3Hash
  allocEv <- mkEvent "RELEASE_ALLOCATION" [pString "allocation00",pDecimal 1000000.0]
             "coin" v3Hash
  assertEqual "Events for tx1 @ block 22" [gasEv1,allocEv,allocTfr] (_crEvents tx22_1)

  -- test another sendXChain events
  tx22_2 <- txResult 2 pwo22
  gasEv2 <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
  sendTfr <- mkTransferEvent "sender00" "" 0.0123 "coin" v3Hash
  let pguard = PGuard (GKeySet (KeySet {_ksKeys = S.fromList [PublicKey {_pubKey = "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"}], _ksPredFun = Name (BareName {_bnName = "keys-all", _bnInfo = def })}))
  yieldEv <- mkEvent "X_YIELD" [pString "0", pString "coin.transfer-crosschain", pList [pString "sender00", pString "sender00", pguard, pString "0", pDecimal 0.0123]] "pact" v3Hash
  assertEqual "Events for tx2 @ block 22" [gasEv2,sendTfr, yieldEv] (_crEvents tx22_2)

  tx22_3 <- txResult 3 pwo22
  assertEqual
    "Should resolve enumerate pact native"
    (Just $ PList $ V.fromList $ PLiteral . LInteger <$> [1..10])
    (tx22_3 ^? crResult . to _pactResult . _Right)

  tx22_4 <- txResult 4 pwo22
  assertEqual
    "Should not allow bad keys"
    (Just "Invalid keyset")
    (tx22_4 ^? crResult . to _pactResult . _Left . to peDoc)


  -- test receive XChain events
  pwo22_0 <- getPWO bdb chain0
  txRcv <- txResult 0 pwo22_0
  gasEvRcv <- mkTransferEvent "sender00" "NoMiner" 0.0014 "coin" v3Hash
  rcvTfr <- mkTransferEvent "" "sender00" 0.0123 "coin" v3Hash
  assertEqual "Events for txRcv" [gasEvRcv,rcvTfr] (_crEvents txRcv)

  -- rewind to savedCut (cut 18)
  void $ swapMVar (_bdbCut bdb) savedCut
  forM_ [(18 :: Int) .. 21] $ const runCut'
  runCut'

  where

    runCut' = runCut testVersion bdb pact (offsetBlockTime second) zeroNoncer

    getPWO (TestBlockDb _ pdb cmv) chid = do
      c <- readMVar cmv
      h <- fromMaybeM (userError $ "chain lookup failed for " ++ show chid) $ HM.lookup chid (_cutMap c)
      casLookupM pdb (_blockPayloadHash h)

    getBlock7 = mempty {
      mpaGetBlock = \_ _ _ bh -> if _blockChainId bh == cid then do
          t0 <- buildHashCmd bh
          t1 <- buildXSend bh
          t2 <- buildNewNativesCmd bh
          t3 <- badKeyset bh
          return $! V.fromList [t0,t1,t2,t3]
          else return mempty
      }

    chain0 = unsafeChainId 0

    getBlock22 proof pid = mempty {
      mpaGetBlock = \_ _ _ bh ->
        let go | _blockChainId bh == cid = do
                   t0 <- buildHashCmd bh
                   t1 <- buildReleaseCommand bh
                   t2 <- buildXSend bh
                   t3 <- buildNewNativesCmd bh
                   t4 <- badKeyset bh
                   return $! V.fromList [t0,t1,t2,t3,t4]
               | _blockChainId bh == chain0 = do
                   V.singleton <$> buildXReceive bh proof pid
               | otherwise = return mempty
        in go
      }

    buildHashCmd bh = buildCwCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbChainId (_blockChainId bh)
        $ set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)
        $ mkCmd (sshow bh)
        $ mkExec' "(at 'hash (describe-module 'coin))"

    badKeyset bh = buildCwCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbChainId (_blockChainId bh)
        $ set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)
        $ mkCmd (sshow bh)
        $ mkExec "(read-keyset 'ks)" $ object ["ks" .= ["badkey"::T.Text]]

    buildNewNativesCmd bh = buildCwCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbChainId (_blockChainId bh)
        $ set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)
        $ mkCmd (sshow bh)
        $ mkExec' (mconcat expressions)
      where
        expressions =
          [
            "(distinct [1 1 2 2 3 3])"
          , "(concat [\"this\" \"is\" \"a\" \"test\"])"
          , "(str-to-list \"test\")"
          , "(enumerate 1 10)"
          ]

    buildXSend bh = buildCwCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbChainId (_blockChainId bh)
        $ set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)
        $ mkCmd (sshow bh)
        $ mkExec
          "(coin.transfer-crosschain 'sender00 'sender00 (read-keyset 'k) \"0\" 0.0123)" $
          mkKeySetData "k" [sender00]

    buildXReceive bh proof pid = buildCwCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)
        $ set cbChainId chain0
        $ mkCmd (sshow bh)
        $ mkCont ((mkContMsg pid 1) { _cmProof = proof })

    buildReleaseCommand bh = buildCwCmd
        $ set cbSigners [mkSigner' sender00 [],mkSigner' allocation00KeyPair []]
        $ set cbChainId (_blockChainId bh)
        $ set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)
        $ mkCmd (sshow bh)
        $ mkExec' "(coin.release-allocation 'allocation00)"

    txResult i o = do
      case preview (ix i . _2) $ _payloadWithOutputsTransactions o of
        Nothing -> throwIO $ userError $ "no tx at " ++ show i
        Just txo -> decodeStrictOrThrow @_ @(CommandResult Hash) (_transactionOutputBytes txo)

    cbResult o = decodeStrictOrThrow @_ @(CommandResult Hash) (_coinbaseOutput $ _payloadWithOutputsCoinbase o)

    pHash = PactResult . Right . PLiteral . LString

moduleNameFork :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
moduleNameFork mpRefIO reqIO = testCase "moduleNameFork" $ do

  (q,bdb) <- reqIO

  -- install in free in block 1
  setMempool mpRefIO (moduleNameMempool "free" "test")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- install in user in block 2
  setMempool mpRefIO (moduleNameMempool "user" "test")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- do something else post-fork
  setMempool mpRefIO (moduleNameMempool "free" "test2")
  void $ runBlock q bdb second "moduleNameFork-1"
  setMempool mpRefIO (moduleNameMempool "user" "test2")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- TODO this test doesn't actually validate, I turn on Debug and make sure it
  -- goes well.

moduleNameMempool :: T.Text -> T.Text -> MemPoolAccess
moduleNameMempool ns mn = mempty
    { mpaGetBlock = getTestBlock
    }
  where
    getTestBlock _ _ _ bh = do
        let txs =
              [ "(namespace '" <> ns <> ") (module " <> mn <> " G (defcap G () (enforce false 'cannotupgrade)))"
              , ns <> "." <> mn <> ".G"
              ]
        fmap V.fromList $ forM (zip txs [0..]) $ \(code,n :: Int) ->
          buildCwCmd $
          set cbSigners [mkSigner' sender00 []] $
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

  setMempool mpRefIO $ mp tx
  -- b2 will be made at start + 30s
  void $ runBlock q bdb s30 "mempoolCreationTimeTest-2"

  where

    makeTx nonce t = buildCwCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbChainId cid
        $ set cbCreationTime (toTxCreationTime $ t)
        $ set cbTTL 300
        $ mkCmd (sshow t <> nonce)
        $ mkExec' "1"
    mp tx = mempty {
      mpaGetBlock = \valid _ _ bh -> getBlock bh tx valid
      }

    getBlock bh tx valid = do
      let txs = V.singleton tx
      oks <- valid (_blockHeight bh) (_blockHash bh) txs
      unless (V.and oks) $ throwIO $ userError "Insert failed"
      return txs


badlistNewBlockTest :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
badlistNewBlockTest mpRefIO reqIO = testCase "badlist-new-block-test" $ do
  (reqQ,_) <- reqIO
  badHashRef <- newIORef $ fromUntypedHash pactInitialHash
  badTx <- buildCwCmd
    $ set cbSigners [mkSigner' sender00 []]
    -- this should exceed the account balance
    $ set cbGasLimit 99999
    $ set cbGasPrice 1_000_000_000_000_000
    $ mkCmd "badListMPA"
    $ mkExec' "(+ 1 2)"
  setMempool mpRefIO (badlistMPA badTx badHashRef)
  newBlock noMiner (ParentHeader genesisHeader) reqQ
    >>= readMVar
    >>= expectFailureContaining "badlistNewBlockTest:newBlock" "Insufficient funds"
  badHash <- readIORef badHashRef
  assertEqual "Badlist should have badtx hash" (_cmdHash badTx) badHash
  where
    badlistMPA badTx badHashRef = mempty
      { mpaGetBlock = \_ _ _ _ -> return $ V.singleton badTx
      , mpaBadlistTx = writeIORef badHashRef
      }


goldenNewBlock :: String -> MemPoolAccess -> IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
goldenNewBlock name mp mpRefIO reqIO = golden name $ do
    (reqQ,_) <- reqIO
    setMempool mpRefIO mp
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
    getTestBlock validate bHeight bHash parent = do
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
        outtxs' <- mkTxs txs
        -- the following is done post-hash which is lame but in
        -- the goldens. TODO boldly overwrite goldens at some point of
        -- great stability
        let f = modifyPayloadWithText . set (pMeta . pmCreationTime)
            g = modifyPayloadWithText . set (pMeta . pmTTL)
            t = toTxCreationTime $ _bct $ _blockCreationTime parent
        let outtxs = flip V.map outtxs' $ \tx ->
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60
                in fmap (g ttl . f t) tx
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
          set cbSigners [mkSigner' sender00 []] $
          set cbGasPrice 0.01 $
          set cbTTL 1_000_000 $ -- match old goldens
          mkCmd ("1" <> sshow n) $
          mkExec code $
          mkKeySetData "test-admin-keyset" [sender00]
    modifyPayloadWithText f pwt = mkPayloadWithText newPayload
      where
        oldPayload = payloadObj pwt
        newPayload = f oldPayload
