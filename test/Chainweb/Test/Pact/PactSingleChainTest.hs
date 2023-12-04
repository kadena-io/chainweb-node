{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Chainweb.Test.Pact.PactSingleChainTest
( tests
) where

import Control.Arrow ((&&&))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens hiding ((.=), matching)
import Control.Monad
import Control.Monad.Catch
import Patience qualified as PatienceL
import Patience.Map qualified as PatienceM
import Patience.Map (Delta(..))

import Data.Aeson (object, (.=), Value(..), decodeStrict, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Either (isRight, fromRight)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import GHC.Stack

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Persistence
import Pact.Types.PactError
import Pact.Types.RowData
import Pact.Types.RPC
import Pact.Types.Util (fromText')

import Pact.JSON.Encode qualified as J
import Pact.JSON.Yaml

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Graph
import Chainweb.Logger (genericLogger)
import Chainweb.Mempool.Mempool
import Chainweb.MerkleLogHash (unsafeMerkleLogHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation hiding (local)
import Chainweb.Pact.Service.PactQueue (PactQueue, newPactQueue)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.PactService (runPactService)
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (emptyPayload)
import Chainweb.Payload
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils hiding (compact)
import Chainweb.Test.Pact.Utils qualified as Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Transaction (ChainwebTransaction)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)

import Chainweb.Storage.Table.RocksDB

import System.Logger.Types qualified as LL
import System.LogLevel (LogLevel(..))

testVersion :: ChainwebVersion
testVersion = slowForkingCpmTestVersion petersonChainGraph

cid :: ChainId
cid = someChainId testVersion

genesisHeader :: BlockHeader
genesisHeader = genesisBlockHeader testVersion cid

tests :: RocksDb -> TestTree
tests rdb = testGroup testName
  [ test $ goldenNewBlock "new-block-0" goldenMemPool
  , test $ goldenNewBlock "empty-block-tests" mempty
  , test newBlockAndValidate
  , test newBlockAndValidationFailure
  , test newBlockRewindValidate
  , test getHistory
  , test testHistLookup1
  , test testHistLookup2
  , test testHistLookup3
  , test badlistNewBlockTest
  , test mempoolCreationTimeTest
  , test moduleNameFork
  , test mempoolRefillTest
  , test blockGasLimitTest
  , testTimeout preInsertCheckTimeoutTest
  , rosettaFailsWithoutFullHistory rdb
  , rewindPastMinBlockHeightFails rdb
  , pactStateSamePreAndPostCompaction rdb
  , compactionIsIdempotent rdb
  , compactionUserTablesDropped rdb
  ]
  where
    testName = "Chainweb.Test.Pact.PactSingleChainTest"
    test = test' rdb
    testTimeout = testTimeout' rdb

    testHistLookup1 = getHistoricalLookupNoTxs "sender00"
      (assertSender00Bal 100_000_000 "check latest entry for sender00 after a no txs block")
    testHistLookup2 = getHistoricalLookupNoTxs "randomAccount"
      (assertEqual "Return Nothing if key absent after a no txs block" Nothing)
    testHistLookup3 = getHistoricalLookupWithTxs "sender00"
      (assertSender00Bal 9.999998042e7 "check latest entry for sender00 after block with txs")

testWithConf' :: ()
  => RocksDb
  -> (IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree)
  -> PactServiceConfig
  -> TestTree
testWithConf' rdb f conf =
  withDelegateMempool $ \dm ->
  withPactTestBlockDb testVersion cid rdb (snd <$> dm) conf $
  f (fst <$> dm)

test' :: ()
  => RocksDb
  -> (IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree)
  -> TestTree
test' rdb f = testWithConf' rdb f testPactServiceConfig

testTimeout' :: ()
  => RocksDb
  -> (IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree)
  -> TestTree
testTimeout' rdb f = testWithConf' rdb f (testPactServiceConfig { _pactPreInsertCheckTimeout = 5 })

forSuccess :: (NFData a, HasCallStack) => String -> IO (MVar (Either PactException a)) -> IO a
forSuccess msg mvio = (`catchAllSynchronous` handler) $ do
  mv <- mvio
  takeMVar mv >>= \case
    Left e -> assertFailure $ msg ++ ": got failure result: " ++ show e
    Right v -> return v
  where
    handler e = assertFailure $ msg ++ ": exception thrown: " ++ show e

runBlock :: (HasCallStack) => PactQueue -> TestBlockDb -> TimeSpan Micros -> IO PayloadWithOutputs
runBlock q bdb timeOffset = do
  ph <- getParentTestBlockDb bdb cid
  let blockTime = add timeOffset $ _bct $ _blockCreationTime ph
  nb <- forSuccess "newBlock" $
        newBlock noMiner (ParentHeader ph) q
  forM_ (chainIds testVersion) $ \c -> do
    let o | c == cid = nb
          | otherwise = emptyPayload
    addTestBlockDb bdb (Nonce 0) (\_ _ -> blockTime) c o
  nextH <- getParentTestBlockDb bdb cid
  forSuccess "newBlockAndValidate: validate" $
       validateBlock nextH (payloadWithOutputsToPayloadData nb) q

newBlockAndValidate :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
newBlockAndValidate refIO reqIO = testCase "newBlockAndValidate" $ do
  (_, q, bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool
  void $ runBlock q bdb second

newBlockAndValidationFailure :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
newBlockAndValidationFailure refIO reqIO = testCase "newBlockAndValidationFailure" $ do
  (_, q, bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool

  ph <- getParentTestBlockDb bdb cid
  let blockTime = add second $ _bct $ _blockCreationTime ph
  nb <- forSuccess ("newBlockAndValidate" <> ": newblock") $
        newBlock noMiner (ParentHeader ph) q
  forM_ (chainIds testVersion) $ \c -> do
    let o | c == cid = nb
          | otherwise = emptyPayload
    addTestBlockDb bdb (Nonce 0) (\_ _ -> blockTime) c o

  nextH <- getParentTestBlockDb bdb cid

  let nextH' = nextH { _blockPayloadHash = BlockPayloadHash $ unsafeMerkleLogHash "0000000000000000000000000000001d" }
  let nb' = nb { _payloadWithOutputsOutputsHash = BlockOutputsHash (unsafeMerkleLogHash "0000000000000000000000000000001d")}
  r <- validateBlock nextH' (payloadWithOutputsToPayloadData nb') q

  takeMVar r >>= \case
    Left (PactInternalError err) -> do
      let txHash = fromRight (error "can't parse") $ fromText' "WgnuCg6L_l6lzbjWtBfMEuPtty_uGcNrUol5HGREO_o"
      lookupResMvar <- lookupPactTxs (NoRewind cid) Nothing (V.fromList [txHash]) q
      takeMVar lookupResMvar >>= \lookupRes ->
        assertEqual "The transaction from the latest block is not at the tip point" (Right mempty) lookupRes

      assertBool "Should be BlockValidationFailure" $ "BlockValidationFailure" `T.isInfixOf` err
    _ -> assertFailure "newBlockAndValidationFailure: expected failure"

toRowData :: HasCallStack => Value -> RowData
toRowData v = case eitherDecode encV of
    Left e -> error $
        "toRowData: failed to encode as row data. " <> e <> "\n" <> show encV
    Right r -> r
  where
    encV = J.encode v

-- Test that PactService fails if Rosetta is enabled and we don't have all of
-- the history.
--
-- We do this in two stages:
--
-- 1:
--   - Start PactService with Rosetta disabled
--   - Run some blocks
--   - Compact to some arbitrary greater-than-genesis height
-- 2:
--   - Start PactService with Rosetta enabled
--   - Catch the exception that should arise at the start of PactService,
--     when performing the history check
rosettaFailsWithoutFullHistory :: ()
  => RocksDb
  -> TestTree
rosettaFailsWithoutFullHistory rdb =
  withTemporaryDir $ \iodir ->
  withSqliteDb cid iodir $ \sqlEnvIO ->
  withDelegateMempool $ \dm ->
    sequentialTestGroup "rosettaFailsWithoutFullHistory" AllSucceed
      [
        -- Run some blocks and then compact
        withPactTestBlockDb' testVersion cid rdb sqlEnvIO mempty testPactServiceConfig $ \reqIO ->
        testCase "runBlocksAndCompact" $ do
          (sqlEnv, q, bdb) <- reqIO

          mempoolRef <- fmap (pure . fst) dm

          setOneShotMempool mempoolRef goldenMemPool
          replicateM_ 10 $ void $ runBlock q bdb second

          Utils.compact LL.Error [C.NoVacuum, C.NoGrandHash] sqlEnv (C.Target (BlockHeight 5))

        -- This needs to run after the previous test
        -- Annoyingly, we must inline the PactService util starts here.
        -- ResourceT will help clean all this up
      , testCase "PactService Should fail" $ do
          pactQueue <- newPactQueue 2000
          blockDb <- mkTestBlockDb testVersion rdb
          bhDb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb blockDb) cid
          sqlEnv <- sqlEnvIO
          mempool <- fmap snd dm
          let payloadDb = _bdbPayloadDb blockDb
          let cfg = testPactServiceConfig { _pactFullHistoryRequired = True }
          let logger = genericLogger System.LogLevel.Error (\_ -> return ())
          e <- try $ runPactService testVersion cid logger pactQueue mempool bhDb payloadDb sqlEnv cfg
          case e of
            Left (FullHistoryRequired {}) -> do
              pure ()
            Left err -> do
              assertFailure $ "Expected FullHistoryRequired exception, instead got: " ++ show err
            Right _ -> do
              assertFailure "Expected FullHistoryRequired exception, instead there was no exception at all."
      ]

rewindPastMinBlockHeightFails :: ()
  => RocksDb
  -> TestTree
rewindPastMinBlockHeightFails rdb =
  compactionSetup "rewindPastMinBlockHeightFails" rdb testPactServiceConfig $ \cr -> do
    setOneShotMempool cr.mempoolRef goldenMemPool
    replicateM_ 10 $ runBlock cr.pactQueue cr.blockDb second

    Utils.compact LL.Error [C.NoVacuum, C.NoGrandHash] cr.sqlEnv (C.Target (BlockHeight 5))

    -- Genesis block header; compacted away by now
    let bh = genesisBlockHeader testVersion cid

    syncResult <- readMVar =<< pactSyncToBlock bh cr.pactQueue
    case syncResult of
      Left (PactInternalError {}) -> do
        return ()
      Left err -> do
        assertFailure $ "Expected a PactInternalError, but got: " ++ show err
      Right _ -> do
        assertFailure "Expected an exception, but didn't encounter one."

pactStateSamePreAndPostCompaction :: ()
  => RocksDb
  -> TestTree
pactStateSamePreAndPostCompaction rdb =
  compactionSetup "pactStateSamePreAndPostCompaction" rdb testPactServiceConfig $ \cr -> do
    let numBlocks :: Num a => a
        numBlocks = 100

    setOneShotMempool cr.mempoolRef goldenMemPool

    let makeTx :: Int -> BlockHeader -> IO ChainwebTransaction
        makeTx nth bh = buildCwCmd testVersion
          $ set cbSigners [mkEd25519Signer' sender00 [mkGasCap, mkTransferCap "sender00" "sender01" 1.0]]
          $ setFromHeader bh
          $ mkCmd (sshow (nth, bh))
          $ mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)"

    supply <- newIORef @Int 0
    madeTx <- newIORef @Bool False
    replicateM_ numBlocks $ do
      setMempool cr.mempoolRef $ mempty {
        mpaGetBlock = \_ _ _ _ bh -> do
          madeTxYet <- readIORef madeTx
          if madeTxYet
          then do
            pure mempty
          else do
            n <- atomicModifyIORef' supply $ \a -> (a + 1, a)
            tx <- makeTx n bh
            writeIORef madeTx True
            pure $ V.fromList [tx]
      }
      void $ runBlock cr.pactQueue cr.blockDb second
      writeIORef madeTx False

    let db = _sConn cr.sqlEnv

    statePreCompaction <- getLatestPactState db

    Utils.compact LL.Error [C.NoVacuum, C.NoGrandHash] cr.sqlEnv (C.Target (BlockHeight numBlocks))

    statePostCompaction <- getLatestPactState db

    let stateDiff = M.filter (not . PatienceM.isSame) (PatienceM.diff statePreCompaction statePostCompaction)
    when (not (null stateDiff)) $ do
      T.putStrLn ""
      forM_ (M.toList stateDiff) $ \(tbl, delta) -> do
        T.putStrLn ""
        T.putStrLn tbl
        case delta of
          Same _ -> do
            pure ()
          Old x -> do
            putStrLn $ "a pre-only value appeared in the pre- and post-compaction diff: " ++ show x
          New x -> do
            putStrLn $ "a post-only value appeared in the pre- and post-compaction diff: " ++ show x
          Delta x1 x2 -> do
            let daDiff = M.filter (not . PatienceM.isSame) (PatienceM.diff x1 x2)
            forM_ daDiff $ \item -> do
              case item of
                Old x -> do
                  putStrLn $ "old: " ++ show x
                New x -> do
                  putStrLn $ "new: " ++ show x
                Same _ -> do
                  pure ()
                Delta x y -> do
                  putStrLn $ "old: " ++ show x
                  putStrLn $ "new: " ++ show y
                  putStrLn ""
      assertFailure "pact state check failed"

compactionIsIdempotent :: ()
  => RocksDb
  -> TestTree
compactionIsIdempotent rdb =
  compactionSetup "compactionIdempotent" rdb testPactServiceConfig $ \cr -> do
    let numBlocks :: Num a => a
        numBlocks = 100

    setOneShotMempool cr.mempoolRef goldenMemPool

    let makeTx :: Int -> BlockHeader -> IO ChainwebTransaction
        makeTx nth bh = buildCwCmd testVersion
          $ set cbSigners [mkEd25519Signer' sender00 [mkGasCap, mkTransferCap "sender00" "sender01" 1.0]]
          $ setFromHeader bh
          $ mkCmd (sshow (nth, bh))
          $ mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)"

    supply <- newIORef @Int 0
    madeTx <- newIORef @Bool False
    replicateM_ numBlocks $ do
      setMempool cr.mempoolRef $ mempty {
        mpaGetBlock = \_ _ _ _ bh -> do
          madeTxYet <- readIORef madeTx
          if madeTxYet
          then do
            pure mempty
          else do
            n <- atomicModifyIORef' supply $ \a -> (a + 1, a)
            tx <- makeTx n bh
            writeIORef madeTx True
            pure $ V.fromList [tx]
      }
      void $ runBlock cr.pactQueue cr.blockDb second
      writeIORef madeTx False

    let db = _sConn cr.sqlEnv

    let compact h =
          Utils.compact LL.Error [C.NoVacuum, C.NoGrandHash] cr.sqlEnv h

    let compactionHeight = C.Target (BlockHeight numBlocks)
    compact compactionHeight
    statePostCompaction1 <- getPactUserTables db
    compact compactionHeight
    statePostCompaction2 <- getPactUserTables db

    let stateDiff = M.filter (not . PatienceM.isSame) (PatienceM.diff statePostCompaction1 statePostCompaction2)
    when (not (null stateDiff)) $ do
      T.putStrLn ""
      forM_ (M.toList stateDiff) $ \(tbl, delta) -> do
        T.putStrLn ""
        T.putStrLn tbl
        case delta of
          Same _ -> do
            pure ()
          Old x -> do
            putStrLn $ "a pre-only value appeared in the compaction idempotency diff: " ++ show x
          New x -> do
            putStrLn $ "a post-only value appeared in the compaction idempotency diff: " ++ show x
          Delta x1 x2 -> do
            let daDiff = PatienceL.pairItems (\a b -> rowKey a == rowKey b) (PatienceL.diff x1 x2)
            forM_ daDiff $ \item -> do
              case item of
                Old x -> do
                  putStrLn $ "old: " ++ show x
                New x -> do
                  putStrLn $ "new: " ++ show x
                Same _ -> do
                  pure ()
                Delta x y -> do
                  putStrLn $ "old: " ++ show x
                  putStrLn $ "new: " ++ show y
                  putStrLn ""
      assertFailure "pact state check failed"

-- | Test that user tables created before the compaction height are kept,
--   while those created after the compaction height are dropped.
compactionUserTablesDropped :: ()
  => RocksDb
  -> TestTree
compactionUserTablesDropped rdb =
  let
    -- creating a module uses about 60k gas. this is
    -- that plus some change.
    gasLimit :: GasLimit
    gasLimit = 70_000

    pactCfg = testPactServiceConfig {
      _pactBlockGasLimit = gasLimit
    }
  in
  compactionSetup "compactionUserTablesDropped" rdb pactCfg $ \cr -> do
    let numBlocks :: Num a => a
        numBlocks = 100
    let halfwayPoint :: Integral a => a
        halfwayPoint = numBlocks `div` 2

    setOneShotMempool cr.mempoolRef goldenMemPool

    let createTable :: Int -> Text -> IO ChainwebTransaction
        createTable n tblName = do
          let tx = T.unlines
                [ "(namespace 'free)"
                , "(module m" <> sshow n <> " G"
                , "  (defcap G () true)"
                , "  (defschema empty-schema)"
                , "  (deftable " <> tblName <> ":{empty-schema})"
                , ")"
                , "(create-table " <> tblName <> ")"
                ]
          buildCwCmd testVersion
            $ signSender00
            $ set cbGasLimit gasLimit
            $ mkCmd ("createTable-" <> tblName <> "-" <> sshow n)
            $ mkExec tx
            $ mkKeySetData "sender00" [sender00]

    let beforeTable = "test_before"
    let afterTable = "test_after"

    supply <- newIORef @Int 0
    madeBeforeTable <- newIORef @Bool False
    madeAfterTable <- newIORef @Bool False
    replicateM_ numBlocks $ do
      setMempool cr.mempoolRef $ mempty {
        mpaGetBlock = \_ _ mBlockHeight _ _ -> do
          let mkTable madeRef tbl = do
                madeYet <- readIORef madeRef
                if madeYet
                then do
                  pure mempty
                else do
                  n <- atomicModifyIORef' supply $ \a -> (a + 1, a)
                  tx <- createTable n tbl
                  writeIORef madeRef True
                  pure (V.fromList [tx])

          if mBlockHeight <= halfwayPoint
          then do
            mkTable madeBeforeTable beforeTable
          else do
            mkTable madeAfterTable afterTable
      }
      void $ runBlock cr.pactQueue cr.blockDb second

    let freeBeforeTbl = "free.m0_" <> beforeTable
    let freeAfterTbl = "free.m1_" <> afterTable

    let db = _sConn cr.sqlEnv

    statePre <- getPactUserTables db
    let assertExists tbl = do
          let msg = "Table " ++ T.unpack tbl ++ " should exist pre-compaction, but it doesn't."
          assertBool msg (isJust (M.lookup tbl statePre))
    assertExists freeBeforeTbl
    assertExists freeAfterTbl

    Utils.compact LL.Error [C.NoVacuum, C.NoGrandHash] cr.sqlEnv (C.Target (BlockHeight halfwayPoint))

    statePost <- getPactUserTables db
    flip assertBool (isJust (M.lookup freeBeforeTbl statePost)) $
      T.unpack beforeTable ++ " was dropped; it wasn't supposed to be."

    flip assertBool (isNothing (M.lookup freeAfterTbl statePost)) $
      T.unpack afterTable ++ " wasn't dropped; it was supposed to be."

getHistory :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
getHistory refIO reqIO = testCase "getHistory" $ do
  (_, q, bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool
  void $ runBlock q bdb second
  h <- getParentTestBlockDb bdb cid
  mv <- pactBlockTxHistory h (UserTables "coin_coin-table") q

  (BlockTxHistory hist prevBals) <- forSuccess "getHistory" (return mv)
  -- just check first one here
  assertEqual "check first entry of history"
    (Just [TxLog "coin_coin-table" "sender00"
      (toRowData $ object
       [ "guard" .= object
         [ "pred" .= ("keys-all" :: T.Text)
         , "keys" .=
           ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text]
         ]
       , "balance" .= Number 99_999_900.0
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
        (toRowData $ object
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
    -> (Maybe (TxLog RowData) -> IO ())
    -> IO (IORef MemPoolAccess)
    -> IO (SQLiteEnv, PactQueue, TestBlockDb)
    -> TestTree
getHistoricalLookupNoTxs key assertF refIO reqIO =
  testCase (T.unpack ("getHistoricalLookupNoTxs: " <> key)) $ do
    (_, q, bdb) <- reqIO
    setOneShotMempool refIO mempty
    void $ runBlock q bdb second
    h <- getParentTestBlockDb bdb cid
    histLookup q h key >>= assertF

getHistoricalLookupWithTxs
    :: T.Text
    -> (Maybe (TxLog RowData) -> IO ())
    -> IO (IORef MemPoolAccess)
    -> IO (SQLiteEnv, PactQueue, TestBlockDb)
    -> TestTree
getHistoricalLookupWithTxs key assertF refIO reqIO =
  testCase (T.unpack ("getHistoricalLookupWithTxs: " <> key)) $ do
    (_, q, bdb) <- reqIO
    setOneShotMempool refIO goldenMemPool
    void $ runBlock q bdb second
    h <- getParentTestBlockDb bdb cid
    histLookup q h key >>= assertF

histLookup :: PactQueue -> BlockHeader -> T.Text -> IO (Maybe (TxLog RowData))
histLookup q bh k = do
  mv <- pactHistoricalLookup bh (UserTables "coin_coin-table") (RowKey k) q
  forSuccess "histLookup" (return mv)

assertSender00Bal :: Rational -> String -> Maybe (TxLog RowData) -> Assertion
assertSender00Bal bal msg hist =
  assertEqual msg
    (Just (TxLog "coin_coin-table" "sender00"
      (toRowData $ object
        [ "guard" .= object
          [ "pred" .= ("keys-all" :: T.Text)
          , "keys" .=
            ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text]
          ]
        , "balance" .= Number (fromRational bal)
        ])))
    hist

newBlockRewindValidate :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
newBlockRewindValidate mpRefIO reqIO = testCase "newBlockRewindValidate" $ do
  (_, q, bdb) <- reqIO
  setOneShotMempool mpRefIO chainDataMemPool
  cut0 <- readMVar $ _bdbCut bdb -- genesis cut

  -- cut 1a
  void $ runBlock q bdb second
  cut1a <- readMVar $ _bdbCut bdb

  -- rewind, cut 1b
  void $ swapMVar (_bdbCut bdb) cut0
  void $ runBlock q bdb second

  -- rewind to cut 1a to trigger replay with chain data bug
  void $ swapMVar (_bdbCut bdb) cut1a
  void $ runBlock q bdb (secondsToTimeSpan 2)

  where

    chainDataMemPool = mempty {
      mpaGetBlock = \_ _ _ _ bh -> do
          fmap V.singleton $ buildCwCmd testVersion
              $ signSender00
              $ setFromHeader bh
              $ mkCmd (sshow bh)
              $ mkExec' "(chain-data)"
      }

signSender00 :: CmdBuilder -> CmdBuilder
signSender00 = set cbSigners [mkEd25519Signer' sender00 []]

setFromHeader :: BlockHeader -> CmdBuilder -> CmdBuilder
setFromHeader bh =
  set cbChainId (_blockChainId bh)
  . set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)


pattern BlockGasLimitError :: forall b. Either PactException b
pattern BlockGasLimitError <-
  Left (PactInternalError (decodeStrict . T.encodeUtf8 -> Just (PactExceptionTag "BlockGasLimitExceeded")))

-- this test relies on block gas errors being thrown before other Pact errors.
blockGasLimitTest :: HasCallStack => IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
blockGasLimitTest _ reqIO = testCase "blockGasLimitTest" $ do
  (_, q, _) <- reqIO

  let
    useGas g = do
      bigTx <- buildCwCmd testVersion $ set cbGasLimit g $ signSender00 $ mkCmd "cmd" $ mkExec' "TESTING"
      let
        cr = CommandResult
          (RequestKey (Hash "0")) Nothing
          (PactResult $ Left $ PactError EvalError (Pact.Types.Info.Info Nothing) [] mempty)
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

mempoolRefillTest :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
mempoolRefillTest mpRefIO reqIO = testCase "mempoolRefillTest" $ do

  (_, q, bdb) <- reqIO
  supply <- newMVar (0 :: Int)

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [badTx] ) ]
  runBlock q bdb second >>= checkCount 2

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [goodTx, badTx] ) ]
  runBlock q bdb second >>= checkCount 3

  mp supply [ ( 0, [badTx, goodTx] ), ( 1, [goodTx, badTx] ) ]
  runBlock q bdb second >>= checkCount 2

  mp supply [ ( 0, [badTx] ), ( 1, [goodTx, goodTx] ) ]
  runBlock q bdb second >>= checkCount 2

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [badTx, badTx] ) ]
  runBlock q bdb second >>= checkCount 2

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

    goodTx i bh = buildCwCmd testVersion
        $ signSender00
        $ mkCmd' bh (sshow (i,bh))
        $ mkExec' "(+ 1 2)"

    badTx i bh = buildCwCmd testVersion
        $ signSender00
        $ set cbSender "bad"
        $ mkCmd' bh (sshow (i,bh))
        $ mkExec' "(+ 1 2)"


    mkCmd' :: BlockHeader -> T.Text -> PactRPC T.Text -> CmdBuilder
    mkCmd' bh nonce =
      setFromHeader bh
      . mkCmd nonce

moduleNameFork :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
moduleNameFork mpRefIO reqIO = testCase "moduleNameFork" $ do

  (_, q, bdb) <- reqIO

  -- install in free in block 1
  setOneShotMempool mpRefIO (moduleNameMempool "free" "test")
  void $ runBlock q bdb second

  -- install in user in block 2
  setOneShotMempool mpRefIO (moduleNameMempool "user" "test")
  void $ runBlock q bdb second

  -- do something else post-fork
  setOneShotMempool mpRefIO (moduleNameMempool "free" "test2")
  void $ runBlock q bdb second
  setOneShotMempool mpRefIO (moduleNameMempool "user" "test2")
  void $ runBlock q bdb second

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
          buildCwCmd testVersion $
          signSender00 $
          set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh) $
          mkCmd ("1" <> sshow n) $
          mkExec' code


mempoolCreationTimeTest :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
mempoolCreationTimeTest mpRefIO reqIO = testCase "mempoolCreationTimeTest" $ do

  (_, q, bdb) <- reqIO

  let start@(Time startSpan) :: Time Micros = Time (TimeSpan (Micros 100_000_000))
      s30 = scaleTimeSpan (30 :: Int) second
      s15 = scaleTimeSpan (15 :: Int) second
  -- b1 block time is start
  void $ runBlock q bdb startSpan


  -- do pre-insert check with transaction at start + 15s
  tx <- makeTx "tx-now" (add s15 start)
  void $ forSuccess "mempoolCreationTimeTest: pre-insert tx" $
    pactPreInsertCheck (V.singleton tx) q

  setOneShotMempool mpRefIO $ mp tx
  -- b2 will be made at start + 30s
  void $ runBlock q bdb s30

  where

    makeTx nonce t = buildCwCmd testVersion
        $ signSender00
        $ set cbChainId cid
        $ set cbCreationTime (toTxCreationTime t)
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

preInsertCheckTimeoutTest :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
preInsertCheckTimeoutTest _ reqIO = testCase "preInsertCheckTimeoutTest" $ do
  (_, q, _) <- reqIO

  coinV3 <- T.readFile "pact/coin-contract/v3/coin-v3.pact"
  coinV4 <- T.readFile "pact/coin-contract/v4/coin-v4.pact"
  coinV5 <- T.readFile "pact/coin-contract/v5/coin-v5.pact"

  txCoinV3 <- buildCwCmd testVersion
        $ signSender00
        $ set cbChainId cid
        $ mkCmd "tx-now-coinv3"
        $ mkExec' coinV3

  txCoinV4 <- buildCwCmd testVersion
        $ signSender00
        $ set cbChainId cid
        $ mkCmd "tx-now-coinv4"
        $ mkExec' coinV4

  txCoinV5 <- buildCwCmd testVersion
        $ signSender00
        $ set cbChainId cid
        $ mkCmd "tx-now-coinv5"
        $ mkExec' coinV5

  rs <- forSuccess "preInsertCheckTimeoutTest" $ pactPreInsertCheck (V.fromList [txCoinV3, txCoinV4, txCoinV5]) q
  assertBool ("should be InsertErrorTimedOut but got " ++ show rs) $ V.and $ V.map (== Left InsertErrorTimedOut) rs

badlistNewBlockTest :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
badlistNewBlockTest mpRefIO reqIO = testCase "badlistNewBlockTest" $ do
  (_, reqQ, _) <- reqIO
  let hashToTxHashList = V.singleton . requestKeyToTransactionHash . RequestKey . toUntypedHash @'Blake2b_256
  badHashRef <- newIORef $ hashToTxHashList initialHash
  badTx <- buildCwCmd testVersion
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


goldenNewBlock :: String -> MemPoolAccess -> IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
goldenNewBlock name mp mpRefIO reqIO = golden name $ do
    (_, reqQ, _) <- reqIO
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
    goldenBytes a = return $ BL.fromStrict $ encodeYaml $ object
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
          buildCwCmd testVersion $
          signSender00 $
          set cbGasPrice 0.01 $
          set cbTTL 1_000_000 $ -- match old goldens
          mkCmd ("1" <> sshow n) $
          mkExec code $
          mkKeySetData "test-admin-keyset" [sender00]

data CompactionResources = CompactionResources
  { mempoolRef :: IO (IORef MemPoolAccess)
  , mempool :: MemPoolAccess
  , sqlEnv :: SQLiteEnv
  , pactQueue :: PactQueue
  , blockDb :: TestBlockDb
  }

compactionSetup :: ()
  => String
     -- ^ test pattern
  -> RocksDb
  -> PactServiceConfig
  -> (CompactionResources -> IO ())
  -> TestTree
compactionSetup pat rdb pactCfg f =
  withTemporaryDir $ \iodir ->
  withSqliteDb cid iodir $ \sqlEnvIO ->
  withDelegateMempool $ \dm ->
    testCase pat $ do
      blockDb <- mkTestBlockDb testVersion rdb
      bhDb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb blockDb) cid
      let payloadDb = _bdbPayloadDb blockDb
      sqlEnv <- sqlEnvIO
      (mempoolRef, mempool) <- do
        (ref, nonRef) <- dm
        pure (pure ref, nonRef)
      pactQueue <- newPactQueue 2000

      let logger = genericLogger System.LogLevel.Error (\_ -> return ())

      void $ forkIO $ runPactService testVersion cid logger pactQueue mempool bhDb payloadDb sqlEnv pactCfg

      f $ CompactionResources
        { mempoolRef = mempoolRef
        , mempool = mempool
        , sqlEnv = sqlEnv
        , pactQueue = pactQueue
        , blockDb = blockDb
        }
