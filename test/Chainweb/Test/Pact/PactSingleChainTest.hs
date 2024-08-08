{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.PactSingleChainTest
( tests
) where

import Control.Arrow ((&&&))
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens hiding ((.=), matching)
import Control.Monad
import Control.Monad.Catch
import Data.Ord (Down(..))
import Patience.Map qualified as PatienceM
import Patience.Map (Delta(..))
import Streaming.Prelude qualified as S

import Data.Int (Int64)
import Data.Aeson (object, (.=), Value(..), eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight, fromRight)
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Database.SQLite3 qualified as Lite

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
import Pact.Types.Util (fromText')

import Pact.JSON.Encode qualified as J
import Pact.JSON.Yaml

import Chainweb.BlockCreationTime
import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader.Internal
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Graph
import Chainweb.Logger (genericLogger)
import Chainweb.Mempool.Mempool
import Chainweb.MerkleLogHash (unsafeMerkleLogHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.PactState.GrandHash.Algorithm (computeGrandHash)
import Chainweb.Pact.Backend.PactState qualified as PS
import Chainweb.Pact.Backend.PactState (PactRowContents(..))
import Chainweb.Pact.Backend.Types hiding (RunnableBlock(..))
import Chainweb.Pact.Service.BlockValidation hiding (local)
import Chainweb.Pact.Service.PactQueue (PactQueue, newPactQueue)
import Chainweb.Pact.Service.Types hiding (runBlock)
import Chainweb.Pact.PactService (runPactService)
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (emptyPayload)
import Chainweb.Payload
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Pact.Utils qualified as Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Transaction (ChainwebTransaction)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact

import Chainweb.Storage.Table.RocksDB

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
  , test $ goldenNewBlock "empty-block-tests" (return mempty)
  , test newBlockAndValidate
  , test newBlockNoFill
  , test newBlockAndContinue
  , test newBlockAndValidationFailure
  -- this test needs to see all of the writes done in the block;
  -- it uses the BlockTxHistory Pact request to see them.
  , testWithConf getHistory
    testPactServiceConfig { _pactPersistIntraBlockWrites = PersistIntraBlockWrites }
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
  , compactionGrandHashUnchanged rdb
  , compactionDoesNotDisruptDuplicateDetection rdb
  , compactionResilientToRowIdOrdering rdb
  ]
  where
    testName = "Chainweb.Test.Pact.PactSingleChainTest"
    test = test' rdb
    testWithConf = testWithConf' rdb
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
testTimeout' rdb f = testWithConf' rdb f (testPactServiceConfig { _pactPreInsertCheckTimeout = 1 })

forSuccess :: (NFData a, HasCallStack) => String -> IO (Either PactException a) -> IO a
forSuccess msg act = (`catchAllSynchronous` handler) $ do
  r <- act
  case r of
    Left e -> assertFailure $ msg ++ ": got failure result: " ++ show e
    Right v -> return v
  where
    handler e = assertFailure $ msg ++ ": exception thrown: " ++ show e

runBlockE :: (HasCallStack) => PactQueue -> TestBlockDb -> TimeSpan Micros -> IO (Either PactException PayloadWithOutputs)
runBlockE q bdb timeOffset = do
  ph <- getParentTestBlockDb bdb cid
  bip <- throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader ph) q
  let nb = blockInProgressToPayloadWithOutputs bip
  let blockTime = add timeOffset $ _bct $ view blockCreationTime ph
  forM_ (chainIds testVersion) $ \c -> do
    let o | c == cid = nb
          | otherwise = emptyPayload
    addTestBlockDb bdb (succ $ view blockHeight ph) (Nonce 0) (\_ _ -> blockTime) c o
  nextH <- getParentTestBlockDb bdb cid
  try (validateBlock nextH (CheckablePayloadWithOutputs nb) q)

-- edmund: why does any of this return PayloadWithOutputs instead of a
-- list of Pact CommandResult?
runBlock :: (HasCallStack) => PactQueue -> TestBlockDb -> TimeSpan Micros -> IO PayloadWithOutputs
runBlock q bdb timeOffset = do
  forSuccess "newBlockAndValidate: validate" $
    runBlockE q bdb timeOffset

newBlockAndValidate :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
newBlockAndValidate refIO reqIO = testCase "newBlockAndValidate" $ do
  (_, q, bdb) <- reqIO
  setOneShotMempool refIO =<< goldenMemPool
  void $ runBlock q bdb second

newBlockAndContinue :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
newBlockAndContinue refIO reqIO = testCase "newBlockAndContinue" $ do
  (_, q, bdb) <- reqIO
  let mk = signSender00 . set cbGasPrice 0.01 . set cbTTL 1_000_000
  c1 <- buildCwCmd "1" testVersion $
        mk $
        set cbRPC (mkExec "(+ 1 2)" (object [])) $
        defaultCmd
  c2 <- buildCwCmd "2" testVersion $
        mk $
        set cbRPC (mkExec "(+ 3 4)" (object [])) $
        defaultCmd
  c3 <- buildCwCmd "3" testVersion $
        mk $
        set cbRPC (mkExec "(+ 5 6)" (object [])) $
        defaultCmd
  setMempool refIO =<< mempoolOf
    [ V.fromList [ c1 ]
    , mempty
    , V.fromList [ c2 ]
    , mempty
    , V.fromList [ c3 ]
    ]

  bipStart <- throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader genesisHeader) q
  let ParentHeader ph = _blockInProgressParentHeader bipStart
  bipContinued <- throwIfNoHistory =<< continueBlock bipStart q
  bipFinal <- throwIfNoHistory =<< continueBlock bipContinued q
  -- we must make progress on the same parent header
  assertEqual "same parent header after continuing block"
    (_blockInProgressParentHeader bipStart) (_blockInProgressParentHeader bipContinued)
  assertBool "made progress (1)"
    (bipStart /= bipContinued)
  assertEqual "same parent header after finishing block"
    (_blockInProgressParentHeader bipContinued) (_blockInProgressParentHeader bipFinal)
  assertBool "made progress (2)"
    (bipContinued /= bipFinal)
  let nbContinued = blockInProgressToPayloadWithOutputs bipFinal
  -- add block to database
  let blockTime = add second $ _bct $ view blockCreationTime ph
  forM_ (chainIds testVersion) $ \c -> do
    let o | c == cid = nbContinued
          | otherwise = emptyPayload
    addTestBlockDb bdb (succ $ view blockHeight ph) (Nonce 0) (\_ _ -> blockTime) c o
  nextH <- getParentTestBlockDb bdb cid
  -- a continued block must be valid
  _ <- validateBlock nextH (CheckablePayloadWithOutputs nbContinued) q

  -- reset to parent
  pactSyncToBlock ph q
  setMempool refIO =<< mempoolOf
    [ V.fromList
      [ c1, c2, c3 ]
    ]
  bipAllAtOnce <- throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader genesisHeader) q
  let nbAllAtOnce = blockInProgressToPayloadWithOutputs bipAllAtOnce
  assertEqual "a continued block, and one that's all done at once, should be exactly equal"
    nbContinued nbAllAtOnce
  _ <- validateBlock nextH (CheckablePayloadWithOutputs nbAllAtOnce) q

  return ()

newBlockNoFill :: IO (IORef MemPoolAccess)
               -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
newBlockNoFill refIO reqIO = testCase "newBlockNoFill" $ do
  (_, q, _) <- reqIO
  c1 <- buildCwCmd "1" testVersion $
    signSender00 $
    set cbGasPrice 0.01 $
    set cbTTL 1_000_000 $
    set cbRPC (mkExec "1" (object [])) $
    defaultCmd
  setMempool refIO =<< mempoolOf [V.fromList [c1]]
  noFillPwo <- fmap blockInProgressToPayloadWithOutputs . throwIfNoHistory =<<
    newBlock noMiner NewBlockEmpty (ParentHeader genesisHeader) q
  assertEqual
    "an unfilled newblock must have no transactions, even with a full mempool"
    mempty
    (_payloadWithOutputsTransactions noFillPwo)
  fillPwo <- fmap blockInProgressToPayloadWithOutputs . throwIfNoHistory =<<
    newBlock noMiner NewBlockFill (ParentHeader genesisHeader) q
  assertEqual
    "an filled newblock has transactions with a full mempool"
    1
    (V.length $ _payloadWithOutputsTransactions fillPwo)

newBlockAndValidationFailure :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
newBlockAndValidationFailure refIO reqIO = testCase "newBlockAndValidationFailure" $ do
  (_, q, bdb) <- reqIO
  setOneShotMempool refIO =<< goldenMemPool

  bip <- throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader genesisHeader) q
  let nb = blockInProgressToPayloadWithOutputs bip
  let blockTime = add second $ _bct $ view blockCreationTime genesisHeader
  forM_ (chainIds testVersion) $ \c -> do
    let o | c == cid = nb
          | otherwise = emptyPayload
    addTestBlockDb bdb (succ $ view blockHeight genesisHeader) (Nonce 0) (\_ _ -> blockTime) c o

  nextH <- getParentTestBlockDb bdb cid

  let nextH' = nextH
        & blockPayloadHash .~ BlockPayloadHash (unsafeMerkleLogHash "0000000000000000000000000000001d")
  let nb' = nb { _payloadWithOutputsOutputsHash = BlockOutputsHash (unsafeMerkleLogHash "0000000000000000000000000000001d")}
  try (validateBlock nextH' (CheckablePayloadWithOutputs nb') q) >>= \case
    Left BlockValidationFailure {} -> do
      let txHash = fromRight (error "can't parse") $ fromText' "WgnuCg6L_l6lzbjWtBfMEuPtty_uGcNrUol5HGREO_o"
      lookupRes <- lookupPactTxs Nothing (V.fromList [txHash]) q
      assertEqual "The transaction from the latest block is not at the tip point" mempty lookupRes

    _ -> assertFailure "newBlockAndValidationFailure: expected BlockValidationFailure"

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
  withTemporaryDir $ \srcDir -> withSqliteDb cid srcDir $ \srcSqlEnvIO ->
  withTemporaryDir $ \targetDir -> withSqliteDb cid targetDir $ \targetSqlEnvIO ->
  withDelegateMempool $ \dm ->
    sequentialTestGroup "rosettaFailsWithoutFullHistory" AllSucceed
      [
        -- Run some blocks and then compact
        withPactTestBlockDb' testVersion cid rdb srcSqlEnvIO mempty testPactServiceConfig $ \reqIO ->
        testCase "runBlocksAndCompact" $ do
          (srcSqlEnv, q, bdb) <- reqIO

          mempoolRef <- fmap (pure . fst) dm

          setOneShotMempool mempoolRef =<< goldenMemPool
          replicateM_ 10 $ void $ runBlock q bdb second

          targetSqlEnv <- targetSqlEnvIO
          Utils.sigmaCompact srcSqlEnv targetSqlEnv (BlockHeight 5)

        -- This needs to run after the previous test
        -- Annoyingly, we must inline the PactService util starts here.
        -- ResourceT will help clean all this up
      , testCase "PactService Should fail" $ do
          pactQueue <- newPactQueue 2000
          blockDb <- mkTestBlockDb testVersion rdb
          bhDb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb blockDb) cid
          sqlEnv <- targetSqlEnvIO
          mempool <- fmap snd dm
          let payloadDb = _bdbPayloadDb blockDb
          let cfg = testPactServiceConfig { _pactFullHistoryRequired = True }
          let logger = genericLogger System.LogLevel.Error (\_ -> return ())
          e <- try $ runPactService testVersion cid logger Nothing pactQueue mempool bhDb payloadDb sqlEnv cfg
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
    replicateM_ 10 $ runBlock cr.srcPactQueue cr.blockDb second

    Utils.sigmaCompact cr.srcSqlEnv cr.targetSqlEnv (BlockHeight 5)

    -- Genesis block header; compacted away by now
    let bh = genesisBlockHeader testVersion cid

    syncResult <- try (pactSyncToBlock bh cr.targetPactQueue)
    case syncResult of
      Left (BlockHeaderLookupFailure {}) -> do
        return ()
      Left err -> do
        assertFailure $ "Expected a BlockHeaderLookupFailure, but got: " ++ show err
      Right _ -> do
        assertFailure "Expected an exception, but didn't encounter one."

pactStateSamePreAndPostCompaction :: ()
  => RocksDb
  -> TestTree
pactStateSamePreAndPostCompaction rdb =
  compactionSetup "pactStateSamePreAndPostCompaction" rdb testPactServiceConfig $ \cr -> do
    let numBlocks :: Num a => a
        numBlocks = 100

    let makeTx :: Word -> BlockHeader -> IO ChainwebTransaction
        makeTx nth bh = buildCwCmd (sshow (nth, bh)) testVersion
          $ set cbSigners [mkEd25519Signer' sender00 [mkGasCap, mkTransferCap "sender00" "sender01" 1.0]]
          $ setFromHeader bh
          $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)")
          $ defaultCmd

    replicateM_ numBlocks $ do
      runBlockWithTx_ cr.mempoolRef cr.srcPactQueue cr.blockDb
        $ \n _ _ bHeader -> makeTx n bHeader

    statePreCompaction <- getLatestPactState cr.srcSqlEnv
    Utils.sigmaCompact cr.srcSqlEnv cr.targetSqlEnv (BlockHeight numBlocks)
    statePostCompaction <- getLatestPactState cr.targetSqlEnv

    comparePactStateBeforeAndAfter statePreCompaction statePostCompaction

compactionIsIdempotent :: ()
  => RocksDb
  -> TestTree
compactionIsIdempotent rdb =
  -- This requires a bit more than 'compactionSetup', since we
  -- are compacting more than once.
  withTemporaryDir $ \twiceDir -> withSqliteDb cid twiceDir $ \twiceSqlEnvIO ->
  compactionSetup "compactionIsIdempotent" rdb testPactServiceConfig $ \cr -> do
    let numBlocks :: Num a => a
        numBlocks = 100

    let makeTx :: Word -> BlockHeader -> IO ChainwebTransaction
        makeTx nth bh = buildCwCmd (sshow (nth, bh)) testVersion
          $ set cbSigners [mkEd25519Signer' sender00 [mkGasCap, mkTransferCap "sender00" "sender01" 1.0]]
          $ setFromHeader bh
          $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)")
          $ defaultCmd

    replicateM_ numBlocks $ do
      runBlockWithTx_ cr.mempoolRef cr.srcPactQueue cr.blockDb
        $ \n _ _ bHeader -> makeTx n bHeader

    twiceSqlEnv <- twiceSqlEnvIO
    let targetHeight = BlockHeight numBlocks
    -- Compact 'src' into 'target'
    Utils.sigmaCompact cr.srcSqlEnv cr.targetSqlEnv targetHeight
    -- Get table contents of 'target'
    statePostCompaction1 <- getPactUserTables cr.targetSqlEnv
    -- Compact 'target' into 'twice'
    Utils.sigmaCompact cr.targetSqlEnv twiceSqlEnv targetHeight
    -- Get table state of 'twice'
    statePostCompaction2 <- getPactUserTables twiceSqlEnv

    -- In order to use `comparePactStateBeforeAndAfter`, we need to ensure that the rows are properly compacted,
    -- and then put them into a map.
    let ensureIsCompactedAndSortRows :: M.Map Text [PactRow] -> IO (M.Map Text (M.Map Text PactRowContents))
        ensureIsCompactedAndSortRows state = do
          flip M.traverseWithKey state $ \_ rows -> do
            let sortedRows = List.sort rows
            assertBool "Each rowkey only has one entry" $
              List.sort (List.nubBy (\r1 r2 -> r1.rowKey == r2.rowKey) rows) == sortedRows
            pure $ M.fromList $ List.map (\r -> (T.decodeUtf8 r.rowKey, PactRowContents r.rowData r.txId)) sortedRows

    state1 <- ensureIsCompactedAndSortRows statePostCompaction1
    state2 <- ensureIsCompactedAndSortRows statePostCompaction2

    comparePactStateBeforeAndAfter state1 state2

compactionDoesNotDisruptDuplicateDetection :: ()
  => RocksDb
  -> TestTree
compactionDoesNotDisruptDuplicateDetection rdb = do
  compactionSetup "compactionDoesNotDisruptDuplicateDetection" rdb testPactServiceConfig $ \cr -> do
    let makeTx :: IO ChainwebTransaction
        makeTx = buildCwCmd (sshow @Word 0) testVersion
          $ set cbSigners [mkEd25519Signer' sender00 [mkGasCap, mkTransferCap "sender00" "sender01" 1.0]]
          $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)")
          $ defaultCmd

    e1 <- runBlockWithTx cr.mempoolRef cr.srcPactQueue cr.blockDb (\_ _ _ _ -> makeTx)
    assertBool "First tx submission succeeds" (isRight e1)

    Utils.sigmaCompact cr.srcSqlEnv cr.targetSqlEnv =<< PS.getLatestBlockHeight cr.srcSqlEnv

    e2 <- runBlockWithTx cr.mempoolRef cr.targetPactQueue cr.blockDb (\_ _ _ _ -> makeTx)
    assertBool "First tx submission fails" (isLeft e2)

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

    let createTable :: Word -> Text -> IO ChainwebTransaction
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
          buildCwCmd (sshow n) testVersion
            $ signSender00
            $ set cbGasLimit gasLimit
            $ set cbRPC (mkExec tx (mkKeySetData "sender00" [sender00]))
            $ defaultCmd

    let beforeTable = "test_before"
    let afterTable = "test_after"

    supply <- newIORef @Word 0
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
      void $ runBlock cr.srcPactQueue cr.blockDb second

    let freeBeforeTbl = "free.m0_" <> beforeTable
    let freeAfterTbl = "free.m1_" <> afterTable

    statePre <- getPactUserTables cr.srcSqlEnv
    forM_ [freeBeforeTbl, freeAfterTbl] $ \tbl -> do
      let msg = "Table " ++ T.unpack tbl ++ " should exist pre-compaction, but it doesn't."
      assertBool msg (isJust (M.lookup tbl statePre))

    Utils.sigmaCompact cr.srcSqlEnv cr.targetSqlEnv (BlockHeight halfwayPoint)

    statePost <- getPactUserTables cr.targetSqlEnv
    flip assertBool (isJust (M.lookup freeBeforeTbl statePost)) $
      T.unpack beforeTable ++ " was dropped; it wasn't supposed to be."

    flip assertBool (isNothing (M.lookup freeAfterTbl statePost)) $
      T.unpack afterTable ++ " wasn't dropped; it was supposed to be."

compactionGrandHashUnchanged :: ()
  => RocksDb
  -> TestTree
compactionGrandHashUnchanged rdb =
  compactionSetup "compactionGrandHashUnchanged" rdb testPactServiceConfig $ \cr -> do
    setOneShotMempool cr.mempoolRef =<< goldenMemPool

    let numBlocks :: Num a => a
        numBlocks = 100

    let makeTx :: Word -> BlockHeader -> IO ChainwebTransaction
        makeTx nth bh = buildCwCmd (sshow nth) testVersion
          $ set cbSigners [mkEd25519Signer' sender00 [mkGasCap, mkTransferCap "sender00" "sender01" 1.0]]
          $ setFromHeader bh
          $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)")
          $ defaultCmd

    replicateM_ numBlocks
      $ runBlockWithTx_ cr.mempoolRef cr.srcPactQueue cr.blockDb
      $ \n _ _ blockHeader -> makeTx n blockHeader

    let targetHeight = BlockHeight numBlocks

    hashPreCompaction <- computeGrandHash (PS.getLatestPactStateAt cr.srcSqlEnv targetHeight)
    Utils.sigmaCompact cr.srcSqlEnv cr.targetSqlEnv targetHeight
    hashPostCompaction <- computeGrandHash (PS.getLatestPactStateAt cr.targetSqlEnv targetHeight)

    assertEqual "GrandHash pre- and post-compaction are the same" hashPreCompaction hashPostCompaction

compactionResilientToRowIdOrdering :: ()
  => RocksDb
  -> TestTree
compactionResilientToRowIdOrdering rdb =
  compactionSetup "compactionResilientToRowIdOrdering" rdb testPactServiceConfig $ \cr -> do

    let numBlocks :: Num a => a
        numBlocks = 100

    -- Just run a bunch of blocks
    setOneShotMempool cr.mempoolRef =<< goldenMemPool
    let makeTx :: Word -> BlockHeader -> IO ChainwebTransaction
        makeTx nth bh = buildCwCmd (sshow nth) testVersion
          $ set cbSigners [mkEd25519Signer' sender00 [mkGasCap, mkTransferCap "sender00" "sender01" 1.0]]
          $ setFromHeader bh
          $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)")
          $ defaultCmd
    replicateM_ numBlocks
      $ runBlockWithTx_ cr.mempoolRef cr.srcPactQueue cr.blockDb
      $ \n _ _ blockHeader -> makeTx n blockHeader

    -- Get the state after running the blocks but before doing anything else
    statePreCompaction <- getLatestPactState cr.srcSqlEnv

    -- Reverse all of the rowids in the table. We get all the rows in txid DESC order, like so:
    --   rk1, txid=100, rowid=100
    --   rk1, txid=99,  rowid=99
    --   ...
    --
    -- Then we reverse the rowids, so that the table looks like this:
    --   rk1, txid=100, rowid=10_000
    --   rk1, txid=99,  rowid=10_001
    --   ...
    --
    -- Since the compaction algorithm orders by rowid DESC, it will get the rows in reverse order to how they were inserted.
    -- If compaction still results in the same end state, this confirms that the compaction algorithm is resilient to rowid ordering.
    e <- PS.qryStream cr.srcSqlEnv "SELECT rowkey, txid FROM [coin_coin-table] ORDER BY txid ASC" [] [RText, RInt] $ \rows -> do
      Lite.withStatement cr.srcSqlEnv "UPDATE [coin_coin-table] SET rowid = ?3 WHERE rowkey = ?1 AND txid = ?2" $ \stmt -> do
        flip S.mapM_ (S.zip (S.enumFrom @_ @(Down Int64) 10_000) rows) $ \(Down newRowId, row) -> case row of
          [SText rowkey, SInt txid] -> do
            Pact.bindParams stmt [SText rowkey, SInt txid, SInt newRowId]
            stepThenReset stmt

          _ -> error "unexpected row shape"
    assertBool "Didn't encounter a sqlite error during rowid shuffling" (isRight e)

    -- Compact to the tip
    Utils.sigmaCompact cr.srcSqlEnv cr.targetSqlEnv (BlockHeight numBlocks)

    -- Get the state post-randomisation and post-compaction
    statePostCompaction <- getLatestPactState cr.targetSqlEnv

    -- Same logic as in 'pactStateSamePreAndPostCompaction'
    comparePactStateBeforeAndAfter statePreCompaction statePostCompaction

comparePactStateBeforeAndAfter :: (Ord k, Eq a, Show k, Show a) => M.Map Text (M.Map k a) -> M.Map Text (M.Map k a) -> IO ()
comparePactStateBeforeAndAfter statePreCompaction statePostCompaction = do
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

getHistory :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
getHistory refIO reqIO = testCase "getHistory" $ do
  (_, q, bdb) <- reqIO
  setOneShotMempool refIO =<< goldenMemPool
  void $ runBlock q bdb second
  h <- getParentTestBlockDb bdb cid
  Historical (BlockTxHistory hist prevBals) <- pactBlockTxHistory h (UserTables "coin_coin-table") q
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
    setOneShotMempool refIO =<< goldenMemPool
    void $ runBlock q bdb second
    h <- getParentTestBlockDb bdb cid
    histLookup q h key >>= assertF

histLookup :: PactQueue -> BlockHeader -> T.Text -> IO (Maybe (TxLog RowData))
histLookup q bh k =
  throwIfNoHistory =<< pactHistoricalLookup bh (UserTables "coin_coin-table") (RowKey k) q

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

signSender00 :: CmdBuilder -> CmdBuilder
signSender00 = set cbSigners [mkEd25519Signer' sender00 []]

setFromHeader :: BlockHeader -> CmdBuilder -> CmdBuilder
setFromHeader bh =
  set cbChainId (view blockChainId bh)
  . set cbCreationTime (toTxCreationTime $ _bct $ view blockCreationTime bh)


-- this test relies on block gas errors being thrown before other Pact errors.
blockGasLimitTest :: HasCallStack => IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
blockGasLimitTest _ reqIO = testCase "blockGasLimitTest" $ do
  (_, q, _) <- reqIO

  let
    useGas g = do
      bigTx <- buildCwCmd "cmd" testVersion $ set cbGasLimit g $ signSender00 $ set cbRPC (mkExec' "TESTING") defaultCmd
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
      try $ validateBlock bh (CheckablePayloadWithOutputs payload) q
  -- we consume slightly more than the maximum block gas limit and provoke an error.
  useGas 2_000_001 >>= \case
    Left (BlockGasLimitExceeded _) ->
      return ()
    r ->
      error $ "not a BlockGasLimitExceeded error: " <> sshow r
  -- we consume much more than the maximum block gas limit and expect an error.
  useGas 3_000_000 >>= \case
    Left (BlockGasLimitExceeded _) ->
      return ()
    r ->
      error $ "not a BlockGasLimitExceeded error: " <> sshow r
  -- we consume exactly the maximum block gas limit and expect no such error.
  useGas 2_000_000 >>= \case
    Left (BlockGasLimitExceeded _) ->
      error "consumed exactly block gas limit but errored"
    _ ->
      return ()
  -- we consume much less than the maximum block gas limit and expect no such error.
  useGas 1_000_000 >>= \case
    Left (BlockGasLimitExceeded _) ->
      error "consumed much less than block gas limit but errored"
    _ ->
      return ()
  -- we consume zero gas and expect no such error.
  useGas 0 >>= \case
    Left (BlockGasLimitExceeded _) ->
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

    checkCount :: HasCallStack => Int -> PayloadWithOutputs_ a -> Assertion
    checkCount n = assertEqual "tx return count" n . V.length . _payloadWithOutputsTransactions

    mp supply txRefillMap = setMempool mpRefIO $ mempty {
      mpaGetBlock = \BlockFill{..} _ _ _ bh -> case M.lookup _bfCount (M.fromList txRefillMap) of
          Nothing -> return mempty
          Just txs -> fmap V.fromList $ sequence $ map (next supply bh) txs
      }


    next supply bh f = do
      i <- modifyMVar supply $ return . (succ &&& id)
      f i bh

    goodTx i bh = buildCwCmd (sshow (i, bh)) testVersion
        $ signSender00
        $ setFromHeader bh
        $ set cbRPC (mkExec' "(+ 1 2)")
        $ defaultCmd

    badTx i bh = buildCwCmd (sshow (i, bh)) testVersion
        $ signSender00
        $ set cbSender "bad"
        $ setFromHeader bh
        $ set cbRPC (mkExec' "(+ 1 2)")
        $ defaultCmd

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
          buildCwCmd ("1" <> sshow n) testVersion $
          signSender00 $
          set cbCreationTime (toTxCreationTime $ _bct $ view blockCreationTime bh) $
          set cbRPC (mkExec' code) $
          defaultCmd

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
  void $ pactPreInsertCheck (V.singleton tx) q

  setOneShotMempool mpRefIO $ mp tx
  -- b2 will be made at start + 30s
  void $ runBlock q bdb s30

  where

    makeTx nonce t = buildCwCmd (sshow t <> nonce) testVersion
        $ signSender00
        $ set cbChainId cid
        $ set cbCreationTime (toTxCreationTime t)
        $ set cbTTL 300
        $ set cbRPC (mkExec' "1")
        $ defaultCmd
    mp tx = mempty {
      mpaGetBlock = \_ valid _ _ bh -> getBlock bh tx valid
      }

    getBlock bh tx valid = do
      let txs = V.singleton tx
      oks <- valid (view blockHeight bh) (view blockHash bh) txs
      unless (V.and oks) $ throwM $ userError "Insert failed"
      return txs

preInsertCheckTimeoutTest :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
preInsertCheckTimeoutTest _ reqIO = testCase "preInsertCheckTimeoutTest" $ do
  (_, q, _) <- reqIO

  coinV3 <- T.readFile "pact/coin-contract/v3/coin-v3.pact"
  coinV4 <- T.readFile "pact/coin-contract/v4/coin-v4.pact"
  coinV5 <- T.readFile "pact/coin-contract/v5/coin-v5.pact"

  txCoinV3 <- buildCwCmd "tx-now-coinv3" testVersion
        $ signSender00
        $ set cbChainId cid
        $ set cbRPC (mkExec' coinV3)
        $ defaultCmd

  txCoinV4 <- buildCwCmd "tx-now-coinv4" testVersion
        $ signSender00
        $ set cbChainId cid
        $ set cbRPC (mkExec' coinV4)
        $ defaultCmd

  txCoinV5 <- buildCwCmd "tx-now-coinv5" testVersion
        $ signSender00
        $ set cbChainId cid
        $ set cbRPC (mkExec' coinV5)
        $ defaultCmd

  -- timeouts are tricky to trigger in GH actions.
  -- we're satisfied if it's triggered once in 100 runs.
  rs <- replicateM 100
    (pactPreInsertCheck (V.fromList [txCoinV3, txCoinV4, txCoinV5]) q)
  assertBool "should get at least one InsertErrorTimedOut" $ any
    (V.all (== Left InsertErrorTimedOut))
    rs

badlistNewBlockTest :: IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
badlistNewBlockTest mpRefIO reqIO = testCase "badlistNewBlockTest" $ do
  (_, reqQ, _) <- reqIO
  let hashToTxHashList = V.singleton . requestKeyToTransactionHash . RequestKey . toUntypedHash @'Blake2b_256
  badHashRef <- newIORef $ hashToTxHashList initialHash
  badTx <- buildCwCmd "badListMPA" testVersion
    $ signSender00
    -- this should exceed the account balance
    $ set cbGasLimit 99_999
    $ set cbGasPrice 1_000_000_000_000_000
    $ set cbRPC (mkExec' "(+ 1 2)")
    $ defaultCmd
  setOneShotMempool mpRefIO (badlistMPA badTx badHashRef)
  bip <- throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader genesisHeader) reqQ
  let resp = blockInProgressToPayloadWithOutputs bip
  assertEqual "bad tx filtered from block" mempty (_payloadWithOutputsTransactions resp)
  badHash <- readIORef badHashRef
  assertEqual "Badlist should have badtx hash" (hashToTxHashList $ _cmdHash badTx) badHash
  where
    badlistMPA badTx badHashRef = mempty
      { mpaGetBlock = \_ _ _ _ _ -> return (V.singleton badTx)
      , mpaBadlistTx = \v -> writeIORef badHashRef v
      }

goldenNewBlock :: String -> IO MemPoolAccess -> IO (IORef MemPoolAccess) -> IO (SQLiteEnv, PactQueue, TestBlockDb) -> TestTree
goldenNewBlock name mpIO mpRefIO reqIO = golden name $ do
    mp <- mpIO
    (_, reqQ, _) <- reqIO
    setOneShotMempool mpRefIO mp
    blockInProgress <- throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader genesisHeader) reqQ
    let resp = blockInProgressToPayloadWithOutputs blockInProgress
    -- ensure all golden txs succeed
    forM_ (_payloadWithOutputsTransactions resp) $ \(txIn,TransactionOutput out) -> do
      cr :: CommandResult Hash <- decodeStrictOrThrow out
      assertSatisfies ("golden tx succeeds, input: " ++ show txIn) (_crResult cr) (isRight . (\(PactResult r) -> r))
    goldenBytes resp blockInProgress
  where
    hmToSortedList :: Ord k => HM.HashMap k v -> [(k, v)]
    hmToSortedList = List.sortOn fst . HM.toList
    -- missing some fields, only includes the fields that are "outputs" of
    -- running txs, but not the module cache

    blockInProgressToJSON BlockInProgress {..} = J.object
      [ "blockGasLimit" J..= J.Aeson (fromIntegral @_ @Int _blockInProgressRemainingGasLimit)
      , "parentHeader" J..= J.encodeWithAeson (_parentHeader _blockInProgressParentHeader)
      , "pendingData" J..= J.object
        [ "pendingSuccessfulTxs" J..= J.array
          (encodeB64UrlNoPaddingText <$> List.sort (toList _pendingSuccessfulTxs))
        , "pendingTableCreation" J..= J.array
            (T.decodeUtf8 <$> List.sort (toList _pendingTableCreation))
        , "pendingWrites" J..= pendingWritesJson
        ]
      , "txId" J..= J.Aeson (fromIntegral @_ @Int _blockInProgressTxId)
      ]
     where
      SQLitePendingData{..} = _blockInProgressPendingData
      pendingWritesJson = J.Object
            [ (T.decodeUtf8 _dkTable, J.Object
                [ (T.decodeUtf8 _dkRowKey, J.Object
                    [ ((sshow @_ @T.Text. fromIntegral @TxId @Word) _deltaTxId, T.decodeUtf8 _deltaData)
                    | SQLiteRowDelta {..} <- toList rowKeyWrites
                    ])
                | (_dkRowKey, rowKeyWrites) <- hmToSortedList tableWrites
                ])
            | (_dkTable, tableWrites) <- hmToSortedList _pendingWrites
            ]

    goldenBytes :: PayloadWithOutputs -> BlockInProgress -> IO BL.ByteString
    goldenBytes a b = return $ BL.fromStrict $ encodeYaml $ J.object
      [ "test-group" J..= ("new-block" :: T.Text)
      , "results" J..= J.encodeWithAeson a
      , "blockInProgress" J..= blockInProgressToJSON b
      ]

goldenMemPool :: IO MemPoolAccess
goldenMemPool = do
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
    outTxs <- mkTxs txs
    mempoolOf [outTxs]
    where
      mkTxs txs =
          fmap V.fromList $ forM (zip txs [0..]) $ \(code,n :: Int) ->
            buildCwCmd ("1" <> sshow n) testVersion $
            signSender00 $
            set cbGasPrice 0.01 $
            set cbTTL 1_000_000 $ -- match old goldens
            set cbRPC (mkExec code $ mkKeySetData "test-admin-keyset" [sender00]) $
            defaultCmd

mempoolOf :: [V.Vector ChainwebTransaction] -> IO MemPoolAccess
mempoolOf blocks = do
  blocksRemainingRef <- newIORef blocks
  return mempty
    { mpaGetBlock = getTestBlock blocksRemainingRef
    }
  where
    getTestBlock blocksRemainingRef _ validate bHeight bHash _parent = do
        outtxs <- atomicModifyIORef' blocksRemainingRef $ \case
          (b:bs) -> (bs, b)
          [] -> ([], mempty)
        oks <- validate bHeight bHash outtxs
        unless (V.and oks) $ fail $ mconcat
            [ "tx failed validation! \nouttxs: \n"
            , show outtxs
            , "\n\noks: \n"
            , show oks ]
        return outtxs

data CompactionResources = CompactionResources
  { mempoolRef :: IO (IORef MemPoolAccess)
  , mempool :: MemPoolAccess
  , srcSqlEnv :: SQLiteEnv
  , targetSqlEnv :: SQLiteEnv
  , srcPactQueue :: PactQueue
  , targetPactQueue :: PactQueue
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
  withTemporaryDir $ \srcDir -> withSqliteDb cid srcDir $ \srcSqlEnvIO ->
  withTemporaryDir $ \targetDir -> withSqliteDb cid targetDir $ \targetSqlEnvIO ->
  withDelegateMempool $ \dm ->
    testCase pat $ do
      blockDb <- mkTestBlockDb testVersion rdb
      bhDb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb blockDb) cid
      let payloadDb = _bdbPayloadDb blockDb
      srcSqlEnv <- srcSqlEnvIO
      targetSqlEnv <- targetSqlEnvIO
      (mempoolRef, mempool) <- do
        (ref, nonRef) <- dm
        pure (pure ref, nonRef)
      srcPactQueue <- newPactQueue 2_000
      targetPactQueue <- newPactQueue 2_000

      let logger = genericLogger System.LogLevel.Error (\_ -> return ())

      -- Start pact service for the src and target
      let srcPactService = runPactService testVersion cid logger Nothing    srcPactQueue mempool bhDb payloadDb    srcSqlEnv pactCfg
      let targetPactService = runPactService testVersion cid logger Nothing targetPactQueue mempool bhDb payloadDb targetSqlEnv pactCfg

      setOneShotMempool mempoolRef =<< goldenMemPool

      withAsync srcPactService $ \_ -> do
        withAsync targetPactService $ \_ -> do
          f $ CompactionResources
            { mempoolRef = mempoolRef
            , mempool = mempool
            , srcSqlEnv = srcSqlEnv
            , targetSqlEnv = targetSqlEnv
            , srcPactQueue = srcPactQueue
            , targetPactQueue = targetPactQueue
            , blockDb = blockDb
            }

runBlockWithTx :: ()
  => IO (IORef MemPoolAccess) -- ^ mempoolRef
  -> PactQueue
  -> TestBlockDb
  -> (Word -> BlockHeight -> BlockHash -> BlockHeader -> IO ChainwebTransaction)
  -> IO (Either PactException PayloadWithOutputs)
runBlockWithTx mempoolRef pactQueue blockDb makeTx = do
  madeTx <- newIORef @Bool False
  supply <- newIORef @Word 0
  setMempool mempoolRef $ mempty {
    mpaGetBlock = \_ _ bHeight bHash bHeader -> do
      madeTxYet <- readIORef madeTx
      if madeTxYet
      then do
        pure mempty
      else do
        n <- atomicModifyIORef' supply $ \a -> (a + 1, a)
        tx <- makeTx n bHeight bHash bHeader
        writeIORef madeTx True
        pure $ V.fromList [tx]
  }
  e <- runBlockE pactQueue blockDb second
  writeIORef madeTx False
  pure e

runBlockWithTx_ :: ()
  => IO (IORef MemPoolAccess) -- ^ mempoolRef
  -> PactQueue
  -> TestBlockDb
  -> (Word -> BlockHeight -> BlockHash -> BlockHeader -> IO ChainwebTransaction)
  -> IO PayloadWithOutputs
runBlockWithTx_ mempoolRef pactQueue blockDb makeTx = do
  runBlockWithTx mempoolRef pactQueue blockDb makeTx >>= \case
    Left e -> assertFailure $ "newBlockAndValidate: validate: got failure result: " ++ show e
    Right v -> pure v

-- | Step through a prepared statement, then clear the statement's bindings
--   and reset the statement.
stepThenReset :: Lite.Statement -> IO Lite.StepResult
stepThenReset stmt = do
  Lite.stepNoCB stmt `finally` (Lite.clearBindings stmt >> Lite.reset stmt)
