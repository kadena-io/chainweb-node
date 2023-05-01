{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wwarn #-}

module Chainweb.Test.Pact.Compaction (tests) where

import Control.Concurrent.MVar (MVar)
import Control.Lens hiding ((.=))
import Control.Monad (forM, forM_, void)
import Data.ByteString (ByteString)
import Data.List qualified as List
--import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Map.Strict qualified as M
import Data.Map (Map)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite3.Direct (Utf8(..))
import Database.SQLite3.Direct qualified as SQ3
import Data.Vector qualified as V
import Patience.Map qualified as Patience
import Patience.Map (Delta(..))
import Prelude hiding (head)

import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Logger (newLogger)
import Pact.Types.RowData (RowData(..), RowDataVersion(..), RowDataValue(..))
import Pact.Types.Runtime (PactDb(..), Domain(..), ExecutionMode(..), Literal(..), WriteType(..), ObjectMap(..), TableName(..), RowKey(..))

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, {-assertEqual,-} assertFailure)

import System.Logger (LogLevel(..))

-- internal imports

import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Pact.Types (defaultModuleCacheLimit)
import Chainweb.Pact.Backend.Compaction (CompactFlag(..), compact, runCompactM, mkCompactEnv, withDefaultLogger)
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.Types (CheckpointEnv(..), SQLiteEnv(..), PactDbEnv'(..), Checkpointer(..), BlockEnv, ParentHash, initBlockState, bsModuleNameFix)
import Chainweb.Test.Pact.Utils (pactTestLogger)
import Chainweb.Test.Utils (ScheduledTest, testGroupSch, withTempSQLiteResource, getArbitrary, peterson)
import Chainweb.Version (ChainwebVersion(..), ChainId, genesisHeight, unsafeChainId)

import Chainweb.Test.Orphans.Internal ({- Arbitrary BlockHash -})

import Pact.Types.SQLite qualified as Pact

-- -------------------------------------------------------------------------- --
-- Tests

tests :: ScheduledTest
tests = testGroupSch "Chainweb.Test.Pact.Compaction"
    [ testCompactCheckpointer
    ]

testCompactCheckpointer :: TestTree
testCompactCheckpointer =
  withTempSQLiteResource $ runSQLite' $ \resIO ->
  testCase "testCompactCheckpointer" $ do

    (CheckpointEnv {..}, SQLiteEnv {..}) <- resIO

    let hashes = V.unfoldrExactN 20 (\i -> (getArbitrary @BlockHash i, i + 1)) 0
    let nthHash :: BlockHeight -> ParentHash
        nthHash h = hashes V.! (fromIntegral h)

    let restore :: BlockHeight -> IO PactDbEnv'
        restore h = do
          let head = if h == 0
                     then Nothing
                     else Just (h, nthHash (h - 1))
          _cpRestore _cpeCheckpointer head
    let restore_ :: BlockHeight -> IO ()
        restore_ h = do
          void $ restore h
    let save :: BlockHeight -> IO ()
        save h = do
          _cpSave _cpeCheckpointer (nthHash h)

    -- genesis block
    restore_ 0
    save 0

    -- block 1, empty block
    restore_ 1
    save 1

    -- block 2: create tables, add/mutate values
    PactDbEnv' (PactDbEnv pactdb mvar) <- restore 2

    let withTx :: IO a -> IO ()
        withTx io = do
          void $ _beginTx pactdb Transactional mvar
          void io
          void $ _commitTx pactdb mvar
    let rd1 = RowData RDV1 $ ObjectMap $ M.fromList [("a", RDLiteral (LBool False))]
    let rd2 = RowData RDV1 $ ObjectMap $ M.fromList [("a", RDLiteral (LBool True))]

    -- tx: create tables
    withTx $ do
      _createUserTable pactdb "test_A" "mod" mvar
      _createUserTable pactdb "test_AA" "mod" mvar

    -- setup potential hash collisions.
    withTx $ do
      _writeRow pactdb Insert (UserTables "test_A") "AA" rd1 mvar -- test table+key collision
      _writeRow pactdb Insert (UserTables "test_AA") "A" rd1 mvar
      _writeRow pactdb Insert (UserTables "test_A") "B" rd2 mvar -- test row+key collision
      _writeRow pactdb Insert (UserTables "test_AA") "B" rd2 mvar

    -- tx: update, insert
    withTx $ do
      _writeRow pactdb Update (UserTables "test_A") "B" rd2 mvar -- test update to same data collision
      _writeRow pactdb Insert (UserTables "test_A") "C" rd1 mvar -- new insert

    save 2

    -- block 3: updates
    restore_ 3

    withTx $ do
      _writeRow pactdb Update (UserTables "test_A") "B" rd1 mvar -- test updated hash x2
      _writeRow pactdb Update (UserTables "test_A") "C" rd2 mvar -- test updated hash

    save 3

    let compactToHeight :: BlockHeight -> [CompactFlag] -> IO ByteString
        compactToHeight h flags = withDefaultLogger Debug $ \logger -> do
          runCompactM (mkCompactEnv logger _sConn h flags) $ do
            -- compact and capture global grand hash
            compact

    tables <- getTestTables _sConn

    checkTablesBeforeAfter pactdb mvar (compactToHeight 2 [Flag_KeepCompactTables]) tables $
      M.fromList
        [ ( "test_A"
          , TableBeforeAfter
            { before =
                [ ("AA", rd1)
                , ("B", rd1)
                , ("C", rd2)
                ]
            , diff =
                [ ("B", Delta rd1 rd2)
                , ("C", Delta rd2 rd1)
                ]
            }
          )
        , ( "test_AA"
          , TableBeforeAfter
            { before =
                [ ("A", rd1)
                , ("B", rd2)
                ]
            , diff = []
            }
          )
        ]

{-
test coverage TODOs

- Tables are created at particular heights, so we'd want to see them
  dropped if they're after the target height

- Restart robustness: Show Checkpointer firing up after compaction and
  happily moving forward. Ideally, also do this with a simulated comapction
  failure.

- Check that we can compute the "grand hashes" in pure Haskell.
-}

-- -------------------------------------------------------------------------- --
-- Chainweb Settings

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

testChainId :: ChainId
testChainId = unsafeChainId 0

-- -------------------------------------------------------------------------- --
-- Checkpointer Utils

runSQLite'
    :: (IO (CheckpointEnv,SQLiteEnv) -> TestTree)
    -> IO SQLiteEnv
    -> TestTree
runSQLite' runTest sqlEnvIO = runTest $ do
    sqlenv <- sqlEnvIO
    cp <- initRelationalCheckpointer initialBlockState sqlenv logger testVer testChainId
    return (cp, sqlenv)
  where
    initialBlockState = set bsModuleNameFix True $
        initBlockState defaultModuleCacheLimit $ genesisHeight testVer testChainId
    logger = newLogger (pactTestLogger False) "RelationalCheckpointer"

-- -------------------------------------------------------------------------- --
-- Diff Utils

prettyDiffOfDiffs :: (a -> String) -> Delta (Delta a) -> IO String
prettyDiffOfDiffs p = \case
  -- Can't appear: We filter these out
  Same _ -> assertFailure sameMsg
  Old (Same _) -> assertFailure sameMsg
  New (Same _) -> assertFailure sameMsg
  Delta (Same _) _ -> assertFailure sameMsg
  Delta _ (Same _) -> assertFailure sameMsg

  -- Nested deltas can't exist, since we only have a diff in one
  -- direction
  Delta {} -> pure $
    "Unexpected nested delta.\n\n" ++
    "- Did you add a key and corresponding diff, for a key that shouldn't exist?"

  -- outer Old will appear in `expected`, i.e. the user-supplied diff
  Old (Old x) -> pure $
    "Expected a before-only value:\n\n  " ++
    p x ++ "\n\n" ++
    "But the before-only value doesn't exist.\n\n" ++
    "- Are you sure this key really exists?\n" ++
    "- Did you add an extraneous entry to the diff?"
  Old (New x) -> pure $
    "Expected an after-only value:\n\n  " ++
    p x ++ "\n\n" ++
    "But the after-only value doesn't exist.\n\n" ++
    "- Are you sure this key really exists?\n" ++
    "- Did you add an extraneous entry to the diff?"
  Old (Delta x1 x2) -> pure $
    "Expected a delta:\n\n  " ++
    p x1 ++ "\n\n  vs  \n\n  " ++
    p x2 ++ "\n\n" ++
    "- Are you sure this key really exists?\n" ++
    "- Did you add an extraneous entry to the diff?"

  -- outer New will appear in `actual`, i.e. the actual computed diff
  New (Old x) -> pure $
    "Unexpected before-only value:\n\n  " ++
    p x ++ "\n\n" ++
    "- Are you sure this key really exists?\n" ++
    "- Did you forget to add an entry to the expected diff?"
  New (New x) -> pure $
    "Unexpected after-only value:\n\n  " ++
    p x ++ "\n\n" ++
    "- Are you sure this key really exists?\n" ++
    "- Did you forget to add an entry to the expected diff?"
  New (Delta x1 x2) -> pure $
    "Unexpected delta:\n\n  " ++
    p x1 ++ "\n\n  vs  \n\n  " ++
    p x2 ++ "\n\n" ++
    "- Are you sure this key really exists?\n" ++
    "- Did you forget to add an entry to the expected diff?"
  where
    internalErr msg = "prettyDiffOfDiffs: internal error: " ++ msg
    sameMsg = internalErr "impossible Same case"

-- | Check a table before and after by verifying that the diff is correct
checkTableDiff :: ()
  => PactDb (BlockEnv SQLiteEnv)
  -> MVar (BlockEnv SQLiteEnv)
  -> TableName -- ^ table
  -> [(RowKey, RowData)] -- ^ before the operation
  -> [(RowKey, Delta RowData)] -- ^ diff from the operation
  -> IO ()
checkTableDiff pactdb mvar tbl before expectedDiff = do
  keys <- List.sort <$> _keys pactdb (UserTables tbl) mvar

  after <- forM keys $ \key -> do
    mrda <- _readRow pactdb (UserTables tbl) key mvar
    case mrda of
      Nothing -> do
        assertFailure $ "checkTableDiff: _readRow: No row found for key: " ++ show key
      Just rda -> do
        pure rda

  -- Filter (not . isSame) because we don't care about non-updated
  -- values
  let actualDiff :: M.Map RowKey (Delta RowData)
      actualDiff = M.filter (not . Patience.isSame)
        (Patience.diff (M.fromList before) (M.fromList (List.zip keys after)))
  let diffBetween :: M.Map RowKey (Delta (Delta RowData))
      diffBetween = Patience.diff
        (M.fromList (List.filter (not . Patience.isSame . snd) expectedDiff))
        actualDiff

  errors <- forM (M.toList diffBetween) $ \(key, delta) -> do
    let showKey (RowKey k) = "\"" ++ Text.unpack k ++ "\""
    let showTbl (TableName t) = "\"" ++ Text.unpack t ++ "\""
    let showIdx t rk = "(table=" ++ showTbl t ++ ", key=" ++ showKey rk ++ ")"
    case delta of
      Patience.Same _ -> do
        pure Nothing
      d -> do
        msg <- prettyDiffOfDiffs show d
        pure $ Just $ List.unlines
          [ ""
          , "Diff failure at " ++ showIdx tbl key
          , msg
          ]

  case catMaybes errors of
    [] -> pure ()
    errs -> do
      let bars = "\n===================================================================\n\n"
      assertFailure (List.intercalate bars errs)

data TableBeforeAfter k v = TableBeforeAfter
  { before :: [(k, v)]
  , diff :: [(k, Delta v)]
  }
  deriving stock (Eq, Functor)

_applyDiffForwards :: forall k v. (Ord k) => [(k, v)] -> [(k, Delta v)] -> [(k, v)]
_applyDiffForwards values d = mapMaybe go values
  where
    diff = M.fromList d
    go :: (k, v) -> Maybe (k, v)
    go (k, cur) =
      case M.lookup k diff of
        Nothing -> pure (k, cur)
        Just delta -> case delta of
          Same _ -> pure (k, cur)
          Old _ -> pure (k, cur)
          New x -> pure (k, x)
          Delta _x1 x2 -> pure (k, x2)

_applyDiffBackwards :: forall k v. (Ord k) => [(k, v)] -> [(k, Delta v)] -> [(k, v)]
_applyDiffBackwards values d = mapMaybe go values
  where
    diff = M.fromList d
    go :: (k, v) -> Maybe (k, v)
    go (k, cur) =
      case M.lookup k diff of
        Nothing -> pure (k, cur)
        Just delta -> case delta of
          Same _ -> pure (k, cur)
          Old x -> pure (k, x)
          New _ -> pure (k, cur)
          Delta x1 _x2 -> pure (k, x1)

checkTablesBeforeAfter :: ()
  => PactDb (BlockEnv SQLiteEnv)
  -> MVar (BlockEnv SQLiteEnv)
  -> IO a
  -> [TableName]
  -> Map TableName (TableBeforeAfter RowKey RowData)
  -> IO ()
checkTablesBeforeAfter pactdb mvar io tbls tblDiffs = do
  let findTable tbl = case M.lookup tbl tblDiffs of
        Just v -> pure v
        Nothing -> assertFailure $ "checkTablesBeforeAfter: Failed to find a Before/After for " ++ show tbl
  forM_ tbls $ \tbl -> do
    TableBeforeAfter beforeContents _expectedDiff <- findTable tbl
    checkTableDiff pactdb mvar tbl beforeContents []

  void io

  forM_ tbls $ \tbl -> do
    TableBeforeAfter beforeContents expectedDiff <- findTable tbl
    checkTableDiff pactdb mvar tbl beforeContents expectedDiff

-- -------------------------------------------------------------------------- --
-- Diff Utils

getTestTables :: SQ3.Database -> IO [TableName]
getTestTables db = do
  allTables <- Pact.qry
    db
    "SELECT name FROM sqlite_schema WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"
    []
    [Pact.RText]
  pure $ flip concatMap allTables $ \case
    [Pact.SText (Utf8 t)] -> do
      let name = Text.decodeUtf8 t
      if "test_" `Text.isPrefixOf` name
      then [TableName name]
      else []
    _ -> []
