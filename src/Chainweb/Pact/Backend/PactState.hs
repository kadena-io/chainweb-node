{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

{-# options_ghc -fno-warn-unused-imports -fno-warn-unused-top-binds #-}

{-# options_ghc -ddump-simpl -dsuppress-all -ddump-to-file #-}

-- |
-- Module: Chainweb.Pact.Backend.PactState
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: see LICENSE.md
--
-- Diff Pact state between two databases.
--
-- There are other utilities provided by this module whose purpose is either
-- to get the pact state.
--
-- The code in this module operates primarily on 'Stream's, because the amount
-- of user data can grow quite large. by comparing one table at a time, we can
-- keep maximum memory utilisation in check.
--

module Chainweb.Pact.Backend.PactState
  ( --getPactTableNames
  --, getPactTables
  --, getLatestPactState
  --, getLatestBlockHeight

  --, PactRow(..)
  --, Table(..)

    pactDiffMain
  )
  where

import Data.Word (Word64)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Aeson (ToJSON(..), (.=))
import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.ByteString (ByteString)
import Data.Foldable qualified as F
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite3.Direct (Utf8(..), Database, Statement)
import Database.SQLite3.Direct qualified as SQL
import Database.SQLite3.Bindings.Types (CStatement)
import Foreign.ForeignPtr
import Data.Coerce (coerce)
import Foreign.Ptr (FunPtr, Ptr)
import Options.Applicative

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Logger (Logger, logFunctionText, logFunctionJson)
import Chainweb.Utils (HasTextRepresentation, fromText, toText, int, sshow)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName, ChainId, chainIdToText)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (withSqliteDb)
import Chainweb.Pact.Backend.Compaction qualified as C

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.Logger (LogLevel(..))
import System.LogLevel qualified as LL

import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S

excludedTables :: [Utf8]
excludedTables = checkpointerTables ++ compactionTables
  where
    checkpointerTables = ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"]
    compactionTables = ["CompactGrandHash", "CompactActiveRow"]

getLatestBlockHeight :: Database -> IO BlockHeight
getLatestBlockHeight db = do
  let qryText = "SELECT MAX(blockheight) FROM BlockHistory"
  Pact.qry db qryText [] [RInt] >>= \case
    [[SInt bh]] -> pure (BlockHeight (int bh))
    _ -> error "getLatestBlockHeight: expected int"

getPactTableNames :: Database -> IO (Vector Utf8)
getPactTableNames db = do
  let sortedTableNames :: [[SType]] -> [Utf8]
      sortedTableNames rows = M.elems $ M.fromListWith const $ flip List.map rows $ \case
        [SText u] -> (Text.toLower (utf8ToText u), u)
        _ -> error "getPactTableNames.sortedTableNames: expected text"

  tables <- fmap sortedTableNames $ do
    let qryText =
          "SELECT name FROM sqlite_schema \
          \WHERE \
          \  type = 'table' \
          \AND \
          \  name NOT LIKE 'sqlite_%'"
    Pact.qry db qryText [] [RText]

  pure (Vector.fromList tables)

-- | Get all of the rows for each table. The tables will be sorted
--   lexicographically by name.
getPactTables :: Database -> Stream (Of Table) IO ()
getPactTables db = do
  let fmtTable x = "\"" <> x <> "\""

  tables <- liftIO $ getPactTableNames db

  forM_ tables $ \tbl -> do
    if tbl `notElem` excludedTables
    then do
      let qryText = "SELECT rowkey, rowdata, txid FROM "
            <> fmtTable tbl
      userRows <- liftIO $ Pact.qry db qryText [] [RText, RBlob, RInt]
      shapedRows <- forM userRows $ \case
        [SText (Utf8 rowKey), SBlob rowData, SInt txId] -> do
          pure $ PactRow {..}
        _ -> error "getPactTableNames: unexpected shape of user table row"
      S.yield $ Table (utf8ToText tbl) shapedRows
    else do
      pure ()

stepStatement :: SQL.Statement -> [RType] -> Stream (Of [SType]) IO QryResult
stepStatement stmt rts = do
  e <- runExceptT $ do
    -- todo: rename from acc
    let acc :: SQL.StepResult -> ExceptT SQL.Error (Stream (Of [SType]) IO) ()
        acc = \case
          SQL.Done -> do
            pure ()
          SQL.Row -> do
            as <- forM (List.zip [0..] rts) $ \(colIx, expectedColType) -> do
              liftIO $ case expectedColType of
                RInt -> SInt <$> SQL.columnInt64 stmt colIx
                RDouble -> SDouble <$> SQL.columnDouble stmt colIx
                RText -> SText <$> SQL.columnText stmt colIx
                RBlob -> SBlob <$> SQL.columnBlob stmt colIx
            lift $ S.yield as
            liftIO (SQL.step stmt) >>= \case
              Left err -> do
                throwError err
              Right sr -> do
                acc sr

    -- maybe use stepNoCB
    ExceptT (liftIO (SQL.step stmt)) >>= acc

  pure (QryResult e)

--foreign import ccall "sqlite3.h &sqlite3_finalize"
--  c_sqlite3_finalize_funptr :: FunPtr (Ptr CStatement -> IO ())

data QryResult = QryResult {-(ForeignPtr CStatement)-} (Either SQL.Error ())

-- | Prepare/execute query with params
qry :: ()
  => Database
  -> Utf8
  -> [SType]
  -> [RType]
  -> (Stream (Of [SType]) IO QryResult -> IO x)
  -> IO x
qry db qryText args returnTypes k = do
  stmt <- Pact.prepStmt db qryText
  --fptr <- newForeignPtr c_sqlite3_finalize_funptr (coerce stmt)
  Pact.bindParams stmt args
  k (stepStatement stmt returnTypes)

getLatestPactState :: (Logger logger) => logger -> Text -> Database -> Stream (Of TableDiffable) IO ()
getLatestPactState logger nth db = do
  let fmtTable x = "\"" <> x <> "\""

  tables <- liftIO $ getPactTableNames db

  forM_ tables $ \tbl -> do
    when (tbl `notElem` excludedTables) $ do
      liftIO $ logFunctionText logger LL.Info $
        "[Starting on table " <> utf8ToText tbl <> " from " <> nth <> " db]"
      latestState <- liftIO $ do
        let t = fmtTable tbl
        {-
        let qryText = "SELECT rowkey, rowdata "
              <> "FROM " <> t <> " t1 "
              <> "WHERE txid=(SELECT MAX(txid) FROM " <> t <> " t2 WHERE t1.rowkey=t2.rowkey) "
              <> "GROUP BY rowkey"
        -}
        let qryText = "SELECT rowkey, rowdata "
              <> "FROM " <> t <> " "
              <> "ORDER BY rowkey DESC, txid DESC"

        qry db qryText [] [RText, RBlob] $ \rows -> do
          pure $ flip S.mapM (S.zip (S.enumFrom (0 :: Word64)) rows) $ \case
            (n, [SText (Utf8 rowKey), SBlob rowData]) -> do
              when (n `mod` 1_000_000 == 0 && (tbl == "coin_coin-table" || tbl == "SYS:Pacts")) $ do
                liftIO $ logFunctionText logger LL.Info $ "[getLatestPactState row " <> sshow n <> "]"
              pure (rowKey, rowData)
            _ -> error "getLatestPactState: expected (text, blob)"

{-
        let qryText1 = "SELECT rowkey, txid FROM "
              <> fmtTable tbl
              <> " ORDER BY txid DESC, rowkey"
        activeRows <- qry db qryText1 [] [RText, RInt] $ \rows -> do
          let go :: Map ByteString Int64 -> [SType] -> Map ByteString Int64
              go m = \case
                [SText (Utf8 rowKey), SInt txId] -> M.insertWith max rowKey txId m
                _ -> error "getLatestPactState: unexpected shape of user table row"
          S.fold_ go M.empty id rows

        flip M.traverseWithKey activeRows $ \rowKey txId -> do
          let qryText2 = "SELECT rowdata FROM "
                <> fmtTable tbl
                <> " WHERE rowkey=?1 AND txid=?2"
          Pact.qry db qryText2 [SText (Utf8 rowKey), SInt txId] [RBlob] >>= \case
            [[SBlob rowData]] -> pure rowData
            _ -> error "getLatestPactState.qry2: expected Blob"
-}
      S.yield (TableDiffable (utf8ToText tbl) latestState)

-- This assumes the same tables (essentially zipWith).
--   Note that this assumes we got the state from `getLatestPactState`,
--   because `getPactTableNames` sorts the table names, and `getLatestPactState`
--   sorts the [PactRow] by rowKey.
--
-- If we ever step across two tables that do not have the same name, we throw an error.
--
-- This diminishes the utility of comparing two pact states that are known to be
-- at different heights, but that hurts our ability to perform the diff in
-- constant memory.
--
-- TODO: maybe inner stream should be a ByteStream
diffLatestPactState :: (Logger logger)
  => logger
  -> Stream (Of TableDiffable) IO ()
  -> Stream (Of TableDiffable) IO ()
  -> Stream (Of (Text, Stream (Of RowKeyDiffExists) IO ())) IO ()
diffLatestPactState logger = go
  where
  go :: ()
    => Stream (Of TableDiffable) IO ()
    -> Stream (Of TableDiffable) IO ()
    -> Stream (Of (Text, Stream (Of RowKeyDiffExists) IO ())) IO ()
  go s1 s2 = do
    e1 <- liftIO $ S.next s1
    e2 <- liftIO $ S.next s2

    case (e1, e2) of
      (Left (), Left ()) -> do
        pure ()
      (Right _, Left ()) -> do
        error "left stream longer than right"
      (Left (), Right _) -> do
        error "right stream longer than left"
      (Right (t1, next1), Right (t2, next2)) -> do
        when (t1.name /= t2.name) $ do
          error "diffLatestPactState: mismatched table names"
        S.yield (t1.name, diffTables logger t1 t2)
        go next1 next2

-- | We don't include the entire rowdata in the diff, only the rowkey.
--   This is just a space-saving measure.
data RowKeyDiffExists
  = Old ByteString
    -- ^ The rowkey exists in the same table of the first db, but not the second.
  | New ByteString
    -- ^ The rowkey exists in the same table of the second db, but not the first.
  | Delta ByteString
    -- ^ The rowkey exists in the same table of both dbs, but the rowdata
    --   differs.

diffTables :: (Logger logger) => logger -> TableDiffable -> TableDiffable -> Stream (Of RowKeyDiffExists) IO ()
diffTables logger td1 td2 = go (0 :: Word64) td1.rows td2.rows
  where
    go !n t1 t2 = do
      e1 <- liftIO $ S.next t1
      e2 <- liftIO $ S.next t2
      case (e1, e2) of
        (Left (QryResult r1), Left (QryResult r2)) -> do
          case r1 >> r2 of
            Left err -> do
              error $ "diffTables: SQLite error: " <> show err
            Right () -> do
              pure ()
        (Right ((rk1, _rd1), next1), Left r) -> do
          S.yield (Old rk1)
          go (n + 1) (S.dropWhile (\(rk, _) -> rk == rk1) next1) (pure r)
        (Left r, Right ((rk2, _rd2), next2)) -> do
          S.yield (New rk2)
          go (n + 1) (pure r) (S.dropWhile (\(rk, _) -> rk == rk2) next2)
        (Right ((rk1, _rd1), next1), Right ((rk2, _rd2), next2)) -> do
          when (n `mod` 100 == 0 && (td1.name == "SYS:Pacts" || td1.name == "coin_coin-table")) $ do
            liftIO $ logFunctionText logger LL.Info $ "[diffTables rowkey " <> sshow n <> "]"
          go (n + 1) (S.dropWhile (\(rk, _) -> rk == rk1) next1) (S.dropWhile (\(rk, _) -> rk == rk2) next2)
{-
          case compare rk1 rk2 of
            EQ -> do
              when (rd1 /= rd2) $ do
                S.yield (Delta rk1)
              go (n + 1) (S.dropWhile (\(rk, _) -> rk == rk1) next1) (S.dropWhile (\(rk, _) -> rk == rk2) next2)
            GT -> do
              S.yield (Old rk1)
              go (n + 1) (S.dropWhile (\(rk, _) -> rk == rk1) next1) (S.cons (rk2, rd2) next2)
            LT -> do
              S.yield (New rk2)
              go (n + 1) (S.cons (rk1, rd1) next1) (S.dropWhile (\(rk, _) -> rk == rk2) next2)
-}

rowKeyDiffExistsToObject :: RowKeyDiffExists -> Aeson.Value
rowKeyDiffExistsToObject = \case
  Old rk -> Aeson.object
    [ "old" .= Text.decodeUtf8 rk
    ]
  New rk -> Aeson.object
    [ "new" .= Text.decodeUtf8 rk
    ]
  Delta rk -> Aeson.object
    [ "delta" .= Text.decodeUtf8 rk
    ]

-- | A pact table - just its name and its rows.
data Table = Table
  { name :: Text
  , rows :: [PactRow]
  }
  deriving stock (Eq, Show)

-- | A diffable pact table - its name and the _active_ pact state
--   as a Map from RowKey to RowData.
data TableDiffable = TableDiffable
  { name :: Text
  , rows :: Stream (Of (ByteString, ByteString)) IO QryResult -- Stream (Of (RowKey, RowData)) IO ()
  }

data PactRow = PactRow
  { rowKey :: ByteString
  , rowData :: ByteString
  , txId :: Int64
  }
  deriving stock (Eq, Show)

instance ToJSON PactRow where
  toJSON pr = Aeson.object
    [ "row_key" .= Text.decodeUtf8 pr.rowKey
    , "row_data" .= Text.decodeUtf8 pr.rowData
    , "tx_id" .= pr.txId
    ]

data PactRowContents = PactRowContents
  { rowData :: ByteString
  , txId :: Int64
  }
  deriving stock (Eq, Show)

data PactDiffConfig = PactDiffConfig
  { firstDbDir :: FilePath
  , secondDbDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , logDir :: FilePath
  }

data Diffy = Difference | NoDifference
  deriving stock (Eq)

instance Semigroup Diffy where
  Difference <> _ = Difference
  _ <> Difference = Difference
  _ <> _          = NoDifference

instance Monoid Diffy where
  mempty = NoDifference

pactDiffMain :: IO ()
pactDiffMain = do
  cfg <- execParser opts

  when (cfg.firstDbDir == cfg.secondDbDir) $ do
    Text.putStrLn "Source and target Pact database directories cannot be the same."
    exitFailure

  let cids = List.sort $ F.toList $ chainIdsAt cfg.chainwebVersion (BlockHeight maxBound)

  diffyRef <- newIORef @(Map ChainId Diffy) M.empty

  forM_ cids $ \cid -> do
    C.withPerChainFileLogger cfg.logDir cid Info $ \logger -> do
      let logText = logFunctionText logger

      sqliteFileExists1 <- doesPactDbExist cid cfg.firstDbDir
      sqliteFileExists2 <- doesPactDbExist cid cfg.secondDbDir

      if | not sqliteFileExists1 -> do
             logText LL.Warn $ "[SQLite for chain in " <> Text.pack cfg.firstDbDir <> " doesn't exist. Skipping]"
         | not sqliteFileExists2 -> do
             logText LL.Warn $ "[SQLite for chain in " <> Text.pack cfg.secondDbDir <> " doesn't exist. Skipping]"
         | otherwise -> do
             let resetDb = False
             withSqliteDb cid logger cfg.firstDbDir resetDb $ \(SQLiteEnv db1 _) -> do
               withSqliteDb cid logger cfg.secondDbDir resetDb $ \(SQLiteEnv db2 _) -> do
                 logText LL.Info "[Starting diff]"
                 let diff = diffLatestPactState logger
                       (getLatestPactState logger "first" db1)
                       (getLatestPactState logger "second" db2)
                 diffy <- S.foldMap_ id $ flip S.mapM diff $ \(tblName, tblDiff) -> do
                   d <- S.foldMap_ id $ flip S.mapM tblDiff $ \d -> do
                     when False $ do
                       logFunctionJson logger LL.Warn $ rowKeyDiffExistsToObject d
                     pure Difference
                   logText LL.Info $ "[Finished diffing table " <> tblName <> "]"
                   pure d

                 logText LL.Info $ case diffy of
                   Difference -> "[Non-empty diff]"
                   NoDifference -> "[Empty diff]"
                 logText LL.Info $ "[Finished chain " <> chainIdToText cid <> "]"

                 atomicModifyIORef' diffyRef $ \m -> (M.insert cid diffy m, ())

  diffy <- readIORef diffyRef
  case M.foldMapWithKey (\_ d -> d) diffy of
    Difference -> do
      Text.putStrLn "Diff complete. Differences found."
      exitFailure
    NoDifference -> do
      Text.putStrLn "Diff complete. No differences found."
  where
    opts :: ParserInfo PactDiffConfig
    opts = info (parser <**> helper)
      (fullDesc <> progDesc "Compare two Pact databases")

    parser :: Parser PactDiffConfig
    parser = PactDiffConfig
      <$> strOption
           (long "first-database-dir"
            <> metavar "PACT_DB_DIRECTORY"
            <> help "First Pact database directory")
      <*> strOption
           (long "second-database-dir"
            <> metavar "PACT_DB_DIRECTORY"
            <> help "Second Pact database directory")
      <*> (fmap (lookupVersionByName . fromTextSilly @ChainwebVersionName) $ strOption
           (long "graph-version"
            <> metavar "CHAINWEB_VERSION"
            <> help "Chainweb version for graph. Only needed for non-standard graphs."
            <> value (toText (_versionName mainnet))
            <> showDefault))
      <*> strOption
           (long "log-dir"
            <> metavar "LOG_DIRECTORY"
            <> help "Directory where logs will be placed"
            <> value ".")

fromTextSilly :: HasTextRepresentation a => Text -> a
fromTextSilly t = case fromText t of
  Just a -> a
  Nothing -> error "fromText failed"

utf8ToText :: Utf8 -> Text
utf8ToText (Utf8 u) = Text.decodeUtf8 u

doesPactDbExist :: ChainId -> FilePath -> IO Bool
doesPactDbExist cid dbDir = do
  let chainDbFileName = mconcat
        [ "pact-v1-chain-"
        , Text.unpack (chainIdToText cid)
        , ".sqlite"
        ]
  let file = dbDir </> chainDbFileName
  doesFileExist file
