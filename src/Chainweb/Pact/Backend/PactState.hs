{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

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
  ( getPactTableNames
  , getPactTables
  , getLatestPactState
  , getLatestBlockHeight

  , PactRow(..)
  , Table(..)
  , TableDiffable(..)

  , pactDiffMain
  )
  where

import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Control.Exception (bracket)
import Control.Monad (forM, forM_, when, void)
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
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite3.Direct (Utf8(..), Database)
import Database.SQLite3.Direct qualified as SQL
import Options.Applicative

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Logger (logFunctionText, logFunctionJson)
import Chainweb.Utils (HasTextRepresentation, fromText, toText, int)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName, ChainId, chainIdToText)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (fromUtf8, withSqliteDb)
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
        [SText u] -> (Text.toLower (fromUtf8 u), u)
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
      S.yield $ Table (fromUtf8 tbl) shapedRows
    else do
      pure ()

stepStatement :: SQL.Statement -> [RType] -> Stream (Of [SType]) IO (Either SQL.Error ())
stepStatement stmt rts = runExceptT $ do
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

-- | Prepare/execute query with params
qry :: ()
  => Database
  -> Utf8
  -> [SType]
  -> [RType]
  -> (Stream (Of [SType]) IO (Either SQL.Error ()) -> IO x)
  -> IO x
qry db qryText args returnTypes k = do
  bracket (Pact.prepStmt db qryText) SQL.finalize $ \stmt -> do
    Pact.bindParams stmt args
    k (stepStatement stmt returnTypes)

getLatestPactState :: Database -> Stream (Of TableDiffable) IO ()
getLatestPactState db = do
  let fmtTable x = "\"" <> x <> "\""

  tables <- liftIO $ getPactTableNames db

  forM_ tables $ \tbl -> do
    when (tbl `notElem` excludedTables) $ do
      let qryText = "SELECT rowkey, rowdata, txid FROM "
            <> fmtTable tbl
      latestState <- fmap (M.map (\prc -> prc.rowData)) $ liftIO $ qry db qryText [] [RText, RBlob, RInt] $ \rows -> do
        let go :: Map ByteString PactRowContents -> [SType] -> Map ByteString PactRowContents
            go m = \case
              [SText (Utf8 rowKey), SBlob rowData, SInt txId] ->
                M.insertWith (\prc1 prc2 -> if prc1.txId > prc2.txId then prc1 else prc2) rowKey (PactRowContents rowData txId) m
              _ -> error "getLatestPactState: unexpected shape of user table row"
        S.fold_ go M.empty id rows
      S.yield (TableDiffable (fromUtf8 tbl) latestState)

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
diffLatestPactState :: ()
  => Stream (Of TableDiffable) IO ()
  -> Stream (Of TableDiffable) IO ()
  -> Stream (Of (Text, Stream (Of RowKeyDiffExists) IO ())) IO ()
diffLatestPactState = go
  where
  go :: Stream (Of TableDiffable) IO () -> Stream (Of TableDiffable) IO () -> Stream (Of (Text, Stream (Of RowKeyDiffExists) IO ())) IO ()
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
        S.yield (t1.name, diffTables t1 t2)
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

diffTables :: TableDiffable -> TableDiffable -> Stream (Of RowKeyDiffExists) IO ()
diffTables t1 t2 = do
  void $ Merge.mergeA
    (Merge.traverseMaybeMissing $ \rk _rd -> do
      S.yield (Old rk)
      pure Nothing
    )
    (Merge.traverseMaybeMissing $ \rk _rd -> do
      S.yield (New rk)
      pure Nothing
    )
    (Merge.zipWithMaybeAMatched $ \rk rd1 rd2 -> do
      when (rd1 /= rd2) $ do
        S.yield (Delta rk)
      pure Nothing
    )
    t1.rows
    t2.rows

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
  , rows :: Map ByteString ByteString -- Map RowKey RowData
  }
  deriving stock (Eq, Ord, Show)

data PactRow = PactRow
  { rowKey :: ByteString
  , rowData :: ByteString
  , txId :: Int64
  }
  deriving stock (Eq, Show)

instance Ord PactRow where
  compare pr1 pr2 =
    compare pr1.txId pr2.txId
    <> compare pr1.rowKey pr2.rowKey
    <> compare pr1.rowData pr2.rowData

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
                 let diff = diffLatestPactState (getLatestPactState db1) (getLatestPactState db2)
                 diffy <- S.foldMap_ id $ flip S.mapM diff $ \(tblName, tblDiff) -> do
                   logText LL.Info $ "[Starting table " <> tblName <> "]"
                   d <- S.foldMap_ id $ flip S.mapM tblDiff $ \d -> do
                     logFunctionJson logger LL.Warn $ rowKeyDiffExistsToObject d
                     pure Difference
                   logText LL.Info $ "[Finished table " <> tblName <> "]"
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

doesPactDbExist :: ChainId -> FilePath -> IO Bool
doesPactDbExist cid dbDir = do
  let chainDbFileName = mconcat
        [ "pact-v1-chain-"
        , Text.unpack (chainIdToText cid)
        , ".sqlite"
        ]
  let file = dbDir </> chainDbFileName
  doesFileExist file
