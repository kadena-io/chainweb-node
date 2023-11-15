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
{-# LANGUAGE TupleSections #-}

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
import Control.Exception (bracket, throwIO)
import Control.Monad
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource
import Data.Aeson (ToJSON(..), (.=))
import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.ByteString (ByteString)
import Data.Foldable qualified as F
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Merge.Strict
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
import GHC.IO.Unsafe

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

allRows :: SQL.Statement -> [RType] -> IO [[SType]]
allRows stmt rts =
  go
  where
  go = unsafeDupableInterleaveIO $
    -- maybe use stepNoCB
    SQL.step stmt >>= \case
      Left err -> error $ "sql error: " <> show err
      Right sr -> step sr
  step SQL.Done = pure []
  step SQL.Row = do
    as <- forM (List.zip [0..] rts) $ \(colIx, expectedColType) -> do
      liftIO $ case expectedColType of
        RInt -> SInt <$> SQL.columnInt64 stmt colIx
        RDouble -> SDouble <$> SQL.columnDouble stmt colIx
        RText -> SText <$> SQL.columnText stmt colIx
        RBlob -> SBlob <$> SQL.columnBlob stmt colIx
    rest <- go
    return (as:rest)

--foreign import ccall "sqlite3.h &sqlite3_finalize"
--  c_sqlite3_finalize_funptr :: FunPtr (Ptr CStatement -> IO ())

data QryResult = QryResult {-(ForeignPtr CStatement)-} (Either SQL.Error ())

-- | Prepare/execute query with params
qry :: ()
  => Database
  -> Utf8
  -> [SType]
  -> [RType]
  -> ResourceT IO [[SType]]
qry db qryText args returnTypes = do
  (_rk, stmt) <- allocate (Pact.prepStmt db qryText) (SQL.finalize >=> either (error . show) return)
  liftIO $ Pact.bindParams stmt args
  rows <- liftIO $ allRows stmt returnTypes
  return rows

interleaveForM :: [a] -> (a -> ResourceT IO b) -> ResourceT IO [b]
interleaveForM xs f = do
  st <- getInternalState
  liftIO $ unsafeDupableInterleaveIO $
    case xs of
      [] -> return []
      (y:ys) -> do
        y' <- runInternalState (f y) st
        ys' <- runInternalState (interleaveForM ys f) st
        return (y' : ys')

      -- (:) <$> f y <*> interleaveForM ys f

getLatestPactState
  :: (Logger logger)
  => logger
  -> Text
  -> Database
  -> IO (Map Text (ResourceT IO [(ByteString, ByteString)]))
getLatestPactState logger nth db = do
  let fmtTable x = "\"" <> x <> "\""

  tables <- liftIO $ getPactTableNames db

  tableContents <- forM (Vector.toList tables) $ \tbl -> do
    guard (tbl `notElem` excludedTables)
    let t = fmtTable tbl
    let qryText = "SELECT rowkey, rowdata "
          <> "FROM " <> t <> " "
          <> "ORDER BY rowkey DESC, txid DESC"

    return $ (utf8ToText tbl,) $ do
      rows <- qry db qryText [] [RText, RBlob]
      rows' <- interleaveForM (zip [(0 :: Word64)..] rows) $ \case
        (n, [SText (Utf8 rowKey), SBlob rowData]) -> do
          when (n `mod` 1_000_000 == 0 && (tbl == "coin_coin-table" || tbl == "SYS:Pacts")) $ do
            liftIO $ logFunctionText logger LL.Info $ "[getLatestPactState row " <> sshow n <> "]"
          pure (rowKey, rowData)
        _ -> error "getLatestPactState: expected (text, blob)"
      return rows'

  return $ M.fromList tableContents
      -- liftIO $ logFunctionText logger LL.Info $
      --   "[Starting on table " <> utf8ToText tbl <> " from " <> nth <> " db]"
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
      -- S.yield (TableDiffable (utf8ToText tbl) latestState)

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
  -> Map Text (ResourceT IO [(ByteString, ByteString)])
  -> Map Text (ResourceT IO [(ByteString, ByteString)])
  -> Map Text (Either TableDiffExists (ResourceT IO [RowKeyDiffExists]))
diffLatestPactState logger =
  merge
    (mapMissing $ \k _ -> Left $ OldTable k)
    (mapMissing $ \k _ -> Left $ NewTable k)
    (zipWithMatched $ \_ t1 t2 -> Right $ join $ fmap liftIO $ diffTables logger <$> t1 <*> t2)

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

data TableDiffExists
  = OldTable Text
  | NewTable Text

diffTables :: (Logger logger) => logger -> [(ByteString, ByteString)] -> [(ByteString, ByteString)] -> IO [RowKeyDiffExists]
diffTables logger td1 td2 = go (0 :: Word64) td1 td2
  where
    go :: Word64 -> [(ByteString, ByteString)] -> [(ByteString, ByteString)] -> IO [RowKeyDiffExists]
    go !n t1 t2 = unsafeDupableInterleaveIO $ do
      case (t1, t2) of
        ([], []) ->
          return []
        ((rk1, _rd1) : next1, []) -> do
          (Old rk1 :) <$> go (n + 1) (dropWhile (\(rk, _) -> rk == rk1) next1) []
        ([], (rk2, _rd2) : next2) -> do
          (Old rk2 :) <$> go (n + 1) [] (dropWhile (\(rk, _) -> rk == rk2) next2)
        ((rk1, rd1) : next1, (rk2, rd2) : next2) -> do
          -- when (n `mod` 100 == 0 && (td1.name == "SYS:Pacts" || td1.name == "coin_coin-table")) $ do
          --   liftIO $ logFunctionText logger LL.Info $ "[diffTables rowkey " <> sshow n <> "]"
          case compare rk1 rk2 of
            EQ -> do
              let
                extend =
                  if (rd1 /= rd2)
                  then (Delta rk1 :)
                  else id
              extend <$> go (n + 1) (dropWhile (\(rk, _) -> rk == rk1) next1) (dropWhile (\(rk, _) -> rk == rk2) next2)
            GT -> do
              (Old rk1 :) <$> go (n + 1) (dropWhile (\(rk, _) -> rk == rk1) next1) t2
            LT -> do
              (New rk2 :) <$> go (n + 1) t1 (dropWhile (\(rk, _) -> rk == rk2) next2)

tableDiffExistsToObject :: TableDiffExists -> Aeson.Value
tableDiffExistsToObject = \case
  OldTable tblName -> Aeson.object
    [ "oldTable" .= tblName
    ]
  NewTable tblName -> Aeson.object
    [ "newTable" .= tblName
    ]


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
  , rows :: IO [(ByteString, ByteString)] -- Stream (Of (RowKey, RowData)) IO ()
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

foldMapM
  :: (Monad m, Monoid w, Foldable t)
  => (a -> m w)
  -> t a
  -> m w
foldMapM f = F.foldlM
  (\acc a -> do
    w <- f a
    return $! mappend acc w
    )
  mempty

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

      if
        | not sqliteFileExists1 -> do
          logText LL.Warn $ "[SQLite for chain in " <> Text.pack cfg.firstDbDir <> " doesn't exist. Skipping]"
        | not sqliteFileExists2 -> do
          logText LL.Warn $ "[SQLite for chain in " <> Text.pack cfg.secondDbDir <> " doesn't exist. Skipping]"
        | otherwise -> do
          let resetDb = False
          withSqliteDb cid logger cfg.firstDbDir resetDb $ \(SQLiteEnv db1 _) -> do
            withSqliteDb cid logger cfg.secondDbDir resetDb $ \(SQLiteEnv db2 _) -> do
              logText LL.Info "[Starting diff]"
              allTableDiffs <- diffLatestPactState logger
                    <$> (getLatestPactState logger "first" db1)
                    <*> (getLatestPactState logger "second" db2)
              diffy <- flip M.foldMapWithKey allTableDiffs $ \tblName -> \case
                Left tblDiff -> do
                  logFunctionJson logger LL.Warn $ tableDiffExistsToObject tblDiff
                  pure Difference
                Right getTblDiff -> runResourceT $ do
                  liftIO $ logText LL.Info $ "[Starting to diff table " <> tblName <> "]"
                  tblDiffs <- getTblDiff
                  diff <- flip foldMapM tblDiffs $ \rowDiff -> do
                    liftIO $ logFunctionJson logger LL.Warn $ rowKeyDiffExistsToObject rowDiff
                    pure Difference
                  liftIO $ logText LL.Info $ "[Finished diffing table " <> tblName <> "]"
                  return diff

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
