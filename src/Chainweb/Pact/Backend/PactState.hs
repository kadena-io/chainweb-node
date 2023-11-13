{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

  , pactDiffMain
  )
  where

import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Control.Monad (forM, forM_, when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (ToJSON(..), (.=))
import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable qualified as F
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Merge.Lazy qualified as Merge
import Data.Map.Lazy qualified as M
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite3.Direct (Utf8(..), Database)
import Options.Applicative

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Utils (HasTextRepresentation, fromText, toText, int)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName, ChainId, chainIdToText, unsafeChainId)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (withSqliteDb)
import Chainweb.Pact.Backend.Compaction qualified as C

import System.Exit (exitFailure)
import System.Logger (LogLevel(..), loggerFunIO)
import System.Mem (performMajorGC)
import Data.LogMessage (TextLog(..), toLogMessage)

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

getLatestPactState :: Database -> Stream (Of TableDiffable) IO ()
getLatestPactState db = do
  S.map getActiveRows (getPactTables db)

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
diffLatestPactState :: Stream (Of TableDiffable) IO () -> Stream (Of TableDiffable) IO () -> Stream (Of (Text, Stream (Of RowKeyDiffExists) IO ())) IO ()
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
  liftIO performMajorGC

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
  deriving stock (Eq, Ord, Show)

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
  deriving stock (Eq, Ord, Show)

instance ToJSON PactRow where
  toJSON pr = Aeson.object
    [ "row_key" .= Text.decodeUtf8 pr.rowKey
    , "row_data" .= Text.decodeUtf8 pr.rowData
    , "tx_id" .= pr.txId
    ]

-- | Get the active rows of a table.
--
--   - sort the rows by row key
--   - group into chunks by rowkey
--   - for each chunk, keep only the latest txId
--   - shove all the (rowkey, rowdata) into a Map
getActiveRows :: Table -> TableDiffable
getActiveRows (Table name rows) = TableDiffable
  { name = name
  , rows = M.fromList
      $ List.map (pactRowToEntry . takeHead . List.sortOn (\pr -> Down pr.txId))
      $ List.groupBy (\x y -> x.rowKey == y.rowKey)
      $ List.sortOn (\pr -> pr.rowKey) rows
  }
  where
    takeHead :: [a] -> a
    takeHead = \case
      [] -> error "getLatestPactState.getActiveRows.takeHead: impossible case"
      (x : _) -> x

    pactRowToEntry :: PactRow -> (ByteString, ByteString)
    pactRowToEntry pr = (pr.rowKey, pr.rowData)

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

  cids <- getCids cfg.firstDbDir cfg.chainwebVersion

  diffyRef <- newIORef @(Map ChainId Diffy) M.empty

  forM_ cids $ \cid -> do
    C.withPerChainFileLogger cfg.logDir cid Info $ \logger -> do
      let resetDb = False

      withSqliteDb cid logger cfg.firstDbDir resetDb $ \(SQLiteEnv db1 _) -> do
        withSqliteDb cid logger cfg.secondDbDir resetDb $ \(SQLiteEnv db2 _) -> do
          loggerFunIO logger Info $ toLogMessage $
            TextLog "[Starting diff]"
          let diff = diffLatestPactState (getLatestPactState db1) (getLatestPactState db2)
          diffy <- S.foldMap_ id $ flip S.mapM diff $ \(tblName, tblDiff) -> do
            loggerFunIO logger Info $ toLogMessage $
              TextLog $ "[Starting table " <> tblName <> "]"
            d <- S.foldMap_ id $ flip S.mapM tblDiff $ \d -> do
              loggerFunIO logger Warn $ toLogMessage $
                TextLog $ Text.decodeUtf8 $ BSL.toStrict $
                  Aeson.encode $ rowKeyDiffExistsToObject d
              pure Difference
            loggerFunIO logger Info $ toLogMessage $
              TextLog $ "[Finished table " <> tblName <> "]"
            pure d

          loggerFunIO logger Warn $ toLogMessage $
            TextLog $ case diffy of
              Difference -> "[Non-empty diff]"
              NoDifference -> "[Empty diff]"
          loggerFunIO logger Info $ toLogMessage $
            TextLog $ "[Finished chain " <> chainIdToText cid <> "]"

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

getCids :: FilePath -> ChainwebVersion -> IO [ChainId]
getCids pactDbDir chainwebVersion = do
  -- Get the latest block height on chain 0 for the purpose of calculating all
  -- the chain ids at the current (version,height) pair
  latestBlockHeight <- C.withDefaultLogger Error $ \logger -> do
    let resetDb = False
    withSqliteDb (unsafeChainId 0) logger pactDbDir resetDb $ \(SQLiteEnv db _) -> do
      getLatestBlockHeight db
  pure $ List.sort $ F.toList $ chainIdsAt chainwebVersion latestBlockHeight
