{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Pact.Backend.PactState
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: see LICENSE.md
--
-- Diff Pact state pre- and post-compaction.
--
-- There are other utilities provided by this module whose purpose is either
-- to get the pact state or perform diffs, without compacting.
--
-- The code in this module operates primarily on 'Stream's, because the amount
-- of user data can grow quite large. by comparing one table at a time, we can
-- keep maximum memory utilisation in check.
--

module Chainweb.Pact.Backend.PactState
  ( getPactTables
  , getPactUserTables
  , getLatestPactState
  , getLatestBlockHeight

  , PactRow(..)
  , UserTable(..)
  , UserTableDiff(..)

  , pactDiffMain
  , pactBreakdownMain
  )
  where

import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Word (Word64)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Control.Lens (over)
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
import Data.Map.Strict qualified as M
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite3.Direct (Utf8(..), Database)
import Options.Applicative
import Patience qualified
import Patience.Map qualified as PatienceM
import Patience.Delta (Delta(..))

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Utils (sshow, HasTextRepresentation, fromText, toText, int)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName, ChainId, unsafeChainId, chainIdToText)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (withSqliteDb)
import Chainweb.Pact.Backend.Compaction qualified as C

import System.Exit (exitFailure)
import System.IO qualified as IO
import System.Logger (LogLevel(..), setLoggerScope, loggerFunIO)
import Data.LogMessage (TextLog(..), toLogMessage)

import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S

checkpointerTables :: [Utf8]
checkpointerTables = ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"]

compactionTables :: [Utf8]
compactionTables = ["CompactGrandHash", "CompactActiveRow"]

sysTables :: [Utf8]
sysTables = ["SYS:usertables", "SYS:KeySets", "SYS:Modules", "SYS:Namespaces", "SYS:Pacts"]

excludedTables :: [Utf8]
excludedTables = checkpointerTables ++ compactionTables

data TableType
  = TableTypeSystem
  | TableTypeCompaction
  | TableTypeUser
  deriving stock (Eq)

instance ToJSON TableType where
  toJSON = \case
    TableTypeSystem -> "system"
    TableTypeCompaction -> "compaction"
    TableTypeUser -> "user"

prettyTableType :: TableType -> String
prettyTableType = \case
  TableTypeSystem -> "System table"
  TableTypeCompaction -> "Compaction table"
  TableTypeUser -> "User table"

data SizedTable = SizedTable
  { tableName :: Text
  , tableSizeBytes :: Word64
  , tableType :: TableType
  }

instance ToJSON SizedTable where
  toJSON tbl = Aeson.object
    [ "table_name" .= tbl.tableName
    , "table_size_bytes" .= tbl.tableSizeBytes
    , "table_type" .= tbl.tableType
    ]

getTableSizesBytes :: Database -> IO [SizedTable]
getTableSizesBytes db = do
  let qryText = "SELECT name, SUM(\"pgsize\") table_size FROM \"dbstat\" GROUP BY name ORDER BY table_size DESC"
  Pact.qry db qryText [] [RText, RInt] >>= mapM go
  where
    go :: [SType] -> IO SizedTable
    go = \case
      [SText tbl, SInt tblSize] -> do
        let tblType =
              if | tbl `elem` checkpointerTables -> TableTypeSystem
                 | tbl `elem` sysTables -> TableTypeSystem
                 | tbl `elem` compactionTables -> TableTypeCompaction
                 | otherwise -> TableTypeUser
        pure (SizedTable (utf8ToText tbl) (fromIntegral tblSize) tblType)
      _ -> error "getTableSizesBytes: expected (text, int)"

getLatestBlockHeight :: Database -> IO BlockHeight
getLatestBlockHeight db = do
  let qryText = "SELECT MAX(blockheight) FROM BlockHistory"
  Pact.qry db qryText [] [RInt] >>= \case
    [[SInt bh]] -> pure (BlockHeight (int bh))
    _ -> error "getLatestBlockHeight: expected int"

getPactTables :: Database -> IO (Vector Utf8)
getPactTables db = do
  let sortedTableNames :: [[SType]] -> [Utf8]
      sortedTableNames rows = M.elems $ M.fromListWith const $ flip List.map rows $ \case
        [SText u] -> (Text.toLower (utf8ToText u), u)
        _ -> error "getPactUserTables.sortedTableNames: expected text"

  tables <- fmap sortedTableNames $ do
    let qryText =
          "SELECT name FROM sqlite_schema \
          \WHERE \
          \  type = 'table' \
          \AND \
          \  name NOT LIKE 'sqlite_%'"
    Pact.qry db qryText [] [RText]

  pure (Vector.fromList tables)

-- | Get all of the rows for each user table. The tables will be sorted.
--
--   The 'MVar' 'Word' argument is supposed to be supplied as a 'newEmptyMVar'.
--   This will get filled with the number of tables, once it is known.
getPactUserTables :: Database -> MVar Word -> Stream (Of UserTable) IO ()
getPactUserTables db numTables = do
  let fmtTable x = "\"" <> x <> "\""

  tables <- liftIO $ getPactTables db

  liftIO $ putMVar numTables (fromIntegral (Vector.length tables))

  forM_ tables $ \tbl -> do
    if tbl `notElem` excludedTables
    then do
      let qryText = "SELECT rowkey, rowdata, txid FROM "
            <> fmtTable tbl
      userRows <- liftIO $ Pact.qry db qryText [] [RText, RBlob, RInt]
      shapedRows <- forM userRows $ \case
        [SText (Utf8 rowKey), SBlob rowData, SInt txId] -> do
          pure $ PactRow {..}
        _ -> error "getPactUserTables: unexpected shape of user table row"
      S.yield $ UserTable (utf8ToText tbl) shapedRows
    else do
      pure ()

getLatestPactState :: Database -> Stream (Of UserTable) IO ()
getLatestPactState db = do
  numTablesVar <- liftIO newEmptyMVar

  let go :: Word -> Stream (Of UserTable) IO () -> Stream (Of UserTable) IO ()
      go !tablesRemaining s = do
        if tablesRemaining == 0
        then do
          pure ()
        else do
          e <- liftIO $ S.next s
          case e of
            Left () -> do
              pure ()
            Right (userTable, rest) -> do
              S.yield (getActiveRows userTable)
              go (tablesRemaining - 1) rest

  e <- liftIO $ S.next (getPactUserTables db numTablesVar)
  case e of
    Left () -> do
      pure ()
    Right (userTable, rest) -> do
      numRows <- liftIO $ takeMVar numTablesVar
      when (numRows > 0) $ do
        S.yield (getActiveRows userTable)
        go (numRows - 1) rest

-- This assumes the same tables (essentially zipWith).
--   Note that this assumes we got the state from `getLatestPactState`,
--   because `getPactUserTables` sorts the table names, and `getLatestPactState`
--   sorts the [PactRow] by rowKey.
--
-- If we ever find two tables that are not the same, we throw an error.
--
-- This diminishes the utility of comparing two pact states that are known to be
-- at different heights, but that hurts our ability to perform the diff in
-- constant memory.
diffLatestPactState :: Stream (Of UserTable) IO () -> Stream (Of UserTable) IO () -> Stream (Of UserTableDiff) IO ()
diffLatestPactState s1 s2 = do
  let diff :: UserTable -> UserTable -> UserTableDiff
      diff ut1 ut2
        | ut1.tableName /= ut2.tableName = error "diffLatestPactState: mismatched table names"
        | otherwise = UserTableDiff ut1.tableName
            $ List.filter (not . PatienceM.isSame)
            $ Patience.pairItems (\x y -> x.rowKey == y.rowKey)
            $ Patience.diff ut1.rows ut2.rows

  S.zipWith diff s1 s2

data UserTableDiff = UserTableDiff
  { tableName :: !Text
  , rowDiff :: [Delta PactRow]
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON UserTableDiff where
  toJSON utd = Aeson.object
    [ "table_name" .= utd.tableName
    , "row_diff" .= List.map deltaToObject utd.rowDiff
    ]
    where
      deltaToObject :: (ToJSON a) => Delta a -> Aeson.Value
      deltaToObject = \case
        Old x -> Aeson.object
          [ "old" .= x
          ]
        New x -> Aeson.object
          [ "new" .= x
          ]
        Delta x y -> Aeson.object
          [ "old" .= x
          , "new" .= y
          ]
        Same _ -> Aeson.Null

data UserTable = UserTable
  { tableName :: !Text
  , rows :: [PactRow]
  }
  deriving stock (Eq, Ord, Show)

data PactRow = PactRow
  { rowKey :: !ByteString
  , rowData :: !ByteString
  , txId :: !Int64
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON PactRow where
  toJSON pr = Aeson.object
    [ "row_key" .= Text.decodeUtf8 pr.rowKey
    , "row_data" .= Text.decodeUtf8 pr.rowData
    , "tx_id" .= pr.txId
    ]

getActiveRows :: UserTable -> UserTable
getActiveRows (UserTable name rows) = UserTable name
  $ List.map (takeHead . List.sortOn (Down . txId))
  $ List.groupBy (\x y -> rowKey x == rowKey y)
  $ List.sortOn rowKey rows
  where
    takeHead :: [a] -> a
    takeHead = \case
      [] -> error "getLatestPactState.getActiveRows.takeHead: impossible case"
      (x : _) -> x

utf8ToText :: Utf8 -> Text
utf8ToText (Utf8 u) = Text.decodeUtf8 u

data ChainSizeInfo = ChainSizeInfo
  { totalSizeBytes :: Word64
  , tableSizes :: [SizedTable]
  }

instance ToJSON ChainSizeInfo where
  toJSON cInfo = Aeson.object
    [ "total_size_bytes" .= cInfo.totalSizeBytes
    , "table_sizes" .= cInfo.tableSizes
    ]

mkChainSizeInfo :: [SizedTable] -> ChainSizeInfo
mkChainSizeInfo tbls = ChainSizeInfo
  { totalSizeBytes = List.foldl' (\acc tbl -> acc + tbl.tableSizeBytes) 0 tbls
  , tableSizes = tbls
  }

data PactBreakdown = PactBreakdown
  { totalSizeBytes :: Word64
  , sizes :: Map ChainId ChainSizeInfo
  }

instance ToJSON PactBreakdown where
  toJSON b = Aeson.object
    [ "total_size_bytes" .= b.totalSizeBytes
    , "chain_sizes" .= b.sizes
    ]

reportBreakdown :: PactBreakdown -> IO ()
reportBreakdown breakdown = do
  IO.withFile "report.txt" IO.AppendMode $ \h -> do
    let put = IO.hPutStrLn h
    put $ "Total Size of All Chains: " ++ showBytes breakdown.totalSizeBytes
    forM_ (M.toAscList breakdown.sizes) $ \(cid, cInfo) -> do
      put ""
      put $ "Chain " ++ Text.unpack (chainIdToText cid)
      put $ "Total Size: " ++ showBytes cInfo.totalSizeBytes
      forM_ cInfo.tableSizes $ \tbl -> do
        put $ Text.unpack tbl.tableName ++
          " (" ++ prettyTableType tbl.tableType ++ "): " ++
          showBytes tbl.tableSizeBytes

data PactBreakdownConfig = PactBreakdownConfig
  { pactDbDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , numThreads :: Int
  }

pactBreakdownMain :: IO ()
pactBreakdownMain = do
  cfg <- execParser opts

  cids <- getCids cfg.pactDbDir cfg.chainwebVersion

  sizesRef <- newIORef @(Map ChainId [SizedTable]) M.empty

  flip (pooledMapConcurrentlyN_ cfg.numThreads) cids $ \cid -> do
    C.withDefaultLogger Error $ \logger -> do
      let resetDb = False
      withSqliteDb cid logger cfg.pactDbDir resetDb $ \(SQLiteEnv db _) -> do
        sizedTables <- getTableSizesBytes db
        atomicModifyIORef' sizesRef $ \m -> (M.insert cid sizedTables m, ())

  sizes <- readIORef sizesRef
  let chainSizeInfos = M.map mkChainSizeInfo sizes
  let breakdown = PactBreakdown
        { totalSizeBytes = M.foldl' (\acc cInfo -> acc + cInfo.totalSizeBytes) 0 chainSizeInfos
        , sizes = chainSizeInfos
        }
  reportBreakdown breakdown

  where
    opts :: ParserInfo PactBreakdownConfig
    opts = info (parser <**> helper)
      (fullDesc <> progDesc "Pact DB compare-and-compare")

    parser :: Parser PactBreakdownConfig
    parser = PactBreakdownConfig
      <$> strOption
           (long "pact-database-dir"
            <> metavar "PACT_DB_DIRECTORY"
            <> help "Pact database directory")
      <*> (fmap (lookupVersionByName . fromTextSilly @ChainwebVersionName) $ strOption
           (long "graph-version"
            <> metavar "CHAINWEB_VERSION"
            <> help "Chainweb version for graph. Only needed for non-standard graphs."
            <> value (toText (_versionName mainnet))
            <> showDefault))
      <*> option auto
           (long "threads"
            <> metavar "NUM_THREADS"
            <> help "Number of threads on which to run compaction."
            <> value 4)

data PactDiffConfig = PactDiffConfig
  { pactDbDir :: FilePath
  , compactDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , logDir :: FilePath
  , numThreads :: Int
  }

pactDiffMain :: IO ()
pactDiffMain = do
  cfg <- execParser opts

  when (cfg.pactDbDir == cfg.compactDir) $ do
    Text.putStrLn "Pact database directory and compacted Pact database directory cannot be the same."
    exitFailure

  cids <- getCids cfg.pactDbDir cfg.chainwebVersion

  flip (pooledMapConcurrentlyN_ cfg.numThreads) cids $ \cid -> do
    C.withPerChainFileLogger cfg.logDir cid Debug $ \logger' -> do
      let logger = over setLoggerScope (("chain-id", sshow cid) :) logger'
      let resetDb = False
      withSqliteDb cid logger cfg.compactDir resetDb $ \(SQLiteEnv cpDb _) -> do
        latestBlockHeight <- getLatestBlockHeight cpDb
        void $ C.compact latestBlockHeight logger cpDb []

        withSqliteDb cid logger cfg.pactDbDir resetDb $ \(SQLiteEnv db _) -> do
          diffEmptyRef <- newIORef True

          let diff = diffLatestPactState (getLatestPactState db) (getLatestPactState cpDb)
          flip S.mapM_ diff $ \utd -> do
            writeIORef diffEmptyRef False
            loggerFunIO logger Warn $ toLogMessage $
              TextLog $ Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode utd

          diffEmpty <- readIORef diffEmptyRef
          when (not diffEmpty) $ do
            loggerFunIO logger Warn $ toLogMessage $
              TextLog $ "Non-empty diff."
            exitFailure
  where
    opts :: ParserInfo PactDiffConfig
    opts = info (parser <**> helper)
      (fullDesc <> progDesc "Pact DB compare-and-compare")

    parser :: Parser PactDiffConfig
    parser = PactDiffConfig
      <$> strOption
           (long "pact-database-dir"
            <> metavar "PACT_DB_DIRECTORY"
            <> help "Pact database directory")
      <*> strOption
           (long "compact-dir"
            <> metavar "PACT_DB_DIRECTORY"
            <> help "Copy of the pact database directory, will be compacted")
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
      <*> option auto
           (short 't'
            <> long "threads"
            <> metavar "NUM_THREADS"
            <> help "Number of threads on which to run compaction."
            <> value 4)

fromTextSilly :: HasTextRepresentation a => Text -> a
fromTextSilly t = case fromText t of
  Just a -> a
  Nothing -> error "fromText failed"

getCids :: FilePath -> ChainwebVersion -> IO [ChainId]
getCids pactDbDir chainwebVersion = do
  -- Get the latest block height on chain 0 for the purpose of calculating all
  -- the chain ids at the current (version,height) pair
  latestBlockHeight <- C.withDefaultLogger Error $ \logger -> do
    let resetDb = False
    withSqliteDb (unsafeChainId 0) logger pactDbDir resetDb $ \(SQLiteEnv db _) -> do
      getLatestBlockHeight db
  pure $ List.sort $ F.toList $ chainIdsAt chainwebVersion latestBlockHeight

showBytes :: Word64 -> String
showBytes bytes
  | bytes > oneMB = show (w2d bytes / w2d oneMB) ++ " MB"
  | otherwise = show bytes ++ " bytes"
  where
    oneMB :: Word64
    oneMB = 1024 * 1024

    w2d :: Word64 -> Double
    w2d = fromIntegral
