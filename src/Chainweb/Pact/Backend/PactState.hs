{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# options_ghc -fno-warn-unused-top-binds #-}

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
  , TableDiff(..)

  , pactDiffMain
  , pactBreakdownMain
  )
  where

import Data.String (IsString)
import Data.Bifunctor (first)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Word (Word64)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Control.Lens (over)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BSL8
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
import GHC.Records (HasField(..))
import Options.Applicative
import Patience qualified
import Patience.Map qualified as PatienceM
import Patience.Delta (Delta(..))

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Utils (sshow, HasTextRepresentation, fromText, toText, int)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName, ChainId, chainIdToText, unsafeChainId)
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

import Pact.JSON.Encode qualified as J
import Pact.JSON.Decode qualified as JD
import Pact.JSON.Encode ((.=))
import Pact.Types.RowData (RowDataVersion(..), RowData(..), RowDataValue(..))
import Pact.Types.Exp (Literal(..))
import Pact.Types.Term (ObjectMap(..), Guard(..), ModRef(..))
import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S

import Unsafe.Coerce (unsafeCoerce)

checkpointerTables :: [Utf8]
checkpointerTables = ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"]

compactionTables :: [Utf8]
compactionTables = ["CompactGrandHash", "CompactActiveRow"]

sysTables :: [Utf8]
sysTables = ["SYS:usertables", "SYS:KeySets", "SYS:Modules", "SYS:Namespaces", "SYS:Pacts"]

nonUserTables :: [Utf8]
nonUserTables = checkpointerTables ++ compactionTables

data TableType
  = System
  | Compaction
  | User
  deriving stock (Eq, Ord, Show)

instance J.Encode TableType where
  build = \case
    System -> J.text "system"
    Compaction -> J.text "compaction"
    User -> J.text "user"

prettyTableType :: TableType -> String
prettyTableType = \case
  System -> "System table"
  Compaction -> "Compaction table"
  User -> "User table"

mkTableType :: Utf8 -> TableType
mkTableType tbl
  | tbl `elem` checkpointerTables = System
  | tbl `elem` sysTables = System
  | tbl `elem` compactionTables = Compaction
  | otherwise = User

buildW64 :: Word64 -> J.Builder
buildW64 = unsafeCoerce BB.word64Dec

buildI64 :: Int64 -> J.Builder
buildI64 = unsafeCoerce BB.int64Dec

data Sized a = Sized
  { sizeBytes :: Word64
  , item :: a
  }
  deriving stock (Eq, Ord, Show)

instance (J.Encode a) => J.Encode (Sized a) where
  build s = J.object
    [ "size_bytes" .= buildW64 s.sizeBytes
    , "item" .= s.item
    ]

data Table = Table
  { name :: Text
  , typ :: TableType
  , rows :: [PactRow]
  }
  deriving stock (Eq, Ord, Show)

instance J.Encode Table where
  build tbl = J.object
    [ "name" .= tbl.name
    , "type" .= tbl.typ
    ]

data SizedTable = SizedTable
  { name :: Text
  , typ :: TableType
  , sizeBytes :: Word64
  , rows :: [SizedPactRow]
  }
  deriving stock (Eq, Ord, Show)

instance J.Encode SizedTable where
  build tbl = J.object
    [ "name" .= tbl.name
    , "type" .= tbl.typ
    , "size_bytes" .= buildW64 tbl.sizeBytes
    , "sized_rows" .= J.array tbl.rows
    ]

getPactSizedTableNames :: Database -> IO (Vector (Sized Utf8))
getPactSizedTableNames db = do
  let sortedTableNames :: [[SType]] -> [Sized Utf8]
      sortedTableNames rows =
        List.sortOn (\s -> s.item)
        $ flip List.map rows $ \case
            [SText tbl, SInt tblSize] -> Sized (fromIntegral @_ @Word64 tblSize) tbl
            _ -> error "getPactTables.sortedTableNames: expected (text, int)"

  let qryText = "SELECT name, SUM(\"pgsize\") table_size FROM \"dbstat\" WHERE name NOT LIKE \"sqlite_%\" AND name NOT LIKE \"%_ix\" AND name NOT LIKE \"transactionIndexByBH\" GROUP BY name ORDER BY table_size DESC"
  tables <- sortedTableNames <$> Pact.qry db qryText [] [RText, RInt]
  pure (Vector.fromList tables)

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
        _ -> error "getPactTables.sortedTableNames: expected text"

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
--
--   The 'MVar' 'Word' argument is supposed to be supplied as a 'newEmptyMVar'.
--   This will get filled with the number of tables, once it is known.
--
--   The ['Utf8'] argument are tables to exclude.
getPactTables :: ()
  => [Utf8]
  -> Database
  -> MVar Word
  -> Stream (Of Table) IO ()
getPactTables excludedTables db numTables = do
  let fmtTable x = "\"" <> x <> "\""

  tables <- liftIO $ getPactTableNames db

  liftIO $ putMVar numTables (fromIntegral (Vector.length tables))

  forM_ tables $ \tbl -> do
    when (tbl `notElem` excludedTables) $ do
      let qryText = "SELECT rowkey, rowdata, txid FROM "
            <> fmtTable tbl
      userRows <- liftIO $ Pact.qry db qryText [] [RText, RBlob, RInt]
      shapedRows <- forM userRows $ \case
        [SText (Utf8 rowKey), SBlob rowData, SInt txId] -> do
          pure $ PactRow {..}
        _ -> error "getPactTables: unexpected shape of user table row"
      S.yield $ Table
        { name = utf8ToText tbl
        , typ = mkTableType tbl
        , rows = shapedRows
        }

getPactSizedTables :: ()
  => [Utf8]
  -> Database
  -> Stream (Of SizedTable) IO ()
getPactSizedTables excludedTables db = do
  let fmtTable :: (IsString s, Monoid s) => s -> s
      fmtTable x = "\"" <> x <> "\""

  tables <- liftIO $ getPactSizedTableNames db

  forM_ tables $ \(Sized sz tbl) -> do
    when (tbl `notElem` excludedTables) $ do
      let qryText = "SELECT rowkey, rowdata, txid FROM "
            <> fmtTable tbl
      userRows <- liftIO $ Pact.qry db qryText [] [RText, RBlob, RInt]
      shapedRows <- forM userRows $ \case
        [SText (Utf8 rowKey), SBlob rowData, SInt txId] -> do
          pure $ sizeTagPactRow (utf8ToText tbl, rowKey) $ PactRow {..}
        _ -> error "getPactSizedTables: unexpected shape of user table row"
      let rows =
            List.sortOn (\r -> Down r.sizedRowData.size)
            $ List.filter (\r -> r.sizedRowData.size > oneKB) shapedRows
      when (not (null rows)) $ do
        S.yield $ SizedTable
          { name = utf8ToText tbl
          , typ = mkTableType tbl
          , sizeBytes = sz
          , rows = rows
          }

getLatestPactState :: Database -> Stream (Of Table) IO ()
getLatestPactState db = do
  numTablesVar <- liftIO newEmptyMVar

  let go :: Word -> Stream (Of Table) IO () -> Stream (Of Table) IO ()
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

  e <- liftIO $ S.next (getPactTables nonUserTables db numTablesVar)
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
--   because `getPactTables` sorts the table names, and `getLatestPactState`
--   sorts the [PactRow] by rowKey.
--
-- If we ever find two tables that are not the same, we throw an error.
--
-- This diminishes the utility of comparing two pact states that are known to be
-- at different heights, but that hurts our ability to perform the diff in
-- constant memory.
diffLatestPactState :: Stream (Of Table) IO () -> Stream (Of Table) IO () -> Stream (Of TableDiff) IO ()
diffLatestPactState s1 s2 = do
  let diff :: Table -> Table -> TableDiff
      diff tbl1 tbl2
        | tbl1.name /= tbl2.name = error "diffLatestPactState: mismatched table names"
        | otherwise = TableDiff tbl1.name
            $ List.filter (not . PatienceM.isSame)
            $ Patience.pairItems (\x y -> x.rowKey == y.rowKey)
            $ Patience.diff tbl1.rows tbl2.rows

  S.zipWith diff s1 s2

data TableDiff = TableDiff
  { name :: !Text
  , rowDiff :: [Delta PactRow]
  }
  deriving stock (Eq, Ord, Show)

instance J.Encode TableDiff where
  build td = J.object
    [ "table_name" .= td.name
    , "row_diff" .= J.array (List.map deltaToObject td.rowDiff)
    ]
    where
      deltaToObject :: (J.Encode a) => Delta a -> J.Builder
      deltaToObject = \case
        Old x -> J.object
          [ "old" .= x
          ]
        New x -> J.object
          [ "new" .= x
          ]
        Delta x y -> J.object
          [ "old" .= x
          , "new" .= y
          ]
        Same _ -> J.null

data PactRow = PactRow
  { rowKey :: ByteString
  , rowData :: ByteString
  , txId :: Int64
  }
  deriving stock (Eq, Ord, Show)

instance J.Encode PactRow where
  build pr = J.object
    [ "row_key" .= Text.decodeUtf8 pr.rowKey
    , "row_data" .= Text.decodeUtf8 pr.rowData
    , "tx_id" .= buildI64 pr.txId
    ]

sizeTagPactRow :: (Text, ByteString) -> PactRow -> SizedPactRow
sizeTagPactRow dbgInfo pr = SizedPactRow
  { rowKey = pr.rowKey
  , sizedRowData = sizeTagRowData dbgInfo oneMB pr.rowData
  , txId = pr.txId
  }

data SizedPactRow = SizedPactRow
  { rowKey :: ByteString
  , sizedRowData :: SizedRowData
  , txId :: Int64
  }
  deriving stock (Eq, Ord, Show)

instance J.Encode SizedPactRow where
  build pr = J.object
    [ "row_key" .= Text.decodeUtf8 pr.rowKey
    , "sized_row_data" .= pr.sizedRowData
    , "tx_id" .= buildI64 pr.txId
    ]

getActiveRows :: Table -> Table
getActiveRows tbl = Table
  { name = tbl.name
  , typ = tbl.typ
  , rows =
      List.map (takeHead . List.sortOn (\rd -> Down rd.txId))
      $ List.groupBy (\rd1 rd2 -> rd1.rowKey == rd2.rowKey)
      $ List.sortOn (\rd -> rd.rowKey) tbl.rows
  }
  where
    takeHead :: [a] -> a
    takeHead = \case
      [] -> error "getLatestPactState.getActiveRows.takeHead: impossible case"
      (x : _) -> x

utf8ToText :: Utf8 -> Text
utf8ToText (Utf8 u) = Text.decodeUtf8 u

data SizedRowDataValue
  = SRDLiteral Word64 Literal
  | SRDList Word64 (Vector SizedRowDataValue)
  | SRDObject Word64 (ObjectMap SizedRowDataValue)
  | SRDGuard Word64 (Guard SizedRowDataValue)
  | SRDModRef Word64 ModRef
  deriving stock (Eq, Ord, Show)

instance J.Encode SizedRowDataValue where
  build = \case
    SRDLiteral sz l -> J.object
      [ "size" .= buildW64 sz
      , "literal" .= l
      ]
    SRDList sz v -> J.object
      [ "size" .= buildW64 sz
      , "list" .= J.array v
      ]
    SRDObject sz o -> J.object
      [ "size" .= buildW64 sz
      , "object" .= o
      ]
    SRDGuard sz g -> J.object
      [ "size" .= buildW64 sz
      , "guard" .= g
      ]
    SRDModRef sz (ModRef refName refSpec _) -> J.object
      [ "size" .= buildW64 sz
      , "mod_ref" .= J.object
          [ "ref_spec" .= fmap J.array refSpec
          , "ref_name" .= refName
          ]
      ]

jsonSize :: (J.Encode a) => a -> Word64
jsonSize x = fromIntegral (BS.length (J.encodeStrict x))

sizeTagRowDataValue :: RowDataValue -> SizedRowDataValue
sizeTagRowDataValue = go
  where
    go :: RowDataValue -> SizedRowDataValue
    go rdv =
      let
        topLevelSize = jsonSize rdv
      in
      case rdv of
        RDLiteral l -> SRDLiteral topLevelSize l
        RDList ls -> SRDList topLevelSize (recur ls)
        RDObject o -> SRDObject topLevelSize (recur o)
        RDGuard g -> SRDGuard topLevelSize (recur g)
        RDModRef m -> SRDModRef topLevelSize m

    recur :: (Functor f) => f RowDataValue -> f SizedRowDataValue
    recur = fmap go

data SizedRowData
  = SRDUnderThreshold Word64
  | SRDAboveThreshold Word64 RowDataVersion (ObjectMap SizedRowDataValue)
  deriving stock (Eq, Ord, Show)

instance HasField "size" SizedRowData Word64 where
  getField = \case
    SRDUnderThreshold sz -> sz
    SRDAboveThreshold sz _ _ -> sz

instance J.Encode SizedRowData where
  build = \case
    SRDUnderThreshold w -> J.object
      [ "tag" .= J.text "under_threshold"
      , "size" .= buildW64 w
      ]
    SRDAboveThreshold sz v d -> J.object
      [ "tag" .= J.text "above_threshold"
      , "version" .= v
      , "size" .= buildW64 sz
      , "sized_data" .= d
      ]

sizeTagRowData :: (Text, ByteString) -> Word64 -> ByteString -> SizedRowData
sizeTagRowData (tblName, rowKey) threshold rdBytes
  | len < threshold = SRDUnderThreshold len
  | tblName == "SYS:Modules" = SRDUnderThreshold len
  | otherwise = case JD.eitherDecodeStrict' @RowData rdBytes of
      Left err -> error $ "sizeTagRowData: (tblName = " ++ Text.unpack tblName ++ ", rowKey = " ++ BS8.unpack rowKey ++ ") invalid rowData: " ++ err
      Right rd -> SRDAboveThreshold
        len
        rd._rdVersion
        (fmap sizeTagRowDataValue rd._rdData)
  where
    len = fromIntegral @_ @Word64 (BS.length rdBytes)

data ChainSizeInfo = ChainSizeInfo
  { totalSizeBytes :: Word64
  , tableSizes :: Vector SizedTable
  }

instance J.Encode ChainSizeInfo where
  build cInfo = J.object
    [ "total_size_bytes" .= buildW64 cInfo.totalSizeBytes
    , "table_sizes" .= J.array cInfo.tableSizes
    ]

mkChainSizeInfo :: Vector SizedTable -> ChainSizeInfo
mkChainSizeInfo tbls = ChainSizeInfo
  { totalSizeBytes = Vector.foldl' (\acc tbl -> acc + tbl.sizeBytes) 0 tbls
  , tableSizes = tbls
  }

data PactBreakdown = PactBreakdown
  { totalSizeBytes :: Word64
  , sizes :: Map ChainId ChainSizeInfo
  }

instance J.Encode PactBreakdown where
  build b = J.object
    [ "total_size_bytes" .= buildW64 b.totalSizeBytes
    , "chain_sizes" .= J.Object (List.map (first chainIdToText) (M.toList b.sizes))
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
        put $ Text.unpack tbl.name ++
          " (" ++ prettyTableType tbl.typ ++ "): " ++
          showBytes tbl.sizeBytes

data PactBreakdownConfig = PactBreakdownConfig
  { pactDbDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , numThreads :: Int
  }

pactBreakdownMain :: IO ()
pactBreakdownMain = do
  cfg <- execParser opts

  cids <- getCids cfg.pactDbDir cfg.chainwebVersion

  sizesRef <- newIORef @(Map ChainId (Vector SizedTable)) M.empty

  flip (pooledMapConcurrentlyN_ cfg.numThreads) cids $ \cid -> do
    C.withDefaultLogger Error $ \logger -> do
      let resetDb = False
      withSqliteDb cid logger cfg.pactDbDir resetDb $ \(SQLiteEnv db _) -> do
        e <- S.next (getPactSizedTables nonUserTables db)
        case e of
          Left () -> do
            -- the stream was entirely empty, but we need an entry
            atomicModifyIORef' sizesRef $ \m -> (M.insert cid Vector.empty m, ())
          Right (tbl, rest) -> do
            sizedTables <- (tbl :) <$> S.toList_ rest
            atomicModifyIORef' sizesRef $ \m -> (M.insert cid (Vector.fromList sizedTables) m, ())

  sizes <- readIORef sizesRef
  let chainSizeInfos = M.map mkChainSizeInfo sizes
  let breakdown = PactBreakdown
        { totalSizeBytes = M.foldl' (\acc cInfo -> acc + cInfo.totalSizeBytes) 0 chainSizeInfos
        , sizes = chainSizeInfos
        }
  BSL8.hPut IO.stdout (J.encode breakdown)

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
  { firstDbDir :: FilePath
  , secondDbDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , logDir :: FilePath
  , numThreads :: Int
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

  flip (pooledMapConcurrentlyN_ cfg.numThreads) cids $ \cid -> do
    C.withPerChainFileLogger cfg.logDir cid Debug $ \logger' -> do
      let logger = over setLoggerScope (("chain-id", sshow cid) :) logger'
      let resetDb = False
      withSqliteDb cid logger cfg.firstDbDir resetDb $ \(SQLiteEnv db1 _) -> do
        withSqliteDb cid logger cfg.secondDbDir resetDb $ \(SQLiteEnv db2 _) -> do
          let diff = diffLatestPactState (getLatestPactState db1) (getLatestPactState db2)
          diffy <- S.foldMap_ id $ flip S.mapM diff $ \td -> do
            loggerFunIO logger Warn $ toLogMessage $
              TextLog $ Text.decodeUtf8 $ J.encodeStrict td
            pure Difference
          atomicModifyIORef' diffyRef $ \m -> (M.insert cid diffy m, ())

  diffy <- readIORef diffyRef
  forM_ (M.toAscList diffy) $ \(cid, d) -> do
    when (d == Difference) $ do
      Text.putStrLn $ "Non-empty diff on chain " <> chainIdToText cid
  when (M.size diffy > 0) $ do
    exitFailure
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
    w2d :: Word64 -> Double
    w2d = fromIntegral

oneKB :: Word64
oneKB = 1024

oneMB :: Word64
oneMB = oneKB * oneKB
