{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Chainweb.Pact.ChainwebPactDb
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- SQLite interaction utilities.

module Chainweb.Pact.Backend.Utils
  ( -- * General utils
    chainDbFileName
    -- * Shared Pact database interactions
  , doLookupSuccessful
  , commitBlockStateToDatabase
  , createVersionedTable
  , tbl
  , initSchema
    -- * Savepoints
  , withSavepoint
  , beginSavepoint
  , commitSavepoint
  , rollbackSavepoint
  , abortSavepoint
  , SavepointName(..)
  -- * SQLite conversions and assertions
  , toUtf8
  , fromUtf8
  , toTextUtf8
  , asStringUtf8
  , domainTableName
  , domainTableNameCore
  , tableNameCore
  , convKeySetName
  , convKeySetNameCore
  , convModuleName
  , convModuleNameCore
  , convNamespaceName
  , convNamespaceNameCore
  , convRowKey
  , convRowKeyCore
  , convPactId
  , convPactIdCore
  , convHashedModuleName
  , convSavepointName
  , expectSingleRowCol
  , expectSingle
  , execMulti
  -- * SQLite runners
  , withSqliteDb
  , startSqliteDb
  , startReadSqliteDb
  , stopSqliteDb
  , withSQLiteConnection
  , openSQLiteConnection
  , closeSQLiteConnection
  , withTempSQLiteConnection
  , withInMemSQLiteConnection
  -- * SQLite Pragmas
  , chainwebPragmas
  ) where

import Control.Exception (SomeAsyncException, evaluate)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict

import Data.Bits
import Data.Foldable
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.SQLite3.Direct as SQ3

import Prelude hiding (log)

import System.Directory
import System.FilePath
import System.LogLevel

-- pact

import Pact.Types.Persistence
import Pact.Types.SQLite
import Pact.Types.Term
    (KeySetName(..), ModuleName(..), NamespaceName(..), PactId(..))
import Pact.Types.Util (AsString(..))

import qualified Pact.Core.Names as PCore
import qualified Pact.Core.Persistence as PCore
import qualified Pact.Core.Guards as PCore


-- chainweb

import Chainweb.Logger
import Chainweb.Pact.Backend.SQLite.DirectV2

import Chainweb.Pact.Types
import Chainweb.Version
import Chainweb.Utils
import Chainweb.BlockHash
import Chainweb.BlockHeight
import Database.SQLite3.Direct
import GHC.Stack (HasCallStack)
import qualified Data.ByteString.Short as SB
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HashMap
import Chainweb.Utils.Serialization
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Pact.Types.Persistence as Pact4
import Chainweb.Pact.Backend.Types
import qualified Pact.Core.Persistence as Pact5
import Network.Wai.Middleware.OpenApi (HasReadOnly(readOnly))

-- -------------------------------------------------------------------------- --
-- SQ3.Utf8 Encodings
instance AsString (PCore.Domain k v b i) where
    asString = PCore.renderDomain


instance AsString (PCore.TableName) where
    asString = PCore.renderTableName

toUtf8 :: T.Text -> SQ3.Utf8
toUtf8 = SQ3.Utf8 . T.encodeUtf8
{-# INLINE toUtf8 #-}

fromUtf8 :: SQ3.Utf8 -> T.Text
fromUtf8 (SQ3.Utf8 bytes) = T.decodeUtf8 bytes
{-# INLINE fromUtf8 #-}

toTextUtf8 :: HasTextRepresentation a => a -> SQ3.Utf8
toTextUtf8 = toUtf8 . toText
{-# INLINE toTextUtf8 #-}

asStringUtf8 :: AsString a => a -> SQ3.Utf8
asStringUtf8 = toUtf8 . asString
{-# INLINE asStringUtf8 #-}

domainTableName :: Pact4.Domain k v -> SQ3.Utf8
domainTableName = asStringUtf8

convKeySetName :: KeySetName -> SQ3.Utf8
convKeySetName = toUtf8 . asString

domainTableNameCore :: PCore.Domain k v b i -> SQ3.Utf8
domainTableNameCore = asStringUtf8

tableNameCore :: PCore.TableName -> SQ3.Utf8
tableNameCore = asStringUtf8

convKeySetNameCore :: PCore.KeySetName -> SQ3.Utf8
convKeySetNameCore = toUtf8 . PCore.renderKeySetName

convModuleName
  :: Bool
     -- ^ whether to apply module name fix
  -> ModuleName
  -> SQ3.Utf8
convModuleName False (ModuleName name _) = toUtf8 name
convModuleName True mn = asStringUtf8 mn

convModuleNameCore
  :: Bool
     -- ^ whether to apply module name fix
  -> PCore.ModuleName
  -> SQ3.Utf8
convModuleNameCore False (PCore.ModuleName name _) = toUtf8 name
convModuleNameCore True mn = toUtf8 $ PCore.renderModuleName mn


convNamespaceName :: NamespaceName -> SQ3.Utf8
convNamespaceName (NamespaceName name) = toUtf8 name

convNamespaceNameCore :: PCore.NamespaceName -> SQ3.Utf8
convNamespaceNameCore (PCore.NamespaceName name) = toUtf8 name

convRowKey :: RowKey -> SQ3.Utf8
convRowKey (RowKey name) = toUtf8 name

convRowKeyCore :: PCore.RowKey -> SQ3.Utf8
convRowKeyCore (PCore.RowKey name) = toUtf8 name

convPactId :: PactId -> SQ3.Utf8
convPactId = toUtf8 . sshow

-- to match legacy keys
convPactIdCore :: PCore.DefPactId -> SQ3.Utf8
convPactIdCore pid = "PactId \"" <> toUtf8 (PCore.renderDefPactId pid) <> "\""

convSavepointName :: SavepointName -> SQ3.Utf8
convSavepointName = toTextUtf8

convHashedModuleName :: PCore.HashedModuleName -> SQ3.Utf8
convHashedModuleName = toUtf8 . PCore.renderHashedModuleName

-- -------------------------------------------------------------------------- --
--

withSavepoint
    :: SQLiteEnv
    -> SavepointName
    -> IO a
    -> IO a
withSavepoint db name action = mask $ \resetMask -> do
    beginSavepoint db name
    go resetMask `catches` handlers
  where
    go resetMask = do
        r <- resetMask action `onException` abortSavepoint db name
        liftIO $ commitSavepoint db name
        liftIO $ evaluate r
    throwErr s = internalError $ "withSavepoint (" <> toText name <> "): " <> s
    handlers = [ Handler $ \(e :: PactException) -> throwErr (sshow e)
               , Handler $ \(e :: SomeAsyncException) -> throwM e
               , Handler $ \(e :: SomeException) -> throwErr ("non-pact exception: " <> sshow e)
               ]

beginSavepoint :: SQLiteEnv -> SavepointName -> IO ()
beginSavepoint db name =
  exec_ db $ "SAVEPOINT [" <> convSavepointName name <> "];"

commitSavepoint :: SQLiteEnv -> SavepointName -> IO ()
commitSavepoint db name =
  exec_ db $ "RELEASE SAVEPOINT [" <> convSavepointName name <> "];"

-- | @rollbackSavepoint n@ rolls back all database updates since the most recent
-- savepoint with the name @n@ and restarts the transaction.
--
-- /NOTE/ that the savepoint is not removed from the savepoint stack. In order to
-- also remove the savepoint @rollbackSavepoint n >> commitSavepoint n@ can be
-- used to release the (empty) transaction.
--
-- Cf. <https://www.sqlite.org/lang_savepoint.html> for details about
-- savepoints.
--
rollbackSavepoint :: SQLiteEnv -> SavepointName -> IO ()
rollbackSavepoint db name =
  exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT [" <> convSavepointName name <> "];"

-- | @abortSavepoint n@ rolls back all database updates since the most recent
-- savepoint with the name @n@ and removes it from the savepoint stack.
abortSavepoint :: SQLiteEnv -> SavepointName -> IO ()
abortSavepoint db name = do
  rollbackSavepoint db name
  commitSavepoint db name

data SavepointName = BatchSavepoint | DbTransaction | PreBlock
  deriving (Eq, Ord, Enum, Bounded)

instance Show SavepointName where
    show = T.unpack . toText

instance HasTextRepresentation SavepointName where
    toText BatchSavepoint = "batch"
    toText DbTransaction = "db-transaction"
    toText PreBlock = "preblock"
    {-# INLINE toText #-}

    fromText "batch" = pure BatchSavepoint
    fromText "db-transaction" = pure DbTransaction
    fromText "preblock" = pure PreBlock
    fromText t = throwM $ TextFormatException
        $ "failed to decode SavepointName " <> t
        <> ". Valid names are " <> T.intercalate ", " (toText @SavepointName <$> [minBound .. maxBound])
    {-# INLINE fromText #-}

-- instance AsString SavepointName where
--   asString = toText

expectSingleRowCol :: Show a => String -> [[a]] -> IO a
expectSingleRowCol _ [[s]] = return s
expectSingleRowCol s v =
  internalError $
  "expectSingleRowCol: "
  <> asString s <>
  " expected single row and column result, got: "
  <> asString (show v)

expectSingle :: Show a => String -> [a] -> IO a
expectSingle _ [s] = return s
expectSingle desc v =
  internalError $
  "Expected single-" <> asString (show desc) <> " result, got: " <>
  asString (show v)

chainwebPragmas :: [Pragma]
chainwebPragmas =
  [ "synchronous = NORMAL"
  , "journal_mode = WAL"
  , "locking_mode = NORMAL"
      -- changed from locking_mode = EXCLUSIVE to allow backups to run concurrently
      -- with Pact service operation. the effect of this change is twofold:
      --   - now file locks are grabbed at the beginning of each transaction; with
      --     EXCLUSIVE, file locks are never let go until the entire connection closes.
      --     (see https://web.archive.org/web/20220222231602/https://sqlite.org/pragma.html#pragma_locking_mode)
      --   - now we can query the database while another connection is open,
      --     taking full advantage of WAL mode.
      --     (see https://web.archive.org/web/20220226212219/https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode)
  , "temp_store = MEMORY"
  , "auto_vacuum = NONE"
  , "page_size = 1024"
  ]

execMulti :: Traversable t => SQ3.Database -> SQ3.Utf8 -> t [SType] -> IO ()
execMulti db q rows = bracket (prepStmt db q) destroy $ \stmt -> do
    forM_ rows $ \row -> do
        SQ3.reset stmt >>= checkError
        SQ3.clearBindings stmt
        bindParams stmt row
        SQ3.step stmt >>= checkError
  where
    checkError (Left e) = void $ fail $ "error during batch insert: " ++ show e
    checkError (Right _) = return ()

    destroy x = void (SQ3.finalize x >>= checkError)

withSqliteDb
    :: Logger logger
    => ChainId
    -> logger
    -> FilePath
    -> Bool
    -> (SQLiteEnv -> IO a)
    -> IO a
withSqliteDb cid logger dbDir resetDb = bracket
    (startSqliteDb cid logger dbDir resetDb)
    stopSqliteDb

startSqliteDb
    :: Logger logger
    => ChainId
    -> logger
    -> FilePath
    -> Bool
    -> IO SQLiteEnv
startSqliteDb cid logger dbDir doResetDb = do
    when doResetDb resetDb
    createDirectoryIfMissing True dbDir
    logFunctionText logger Debug $ "opening sqlitedb named " <> T.pack sqliteFile
    openSQLiteConnection sqliteFile chainwebPragmas
  where
    resetDb = removeDirectoryRecursive dbDir
    sqliteFile = dbDir </> chainDbFileName cid

startReadSqliteDb
    :: Logger logger
    => ChainId
    -> logger
    -> FilePath
    -> IO SQLiteEnv
startReadSqliteDb cid logger dbDir = do
    logFunctionText logger Debug $ "(read-only) opening sqlitedb named " <> T.pack sqliteFile
    openSQLiteConnection sqliteFile chainwebPragmas
  where
    sqliteFile = dbDir </> chainDbFileName cid

chainDbFileName :: ChainId -> FilePath
chainDbFileName cid = fold
    [ "pact-v1-chain-"
    , T.unpack (chainIdToText cid)
    , ".sqlite"
    ]

stopSqliteDb :: SQLiteEnv -> IO ()
stopSqliteDb = closeSQLiteConnection

withSQLiteConnection :: String -> [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection file ps =
    bracket (openSQLiteConnection file ps) closeSQLiteConnection

openSQLiteConnection :: String -> [Pragma] -> IO SQLiteEnv
openSQLiteConnection file ps = open_v2
    (fromString file)
    (collapseFlags [sqlite_open_readwrite , sqlite_open_create , sqlite_open_nomutex])
    Nothing -- Nothing corresponds to the nullPtr
  >>= \case
    Left (err, msg) ->
      internalError $
      "withSQLiteConnection: Can't open db with "
      <> asString (show err) <> ": " <> asString (show msg)
    Right r -> do
      runPragmas r ps
      return r

openReadSQLiteConnection :: String -> [Pragma] -> IO SQLiteEnv
openReadSQLiteConnection file ps = open_v2
    (fromString file)
    (collapseFlags [sqlite_open_readonly , sqlite_open_create , sqlite_open_nomutex])
    Nothing -- Nothing corresponds to the nullPtr
  >>= \case
    Left (err, msg) ->
      internalError $
      "withSQLiteConnection: Can't open db with "
      <> asString (show err) <> ": " <> asString (show msg)
    Right r -> do
      runPragmas r ps
      return r

closeSQLiteConnection :: SQLiteEnv -> IO ()
closeSQLiteConnection c = void $ close_v2 c

-- passing the empty string as filename causes sqlite to use a temporary file
-- that is deleted when the connection is closed. In practice, unless the database becomes
-- very large, the database will reside memory and no data will be written to disk.
--
-- Cf. https://www.sqlite.org/inmemorydb.html
--
withTempSQLiteConnection :: [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withTempSQLiteConnection = withSQLiteConnection ""

-- Using the special file name @:memory:@ causes sqlite to create a temporary in-memory
-- database.
--
-- Cf. https://www.sqlite.org/inmemorydb.html
--
withInMemSQLiteConnection :: [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withInMemSQLiteConnection = withSQLiteConnection ":memory:"

collapseFlags :: [SQLiteFlag] -> SQLiteFlag
collapseFlags xs =
    if Prelude.null xs then error "collapseFlags: You must pass a non-empty list"
    else Prelude.foldl1 (.|.) xs

sqlite_open_readwrite, sqlite_open_create, sqlite_open_nomutex :: SQLiteFlag
sqlite_open_readwrite = 0x00000002
sqlite_open_readonly = 0x00000001
sqlite_open_create = 0x00000004
sqlite_open_nomutex = 0x00008000

markTableMutation :: Utf8 -> BlockHeight -> Database -> IO ()
markTableMutation tablename blockheight db = do
    exec' db mutq [SText tablename, SInt (fromIntegral blockheight)]
  where
    mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"

commitBlockStateToDatabase :: SQLiteEnv -> BlockHash -> BlockHeight -> BlockHandle -> IO ()
commitBlockStateToDatabase db hsh bh blockHandle = do
  let newTables = _pendingTableCreation $ _blockHandlePending blockHandle
  mapM_ (\tn -> createUserTable (Utf8 tn)) newTables
  let writeV = toChunks $ _pendingWrites (_blockHandlePending blockHandle)
  backendWriteUpdateBatch writeV
  indexPendingPactTransactions
  let nextTxId = _blockHandleTxId blockHandle
  blockHistoryInsert nextTxId
  where
    toChunks writes =
      over _2 (concatMap toList . HashMap.elems) .
      over _1 Utf8 <$> HashMap.toList writes

    backendWriteUpdateBatch
        :: [(Utf8, [SQLiteRowDelta])]
        -> IO ()
    backendWriteUpdateBatch writesByTable = mapM_ writeTable writesByTable
       where
         prepRow (SQLiteRowDelta _ txid rowkey rowdata) =
            [ SText (Utf8 rowkey)
            , SInt (fromIntegral txid)
            , SBlob rowdata
            ]

         writeTable (tableName, writes) = do
            execMulti db q (map prepRow writes)
            markTableMutation tableName bh db
           where
            q = "INSERT OR REPLACE INTO " <> tbl tableName <> "(rowkey,txid,rowdata) VALUES(?,?,?)"

    -- | Record a block as being in the history of the checkpointer
    blockHistoryInsert :: TxId -> IO ()
    blockHistoryInsert t =
        exec' db stmt
            [ SInt (fromIntegral bh)
            , SBlob (runPutS (encodeBlockHash hsh))
            , SInt (fromIntegral t)
            ]
      where
        stmt =
          "INSERT INTO BlockHistory ('blockheight','hash','endingtxid') VALUES (?,?,?);"

    createUserTable :: Utf8 -> IO ()
    createUserTable tablename = do
        createVersionedTable tablename db
        exec' db insertstmt insertargs
      where
        insertstmt = "INSERT OR IGNORE INTO VersionedTableCreation VALUES (?,?)"
        insertargs =  [SText tablename, SInt (fromIntegral bh)]

    -- | Commit the index of pending successful transactions to the database
    indexPendingPactTransactions :: IO ()
    indexPendingPactTransactions = do
        let txs = _pendingSuccessfulTxs $ _blockHandlePending blockHandle
        dbIndexTransactions txs

      where
        toRow b = [SBlob b, SInt (fromIntegral bh)]
        dbIndexTransactions txs = do
            let rows = map toRow $ toList txs
            execMulti db "INSERT INTO TransactionIndex (txhash, blockheight) \
                         \ VALUES (?, ?)" rows

tbl :: HasCallStack => Utf8 -> Utf8
tbl t@(Utf8 b)
    | B8.elem ']' b = error $ "Chainweb.Pact4.Backend.ChainwebPactDb: Code invariant violation. Illegal SQL table name " <> sshow b <> ". Please report this as a bug."
    | otherwise = "[" <> t <> "]"

createVersionedTable :: Utf8 -> Database -> IO ()
createVersionedTable tablename db = do
    exec_ db createtablestmt
    exec_ db indexcreationstmt
  where
    ixName = tablename <> "_ix"
    createtablestmt =
      "CREATE TABLE IF NOT EXISTS " <> tbl tablename <> " \
             \ (rowkey TEXT\
             \, txid UNSIGNED BIGINT NOT NULL\
             \, rowdata BLOB NOT NULL\
             \, UNIQUE (rowkey, txid));"
    indexcreationstmt =
        "CREATE INDEX IF NOT EXISTS " <> tbl ixName <> " ON " <> tbl tablename <> "(txid DESC);"


doLookupSuccessful :: Database -> BlockHeight -> V.Vector SB.ShortByteString -> IO (HashMap.HashMap SB.ShortByteString (T2 BlockHeight BlockHash))
doLookupSuccessful db curHeight hashes = do
  fmap buildResultMap $ do -- swizzle results of query into a HashMap
      let
        hss = V.toList hashes
        params = BS.intercalate "," (map (const "?") hss)
        qtext = Utf8 $ BS.intercalate " "
            [ "SELECT blockheight, hash, txhash"
            , "FROM TransactionIndex"
            , "INNER JOIN BlockHistory USING (blockheight)"
            , "WHERE txhash IN (" <> params <> ")" <> " AND blockheight <= ?;"
            ]
        qvals
          -- match query params above. first, hashes
          = map (\h -> SBlob $ SB.fromShort h) hss
          -- then, the block height; we don't want to see txs from the
          -- current block in the db, because they'd show up in pending data
          ++ [SInt $ fromIntegral (pred curHeight)]

      qry db qtext qvals [RInt, RBlob, RBlob] >>= mapM go
  where
    -- NOTE: it's useful to keep the types of 'go' and 'buildResultMap' in sync
    -- for readability but also to ensure the compiler and reader infer the
    -- right result types from the db query.

    buildResultMap :: [T3 SB.ShortByteString BlockHeight BlockHash] -> HashMap.HashMap SB.ShortByteString (T2 BlockHeight BlockHash)
    buildResultMap xs = HashMap.fromList $
      map (\(T3 txhash blockheight blockhash) -> (txhash, T2 blockheight blockhash)) xs

    go :: [SType] -> IO (T3 SB.ShortByteString BlockHeight BlockHash)
    go (SInt blockheight:SBlob blockhash:SBlob txhash:_) = do
        !blockhash' <- either fail return $ runGetEitherS decodeBlockHash blockhash
        let !txhash' = SB.toShort txhash
        return $! T3 txhash' (fromIntegral blockheight) blockhash'
    go _ = fail "impossible"

-- | Create all tables that exist pre-genesis
initSchema :: (Logger logger) => logger -> SQLiteEnv -> IO ()
initSchema logger sql =
    withSavepoint sql DbTransaction $ do
        createBlockHistoryTable
        createTableCreationTable
        createTableMutationTable
        createTransactionIndexTable
        create (domainTableName KeySets)
        create (domainTableName Modules)
        create (domainTableName Namespaces)
        create (domainTableName Pacts)
        -- TODO: migrate this logic to the checkpointer itself?
        create (toUtf8 $ Pact5.renderDomain Pact5.DModuleSource)
  where
    create tablename = do
      logDebug_ logger $ "initSchema: "  <> fromUtf8 tablename
      createVersionedTable tablename sql

    createBlockHistoryTable :: IO ()
    createBlockHistoryTable =
      exec_ sql
        "CREATE TABLE IF NOT EXISTS BlockHistory \
        \(blockheight UNSIGNED BIGINT NOT NULL,\
        \ hash BLOB NOT NULL,\
        \ endingtxid UNSIGNED BIGINT NOT NULL, \
        \ CONSTRAINT blockHashConstraint UNIQUE (blockheight));"

    createTableCreationTable :: IO ()
    createTableCreationTable =
      exec_ sql
        "CREATE TABLE IF NOT EXISTS VersionedTableCreation\
        \(tablename TEXT NOT NULL\
        \, createBlockheight UNSIGNED BIGINT NOT NULL\
        \, CONSTRAINT creation_unique UNIQUE(createBlockheight, tablename));"

    createTableMutationTable :: IO ()
    createTableMutationTable =
      exec_ sql
        "CREATE TABLE IF NOT EXISTS VersionedTableMutation\
         \(tablename TEXT NOT NULL\
         \, blockheight UNSIGNED BIGINT NOT NULL\
         \, CONSTRAINT mutation_unique UNIQUE(blockheight, tablename));"

    createTransactionIndexTable :: IO ()
    createTransactionIndexTable = do
      exec_ sql
        "CREATE TABLE IF NOT EXISTS TransactionIndex \
         \ (txhash BLOB NOT NULL, \
         \ blockheight UNSIGNED BIGINT NOT NULL, \
         \ CONSTRAINT transactionIndexConstraint UNIQUE(txhash));"
      exec_ sql
        "CREATE INDEX IF NOT EXISTS \
         \ transactionIndexByBH ON TransactionIndex(blockheight)";
