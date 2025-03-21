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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

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
    open2
  , chainDbFileName
    -- * Shared Pact database interactions
  , doLookupSuccessful
  , tbl
  , rewindDbTo
  , rewindDbToBlock
  , rewindDbToGenesis
  , getEndTxId
  , getEndTxId'
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
  , asStringUtf8
  , convSavepointName
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

import Control.Exception (evaluate)
import Control.Exception.Safe
import Control.Monad
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

import qualified Pact.Types.SQLite as Pact4
import Pact.Types.Util (AsString(..))


-- chainweb

import Chainweb.Logger
import Chainweb.Pact.Backend.SQLite.DirectV2

import Chainweb.Version
import Chainweb.Utils
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Parent
import Database.SQLite3.Direct hiding (open2)
import GHC.Stack
import qualified Data.ByteString.Short as SB
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HashMap
import Chainweb.Utils.Serialization
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import Chainweb.Pact.Backend.Types
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Pact.Core.Persistence as Pact
import Control.Monad.Catch (ExitCase(..))

-- -------------------------------------------------------------------------- --
-- SQ3.Utf8 Encodings

toUtf8 :: Text -> SQ3.Utf8
toUtf8 = SQ3.Utf8 . T.encodeUtf8
{-# INLINE toUtf8 #-}

fromUtf8 :: SQ3.Utf8 -> Text
fromUtf8 (SQ3.Utf8 bytes) = T.decodeUtf8 bytes
{-# INLINE fromUtf8 #-}

asStringUtf8 :: AsString a => a -> SQ3.Utf8
asStringUtf8 = toUtf8 . asString
{-# INLINE asStringUtf8 #-}


-- -------------------------------------------------------------------------- --
--

withSavepoint
    :: (MonadMask m, MonadIO m)
    => SQLiteEnv
    -> SavepointName
    -> m a
    -> m a
withSavepoint db name action = fmap fst $ generalBracket
    (liftIO $ beginSavepoint db name)
    (\_ -> liftIO . \case
        ExitCaseSuccess {} -> commitSavepoint db name
        _ -> abortSavepoint db name
    ) $ \_ -> action

beginSavepoint :: SQLiteEnv -> SavepointName -> IO ()
beginSavepoint db name =
  Pact4.exec_ db $ "SAVEPOINT [" <> convSavepointName name <> "];"

commitSavepoint :: SQLiteEnv -> SavepointName -> IO ()
commitSavepoint db name =
  Pact4.exec_ db $ "RELEASE SAVEPOINT [" <> convSavepointName name <> "];"

convSavepointName :: SavepointName -> SQ3.Utf8
convSavepointName = toUtf8 . toText

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
  Pact4.exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT [" <> convSavepointName name <> "];"

-- | @abortSavepoint n@ rolls back all database updates since the most recent
-- savepoint with the name @n@ and removes it from the savepoint stack.
abortSavepoint :: SQLiteEnv -> SavepointName -> IO ()
abortSavepoint db name = do
  rollbackSavepoint db name
  commitSavepoint db name

data SavepointName
    = ReadFromSavepoint
    | ReadFromNSavepoint
    | RestoreAndSaveSavePoint
    | RewindSavePoint
    | InitSchemaSavePoint
    | ValidateBlockSavePoint
    deriving (Eq, Ord, Enum, Bounded)

instance Show SavepointName where
    show = T.unpack . toText

instance HasTextRepresentation SavepointName where
    toText ReadFromSavepoint = "read-from"
    toText ReadFromNSavepoint = "read-from-n"
    toText RestoreAndSaveSavePoint = "restore-and-save"
    toText RewindSavePoint = "rewind"
    toText InitSchemaSavePoint = "init-schema"
    toText ValidateBlockSavePoint = "validate-block"
    {-# INLINE toText #-}

    fromText "read-from" = pure ReadFromSavepoint
    fromText "read-from-n" = pure ReadFromNSavepoint
    fromText "restore-and-save" = pure RestoreAndSaveSavePoint
    fromText "rewind" = pure RewindSavePoint
    fromText "init-schema" = pure InitSchemaSavePoint
    fromText "validate-block" = pure ValidateBlockSavePoint
    fromText t = throwM $ TextFormatException
        $ "failed to decode SavepointName " <> t
        <> ". Valid names are " <> T.intercalate ", " (toText @SavepointName <$> [minBound .. maxBound])
    {-# INLINE fromText #-}

chainwebPragmas :: [Pact4.Pragma]
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

withSQLiteConnection :: String -> [Pact4.Pragma] -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection file ps =
    bracket (openSQLiteConnection file ps) closeSQLiteConnection

openSQLiteConnection :: String -> [Pact4.Pragma] -> IO SQLiteEnv
openSQLiteConnection file ps = open2 file >>= \case
    Left (err, msg) ->
      error $
      "withSQLiteConnection: Can't open db with "
      <> show err <> ": " <> show msg
    Right r -> do
      Pact4.runPragmas r ps
      return r

closeSQLiteConnection :: SQLiteEnv -> IO ()
closeSQLiteConnection c = void $ close_v2 c

-- passing the empty string as filename causes sqlite to use a temporary file
-- that is deleted when the connection is closed. In practice, unless the database becomes
-- very large, the database will reside memory and no data will be written to disk.
--
-- Cf. https://www.sqlite.org/inmemorydb.html
--
withTempSQLiteConnection :: [Pact4.Pragma] -> (SQLiteEnv -> IO c) -> IO c
withTempSQLiteConnection = withSQLiteConnection ""

-- Using the special file name @:memory:@ causes sqlite to create a temporary in-memory
-- database.
--
-- Cf. https://www.sqlite.org/inmemorydb.html
--
withInMemSQLiteConnection :: [Pact4.Pragma] -> (SQLiteEnv -> IO c) -> IO c
withInMemSQLiteConnection = withSQLiteConnection ":memory:"

open2 :: String -> IO (Either (SQ3.Error, SQ3.Utf8) SQ3.Database)
open2 file = open_v2
    (fromString file)
    (collapseFlags [sqlite_open_readwrite , sqlite_open_create , sqlite_open_fullmutex])
    Nothing -- Nothing corresponds to the nullPtr

collapseFlags :: [SQLiteFlag] -> SQLiteFlag
collapseFlags xs =
    if Prelude.null xs then error "collapseFlags: You must pass a non-empty list"
    else Prelude.foldr1 (.|.) xs

sqlite_open_readwrite, sqlite_open_create, sqlite_open_fullmutex :: SQLiteFlag
sqlite_open_readwrite = 0x00000002
sqlite_open_create = 0x00000004
sqlite_open_fullmutex = 0x00010000

tbl :: HasCallStack => Utf8 -> Utf8
tbl t@(Utf8 b)
    | B8.elem ']' b = error $ "Chainweb.Pact.Backend.ChainwebPactDb: Code invariant violation. Illegal SQL table name " <> sshow b <> ". Please report this as a bug."
    | otherwise = "[" <> t <> "]"

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
            , "WHERE txhash IN (" <> params <> ")" <> " AND blockheight < ?;"
            ]
        qvals
          -- match query params above. first, hashes
          = map (\h -> Pact4.SBlob $ SB.fromShort h) hss
          -- then, the block height; we don't want to see txs from the
          -- current block in the db, because they'd show up in pending data
          ++ [Pact4.SInt $ fromIntegral curHeight]

      Pact4.qry db qtext qvals [Pact4.RInt, Pact4.RBlob, Pact4.RBlob] >>= mapM go
  where
    -- NOTE: it's useful to keep the types of 'go' and 'buildResultMap' in sync
    -- for readability but also to ensure the compiler and reader infer the
    -- right result types from the db query.

    buildResultMap :: [T3 SB.ShortByteString BlockHeight BlockHash] -> HashMap.HashMap SB.ShortByteString (T2 BlockHeight BlockHash)
    buildResultMap xs = HashMap.fromList $
      map (\(T3 txhash blockheight blockhash) -> (txhash, T2 blockheight blockhash)) xs

    go :: [Pact4.SType] -> IO (T3 SB.ShortByteString BlockHeight BlockHash)
    go (Pact4.SInt blockheight:Pact4.SBlob blockhash:Pact4.SBlob txhash:_) = do
        !blockhash' <- either fail return $ runGetEitherS decodeBlockHash blockhash
        let !txhash' = SB.toShort txhash
        return $! T3 txhash' (fromIntegral blockheight) blockhash'
    go _ = fail "impossible"

getEndTxId :: ChainwebVersion -> ChainId -> Text -> SQLiteEnv -> Parent RankedBlockHash -> IO (Historical Pact.TxId)
getEndTxId v cid msg sql pc
  | isGenesisBlockHeader' v cid (_rankedBlockHashHash <$> pc) =
    return (Historical (Pact.TxId 0))
  | otherwise =
    getEndTxId' msg sql pc

getEndTxId' :: Text -> SQLiteEnv -> Parent RankedBlockHash -> IO (Historical Pact.TxId)
getEndTxId' msg sql (Parent rbh) = do
    r <- Pact4.qry sql
      "SELECT endingtxid FROM BlockHistory WHERE blockheight = ? and hash = ?;"
      [ Pact4.SInt $ fromIntegral $ _rankedBlockHashHeight rbh
      , Pact4.SBlob $ runPutS (encodeBlockHash $ _rankedBlockHashHash rbh)
      ]
      [Pact4.RInt]
    case r of
      [[Pact4.SInt tid]] -> return $ Historical (Pact.TxId (fromIntegral tid))
      [] -> return NoHistory
      _ -> error $ T.unpack msg <> ".getEndTxId: expected single-row int result, got " <> sshow r


-- | Delete any state from the database newer than the input parent header.
-- Returns the ending txid of the input parent header.
rewindDbTo
    :: ChainwebVersion
    -> ChainId
    -> SQLiteEnv
    -> RankedBlockHash
    -> IO Pact.TxId
rewindDbTo v cid db pc
  | isGenesisBlockHeader' v cid (Parent $ _rankedBlockHashHash pc) = do
    rewindDbToGenesis db
    return (Pact.TxId 0)
  | otherwise = do
    !historicalEndingTxId <- getEndTxId v cid "rewindDbToBlock" db (Parent pc)
    endingTxId <- case historicalEndingTxId of
      NoHistory ->
        error
          $ "rewindDbTo.getEndTxId: not in db: "
          <> sshow pc
      Historical endingTxId ->
        return endingTxId
    rewindDbToBlock db (_rankedBlockHashHeight <$> Parent pc) endingTxId
    return endingTxId

-- rewind before genesis, delete all user tables and all rows in all tables
rewindDbToGenesis
  :: SQLiteEnv
  -> IO ()
rewindDbToGenesis db = do
    Pact4.exec_ db "DELETE FROM BlockHistory;"
    Pact4.exec_ db "DELETE FROM [SYS:KeySets];"
    Pact4.exec_ db "DELETE FROM [SYS:Modules];"
    Pact4.exec_ db "DELETE FROM [SYS:Namespaces];"
    Pact4.exec_ db "DELETE FROM [SYS:Pacts];"
    Pact4.exec_ db "DELETE FROM [SYS:ModuleSources];"
    tblNames <- Pact4.qry_ db "SELECT tablename FROM VersionedTableCreation;" [Pact4.RText]
    forM_ tblNames $ \t -> case t of
      [Pact4.SText tn] -> Pact4.exec_ db ("DROP TABLE [" <> tn <> "];")
      _ -> error "Something went wrong when resetting tables."
    Pact4.exec_ db "DELETE FROM VersionedTableCreation;"
    Pact4.exec_ db "DELETE FROM VersionedTableMutation;"
    Pact4.exec_ db "DELETE FROM TransactionIndex;"

-- | Rewind the database to a particular block, given the end tx id of that
-- block.
rewindDbToBlock
  :: Database
  -> Parent BlockHeight
  -> Pact.TxId
  -> IO ()
rewindDbToBlock db (Parent bh) endingTxId = do
    tableMaintenanceRowsVersionedSystemTables
    droppedtbls <- dropTablesAtRewind
    vacuumTablesAtRewind droppedtbls
    deleteHistory
    clearTxIndex
  where
    dropTablesAtRewind :: IO (HashSet BS.ByteString)
    dropTablesAtRewind = do
        toDropTblNames <- Pact4.qry db findTablesToDropStmt
                          [Pact4.SInt (fromIntegral bh)] [Pact4.RText]
        tbls <- fmap HashSet.fromList . forM toDropTblNames $ \case
            [Pact4.SText tblname@(Utf8 tn)] -> do
                Pact4.exec_ db $ "DROP TABLE IF EXISTS " <> tbl tblname
                return tn
            _ -> error rewindmsg
        Pact4.exec' db
            "DELETE FROM VersionedTableCreation WHERE createBlockheight > ?"
            [Pact4.SInt (fromIntegral bh)]
        return tbls
    findTablesToDropStmt =
      "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight > ?;"
    rewindmsg =
      "rewindBlock: dropTablesAtRewind: Couldn't resolve the name of the table to drop."

    deleteHistory :: IO ()
    deleteHistory =
        Pact4.exec' db "DELETE FROM BlockHistory WHERE blockheight > ?"
              [Pact4.SInt (fromIntegral bh)]

    vacuumTablesAtRewind :: HashSet BS.ByteString -> IO ()
    vacuumTablesAtRewind droppedtbls = do
        let processMutatedTables ms = fmap HashSet.fromList . forM ms $ \case
              [Pact4.SText (Utf8 tn)] -> return tn
              _ -> error
                "rewindBlock: vacuumTablesAtRewind: Couldn't resolve the name \
                \of the table to possibly vacuum."
        mutatedTables <- Pact4.qry db
            "SELECT DISTINCT tablename FROM VersionedTableMutation WHERE blockheight > ?;"
          [Pact4.SInt (fromIntegral bh)]
          [Pact4.RText]
          >>= processMutatedTables
        let toVacuumTblNames = HashSet.difference mutatedTables droppedtbls
        forM_ toVacuumTblNames $ \tblname ->
            Pact4.exec' db ("DELETE FROM " <> tbl (Utf8 tblname) <> " WHERE txid >= ?")
                  [Pact4.SInt $! fromIntegral $ Pact._txId endingTxId]
        Pact4.exec' db "DELETE FROM VersionedTableMutation WHERE blockheight > ?;"
              [Pact4.SInt (fromIntegral bh)]

    tableMaintenanceRowsVersionedSystemTables :: IO ()
    tableMaintenanceRowsVersionedSystemTables = do
        Pact4.exec' db "DELETE FROM [SYS:KeySets] WHERE txid >= ?" tx
        Pact4.exec' db "DELETE FROM [SYS:Modules] WHERE txid >= ?" tx
        Pact4.exec' db "DELETE FROM [SYS:Namespaces] WHERE txid >= ?" tx
        Pact4.exec' db "DELETE FROM [SYS:Pacts] WHERE txid >= ?" tx
        Pact4.exec' db "DELETE FROM [SYS:ModuleSources] WHERE txid >= ?" tx
      where
        tx = [Pact4.SInt $! fromIntegral $ Pact._txId endingTxId]

    -- | Delete all future transactions from the index
    clearTxIndex :: IO ()
    clearTxIndex =
        Pact4.exec' db "DELETE FROM TransactionIndex WHERE blockheight > ?;"
              [ Pact4.SInt (fromIntegral bh) ]
