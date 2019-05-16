{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module: Chainweb.Pact.ChainwebPactDb
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb
  ( chainwebpactdb
  , detectVersionChange
  , versionUpdate
  , blockHistoryInsert
  , versionHistoryInsert
  , versionedTablesInsert
  , withSQLiteConnection
  , withTempSQLiteConnection
  , beginSavepoint
  , commitSavepoint
  , rollbackSavepoint
  , initSchema
  , runBlockEnv
  , callDb
  , withPreBlockSavepoint
  , domainTableName
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Exception.Safe hiding (bracket)
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Maybe
import Data.Serialize (encode)
import Data.String
import Data.String.Conv
import qualified Data.Text as T

import Database.SQLite3.Direct as SQ3

import Prelude hiding (log)

import System.Directory (removeFile)
import System.IO.Extra

-- pact

import Pact.Persist hiding (beginTx, commitTx, rollbackTx, createTable)
import Pact.Persist.SQLite
import Pact.Types.Persistence
import Pact.Types.Pretty (prettyString, viaShow)
import Pact.Types.Runtime (throwDbError)
import Pact.Types.SQLite
import Pact.Types.Term(KeySetName(..), NamespaceName(..), TableName(..), ModuleName(..), PactId(..))
import Pact.Types.Util (AsString(..))

-- chainweb

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

newtype SavepointName = SavepointName BS.ByteString
  deriving (Eq, Ord, IsString)
  deriving newtype Show

chainwebpactdb :: PactDb (BlockEnv SQLiteEnv)
chainwebpactdb = PactDb
  {
    _readRow = readRow
  , _writeRow = writeRow
  , _keys = keys
  , _txids = txids
  , _createUserTable = createUserTable
  , _getUserTableInfo = error "WILL BE DEPRECATED!"
  , _beginTx = beginTx
  , _commitTx = commitTx
  , _rollbackTx = rollbackTx
  , _getTxLog = getTxLog
  }

-- MOVE THESE LATER --

domainTableName :: Domain k v -> Utf8
domainTableName = Utf8 . toS . T.filter accept . asString
  where
    accept = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

convKeySetName :: KeySetName -> Utf8
convKeySetName (KeySetName name) = Utf8 $ toS name

convModuleName :: ModuleName -> Utf8
convModuleName (ModuleName name _) = Utf8 $ toS name

convNamespaceName :: NamespaceName -> Utf8
convNamespaceName (NamespaceName name) = Utf8 $ toS name

convRowKey :: RowKey -> Utf8
convRowKey (RowKey name) = Utf8 $ toS name

convPactId :: PactId -> Utf8
convPactId = Utf8 . toS . show

-- MOVE THESE LATER --

callDb :: (MonadReader SQLiteEnv m, MonadIO m) => (Database -> IO b) -> m b
callDb action = do
  c <- view sConn
  liftIO $ action c

readRow :: (IsString k, FromJSON v) => Domain k v -> k -> Method (BlockEnv SQLiteEnv) (Maybe v)
readRow d k e = runBlockEnv e $
    case d of
      KeySets -> callDbWithKey (convKeySetName k)
      -- TODO: This is incomplete (the modules case), due to namespace
      -- resolution concerns
      Modules -> callDbWithKey (convModuleName k)
      Namespaces -> callDbWithKey (convNamespaceName k)
      (UserTables _) -> callDbWithKey (convRowKey k)
      Pacts -> callDbWithKey (convPactId k)
 where
   callDbWithKey :: FromJSON v => Utf8 -> VersionHandler SQLiteEnv (Maybe v)
   callDbWithKey kstr = do
     BlockVersion (BlockHeight bh) (ReorgVersion version)  <- gets _bsBlockVersion
     result <- callDb (\db -> qry db
                        "SELECT tabledata FROM ? WHERE rowkey = ? AND blockheight = ? AND version = ?"
                        [SText kstr, SInt (fromIntegral bh), SInt (fromIntegral version)] [RBlob])
     case result of
         [] -> return Nothing
         [[SBlob a]] -> return $ decode $ fromStrict a
         err -> throwM $ userError $ "readRow: Expected (at most) a single result, but got: " <> show err

runBlockEnv :: MVar (BlockEnv SQLiteEnv) -> VersionHandler SQLiteEnv a -> IO a
runBlockEnv e m = modifyMVar e $
  \(BlockEnv  db bs)  -> do
    (a,s) <- runStateT (runReaderT (runVersionHandler m) db) bs
    return (BlockEnv db s, a)

writeRow :: ToJSON v => WriteType -> Domain k v -> k -> v -> Method (BlockEnv SQLiteEnv) ()
writeRow wt d k v e =
  runBlockEnv e $ do
    (BlockVersion bh version) <- gets _bsBlockVersion
    txid <- gets _bsTxId
    case d of
      KeySets -> callDb (write (convKeySetName k) bh version txid)
      Modules -> callDb (write (convModuleName k) bh version txid)
      Namespaces -> callDb (write (convNamespaceName k) bh version txid)
      (UserTables _) -> callDb (write (convRowKey k) bh version txid)
      Pacts -> callDb (write (convPactId k) bh version txid)
  where
    write :: Utf8 -> BlockHeight -> ReorgVersion -> TxId -> Database -> IO ()
    write key@(Utf8 kk) bh@(BlockHeight innerbh) (ReorgVersion version) (TxId txid) db =
      let row = [SText key, SInt (fromIntegral bh), SInt (fromIntegral version), SInt (fromIntegral txid), SBlob (toStrict (Data.Aeson.encode v))]
      in case wt of
        Insert -> do
          res <- qry db ("SELECT rowkey FROM " <> domainTableName d <> " WHERE rowkey = ? AND blockheight = ? AND version = ? ;")
            [SText key, SInt (fromIntegral innerbh), SInt (fromIntegral version)]
            [RText]
          case res of
            [] -> exec' db
                  ("INSERT INTO " <> domainTableName d <> " ('rowkey','blockheight','version','txid','tabledata') VALUES (?,?,?,?,?);")
                   row
            _ -> throwM $ userError $ "writeRow: key " <>  toS kk <> " was found in table."
        Update -> do
          res <- qry db
                 ("SELECT rowkey FROM " <> domainTableName d <> " WHERE rowkey = ? AND blockheight = ? AND version = ?;")
                 [SText key, SInt (fromIntegral innerbh), SInt (fromIntegral version)]
                 [RText]
          case res of
            [] -> throwM $ userError $ "writeRow: key " <>  toS kk <> " was not found in table."
            _ -> exec' db
                 ("UPDATE " <> domainTableName d <> " SET rowkey = ?, blockheight = ?, version =  ?, txid = ?, tabledata = ?")
                 row
        Write -> do
          res <- qry db
                 ("SELECT rowkey FROM " <> domainTableName d <> " WHERE rowkey = ? AND blockheight = ? AND version = ?")
                 [SText key, SInt (fromIntegral innerbh), SInt (fromIntegral version)]
                 [RText]
          case res of
            [] -> exec' db
                  ("INSERT INTO " <> domainTableName d <> " ('rowkey','blockheight','version','txid','tabledata') VALUES (?,?,?,?,?);")
                  row
            _ -> exec' db
                 ("UPDATE " <> domainTableName d <> " SET rowkey = ?, blockheight = ?, version =  ?, txid = ?, tabledata = ?")
                 row

keys :: Domain k v -> Method (BlockEnv SQLiteEnv) [k]
keys = undefined

txids :: TableName -> TxId -> Method (BlockEnv SQLiteEnv) [TxId]
txids = undefined

createUserTable :: TableName -> ModuleName -> Method (BlockEnv SQLiteEnv) ()
createUserTable = undefined

-- getUserTableInfo :: TableName -> Method e ModuleName
-- getUserTableInfo = undefined

beginTx :: ExecutionMode -> Method (BlockEnv SQLiteEnv) (Maybe TxId)
beginTx = undefined

commitTx :: Method (BlockEnv SQLiteEnv) [TxLog Value]
commitTx = undefined

rollbackTx :: Method (BlockEnv SQLiteEnv) ()
rollbackTx = undefined

getTxLog :: Domain k v -> TxId -> Method (BlockEnv SQLiteEnv) [TxLog v]
getTxLog = undefined

withSQLiteConnection ::
     BlockHeight -> ReorgVersion -> String -> [Pragma] -> Bool -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection _bh _version file ps todelete action = do
  result <- bracket opener (close . _sConn) action
  when todelete (removeFile file)
  return result
  where
    opener = do
      e <- open $ fromString file
      case e of
        Left (err,msg) ->
          throwM $ userError $ "Can't open db with "  <> show err <> ": " <> show msg
        Right r -> return $ mkSQLiteEnv r
    mkSQLiteEnv connection =
      SQLiteEnv connection
      (SQLiteConfig file ps)

withTempSQLiteConnection :: [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withTempSQLiteConnection ps action =
  withTempFile (\file -> withSQLiteConnection 0 0 file ps False action)

blockHistoryInsert :: BlockHeight -> BlockHash -> VersionHandler SQLiteEnv ()
blockHistoryInsert bh hsh = do
  let s = "INSERT INTO BlockHistory ('blockheight','hash') VALUES (?,?);"
  callDb $ \db ->
    exec' db s [SInt (fromIntegral bh), SBlob (Data.Serialize.encode hsh)]

versionHistoryInsert :: BlockVersion -> VersionHandler SQLiteEnv ()
versionHistoryInsert (BlockVersion bh version) = do
  let s = "INSERT INTO VersionHistory ('version','blockheight') VALUES (?,?);"
  callDb $ \db ->
    exec' db s [SInt (_getReorgVersion version), SInt (fromIntegral bh)]

versionedTablesInsert :: TableName -> BlockVersion -> VersionHandler SQLiteEnv ()
versionedTablesInsert (TableName name) (BlockVersion (BlockHeight bh) (ReorgVersion bv)) = do
      let s = "INSERT INTO VersionedTables ('tablename','blockheight','version') VALUES (?,?,?);"
          theTableName = Utf8 (toS name)
      callDb $ \db -> exec' db s
        [ SText theTableName
        , SInt (fromIntegral bh)
        , SInt (fromIntegral bv)
        ]

createBlockHistoryTable :: VersionHandler SQLiteEnv ()
createBlockHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE BlockHistory\
    \(blockheight UNSIGNED BIGINT PRIMARY KEY,\
    \hash BLOB);"

createVersionHistoryTable  :: VersionHandler SQLiteEnv ()
createVersionHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionHistory\
    \ (version UNSIGNED BIGINT\
    \,blockheight UNSIGNED BIGINT\
    \,CONSTRAINT versionConstraint UNIQUE (version, blockheight));"

createVersionedTablesTable  :: VersionHandler SQLiteEnv ()
createVersionedTablesTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionedTables (tablename TEXT PRIMARY KEY\
  \ , blockheight UNSIGNED BIGINT\
  \ , version UNSIGNED BIGINT);"

createVersionedTable :: TableName -> BlockVersion -> VersionHandler SQLiteEnv ()
createVersionedTable name@(TableName _) b = do
  callDb $ \db -> exec_ db $
                  "CREATE TABLE "
                   <> Utf8 (toS (asString name))
                   <> " (rowkey TEXT,blockheight UNSIGNED BIGINT, version UNSIGNED BIGINT,txid UNSIGNED BIGINT,tabledata BLOB)"
  versionedTablesInsert name b

expectSingleRowCol :: Show a => String -> [[a]] -> IO a
expectSingleRowCol _ [[s]] = return s
expectSingleRowCol s v = throwDbError $ prettyString s <> " expected single row and column result, got: " <> viaShow v

detectVersionChange :: BlockHeight -> BlockHash -> VersionHandler SQLiteEnv VersionUpdate
detectVersionChange bRestore hsh  = do
  bCurrent <- do
    r <- callDb $ \ db -> qry_ db "SELECT max(blockheight) AS current_block_height FROM BlockHistory;" [RInt]
    SInt bh <- liftIO $ expectSingleRowCol "detectVersionChange (block):" r
    return $ BlockHeight (fromIntegral bh)
  vCurrent <- do
    r <- callDb $ \db -> qry_ db "SELECT max(version) AS current_version FROM VersionHistory;" [RInt]
    SInt version <- liftIO $ expectSingleRowCol "detectVersionChange (version):" r
    return $ ReorgVersion version

  -- REWRITE ERROR WITH INTERNALERROR PREFIX
  -- enforce invariant that the history has (B_restore-1,H_parent).
  historyInvariant <- callDb $ \db -> do
        res <- qry db "SELECT COUNT(*)\
           \ FROM BlockHistory\
           \ WHERE blockheight = ?\
           \ AND hash = ?;"
            [SInt $ fromIntegral $ pred bRestore, SBlob (Data.Serialize.encode hsh)] [RInt] >>= expectSingleRowCol "detectVersionChange (historyInvariant):"
        return res

  if historyInvariant /= SInt 1
    then throwDbError "History invariant violation"
  -- enforce invariant that B_restore is not greater than B_current + 1
    else case compare bRestore (bCurrent + 1) of
        GT -> throwDbError "Block_Restore invariant violation!"
        EQ -> return $ SameVersion (BlockVersion bRestore vCurrent)
        LT -> return $ VersionChange (BlockVersion bRestore (vCurrent + 1))

data VersionUpdate
  = VersionChange !BlockVersion
  | SameVersion !BlockVersion
  deriving Show

versionUpdate :: VersionUpdate -> VersionHandler SQLiteEnv ()
versionUpdate (SameVersion bv) = assign bsBlockVersion bv
versionUpdate (VersionChange bvEnv@(BlockVersion bRestore vNext)) = do
  assign bsBlockVersion (BlockVersion bRestore vNext)
  tableMaintenanceRowsVersionedTables bvEnv
  dropVersionedTables bvEnv
  deleteHistory bvEnv

tableMaintenanceRowsVersionedTables :: BlockVersion -> VersionHandler SQLiteEnv ()
tableMaintenanceRowsVersionedTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db ->
    qry db "SELECT tablename FROM VersionedTables WHERE blockheight < ?;" [SInt (fromIntegral bh)] [RText]
  forM_ tblNames $ \case
      [SText tbl] -> do
        callDb $ \db ->
          exec' db
          ("DELETE FROM " <> sanitize tbl <> " WHERE blockheight >= ?")
          [SInt (fromIntegral bh)]
      _ -> throwDbError "An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

dropVersionedTables :: BlockVersion -> VersionHandler SQLiteEnv ()
dropVersionedTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db -> qry db
              "SELECT tablename FROM VersionedTables\
              \ WHERE blockheight >=?"
              [SInt (fromIntegral bh)]
              [RText]
  -- REWRITE ERROR WITH INTERNALERROR PREFIX
  forM_ tblNames $ \case
      [SText name] -> do
        callDb $ \db -> exec_ db $ "DROP TABLE " <> sanitize name
      _ -> throwDbError "An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

deleteHistory :: BlockVersion -> VersionHandler SQLiteEnv ()
deleteHistory (BlockVersion bh _) = do
  callDb $ \db -> exec' db "DELETE FROM BlockHistory\
                           \ WHERE BlockHistory.blockheight >= ?"
                  [SInt (fromIntegral bh)]

beginSavepoint :: SavepointName ->  VersionHandler SQLiteEnv ()
beginSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "SAVEPOINT " <>  sanitize (Utf8 name)

commitSavepoint :: SavepointName -> VersionHandler SQLiteEnv ()
commitSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "RELEASE SAVEPOINT " <> sanitize (Utf8 name)

rollbackSavepoint :: SavepointName -> VersionHandler SQLiteEnv ()
rollbackSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT " <> sanitize (Utf8 name)

withPreBlockSavepoint :: VersionHandler SQLiteEnv a -> VersionHandler SQLiteEnv a
withPreBlockSavepoint action = do
  beginSavepoint "PREBLOCK"
  result <- tryAny action
  case result of
    Left (SomeException err) -> do
      rollbackSavepoint "PREBLOCK"
      throwM err
    Right r -> do
      commitSavepoint "PREBLOCK"
      return r

initSchema :: VersionHandler SQLiteEnv ()
initSchema = do
  let initBlock = BlockVersion (BlockHeight 0) (ReorgVersion 0)
      toTableName :: Domain k v -> TableName
      toTableName = TableName . T.filter accept . asString
        where
          accept = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
      bh00 = BlockHeight 0
      hash00 = nullBlockHash
  createBlockHistoryTable
  createVersionHistoryTable
  createVersionedTablesTable
  {- initSchema records the FIRST BLOCK -}
  blockHistoryInsert bh00 hash00
  versionHistoryInsert initBlock
  createVersionedTable (toTableName KeySets) initBlock
  createVersionedTable (toTableName Modules) initBlock
  createVersionedTable (toTableName Namespaces) initBlock
  createVersionedTable (toTableName Pacts) initBlock
  -- createVersionedTable "SYS:txid" initBlock -- I think this is an error.

{-- ensure that restore on a new block cannot cause a version change --}

sanitize :: Utf8 -> Utf8
sanitize (Utf8 string) = Utf8 $ BS.filter accept string
  where
    accept = flip BS.elem ("abcdefghijklmnopqrstuvwxyz" <> "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <> "0123456789")
