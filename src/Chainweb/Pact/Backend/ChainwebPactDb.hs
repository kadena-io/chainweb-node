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
  , handleVersion
  , blockHistoryInsert
  , versionHistoryInsert
  , versionedTablesInsert
  , initSchema
  , doCreateUserTable
  ) where

import Control.Exception hiding (try)
import Control.Lens
import Control.Exception.Safe hiding (bracket)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson hiding ((.=))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Serialize (encode)
import Data.String
import Data.String.Conv
import qualified Data.Text as T

import Database.SQLite3.Direct as SQ3

import Prelude hiding (log)

-- pact

import Pact.Persist
import Pact.PersistPactDb hiding (db)
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.SQLite
import Pact.Types.Term(TableName(..), ModuleName(..), ObjectMap(..))
import Pact.Types.Util (AsString(..))
import Pact.Types.Logger

-- chainweb

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types (internalError)

chainwebpactdb :: PactDb (BlockEnv SQLiteEnv)
chainwebpactdb = PactDb
  {
    _readRow = \d k e -> runBlockEnv e $ readRow d k
  , _writeRow = \wt d k v e -> runBlockEnv e $ writeRow wt d k v
  , _keys = \d e -> runBlockEnv e $ doKeys d
  , _txids = \t txid e -> runBlockEnv e $ doTxIds t txid
  , _createUserTable = \tn mn e -> runBlockEnv e $ doCreateUserTable tn mn
  , _getUserTableInfo = error "WILL BE DEPRECATED!"
  , _beginTx = \m e -> runBlockEnv e $ doBegin m
  , _commitTx = \e -> runBlockEnv e doCommit
  , _rollbackTx = \e -> runBlockEnv e doRollback
  , _getTxLog = \d tid e -> runBlockEnv e $ doGetTxLog d tid
  }

readRow :: (IsString k, FromJSON v) => Domain k v -> k -> BlockHandler SQLiteEnv (Maybe v)
readRow d k =
    case d of
      KeySets -> callDbWithKey (convKeySetName k)
      -- TODO: This is incomplete (the modules case), due to namespace
      -- resolution concerns
      Modules -> callDbWithKey (convModuleName k)
      Namespaces -> callDbWithKey (convNamespaceName k)
      (UserTables _) -> callDbWithKey (convRowKey k)
      Pacts -> callDbWithKey (convPactId k)
 where
   callDbWithKey :: FromJSON v => Utf8 -> BlockHandler SQLiteEnv (Maybe v)
   callDbWithKey kstr = do
     result <- callDb "readRow" $ \db -> do
                let stmt =
                      "SELECT rowdata \
                      \FROM [" <> domainTableName d <> "]\
                      \ WHERE rowkey = ?\
                      \ ORDER BY blockheight DESC, version DESC\
                      \ LIMIT 1"
                qry db stmt [SText kstr] [RBlob]
     case result of
         [] -> return Nothing
         [[SBlob a]] -> return $ decode $ fromStrict a
         err -> internalError $ "readRow: Expected (at most) a single result, but got: " <> T.pack (show err)

writeSys :: (AsString k, ToJSON v) => WriteType -> Domain k v -> k -> v -> BlockHandler SQLiteEnv ()
writeSys wt d k v = do
        BlockVersion bh version <- gets _bsBlockVersion
        txid <- gets _bsTxId
        callDb "writeSys" (write (getKeyString k) bh version txid)
        record (toTableName $ domainTableName d) d k v
  where
    toTableName (Utf8 str) = TableName $ toS str
    getKeyString key = case d of
      KeySets -> convKeySetName key
      Modules -> convModuleName key
      Namespaces -> convNamespaceName key
      Pacts -> convPactId key
      UserTables _ -> error "impossible"
    write key bh version txid db = case wt of
        Insert -> backendWriteInsert key (domainTableName d) bh version txid v db
        Update -> backendWriteUpdate key (domainTableName d) bh version txid v db
        Write -> backendWriteWrite key (domainTableName d) bh version txid v db

backendWriteInsert :: ToJSON v => Utf8 -> Utf8 -> BlockHeight -> ReorgVersion -> TxId -> v -> Database -> IO ()
backendWriteInsert key tn bh version txid v db =
  exec' db ("INSERT INTO [" <> tn <> "] ('rowkey','blockheight','version','txid','rowdata') VALUES (?,?,?,?,?);")
    [SText key
    , SInt (fromIntegral bh)
    , SInt (fromIntegral version)
    , SInt (fromIntegral txid)
    , SBlob (toStrict (Data.Aeson.encode v))]

backendWriteUpdate :: ToJSON v => Utf8 -> Utf8 -> BlockHeight -> ReorgVersion -> TxId -> v -> Database -> IO ()
backendWriteUpdate key tn bh version txid v db =
  exec' db ("UPDATE [" <> tn <> "] SET rowdata = ? WHERE rowkey = ? AND blockheight = ? AND version =  ? AND txid = ? ;")
    [SBlob (toStrict (Data.Aeson.encode v))
    , SText key
    , SInt (fromIntegral bh)
    , SInt (fromIntegral version)
    , SInt (fromIntegral txid)
    ]

backendWriteWrite :: ToJSON v => Utf8 -> Utf8 -> BlockHeight -> ReorgVersion -> TxId -> v -> Database -> IO ()
backendWriteWrite key tn bh version txid v db =
  exec' db ("INSERT OR REPLACE INTO [" <> tn <> "] ('rowkey','blockheight','version','txid','rowdata') VALUES (?,?,?,?,?) ;")
    [SText key, SInt (fromIntegral bh), SInt (fromIntegral version), SInt (fromIntegral txid), SBlob (toStrict (Data.Aeson.encode v))]

writeUser :: WriteType -> Domain RowKey (ObjectMap PactValue) -> RowKey -> ObjectMap PactValue -> BlockHandler SQLiteEnv ()
writeUser wt d k row = do
  BlockVersion bh version <- gets _bsBlockVersion
  txid <- gets _bsTxId
  userWrite k bh version txid
  where
    toTableName (Utf8 str) = TableName $ toS str
    userWrite key bh version txid = do
      olds <- readRow d key
      case (olds, wt) of
        (Nothing, Insert) -> ins
        (Just _, Insert) -> internalError $ "writeUser: Insert: row not found for key " <> asString key
        (Nothing, Write) -> ins
        (Just old, Write) -> upd old
        (Just old, Update) -> upd old
        (Nothing, Update) -> internalError $ "writeUser: Update: no row found for key " <> asString key
      where
        upd oldrow = do
          let row' = ObjectMap (M.union (_objectMap row) (_objectMap oldrow))
              tn = domainTableName d
          callDb "writeUser" $ backendWriteUpdate (Utf8 $ toS $ asString $ key) tn bh version txid row'
          record (toTableName tn) d key row'
        ins = do
          let tn = domainTableName d
          callDb "writeUser" $ backendWriteInsert (Utf8 $ toS $ asString $ key) tn bh version txid row
          record (toTableName tn) d key row

writeRow :: (AsString k, ToJSON v) => WriteType -> Domain k v -> k -> v -> BlockHandler SQLiteEnv ()
writeRow wt d k v = do
    case d of
      (UserTables _) -> writeUser wt d k v
      _ -> writeSys wt d k v

doKeys :: (IsString k, AsString k) => Domain k v -> BlockHandler SQLiteEnv [k]
doKeys d = do
  let tn = domainTableName d
  (ReorgVersion version) <- gets (_bvVersion . _bsBlockVersion)
  ks <- callDb "doKeys" $ \db -> qry db
            ("SELECT rowkey FROM [" <> tn <> "] WHERE version = ?;")
            [SInt (fromIntegral version)]
            [RText]
  forM ks $ \row -> do
    case row of
      [SText (Utf8 k)] -> return $ fromString $ toS k
      _ -> internalError "doKeys: The impossible happpened."
{-# INLINE doKeys #-}

doTxIds :: TableName -> TxId -> BlockHandler SQLiteEnv [TxId]
doTxIds (TableName tn) (TxId tid) = do
  (ReorgVersion version) <- gets (_bvVersion . _bsBlockVersion)
  rows <- callDb "doTxIds" $ \db ->
    qry db ("SELECT txid FROM [" <> Utf8 (toS tn) <> "] WHERE version = ? AND txid > ?")
    [SInt (fromIntegral version), SInt (fromIntegral tid)]
    [RInt]
  forM rows $ \row -> do
    case row of
      [SInt tid'] -> return $ TxId (fromIntegral tid')
      _ -> internalError "doTxIds: the impossible happened"
{-# INLINE doTxIds #-}

record :: (AsString k, ToJSON v) => TableName -> Domain k v -> k -> v -> BlockHandler SQLiteEnv ()
record tt d k v = bsTxRecord %= M.insertWith (flip (++)) tt [TxLog (asString d) (asString k) (toJSON v)]

doCreateUserTable :: TableName -> ModuleName -> BlockHandler SQLiteEnv ()
doCreateUserTable tn@(TableName _) mn = do
  bs <- gets _bsBlockVersion
  createUserTable tn bs
  let uti = UserTableInfo mn
  {- until I find a better way to do this -}
  bsTxRecord %= M.insertWith (flip (++)) "SYS:usertables" [TxLog "SYS:usertables" (asString tn) (toJSON uti)]
{-# INLINE doCreateUserTable #-}

doRollback :: BlockHandler SQLiteEnv ()
doRollback = do
  tryAny (rollbackSavepoint DbTransaction) >>= \case
    Left (SomeException err) -> logError $ "doRollback: " ++ show err
    Right _ -> return ()
  resetTemp

doBegin :: ExecutionMode -> BlockHandler SQLiteEnv (Maybe TxId)
doBegin m = do
  use bsMode >>= \m' -> case m' of
    Just {} -> do
      logError "beginTx: In transaction, rolling back"
      doRollback
    Nothing -> return ()
  resetTemp
  beginSavepoint DbTransaction
  bsMode .= Just m
  case m of
    Transactional -> Just <$> use bsTxId
    Local -> pure Nothing
{-# INLINE doBegin #-}

resetTemp :: BlockHandler SQLiteEnv ()
resetTemp = bsMode  .= Nothing >> bsTxRecord .= M.empty
-- WE SHOULD FLUSH THE IN-BLOCK CACHE HERE

doCommit :: BlockHandler SQLiteEnv [TxLog Value]
doCommit = use bsMode >>= \mm -> case mm  of
  Nothing -> doRollback >> internalError "doCommit: Not in transaction"
  Just m -> do
    txrs <- use bsTxRecord
    if m == Transactional then do
      bsTxId += 1
      -- commit
      commitSavepoint DbTransaction
      resetTemp
    else doRollback
    return $ fromdlist $ foldr (\a b -> todlist a . b) id txrs
{-# INLINE doCommit #-}

fromdlist :: ([a] -> [a]) -> [a]
fromdlist dl = dl []

todlist :: [a] -> [a] -> [a]
todlist xs = foldr go id xs
  where
    go a b = (a :) . b

doGetTxLog :: FromJSON v => Domain k v -> TxId -> BlockHandler SQLiteEnv [TxLog v]
doGetTxLog d txid = do
  BlockVersion (BlockHeight bh) (ReorgVersion version) <- gets _bsBlockVersion
  rows <- callDb "doGetTxLog" $ \db -> qry db
        ("SELECT rowkey, rowdata FROM [" <> domainTableName d <> "] WHERE txid = ? AND version = ? AND blockheight = ?")
        [SInt (fromIntegral txid), SInt (fromIntegral bh), SInt (fromIntegral version)]
        [RText, RBlob]
  forM rows $ \case
    [SText key, SBlob value] ->
      case Data.Aeson.decode $ fromStrict value of
        Nothing -> internalError $ "doGetTxLog: Unexpected value, unable to deserialize log"
        Just v -> return $ TxLog (toS $ unwrap $ domainTableName d) (toS $ unwrap $ key) v
    err -> internalError $ "doGetTxLog: Expected single row with two columns as the result, got: " <> T.pack (show err)

unwrap :: Utf8 -> BS.ByteString
unwrap (Utf8 str) = str

blockHistoryInsert :: BlockHeight -> BlockHash -> BlockHandler SQLiteEnv ()
blockHistoryInsert bh hsh = do
  let s = "INSERT INTO BlockHistory ('blockheight','hash') VALUES (?,?);"
  callDb "blockHistoryInsert" $ \db ->
    exec' db s [SInt (fromIntegral bh), SBlob (Data.Serialize.encode hsh)]

versionHistoryInsert :: BlockVersion -> BlockHandler SQLiteEnv ()
versionHistoryInsert (BlockVersion bh version) = do
  let s = "INSERT INTO VersionHistory ('version','blockheight') VALUES (?,?);"
  callDb "versionHistoryInsert" $ \db ->
    exec' db s [SInt (fromIntegral version), SInt (fromIntegral bh)]

versionedTablesInsert :: TableName -> BlockVersion -> BlockHandler SQLiteEnv ()
versionedTablesInsert (TableName name) (BlockVersion bh version) = do
      let s = "INSERT INTO UserTables ('tablename','createBlockHeight','version') VALUES (?,?,?);"
          theTableName = Utf8 (toS name)
      callDb "versionedTablesInsert" $ \db -> exec' db s
        [ SText theTableName
        , SInt (fromIntegral bh)
        , SInt (fromIntegral version)
        ]

createBlockHistoryTable :: BlockHandler SQLiteEnv ()
createBlockHistoryTable =
  callDb "createBlockHistoryTable" $ \db -> exec_ db
    "CREATE TABLE BlockHistory \
    \(blockheight UNSIGNED BIGINT,\
    \ hash BLOB,\
    \ CONSTRAINT blockHashConstraint UNIQUE (blockheight, hash));"

createVersionHistoryTable  :: BlockHandler SQLiteEnv ()
createVersionHistoryTable =
  callDb "createVersionHistoryTable" $ \db -> exec_ db
    "CREATE TABLE VersionHistory (version UNSIGNED BIGINT,\
    \ blockheight UNSIGNED BIGINT,\
    \ CONSTRAINT versionConstraint UNIQUE (version, blockheight));"

createUserTablesTable  :: BlockHandler SQLiteEnv ()
createUserTablesTable =
  callDb "createUserTablesTable" $ \db -> exec_ db
    "CREATE TABLE UserTables (tablename TEXT\
  \ , createBlockHeight UNSIGNED BIGINT\
  \ , version UNSIGNED BIGINT\
  \ , CONSTRAINT versionTableConstraint UNIQUE(version, createBlockHeight, tablename));"

createUserTable :: TableName -> BlockVersion -> BlockHandler SQLiteEnv ()
createUserTable name b = do
  callDb "createUserTable" $ \db -> exec_ db $
                  "CREATE TABLE "
                   <> Utf8 (toS (flip T.snoc ']' $ T.cons '[' $ (asString name)))
                   <> " (rowkey TEXT\
                      \, blockheight UNSIGNED BIGINT\
                      \, version UNSIGNED BIGINT\
                      \, txid UNSIGNED BIGINT\
                      \, rowdata BLOB)"
  versionedTablesInsert name b

handleVersion :: BlockHeight -> ParentHash -> BlockHandler SQLiteEnv ()
handleVersion bRestore hsh = do
  bCurrent <- do
    r <- callDb "handleVersion" $ \ db -> qry_ db "SELECT max(blockheight) AS current_block_height FROM BlockHistory;" [RInt]
    SInt bh <- liftIO $ expectSingleRowCol "handleVersion: (block):" r
    return $ BlockHeight (fromIntegral bh)
  vCurrent <- do
    r <- callDb "handleVersion" $ \db -> qry_ db "SELECT max(version) AS current_version FROM VersionHistory;" [RInt]
    SInt version <- liftIO $ expectSingleRowCol "handleVersion: (version):" r
    return $ ReorgVersion version

  -- enforce invariant that the history has (B_restore-1,H_parent).
  historyInvariant <- callDb "handleVersion" $ \db -> do
        res <- qry db "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"
            [SInt $ fromIntegral $ pred bRestore, SBlob (Data.Serialize.encode hsh)] [RInt]
            >>= expectSingleRowCol "handleVersion: (historyInvariant):"
        return res
  when (historyInvariant /= SInt 1) $
    internalError "handleVersion: History invariant violation"


  case compare bRestore (bCurrent + 1) of
   GT -> internalError "handleVersion: Block_Restore invariant violation!"
   EQ -> do
     assign bsBlockVersion (BlockVersion bRestore vCurrent)
     bs@(BlockVersion bh version) <- gets _bsBlockVersion
     count <- callDb "handleVersion" $ \db -> do
       result <- qry db
         "SELECT COUNT(*) FROM VersionHistory WHERE blockheight = ? AND version = ?;"
         [SInt (fromIntegral bh), SInt (fromIntegral version)]
         [RInt]
       expectSingleRowCol "handleVersion: block version check" result
     case count of
      SInt 0 -> versionHistoryInsert bs
      _ -> return ()
   LT -> do
     bsBlockVersion .= BlockVersion bRestore (succ vCurrent)
     tableMaintenanceRowsVersionedTables
     dropUserTables
     deleteHistory
     gets _bsBlockVersion >>= versionHistoryInsert

tableMaintenanceRowsVersionedTables :: BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedTables = do
  tableMaintenanceRowsVersionedSystemTables
  tableMaintenanceRowsVersionedUserTables

tableMaintenanceRowsVersionedSystemTables :: BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedSystemTables  = do
  (BlockVersion bh _) <- gets _bsBlockVersion
  callDb "tableMaintenanceRowsVersionedSystemTables" $ \db -> do
    exec' db "DELETE FROM [SYS:keysets] WHERE blockheight >= ?" [SInt (fromIntegral bh)]
    exec' db "DELETE FROM [SYS:modules] WHERE blockheight >= ?" [SInt (fromIntegral bh)]
    exec' db "DELETE FROM [SYS:namespaces] WHERE blockheight >= ?" [SInt (fromIntegral bh)]
    exec' db "DELETE FROM [SYS:pacts] WHERE blockheight >= ?" [SInt (fromIntegral bh)]

tableMaintenanceRowsVersionedUserTables :: BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedUserTables = do
  (BlockVersion bh _) <- gets _bsBlockVersion
  tblNames <- callDb "tableMaintenanceRowsVersionedUserTables" $ \db -> qry db
    "SELECT tablename\
    \ FROM UserTables\
    \ WHERE createBlockHeight < ?;"
    [SInt (fromIntegral bh)]
    [RText]
  forM_ tblNames $ \case
      [SText tbl] -> do
        callDb "tableMaintenanceRowsVersionedUserTables" $ \db ->
          exec' db
          ("DELETE FROM [" <> tbl <> "] WHERE blockheight >= ?")
          [SInt (fromIntegral bh)]
      _ -> internalError "tableMaintenanceRowsUserTables: An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

dropUserTables :: BlockHandler SQLiteEnv ()
dropUserTables = do
  (BlockVersion (BlockHeight bh) _) <- gets _bsBlockVersion
  tblNames <- callDb "dropUserTables" $ \db -> qry db
    "SELECT tablename\
    \ FROM UserTables\
    \ WHERE createBlockHeight >=?"
    [SInt (fromIntegral bh)]
    [RText]
  forM_ tblNames $ \case
      [SText name] -> do
        callDb "dropUserTables" $ \db -> exec_ db $ "DROP TABLE [" <> name <> "];"
      _ -> internalError "dropUserTables: An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

deleteHistory :: BlockHandler SQLiteEnv ()
deleteHistory = do
  (BlockVersion bh _) <- gets _bsBlockVersion
  callDb "deleteHistory" $ \db -> exec' db
    "DELETE FROM BlockHistory\
    \ WHERE BlockHistory.blockheight >= ?"
    [SInt (fromIntegral bh)]

initSchema :: BlockHandler SQLiteEnv ()
initSchema = withSavepoint DbTransaction $ do
  createBlockHistoryTable
  createVersionHistoryTable
  createUserTablesTable
  create (toTableName KeySets)
  create (toTableName Modules)
  create (toTableName Namespaces)
  create (toTableName Pacts)
  where
    toTableName :: Domain k v -> TableName
    toTableName = TableName . asString
    create name = do
      log "DDL" $ "initSchema: "  ++ show name
      callDb "initSchema" $ \db ->
        exec_ db $
        "CREATE TABLE "
        <> Utf8 (toS (flip T.snoc ']' $ T.cons '[' $ asString name))
        <> " (rowkey TEXT\
           \, blockheight UNSIGNED BIGINT\
           \, version UNSIGNED BIGINT\
           \, txid UNSIGNED BIGINT\
           \, rowdata BLOB)"

{-

NOTE: Ensure that restore on a new block cannot cause a version
change. A call to discard should happen after the restore call in
execNewBlock. This should revert any version change which may happen
in the restore call.

-}
