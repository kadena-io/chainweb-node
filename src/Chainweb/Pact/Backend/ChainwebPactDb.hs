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
  ( chainwebPactDb
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
import Data.Foldable (concat)
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Serialize (encode)
import Data.String
import Data.String.Conv
import qualified Data.Text as T

import Database.SQLite3.Direct as SQ3

import Prelude hiding (log, concat)

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
import Chainweb.BlockHeaderDB
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types (internalError)
import Chainweb.Payload.PayloadStore

chainwebPactDb :: PactDb (BlockEnv SQLiteEnv)
chainwebPactDb = PactDb
  {
    _readRow = \d k e -> runBlockEnv e $ doReadRow d k
  , _writeRow = \wt d k v e -> runBlockEnv e $ doWriteRow wt d k v
  , _keys = \d e -> runBlockEnv e $ doKeys d
  , _txids = \t txid e -> runBlockEnv e $ doTxIds t txid
  , _createUserTable = \tn mn e -> runBlockEnv e $ doCreateUserTable tn mn
  , _getUserTableInfo = error "WILL BE DEPRECATED!"
  , _beginTx = \m e -> runBlockEnv e $ doBegin m
  , _commitTx = \e -> runBlockEnv e doCommit
  , _rollbackTx = \e -> runBlockEnv e doRollback
  , _getTxLog = \d tid e -> runBlockEnv e $ doGetTxLog d tid
  }

doReadRow :: (IsString k, FromJSON v) => Domain k v -> k -> BlockHandler SQLiteEnv (Maybe v)
doReadRow d k =
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
     result <- callDb "doReadRow" $ \db -> do
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
         err -> internalError $ "doReadRow: Expected (at most) a single result, but got: " <> T.pack (show err)

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
      olds <- doReadRow d key
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

doWriteRow :: (AsString k, ToJSON v) => WriteType -> Domain k v -> k -> v -> BlockHandler SQLiteEnv ()
doWriteRow wt d k v = do
    case d of
      (UserTables _) -> writeUser wt d k v
      _ -> writeSys wt d k v

doKeys :: (IsString k, AsString k) => Domain k v -> BlockHandler SQLiteEnv [k]
doKeys d = do
  let tn = domainTableName d
  version <- gets (_bvVersion . _bsBlockVersion)
  ks <- callDb "doKeys" $ \db -> qry db
            ("SELECT rowkey FROM [" <> tn <> "] WHERE version = ?;")
            [SInt (fromIntegral version)]
            [RText]
  forM ks $ \row -> do
    case row of
      [SText (Utf8 k)] -> return $ fromString $ toS k
      _ -> internalError "doKeys: The impossible happpened."
{-# INLINE doKeys #-}

-- tid is non-inclusive lower bound for the search
doTxIds :: TableName -> TxId -> BlockHandler SQLiteEnv [TxId]
doTxIds (TableName tn) (TxId tid) = do
  version <- gets (_bvVersion . _bsBlockVersion)
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
  tryAny (rollbackSavepoint PactDbTransaction) >>= \case
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
  beginSavepoint PactDbTransaction
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
      modify' (over bsTxId succ)
      -- commit
      commitSavepoint PactDbTransaction
      resetTemp
    else doRollback
    return $ concat txrs
{-# INLINE doCommit #-}

doGetTxLog :: FromJSON v => Domain k v -> TxId -> BlockHandler SQLiteEnv [TxLog v]
doGetTxLog d txid = do
  BlockVersion bh version <- gets _bsBlockVersion
  rows <- callDb "doGetTxLog" $ \db -> qry db
        ("SELECT rowkey, rowdata FROM [" <> domainTableName d <> "] WHERE txid = ? AND version = ? AND blockheight = ?")
        [SInt (fromIntegral txid), SInt (fromIntegral bh), SInt (fromIntegral version)]
        [RText, RBlob]
  forM rows $ \case
    [SText key, SBlob value] ->
      case Data.Aeson.decodeStrict' value of
        Nothing -> internalError $ "doGetTxLog: Unexpected value, unable to deserialize log"
        Just v -> return $ TxLog (toS $ unwrap $ domainTableName d) (toS $ unwrap $ key) v
    err -> internalError $ "doGetTxLog: Expected single row with two columns as the result, got: " <> T.pack (show err)

unwrap :: Utf8 -> BS.ByteString
unwrap (Utf8 str) = str

blockHistoryInsert :: BlockHeight -> BlockHash -> TxId -> BlockHandler SQLiteEnv ()
blockHistoryInsert bh hsh t = do
  let s = "INSERT INTO BlockHistory ('blockheight','hash','endingtxid') VALUES (?,?,?);"
  callDb "blockHistoryInsert" $ \db ->
    exec' db s [SInt (fromIntegral bh), SBlob (Data.Serialize.encode hsh), SInt (fromIntegral t)]

versionHistoryInsert :: BlockVersion -> TxId -> BlockHandler SQLiteEnv ()
versionHistoryInsert (BlockVersion bh version) txid = do
  let s = "INSERT INTO VersionHistory ('version','blockheight','txid') VALUES (?,?,?);"
  callDb "versionHistoryInsert" $ \db ->
    exec' db s [SInt (fromIntegral version), SInt (fromIntegral bh), SInt (fromIntegral txid)]

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
    \(blockheight UNSIGNED BIGINT NOT NULL,\
    \ hash BLOB NOT NULL,\
    \ endingtxid UNSIGNED BIGINT NOT NULL, \
    \ CONSTRAINT blockHashConstraint UNIQUE (blockheight));"

createVersionHistoryTable  :: BlockHandler SQLiteEnv ()
createVersionHistoryTable =
  callDb "createVersionHistoryTable" $ \db -> do
     exec_ db
       "CREATE TABLE VersionHistory (version UNSIGNED BIGINT NOT NULL,\
       \ blockheight UNSIGNED BIGINT NOT NULL,\
       \ txid UNSIGNED BIGINT NOT NULL,\
       \ CONSTRAINT versionConstraint UNIQUE (version, blockheight));"

createUserTablesTable  :: BlockHandler SQLiteEnv ()
createUserTablesTable =
  callDb "createUserTablesTable" $ \db -> exec_ db
    "CREATE TABLE UserTables (tablename TEXT NOT NULL\
  \ , createBlockHeight UNSIGNED BIGINT NOT NULL\
  \ , version UNSIGNED BIGINT NOT NULL\
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
                      \, rowdata BLOB \
                      \, UNIQUE(blockheight, rowkey, txid));"
  versionedTablesInsert name b

handleVersion :: BlockHeight -> ParentHash -> BlockHeaderDb -> Maybe (PayloadDb cas) -> BlockHandler SQLiteEnv TxId
handleVersion bRestore hsh _cdb _payloadDb = do
  bCurrent <- do
    r <- callDb "handleVersion" $ \ db -> qry_ db "SELECT max(blockheight) AS current_block_height FROM BlockHistory;" [RInt]
    SInt bh <- liftIO $ expectSingleRowCol "handleVersion: (block):" r
    return $! BlockHeight (fromIntegral bh)
  vCurrent <- do
    r <- callDb "handleVersion" $ \db -> qry_ db "SELECT max(version) AS current_version FROM VersionHistory;" [RInt]
    SInt version <- liftIO $ expectSingleRowCol "handleVersion: (version):" r
    return $! ReorgVersion version

  -- enforce invariant that the history has (B_restore-1,H_parent).
  historyInvariant <- callDb "handleVersion" $ \db -> do
        res <- qry db "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"
            [SInt $ fromIntegral $ pred bRestore, SBlob (Data.Serialize.encode hsh)] [RInt]
            >>= expectSingleRowCol "handleVersion: (historyInvariant):"
        return $! res
  when (historyInvariant /= SInt 1) $
    internalError "handleVersion: History invariant violation"

  case compare bRestore (bCurrent + 1) of
   GT -> internalError "handleVersion: Block_Restore invariant violation!"
   EQ -> do
     assign bsBlockVersion (BlockVersion bRestore vCurrent)
     (SInt txid) <- callDb "getting txid" $ \db ->
       expectSingleRowCol "blah" =<< qry db
         "SELECT endingtxid FROM BlockHistory WHERE blockheight = ?;"
         [SInt (fromIntegral bCurrent)]
         [RInt]
     bs@(BlockVersion bh version) <- gets _bsBlockVersion
     result <- callDb "handleVersion" $ \db -> qry db
         "SELECT txid FROM VersionHistory WHERE blockheight = ? AND version = ?;"
         [SInt (fromIntegral bh), SInt (fromIntegral version)]
         [RInt]
     newtxid <- if null result
       then do
         versionHistoryInsert bs (fromIntegral txid)
         return $! (fromIntegral txid :: TxId)
       else do
        r <- liftIO $ expectSingleRowCol "handleVersion: block version check" result
        case r of
          SInt res -> return $! (fromIntegral res)
          _ -> error "handleVersion: We failed to get an integer for the txid."
     assign bsTxId newtxid
     return newtxid
   LT -> do
     bsBlockVersion .= BlockVersion bRestore (succ vCurrent)
     tableMaintenanceRowsVersionedTables
     dropUserTables
     t <- deleteHistory
     assign bsTxId t
     b <- gets _bsBlockVersion
     versionHistoryInsert b t
     return $! t

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
  callDb "tableMaintenanceRowsVersionedUserTables" $ \db -> do
    tblNames <- qry db "SELECT tablename FROM UserTables WHERE createBlockHeight < ?;" [SInt (fromIntegral bh)] [RText]
    forM_ tblNames $ \case
      [SText tbl] -> exec' db ("DELETE FROM [" <> tbl <> "] WHERE blockheight >= ?") [SInt (fromIntegral bh)]
      _ -> internalError "tableMaintenanceRowsUserTables: An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

dropUserTables :: BlockHandler SQLiteEnv ()
dropUserTables = do
  (BlockVersion (BlockHeight bh) _) <- gets _bsBlockVersion
  callDb "dropUserTables" $ \db -> do
    tblNames <- qry db "SELECT tablename FROM UserTables WHERE createBlockHeight >=?" [SInt (fromIntegral bh)] [RText]
    forM_ tblNames $ \case
      [SText name] -> exec_ db $ "DROP TABLE [" <> name <> "];"
      _ -> internalError "dropUserTables: An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

deleteHistory :: BlockHandler SQLiteEnv TxId
deleteHistory = do
  (BlockVersion bh v) <- gets _bsBlockVersion
  callDb "Deleting from BlockHistory, VersionHistory. Also get txid." $ \db -> do
    exec' db "DELETE FROM BlockHistory WHERE blockheight >= ?" [SInt (fromIntegral bh)]
    exec' db "DELETE FROM VersionHistory WHERE version >= ?;" [SInt (fromIntegral v)]
    qry db "SELECT endingtxid FROM BlockHistory WHERE blockheight = ?" [SInt (fromIntegral $ pred bh)] [RInt]
      >>= fmap convert . expectSingleRowCol "txid after delete history"
  where
    convert (SInt thing) = fromIntegral thing
    convert _ = error "deleteHistory: Something went wrong!"

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
           \, rowdata BLOB\
           \, UNIQUE(blockheight, rowkey, txid));"
