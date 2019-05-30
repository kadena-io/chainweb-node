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
     result <- callDb $ \db -> do
                let stmt =
                      "SELECT tabledata \
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
        callDb (write (getKeyString k) bh version txid)
        let toTableName (Utf8 str) = TableName $ toS str
        record (toTableName $ domainTableName d) d k v
  where
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
  exec' db ("INSERT INTO [" <> tn <> "] ('rowkey','blockheight','version','txid','tabledata') VALUES (?,?,?,?,?);")
    [SText key
    , SInt (fromIntegral bh)
    , SInt (fromIntegral version)
    , SInt (fromIntegral txid)
    , SBlob (toStrict (Data.Aeson.encode v))]

backendWriteUpdate :: ToJSON v => Utf8 -> Utf8 -> BlockHeight -> ReorgVersion -> TxId -> v -> Database -> IO ()
backendWriteUpdate key tn bh version txid v db =
  exec' db ("UPDATE [" <> tn <> "] SET rowkey = ?, blockheight = ?, version =  ?, txid = ?, tabledata = ? ;")
    [SText key
    , SInt (fromIntegral bh)
    , SInt (fromIntegral version)
    , SInt (fromIntegral txid)
    , SBlob (toStrict (Data.Aeson.encode v))]

-- don't need the initial select, it will just fail. Look at pact
backendWriteWrite :: ToJSON v => Utf8 -> Utf8 -> BlockHeight -> ReorgVersion -> TxId -> v -> Database -> IO ()
backendWriteWrite key tn bh version txid v db =
  exec' db ("INSERT OR REPLACE INTO [" <> tn <> "] ('rowkey','blockheight','version','txid','tabledata') VALUES (?,?,?,?,?) ;")
    [SText key, SInt (fromIntegral bh), SInt (fromIntegral version), SInt (fromIntegral txid), SBlob (toStrict (Data.Aeson.encode v))]

writeUser :: WriteType -> Domain RowKey (ObjectMap PactValue) -> RowKey -> ObjectMap PactValue -> BlockHandler SQLiteEnv ()
writeUser wt d k row = do
      case d of
        (UserTables _) -> do
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
          callDb (backendWriteUpdate (Utf8 $ toS $ asString $ key) tn bh version txid row')
          record (prependToTableName "USER:" (toTableName $ tn)) d k row'
        ins = do
          let tn = domainTableName d
          callDb $ backendWriteInsert (Utf8 $ toS $ asString $ key) tn bh version txid row
          record (prependToTableName "USER:" (toTableName $ domainTableName d)) d k row

prependToTableName :: Text -> TableName -> TableName
prependToTableName txt (TableName name) =
  TableName (txt <> name)

writeRow :: (AsString k, ToJSON v) => WriteType -> Domain k v -> k -> v -> BlockHandler SQLiteEnv ()
writeRow wt d k v = do
    case d of
      (UserTables _) -> writeUser wt d k v
      _ -> writeSys wt d k v

--  restore a block and then run regression test on the pact db

doKeys :: (IsString k, AsString k) => Domain k v -> BlockHandler SQLiteEnv [k]
doKeys d = do
  let tn = domainTableName d
  (ReorgVersion version) <- gets (_bvVersion . _bsBlockVersion)
  ks <- callDb $ \db -> qry db
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
  rows <- callDb $ \db ->
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
  createVersionedTable tn bs
  let uti = UserTableInfo mn
  {- until I find a better way to do this -}
  bsTxRecord %= M.insertWith (flip (++)) "SYS:usertables" [TxLog "SYS:usertables" (asString tn) (toJSON uti)]
{-# INLINE doCreateUserTable #-}

doRollback :: BlockHandler SQLiteEnv ()
doRollback = do
  tryAny (rollbackSavepoint Transaction) >>= \case
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
  beginSavepoint Transaction
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
      commitSavepoint Transaction
      resetTemp
    else doRollback
    return $ fromdlist $ foldr (\a b -> todlist a . b) id txrs
{-# INLINE doCommit #-}

-- -- make this in memory
-- createTxLogMap :: Set (TableName, DomainText) -> BlockHandler SQLiteEnv (M.Map TableName [TxLog Value])
-- createTxLogMap tables = foldM go M.empty tables
--   where
--     go map' (tablename@(TableName name), domain) = do
--       BlockVersion bh version <- gets _bsBlockVersion
--       txid <- gets _bsTxId
--       rows <- callDb (\db ->
--                         qry db
--                         ("SELECT rowkey,tabledata FROM [" <> Utf8 (toS name) <> "] WHERE blockheight = ? AND version = ? AND txid = ? ;")
--                         [SInt (fromIntegral bh), SInt (fromIntegral version), SInt (fromIntegral txid)]
--                         [RText,RBlob])
--       logs <- forM rows $ \case
--         [SText (Utf8 key), SBlob tbldata] ->
--           case decode (fromStrict tbldata) of
--             Nothing -> internalError "doCommit:createMap: decoding error"
--             Just v -> return (TxLog domain (toS key) v)
--         _ -> internalError "doCommit: Retrieved a malformed TxLog."
--       pure $ M.insert tablename logs map'

fromdlist :: ([a] -> [a]) -> [a]
fromdlist dl = dl []

todlist :: [a] -> [a] -> [a]
todlist xs = foldr go id xs
  where
    go a b = (a :) . b

doGetTxLog :: FromJSON v => Domain k v -> TxId -> BlockHandler SQLiteEnv [TxLog v]
doGetTxLog d txid = do
  BlockVersion (BlockHeight bh) (ReorgVersion version) <- gets _bsBlockVersion
  rows <- callDb $ \db -> qry db
        ("SELECT rowkey, tabledata FROM [" <> domainTableName d <> "] WHERE txid = ? AND version = ? AND blockheight = ?")
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
  callDb $ \db ->
    exec' db s [SInt (fromIntegral bh), SBlob (Data.Serialize.encode hsh)]

versionHistoryInsert :: BlockVersion -> BlockHandler SQLiteEnv ()
versionHistoryInsert (BlockVersion bh version) = do
  let s = "INSERT INTO VersionHistory ('version','blockheight') VALUES (?,?);"
  callDb $ \db ->
    exec' db s [SInt (fromIntegral version), SInt (fromIntegral bh)]

versionedTablesInsert :: TableName -> BlockVersion -> BlockHandler SQLiteEnv ()
versionedTablesInsert (TableName name) (BlockVersion bh version) = do
      let s = "INSERT INTO VersionedTables ('tablename','blockheight','version') VALUES (?,?,?);"
          theTableName = Utf8 (toS name)
      callDb $ \db -> exec' db s
        [ SText theTableName
        , SInt (fromIntegral bh)
        , SInt (fromIntegral version)
        ]

createBlockHistoryTable :: BlockHandler SQLiteEnv ()
createBlockHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE BlockHistory (blockheight UNSIGNED BIGINT\
    \, hash BLOB\
    \, CONSTRAINT blockHashConstraint UNIQUE (blockheight, hash));"

createVersionHistoryTable  :: BlockHandler SQLiteEnv ()
createVersionHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionHistory  (version UNSIGNED BIGINT,\
    \ blockheight UNSIGNED BIGINT,\
    \ CONSTRAINT versionConstraint UNIQUE (version, blockheight));"

createVersionedTablesTable  :: BlockHandler SQLiteEnv ()
createVersionedTablesTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionedTables (tablename TEXT\
  \ , blockheight UNSIGNED BIGINT\
  \ , version UNSIGNED BIGINT\
  \ , CONSTRAINT versionTableConstraint UNIQUE(version, blockheight));"

createVersionedTable :: TableName -> BlockVersion -> BlockHandler SQLiteEnv ()
createVersionedTable name b = do
  callDb $ \db -> exec_ db $
                  "CREATE TABLE "
                   <> Utf8 (toS (flip T.snoc ']' $ T.cons '[' $ asString name))
                   <> " (rowkey TEXT,blockheight UNSIGNED BIGINT, version UNSIGNED BIGINT,txid UNSIGNED BIGINT,tabledata BLOB)"
                   -- rename tabledata to rowdata or just data
  versionedTablesInsert name b

type ParentHash = BlockHash

handleVersion :: BlockHeight -> Maybe ParentHash -> BlockHandler SQLiteEnv ()
handleVersion _ Nothing = return ()
handleVersion bRestore (Just hsh) = do
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
            [SInt $ fromIntegral $ pred bRestore, SBlob (Data.Serialize.encode hsh)] [RInt]
            >>= expectSingleRowCol "detectVersionChange (historyInvariant):"
        return res

  when (historyInvariant /= SInt 1) $
    internalError "History invariant violation"

  case compare bRestore (bCurrent + 1) of
   GT -> internalError "PactInternalError: detectVersionChange: Block_Restore invariant violation!"
   EQ -> assign bsBlockVersion (BlockVersion bRestore vCurrent)
   LT -> do
     let bvEnv = (BlockVersion bRestore (succ vCurrent))
     assign bsBlockVersion bvEnv
     tableMaintenanceRowsVersionedTables bvEnv
     dropVersionedTables bvEnv
     deleteHistory bvEnv

tableMaintenanceRowsVersionedTables :: BlockVersion -> BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedTables bv = do
  tableMaintenanceRowsVersionedSystemTables bv
  tableMaintenanceRowsVersionedUserTables bv

tableMaintenanceRowsVersionedSystemTables :: BlockVersion -> BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedSystemTables (BlockVersion bh _) = callDb $ \db -> do
  exec' db "DELETE FROM [SYS:KeySets] WHERE blockheight >= ?" [SInt (fromIntegral bh)]
  exec' db "DELETE FROM [SYS:Modules] WHERE blockheight >= ?" [SInt (fromIntegral bh)]
  exec' db "DELETE FROM [SYS:Namespaces] WHERE blockheight >= ?" [SInt (fromIntegral bh)]
  exec' db "DELETE FROM [SYS:Pacts] WHERE blockheight >= ?" [SInt (fromIntegral bh)]

{- rename VersionedTables to UserTables -}
tableMaintenanceRowsVersionedUserTables :: BlockVersion -> BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedUserTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db ->
    qry db "SELECT tablename FROM VersionedTables WHERE blockheight < ?;" [SInt (fromIntegral bh)] [RText]
  forM_ tblNames $ \case
      [SText tbl] -> do
        callDb $ \db ->
          exec' db
          ("DELETE FROM [" <> tbl <> "] WHERE blockheight >= ?")
          [SInt (fromIntegral bh)]
      _ -> internalError "tableMaintenanceRowsVersionedTables: An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

{- rename blockheight column in VersionedTables to createBlockHeight -}

dropVersionedTables :: BlockVersion -> BlockHandler SQLiteEnv ()
dropVersionedTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db -> qry db
              "SELECT tablename FROM VersionedTables\
              \ WHERE blockheight >=?"
              [SInt (fromIntegral bh)]
              [RText]
  forM_ tblNames $ \case
      [SText name] -> do
        callDb $ \db -> exec_ db $ "DROP TABLE [" <> name <> "]"
      _ -> internalError "dropVersionedTables: An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

deleteHistory :: BlockVersion -> BlockHandler SQLiteEnv ()
deleteHistory (BlockVersion bh _) = do
  callDb $ \db -> exec' db "DELETE FROM BlockHistory\
                           \ WHERE BlockHistory.blockheight >= ?"
                  [SInt (fromIntegral bh)]

initSchema :: BlockHandler SQLiteEnv ()
initSchema = do
  let toTableName :: Domain k v -> TableName
      toTableName = TableName . asString
  withSavepoint Transaction $ do
     createBlockHistoryTable
     createVersionHistoryTable
     createVersionedTablesTable
     create (toTableName KeySets)
     create (toTableName Modules)
     create (toTableName Namespaces)
     create (toTableName Pacts)
  where
    create name =
      callDb $ \db ->
        exec_ db $
        "CREATE TABLE "
        <> Utf8 (toS (flip T.snoc ']' $ T.cons '[' $ asString name))
        <> " (rowkey TEXT,blockheight UNSIGNED BIGINT, version UNSIGNED BIGINT,txid UNSIGNED BIGINT,tabledata BLOB)"

{-

NOTE: Ensure that restore on a new block cannot cause a version
change. A call to discard should happen after the restore call in
execNewBlock. This should revert any version change which may happen
in the restore call.

-}
