{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Pact.Backend.ChainwebPactDb
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb
( chainwebPactDb
, handlePossibleRewind
, blockHistoryInsert
, initSchema
, doCreateUserTable
) where

import Control.Exception hiding (try)
import Control.Exception.Safe hiding (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson hiding ((.=))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (concat)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Serialize (encode)
import Data.String
import Data.String.Conv
import qualified Data.Text as T

import Database.SQLite3.Direct as SQ3

import Prelude hiding (concat, log)

-- pact

import Pact.Persist
import Pact.PersistPactDb hiding (db)
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.SQLite
import Pact.Types.Term (ModuleName(..), ObjectMap(..), TableName(..))
import Pact.Types.Util (AsString(..))

-- chainweb

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types (internalError)

chainwebPactDb :: PactDb (BlockEnv SQLiteEnv)
chainwebPactDb = PactDb
    { _readRow = \d k e -> runBlockEnv e $ doReadRow d k
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


doReadRow
    :: (IsString k, FromJSON v)
    => Domain k v
    -> k
    -> BlockHandler SQLiteEnv (Maybe v)
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
                       \ ORDER BY blockheight DESC\
                       \ LIMIT 1"
                 qry db stmt [SText kstr] [RBlob]
      case result of
          [] -> return Nothing
          [[SBlob a]] -> return $ decode $ fromStrict a
          err -> internalError $
                   "doReadRow: Expected (at most) a single result, but got: " <>
                   T.pack (show err)

writeSys
    :: (AsString k, ToJSON v)
    => WriteType
    -> Domain k v
    -> k
    -> v
    -> BlockHandler SQLiteEnv ()
writeSys wt d k v = do
    -- BlockVersion bh _version <- gets _bsBlockVersion
    bh <- gets _bsBlockHeight
    txid <- gets _bsTxId
    callDb "writeSys" (write (getKeyString k) bh txid)
    record (toTableName $ domainTableName d) d k v
  where
    toTableName (Utf8 str) = TableName $ toS str

    getKeyString = case d of
        KeySets -> convKeySetName
        Modules -> convModuleName
        Namespaces -> convNamespaceName
        Pacts -> convPactId
        UserTables _ -> error "impossible"
    write key bh txid db =
        let f = case wt of
                    Insert -> backendWriteInsert
                    Update -> backendWriteUpdate
                    Write -> backendWriteWrite
        in f key (domainTableName d) bh txid v db

backendWriteInsert
    :: ToJSON v
    => Utf8
    -> Utf8
    -> BlockHeight
    -> TxId
    -> v
    -> Database
    -> IO ()
backendWriteInsert key tn bh txid v db = do
    exec' db q [ SText key
               , SInt (fromIntegral bh)
               , SInt (fromIntegral txid)
               , SBlob (toStrict (Data.Aeson.encode v))]
    exec' db mutq [SText tn, SInt (fromIntegral bh)]
  where
    mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"
    q = mconcat [ "INSERT INTO ["
                , tn
                , "] ('rowkey','blockheight','txid','rowdata') \
                  \ VALUES (?,?,?,?);"
                ]

backendWriteUpdate
    :: ToJSON v
    => Utf8
    -> Utf8
    -> BlockHeight
    -> TxId
    -> v
    -> Database
    -> IO ()
backendWriteUpdate key tn bh txid v db = do
  exec' db q [ SBlob (toStrict (Data.Aeson.encode v))
             , SText key
             , SInt (fromIntegral bh)
             , SInt (fromIntegral txid)
             ]
  exec' db mutq [SText tn, SInt (fromIntegral bh)]
  where
    mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"
    q = mconcat [ "UPDATE ["
                , tn
                , "] SET rowdata = ? WHERE rowkey = ? AND blockheight = ? \
                  \ AND txid = ? ;"
                ]

backendWriteWrite
    :: ToJSON v
    => Utf8
    -> Utf8
    -> BlockHeight
    -> TxId
    -> v
    -> Database
    -> IO ()
backendWriteWrite key tn bh txid v db = do
    exec' db q [ SText key
               , SInt (fromIntegral bh)
               , SInt (fromIntegral txid)
               , SBlob (toStrict (Data.Aeson.encode v))
               ]
    exec' db mutq [SText tn, SInt (fromIntegral bh)]
  where
    mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"
    q = mconcat [ "INSERT OR REPLACE INTO ["
                , tn
                , "] ('rowkey','blockheight','txid','rowdata') \
                  \ VALUES (?,?,?,?) ;"
                ]

writeUser
  :: WriteType
    -> Domain RowKey (ObjectMap PactValue)
    -> RowKey
    -> ObjectMap PactValue
    -> BlockHandler SQLiteEnv ()
writeUser wt d k row = do
  txid <- gets _bsTxId
  bh <- gets _bsBlockHeight
  userWrite k bh txid
  where
    toTableName (Utf8 str) = TableName $ toS str

    userWrite key bh txid = do
      olds <- doReadRow d key
      case (olds, wt) of
        (Nothing, Insert) -> ins
        (Just _, Insert) -> err
        (Nothing, Write) -> ins
        (Just old, Write) -> upd old
        (Just old, Update) -> upd old
        (Nothing, Update) -> err

      where
        err = internalError $
          "writeUser: Update: no row found for key " <>
          asString key
        upd oldrow = do
          let row' = ObjectMap (M.union (_objectMap row) (_objectMap oldrow))
              tn = domainTableName d
          callDb "writeUser"
            $ backendWriteUpdate (Utf8 $! toS $ asString key) tn bh txid row'
          record (toTableName tn) d key row'
        ins = do
          let tn = domainTableName d
          callDb "writeUser"
            $ backendWriteInsert (Utf8 $! toS $ asString key) tn bh txid row
          record (toTableName tn) d key row

doWriteRow
  :: (AsString k, ToJSON v)
    => WriteType
    -> Domain k v
    -> k
    -> v
    -> BlockHandler SQLiteEnv ()
doWriteRow wt d k v = case d of
  (UserTables _) -> writeUser wt d k v
  _ -> writeSys wt d k v

doKeys
    :: (IsString k, AsString k)
    => Domain k v
    -> BlockHandler SQLiteEnv [k]
doKeys d = do
    let tn = domainTableName d
    ks <- callDb "doKeys" $ \db ->
          qry_ db
              ("SELECT rowkey FROM [" <> tn <> "];")
              [RText]
    forM ks $ \row -> do
        case row of
            [SText (Utf8 k)] -> return $ fromString $ toS k
            _ -> internalError "doKeys: The impossible happened."
{-# INLINE doKeys #-}

-- tid is non-inclusive lower bound for the search
doTxIds :: TableName -> TxId -> BlockHandler SQLiteEnv [TxId]
doTxIds (TableName tn) (TxId tid) = do
  rows <- callDb "doTxIds" $ \db ->
    qry db
    ("SELECT txid FROM [" <> Utf8 (toS tn) <> "] WHERE txid > ?")
    [SInt (fromIntegral tid)]
    [RInt]
  forM rows $ \case
    [SInt tid'] -> return $ TxId (fromIntegral tid')
    _ -> internalError "doTxIds: the impossible happened"
{-# INLINE doTxIds #-}

record
    :: (AsString k, ToJSON v)
    => TableName
    -> Domain k v
    -> k
    -> v
    -> BlockHandler SQLiteEnv ()
record tt d k v =
    bsTxRecord %= M.insertWith (flip (++)) tt txlogs
  where
    txlogs = [TxLog (asString d) (asString k) (toJSON v)]

doCreateUserTable :: TableName -> ModuleName -> BlockHandler SQLiteEnv ()
doCreateUserTable tn mn = do
  bh <- gets _bsBlockHeight
  createUserTable tn bh
  bsTxRecord %= M.insertWith (flip (++)) "SYS:usertables" txlogs
  where
    uti = UserTableInfo mn
    txlogs = [TxLog "SYS:usertables" (asString tn) (toJSON uti)]
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

doCommit :: BlockHandler SQLiteEnv [TxLog Value]
doCommit = use bsMode >>= \mm -> case mm of
    Nothing -> doRollback >> internalError "doCommit: Not in transaction"
    Just m -> do
        txrs <- use bsTxRecord
        if m == Transactional
          then do
            modify' (over bsTxId succ)
            -- commit
            commitSavepoint PactDbTransaction
            resetTemp
          else doRollback
        return $! concat txrs
{-# INLINE doCommit #-}

doGetTxLog :: FromJSON v => Domain k v -> TxId -> BlockHandler SQLiteEnv [TxLog v]
doGetTxLog d txid = do
  bh <- gets _bsBlockHeight
  rows <- callDb "doGetTxLog" $ \db -> qry db q
    [ SInt (fromIntegral txid)
    , SInt (fromIntegral bh)
    ]
    [RText, RBlob]
  forM rows $ \case
    [SText key, SBlob value] ->
      case Data.Aeson.decodeStrict' value of
        Nothing -> internalError $
          "doGetTxLog: Unexpected value, unable to deserialize log"
        Just v ->
          return $! TxLog (toS $ unwrap $ domainTableName d) (toS $ unwrap key) v
    err -> internalError $
      "doGetTxLog: Expected single row with two columns as the \
      \result, got: " <> T.pack (show err)
  where
    q = mconcat [ "SELECT rowkey, rowdata FROM ["
                , domainTableName d
                , "] WHERE txid = ? AND blockheight = ?"
                ]

unwrap :: Utf8 -> BS.ByteString
unwrap (Utf8 str) = str

blockHistoryInsert :: BlockHeight -> BlockHash -> TxId -> BlockHandler SQLiteEnv ()
blockHistoryInsert bh hsh t = do
    let s = "INSERT INTO BlockHistory ('blockheight','hash','endingtxid') \
            \ VALUES (?,?,?);"
    callDb "blockHistoryInsert" $ \db ->
        exec' db s [ SInt (fromIntegral bh)
                   , SBlob (Data.Serialize.encode hsh)
                   , SInt (fromIntegral t) ]

createBlockHistoryTable :: BlockHandler SQLiteEnv ()
createBlockHistoryTable =
    callDb "createBlockHistoryTable" $ \db -> exec_ db
        "CREATE TABLE BlockHistory \
        \(blockheight UNSIGNED BIGINT NOT NULL,\
        \ hash BLOB NOT NULL,\
        \ endingtxid UNSIGNED BIGINT NOT NULL, \
        \ CONSTRAINT blockHashConstraint UNIQUE (blockheight));"

createTableCreationTable :: BlockHandler SQLiteEnv ()
createTableCreationTable =
  callDb "createTableCreationTable" $ \db -> exec_ db
    "CREATE TABLE VersionedTableCreation\
    \(tablename TEXT NOT NULL\
    \, createBlockheight UNSIGNED BIGINT NOT NULL\
    \, CONSTRAINT creation_unique UNIQUE(tablename, createBlockheight));"

createTableMutationTable :: BlockHandler SQLiteEnv ()
createTableMutationTable =
  callDb "createTableMutationTable" $ \db -> do
  exec_ db "CREATE TABLE VersionedTableMutation\
           \(tablename TEXT NOT NULL\
           \, blockheight UNSIGNED BIGINT NOT NULL\
           \, CONSTRAINT mutation_unique UNIQUE(tablename,blockheight));"
  exec_ db "CREATE INDEX mutation_bh ON VersionedTableMutation(blockheight);"

createUserTable :: TableName -> BlockHeight -> BlockHandler SQLiteEnv ()
createUserTable name bh = do
  let tablename :: Utf8
      tablename = Utf8 (toS $ asString name)
      indexname = Utf8 (toS $ asString name <> "_bh")
  callDb "createUserTable" $ \db -> do
    exec_ db $ "CREATE TABLE ["
      <> tablename
      <> "] (rowkey TEXT\
           \, blockheight UNSIGNED BIGINT NOT NULL\
           \, txid UNSIGNED BIGINT NOT NULL\
           \, rowdata BLOB NOT NULL\
           \, UNIQUE (blockheight, rowkey, txid));"
    exec_ db $ "CREATE INDEX [" <> indexname  <> "] ON [" <> tablename <> "](blockheight);"
    exec' db "INSERT OR IGNORE INTO VersionedTableCreation VALUES (?,?)" [SText tablename, SInt (fromIntegral bh)]

handlePossibleRewind :: BlockHeight -> ParentHash -> BlockHandler SQLiteEnv TxId
handlePossibleRewind bRestore hsh = do
  bCurrent <- getBCurrent
  checkHistoryInvariant
  case compare bRestore (bCurrent + 1) of
    GT -> internalError "handlePossibleRewind: Block_Restore invariant violation!"
    EQ -> newChildBlock bCurrent
    LT -> rewindBlock bRestore
  where
    getBCurrent = do
        r <- callDb "handlePossibleRewind" $ \db ->
             qry_ db "SELECT max(blockheight) AS current_block_height \
                     \FROM BlockHistory;" [RInt]
        SInt bh <- liftIO $ expectSingleRowCol "handlePossibleRewind: (block):" r
        return $! BlockHeight (fromIntegral bh)

    checkHistoryInvariant = do
      -- enforce invariant that the history has
      -- (B_restore-1,H_parent).
      historyInvariant <- callDb "handlePossibleRewind" $ \db -> do
        qry db "SELECT COUNT(*) FROM BlockHistory WHERE \
               \blockheight = ? and hash = ?;"
               [ SInt $! fromIntegral $ pred bRestore
               , SBlob (Data.Serialize.encode hsh) ]
               [RInt]
        >>= expectSingleRowCol "handlePossibleRewind: (historyInvariant):"
      when (historyInvariant /= SInt 1) $
        internalError "handlePossibleRewind: History invariant violation"

    newChildBlock bCurrent = do
      assign bsBlockHeight bRestore
      let msg = "handlePossibleRewind: newChildBlock: error finding txid"
      SInt txid <- callDb "getting txid" $ \db ->
        expectSingleRowCol msg =<< qry db
            "SELECT endingtxid FROM BlockHistory WHERE blockheight = ?;"
            [SInt (fromIntegral bCurrent)]
            [RInt]
      assign bsTxId (fromIntegral txid)
      return $ fromIntegral txid

    rewindBlock bh = do
      assign bsBlockHeight bh
      tableMaintenanceRowsVersionedSystemTables
      callDb "rewindBlock" $ \db -> do
          droppedtbls <- dropTablesAtRewind bh db
          vacuumTablesAtRewind bh droppedtbls db
      t <- deleteHistory
      assign bsTxId t
      return $! t

dropTablesAtRewind :: BlockHeight -> Database -> IO [Utf8]
dropTablesAtRewind bh db = do
  toDropTblNames <- qry db "SELECT tablename FROM VersionedTableCreation where createBlockheight >= ?;"
    [SInt (fromIntegral bh)]
    [RText]
  forM toDropTblNames $ \case
    [SText tblname] -> do
      exec_ db $ "DROP TABLE " <> tblname <> ";"
      return tblname
    _ -> internalError "rewindBlock: dropTablesAtRewind: Couldn't resolve the name of the table to drop."

vacuumTablesAtRewind :: BlockHeight -> [Utf8] -> Database -> IO ()
vacuumTablesAtRewind bh droppedtbls db = do
  toVacuumTblNames <- qry db "SELECT DISTINCT tablename FROM VersionedTableMutation WHERE blockheight >= ?;"
    [SInt (fromIntegral bh)]
    [RText]
  forM_ toVacuumTblNames $ \case
    [SText tblname] -> do
      when (notElem tblname droppedtbls) $
        exec' db ("DELETE FROM [" <> tblname <> "] WHERE blockheight >= ?") [SInt (fromIntegral bh)]
    _ -> internalError "rewindBlock: Couldn't resolve the name of the table to vacuum."
  exec' db "DELETE FROM VersionedTableMutation WHERE blockheight >= ?;" [SInt (fromIntegral bh)]

tableMaintenanceRowsVersionedSystemTables :: BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedSystemTables = do
  bh <- gets _bsBlockHeight
  callDb "tableMaintenanceRowsVersionedSystemTables" $ \db -> do
      exec' db "DELETE FROM [SYS:KeySets] WHERE blockheight >= ?"
            [SInt (fromIntegral bh)]
      exec' db "DELETE FROM [SYS:Modules] WHERE blockheight >= ?"
            [SInt (fromIntegral bh)]
      exec' db "DELETE FROM [SYS:Namespaces] WHERE blockheight >= ?"
            [SInt (fromIntegral bh)]
      exec' db "DELETE FROM [SYS:Pacts] WHERE blockheight >= ?"
            [SInt (fromIntegral bh)]

deleteHistory :: BlockHandler SQLiteEnv TxId
deleteHistory = do
    bh <- gets _bsBlockHeight
    callDb "Deleting from BlockHistory, VersionHistory. Also get txid." $ \db -> do
        exec' db "DELETE FROM BlockHistory WHERE blockheight >= ?"
              [SInt (fromIntegral bh)]
        -- exec' db "DELETE FROM VersionHistory WHERE version >= ?;" [SInt (fromIntegral v)]
        t <- qry db "SELECT endingtxid FROM BlockHistory WHERE blockheight = ?"
            [SInt (fromIntegral $ pred bh)] [RInt]
          >>= fmap convert . expectSingleRowCol "txid after delete history"
        return t
  where
    convert (SInt thing) = fromIntegral thing
    convert _ = error "deleteHistory: Something went wrong!"

initSchema :: BlockHandler SQLiteEnv ()
initSchema = withSavepoint DbTransaction $ do
    createBlockHistoryTable
    createTableCreationTable
    createTableMutationTable
    create (toTableName KeySets)
    create (toTableName Modules)
    create (toTableName Namespaces)
    create (toTableName Pacts)
  where
    toTableName :: Domain k v -> Utf8
    toTableName = domainTableName
    create name = do
        log "DDL" $ "initSchema: "  ++ toS name
        callDb "initSchema" $ \db -> do
            let tablename = name
                indexname = name <> "_bh"
            exec_ db $
              "CREATE TABLE ["
              <> name
              <> "] (rowkey TEXT\
                 \, blockheight UNSIGNED BIGINT NOT NULL\
                 \, txid UNSIGNED BIGINT NOT NULL\
                 \, rowdata BLOB NOT NULL\
                 \, UNIQUE(blockheight, rowkey, txid));"
            exec_ db $ "CREATE INDEX [" <>  indexname <> "] ON [" <> tablename <> "](blockheight);"
