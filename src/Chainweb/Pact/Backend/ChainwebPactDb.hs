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
) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson hiding ((.=))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (concat)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Serialize (encode)
import qualified Data.Set as Set
import Data.String
import Data.String.Conv
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
        result <- callDb "doReadRow" $ \db -> qry db stmt [SText kstr] [RBlob]
        case result of
            [] -> return Nothing
            [[SBlob a]] -> return $ decode $ fromStrict a
            err -> internalError $
                     "doReadRow: Expected (at most) a single result, but got: " <>
                     T.pack (show err)
      where
        stmt =
          "SELECT rowdata \
          \FROM [" <> domainTableName d <> "]\
          \ WHERE rowkey = ?\
          \ ORDER BY blockheight DESC, txid DESC\
          \ LIMIT 1"

writeSys
    :: (AsString k, ToJSON v)
    => WriteType
    -> Domain k v
    -> k
    -> v
    -> BlockHandler SQLiteEnv ()
writeSys wt d k v = do
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

    write key bh txid db = f key (domainTableName d) bh txid v db
      where
        f = case wt of
            Insert -> backendWriteInsert
            Update -> backendWriteUpdate
            Write -> backendWriteWrite

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
    markTableMutation tn bh db
  where
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
    exec' db q [ SText key
               , SInt (fromIntegral bh)
               , SInt (fromIntegral txid)
               , SBlob (toStrict (Data.Aeson.encode v))
               ]
    markTableMutation tn bh db
  where
    q = mconcat
      ["INSERT OR REPLACE INTO ["
      , tn
      , "](rowkey,blockheight,txid,rowdata) "
      , "VALUES(?,?,?,?)"]

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
    markTableMutation tn bh db
  where
    q = mconcat [ "INSERT OR REPLACE INTO ["
                , tn
                , "] ('rowkey','blockheight','txid','rowdata') \
                  \ VALUES (?,?,?,?) ;"
                ]

markTableMutation :: Utf8 -> BlockHeight -> Database -> IO ()
markTableMutation tablename blockheight db = do
    exec' db mutq [SText tablename, SInt (fromIntegral blockheight)]
  where
    mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"


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
            (Just _, Insert) -> err "Insert: row found for key "
            (Nothing, Write) -> ins
            (Just old, Write) -> upd old
            (Just old, Update) -> upd old
            (Nothing, Update) -> err "Update: no row found for key "
      where
        err msg = internalError $
          "writeUser: " <> msg <>
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
    ks <- callDb "doKeys" $ \db ->
            qry_ db  ("SELECT DISTINCT rowkey FROM [" <> tn <> "];") [RText]
    dbKeys <- forM ks $ \row -> do
        case row of
            [SText (Utf8 k)] -> return $ toS k
            _ -> internalError "doKeys: The impossible happened."

    pb <- use bsPendingBlock
    mptx <- use bsPendingTx

    let memKeys = map (toS . _deltaRowKey) $
                  collect pb ++ maybe [] collect mptx

    let allKeys = map fromString $
                  HashSet.toList $
                  HashSet.fromList $
                  dbKeys ++ memKeys
    return allKeys

  where
    tn = domainTableName d
    tnS = toS tn
    collect (_, m) =
        let flt k _ = _dkTable k == tnS
        in concat $ HashMap.elems $ HashMap.filterWithKey flt m
{-# INLINE doKeys #-}

-- tid is non-inclusive lower bound for the search
doTxIds :: TableName -> TxId -> BlockHandler SQLiteEnv [TxId]
doTxIds (TableName tn) _tid@(TxId tid) = do
    rows <- callDb "doTxIds" $ \db ->
      qry db stmt
        [SInt (fromIntegral tid)]
        [RInt]
    dbOut <- forM rows $ \case
        [SInt tid'] -> return $ TxId (fromIntegral tid')
        _ -> internalError "doTxIds: the impossible happened"

    -- also collect from pending non-committed data
    pb <- use bsPendingBlock
    mptx <- use bsPendingTx

    -- uniquify txids before returning
    return $! Set.toList
           $! Set.fromList
           $ dbOut ++ collect pb ++ maybe [] collect mptx

  where
    stmt = "SELECT DISTINCT txid FROM [" <> Utf8 (toS tn) <> "] WHERE txid > ?"

    tnS = toS tn
    collect (_, m) = let flt k _ = _dkTable k == tnS
                         txids = map _deltaTxId $
                                 concat $
                                 HashMap.elems $
                                 HashMap.filterWithKey flt m
                     in filter (> _tid) txids
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

modifyPendingData :: (SQLitePendingData -> SQLitePendingData)
              -> BlockHandler SQLiteEnv ()
modifyPendingData f = do
    m <- use bsPendingTx
    case m of
      Just d -> bsPendingTx .= Just (f d)
      Nothing -> bsPendingBlock %= f

doCreateUserTable :: TableName -> ModuleName -> BlockHandler SQLiteEnv ()
doCreateUserTable tn@(TableName ttxt) mn = do
    -- TODO: perform createUserTable at save
    -- bh <- gets _bsBlockHeight
    void $ fail "FIXME"
    createUserTable tn 0
    -- createUserTable tn bh
    modifyPendingData $ \(c, w) -> (HashSet.insert (T.encodeUtf8 ttxt) c, w)
    bsTxRecord %= M.insertWith (flip (++)) "SYS:usertables" txlogs
  where
    stn = asString tn
    uti = UserTableInfo mn
    txlogs = [TxLog "SYS:usertables" stn (toJSON uti)]
{-# INLINE doCreateUserTable #-}

doRollback :: BlockHandler SQLiteEnv ()
doRollback = bsPendingTx .= Nothing

doCommit :: BlockHandler SQLiteEnv [TxLog Value]
doCommit = use bsMode >>= \mm -> case mm of
    Nothing -> doRollback >> internalError "doCommit: Not in transaction"
    Just m -> do
        txrs <- use bsTxRecord
        if m == Transactional
          then do
              modify' (over bsTxId succ)
              -- merge pending tx into block data
              pending <- use bsPendingTx
              modify' (over bsPendingBlock $ merge pending)
              bsPendingTx .= Nothing
              resetTemp
          else doRollback
        return $! concat txrs
  where
    merge Nothing a = a
    merge (Just (creationsA, writesA)) (creationsB, writesB) =
        let creations = HashSet.union creationsA creationsB
            writes = HashMap.unionWith (flip (++)) writesA writesB
        in (creations, writes)
{-# INLINE doCommit #-}

doBegin :: ExecutionMode -> BlockHandler SQLiteEnv (Maybe TxId)
doBegin m = do
    use bsMode >>= \m' -> case m' of
        Just {} -> do
            logError "beginTx: In transaction, rolling back"
            doRollback
        Nothing -> return ()
    resetTemp
    bsMode .= Just m
    bsPendingTx .= Just (mempty, mempty)
    case m of
        Transactional -> Just <$> use bsTxId
        Local -> pure Nothing
{-# INLINE doBegin #-}

resetTemp :: BlockHandler SQLiteEnv ()
resetTemp = bsMode  .= Nothing >> bsTxRecord .= M.empty

doGetTxLog :: FromJSON v => Domain k v -> TxId -> BlockHandler SQLiteEnv [TxLog v]
doGetTxLog d txid = do
    -- try to look up this tx from pending log -- if we find it there it can't
    -- possibly be in the db.
    p <- readFromPending
    if null p then readFromDb else return p

  where
    readFromPending = do
        (_, pendingWrites) <- use bsPendingBlock
        let deltas = concat $ HashMap.elems pendingWrites
        let ourDeltas = filter (\delta -> _deltaTxId delta == txid) deltas
        mapM (\x -> toTxLog (Utf8 $ _deltaRowKey x) (_deltaData x)) ourDeltas

    readFromDb = do
        bh <- gets _bsBlockHeight
        rows <- callDb "doGetTxLog" $ \db -> qry db stmt
          [ SInt (fromIntegral txid)
          , SInt (fromIntegral bh)
          ]
          [RText, RBlob]
        forM rows $ \case
            [SText key, SBlob value] -> toTxLog key value
            err -> internalError $
              "doGetTxLog: Expected single row with two columns as the \
              \result, got: " <> T.pack (show err)

    toTxLog key value =
        case Data.Aeson.decodeStrict' value of
            Nothing -> internalError $
              "doGetTxLog: Unexpected value, unable to deserialize log"
            Just v ->
              return $! TxLog (toS $ unwrap $ domainTableName d) (toS $ unwrap key) v

    stmt = mconcat [ "SELECT rowkey, rowdata FROM ["
                , domainTableName d
                , "] WHERE txid = ? AND blockheight = ?"
                ]

unwrap :: Utf8 -> BS.ByteString
unwrap (Utf8 str) = str

blockHistoryInsert :: BlockHeight -> BlockHash -> TxId -> BlockHandler SQLiteEnv ()
blockHistoryInsert bh hsh t =
    callDb "blockHistoryInsert" $ \db ->
        exec' db stmt [ SInt (fromIntegral bh)
                   , SBlob (Data.Serialize.encode hsh)
                   , SInt (fromIntegral t) ]
  where
    stmt =
      "INSERT INTO BlockHistory ('blockheight','hash','endingtxid') \
            \ VALUES (?,?,?);"

createBlockHistoryTable :: BlockHandler SQLiteEnv ()
createBlockHistoryTable =
    callDb "createBlockHistoryTable" $ \db -> exec_ db
        "CREATE TABLE IF NOT EXISTS BlockHistory \
        \(blockheight UNSIGNED BIGINT NOT NULL,\
        \ hash BLOB NOT NULL,\
        \ endingtxid UNSIGNED BIGINT NOT NULL, \
        \ CONSTRAINT blockHashConstraint UNIQUE (blockheight));"

createTableCreationTable :: BlockHandler SQLiteEnv ()
createTableCreationTable =
    callDb "createTableCreationTable" $ \db -> exec_ db
      "CREATE TABLE IF NOT EXISTS VersionedTableCreation\
      \(tablename TEXT NOT NULL\
      \, createBlockheight UNSIGNED BIGINT NOT NULL\
      \, CONSTRAINT creation_unique UNIQUE(tablename, createBlockheight));"

createTableMutationTable :: BlockHandler SQLiteEnv ()
createTableMutationTable =
    callDb "createTableMutationTable" $ \db -> do
        exec_ db "CREATE TABLE IF NOT EXISTS VersionedTableMutation\
                 \(tablename TEXT NOT NULL\
                 \, blockheight UNSIGNED BIGINT NOT NULL\
                 \, CONSTRAINT mutation_unique UNIQUE(tablename,blockheight));"
        exec_ db "CREATE INDEX IF NOT EXISTS mutation_bh ON VersionedTableMutation(blockheight);"

createUserTable :: TableName -> BlockHeight -> BlockHandler SQLiteEnv ()
createUserTable name bh =
    callDb "createUserTable" $ \db -> do
        createVersionedTable tablename db
        exec' db insertstmt insertargs
  where
    insertstmt = "INSERT OR IGNORE INTO VersionedTableCreation VALUES (?,?)"
    insertargs =  [SText tablename, SInt (fromIntegral bh)]
    tablename = Utf8 (toS $ asString name)

createVersionedTable :: Utf8 -> Database -> IO ()
createVersionedTable tablename db = do
    exec_ db createtablestmt
    exec_ db indexcreationstmt
  where
    createtablestmt =
      "CREATE TABLE IF NOT EXISTS["
        <> tablename
        <> "] (rowkey TEXT\
             \, blockheight UNSIGNED BIGINT NOT NULL\
             \, txid UNSIGNED BIGINT NOT NULL\
             \, rowdata BLOB NOT NULL\
             \, UNIQUE (blockheight, rowkey, txid));"
    indexcreationstmt =
       mconcat
           ["CREATE INDEX IF NOT EXISTS ["
           , tablename
           , "_ix] ON ["
           , tablename
           , "](rowkey, blockheight, txid);"]

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
                   \blockheight = ? AND hash = ?;"
                   [ SInt $! fromIntegral $ pred bRestore
                   , SBlob (Data.Serialize.encode hsh) ]
                   [RInt]
            >>= expectSingleRowCol "handlePossibleRewind: (historyInvariant):"
        when (historyInvariant /= SInt 1) $
          internalError "handlePossibleRewind: History invariant violation"

    newChildBlock bCurrent = do
        assign bsBlockHeight bRestore
        SInt txid <- callDb "getting txid" $ \db ->
          expectSingleRowCol msg =<< qry db
              "SELECT endingtxid FROM BlockHistory WHERE blockheight = ?;"
              [SInt (fromIntegral bCurrent)]
              [RInt]
        assign bsTxId (fromIntegral txid)
        return $ fromIntegral txid
      where msg = "handlePossibleRewind: newChildBlock: error finding txid"

    rewindBlock bh = do
        assign bsBlockHeight bh
        tableMaintenanceRowsVersionedSystemTables
        callDb "rewindBlock" $ \db -> do
            droppedtbls <- dropTablesAtRewind bh db
            vacuumTablesAtRewind bh droppedtbls db
        t <- deleteHistory
        assign bsTxId t
        return $! t

dropTablesAtRewind :: BlockHeight -> Database -> IO (HashSet BS.ByteString)
dropTablesAtRewind bh db = do
    toDropTblNames <- qry db findTablesToDropStmt
                      [SInt (fromIntegral bh)] [RText]
    tbls <- fmap (HashSet.fromList) . forM toDropTblNames $ \case
        [SText tblname@(Utf8 tbl)] -> do
            exec_ db $ "DROP TABLE " <> tblname <> ";"
            return tbl
        _ -> internalError rewindmsg
    exec' db
        "DELETE FROM VersionedTableCreation WHERE createBlockheight >= ?"
        [SInt (fromIntegral bh)]
    return tbls
  where findTablesToDropStmt =
          "SELECT tablename FROM\
          \ VersionedTableCreation\
          \ WHERE createBlockheight >= ?;"
        rewindmsg =
          "rewindBlock:\
          \ dropTablesAtRewind: \
          \Couldn't resolve the name of the table to drop."

vacuumTablesAtRewind :: BlockHeight -> HashSet BS.ByteString -> Database -> IO ()
vacuumTablesAtRewind bh droppedtbls db = do
    let processMutatedTables ms = fmap (HashSet.fromList) . forM ms $ \case
          [SText (Utf8 tbl)] -> return tbl
          _ -> internalError "rewindBlock: Couldn't resolve the name of the table to possibly vacuum."
    mutatedTables <- qry db
        "SELECT DISTINCT tablename\
        \ FROM VersionedTableMutation WHERE blockheight >= ?;"
      [SInt (fromIntegral bh)]
      [RText]
      >>= processMutatedTables

    let toVacuumTblNames = HashSet.difference mutatedTables droppedtbls

    forM_ toVacuumTblNames $ \tblname ->
      exec' db ("DELETE FROM [" <> (Utf8 tblname) <> "] WHERE blockheight >= ?") [SInt (fromIntegral bh)]

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
    create (domainTableName KeySets)
    create (domainTableName Modules)
    create (domainTableName Namespaces)
    create (domainTableName Pacts)
  where
    create tablename = do
        log "DDL" $ "initSchema: "  ++ toS tablename
        callDb "initSchema" $ createVersionedTable tablename
