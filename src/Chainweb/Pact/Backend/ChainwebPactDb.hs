{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

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
, indexPactTransaction
, indexPendingPactTransactions
, clearPendingTxState
, backendWriteUpdateBatch
, createUserTable
, vacuumDb
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Catch (throwM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.Aeson hiding ((.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.DList as DL
import Data.Foldable (concat, toList)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Serialize
import qualified Data.Set as Set
import Data.String
import Data.String.Conv
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

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
import Chainweb.Pact.Service.Types (internalError, PactException(..))

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

getPendingData :: BlockHandler SQLiteEnv [SQLitePendingData]
getPendingData = do
    pb <- use bsPendingBlock
    ptx <- maybe [] (:[]) <$> use bsPendingTx
    -- lookup in pending transactions first
    return $ ptx ++ [pb]

doReadRow
    :: (IsString k, FromJSON v)
    => Domain k v
    -> k
    -> BlockHandler SQLiteEnv (Maybe v)
doReadRow d k =
    case d of
        KeySets -> lookupWithKey (convKeySetName k)
        -- TODO: This is incomplete (the modules case), due to namespace
        -- resolution concerns
        Modules -> lookupWithKey (convModuleName k)
        Namespaces -> lookupWithKey (convNamespaceName k)
        (UserTables _) -> lookupWithKey (convRowKey k)
        Pacts -> lookupWithKey (convPactId k)
  where
    tableName = domainTableName d
    (Utf8 tableNameBS) = tableName

    queryStmt = mconcat [ "SELECT rowdata FROM ["
                        , tableName
                        , "]  WHERE rowkey = ? \
                          \ ORDER BY txid DESC \
                          \ LIMIT 1;"
                        ]

    lookupWithKey :: forall v . FromJSON v => Utf8 -> BlockHandler SQLiteEnv (Maybe v)
    lookupWithKey key = do
        pds <- getPendingData
        let lookPD = foldr1 (<|>) $ map (lookupInPendingData key) pds
        let lookDB = lookupInDb key
        runMaybeT (lookPD <|> lookDB)

    lookupInPendingData
        :: forall v . FromJSON v
        => Utf8
        -> SQLitePendingData
        -> MaybeT (BlockHandler SQLiteEnv) v
    lookupInPendingData (Utf8 rowkey) p = do
        let deltaKey = SQLiteDeltaKey tableNameBS rowkey
        ddata <- fmap _deltaData <$> MaybeT (return $ HashMap.lookup deltaKey (_pendingWrites p))
        if null ddata
            -- should be impossible, but we'll check this case
            then mzero
            -- we merge with (++) which should produce txids most-recent-first
            -- -- we care about the most recent update to this rowkey
            else MaybeT $ return $! decode $ fromStrict $ DL.head ddata

    lookupInDb :: forall v . FromJSON v => Utf8 -> MaybeT (BlockHandler SQLiteEnv) v
    lookupInDb rowkey = do
        -- First, check: did we create this table during this block? If so,
        -- there's no point in looking up the key.
        checkDbTableExists tableName
        result <- lift $ callDb "doReadRow"
                       $ \db -> qry db queryStmt [SText rowkey] [RBlob]
        case result of
            [] -> mzero
            [[SBlob a]] -> MaybeT $ return $! decode $ fromStrict a
            err -> internalError $
                     "doReadRow: Expected (at most) a single result, but got: " <>
                     T.pack (show err)

checkDbTableExists :: Utf8 -> MaybeT (BlockHandler SQLiteEnv) ()
checkDbTableExists tableName = do
    pds <- lift getPendingData
    forM_ pds $ \p ->
        when (HashSet.member tableNameBS (_pendingTableCreation p)) mzero
  where
    (Utf8 tableNameBS) = tableName

writeSys
    :: (AsString k, ToJSON v)
    => Domain k v
    -> k
    -> v
    -> BlockHandler SQLiteEnv ()
writeSys d k v = gets _bsTxId >>= go
  where
    go txid = do
        recordPendingUpdate keyStr tableName txid v
        recordTxLog (toTableName tableName) d k v

    keyStr = getKeyString k
    toTableName (Utf8 str) = TableName $ toS str
    tableName = domainTableName d

    getKeyString = case d of
        KeySets -> convKeySetName
        Modules -> convModuleName
        Namespaces -> convNamespaceName
        Pacts -> convPactId
        UserTables _ -> error "impossible"

recordPendingUpdate
    :: ToJSON v
    => Utf8
    -> Utf8
    -> TxId
    -> v
    -> BlockHandler SQLiteEnv ()
recordPendingUpdate (Utf8 key) (Utf8 tn) txid v = modifyPendingData modf
  where
    !vs = toStrict (Data.Aeson.encode v)
    delta = SQLiteRowDelta tn txid key vs
    deltaKey = SQLiteDeltaKey tn key

    modf = over pendingWrites upd
    {- "const" is used here because prefer the latest update of the rowkey for
    the current transaction  -}
    upd = HashMap.insertWith const deltaKey (DL.singleton delta)


backendWriteUpdateBatch
    :: BlockHeight
    -> [(Utf8, V.Vector SQLiteRowDelta)]    -- ^ updates chunked on table name
    -> Database
    -> IO ()
backendWriteUpdateBatch bh writesByTable db = mapM_ writeTable writesByTable
  where
    prepRow (SQLiteRowDelta _ txid rowkey rowdata) =
        [ SText (Utf8 rowkey)
        , SInt (fromIntegral txid)
        , SBlob rowdata ]

    writeTable (tableName, writes) = do
        execMulti db q (V.toList $ V.map prepRow writes)
        markTableMutation tableName bh db
      where
        q = mconcat
          ["INSERT OR REPLACE INTO ["
          , tableName
          , "](rowkey,txid,rowdata) "
          , "VALUES(?,?,?)"]


markTableMutation :: Utf8 -> BlockHeight -> Database -> IO ()
markTableMutation tablename blockheight db = do
    exec' db mutq [SText tablename, SInt (fromIntegral blockheight)]
  where
    mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"

checkInsertIsOK
    :: WriteType
    -> Domain RowKey (ObjectMap PactValue)
    -> RowKey
    -> BlockHandler SQLiteEnv (Maybe (ObjectMap PactValue))
checkInsertIsOK wt d k = do
    olds <- doReadRow d k
    case (olds, wt) of
        (Nothing, Insert) -> return Nothing
        (Just _, Insert) -> err "Insert: row found for key "
        (Nothing, Write) -> return Nothing
        (Just old, Write) -> return $ Just old
        (Just old, Update) -> return $ Just old
        (Nothing, Update) -> err "Update: no row found for key "
  where
    err msg = internalError $ "checkInsertIsOK: " <> msg <> asString k

writeUser
    :: WriteType
    -> Domain RowKey (ObjectMap PactValue)
    -> RowKey
    -> ObjectMap PactValue
    -> BlockHandler SQLiteEnv ()
writeUser wt d k row = gets _bsTxId >>= go
  where
    toTableName (Utf8 str) = TableName $ toS str
    tn = domainTableName d
    ttn = toTableName tn

    go txid = do
        m <- checkInsertIsOK wt d k
        row' <- case m of
                    Nothing -> ins
                    (Just old) -> upd old
        recordTxLog ttn d k row'

      where
        upd oldrow = do
            let row' = ObjectMap (M.union (_objectMap row) (_objectMap oldrow))
            recordPendingUpdate (Utf8 $! toS $ asString k) tn txid row'
            return row'

        ins = do
            recordPendingUpdate (Utf8 $! toS $ asString k) tn txid row
            return row

doWriteRow
  :: (AsString k, ToJSON v)
    => WriteType
    -> Domain k v
    -> k
    -> v
    -> BlockHandler SQLiteEnv ()
doWriteRow wt d k v = case d of
    (UserTables _) -> writeUser wt d k v
    _ -> writeSys d k v

doKeys
    :: (IsString k, AsString k)
    => Domain k v
    -> BlockHandler SQLiteEnv [k]
doKeys d = do
    dbKeys <- getDbKeys
    pb <- use bsPendingBlock
    mptx <- use bsPendingTx

    let memKeys = DL.toList $
                  fmap (toS . _deltaRowKey) $
                  collect pb `DL.append` maybe DL.empty collect mptx

    let allKeys = map fromString $
                  HashSet.toList $
                  HashSet.fromList $
                  dbKeys ++ memKeys
    return allKeys

  where
    getDbKeys = do
        m <- runMaybeT $ checkDbTableExists $ Utf8 tnS
        case m of
            Nothing -> return mempty
            Just () -> do
                ks <- callDb "doKeys" $ \db ->
                          qry_ db  ("SELECT DISTINCT rowkey FROM ["
                                    <> tn <> "];") [RText]
                forM ks $ \row -> do
                    case row of
                        [SText (Utf8 k)] -> return $! toS k
                        _ -> internalError "doKeys: The impossible happened."

    tn = domainTableName d
    tnS = toS tn
    collect p =
        let flt k _ = _dkTable k == tnS
        in DL.concat $ HashMap.elems $ HashMap.filterWithKey flt (_pendingWrites p)
{-# INLINE doKeys #-}

-- tid is non-inclusive lower bound for the search
doTxIds :: TableName -> TxId -> BlockHandler SQLiteEnv [TxId]
doTxIds (TableName tn) _tid@(TxId tid) = do
    dbOut <- getFromDb

    -- also collect from pending non-committed data
    pb <- use bsPendingBlock
    mptx <- use bsPendingTx

    -- uniquify txids before returning
    return $ Set.toList
           $! Set.fromList
           $ dbOut ++ collect pb ++ maybe [] collect mptx

  where
    getFromDb = do
        m <- runMaybeT $ checkDbTableExists $ Utf8 tnS
        case m of
            Nothing -> return mempty
            Just () -> do
                rows <- callDb "doTxIds" $ \db ->
                  qry db stmt
                    [SInt (fromIntegral tid)]
                    [RInt]
                forM rows $ \case
                    [SInt tid'] -> return $ TxId (fromIntegral tid')
                    _ -> internalError "doTxIds: the impossible happened"

    stmt = "SELECT DISTINCT txid FROM [" <> Utf8 (toS tn) <> "] WHERE txid > ?"

    tnS = toS tn
    collect p =
        let flt k _ = _dkTable k == tnS
            txids = DL.toList $
                    fmap _deltaTxId $
                    DL.concat $
                    HashMap.elems $
                    HashMap.filterWithKey flt (_pendingWrites p)
        in filter (> _tid) txids
{-# INLINE doTxIds #-}

recordTxLog
    :: (AsString k, ToJSON v)
    => TableName
    -> Domain k v
    -> k
    -> v
    -> BlockHandler SQLiteEnv ()
recordTxLog tt d k v = do
    -- are we in a tx?
    mptx <- use bsPendingTx
    case mptx of
      Nothing -> bsPendingBlock . pendingTxLogMap %= upd
      (Just _) -> bsPendingTx . _Just . pendingTxLogMap %= upd

  where
    upd = M.insertWith DL.append tt txlogs
    txlogs = DL.singleton (TxLog (asString d) (asString k) (toJSON v))

modifyPendingData
    :: (SQLitePendingData -> SQLitePendingData)
    -> BlockHandler SQLiteEnv ()
modifyPendingData f = do
    m <- use bsPendingTx
    case m of
      Just d -> bsPendingTx .= Just (f d)
      Nothing -> bsPendingBlock %= f

doCreateUserTable :: TableName -> ModuleName -> BlockHandler SQLiteEnv ()
doCreateUserTable tn@(TableName ttxt) mn = do
    -- first check if tablename already exists in pending queues
    m <- runMaybeT $ checkDbTableExists (Utf8 $ T.encodeUtf8 ttxt)
    case m of
      Nothing -> throwM $ PactDuplicateTableError ttxt
      Just () -> do
          -- then check if it is in the db
          cond <- inDb $ Utf8 $ T.encodeUtf8 ttxt
          when cond $ throwM $ PactDuplicateTableError ttxt
          modifyPendingData
            $ over pendingTableCreation (HashSet.insert (T.encodeUtf8 ttxt))
            . over pendingTxLogMap (M.insertWith DL.append (TableName txlogKey) txlogs)
  where
    inDb t =
      callDb "doCreateUserTable" $ \db -> do
        r <- qry db tableLookupStmt [SText t] [RText]
        return $ case r of
          [[SText rname]] -> rname == t
          _ -> False

    tableLookupStmt = "SELECT name FROM sqlite_master WHERE type='table' and name=?;"
    txlogKey = "SYS:usertables"
    stn = asString tn
    uti = UserTableInfo mn
    txlogs = DL.singleton (TxLog txlogKey stn (toJSON uti))
{-# INLINE doCreateUserTable #-}

doRollback :: BlockHandler SQLiteEnv ()
doRollback = do
  bsMode .= Nothing
  bsPendingTx .= Nothing

doCommit :: BlockHandler SQLiteEnv [TxLog Value]
doCommit = use bsMode >>= \mm -> case mm of
    Nothing -> doRollback >> internalError "doCommit: Not in transaction"
    Just m -> do
        txrs <- if m == Transactional
          then do
              modify' (over bsTxId succ)
              -- merge pending tx into block data
              pending <- use bsPendingTx
              bsPendingBlock %= merge pending
              blockLogs <- use $ bsPendingBlock . pendingTxLogMap
              bsPendingTx .= Nothing
              resetTemp
              return blockLogs
          else doRollback >> return mempty
        return $! concat $ fmap (reverse . DL.toList) txrs
  where
    merge Nothing a = a
    merge (Just a) b = SQLitePendingData
        { _pendingTableCreation = HashSet.union (_pendingTableCreation a) (_pendingTableCreation b)
        , _pendingWrites = HashMap.unionWith mergeW  (_pendingWrites a) (_pendingWrites b)
        , _pendingTxLogMap = _pendingTxLogMap a
        , _pendingSuccessfulTxs = _pendingSuccessfulTxs b
        }

    mergeW a b = case take 1 (DL.toList a) of
        [] -> b
        (x:_) -> DL.cons x b

{-# INLINE doCommit #-}

clearPendingTxState :: BlockHandler SQLiteEnv ()
clearPendingTxState = do
    bsPendingBlock .= emptySQLitePendingData
    bsPendingTx .= Nothing
    resetTemp

doBegin :: ExecutionMode -> BlockHandler SQLiteEnv (Maybe TxId)
doBegin m = do
    use bsMode >>= \m' -> case m' of
        Just {} -> do
            logError "beginTx: In transaction, rolling back"
            doRollback
        Nothing -> return ()
    resetTemp
    bsMode .= Just m
    bsPendingTx .= Just emptySQLitePendingData
    case m of
        Transactional -> Just <$> use bsTxId
        Local -> pure Nothing
{-# INLINE doBegin #-}

resetTemp :: BlockHandler SQLiteEnv ()
resetTemp = do
    bsMode .= Nothing
    -- clear out txlog entries
    bsPendingBlock . pendingTxLogMap .= mempty

doGetTxLog :: FromJSON v => Domain k v -> TxId -> BlockHandler SQLiteEnv [TxLog v]
doGetTxLog d txid = do
    -- try to look up this tx from pending log -- if we find it there it can't
    -- possibly be in the db.
    p <- readFromPending
    if null p then readFromDb else return p

  where
    predicate delta = _deltaTxId delta == txid &&
                      _deltaTableName delta == tableNameBS

    tableName = domainTableName d
    (Utf8 tableNameBS) = tableName

    takeHead [] = []
    takeHead (a:_) = [a]

    readFromPending = do
        ptx <- maybe [] (:[]) <$> use bsPendingTx
        pb <- use bsPendingBlock
        let deltas = concat $
                map (takeHead . DL.toList) $
                concatMap HashMap.elems $ _pendingWrites <$> (ptx ++ [pb])
        let ourDeltas = filter predicate deltas
        mapM (\x -> toTxLog (Utf8 $ _deltaRowKey x) (_deltaData x)) ourDeltas

    readFromDb = do
        rows <- callDb "doGetTxLog" $ \db -> qry db stmt
          [ SInt (fromIntegral txid) ]
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
                , tableName
                , "] WHERE txid = ?"
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

createTransactionIndexTable :: BlockHandler SQLiteEnv ()
createTransactionIndexTable = callDb "createTransactionIndexTable" $ \db -> do
    exec_ db "CREATE TABLE IF NOT EXISTS TransactionIndex \
             \ (txhash BLOB NOT NULL, \
             \ blockheight UNSIGNED BIGINT NOT NULL, \
             \ CONSTRAINT transactionIndexConstraint UNIQUE(txhash));"
    exec_ db "CREATE INDEX IF NOT EXISTS \
             \ transactionIndexByBH ON TransactionIndex(blockheight)";

indexPactTransaction :: ByteString -> BlockHandler SQLiteEnv ()
indexPactTransaction h = bsPendingBlock . pendingSuccessfulTxs %= HashSet.insert h


indexPendingPactTransactions :: BlockHandler SQLiteEnv ()
indexPendingPactTransactions = do
    txs <- _pendingSuccessfulTxs <$> gets _bsPendingBlock
    dbIndexTransactions txs

  where
    toRow bh b = [SBlob b, SInt bh]
    dbIndexTransactions txs = do
        bh <- fromIntegral <$> gets _bsBlockHeight
        let rows = map (toRow bh) $ toList txs
        callDb "dbIndexTransactions" $ \db -> do
            execMulti db "INSERT INTO TransactionIndex (txhash, blockheight) \
                         \ VALUES (?, ?)" rows

clearTxIndex :: BlockHandler SQLiteEnv ()
clearTxIndex = do
    bh <- gets _bsBlockHeight
    callDb "clearTxIndex" $ \db -> do
        exec' db "DELETE FROM TransactionIndex WHERE blockheight >= ?;"
              [ SInt (fromIntegral bh) ]

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
      \, CONSTRAINT creation_unique UNIQUE(createBlockheight, tablename));"

createTableMutationTable :: BlockHandler SQLiteEnv ()
createTableMutationTable =
    callDb "createTableMutationTable" $ \db -> do
        exec_ db "CREATE TABLE IF NOT EXISTS VersionedTableMutation\
                 \(tablename TEXT NOT NULL\
                 \, blockheight UNSIGNED BIGINT NOT NULL\
                 \, CONSTRAINT mutation_unique UNIQUE(blockheight, tablename));"

createUserTable :: Utf8 -> BlockHeight -> BlockHandler SQLiteEnv ()
createUserTable tablename bh =
    callDb "createUserTable" $ \db -> do
        createVersionedTable tablename db
        exec' db insertstmt insertargs
  where
    insertstmt = "INSERT OR IGNORE INTO VersionedTableCreation VALUES (?,?)"
    insertargs =  [SText tablename, SInt (fromIntegral bh)]

createVersionedTable :: Utf8 -> Database -> IO ()
createVersionedTable tablename db = do
    exec_ db createtablestmt
    exec_ db indexcreationstmt
  where
    createtablestmt =
      "CREATE TABLE IF NOT EXISTS["
        <> tablename
        <> "] (rowkey TEXT\
             \, txid UNSIGNED BIGINT NOT NULL\
             \, rowdata BLOB NOT NULL\
             \, UNIQUE (rowkey, txid));"
    indexcreationstmt =
       mconcat
           ["CREATE INDEX IF NOT EXISTS ["
           , tablename
           , "_ix] ON ["
           , tablename
           , "](txid DESC);"]

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
        endingtx <- getEndingTxId bh
        tableMaintenanceRowsVersionedSystemTables endingtx
        callDb "rewindBlock" $ \db -> do
            droppedtbls <- dropTablesAtRewind bh db
            vacuumTablesAtRewind bh endingtx droppedtbls db
        deleteHistory bh
        assign bsTxId endingtx
        clearTxIndex
        return endingtx

dropTablesAtRewind :: BlockHeight -> Database -> IO (HashSet BS.ByteString)
dropTablesAtRewind bh db = do
    toDropTblNames <- qry db findTablesToDropStmt
                      [SInt (fromIntegral bh)] [RText]
    tbls <- fmap (HashSet.fromList) . forM toDropTblNames $ \case
        [SText tblname@(Utf8 tbl)] -> do
            exec_ db $ "DROP TABLE [" <> tblname <> "];"
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

vacuumTablesAtRewind :: BlockHeight -> TxId -> HashSet BS.ByteString -> Database -> IO ()
vacuumTablesAtRewind bh endingtx droppedtbls db = do
    let processMutatedTables ms = fmap (HashSet.fromList) . forM ms $ \case
          [SText (Utf8 tbl)] -> return tbl
          _ -> internalError "rewindBlock: Couldn't resolve the name \
                             \of the table to possibly vacuum."
    mutatedTables <- qry db
        "SELECT DISTINCT tablename\
        \ FROM VersionedTableMutation WHERE blockheight >= ?;"
      [SInt (fromIntegral bh)]
      [RText]
      >>= processMutatedTables
    let toVacuumTblNames = HashSet.difference mutatedTables droppedtbls
    forM_ toVacuumTblNames $ \tblname ->
        exec' db (mconcat ["DELETE FROM [", Utf8 tblname, "] WHERE txid >= ?"])
              [SInt $! fromIntegral endingtx]
    exec' db "DELETE FROM VersionedTableMutation WHERE blockheight >= ?;"
          [SInt (fromIntegral bh)]

tableMaintenanceRowsVersionedSystemTables :: TxId -> BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedSystemTables endingtx = do
    callDb "tableMaintenanceRowsVersionedSystemTables" $ \db -> do
        exec' db "DELETE FROM [SYS:KeySets] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Modules] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Namespaces] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Pacts] WHERE txid >= ?" tx
  where
    tx = [SInt $! fromIntegral endingtx]

deleteHistory :: BlockHeight -> BlockHandler SQLiteEnv ()
deleteHistory bh = do
    callDb "Deleting from BlockHistory, VersionHistory" $ \db -> do
        exec' db "DELETE FROM BlockHistory WHERE blockheight >= ?"
              [SInt (fromIntegral bh)]

initSchema :: BlockHandler SQLiteEnv ()
initSchema = do
    withSavepoint DbTransaction $ do
        createBlockHistoryTable
        createTableCreationTable
        createTableMutationTable
        createTransactionIndexTable
        create (domainTableName KeySets)
        create (domainTableName Modules)
        create (domainTableName Namespaces)
        create (domainTableName Pacts)
  where
    create tablename = do
        log "DDL" $ "initSchema: "  ++ toS tablename
        callDb "initSchema" $ createVersionedTable tablename

getEndingTxId :: BlockHeight -> BlockHandler SQLiteEnv TxId
getEndingTxId bh = callDb "getEndingTxId" $ \db -> do
    if bh == 0
      then return 0
      else
        qry db "SELECT endingtxid FROM BlockHistory where blockheight = ?"
            [SInt (fromIntegral $ pred bh)]
            [RInt]
          >>= fmap convertInt . expectSingleRowCol "endingtxid for block"
  where
    convertInt (SInt thing) = fromIntegral thing
    convertInt _ = error "impossible"

-- Careful doing this! It's expensive and for our use case, probably pointless.
-- We should reserve vacuuming for an offline process
vacuumDb :: BlockHandler SQLiteEnv ()
vacuumDb = callDb "vaccumDb" (`exec_` "VACUUM;")
