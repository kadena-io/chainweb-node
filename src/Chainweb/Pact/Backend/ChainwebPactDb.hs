{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Chainweb.Pact.Backend.ChainwebPactDb
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb
( chainwebPactDb
, rewoundPactDb
, rewindDbTo
, rewindDbToBlock
, commitBlockStateToDatabase
, initSchema
, indexPactTransaction
, vacuumDb
, toTxLog
, getEndTxId
, getEndTxId'
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.Aeson hiding ((.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.DList as DL
import Data.Foldable (toList)
import Data.List(sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.SQLite3.Direct as SQ3

import GHC.Stack

import Prelude hiding (concat, log)

-- pact

import Pact.Persist
import Pact.PersistPactDb hiding (db)
import Pact.Types.Persistence
import Pact.Types.RowData
import Pact.Types.SQLite
import Pact.Types.Term (ModuleName(..), ObjectMap(..), TableName(..))
import Pact.Types.Util (AsString(..))

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Legacy.HashMap as LHM

-- chainweb

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types (logInfo_, logError_)
import Chainweb.Utils
import Chainweb.Utils.Serialization

tbl :: HasCallStack => Utf8 -> Utf8
tbl t@(Utf8 b)
    | B8.elem ']' b = error $ "Chainweb.Pact.Backend.ChainwebPactDb: Code invariant violation. Illegal SQL table name " <> sshow b <> ". Please report this as a bug."
    | otherwise = "[" <> t <> "]"

-- | Pact DB which reads from the tip of the checkpointer
chainwebPactDb :: (Logger logger) => PactDb (BlockEnv logger)
chainwebPactDb = PactDb
    { _readRow = \d k e -> runBlockEnv e $ doReadRow Nothing d k
    , _writeRow = \wt d k v e -> runBlockEnv e $ doWriteRow Nothing wt d k v
    , _keys = \d e -> runBlockEnv e $ doKeys Nothing d
    , _txids = \t txid e -> runBlockEnv e $ doTxIds t txid
    , _createUserTable = \tn mn e -> runBlockEnv e $ doCreateUserTable Nothing tn mn
    , _getUserTableInfo = \_ -> error "WILL BE DEPRECATED!"
    , _beginTx = \m e -> runBlockEnv e $ doBegin m
    , _commitTx = \e -> runBlockEnv e doCommit
    , _rollbackTx = \e -> runBlockEnv e doRollback
    , _getTxLog = \d tid e -> runBlockEnv e $ doGetTxLog d tid
    }

-- | Pact DB which reads from some past block height, instead of the tip of the checkpointer
rewoundPactDb :: (Logger logger) => BlockHeight -> TxId -> PactDb (BlockEnv logger)
rewoundPactDb bh endTxId = chainwebPactDb
    { _readRow = \d k e -> runBlockEnv e $ doReadRow (Just (bh, endTxId)) d k
    , _writeRow = \wt d k v e -> runBlockEnv e $ doWriteRow (Just (bh, endTxId)) wt d k v
    , _keys = \d e -> runBlockEnv e $ doKeys (Just (bh, endTxId)) d
    , _createUserTable = \tn mn e -> runBlockEnv e $ doCreateUserTable (Just bh) tn mn
    }

-- returns pending writes in the reverse order they were made
getPendingData :: BlockHandler logger [SQLitePendingData]
getPendingData = do
    pb <- use bsPendingBlock
    ptx <- maybeToList <$> use bsPendingTx
    -- lookup in pending transactions first
    return $ ptx ++ [pb]

forModuleNameFix :: (Bool -> BlockHandler logger a) -> BlockHandler logger a
forModuleNameFix f = view blockHandlerModuleNameFix >>= f

-- TODO: speed this up, cache it?
tableExistsInDbAtHeight :: Utf8 -> BlockHeight -> BlockHandler logger Bool
tableExistsInDbAtHeight tableName bh = do
    let knownTbls =
          ["SYS:Pacts", "SYS:Modules", "SYS:KeySets", "SYS:Namespaces"]
    if tableName `elem` knownTbls
    then return True
    else callDb "tableExists" $ \db -> do
      let tableExistsStmt =
            -- table names are case-sensitive
            "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight < ? AND lower(tablename) = lower(?)"
      qry db tableExistsStmt [SInt $ max 0 (fromIntegral bh), SText tableName] [RText] >>= \case
        [] -> return False
        _ -> return True

doReadRow
    :: (IsString k, FromJSON v)
    => Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> Domain k v
    -> k
    -> BlockHandler logger (Maybe v)
doReadRow mlim d k = forModuleNameFix $ \mnFix ->
    case d of
        KeySets -> lookupWithKey (convKeySetName k) noCache
        -- TODO: This is incomplete (the modules case), due to namespace
        -- resolution concerns
        Modules -> lookupWithKey (convModuleName mnFix k) checkModuleCache
        Namespaces -> lookupWithKey (convNamespaceName k) noCache
        (UserTables _) -> lookupWithKey (convRowKey k) noCache
        Pacts -> lookupWithKey (convPactId k) noCache
  where
    tableName = domainTableName d
    (Utf8 tableNameBS) = tableName

    lookupWithKey
        :: forall logger v . FromJSON v
        => Utf8
        -> (Utf8 -> BS.ByteString -> MaybeT (BlockHandler logger ) v)
        -> BlockHandler logger (Maybe v)
    lookupWithKey key checkCache = do
        pds <- getPendingData
        let lookPD = foldr1 (<|>) $ map (lookupInPendingData key) pds
        let lookDB = lookupInDb key checkCache
        runMaybeT (lookPD <|> lookDB)

    lookupInPendingData
        :: forall logger v . FromJSON v
        => Utf8
        -> SQLitePendingData
        -> MaybeT (BlockHandler logger) v
    lookupInPendingData (Utf8 rowkey) p = do
        -- we get the latest-written value at this rowkey
        allKeys <- hoistMaybe $ HashMap.lookup tableNameBS (_pendingWrites p)
        ddata <- _deltaData . NE.head <$> hoistMaybe (HashMap.lookup rowkey allKeys)
        MaybeT $ return $! decodeStrict' ddata

    lookupInDb
        :: forall logger v . FromJSON v
        => Utf8
        -> (Utf8 -> BS.ByteString -> MaybeT (BlockHandler logger) v)
        -> MaybeT (BlockHandler logger) v
    lookupInDb rowkey checkCache = do
        -- First, check: did we create this table during this block? If so,
        -- there's no point in looking up the key.
        checkDbTablePendingCreation tableName
        lift $ forM_ mlim $ \(bh, _) ->
            failIfTableDoesNotExistInDbAtHeight "doReadRow" tableName bh
        -- we inject the endingtx limitation to reduce the scope up to the provided block height
        let blockLimitStmt = maybe "" (const " AND txid < ?") mlim
        let blockLimitParam = maybe [] (\(TxId txid) -> [SInt $ fromIntegral txid]) (snd <$> mlim)
        let queryStmt =
                "SELECT rowdata FROM " <> tbl tableName <> " WHERE rowkey = ?" <> blockLimitStmt
                <> " ORDER BY txid DESC LIMIT 1;"
        result <- lift $ callDb "doReadRow"
                       $ \db -> qry db queryStmt ([SText rowkey] ++ blockLimitParam) [RBlob]
        case result of
            [] -> mzero
            [[SBlob a]] -> checkCache rowkey a
            err -> internalError $
                     "doReadRow: Expected (at most) a single result, but got: " <>
                     T.pack (show err)

    checkModuleCache u b = MaybeT $ do
        !txid <- use bsTxId -- cache priority
        mc <- use bsModuleCache
        (r, mc') <- liftIO $ checkDbCache u b txid mc
        modify' (bsModuleCache .~ mc')
        return r

    noCache
        :: FromJSON v
        => Utf8
        -> BS.ByteString
        -> MaybeT (BlockHandler logger) v
    noCache _key rowdata = MaybeT $ return $! decodeStrict' rowdata


checkDbTablePendingCreation :: Utf8 -> MaybeT (BlockHandler logger) ()
checkDbTablePendingCreation tableName = do
    pds <- lift getPendingData
    forM_ pds $ \p ->
        when (HashSet.member tableNameBS (_pendingTableCreation p)) mzero
  where
    (Utf8 tableNameBS) = tableName

writeSys
    :: (AsString k, J.Encode v)
    => Domain k v
    -> k
    -> v
    -> BlockHandler logger ()
writeSys d k v = gets _bsTxId >>= go
  where
    go txid = do
        forModuleNameFix $ \mnFix ->
          recordPendingUpdate (getKeyString mnFix k) tableName txid v
        recordTxLog (toTableName tableName) d k v

    toTableName (Utf8 str) = TableName $ T.decodeUtf8 str
    tableName = domainTableName d

    getKeyString mnFix = case d of
        KeySets -> convKeySetName
        Modules -> convModuleName mnFix
        Namespaces -> convNamespaceName
        Pacts -> convPactId
        UserTables _ -> error "impossible"

recordPendingUpdate
    :: J.Encode v
    => Utf8
    -> Utf8
    -> TxId
    -> v
    -> BlockHandler logger ()
recordPendingUpdate (Utf8 key) (Utf8 tn) txid v = modifyPendingData modf
  where
    !vs = J.encodeStrict v
    delta = SQLiteRowDelta tn txid key vs

    modf = over pendingWrites upd
    upd = HashMap.unionWith
        HashMap.union
        (HashMap.singleton tn
            (HashMap.singleton key (NE.singleton delta)))


markTableMutation :: Utf8 -> BlockHeight -> Database -> IO ()
markTableMutation tablename blockheight db = do
    exec' db mutq [SText tablename, SInt (fromIntegral blockheight)]
  where
    mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"

checkInsertIsOK
    :: Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> WriteType
    -> Domain RowKey RowData
    -> RowKey
    -> BlockHandler logger (Maybe RowData)
checkInsertIsOK mlim wt d k = do
    olds <- doReadRow mlim d k
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
    :: Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> WriteType
    -> Domain RowKey RowData
    -> RowKey
    -> RowData
    -> BlockHandler logger ()
writeUser mlim wt d k rowdata@(RowData _ row) = gets _bsTxId >>= go
  where
    toTableName = TableName . fromUtf8
    tn = domainTableName d
    ttn = toTableName tn

    go txid = do
        m <- checkInsertIsOK mlim wt d k
        row' <- case m of
                    Nothing -> ins
                    (Just old) -> upd old
        recordTxLog ttn d k row'

      where
        upd (RowData oldV oldrow) = do
            let row' = RowData oldV $ ObjectMap (M.union (_objectMap row) (_objectMap oldrow))
            recordPendingUpdate (convRowKey k) tn txid row'
            return row'

        ins = do
            recordPendingUpdate (convRowKey k) tn txid rowdata
            return rowdata

doWriteRow
  :: (AsString k, J.Encode v)
    => Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> WriteType
    -> Domain k v
    -> k
    -> v
    -> BlockHandler logger ()
doWriteRow mlim wt d k v = case d of
    (UserTables _) -> writeUser mlim wt d k v
    _ -> writeSys d k v

doKeys
    :: (IsString k)
    => Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> Domain k v
    -> BlockHandler logger [k]
doKeys mlim d = do
    msort <- views blockHandlerSortedKeys (\c -> if c then sort else id)
    dbKeys <- getDbKeys
    pb <- use bsPendingBlock
    mptx <- use bsPendingTx

    let memKeys = fmap (B8.unpack . _deltaRowKey)
                  $ collect pb ++ maybe [] collect mptx

    let !allKeys = fmap fromString
                  $ msort -- becomes available with Pact42Upgrade
                  $ LHM.sort
                  $ dbKeys ++ memKeys
    return allKeys

  where
    blockLimitStmt = maybe "" (const " WHERE txid < ?;") mlim
    blockLimitParam = maybe [] (\(TxId txid) -> [SInt (fromIntegral txid)]) (snd <$> mlim)
    getDbKeys = do
        m <- runMaybeT $ checkDbTablePendingCreation $ Utf8 tnS
        case m of
            Nothing -> return mempty
            Just () -> do
                forM_ mlim (failIfTableDoesNotExistInDbAtHeight "doKeys" tn . fst)
                ks <- callDb "doKeys" $ \db ->
                          qry db ("SELECT DISTINCT rowkey FROM " <> tbl tn <> blockLimitStmt) blockLimitParam [RText]
                forM ks $ \row -> do
                    case row of
                        [SText k] -> return $! T.unpack $ fromUtf8 k
                        _ -> internalError "doKeys: The impossible happened."

    tn = domainTableName d
    tnS = let (Utf8 x) = tn in x
    collect p =
        concatMap NE.toList $ HashMap.elems $ fromMaybe mempty $ HashMap.lookup tnS (_pendingWrites p)
{-# INLINE doKeys #-}

failIfTableDoesNotExistInDbAtHeight
  :: Text -> Utf8 -> BlockHeight -> BlockHandler logger ()
failIfTableDoesNotExistInDbAtHeight caller tn bh = do
    exists <- tableExistsInDbAtHeight tn bh
    -- we must reproduce errors that were thrown in earlier blocks from tables
    -- not existing, if this table does not yet exist.
    unless exists $
        internalError $ "callDb (" <> caller <> "): user error (Database error: ErrorError)"

-- tid is non-inclusive lower bound for the search
doTxIds :: TableName -> TxId -> BlockHandler logger [TxId]
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
        m <- runMaybeT $ checkDbTablePendingCreation $ Utf8 tnS
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

    stmt = "SELECT DISTINCT txid FROM " <> tbl (toUtf8 tn) <> " WHERE txid > ?"

    tnS = T.encodeUtf8 tn
    collect p =
        let txids = fmap _deltaTxId $
                    concatMap NE.toList $
                    HashMap.elems $
                    fromMaybe mempty $
                    HashMap.lookup tnS (_pendingWrites p)
        in filter (> _tid) txids
{-# INLINE doTxIds #-}

recordTxLog
    :: (AsString k, J.Encode v)
    => TableName
    -> Domain k v
    -> k
    -> v
    -> BlockHandler logger ()
recordTxLog tt d k v = do
    -- are we in a tx?
    mptx <- use bsPendingTx
    modify' $ case mptx of
      Nothing -> over (bsPendingBlock . pendingTxLogMap) upd
      (Just _) -> over (bsPendingTx . _Just . pendingTxLogMap) upd

  where
    !upd = M.insertWith DL.append tt txlogs
    !txlogs = DL.singleton $! encodeTxLog $ TxLog (asString d) (asString k) v

modifyPendingData
    :: (SQLitePendingData -> SQLitePendingData)
    -> BlockHandler logger ()
modifyPendingData f = do
    m <- use bsPendingTx
    modify' $ case m of
      Just d -> set bsPendingTx (Just $! f d)
      Nothing -> over bsPendingBlock f

doCreateUserTable
    :: Maybe BlockHeight
    -- ^ the highest block we should be seeing tables from
    -> TableName
    -> ModuleName
    -> BlockHandler logger ()
doCreateUserTable mbh tn@(TableName ttxt) mn = do
    -- first check if tablename already exists in pending queues
    m <- runMaybeT $ checkDbTablePendingCreation (Utf8 $ T.encodeUtf8 ttxt)
    case m of
      Nothing -> throwM $ PactDuplicateTableError ttxt
      Just () -> do
          -- then check if it is in the db
          lcTables <- view blockHandlerLowerCaseTables
          cond <- inDb lcTables $ Utf8 $ T.encodeUtf8 ttxt
          when cond $ throwM $ PactDuplicateTableError ttxt
          modifyPendingData
            $ over pendingTableCreation (HashSet.insert (T.encodeUtf8 ttxt))
            . over pendingTxLogMap (M.insertWith DL.append (TableName txlogKey) txlogs)
  where
    inDb lcTables t = do
      r <- callDb "doCreateUserTable" $ \db ->
        qry db (tableLookupStmt lcTables) [SText t] [RText]
      case r of
        [[SText rname]] ->
          case mbh of
              -- if lowercase matching, no need to check equality
              -- (wasn't needed before either but leaving alone for replay)
              Nothing -> return (lcTables || rname == t)
              Just bh -> do
                  existsInDb <- tableExistsInDbAtHeight t bh
                  return $ existsInDb && (lcTables || rname == t)
        _ -> return False

    tableLookupStmt False =
      "SELECT name FROM sqlite_master WHERE type='table' and name=?;"
    tableLookupStmt True =
      "SELECT name FROM sqlite_master WHERE type='table' and lower(name)=lower(?);"
    txlogKey = "SYS:usertables"
    stn = asString tn
    uti = UserTableInfo mn
    txlogs = DL.singleton $ encodeTxLog $ TxLog txlogKey stn uti
{-# INLINE doCreateUserTable #-}

doRollback :: BlockHandler logger ()
doRollback = modify'
    $ set bsMode Nothing
    . set bsPendingTx Nothing

-- | Commit a Pact transaction
doCommit :: BlockHandler logger [TxLogJson]
doCommit = use bsMode >>= \case
    Nothing -> doRollback >> internalError "doCommit: Not in transaction"
    Just m -> do
        txrs <- if m == Transactional
          then do
              modify' $ over bsTxId succ
              -- merge pending tx into block data
              pending <- use bsPendingTx
              persistIntraBlockWrites <- view blockHandlerPersistIntraBlockWrites
              modify' $ over bsPendingBlock (merge persistIntraBlockWrites pending)
              blockLogs <- use $ bsPendingBlock . pendingTxLogMap
              modify' $ set bsPendingTx Nothing
              resetTemp
              return blockLogs
          else doRollback >> return mempty
        return $! concatMap (reverse . DL.toList) txrs
  where
    merge _ Nothing a = a
    merge persistIntraBlockWrites (Just txPending) blockPending = SQLitePendingData
        { _pendingTableCreation = HashSet.union (_pendingTableCreation txPending) (_pendingTableCreation blockPending)
        , _pendingWrites = HashMap.unionWith (HashMap.unionWith mergeAtRowKey) (_pendingWrites txPending) (_pendingWrites blockPending)
        , _pendingTxLogMap = _pendingTxLogMap txPending
        , _pendingSuccessfulTxs = _pendingSuccessfulTxs blockPending
        }
        where
        mergeAtRowKey txWrites blockWrites =
            let lastTxWrite = NE.head txWrites
            in case persistIntraBlockWrites of
                PersistIntraBlockWrites -> lastTxWrite `NE.cons` blockWrites
                DoNotPersistIntraBlockWrites -> lastTxWrite :| []
{-# INLINE doCommit #-}

-- | Begin a Pact transaction
doBegin :: (Logger logger) => ExecutionMode -> BlockHandler logger (Maybe TxId)
doBegin m = do
    logger <- view blockHandlerLogger
    use bsMode >>= \case
        Just {} -> do
            logError_ logger "beginTx: In transaction, rolling back"
            doRollback
        Nothing -> return ()
    resetTemp
    modify'
        $ set bsMode (Just m)
        . set bsPendingTx (Just emptySQLitePendingData)
    case m of
        Transactional -> Just <$> use bsTxId
        Local -> pure Nothing
{-# INLINE doBegin #-}

resetTemp :: BlockHandler logger ()
resetTemp = modify'
    $ set bsMode Nothing
    -- clear out txlog entries
    . set (bsPendingBlock . pendingTxLogMap) mempty

doGetTxLog :: Domain k RowData -> TxId -> BlockHandler logger [TxLog RowData]
doGetTxLog d txid = do
    -- try to look up this tx from pending log -- if we find it there it can't
    -- possibly be in the db.
    p <- readFromPending
    if null p then readFromDb else return p

  where
    tableName = domainTableName d
    Utf8 tableNameBS = tableName

    readFromPending = do
        allPendingData <- getPendingData
        let deltas = do
                -- grab all pending writes in this transaction and elsewhere in
                -- this block
                pending <- allPendingData
                -- all writes to the table
                let writesAtTableByKey =
                        fromMaybe mempty $ HashMap.lookup tableNameBS $ _pendingWrites pending
                -- a list of all writes to the table for some particular key
                allWritesForSomeKey <- HashMap.elems writesAtTableByKey
                -- the single latest write to the table for that key which is
                -- from this txid; the most recent writes are inserted at the
                -- front of the pending data
                latestWriteForSomeKey <- take 1
                    [ writeForSomeKey
                    | writeForSomeKey <- NE.toList allWritesForSomeKey
                    , _deltaTxId writeForSomeKey == txid
                    ]
                return latestWriteForSomeKey
        mapM (\x -> toTxLog d (Utf8 $ _deltaRowKey x) (_deltaData x)) deltas

    readFromDb = do
        rows <- callDb "doGetTxLog" $ \db -> qry db stmt
          [SInt (fromIntegral txid)]
          [RText, RBlob]
        forM rows $ \case
            [SText key, SBlob value] -> toTxLog d key value
            err -> internalError $
              "readHistoryResult: Expected single row with two columns as the \
              \result, got: " <> T.pack (show err)
    stmt = "SELECT rowkey, rowdata FROM " <> tbl tableName <> " WHERE txid = ?"


toTxLog :: MonadThrow m =>
           Domain k v -> Utf8 -> BS.ByteString -> m (TxLog RowData)
toTxLog d key value =
        case Data.Aeson.decodeStrict' value of
            Nothing -> internalError
              "toTxLog: Unexpected value, unable to deserialize log"
            Just v ->
              return $! TxLog (asString d) (fromUtf8 key) v

-- | Register a successful transaction in the pending data for the block
indexPactTransaction :: BS.ByteString -> BlockHandler logger ()
indexPactTransaction h = modify' $
    over (bsPendingBlock . pendingSuccessfulTxs) $ HashSet.insert h

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

-- | Delete any state from the database newer than the input parent header.
rewindDbTo
    :: Database
    -> Maybe ParentHeader
    -> IO ()
rewindDbTo db Nothing = rewindDbToGenesis db
rewindDbTo db mh@(Just (ParentHeader ph)) = do
    !endingtxid <- getEndTxId "rewindDbToBlock" db mh
    rewindDbToBlock db (_blockHeight ph) endingtxid

-- rewind before genesis, delete all user tables and all rows in all tables
rewindDbToGenesis
  :: Database
  -> IO ()
rewindDbToGenesis db = do
    exec_ db "DELETE FROM BlockHistory;"
    exec_ db "DELETE FROM [SYS:KeySets];"
    exec_ db "DELETE FROM [SYS:Modules];"
    exec_ db "DELETE FROM [SYS:Namespaces];"
    exec_ db "DELETE FROM [SYS:Pacts];"
    tblNames <- qry_ db "SELECT tablename FROM VersionedTableCreation;" [RText]
    forM_ tblNames $ \t -> case t of
      [SText tn] -> exec_ db ("DROP TABLE [" <> tn <> "];")
      _ -> internalError "Something went wrong when resetting tables."
    exec_ db "DELETE FROM VersionedTableCreation;"
    exec_ db "DELETE FROM VersionedTableMutation;"
    exec_ db "DELETE FROM TransactionIndex;"

-- | Rewind the database to a particular block, given the end tx id of that
-- block.
rewindDbToBlock
  :: Database
  -> BlockHeight
  -> TxId
  -> IO ()
rewindDbToBlock db bh endingTxId = do
    tableMaintenanceRowsVersionedSystemTables
    droppedtbls <- dropTablesAtRewind
    vacuumTablesAtRewind droppedtbls
    deleteHistory
    clearTxIndex
  where
    dropTablesAtRewind :: IO (HashSet BS.ByteString)
    dropTablesAtRewind = do
        toDropTblNames <- qry db findTablesToDropStmt
                          [SInt (fromIntegral bh)] [RText]
        tbls <- fmap HashSet.fromList . forM toDropTblNames $ \case
            [SText tblname@(Utf8 tn)] -> do
                exec_ db $ "DROP TABLE IF EXISTS " <> tbl tblname
                return tn
            _ -> internalError rewindmsg
        exec' db
            "DELETE FROM VersionedTableCreation WHERE createBlockheight > ?"
            [SInt (fromIntegral bh)]
        return tbls
    findTablesToDropStmt =
      "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight > ?;"
    rewindmsg =
      "rewindBlock: dropTablesAtRewind: Couldn't resolve the name of the table to drop."

    deleteHistory :: IO ()
    deleteHistory =
        exec' db "DELETE FROM BlockHistory WHERE blockheight > ?"
              [SInt (fromIntegral bh)]

    vacuumTablesAtRewind :: HashSet BS.ByteString -> IO ()
    vacuumTablesAtRewind droppedtbls = do
        let processMutatedTables ms = fmap HashSet.fromList . forM ms $ \case
              [SText (Utf8 tn)] -> return tn
              _ -> internalError "rewindBlock: vacuumTablesAtRewind: Couldn't resolve the name \
                                 \of the table to possibly vacuum."
        mutatedTables <- qry db
            "SELECT DISTINCT tablename FROM VersionedTableMutation WHERE blockheight > ?;"
          [SInt (fromIntegral bh)]
          [RText]
          >>= processMutatedTables
        let toVacuumTblNames = HashSet.difference mutatedTables droppedtbls
        forM_ toVacuumTblNames $ \tblname ->
            exec' db ("DELETE FROM " <> tbl (Utf8 tblname) <> " WHERE txid >= ?")
                  [SInt $! fromIntegral endingTxId]
        exec' db "DELETE FROM VersionedTableMutation WHERE blockheight > ?;"
              [SInt (fromIntegral bh)]

    tableMaintenanceRowsVersionedSystemTables :: IO ()
    tableMaintenanceRowsVersionedSystemTables = do
        exec' db "DELETE FROM [SYS:KeySets] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Modules] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Namespaces] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Pacts] WHERE txid >= ?" tx
      where
        tx = [SInt $! fromIntegral endingTxId]

    -- | Delete all future transactions from the index
    clearTxIndex :: IO ()
    clearTxIndex =
        exec' db "DELETE FROM TransactionIndex WHERE blockheight > ?;"
              [ SInt (fromIntegral bh) ]

commitBlockStateToDatabase :: Database -> BlockHash -> BlockHeight -> BlockState -> IO ()
commitBlockStateToDatabase db hsh bh blockState = do
  let newTables = _pendingTableCreation $ _bsPendingBlock blockState
  mapM_ (\tn -> createUserTable (Utf8 tn)) newTables
  let writeV = toChunks $ _pendingWrites (_bsPendingBlock blockState)
  backendWriteUpdateBatch writeV
  indexPendingPactTransactions
  let nextTxId = _bsTxId blockState
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
        let txs = _pendingSuccessfulTxs $ _bsPendingBlock blockState
        dbIndexTransactions txs

      where
        toRow b = [SBlob b, SInt (fromIntegral bh)]
        dbIndexTransactions txs = do
            let rows = map toRow $ toList txs
            execMulti db "INSERT INTO TransactionIndex (txhash, blockheight) \
                         \ VALUES (?, ?)" rows


-- | Create all tables that exist pre-genesis
initSchema :: (Logger logger) => logger -> Database -> IO ()
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
  where
    create tablename = do
      logInfo_ logger $ "initSchema: "  <> fromUtf8 tablename
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

getEndTxId :: Text -> Database -> Maybe ParentHeader -> IO TxId
getEndTxId msg sql pc = case pc of
  Nothing -> return 0
  Just (ParentHeader ph) -> getEndTxId' msg sql (_blockHeight ph) (_blockHash ph)

getEndTxId' :: Text -> Database -> BlockHeight -> BlockHash -> IO TxId
getEndTxId' msg sql bh bhsh = do
    r <- qry sql
      "SELECT endingtxid FROM BlockHistory WHERE blockheight = ? and hash = ?;"
      [ SInt $ fromIntegral bh
      , SBlob $ runPutS (encodeBlockHash bhsh)
      ]
      [RInt]
    case r of
      [[SInt tid]] -> return (TxId (fromIntegral tid))
      [] -> throwM $ BlockHeaderLookupFailure $ msg <> ".getEndTxId: not in db: " <>
            sshow (bh, bhsh)
      _ -> internalError $ msg <> ".getEndTxId: expected single-row int result, got " <> sshow r

-- | Careful doing this! It's expensive and for our use case, probably pointless.
-- We should reserve vacuuming for an offline process
vacuumDb :: BlockHandler logger ()
vacuumDb = callDb "vacuumDb" (`exec_` "VACUUM;")
