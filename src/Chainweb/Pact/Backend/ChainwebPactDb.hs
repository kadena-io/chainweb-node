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
, handlePossibleRewind
, blockHistoryInsert
, initSchema
, indexPactTransaction
, indexPendingPactTransactions
, clearPendingTxState
, backendWriteUpdateBatch
, createUserTable
, vacuumDb
, toTxLog
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
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Database.SQLite3.Direct as SQ3

import GHC.Stack

import Prelude hiding (concat, log)

import Text.Printf (printf)

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
import Chainweb.Pact.Service.Types (PactException(..), internalError)
import Chainweb.Pact.Types (logInfo_, logError_)
import Chainweb.Version
import Chainweb.Utils (sshow)
import Chainweb.Utils.Serialization

tbl :: HasCallStack => Utf8 -> Utf8
tbl t@(Utf8 b)
    | B8.elem ']' b =  error $ "Chainweb.Pact.Backend.ChainwebPactDb: Code invariant violation. Illegal SQL table name " <> sshow b <> ". Please report this as a bug."
    | otherwise = "[" <> t <> "]"

chainwebPactDb :: (Logger logger) => PactDb (BlockEnv logger SQLiteEnv)
chainwebPactDb = PactDb
    { _readRow = \d k e -> runBlockEnv e $ doReadRow d k
    , _writeRow = \wt d k v e -> runBlockEnv e $ doWriteRow wt d k v
    , _keys = \d e -> runBlockEnv e $ doKeys d
    , _txids = \t txid e -> runBlockEnv e $ doTxIds t txid
    , _createUserTable = \tn mn e -> runBlockEnv e $ doCreateUserTable tn mn
    , _getUserTableInfo = \_ -> error "WILL BE DEPRECATED!"
    , _beginTx = \m e -> runBlockEnv e $ doBegin m
    , _commitTx = \e -> runBlockEnv e doCommit
    , _rollbackTx = \e -> runBlockEnv e doRollback
    , _getTxLog = \d tid e -> runBlockEnv e $ doGetTxLog d tid
    }

getPendingData :: BlockHandler logger SQLiteEnv [SQLitePendingData]
getPendingData = do
    pb <- use bsPendingBlock
    ptx <- maybeToList <$> use bsPendingTx
    -- lookup in pending transactions first
    return $ ptx ++ [pb]

forModuleNameFix :: (Bool -> BlockHandler logger e a) -> BlockHandler logger e a
forModuleNameFix f = use bsModuleNameFix >>= f

doReadRow
    :: (IsString k, FromJSON v)
    => Domain k v
    -> k
    -> BlockHandler logger SQLiteEnv (Maybe v)
doReadRow d k = forModuleNameFix $ \mnFix ->
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

    queryStmt =
        "SELECT rowdata FROM " <> tbl tableName <> " WHERE rowkey = ? ORDER BY txid DESC LIMIT 1;"

    lookupWithKey
        :: forall logger v . FromJSON v
        => Utf8
        -> (Utf8 -> BS.ByteString -> MaybeT (BlockHandler logger SQLiteEnv) v)
        -> BlockHandler logger SQLiteEnv (Maybe v)
    lookupWithKey key checkCache = do
        pds <- getPendingData
        let lookPD = foldr1 (<|>) $ map (lookupInPendingData key) pds
        let lookDB = lookupInDb key checkCache
        runMaybeT (lookPD <|> lookDB)

    lookupInPendingData
        :: forall logger v . FromJSON v
        => Utf8
        -> SQLitePendingData
        -> MaybeT (BlockHandler logger SQLiteEnv) v
    lookupInPendingData (Utf8 rowkey) p = do
        let deltaKey = SQLiteDeltaKey tableNameBS rowkey
        ddata <- fmap _deltaData <$> MaybeT (return $ HashMap.lookup deltaKey (_pendingWrites p))
        if null ddata
            -- should be impossible, but we'll check this case
            then mzero
            -- we merge with (++) which should produce txids most-recent-first
            -- -- we care about the most recent update to this rowkey
            else MaybeT $ return $! decodeStrict' $ DL.head ddata

    lookupInDb
        :: forall logger v . FromJSON v
        => Utf8
        -> (Utf8 -> BS.ByteString -> MaybeT (BlockHandler logger SQLiteEnv) v)
        -> MaybeT (BlockHandler logger SQLiteEnv) v
    lookupInDb rowkey checkCache = do
        -- First, check: did we create this table during this block? If so,
        -- there's no point in looking up the key.
        checkDbTableExists tableName
        result <- lift $ callDb "doReadRow"
                       $ \db -> qry db queryStmt [SText rowkey] [RBlob]
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
        -> MaybeT (BlockHandler logger SQLiteEnv) v
    noCache _key rowdata = MaybeT $ return $! decodeStrict' rowdata


checkDbTableExists :: Utf8 -> MaybeT (BlockHandler logger SQLiteEnv) ()
checkDbTableExists tableName = do
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
    -> BlockHandler logger SQLiteEnv ()
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
    -> BlockHandler logger SQLiteEnv ()
recordPendingUpdate (Utf8 key) (Utf8 tn) txid v = modifyPendingData modf
  where
    !vs = J.encodeStrict v
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
        , SBlob rowdata
        ]

    writeTable (tableName, writes) = do
        execMulti db q (V.toList $ V.map prepRow writes)
        markTableMutation tableName bh db
      where
        q = "INSERT OR REPLACE INTO " <> tbl tableName <> "(rowkey,txid,rowdata) VALUES(?,?,?)"


markTableMutation :: Utf8 -> BlockHeight -> Database -> IO ()
markTableMutation tablename blockheight db = do
    exec' db mutq [SText tablename, SInt (fromIntegral blockheight)]
  where
    mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"

checkInsertIsOK
    :: WriteType
    -> Domain RowKey RowData
    -> RowKey
    -> BlockHandler logger SQLiteEnv (Maybe RowData)
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
    -> Domain RowKey RowData
    -> RowKey
    -> RowData
    -> BlockHandler logger SQLiteEnv ()
writeUser wt d k rowdata@(RowData _ row) = gets _bsTxId >>= go
  where
    toTableName = TableName . fromUtf8
    tn = domainTableName d
    ttn = toTableName tn

    go txid = do
        m <- checkInsertIsOK wt d k
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
    => WriteType
    -> Domain k v
    -> k
    -> v
    -> BlockHandler logger SQLiteEnv ()
doWriteRow wt d k v = case d of
    (UserTables _) -> writeUser wt d k v
    _ -> writeSys d k v

doKeys
    :: (IsString k)
    => Domain k v
    -> BlockHandler logger SQLiteEnv [k]
doKeys d = do
    msort <- uses bsSortedKeys (\c -> if c then sort else id)
    dbKeys <- getDbKeys
    pb <- use bsPendingBlock
    mptx <- use bsPendingTx

    let memKeys = DL.toList $
                  fmap (B8.unpack . _deltaRowKey) $
                  collect pb `DL.append` maybe DL.empty collect mptx

    let !allKeys = fmap fromString
                  $ msort -- becomes available with Pact42Upgrade
                  $ LHM.sort
                  $ dbKeys ++ memKeys
    return allKeys

  where
    getDbKeys = do
        m <- runMaybeT $ checkDbTableExists $ Utf8 tnS
        case m of
            Nothing -> return mempty
            Just () -> do
                ks <- callDb "doKeys" $ \db ->
                          qry_ db  ("SELECT DISTINCT rowkey FROM " <> tbl tn) [RText]
                forM ks $ \row -> do
                    case row of
                        [SText k] -> return $! T.unpack $ fromUtf8 k
                        _ -> internalError "doKeys: The impossible happened."

    tn = domainTableName d
    tnS = let (Utf8 x) = tn in x
    collect p =
        let flt k _ = _dkTable k == tnS
        in DL.concat $ HashMap.elems $ HashMap.filterWithKey flt (_pendingWrites p)
{-# INLINE doKeys #-}

-- tid is non-inclusive lower bound for the search
doTxIds :: TableName -> TxId -> BlockHandler logger SQLiteEnv [TxId]
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

    stmt = "SELECT DISTINCT txid FROM " <> tbl (toUtf8 tn) <> " WHERE txid > ?"

    tnS = T.encodeUtf8 tn
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
    :: (AsString k, J.Encode v)
    => TableName
    -> Domain k v
    -> k
    -> v
    -> BlockHandler logger SQLiteEnv ()
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
    -> BlockHandler logger SQLiteEnv ()
modifyPendingData f = do
    m <- use bsPendingTx
    modify' $ case m of
      Just d -> set bsPendingTx (Just $! f d)
      Nothing -> over bsPendingBlock f

doCreateUserTable :: TableName -> ModuleName -> BlockHandler logger SQLiteEnv ()
doCreateUserTable tn@(TableName ttxt) mn = do
    -- first check if tablename already exists in pending queues
    m <- runMaybeT $ checkDbTableExists (Utf8 $ T.encodeUtf8 ttxt)
    case m of
      Nothing -> throwM $ PactDuplicateTableError ttxt
      Just () -> do
          -- then check if it is in the db
          lcTables <- use bsLowerCaseTables
          cond <- inDb lcTables $ Utf8 $ T.encodeUtf8 ttxt
          when cond $ throwM $ PactDuplicateTableError ttxt
          modifyPendingData
            $ over pendingTableCreation (HashSet.insert (T.encodeUtf8 ttxt))
            . over pendingTxLogMap (M.insertWith DL.append (TableName txlogKey) txlogs)
  where
    inDb lcTables t =
      callDb "doCreateUserTable" $ \db -> do
        r <- qry db (tableLookupStmt lcTables) [SText t] [RText]
        return $ case r of
          -- if lowercase matching, no need to check equality
          -- (wasn't needed before either but leaving alone for replay)
          [[SText rname]] -> lcTables || rname == t
          _ -> False

    tableLookupStmt False =
      "SELECT name FROM sqlite_master WHERE type='table' and name=?;"
    tableLookupStmt True =
      "SELECT name FROM sqlite_master WHERE type='table' and lower(name)=lower(?);"
    txlogKey = "SYS:usertables"
    stn = asString tn
    uti = UserTableInfo mn
    txlogs = DL.singleton $ encodeTxLog $ TxLog txlogKey stn uti
{-# INLINE doCreateUserTable #-}

doRollback :: BlockHandler logger SQLiteEnv ()
doRollback = modify'
    $ set bsMode Nothing
    . set bsPendingTx Nothing

doCommit :: BlockHandler logger SQLiteEnv [TxLogJson]
doCommit = use bsMode >>= \case
    Nothing -> doRollback >> internalError "doCommit: Not in transaction"
    Just m -> do
        txrs <- if m == Transactional
          then do
              modify' $ over bsTxId succ
              -- merge pending tx into block data
              pending <- use bsPendingTx
              modify' $ over bsPendingBlock (merge pending)
              blockLogs <- use $ bsPendingBlock . pendingTxLogMap
              modify' $ set bsPendingTx Nothing
              resetTemp
              return blockLogs
          else doRollback >> return mempty
        return $! concatMap (reverse . DL.toList) txrs
  where
    merge Nothing a = a
    merge (Just a) b = SQLitePendingData
        { _pendingTableCreation = HashSet.union (_pendingTableCreation a) (_pendingTableCreation b)
        , _pendingWrites = HashMap.unionWith mergeW (_pendingWrites a) (_pendingWrites b)
        , _pendingTxLogMap = _pendingTxLogMap a
        , _pendingSuccessfulTxs = _pendingSuccessfulTxs b
        }

    mergeW a b = case take 1 (DL.toList a) of
        [] -> b
        (x:_) -> DL.cons x b
{-# INLINE doCommit #-}

clearPendingTxState :: BlockHandler logger SQLiteEnv ()
clearPendingTxState = do
    modify'
        $ set bsPendingBlock emptySQLitePendingData
        . set bsPendingTx Nothing
    resetTemp

doBegin :: (Logger logger) => ExecutionMode -> BlockHandler logger SQLiteEnv (Maybe TxId)
doBegin m = do
    logger <- view bdbenvLogger
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

resetTemp :: BlockHandler logger SQLiteEnv ()
resetTemp = modify'
    $ set bsMode Nothing
    -- clear out txlog entries
    . set (bsPendingBlock . pendingTxLogMap) mempty

doGetTxLog :: Domain k RowData -> TxId -> BlockHandler logger SQLiteEnv [TxLog RowData]
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
        ptx <- maybeToList <$> use bsPendingTx
        pb <- use bsPendingBlock
        let deltas = (ptx ++ [pb]) >>= HashMap.elems . _pendingWrites >>= takeHead . DL.toList
        let ourDeltas = filter predicate deltas
        mapM (\x -> toTxLog d (Utf8 $ _deltaRowKey x) (_deltaData x)) ourDeltas

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

blockHistoryInsert :: BlockHeight -> BlockHash -> TxId -> BlockHandler logger SQLiteEnv ()
blockHistoryInsert bh hsh t =
    callDb "blockHistoryInsert" $ \db ->
        exec' db stmt
            [ SInt (fromIntegral bh)
            , SBlob (runPutS (encodeBlockHash hsh))
            , SInt (fromIntegral t)
            ]
  where
    stmt =
      "INSERT INTO BlockHistory ('blockheight','hash','endingtxid') VALUES (?,?,?);"

createTransactionIndexTable :: BlockHandler logger SQLiteEnv ()
createTransactionIndexTable = callDb "createTransactionIndexTable" $ \db -> do
    exec_ db "CREATE TABLE IF NOT EXISTS TransactionIndex \
             \ (txhash BLOB NOT NULL, \
             \ blockheight UNSIGNED BIGINT NOT NULL, \
             \ CONSTRAINT transactionIndexConstraint UNIQUE(txhash));"
    exec_ db "CREATE INDEX IF NOT EXISTS \
             \ transactionIndexByBH ON TransactionIndex(blockheight)";

indexPactTransaction :: BS.ByteString -> BlockHandler logger SQLiteEnv ()
indexPactTransaction h = modify' $
    over (bsPendingBlock . pendingSuccessfulTxs) $ HashSet.insert h


indexPendingPactTransactions :: BlockHandler logger SQLiteEnv ()
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

clearTxIndex :: BlockHandler logger SQLiteEnv ()
clearTxIndex = do
    bh <- gets _bsBlockHeight
    callDb "clearTxIndex" $ \db -> do
        exec' db "DELETE FROM TransactionIndex WHERE blockheight >= ?;"
              [ SInt (fromIntegral bh) ]

createBlockHistoryTable :: BlockHandler logger SQLiteEnv ()
createBlockHistoryTable =
    callDb "createBlockHistoryTable" $ \db -> exec_ db
        "CREATE TABLE IF NOT EXISTS BlockHistory \
        \(blockheight UNSIGNED BIGINT NOT NULL,\
        \ hash BLOB NOT NULL,\
        \ endingtxid UNSIGNED BIGINT NOT NULL, \
        \ CONSTRAINT blockHashConstraint UNIQUE (blockheight));"

createTableCreationTable :: BlockHandler logger SQLiteEnv ()
createTableCreationTable =
    callDb "createTableCreationTable" $ \db -> exec_ db
      "CREATE TABLE IF NOT EXISTS VersionedTableCreation\
      \(tablename TEXT NOT NULL\
      \, createBlockheight UNSIGNED BIGINT NOT NULL\
      \, CONSTRAINT creation_unique UNIQUE(createBlockheight, tablename));"

createTableMutationTable :: BlockHandler logger SQLiteEnv ()
createTableMutationTable =
    callDb "createTableMutationTable" $ \db -> do
        exec_ db "CREATE TABLE IF NOT EXISTS VersionedTableMutation\
                 \(tablename TEXT NOT NULL\
                 \, blockheight UNSIGNED BIGINT NOT NULL\
                 \, CONSTRAINT mutation_unique UNIQUE(blockheight, tablename));"

createUserTable :: Utf8 -> BlockHeight -> BlockHandler logger SQLiteEnv ()
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
    ixName = tablename <> "_ix"
    createtablestmt =
      "CREATE TABLE IF NOT EXISTS " <> tbl tablename <> " \
             \ (rowkey TEXT\
             \, txid UNSIGNED BIGINT NOT NULL\
             \, rowdata BLOB NOT NULL\
             \, UNIQUE (rowkey, txid));"
    indexcreationstmt =
        "CREATE INDEX IF NOT EXISTS " <> tbl ixName <> " ON " <> tbl tablename <> "(txid DESC);"

-- | Rewind the checkpoint in the current BlockHistory to the requested block
-- height and hash. This doesn't handle forks. The requested block hash must
-- exist in the history. Otherwise this function will fail.
--
-- /Precondition:/
--
-- For @handlePossibleRewind v c height parentHash@ the respective block for
-- @parentHash@ must be of blockHeight @height - 1@.
--
handlePossibleRewind
    :: HasCallStack
    => ChainwebVersion
    -> ChainId
    -> BlockHeight
        -- ^ The block height to which the check pointer is restored. This is the
        -- height off the block that is going to be validated.

    -> ParentHash
        -- ^ The parent of the block that is going to be validated. The height
        -- of the respective block is one less than the height provided in the
        -- previous argument.

    -> BlockHandler logger SQLiteEnv TxId
handlePossibleRewind v cid bRestore hsh = do
    bCurrent <- getBCurrentHeight
    checkHistoryInvariant (bCurrent + 1)
    case compare bRestore (bCurrent + 1) of
        GT -> internalError "handlePossibleRewind: Block_Restore invariant violation!"
        EQ -> newChildBlock bCurrent
        LT -> rewindBlock bRestore
  where

    -- The maximum block height that is stored in the block history.
    --
    getBCurrentHeight = do
        r <- callDb "handlePossibleRewind" $ \db ->
             qry_ db "SELECT max(blockheight) AS current_block_height \
                     \FROM BlockHistory;" [RInt]
        bh <- liftIO $ expectSingleRowCol "handlePossibleRewind: (block):" r >>= \case
            SInt x -> return x
            _ -> error "Chainweb.Pact.ChainwebPactDb.handlePossibleRewind.newChildBlock: failed to match SInt"
        return $! BlockHeight (fromIntegral bh)

    -- Check that @bRestore - 1@ exists in the BlockHistory. We expect to find
    -- exactly one block with hash @hsh@.
    --
    checkHistoryInvariant succOfCurrent = do
        -- enforce invariant that the history has
        -- (B_restore-1,H_parent).
        resultCount <- callDb "handlePossibleRewind" $ \db -> do
            qry db "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"
                   [ SInt $! fromIntegral $ pred bRestore
                   , SBlob (runPutS $ encodeBlockHash hsh) ]
                   [RInt]
                >>= expectSingleRowCol "handlePossibleRewind: (historyInvariant):"
        when (resultCount /= SInt 1) $
          internalError $ historyInvariantMessage resultCount
      where
        historyInvariantMessage (SInt entryCount)
            | entryCount < 0 = error "impossible"
            | entryCount == 0 && bRestore > succOfCurrent = futureRestorePointMessage
            | entryCount == 0 && bRestore <= succOfCurrent = missingBlockMessage
            | otherwise = rowCountErrorMessage entryCount
        historyInvariantMessage _ = error "impossible"

        missingBlockMessage :: T.Text
        missingBlockMessage = T.pack $
            printf "handlePossibleRewind: The checkpointer attempted to restore to block hash\
                \ %s at block height %d, which is not in the current block history of the\
                \ checkpointer at height %d."
                (blockHashToText hsh) (getBlockHeight bRestore - 1) (getBlockHeight succOfCurrent - 1)

        rowCountErrorMessage = T.pack .
            printf "At this blockheight/blockhash (%d, %s) in BlockHistoryTable, there are %d entries."
                (fromIntegral bRestore :: Int) (show hsh)

        futureRestorePointMessage :: Text
        futureRestorePointMessage = T.pack $
            printf "handlePossibleRewind: The checkpointer attempted to restore to block hash %s\
                \ at height %d, which is greater than the max entry in the block history of the\
                \ checkpointer at height %d."
                (blockHashToText hsh) (getBlockHeight bRestore - 1) (getBlockHeight succOfCurrent - 1)

    newChildBlock bCurrent = do
        assign bsBlockHeight bRestore
        r <- callDb "getting txid" $ \db ->
          expectSingleRowCol msg =<< qry db
              "SELECT endingtxid FROM BlockHistory WHERE blockheight = ?;"
              [SInt (fromIntegral bCurrent)]
              [RInt]
        !txid <- case r of
            SInt x -> return x
            _ -> error "Chainweb.Pact.ChainwebPactDb.handlePossibleRewind.newChildBlock: failed to match SInt"
        assign bsTxId (fromIntegral txid)
        return $ fromIntegral txid
      where msg = "handlePossibleRewind: newChildBlock: error finding txid"

    rewindBlock bh = do
        assign bsBlockHeight bh
        !endingtx <- getEndingTxId v cid bh
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
    tbls <- fmap HashSet.fromList . forM toDropTblNames $ \case
        [SText tblname@(Utf8 tn)] -> do
            exec_ db $ "DROP TABLE IF EXISTS " <> tbl tblname
            return tn
        _ -> internalError rewindmsg
    exec' db
        "DELETE FROM VersionedTableCreation WHERE createBlockheight >= ?"
        [SInt (fromIntegral bh)]
    return tbls
  where findTablesToDropStmt =
          "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight >= ?;"
        rewindmsg =
          "rewindBlock:\
          \ dropTablesAtRewind: \
          \Couldn't resolve the name of the table to drop."

vacuumTablesAtRewind :: BlockHeight -> TxId -> HashSet BS.ByteString -> Database -> IO ()
vacuumTablesAtRewind bh endingtx droppedtbls db = do
    let processMutatedTables ms = fmap HashSet.fromList . forM ms $ \case
          [SText (Utf8 tn)] -> return tn
          _ -> internalError "rewindBlock: vacuumTablesAtRewind: Couldn't resolve the name \
                             \of the table to possibly vacuum."
    mutatedTables <- qry db
        "SELECT DISTINCT tablename FROM VersionedTableMutation WHERE blockheight >= ?;"
      [SInt (fromIntegral bh)]
      [RText]
      >>= processMutatedTables
    let toVacuumTblNames = HashSet.difference mutatedTables droppedtbls
    forM_ toVacuumTblNames $ \tblname ->
        exec' db ("DELETE FROM " <> tbl (Utf8 tblname) <> " WHERE txid >= ?")
              [SInt $! fromIntegral endingtx]
    exec' db "DELETE FROM VersionedTableMutation WHERE blockheight >= ?;"
          [SInt (fromIntegral bh)]

tableMaintenanceRowsVersionedSystemTables :: TxId -> BlockHandler logger SQLiteEnv ()
tableMaintenanceRowsVersionedSystemTables endingtx = do
    callDb "tableMaintenanceRowsVersionedSystemTables" $ \db -> do
        exec' db "DELETE FROM [SYS:KeySets] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Modules] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Namespaces] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Pacts] WHERE txid >= ?" tx
  where
    tx = [SInt $! fromIntegral endingtx]

deleteHistory :: BlockHeight -> BlockHandler logger SQLiteEnv ()
deleteHistory bh = do
    callDb "Deleting from BlockHistory, VersionHistory" $ \db -> do
        exec' db "DELETE FROM BlockHistory WHERE blockheight >= ?"
              [SInt (fromIntegral bh)]

initSchema :: (Logger logger) => BlockHandler logger SQLiteEnv ()
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
      logger <- view bdbenvLogger
      logInfo_ logger $ "initSchema: "  <> fromUtf8 tablename
      callDb "initSchema" $ createVersionedTable tablename

getEndingTxId :: ChainwebVersion -> ChainId -> BlockHeight -> BlockHandler logger SQLiteEnv TxId
getEndingTxId v cid bh = callDb "getEndingTxId" $ \db -> do
    if bh == genesisHeight v cid
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
vacuumDb :: BlockHandler logger SQLiteEnv ()
vacuumDb = callDb "vacuumDb" (`exec_` "VACUUM;")
