{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Chainweb.Pact.Backend.ChainwebPactCoreDb
( chainwebPactCoreDb
, rewoundPactCoreDb
, toTxLog
, toPactTxLog
) where

import Data.Coerce
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Concurrent.MVar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.DList as DL
import Data.List(sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Default

import Database.SQLite3.Direct

import GHC.Stack

import Prelude hiding (concat, log)

-- pact

import qualified Pact.JSON.Legacy.HashMap as LHM
import qualified Pact.Types.Persistence as Pact4
import Pact.Types.SQLite
import Pact.Types.Util (AsString(..))
import qualified Pact.Types.Term as Pact4


import Pact.Core.Persistence as PCore
import Pact.Core.Serialise
import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Builtin
import Pact.Core.Guards
import Pact.Core.PactValue
import Pact.Core.Literal
import Pact.Core.Gas
import Pact.Core.Errors

-- chainweb

import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types (PactException(..), internalError, IntraBlockPersistence(..))
import Chainweb.Pact.Types (logError_)
import Chainweb.Utils (sshow)

tbl :: HasCallStack => Utf8 -> Utf8
tbl t@(Utf8 b)
    | B8.elem ']' b =  error $ "Chainweb.Pact.Backend.ChainwebPactDb: Code invariant violation. Illegal SQL table name " <> sshow b <> ". Please report this as a bug."
    | otherwise = "[" <> t <> "]"

chainwebPactCoreDb :: (Logger logger) => MVar (BlockEnv logger) -> PactDb CoreBuiltin SpanInfo
chainwebPactCoreDb e = PactDb
    { _pdbPurity = PImpure
    , _pdbRead = \d k -> runBlockEnv e $ doReadRow Nothing d k
    , _pdbWrite = \wt d k v -> do
        gasenv <- ask
        liftIO $ runBlockEnv e $ doWriteRow gasenv Nothing wt d k v
    , _pdbKeys = \d -> runBlockEnv e $ doKeys Nothing d
    , _pdbCreateUserTable = \tn -> do
        gasenv <- ask
        liftIO $ runBlockEnv e $ doCreateUserTable gasenv Nothing tn
    , _pdbBeginTx = \m -> runBlockEnv e $ doBegin m
    , _pdbCommitTx = runBlockEnv e doCommit
    , _pdbRollbackTx = runBlockEnv e doRollback
    , _pdbTxIds = \t txid -> runBlockEnv e $ doTxIds t txid
    , _pdbGetTxLog = \tn tid -> runBlockEnv e $ doGetTxLog tn tid
    }

-- | Pact DB which reads from some past block height, instead of the tip of the checkpointer
rewoundPactCoreDb :: (Logger logger) => MVar (BlockEnv logger) -> BlockHeight -> TxId -> PactDb CoreBuiltin SpanInfo
rewoundPactCoreDb e bh endTxId = (chainwebPactCoreDb e)
    { _pdbRead = \d k -> runBlockEnv e $ doReadRow (Just (bh, endTxId)) d k
    , _pdbWrite = \wt d k v -> do
        gasenv <- ask
        liftIO $ runBlockEnv e $ doWriteRow gasenv (Just (bh, endTxId)) wt d k v
    , _pdbKeys = \d -> runBlockEnv e $ doKeys (Just (bh, endTxId)) d
    , _pdbCreateUserTable = \tn -> do
        gasenv <- ask
        liftIO $ runBlockEnv e $ doCreateUserTable gasenv (Just bh) tn
    }

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
tableExistsInDbAtHeight tablename bh = do
    let knownTbls =
          ["SYS:Pacts", "SYS:Modules", "SYS:KeySets", "SYS:Namespaces"]
    if tablename `elem` knownTbls
    then return True
    else callDb "tableExists" $ \db -> do
      let tableExistsStmt =
            -- table names are case-sensitive
            "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight < ? AND lower(tablename) = lower(?)"
      qry db tableExistsStmt [SInt $ max 0 (fromIntegral bh), SText tablename] [RText] >>= \case
        [] -> return False
        _ -> return True

doReadRow
    :: Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> Domain k v CoreBuiltin SpanInfo
    -> k
    -> BlockHandler logger (Maybe v)
doReadRow mlim d k = forModuleNameFix $ \mnFix ->
    case d of
        DKeySets -> let f = (\v -> (view document <$> _decodeKeySet serialisePact_raw_spaninfo_better v)) in
            lookupWithKey (convKeySetNameCore k) f (noCache f)
        -- TODO: This is incomplete (the modules case), due to namespace
        -- resolution concerns
        DModules -> let f = (\v -> (view document <$> _decodeModuleData serialisePact_raw_spaninfo_better v)) in
            lookupWithKey (convModuleNameCore mnFix k) f (checkModuleCache f)
        DNamespaces -> let f = (\v -> (view document <$> _decodeNamespace serialisePact_raw_spaninfo_better v)) in
            lookupWithKey (convNamespaceNameCore k) f (noCache f)
        (DUserTables _) -> let f = (\v -> (view document <$> _decodeRowData serialisePact_raw_spaninfo_better v)) in
            lookupWithKey (convRowKeyCore k) f (noCache f)
        DDefPacts -> let f = (\v -> (view document <$> _decodeDefPactExec serialisePact_raw_spaninfo_better v)) in
            lookupWithKey (convPactIdCore k) f (noCache f)
  where
    tablename@(Utf8 tableNameBS) = domainTableNameCore d

    lookupWithKey
        :: forall logger v .
            Utf8
        -> (BS.ByteString -> Maybe v)
        -> (Utf8 -> BS.ByteString -> MaybeT (BlockHandler logger) v)
        -> BlockHandler logger (Maybe v)
    lookupWithKey key f checkCache = do
        pds <- getPendingData
        let lookPD = foldr1 (<|>) $ map (lookupInPendingData key f) pds
        let lookDB = lookupInDb key f checkCache
        runMaybeT (lookPD <|> lookDB)

    lookupInPendingData
        :: forall logger v .
            Utf8
        -> (BS.ByteString -> Maybe v)
        -> SQLitePendingData
        -> MaybeT (BlockHandler logger) v
    lookupInPendingData (Utf8 rowkey) f p = do
        -- we get the latest-written value at this rowkey
        allKeys <- hoistMaybe $ HashMap.lookup tableNameBS (_pendingWrites p)
        ddata <- _deltaData . NE.head <$> hoistMaybe (HashMap.lookup rowkey allKeys)
        MaybeT $ return $! f ddata

    lookupInDb
        :: forall logger v .
           Utf8
        -> (BS.ByteString -> Maybe v)
        -> (Utf8 -> BS.ByteString -> MaybeT (BlockHandler logger) v)
        -> MaybeT (BlockHandler logger) v
    lookupInDb rowkey _ checkCache = do
        -- First, check: did we create this table during this block? If so,
        -- there's no point in looking up the key.
        checkDbTablePendingCreation tablename
        lift $ forM_ mlim $ \(bh, _) ->
            failIfTableDoesNotExistInDbAtHeight "doReadRow" tablename bh
        -- we inject the endingtx limitation to reduce the scope up to the provided block height
        let blockLimitStmt = maybe "" (const " AND txid < ?") mlim
        let blockLimitParam = maybe [] (\(TxId txid) -> [SInt $ fromIntegral txid]) (snd <$> mlim)
        let queryStmt =
                "SELECT rowdata FROM " <> tbl tablename <> " WHERE rowkey = ?" <> blockLimitStmt
                <> " ORDER BY txid DESC LIMIT 1;"
        result <- lift $ callDb "doReadRow"
                       $ \db -> qry db queryStmt ([SText rowkey] ++ blockLimitParam) [RBlob]
        case result of
            [] -> mzero
            [[SBlob a]] -> checkCache rowkey a
            err -> internalError $
                     "doReadRow: Expected (at most) a single result, but got: " <>
                     T.pack (show err)

    checkModuleCache f u b = MaybeT $ do
        !txid <- use bsTxId -- cache priority
        mc <- use bsModuleCacheCore
        (r, mc') <- liftIO $ checkDbCache u f b txid mc
        modify' (bsModuleCacheCore .~ mc')
        return r

    noCache
        :: (BS.ByteString -> Maybe v)
        -> Utf8
        -> BS.ByteString
        -> MaybeT (BlockHandler logger) v
    noCache f _key rowdata = MaybeT $ return $! f rowdata


checkDbTablePendingCreation :: Utf8 -> MaybeT (BlockHandler logger) ()
checkDbTablePendingCreation (Utf8 tablename) = do
    pds <- lift getPendingData
    forM_ pds $ \p ->
        when (HashSet.member tablename (_pendingTableCreation p)) mzero

writeSys
    :: Domain k v CoreBuiltin SpanInfo
    -> k
    -> v
    -> BlockHandler logger ()
writeSys d k v = gets _bsTxId >>= go
  where
    go txid = do
        (kk, vv) <- forModuleNameFix $ \mnFix -> pure $ case d of
            DKeySets -> (convKeySetNameCore k, _encodeKeySet serialisePact_raw_spaninfo_better v)
            DModules ->  (convModuleNameCore mnFix k, _encodeModuleData serialisePact_raw_spaninfo_better v)
            DNamespaces -> (convNamespaceNameCore k, _encodeNamespace serialisePact_raw_spaninfo_better v)
            DDefPacts -> (convPactIdCore k, _encodeDefPactExec serialisePact_raw_spaninfo_better v)
            DUserTables _ -> error "impossible"
        recordPendingUpdate kk (toUtf8 tablename) (coerce txid) vv
        recordTxLog (Pact4.TableName tablename) d kk vv
    tablename = asString d

recordPendingUpdate
    :: Utf8
    -> Utf8
    -> PCore.TxId
    -> BS.ByteString
    -> BlockHandler logger ()
recordPendingUpdate (Utf8 key) (Utf8 tn) txid vs = modifyPendingData modf
  where
    delta = SQLiteRowDelta tn (coerce txid) key vs

    modf = over pendingWrites upd
    upd = HashMap.unionWith
        HashMap.union
        (HashMap.singleton tn
            (HashMap.singleton key (NE.singleton delta)))

checkInsertIsOK
    :: Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> WriteType
    -> Domain RowKey RowData CoreBuiltin SpanInfo
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
    err msg = internalError $ "checkInsertIsOK: " <> msg <> _rowKey k

writeUser
    :: GasMEnv (PactError SpanInfo) CoreBuiltin
    -> Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> WriteType
    -> Domain RowKey RowData CoreBuiltin SpanInfo
    -> RowKey
    -> RowData
    -> BlockHandler logger ()
writeUser gasenv mlim wt d k rowdata@(RowData row) = gets _bsTxId >>= go
  where
    tn = asString d

    go (Pact4.TxId txid) = do
        m <- checkInsertIsOK mlim wt d k
        row' <- case m of
                    Nothing -> ins
                    (Just old) -> upd old
        (liftIO $ runGasM [] def gasenv $ _encodeRowData serialisePact_raw_spaninfo_better row') >>= \case
            Left e -> internalError $ "writeUser: row encoding error: " <> sshow e
            Right encoded -> recordTxLog (Pact4.TableName tn) d (convRowKeyCore k) encoded

      where
        upd (RowData oldrow) = do
            let row' = RowData (M.union row oldrow)
            (liftIO $ runGasM [] def gasenv $ _encodeRowData serialisePact_raw_spaninfo_better row') >>= \case
                Left e -> internalError $ "writeUser.upd: row encoding error: " <> sshow e
                Right encoded -> do
                    recordPendingUpdate (convRowKeyCore k) (toUtf8 tn) (PCore.TxId txid) encoded
                    return row'

        ins = do
            (liftIO $ runGasM [] def gasenv $ _encodeRowData serialisePact_raw_spaninfo_better rowdata) >>= \case
                Left e -> internalError $ "writeUser.ins: row encoding error: " <> sshow e
                Right encoded -> do
                    recordPendingUpdate (convRowKeyCore k) (toUtf8 tn) (PCore.TxId txid) encoded
                    return rowdata

doWriteRow
  :: GasMEnv (PactError SpanInfo) CoreBuiltin
    -> Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> WriteType
    -> Domain k v CoreBuiltin SpanInfo
    -> k
    -> v
    -> BlockHandler logger ()
doWriteRow gasenv mlim wt d k v = case d of
    (DUserTables _) -> writeUser gasenv mlim wt d k v
    _ -> writeSys d k v

doKeys
    :: forall k v logger .
       Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> Domain k v CoreBuiltin SpanInfo
    -> BlockHandler logger [k]
doKeys mlim d = do
    msort <- views blockHandlerSortedKeys (\c -> if c then sort else id)
    dbKeys <- getDbKeys
    pb <- use bsPendingBlock
    mptx <- use bsPendingTx

    let memKeys = fmap (T.decodeUtf8 . _deltaRowKey)
                  $ collect pb ++ maybe [] collect mptx

    let !allKeys = msort -- becomes available with Pact42Upgrade
                  $ LHM.sort
                  $ dbKeys ++ memKeys
    case d of
        DKeySets -> do
            let parsed = map parseAnyKeysetName allKeys
            case sequence parsed of
              Left msg -> internalError $ "doKeys.DKeySets: unexpected decoding " <> T.pack msg
              Right v -> pure v
        DModules -> do
            let parsed = map parseModuleName allKeys
            case sequence parsed of
              Nothing -> internalError $ "doKeys.DModules: unexpected decoding"
              Just v -> pure v
        DNamespaces -> pure $ map NamespaceName allKeys
        DDefPacts ->  pure $ map DefPactId allKeys
        DUserTables _ -> pure $ map RowKey allKeys

  where
    blockLimitStmt = maybe "" (const " WHERE txid < ?;") mlim
    blockLimitParam = maybe [] (\(TxId txid) -> [SInt (fromIntegral txid)]) (snd <$> mlim)

    getDbKeys = do
        m <- runMaybeT $ checkDbTablePendingCreation tn
        case m of
            Nothing -> return mempty
            Just () -> do
                forM_ mlim (failIfTableDoesNotExistInDbAtHeight "doKeys" tn . fst)
                ks <- callDb "doKeys" $ \db ->
                          qry db ("SELECT DISTINCT rowkey FROM " <> tbl tn <> blockLimitStmt) blockLimitParam [RText]
                forM ks $ \row -> do
                    case row of
                        [SText k] -> return $ fromUtf8 k
                        _ -> internalError "doKeys: The impossible happened."

    tn@(Utf8 tnBS) = asStringUtf8 d
    collect p =
        concatMap NE.toList $ HashMap.elems $ fromMaybe mempty $ HashMap.lookup tnBS (_pendingWrites p)
{-# INLINE doKeys #-}

failIfTableDoesNotExistInDbAtHeight
  :: T.Text -> Utf8 -> BlockHeight -> BlockHandler logger ()
failIfTableDoesNotExistInDbAtHeight caller tn bh = do
    exists <- tableExistsInDbAtHeight tn bh
    -- we must reproduce errors that were thrown in earlier blocks from tables
    -- not existing, if this table does not yet exist.
    unless exists $
        internalError $ "callDb (" <> caller <> "): user error (Database error: ErrorError)"

-- -- tid is non-inclusive lower bound for the search
doTxIds :: TableName -> TxId -> BlockHandler logger [TxId]
doTxIds tn _tid@(TxId tid) = do
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
        m <- runMaybeT $ checkDbTablePendingCreation $ (tableNameCore tn)
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

    stmt = "SELECT DISTINCT txid FROM " <> tbl (tableNameCore tn) <> " WHERE txid > ?"

    collect p =
        let txids = fmap (coerce . _deltaTxId) $
                    concatMap NE.toList $
                    HashMap.elems $
                    fromMaybe mempty $
                    HashMap.lookup (T.encodeUtf8 $ asString tn) (_pendingWrites p)
        in filter (> _tid) txids
{-# INLINE doTxIds #-}

recordTxLog
    :: Pact4.TableName
    -> Domain k v CoreBuiltin SpanInfo
    -> Utf8
    -> BS.ByteString
    -> BlockHandler logger ()
recordTxLog tn d (Utf8 k) v = do
    -- are we in a tx?
    mptx <- use bsPendingTx
    modify' $ case mptx of
      Nothing -> over (bsPendingBlock . pendingTxLogMapCore) upd
      (Just _) -> over (bsPendingTx . _Just . pendingTxLogMapCore) upd

  where
    !upd = M.insertWith DL.append tn txlogs
    !txlogs = DL.singleton $! TxLog (asString d) (T.decodeUtf8 k) v

modifyPendingData
    :: (SQLitePendingData -> SQLitePendingData)
    -> BlockHandler logger ()
modifyPendingData f = do
    m <- use bsPendingTx
    modify' $ case m of
      Just d -> set bsPendingTx (Just $! f d)
      Nothing -> over bsPendingBlock f

doCreateUserTable
    :: GasMEnv (PactError SpanInfo) CoreBuiltin
    -> Maybe BlockHeight
    -- ^ the highest block we should be seeing tables from
    -> TableName
    -> BlockHandler logger ()
doCreateUserTable gasenv mbh tn = do
    -- first check if tablename already exists in pending queues
    -- traceShowM ("CORE", asString tn, _tableModuleName tn)
    m <- runMaybeT $ checkDbTablePendingCreation (tableNameCore tn)
    case m of
      Nothing -> throwM $ PactDuplicateTableError $ asString tn
      Just () -> do
          -- then check if it is in the db
          lcTables <- view blockHandlerLowerCaseTables
          cond <- inDb lcTables $ Utf8 $ T.encodeUtf8 $ asString tn
          when cond $ throwM $ PactDuplicateTableError $ asString tn

          (liftIO $ runGasM [] def gasenv $ _encodeRowData serialisePact_raw_spaninfo_better rd) >>= \case
            Left e -> internalError $ "doCreateUserTable: row encoding error: " <> sshow e
            Right encoded ->
              modifyPendingData
                $ over pendingTableCreation (HashSet.insert (T.encodeUtf8 $ asString tn))
                . over pendingTxLogMapCore
                  (M.insertWith DL.append (Pact4.TableName txlogKey)
                    (DL.singleton $ TxLog txlogKey (_tableName tn) encoded))
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
    rd = RowData $ M.singleton (Field "utModule")
         (PObject $ M.fromList
          [ (Field "namespace", maybe (PLiteral LUnit) (PString . _namespaceName) (_mnNamespace $ _tableModuleName tn))
          , (Field "name", PString $ _tableName tn)
          ])
{-# INLINE doCreateUserTable #-}

doRollback :: BlockHandler logger ()
doRollback = modify'
    $ set bsMode Nothing
    . set bsPendingTx Nothing

-- | Commit a Pact transaction
doCommit :: BlockHandler logger [TxLog B8.ByteString]
doCommit = use bsMode >>= \case
    Nothing -> doRollback >> internalError "doCommit: Not in transaction"
    Just m -> do
        txrs <- if m == Pact4.Transactional
          then do
              modify' $ over bsTxId succ
              -- merge pending tx into block data
              pending <- use bsPendingTx
              persistIntraBlockWrites <- view blockHandlerPersistIntraBlockWrites
              modify' $ over bsPendingBlock (merge persistIntraBlockWrites pending)
              blockLogs <- use $ bsPendingBlock . pendingTxLogMapCore
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
        , _pendingTxLogMapCore = _pendingTxLogMapCore txPending
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
            logError_ logger "CorePactDb.beginTx: In transaction, rolling back"
            doRollback
        Nothing -> return ()
    resetTemp
    modify'
        $ set bsMode (Just $ fromCoreExecutionMode m)
        . set bsPendingTx (Just emptySQLitePendingData)
    case m of
        Transactional -> Just . coerce <$> use bsTxId
        Local -> pure Nothing
{-# INLINE doBegin #-}

resetTemp :: BlockHandler logger ()
resetTemp = modify'
    $ set bsMode Nothing
    -- clear out txlog entries
    . set (bsPendingBlock . pendingTxLogMapCore) mempty

doGetTxLog :: TableName -> TxId -> BlockHandler logger [TxLog RowData]
doGetTxLog tn txid@(TxId txid') = do
    -- try to look up this tx from pending log -- if we find it there it can't
    -- possibly be in the db.
    p <- readFromPending
    if null p then readFromDb else return p

  where
    tablename@(Utf8 tableNameBS) = asStringUtf8 tn

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
                    , _deltaTxId writeForSomeKey == coerce txid
                    ]
                return latestWriteForSomeKey
        mapM (\x -> toTxLog (asString tn) (Utf8 $ _deltaRowKey x) (_deltaData x)) deltas

    readFromDb = do
        rows <- callDb "doGetTxLog" $ \db -> qry db stmt
          [SInt (fromIntegral txid')]
          [RText, RBlob]
        forM rows $ \case
            [SText key, SBlob value] -> toTxLog (asString tn) key value
            err -> internalError $
              "readHistoryResult: Expected single row with two columns as the \
              \result, got: " <> T.pack (show err)
    stmt = "SELECT rowkey, rowdata FROM " <> tbl tablename <> " WHERE txid = ?"

toTxLog :: MonadThrow m => T.Text -> Utf8 -> BS.ByteString -> m (TxLog RowData)
toTxLog d key value =
        case fmap (view document) $ _decodeRowData serialisePact_raw_spaninfo_better value of
            Nothing -> internalError $ "toTxLog: Unexpected value, unable to deserialize log: " <> sshow value
            Just v ->
              return $! TxLog d (fromUtf8 key) v

toPactTxLog :: TxLog RowData -> Pact4.TxLog RowData
toPactTxLog (TxLog d k v) = Pact4.TxLog d k v