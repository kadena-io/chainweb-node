{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}


module Chainweb.Pact5.Backend.ChainwebPactDb
    ( chainwebPactCoreBlockDb
    , Pact5Db(..)
    , BlockHandlerEnv(..)
    , toTxLog
    , toPactTxLog
    , getEndTxId
    , getEndTxId'
    ) where

import Data.Coerce
import Control.Applicative
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Morph
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
import Pact.Types.SQLite hiding (liftEither)
import Pact.Types.Util (AsString(..))
import qualified Pact.Types.Term as Pact4


import Pact.Core.Evaluate
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

import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Types
import Chainweb.Utils (sshow, T2)
import Pact.Core.StableEncoding (encodeStable)
import Data.Text (Text)
import Chainweb.Version
import qualified Pact.Core.Persistence as Pact5
import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Evaluate as Pact5
import Data.IORef
import qualified Database.SQLite3 as SQ3
import Control.Exception.Safe (tryAny)
import Chainweb.Version.Guards (enableModuleNameFix, chainweb217Pact, pact42)
import Data.DList (DList)
import Data.ByteString (ByteString)
import Chainweb.BlockHeader
import Chainweb.BlockHash
import Chainweb.Utils.Serialization
import Data.ByteString.Short (ShortByteString)
import Data.Vector (Vector)
import qualified Data.ByteString.Short as SB
import Data.HashMap.Strict (HashMap)
import Pact.Core.Command.Types (RequestKey (..))
import Pact.Core.Hash
import qualified Data.HashMap.Strict as HM

data BlockHandlerEnv logger = BlockHandlerEnv
    { _blockHandlerDb :: !SQLiteEnv
    , _blockHandlerLogger :: !logger
    , _blockHandlerVersion :: !ChainwebVersion
    , _blockHandlerBlockHeight :: !BlockHeight
    , _blockHandlerChainId :: !ChainId
    , _blockHandlerMode :: !ExecutionMode
    , _blockHandlerPersistIntraBlockWrites :: !IntraBlockPersistence
    }
data BlockState = BlockState
    { _bsBlockHandle :: !BlockHandle
    , _bsPendingTx :: !(Maybe (SQLitePendingData, DList (Pact5.TxLog ByteString)))
    }

makeLenses ''BlockState
makeLenses ''BlockHandlerEnv

bsPendingTxOrError :: HasCallStack => Text -> Lens' BlockState (SQLitePendingData, DList (Pact5.TxLog ByteString))
bsPendingTxOrError msg = lens
    (\bs -> case _bsPendingTx bs of
        Nothing -> impureThrow (NotInTx msg)
        Just t -> t
    )
    (\bs p -> set bsPendingTx (Just p) bs)

data Pact5Db = Pact5Db
    { doPact5DbTransaction
        :: forall a
        . BlockHandle
        -> Maybe RequestKey
        -> (Pact5.PactDb Pact5.CoreBuiltin Pact5.Info -> IO a)
        -> IO (a, BlockHandle)
    , lookupPactTransactions :: Vector RequestKey -> IO (HashMap RequestKey (T2 BlockHeight BlockHash))
    }

type instance PactDbFor logger Pact5 = Pact5Db

-- this monad allows access to the database environment "at" a particular block,
-- and allows charging gas.
newtype BlockHandler logger a = BlockHandler
    { runBlockHandler
        :: ReaderT (BlockHandlerEnv logger)
            (StateT BlockState (GasM CoreBuiltin Info)) a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadState BlockState
        , MonadThrow
        , MonadIO
        , MonadReader (BlockHandlerEnv logger)
        )

callDb
    :: (MonadThrow m, MonadReader (BlockHandlerEnv logger) m, MonadIO m)
    => T.Text
    -> (SQ3.Database -> IO b)
    -> m b
callDb callerName action = do
    c <- asks _blockHandlerDb
    res <- liftIO $ tryAny $ action c
    case res of
        Left err -> internalError $ "callDb (" <> callerName <> "): " <> sshow err
        Right r -> return r

liftGas :: GasM CoreBuiltin Info a -> BlockHandler logger a
liftGas g = BlockHandler (lift (lift g))

runOnBlockGassed :: BlockHandlerEnv logger -> MVar BlockState -> BlockHandler logger a -> GasM CoreBuiltin Info a
runOnBlockGassed env stateVar act = do
    ge <- ask
    r <- liftIO $ modifyMVar stateVar $ \state -> do
        r <- runExceptT (runReaderT (runGasM (runStateT (runReaderT (runBlockHandler act) env) state)) ge)
        let newState = either (\_ -> state) snd r
        return (newState, fmap fst r)
    liftEither r

runOnBlock :: BlockHandlerEnv logger -> MVar BlockState -> BlockHandler logger a -> IO a
runOnBlock env stateVar = ignoreGas def . runOnBlockGassed env stateVar

chainwebPactCoreBlockDb :: (Logger logger) => Maybe (BlockHeight, TxId) -> ExecutionMode -> BlockHandlerEnv logger -> Pact5Db
chainwebPactCoreBlockDb maybeLimit mode env = Pact5Db
    { doPact5DbTransaction = \blockHandle maybeRequestKey kont -> do
        stateVar <- newMVar $ BlockState blockHandle Nothing
        let basePactDb = PactDb
                { _pdbPurity = PImpure
                , _pdbRead = \d k -> runOnBlock env stateVar $ doReadRow Nothing d k
                , _pdbWrite = \wt d k v ->
                    runOnBlockGassed env stateVar $ doWriteRow Nothing wt d k v
                , _pdbKeys = \d ->
                    runOnBlock env stateVar $ doKeys Nothing d
                , _pdbCreateUserTable = \tn ->
                    runOnBlockGassed env stateVar $ doCreateUserTable Nothing tn
                , _pdbBeginTx = \m ->
                    runOnBlock env stateVar $ doBegin m
                , _pdbCommitTx =
                    runOnBlock env stateVar doCommit
                , _pdbRollbackTx =
                    runOnBlock env stateVar doRollback
                }
        let maybeLimitedPactDb = case maybeLimit of
                Just (bh, endTxId) -> basePactDb
                    { _pdbRead = \d k -> runOnBlock env stateVar $ doReadRow (Just (bh, endTxId)) d k
                    , _pdbWrite = \wt d k v -> do
                        runOnBlockGassed env stateVar $ doWriteRow (Just (bh, endTxId)) wt d k v
                    , _pdbKeys = \d -> runOnBlock env stateVar $ doKeys (Just (bh, endTxId)) d
                    , _pdbCreateUserTable = \tn -> do
                        runOnBlockGassed env stateVar $ doCreateUserTable (Just bh) tn
                    }
                Nothing -> basePactDb
        r <- kont maybeLimitedPactDb
        finalState <- readMVar stateVar
        when (isJust (_bsPendingTx finalState)) $
            internalError "dangling transaction"
        -- | Register a successful transaction in the pending data for the block
        let registerRequestKey = case maybeRequestKey of
                Just requestKey -> HashSet.insert (SB.fromShort $ unHash $ unRequestKey requestKey)
                Nothing -> id
        let finalHandle =
                _bsBlockHandle finalState & blockHandlePending . pendingSuccessfulTxs %~ registerRequestKey

        return (r, finalHandle)
    , lookupPactTransactions =
        fmap (HM.mapKeys (RequestKey . Hash)) .
        doLookupSuccessful (_blockHandlerDb env) (_blockHandlerBlockHeight env) .
        fmap (unHash . unRequestKey)
    }

getPendingData :: HasCallStack => Text -> BlockHandler logger [SQLitePendingData]
getPendingData msg = do
    BlockHandle _ sql <- use bsBlockHandle
    s <- get
    let ptx = s ^. bsPendingTxOrError msg . _1
    -- lookup in pending transactions first
    return $ [ptx, sql]

forModuleNameFix :: (Bool -> BlockHandler logger a) -> BlockHandler logger a
forModuleNameFix f = do
    v <- view blockHandlerVersion
    cid <- view blockHandlerChainId
    bh <- view blockHandlerBlockHeight
    f (enableModuleNameFix v cid bh)

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
    -> Domain k v CoreBuiltin Info
    -> k
    -> BlockHandler logger (Maybe v)
doReadRow mlim d k = forModuleNameFix $ \mnFix ->
    case d of
        DKeySets -> let f = (\v -> (view document <$> _decodeKeySet serialisePact_raw_spaninfo v)) in
            lookupWithKey (convKeySetNameCore k) f (noCache f)
        -- TODO: This is incomplete (the modules case), due to namespace
        -- resolution concerns
        DModules -> let f = (\v -> (view document <$> _decodeModuleData serialisePact_raw_spaninfo v)) in
            lookupWithKey (convModuleNameCore mnFix k) f (noCache f)
        DNamespaces -> let f = (\v -> (view document <$> _decodeNamespace serialisePact_raw_spaninfo v)) in
            lookupWithKey (convNamespaceNameCore k) f (noCache f)
        DUserTables _ -> let f = (\v -> (view document <$> _decodeRowData serialisePact_raw_spaninfo v)) in
            lookupWithKey (convRowKeyCore k) f (noCache f)
        DDefPacts -> let f = (\v -> (view document <$> _decodeDefPactExec serialisePact_raw_spaninfo v)) in
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
        pds <- getPendingData "read"
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
        checkDbTablePendingCreation "read" tablename
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

    noCache
        :: (BS.ByteString -> Maybe v)
        -> Utf8
        -> BS.ByteString
        -> MaybeT (BlockHandler logger) v
    noCache f _key rowdata = MaybeT $ return $! f rowdata


checkDbTablePendingCreation :: Text -> Utf8 -> MaybeT (BlockHandler logger) ()
checkDbTablePendingCreation msg (Utf8 tablename) = do
    pds <- lift (getPendingData msg)
    forM_ pds $ \p ->
        when (HashSet.member tablename (_pendingTableCreation p)) mzero

latestTxId :: Lens' BlockState TxId
latestTxId = bsBlockHandle . blockHandleTxId . coerced

writeSys
    :: Domain k v CoreBuiltin Info
    -> k
    -> v
    -> BlockHandler logger ()
writeSys d k v = do
  txid <- use latestTxId
  (kk, vv) <- forModuleNameFix $ \mnFix -> pure $ case d of
      DKeySets -> (convKeySetNameCore k, _encodeKeySet serialisePact_raw_spaninfo v)
      DModules ->  (convModuleNameCore mnFix k, _encodeModuleData serialisePact_raw_spaninfo v)
      DNamespaces -> (convNamespaceNameCore k, _encodeNamespace serialisePact_raw_spaninfo v)
      DDefPacts -> (convPactIdCore k, _encodeDefPactExec serialisePact_raw_spaninfo v)
      DUserTables _ -> error "impossible"
  recordPendingUpdate kk (toUtf8 tablename) txid vv
  recordTxLog tablename d kk vv
    where
    tablename = asString d

recordPendingUpdate
    :: Utf8
    -> Utf8
    -> PCore.TxId
    -> BS.ByteString
    -> BlockHandler logger ()
recordPendingUpdate (Utf8 key) (Utf8 tn) txid vs = modifyPendingData "write" modf
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
    -> Domain RowKey RowData CoreBuiltin Info
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
    :: Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> WriteType
    -> Domain RowKey RowData CoreBuiltin Info
    -> RowKey
    -> RowData
    -> BlockHandler logger ()
writeUser mlim wt d k rowdata@(RowData row) = do
    Pact5.TxId txid <- use latestTxId
    m <- checkInsertIsOK mlim wt d k
    row' <- case m of
        Nothing -> ins txid
        Just old -> upd txid old
    liftGas (_encodeRowData serialisePact_raw_spaninfo row') >>=
        \encoded -> recordTxLog tn d (convRowKeyCore k) encoded
  where
  tn = asString d

  upd txid (RowData oldrow) = do
      let row' = RowData (M.union row oldrow)
      liftGas (_encodeRowData serialisePact_raw_spaninfo row') >>=
          \encoded -> do
              recordPendingUpdate (convRowKeyCore k) (toUtf8 tn) (PCore.TxId txid) encoded
              return row'

  ins txid = do
      liftGas (_encodeRowData serialisePact_raw_spaninfo rowdata) >>=
          \encoded -> do
              recordPendingUpdate (convRowKeyCore k) (toUtf8 tn) (PCore.TxId txid) encoded
              return rowdata

doWriteRow
    :: Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> WriteType
    -> Domain k v CoreBuiltin Info
    -> k
    -> v
    -> BlockHandler logger ()
doWriteRow mlim wt d k v = case d of
    (DUserTables _) -> writeUser mlim wt d k v
    _ -> writeSys d k v

doKeys
    :: forall k v logger .
       Maybe (BlockHeight, TxId)
    -- ^ the highest block we should be reading writes from
    -> Domain k v CoreBuiltin Info
    -> BlockHandler logger [k]
doKeys mlim d = do
    msort <- asks $ \e ->
        if pact42 (_blockHandlerVersion e) (_blockHandlerChainId e) (_blockHandlerBlockHeight e)
        then sort
        else id
    dbKeys <- getDbKeys
    pb <- use (bsBlockHandle . blockHandlePending)
    (mptx, _) <- use (bsPendingTxOrError "keys")

    let memKeys = fmap (T.decodeUtf8 . _deltaRowKey)
                  $ collect pb ++ collect mptx

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
        m <- runMaybeT $ checkDbTablePendingCreation "keys" tn
        case m of
            Nothing -> return mempty
            Just () -> do
                forM_ mlim (failIfTableDoesNotExistInDbAtHeight "doKeys" tn . fst)
                ks <- callDb "doKeys" $ \db ->
                        qry db ("SELECT DISTINCT rowkey FROM " <> tbl tn <> blockLimitStmt <> " ORDER BY rowkey") blockLimitParam [RText]
                forM ks $ \row -> do
                    case row of
                        [SText k] -> return $ fromUtf8 k
                        _ -> internalError "doKeys: The impossible happened."

    tn@(Utf8 tnBS) = asStringUtf8 d
    collect p =
        concatMap NE.toList $ HashMap.elems $ fromMaybe mempty $ HashMap.lookup tnBS (_pendingWrites p)

failIfTableDoesNotExistInDbAtHeight
    :: T.Text -> Utf8 -> BlockHeight -> BlockHandler logger ()
failIfTableDoesNotExistInDbAtHeight caller tn bh = do
    exists <- tableExistsInDbAtHeight tn bh
    -- we must reproduce errors that were thrown in earlier blocks from tables
    -- not existing, if this table does not yet exist.
    unless exists $
        internalError $ "callDb (" <> caller <> "): user error (Database error: ErrorError)"

recordTxLog
    :: Text
    -> Domain k v CoreBuiltin Info
    -> Utf8
    -> BS.ByteString
    -> BlockHandler logger ()
recordTxLog tn d (Utf8 k) v = do
    -- are we in a tx? if not, error.
    (pendingSQLite, txlogs) <- use (bsPendingTxOrError "write")
    modify' (bsPendingTx .~ Just (pendingSQLite, DL.snoc txlogs newLog))

  where
    !newLog = TxLog (renderDomain d) (T.decodeUtf8 k) v

recordTableCreationTxLog :: TableName -> BlockHandler logger ()
recordTableCreationTxLog tn = do
    (pendingSQLite, txlogs) <- use (bsPendingTxOrError "create table")
    modify' $ bsPendingTx .~ Just (pendingSQLite, DL.snoc txlogs newLog)
    where
    !newLog = TxLog "SYS:usertables" (_tableName tn) (encodeStable uti)
    !uti = UserTableInfo (_tableModuleName tn)

modifyPendingData
    :: Text
    -> (SQLitePendingData -> SQLitePendingData)
    -> BlockHandler logger ()
modifyPendingData msg f = do
    (pending, txlogs) <- use (bsPendingTxOrError msg)
    modify' $ set bsPendingTx $ Just (f pending, txlogs)

doCreateUserTable
    :: Maybe BlockHeight
    -- ^ the highest block we should be seeing tables from
    -> TableName
    -> BlockHandler logger ()
doCreateUserTable mbh tn = do
    -- first check if tablename already exists in pending queues
    m <- runMaybeT $ checkDbTablePendingCreation "create table" (tableNameCore tn)
    case m of
        Nothing -> throwM $ PactDuplicateTableError $ asString tn
        Just () -> do
            -- then check if it is in the db
            lcTables <- asks $ \e ->
                chainweb217Pact
                (_blockHandlerVersion e)
                (_blockHandlerChainId e)
                (_blockHandlerBlockHeight e)
            cond <- inDb lcTables $ Utf8 $ T.encodeUtf8 $ asString tn
            let uti = UserTableInfo (_tableModuleName tn)
            when cond $ throwM $ PactDuplicateTableError $ asString tn

            modifyPendingData "create table"
                $ over pendingTableCreation (HashSet.insert (T.encodeUtf8 $ asString tn))
            recordTableCreationTxLog tn
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

doRollback :: BlockHandler logger ()
doRollback = modify'
    $ set bsPendingTx Nothing

-- | Commit a Pact transaction
doCommit :: BlockHandler logger [TxLog B8.ByteString]
doCommit = view blockHandlerMode >>= \case
    m -> do
        txrs <- if m == Transactional
        then do
            modify' $ over latestTxId (\(TxId tid) -> TxId (succ tid))
            pending <- use (bsPendingTxOrError "commit")
            persistIntraBlockWrites <- view blockHandlerPersistIntraBlockWrites
            -- merge pending tx into pending block data
            modify' $ over (bsBlockHandle . blockHandlePending) (merge persistIntraBlockWrites (fst pending))
            let txLogs = snd pending
            modify' $ set bsPendingTx Nothing
            return txLogs
        else doRollback >> return mempty
        return $! DL.toList txrs
    where
    merge persistIntraBlockWrites txPending blockPending = SQLitePendingData
        { _pendingTableCreation = HashSet.union (_pendingTableCreation txPending) (_pendingTableCreation blockPending)
        , _pendingWrites = HashMap.unionWith (HashMap.unionWith mergeAtRowKey) (_pendingWrites txPending) (_pendingWrites blockPending)
        -- pact4-specific, txlogs are not stored in SQLitePendingData in pact5
        , _pendingTxLogMap = mempty
        , _pendingSuccessfulTxs = _pendingSuccessfulTxs blockPending
        }
        where
        mergeAtRowKey txWrites blockWrites =
            let lastTxWrite = NE.head txWrites
            in case persistIntraBlockWrites of
                PersistIntraBlockWrites -> lastTxWrite `NE.cons` blockWrites
                DoNotPersistIntraBlockWrites -> lastTxWrite :| []

-- | Begin a Pact transaction
doBegin :: (Logger logger) => ExecutionMode -> BlockHandler logger (Maybe TxId)
doBegin m = do
    logger <- view blockHandlerLogger
    use bsPendingTx >>= \case
        Just _ -> do
            txid <- use latestTxId
            throwM (TxAlreadyBegun ("TxId " <> sshow (_txId txid)))
        Nothing -> do
            modify'
                $ set bsPendingTx (Just (emptySQLitePendingData, mempty))
            Just <$> use latestTxId

toTxLog :: MonadThrow m => T.Text -> Utf8 -> BS.ByteString -> m (TxLog RowData)
toTxLog d key value =
        case fmap (view document) $ _decodeRowData serialisePact_raw_spaninfo value of
            Nothing -> internalError $ "toTxLog: Unexpected value, unable to deserialize log: " <> sshow value
            Just v -> return $! TxLog d (fromUtf8 key) v

toPactTxLog :: TxLog RowData -> Pact4.TxLog RowData
toPactTxLog (TxLog d k v) = Pact4.TxLog d k v

getEndTxId :: Text -> SQLiteEnv -> Maybe ParentHeader -> IO (Historical TxId)
getEndTxId msg sql pc = case pc of
    Nothing -> return (Historical (TxId 0))
    Just (ParentHeader ph) -> getEndTxId' msg sql (_blockHeight ph) (_blockHash ph)

getEndTxId' :: Text -> SQLiteEnv -> BlockHeight -> BlockHash -> IO (Historical TxId)
getEndTxId' msg sql bh bhsh = do
    qry sql
        "SELECT endingtxid FROM BlockHistory WHERE blockheight = ? and hash = ?;"
        [ SInt $ fromIntegral bh
        , SBlob $ runPutS (encodeBlockHash bhsh)
        ]
        [RInt] >>= \case
        [[SInt tid]] -> return $ Historical (TxId (fromIntegral tid))
        [] -> return NoHistory
        r -> internalError $ msg <> ".getEndTxId: expected single-row int result, got " <> sshow r
