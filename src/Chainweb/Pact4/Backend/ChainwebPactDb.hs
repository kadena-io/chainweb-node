{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- TODO pact5: fix the orphan PactDbFor instance
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Pact4.Backend.ChainwebPactDb
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact4.Backend.ChainwebPactDb
( chainwebPactDb
, rewoundPactDb
, indexPactTransaction
, vacuumDb
, toTxLog
, CurrentBlockDbEnv(..)
, cpPactDbEnv
, cpRegisterProcessedTx
, cpLookupProcessedTx
, callDb
, BlockEnv(..)
, blockHandlerEnv
, benvBlockState
, runBlockEnv
, BlockState(..)
, bsPendingBlock
, bsTxId
, initBlockState
, BlockHandler(..)
, BlockHandlerEnv(..)
, blockHandlerDb
, blockHandlerLogger
, blockHandlerBlockHeight
, blockHandlerModuleNameFix
, blockHandlerSortedKeys
, blockHandlerLowerCaseTables
, blockHandlerPersistIntraBlockWrites
, mkBlockHandlerEnv

, domainTableName
, convKeySetName
, convModuleName
, convNamespaceName
, convRowKey
, convPactId

, commitBlockStateToDatabase
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.Aeson hiding ((.=))
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
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.SQLite3.Direct as SQ3

import Prelude hiding (concat, log)

-- pact

import Pact.Persist
import Pact.PersistPactDb hiding (db)
import Pact.Types.Persistence
import Pact.Types.RowData
import Pact.Types.SQLite
import Pact.Types.Term (ModuleName(..), ObjectMap(..), TableName(..), KeySetName(..), NamespaceName(..), PactId(..))
import Pact.Types.Util (AsString(..))

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Legacy.HashMap as LHM

-- chainweb

import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Types
import Chainweb.Utils
import Chainweb.Version
import Pact.Interpreter (PactDbEnv)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Control.Concurrent
import Chainweb.Version.Guards
import Control.Exception.Safe
import Pact.Types.Command (RequestKey)
import Chainweb.Pact.Backend.Types
import Chainweb.Utils.Serialization (runPutS)
import Data.Foldable

execMulti :: Traversable t => SQ3.Database -> SQ3.Utf8 -> t [SType] -> IO ()
execMulti db q rows = bracket (prepStmt db q) destroy $ \stmt -> do
    forM_ rows $ \row -> do
        SQ3.reset stmt >>= checkError
        SQ3.clearBindings stmt
        bindParams stmt row
        SQ3.step stmt >>= checkError
  where
    checkError (Left e) = void $ fail $ "error during batch insert: " ++ show e
    checkError (Right _) = return ()

    destroy x = void (SQ3.finalize x >>= checkError)

domainTableName :: Domain k v -> Text
domainTableName = asString

convKeySetName :: KeySetName -> SQ3.Utf8
convKeySetName = toUtf8 . asString

convModuleName
  :: Bool
      -- ^ whether to apply module name fix
  -> ModuleName
  -> SQ3.Utf8
convModuleName False (ModuleName name _) = toUtf8 name
convModuleName True mn = asStringUtf8 mn

convNamespaceName :: NamespaceName -> SQ3.Utf8
convNamespaceName (NamespaceName name) = toUtf8 name

convRowKey :: RowKey -> SQ3.Utf8
convRowKey (RowKey name) = toUtf8 name

convPactId :: PactId -> SQ3.Utf8
convPactId = toUtf8 . sshow

callDb
    :: (MonadCatch m, MonadReader (BlockHandlerEnv logger) m, MonadIO m)
    => T.Text
    -> (SQ3.Database -> IO b)
    -> m b
callDb callerName action = do
  c <- asks _blockHandlerDb
  res <- tryAny $ liftIO $ action c
  case res of
    Left err -> internalError $ "callDb (" <> callerName <> "): " <> sshow err
    Right r -> return r

data BlockHandlerEnv logger = BlockHandlerEnv
    { _blockHandlerDb :: !SQLiteEnv
    , _blockHandlerLogger :: !logger
    , _blockHandlerBlockHeight :: !BlockHeight
    , _blockHandlerModuleNameFix :: !Bool
    , _blockHandlerSortedKeys :: !Bool
    , _blockHandlerLowerCaseTables :: !Bool
    , _blockHandlerPersistIntraBlockWrites :: !IntraBlockPersistence
    }

mkBlockHandlerEnv
  :: ChainwebVersion -> ChainId -> BlockHeight
  -> SQLiteEnv -> IntraBlockPersistence -> logger -> BlockHandlerEnv logger
mkBlockHandlerEnv v cid bh sql p logger = BlockHandlerEnv
    { _blockHandlerDb = sql
    , _blockHandlerLogger = logger
    , _blockHandlerBlockHeight = bh
    , _blockHandlerModuleNameFix = enableModuleNameFix v cid bh
    , _blockHandlerSortedKeys = pact42 v cid bh
    , _blockHandlerLowerCaseTables = chainweb217Pact v cid bh
    , _blockHandlerPersistIntraBlockWrites = p
    }

makeLenses ''BlockHandlerEnv

data BlockEnv logger =
  BlockEnv
    { _blockHandlerEnv :: !(BlockHandlerEnv logger)
    , _benvBlockState :: !BlockState -- ^ The current block state.
    }

runBlockEnv :: MVar (BlockEnv logger) -> BlockHandler logger a -> IO a
runBlockEnv e m = modifyMVar e $
  \(BlockEnv dbenv bs) -> do
    (!a,!s) <- runStateT (runReaderT (runBlockHandler m) dbenv) bs
    return (BlockEnv dbenv s, a)

-- this monad allows access to the database environment "at" a particular block.
-- unfortunately, this is tied to a useless MVar via runBlockEnv, which will
-- be deleted with pact 5.
newtype BlockHandler logger a = BlockHandler
    { runBlockHandler :: ReaderT (BlockHandlerEnv logger) (StateT BlockState IO) a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadState BlockState
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadIO
        , MonadReader (BlockHandlerEnv logger)
        )

-- | Monad state for 'BlockHandler.
-- This notably contains all of the information that's being mutated during
-- blocks, notably _bsPendingBlock, the pending writes in the block, and
-- _bsPendingTx, the pending writes in the transaction which will be discarded
-- on tx failure.
data BlockState = BlockState
    { _bsTxId :: !TxId
    , _bsPendingBlock :: !(SQLitePendingData (PendingWrites Pact4))
    , _bsPendingTx :: !(Maybe (SQLitePendingData (PendingWrites Pact4)))
    , _bsMode :: !(Maybe ExecutionMode)
    , _bsModuleCache :: !(DbCache PersistModuleData)
    }
initBlockState
    :: DbCacheLimitBytes
    -- ^ Module Cache Limit (in bytes of corresponding rowdata)
    -> TxId
    -- ^ next tx id (end txid of previous block)
    -> BlockState
initBlockState cl txid = BlockState
    { _bsTxId = txid
    , _bsMode = Nothing
    , _bsPendingBlock = emptySQLitePendingData mempty
    , _bsPendingTx = Nothing
    , _bsModuleCache = emptyDbCache cl
    }

makeLenses ''BlockEnv
makeLenses ''BlockState


-- this is effectively a read-write snapshot of the Pact state at a block.
data CurrentBlockDbEnv logger = CurrentBlockDbEnv
    { _cpPactDbEnv :: !(PactDbEnv (BlockEnv logger))
    , _cpRegisterProcessedTx :: !(RequestKey -> IO ())
    , _cpLookupProcessedTx ::
        !(Vector RequestKey -> IO (HashMap RequestKey (T2 BlockHeight BlockHash)))
    }
makeLenses ''CurrentBlockDbEnv

type instance PactDbFor logger Pact4 = CurrentBlockDbEnv logger

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
getPendingData :: BlockHandler logger [SQLitePendingData (PendingWrites Pact4)]
getPendingData = do
    pb <- use bsPendingBlock
    ptx <- maybeToList <$> use bsPendingTx
    -- lookup in pending transactions first
    return $ ptx ++ [pb]

forModuleNameFix :: (Bool -> BlockHandler logger a) -> BlockHandler logger a
forModuleNameFix f = view blockHandlerModuleNameFix >>= f

-- TODO: speed this up, cache it?
tableExistsInDbAtHeight :: Text -> BlockHeight -> BlockHandler logger Bool
tableExistsInDbAtHeight tableName bh = do
    let knownTbls =
          ["SYS:Pacts", "SYS:Modules", "SYS:KeySets", "SYS:Namespaces", "SYS:ModuleSources"]
    if tableName `elem` knownTbls
    then return True
    else callDb "tableExists" $ \db -> do
      let tableExistsStmt =
            -- table names are case-sensitive
            "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight < ? AND lower(tablename) = lower(?)"
      qry db tableExistsStmt [SInt $ max 0 (fromIntegral bh), SText (toUtf8 tableName)] [RText] >>= \case
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
        -> SQLitePendingData (PendingWrites Pact4)
        -> MaybeT (BlockHandler logger) v
    lookupInPendingData (Utf8 rowkey) p = do
        -- we get the latest-written value at this rowkey
        allKeys <- hoistMaybe $ HashMap.lookup tableName (_pendingWrites p)
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
                "SELECT rowdata FROM " <> tbl (toUtf8 tableName) <> " WHERE rowkey = ?" <> blockLimitStmt
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
        (r, mc') <- liftIO $ checkDbCache u decodeStrict b txid mc
        modify' (bsModuleCache .~ mc')
        return r

    noCache
        :: FromJSON v
        => Utf8
        -> BS.ByteString
        -> MaybeT (BlockHandler logger) v
    noCache _key rowdata = MaybeT $ return $! decodeStrict' rowdata


checkDbTablePendingCreation :: Text -> MaybeT (BlockHandler logger) ()
checkDbTablePendingCreation tableName = do
    pds <- lift getPendingData
    forM_ pds $ \p ->
        when (HashSet.member tableName (_pendingTableCreation p)) mzero

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
        recordTxLog (TableName tableName) d k v

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
    -> Text
    -> TxId
    -> v
    -> BlockHandler logger ()
recordPendingUpdate (Utf8 key) tn txid v = modifyPendingData modf
  where
    !vs = J.encodeStrict v
    delta = SQLiteRowDelta tn txid key vs

    modf = over pendingWrites upd
    upd = HashMap.unionWith
        HashMap.union
        (HashMap.singleton tn
            (HashMap.singleton key (NE.singleton delta)))


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
    tn = domainTableName d
    ttn = TableName tn

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
        m <- runMaybeT $ checkDbTablePendingCreation $ tn
        case m of
            Nothing -> return mempty
            Just () -> do
                forM_ mlim (failIfTableDoesNotExistInDbAtHeight "doKeys" tn . fst)
                ks <- callDb "doKeys" $ \db ->
                          qry db ("SELECT DISTINCT rowkey FROM " <> tbl (toUtf8 tn) <> blockLimitStmt) blockLimitParam [RText]
                forM ks $ \row -> do
                    case row of
                        [SText k] -> return $! T.unpack $ fromUtf8 k
                        _ -> internalError "doKeys: The impossible happened."

    tn = domainTableName d
    collect p =
        concatMap NE.toList $ HashMap.elems $ fromMaybe mempty $ HashMap.lookup tn (_pendingWrites p)
{-# INLINE doKeys #-}

failIfTableDoesNotExistInDbAtHeight
  :: Text -> Text -> BlockHeight -> BlockHandler logger ()
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
        m <- runMaybeT $ checkDbTablePendingCreation tn
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

    collect p =
        let txids = fmap _deltaTxId $
                    concatMap NE.toList $
                    HashMap.elems $
                    fromMaybe mempty $
                    HashMap.lookup tn (_pendingWrites p)
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
    :: (SQLitePendingData (PendingWrites Pact4) -> SQLitePendingData (PendingWrites Pact4))
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
    m <- runMaybeT $ checkDbTablePendingCreation ttxt
    case m of
      Nothing -> throwM $ PactDuplicateTableError ttxt
      Just () -> do
          -- then check if it is in the db
          lcTables <- view blockHandlerLowerCaseTables
          cond <- inDb lcTables ttxt
          when cond $ throwM $ PactDuplicateTableError ttxt
          modifyPendingData
            $ over pendingTableCreation (HashSet.insert ttxt)
            . over pendingTxLogMap (M.insertWith DL.append (TableName txlogKey) txlogs)
  where
    inDb lcTables t = do
      r <- callDb "doCreateUserTable" $ \db ->
        qry db (tableLookupStmt lcTables) [SText (toUtf8 t)] [RText]
      case r of
        [[SText (Utf8 (T.decodeUtf8 -> rname))]] ->
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
              -- this is mostly a lie; the previous `merge` call has already replaced the tx
              -- logs from bsPendingBlock with those of the transaction.
              -- from what I can tell, it's impossible for `pending` to be `Nothing` here,
              -- but we don't throw an error for it.
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
            logError_ logger "PactDb.beginTx: In transaction, rolling back"
            doRollback
        Nothing -> return ()
    resetTemp
    modify'
        $ set bsMode (Just m)
        . set bsPendingTx (Just $ emptySQLitePendingData mempty)
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

    readFromPending = do
        allPendingData <- getPendingData
        let deltas = do
                -- grab all pending writes in this transaction and elsewhere in
                -- this block
                pending <- allPendingData
                -- all writes to the table
                let writesAtTableByKey =
                        fromMaybe mempty $ HashMap.lookup tableName $ _pendingWrites pending
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
        mapM (\x -> toTxLog (asString d) (Utf8 $ _deltaRowKey x) (_deltaData x)) deltas

    readFromDb = do
        rows <- callDb "doGetTxLog" $ \db -> qry db stmt
          [SInt (fromIntegral txid)]
          [RText, RBlob]
        forM rows $ \case
            [SText key, SBlob value] -> toTxLog (asString d) key value
            err -> internalError $
              "readHistoryResult: Expected single row with two columns as the \
              \result, got: " <> T.pack (show err)
    stmt = "SELECT rowkey, rowdata FROM " <> tbl (toUtf8 tableName) <> " WHERE txid = ?"


toTxLog :: MonadThrow m =>
           T.Text -> Utf8 -> BS.ByteString -> m (TxLog RowData)
toTxLog d key value =
      case Data.Aeson.decodeStrict' value of
            Nothing -> internalError $ "toTxLog: Unexpected value, unable to deserialize log: " <> T.decodeUtf8 value
            Just v ->
              return $! TxLog d (fromUtf8 key) v

-- | Register a successful transaction in the pending data for the block
indexPactTransaction :: BS.ByteString -> BlockHandler logger ()
indexPactTransaction h = modify' $
    over (bsPendingBlock . pendingSuccessfulTxs) $ HashSet.insert h


-- | Careful doing this! It's expensive and for our use case, probably pointless.
-- We should reserve vacuuming for an offline process
vacuumDb :: BlockHandler logger ()
vacuumDb = callDb "vacuumDb" (`exec_` "VACUUM;")

commitBlockStateToDatabase :: SQLiteEnv -> BlockHash -> BlockHeight -> BlockHandle Pact4 -> IO ()
commitBlockStateToDatabase db hsh bh blockHandle = do
  let newTables = _pendingTableCreation $ _blockHandlePending blockHandle
  mapM_ (\tn -> createUserTable tn) newTables
  let writeV = toChunks $ _pendingWrites (_blockHandlePending blockHandle)
  backendWriteUpdateBatch writeV
  indexPendingPactTransactions
  let nextTxId = _blockHandleTxId blockHandle
  blockHistoryInsert nextTxId
  where
    toChunks writes =
      over _2 (concatMap toList . HashMap.elems) .
      over _1 toUtf8 <$> HashMap.toList writes

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
            markTableMutation tableName bh
            where
            q = "INSERT OR REPLACE INTO " <> tbl tableName <> "(rowkey,txid,rowdata) VALUES(?,?,?)"

          -- Mark the table as being mutated during this block, so that we know
          -- to delete from it if we rewind past this block.
          markTableMutation tablename blockheight = do
              exec' db mutq [SText tablename, SInt (fromIntegral blockheight)]
            where
              mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"

    -- | Record a block as being in the history of the checkpointer.
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

    createUserTable :: Text -> IO ()
    createUserTable (toUtf8 -> tablename) = do
        createVersionedTable tablename db
        markTableCreation tablename

    -- Mark the table as being created during this block, so that we know
    -- to drop it if we rewind past this block.
    markTableCreation tablename =
        exec' db insertstmt insertargs
      where
        insertstmt = "INSERT OR IGNORE INTO VersionedTableCreation VALUES (?,?)"
        insertargs =  [SText tablename, SInt (fromIntegral bh)]

    -- | Commit the index of pending successful transactions to the database
    indexPendingPactTransactions :: IO ()
    indexPendingPactTransactions = do
        let txs = _pendingSuccessfulTxs $ _blockHandlePending blockHandle
        dbIndexTransactions txs

      where
        toRow b = [SBlob b, SInt (fromIntegral bh)]
        dbIndexTransactions txs = do
            let rows = map toRow $ toList txs
            execMulti db "INSERT INTO TransactionIndex (txhash, blockheight) \
                         \ VALUES (?, ?)" rows


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
