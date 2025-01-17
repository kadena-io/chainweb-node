{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
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

-- TODO pact5: fix the orphan PactDbFor instance
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The database operations that manipulate and read the Pact state.

-- Note: [Pact Transactions]

-- What is a "pact transaction"? There are three levels of transactions going on:
-- +-------------------------------------------------------------------------------------------------------------------------+
-- | Block                                                                                                                   |
-- |                                                                                                                         |
-- | +-----------------+ +---------------------------------------------+   +-----------------------------------------------+ |
-- | | Apply Coinbase  | | Apply Command                               |   | Apply Command                                 | |
-- | | |               | |+------------+ +------------+ +------------+ |   |+------------+ +------------+ +------------+   | |
-- | | |               | || Buy        | | Run        | | Redeem     | |   || Buy        | | Run        | | Redeem     |   | |
-- | | v               | || Gas        | | Payload    | | Gas        | |   || Gas        | | Payload    | | Gas        |   | |
-- | | Pact tx         | || |          | | |          | | |          | |   || |          | | |          | | |          |   | |
-- | | (begin-tx)      | || v          | | v          | | v          | +-->|| v          | | v          | | v          |   | |
-- | +v----------------+ || Pact tx    | | Pact tx    | | Pact tx    | |   || Pact tx    | | Pact tx    | | Pact tx    |   | |
-- |  Pact5Db tx         || (begin-tx) | | (begin-tx) | | (begin-tx) | |   || (begin-tx) | | (begin-tx) | | (begin-tx) |   | |
-- |                     ||            | |            | |            | |   ||            | |            | |            |   | |
-- |                     |+------------+ +------------+ +------------+ |   |+------------+ +------------+ +------------+   | |
-- |                     |                                             |   |                                               | |
-- |                     +v--------------------------------------------+   +v----------------------------------------------+ |
-- |                      Pact5Db tx                                        Pact5Db tx                                       |
-- +v------------------------------------------------------------------------------------------------------------------------+
--  SQLite tx (withSavepoint)
--    (in some cases multiple blocks in tx)
--
--
-- Transactions must be nested in this way.
--
-- SQLite transaction ensures that the Pact5Db transaction
-- sees a consistent view of the database, especially if its
-- writes are committed later.
--
-- Pact5Db tx ensures that the Pact tx's writes
-- are recorded.
--
-- Pact tx ensures that failed transactions' writes are not recorded.


module Chainweb.Pact5.Backend.ChainwebPactDb
    ( chainwebPactBlockDb
    , Pact5Db(..)
    , BlockHandlerEnv(..)
    , blockHandlerDb
    , blockHandlerLogger
    , toTxLog
    , toPactTxLog
    , domainTableName
    , convRowKey
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import Data.Default
import qualified Database.SQLite3.Direct as SQ3

import GHC.Stack

import Prelude hiding (concat, log)

-- pact

import qualified Pact.Types.Persistence as Pact4
import Pact.Types.SQLite hiding (liftEither)


import qualified Pact.Core.Evaluate as Pact
import qualified Pact.Core.Guards as Pact
import qualified Pact.Core.Names as Pact
import qualified Pact.Core.Persistence as Pact
import qualified Pact.Core.Serialise as Pact
import qualified Pact.Core.Builtin as Pact
import qualified Pact.Core.Errors as Pact
import qualified Pact.Core.Gas as Pact

-- chainweb

import Chainweb.BlockHeight
import Chainweb.Logger

import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.Types
import Chainweb.Utils (sshow, T2)
import Pact.Core.StableEncoding (encodeStable)
import Data.Text (Text)
import Chainweb.Version
import Data.DList (DList)
import Data.ByteString (ByteString)
import Chainweb.BlockHash
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
    , _blockHandlerMode :: !Pact.ExecutionMode
    , _blockHandlerPersistIntraBlockWrites :: !IntraBlockPersistence
    }

-- | The state used by database operations.
-- Includes both the state re: the whole block, and the state re: a transaction in progress.
data BlockState = BlockState
    { _bsBlockHandle :: !BlockHandle
    , _bsPendingTx :: !(Maybe (SQLitePendingData, DList (Pact.TxLog ByteString)))
    }

makeLenses ''BlockState
makeLenses ''BlockHandlerEnv

getPendingTxOrError :: Text -> BlockHandler logger (SQLitePendingData, DList (Pact.TxLog ByteString))
getPendingTxOrError msg = do
    use bsPendingTx >>= \case
        Nothing -> liftGas $ Pact.throwDbOpErrorGasM (Pact.NotInTx msg)
        Just t -> return t

-- | The Pact 5 database as it's provided by the checkpointer.
data Pact5Db = Pact5Db
    { doPact5DbTransaction
        :: forall a
        . BlockHandle
        -> Maybe RequestKey
        -> (Pact.PactDb Pact.CoreBuiltin Pact.Info -> IO a)
        -> IO (a, BlockHandle)
        -- ^ Give this function a BlockHandle representing the state of a pending
        -- block and it will pass you a PactDb which contains the Pact state as of
        -- that point in the block. After you're done, it passes you back a
        -- BlockHandle representing the state of the block extended with any writes
        -- you made to the PactDb.
        -- Note also that this function handles registering
        -- transactions as completed, if you pass it a RequestKey.
    , lookupPactTransactions :: Vector RequestKey -> IO (HashMap RequestKey (T2 BlockHeight BlockHash))
        -- ^ Used to implement transaction polling.
    }

type instance PactDbFor logger Pact5 = Pact5Db

-- this monad allows access to the database environment "in" a particular
-- transaction, and allows charging gas for database operations.
newtype BlockHandler logger a = BlockHandler
    { runBlockHandler
        :: ReaderT (BlockHandlerEnv logger)
            (StateT BlockState (Pact.GasM Pact.CoreBuiltin Pact.Info)) a
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
        Left err -> internalDbError $ "callDb (" <> callerName <> "): " <> sshow err
        Right r -> return r

domainTableName :: Pact.Domain k v b i -> SQ3.Utf8
domainTableName = toUtf8 . Pact.renderDomain

tableName :: Pact.TableName -> SQ3.Utf8
tableName = toUtf8 . Pact.renderTableName

convKeySetName :: Pact.KeySetName -> SQ3.Utf8
convKeySetName = toUtf8 . Pact.renderKeySetName

convModuleName :: Pact.ModuleName -> SQ3.Utf8
convModuleName mn = toUtf8 $ Pact.renderModuleName mn

convNamespaceName :: Pact.NamespaceName -> SQ3.Utf8
convNamespaceName (Pact.NamespaceName name) = toUtf8 name

convRowKey :: Pact.RowKey -> SQ3.Utf8
convRowKey (Pact.RowKey name) = toUtf8 name

-- to match legacy keys
convPactId :: Pact.DefPactId -> SQ3.Utf8
convPactId pid = "PactId \"" <> toUtf8 (Pact.renderDefPactId pid) <> "\""

convHashedModuleName :: Pact.HashedModuleName -> SQ3.Utf8
convHashedModuleName = toUtf8 . Pact.renderHashedModuleName


newtype InternalDbException = InternalDbException Text
  deriving newtype (Eq)
  deriving stock (Show)
  deriving anyclass (Exception)

internalDbError :: MonadThrow m => Text -> m a
internalDbError = throwM . InternalDbException

liftGas :: Pact.GasM Pact.CoreBuiltin Pact.Info a -> BlockHandler logger a
liftGas g = BlockHandler (lift (lift g))

runOnBlockGassed
    :: BlockHandlerEnv logger -> MVar BlockState
    -> BlockHandler logger a
    -> Pact.GasM Pact.CoreBuiltin Pact.Info a
runOnBlockGassed env stateVar act = do
    ge <- ask
    r <- liftIO $ modifyMVar stateVar $ \s -> do
        r <- runExceptT (runReaderT (Pact.runGasM (runStateT (runReaderT (runBlockHandler act) env) s)) ge)
        let newState = either (\_ -> s) snd r
        return (newState, fmap fst r)
    liftEither r

chainwebPactBlockDb :: (Logger logger) => Maybe (BlockHeight, Pact.TxId) -> BlockHandlerEnv logger -> Pact5Db
chainwebPactBlockDb maybeLimit env = Pact5Db
    { doPact5DbTransaction = \blockHandle maybeRequestKey kont -> do
        stateVar <- newMVar $ BlockState blockHandle Nothing
        let basePactDb = Pact.PactDb
                { Pact._pdbPurity = Pact.PImpure
                , Pact._pdbRead = \d k -> runOnBlockGassed env stateVar $ doReadRow Nothing d k
                , Pact._pdbWrite = \wt d k v ->
                    runOnBlockGassed env stateVar $ doWriteRow Nothing wt d k v
                , Pact._pdbKeys = \d ->
                    runOnBlockGassed env stateVar $ doKeys Nothing d
                , Pact._pdbCreateUserTable = \tn ->
                    runOnBlockGassed env stateVar $ doCreateUserTable Nothing tn
                , Pact._pdbBeginTx = \m ->
                    runOnBlockGassed env stateVar $ doBegin m
                , Pact._pdbCommitTx =
                    runOnBlockGassed env stateVar doCommit
                , Pact._pdbRollbackTx =
                    runOnBlockGassed env stateVar doRollback
                }
        let maybeLimitedPactDb = case maybeLimit of
                Just (bh, endTxId) -> basePactDb
                    { Pact._pdbRead = \d k -> runOnBlockGassed env stateVar $ doReadRow (Just (bh, endTxId)) d k
                    , Pact._pdbWrite = \wt d k v -> do
                        runOnBlockGassed env stateVar $ doWriteRow (Just (bh, endTxId)) wt d k v
                    , Pact._pdbKeys = \d -> runOnBlockGassed env stateVar $ doKeys (Just (bh, endTxId)) d
                    , Pact._pdbCreateUserTable = \tn -> do
                        runOnBlockGassed env stateVar $ doCreateUserTable (Just bh) tn
                    }
                Nothing -> basePactDb
        r <- kont maybeLimitedPactDb
        finalState <- readMVar stateVar
        -- TODO: this may not be wanted when we allow more unconstrained access
        -- to the Pact state - which we may do in the future, to run Pact REPL files
        -- with chainweb's Pact state. Perhaps we use ExecutionMode to flag this?
        when (isJust (_bsPendingTx finalState)) $
            internalDbError "dangling transaction"
        -- Register a successful transaction in the pending data for the block
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
    BlockHandle _ sql _ <- use bsBlockHandle
    ptx <- view _1 <$> getPendingTxOrError msg
    -- lookup in pending transactions first
    return $ [ptx, sql]

-- TODO: speed this up, cache it?
tableExistsInDbAtHeight :: SQ3.Utf8 -> BlockHeight -> BlockHandler logger Bool
tableExistsInDbAtHeight tablename bh = do
    let knownTbls =
            ["SYS:Pacts", "SYS:Modules", "SYS:KeySets", "SYS:Namespaces", "SYS:ModuleSources"]
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
    :: Maybe (BlockHeight, Pact.TxId)
    -- ^ the highest block we should be reading writes from
    -> Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> BlockHandler logger (Maybe v)
doReadRow mlim d k =
    case d of
        Pact.DKeySets -> let f = (\v -> (view Pact.document <$> Pact._decodeKeySet Pact.serialisePact_lineinfo v)) in
            lookupWithKey (convKeySetName k) f (noCache f)
        -- TODO: This is incomplete (the modules case), due to namespace
        -- resolution concerns
        Pact.DModules -> let f = (\v -> (view Pact.document <$> Pact._decodeModuleData Pact.serialisePact_lineinfo v)) in
            lookupWithKey (convModuleName k) f (noCacheChargeModuleSize f)
        Pact.DNamespaces -> let f = (\v -> (view Pact.document <$> Pact._decodeNamespace Pact.serialisePact_lineinfo v)) in
            lookupWithKey (convNamespaceName k) f (noCache f)
        Pact.DUserTables _ -> let f = (\v -> (view Pact.document <$> Pact._decodeRowData Pact.serialisePact_lineinfo v)) in
            lookupWithKey (convRowKey k) f (noCache f)
        Pact.DDefPacts -> let f = (\v -> (view Pact.document <$> Pact._decodeDefPactExec Pact.serialisePact_lineinfo v)) in
            lookupWithKey (convPactId k) f (noCache f)
        Pact.DModuleSource -> let f = (\v -> (view Pact.document <$> Pact._decodeModuleCode Pact.serialisePact_lineinfo v)) in
            lookupWithKey (convHashedModuleName k) f (noCache f)
  where
    tablename@(SQ3.Utf8 tableNameBS) = domainTableName d

    lookupWithKey
        :: forall logger v .
            SQ3.Utf8
        -> (BS.ByteString -> Maybe v)
        -> (SQ3.Utf8 -> BS.ByteString -> MaybeT (BlockHandler logger) v)
        -> BlockHandler logger (Maybe v)
    lookupWithKey key f checkCache = do
        pds <- getPendingData "read"
        let lookPD = foldr1 (<|>) $ map (lookupInPendingData key f) pds
        let lookDB = lookupInDb key f checkCache
        runMaybeT (lookPD <|> lookDB)

    lookupInPendingData
        :: forall logger v .
            SQ3.Utf8
        -> (BS.ByteString -> Maybe v)
        -> SQLitePendingData
        -> MaybeT (BlockHandler logger) v
    lookupInPendingData (SQ3.Utf8 rowkey) f p = do
        -- we get the latest-written value at this rowkey
        allKeys <- hoistMaybe $ HashMap.lookup tableNameBS (_pendingWrites p)
        ddata <- _deltaData . NE.head <$> hoistMaybe (HashMap.lookup rowkey allKeys)
        MaybeT $ return $! f ddata

    lookupInDb
        :: forall logger v .
           SQ3.Utf8
        -> (BS.ByteString -> Maybe v)
        -> (SQ3.Utf8 -> BS.ByteString -> MaybeT (BlockHandler logger) v)
        -> MaybeT (BlockHandler logger) v
    lookupInDb rowkey _ checkCache = do
        -- First, check: did we create this table during this block? If so,
        -- there's no point in looking up the key.
        checkDbTablePendingCreation "read" tablename
        lift $ forM_ mlim $ \(bh, _) ->
            failIfTableDoesNotExistInDbAtHeight "doReadRow" tablename bh
        -- we inject the endingtx limitation to reduce the scope up to the provided block height
        let blockLimitStmt = maybe "" (const " AND txid < ?") mlim
        let blockLimitParam = maybe [] (\(Pact.TxId txid) -> [SInt $ fromIntegral txid]) (snd <$> mlim)
        let queryStmt =
                "SELECT rowdata FROM " <> tbl tablename <> " WHERE rowkey = ?" <> blockLimitStmt
                <> " ORDER BY txid DESC LIMIT 1;"
        result <- lift $ callDb "doReadRow"
                       $ \db -> qry db queryStmt ([SText rowkey] ++ blockLimitParam) [RBlob]
        case result of
            [] -> mzero
            [[SBlob a]] -> checkCache rowkey a
            err -> internalDbError $
                     "doReadRow: Expected (at most) a single result, but got: " <>
                     T.pack (show err)

    noCache
        :: (BS.ByteString -> Maybe v)
        -> SQ3.Utf8
        -> BS.ByteString
        -> MaybeT (BlockHandler logger) v
    noCache f _key rowdata = MaybeT $ return $! f rowdata

    noCacheChargeModuleSize
        :: (BS.ByteString -> Maybe (Pact.ModuleData Pact.CoreBuiltin Pact.Info))
        -> SQ3.Utf8
        -> BS.ByteString
        -> MaybeT (BlockHandler logger) (Pact.ModuleData Pact.CoreBuiltin Pact.Info)
    noCacheChargeModuleSize f _key rowdata = do
        lift $ BlockHandler $ lift $ lift (Pact.chargeGasM (Pact.GModuleOp (Pact.MOpLoadModule (BS.length rowdata))))
        MaybeT $ return $! f rowdata


checkDbTablePendingCreation :: Text -> SQ3.Utf8 -> MaybeT (BlockHandler logger) ()
checkDbTablePendingCreation msg (SQ3.Utf8 tablename) = do
    pds <- lift (getPendingData msg)
    forM_ pds $ \p ->
        when (HashSet.member tablename (_pendingTableCreation p)) mzero

latestTxId :: Lens' BlockState Pact.TxId
latestTxId = bsBlockHandle . blockHandleTxId . coerced

writeSys
    :: Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> v
    -> BlockHandler logger ()
writeSys d k v = do
  txid <- use latestTxId
  (kk, vv) <- pure $ case d of
      Pact.DKeySets -> (convKeySetName k, Pact._encodeKeySet Pact.serialisePact_lineinfo v)
      Pact.DModules ->  (convModuleName k, Pact._encodeModuleData Pact.serialisePact_lineinfo v)
      Pact.DNamespaces -> (convNamespaceName k, Pact._encodeNamespace Pact.serialisePact_lineinfo v)
      Pact.DDefPacts -> (convPactId k, Pact._encodeDefPactExec Pact.serialisePact_lineinfo v)
      Pact.DUserTables _ -> error "impossible"
      Pact.DModuleSource -> (convHashedModuleName k, Pact._encodeModuleCode Pact.serialisePact_lineinfo v)
  recordPendingUpdate kk (toUtf8 tablename) txid vv
  recordTxLog d kk vv
    where
    tablename = Pact.renderDomain d

recordPendingUpdate
    :: SQ3.Utf8
    -> SQ3.Utf8
    -> Pact.TxId
    -> BS.ByteString
    -> BlockHandler logger ()
recordPendingUpdate (SQ3.Utf8 key) (SQ3.Utf8 tn) txid vs = modifyPendingData "write" modf
  where
    delta = SQLiteRowDelta tn (coerce txid) key vs

    modf = over pendingWrites upd
    upd = HashMap.unionWith
        HashMap.union
        (HashMap.singleton tn
            (HashMap.singleton key (NE.singleton delta)))

checkInsertIsOK
    :: Maybe (BlockHeight, Pact.TxId)
    -> Pact.TableName
    -- ^ the highest block we should be reading writes from
    -> Pact.WriteType
    -> Pact.Domain Pact.RowKey Pact.RowData Pact.CoreBuiltin Pact.Info
    -> Pact.RowKey
    -> BlockHandler logger (Maybe Pact.RowData)
checkInsertIsOK mlim tn wt d k = do
    olds <- doReadRow mlim d k
    case (olds, wt) of
        (Nothing, Pact.Insert) -> return Nothing
        (Just _, Pact.Insert) -> liftGas $ Pact.throwDbOpErrorGasM (Pact.RowFoundError tn k)
        (Nothing, Pact.Write) -> return Nothing
        (Just old, Pact.Write) -> return $ Just old
        (Just old, Pact.Update) -> return $ Just old
        (Nothing, Pact.Update) -> liftGas $ Pact.throwDbOpErrorGasM (Pact.NoRowFound tn k)

writeUser
    :: Maybe (BlockHeight, Pact.TxId)
    -- ^ the highest block we should be reading writes from
    -> Pact.WriteType
    -> Pact.Domain Pact.RowKey Pact.RowData Pact.CoreBuiltin Pact.Info
    -> Pact.RowKey
    -> Pact.RowData
    -> BlockHandler logger ()
writeUser mlim wt d k rowdata@(Pact.RowData row) = do
    Pact.TxId txid <- use latestTxId
    let (Pact.DUserTables tname) = d
    m <- checkInsertIsOK mlim tname wt d k
    row' <- case m of
        Nothing -> ins txid
        Just old -> upd txid old
    liftGas (Pact._encodeRowData Pact.serialisePact_lineinfo row') >>=
        \encoded -> recordTxLog d (convRowKey k) encoded
  where
  tn = Pact.renderDomain d

  upd txid (Pact.RowData oldrow) = do
      let row' = Pact.RowData (M.union row oldrow)
      liftGas (Pact._encodeRowData Pact.serialisePact_lineinfo row') >>=
          \encoded -> do
              recordPendingUpdate (convRowKey k) (toUtf8 tn) (Pact.TxId txid) encoded
              return row'

  ins txid = do
      liftGas (Pact._encodeRowData Pact.serialisePact_lineinfo rowdata) >>=
          \encoded -> do
              recordPendingUpdate (convRowKey k) (toUtf8 tn) (Pact.TxId txid) encoded
              return rowdata

doWriteRow
    :: Maybe (BlockHeight, Pact.TxId)
    -- ^ the highest block we should be reading writes from
    -> Pact.WriteType
    -> Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> v
    -> BlockHandler logger ()
doWriteRow mlim wt d k v = case d of
    (Pact.DUserTables _) -> writeUser mlim wt d k v
    _ -> writeSys d k v

doKeys
    :: forall k v logger .
       Maybe (BlockHeight, Pact.TxId)
    -- ^ the highest block we should be reading writes from
    -> Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> BlockHandler logger [k]
doKeys mlim d = do
    dbKeys <- getDbKeys
    pb <- use (bsBlockHandle . blockHandlePending)
    (mptx, _) <- getPendingTxOrError "keys"

    let memKeys = fmap (T.decodeUtf8 . _deltaRowKey)
                  $ collect pb ++ collect mptx

    let !allKeys = sort (dbKeys ++ memKeys)
    case d of
        Pact.DKeySets -> do
            let parsed = map Pact.parseAnyKeysetName allKeys
            case sequence parsed of
              Left msg -> internalDbError $ "doKeys.DKeySets: unexpected decoding " <> T.pack msg
              Right v -> pure v
        Pact.DModules -> do
            let parsed = map Pact.parseModuleName allKeys
            case sequence parsed of
              Nothing -> internalDbError $ "doKeys.DModules: unexpected decoding"
              Just v -> pure v
        Pact.DNamespaces -> pure $ map Pact.NamespaceName allKeys
        Pact.DDefPacts ->  pure $ map Pact.DefPactId allKeys
        Pact.DUserTables _ -> pure $ map Pact.RowKey allKeys
        Pact.DModuleSource -> do
            let parsed = map Pact.parseHashedModuleName allKeys
            case sequence parsed of
              Just v -> pure v
              Nothing -> internalDbError $ "doKeys.DModuleSources: unexpected decoding"

    where
    blockLimitStmt = maybe "" (const " WHERE txid < ?;") mlim
    blockLimitParam = maybe [] (\(Pact.TxId txid) -> [SInt (fromIntegral txid)]) (snd <$> mlim)

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
                        _ -> internalDbError "doKeys: The impossible happened."

    tn@(SQ3.Utf8 tnBS) = toUtf8 $ Pact.renderDomain d
    collect p =
        concatMap NE.toList $ HashMap.elems $ fromMaybe mempty $ HashMap.lookup tnBS (_pendingWrites p)

failIfTableDoesNotExistInDbAtHeight
    :: T.Text -> SQ3.Utf8 -> BlockHeight -> BlockHandler logger ()
failIfTableDoesNotExistInDbAtHeight caller tn bh = do
    exists <- tableExistsInDbAtHeight tn bh
    -- we must reproduce errors that were thrown in earlier blocks from tables
    -- not existing, if this table does not yet exist.
    unless exists $
        internalDbError $ "callDb (" <> caller <> "): user error (Database error: ErrorError)"

recordTxLog
    :: Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> SQ3.Utf8
    -> BS.ByteString
    -> BlockHandler logger ()
recordTxLog d (SQ3.Utf8 k) v = do
    -- are we in a tx? if not, error.
    (pendingSQLite, txlogs) <- getPendingTxOrError "write"
    modify' (bsPendingTx .~ Just (pendingSQLite, DL.snoc txlogs newLog))

  where
    !newLog = Pact.TxLog (Pact.renderDomain d) (T.decodeUtf8 k) v

recordTableCreationTxLog :: Pact.TableName -> BlockHandler logger ()
recordTableCreationTxLog tn = do
    (pendingSQLite, txlogs) <- getPendingTxOrError "create table"
    modify' $ bsPendingTx .~ Just (pendingSQLite, DL.snoc txlogs newLog)
    where
    !newLog = Pact.TxLog "SYS:usertables" (Pact._tableName tn) (encodeStable uti)
    !uti = Pact.UserTableInfo (Pact._tableModuleName tn)

modifyPendingData
    :: Text
    -> (SQLitePendingData -> SQLitePendingData)
    -> BlockHandler logger ()
modifyPendingData msg f = do
    (pending, txlogs) <- getPendingTxOrError msg
    modify' $ set bsPendingTx $ Just (f pending, txlogs)

doCreateUserTable
    :: Maybe BlockHeight
    -- ^ the highest block we should be seeing tables from
    -> Pact.TableName
    -> BlockHandler logger ()
doCreateUserTable mbh tn = do
    -- first check if tablename already exists in pending queues
    m <- runMaybeT $ checkDbTablePendingCreation "create table" (tableName tn)
    case m of
        Nothing ->
            liftGas $ Pact.throwDbOpErrorGasM $ Pact.TableAlreadyExists tn
        Just () -> do
            -- then check if it is in the db
            cond <- inDb $ SQ3.Utf8 $ T.encodeUtf8 $ Pact.renderTableName tn
            when cond $
                liftGas $ Pact.throwDbOpErrorGasM $ Pact.TableAlreadyExists tn

            modifyPendingData "create table"
                $ over pendingTableCreation (HashSet.insert (T.encodeUtf8 $ Pact.renderTableName tn))
            recordTableCreationTxLog tn
    where
    inDb t = do
        r <- callDb "doCreateUserTable" $ \db ->
            qry db tableLookupStmt [SText t] [RText]
        case r of
            [[SText _]] ->
                case mbh of
                    -- if lowercase matching, no need to check equality
                    -- (wasn't needed before either but leaving alone for replay)
                    Nothing -> return True
                    Just bh -> tableExistsInDbAtHeight t bh
            _ -> return False

    tableLookupStmt =
        "SELECT name FROM sqlite_master WHERE type='table' and lower(name)=lower(?);"

doRollback :: BlockHandler logger ()
doRollback = modify'
    $ set bsPendingTx Nothing

-- | Commit a Pact transaction
doCommit :: BlockHandler logger [Pact.TxLog B8.ByteString]
doCommit = view blockHandlerMode >>= \case
    m -> do
        txrs <- if m == Pact.Transactional
        then do
            modify' $ over latestTxId (\(Pact.TxId tid) -> Pact.TxId (succ tid))
            pending <- getPendingTxOrError "commit"
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

-- | Begin a Pact transaction. Note that we don't actually use the ExecutionMode anymore.
doBegin :: (Logger logger) => Pact.ExecutionMode -> BlockHandler logger (Maybe Pact.TxId)
doBegin _m = do
    use bsPendingTx >>= \case
        Just _ -> do
            txid <- use latestTxId
            liftGas $ Pact.throwDbOpErrorGasM (Pact.TxAlreadyBegun ("TxId " <> sshow (Pact._txId txid)))
        Nothing -> do
            modify'
                $ set bsPendingTx (Just (emptySQLitePendingData, mempty))
            Just <$> use latestTxId

toTxLog :: MonadThrow m => T.Text -> SQ3.Utf8 -> BS.ByteString -> m (Pact.TxLog Pact.RowData)
toTxLog d key value =
        case fmap (view Pact.document) $ Pact._decodeRowData Pact.serialisePact_lineinfo value of
            Nothing -> internalDbError $ "toTxLog: Unexpected value, unable to deserialize log: " <> sshow value
            Just v -> return $! Pact.TxLog d (fromUtf8 key) v

toPactTxLog :: Pact.TxLog Pact.RowData -> Pact4.TxLog Pact.RowData
toPactTxLog (Pact.TxLog d k v) = Pact4.TxLog d k v
