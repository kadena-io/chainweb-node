{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyCase #-}

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
-- SQLite transaction ensures that the ChainwebPactDb transaction
-- sees a consistent view of the database, especially if its
-- writes are committed later.
--
-- ChainwebPactDb tx ensures that the Pact tx's writes
-- are recorded.
--
-- Pact tx ensures that failed transactions' writes are not recorded.


module Chainweb.Pact.Backend.ChainwebPactDb
    ( chainwebPactBlockDb
    , ChainwebPactDb(..)
    , BlockHandlerEnv(..)
    , blockHandlerDb
    , blockHandlerLogger
    , toTxLog
    , domainTableName
    , convRowKey
    , commitBlockStateToDatabase
    , initSchema
    , lookupBlockWithHeight
    , lookupParentBlockHash
    , lookupParentBlockRanked
    , getPayloadsAfter
    , getEarliestBlock
    , getConsensusState
    , setConsensusState
    , throwOnDbError
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Short qualified as SB
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Int
import Data.List(sort)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Singletons (Dict(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.SQLite3.Direct qualified as SQ3
import GHC.Stack

import Prelude hiding (concat, log)

import Pact.Core.Builtin qualified as Pact
import Pact.Core.Command.Types (RequestKey (..))
import Pact.Core.Errors qualified as Pact
import Pact.Core.Evaluate qualified as Pact
import Pact.Core.Gas qualified as Pact
import Pact.Core.Guards qualified as Pact
import Pact.Core.Hash hiding (hash)
import Pact.Core.Info qualified as Pact
import Pact.Core.Names qualified as Pact
import Pact.Core.Persistence (throwDbOpErrorGasM)
import Pact.Core.Persistence qualified as Pact
import Pact.Core.Serialise qualified as Pact
import Pact.Core.StableEncoding (encodeStable)

import Chainweb.BlockHash
import Chainweb.BlockHeader (encodeBlockPayloadHash, decodeBlockPayloadHash, BlockPayloadHash)
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Pact.Backend.InMemDb qualified as InMemDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.SPV (pactSPV)
import Chainweb.Parent
import Chainweb.PayloadProvider (ConsensusState (..), SyncState (..), EvaluationCtx (_evaluationCtxPayload, _evaluationCtxParentHash), _evaluationCtxCurrentHeight)
import Chainweb.Utils (sshow)
import Chainweb.Utils.Serialization (runPutS, runGetEitherS)
import Chainweb.Version
import Chainweb.Version.Guards (pact5Serialiser)
import Chainweb.Ranked

data InternalDbException = InternalDbException CallStack Text
instance Show InternalDbException where show = displayException
instance Exception InternalDbException where
    displayException (InternalDbException stack text) =
        T.unpack text <> "\n\n" <>
        prettyCallStack stack

internalDbError :: HasCallStack => MonadThrow m => Text -> m a
internalDbError = throwM . InternalDbException callStack

throwOnDbError :: (HasCallStack, MonadThrow m) => ExceptT SQ3.Error m a -> m a
throwOnDbError act = runExceptT act >>= either (internalDbError . sshow) return

-- | Statement input types
data SType = SInt Int64 | SDouble Double | SText SQ3.Utf8 | SBlob BS.ByteString deriving (Eq,Show)
-- | Result types
data RType = RInt | RDouble | RText | RBlob deriving (Eq,Show)

bindParams :: SQ3.Statement -> [SType] -> ExceptT SQ3.Error IO ()
bindParams stmt as =
    forM_ (zip as [1..]) $ \(a,i) -> ExceptT $
      case a of
        SInt n -> SQ3.bindInt64 stmt i n
        SDouble n -> SQ3.bindDouble stmt i n
        SText n -> SQ3.bindText stmt i n
        SBlob n -> SQ3.bindBlob stmt i n

prepStmt :: HasCallStack => SQ3.Database -> SQ3.Utf8 -> ExceptT SQ3.Error IO SQ3.Statement
prepStmt c q = do
    r <- ExceptT $ SQ3.prepare c q
    case r of
        Nothing -> internalDbError "No SQL statements in prepared statement"
        Just s -> return s

execMulti :: Traversable t => SQ3.Database -> SQ3.Utf8 -> t [SType] -> ExceptT SQ3.Error IO ()
execMulti db q rows = bracket (prepStmt db q) (liftIO . SQ3.finalize) $ \stmt -> do
    forM_ rows $ \row -> do
        ExceptT $ SQ3.reset stmt
        liftIO $ SQ3.clearBindings stmt
        bindParams stmt row
        ExceptT $ SQ3.step stmt

-- | Prepare/execute query with params
qry :: SQ3.Database -> SQ3.Utf8 -> [SType] -> [RType] -> ExceptT SQ3.Error IO [[SType]]
qry e q as rts = bracket (prepStmt e q) (ExceptT . SQ3.finalize) $ \stmt -> do
    bindParams stmt as
    reverse <$> stepStmt stmt rts

stepStmt :: SQ3.Statement -> [RType] -> ExceptT SQ3.Error IO [[SType]]
stepStmt stmt rts = do
    let acc rs SQ3.Done = return rs
        acc rs SQ3.Row = do
            as <- lift $ forM (zip rts [0..]) $ \(rt,ci) ->
                case rt of
                    RInt -> SInt <$> SQ3.columnInt64 stmt ci
                    RDouble -> SDouble <$> SQ3.columnDouble stmt ci
                    RText -> SText <$> SQ3.columnText stmt ci
                    RBlob -> SBlob <$> SQ3.columnBlob stmt ci
            sr <- ExceptT $ SQ3.step stmt
            acc (as:rs) sr
    sr <- ExceptT $ SQ3.step stmt
    acc [] sr

-- | Prepare/exec statement with no params
exec_ :: SQ3.Database -> SQ3.Utf8 -> ExceptT SQ3.Error IO ()
exec_ e q = ExceptT $ over _Left fst <$> SQ3.exec e q

-- | Prepare/exec statement with params
exec' :: SQ3.Database -> SQ3.Utf8 -> [SType] -> ExceptT SQ3.Error IO ()
exec' e q as = bracket (prepStmt e q) (ExceptT . SQ3.finalize) $ \stmt -> do
    bindParams stmt as
    void $ ExceptT (SQ3.step stmt)

data BlockHandlerEnv logger = BlockHandlerEnv
    { _blockHandlerDb :: !SQLiteEnv
    , _blockHandlerLogger :: !logger
    , _blockHandlerVersion :: !ChainwebVersion
    , _blockHandlerBlockHeight :: !BlockHeight
    , _blockHandlerUpperBoundTxId :: !Pact.TxId
    , _blockHandlerChainId :: !ChainId
    , _blockHandlerMode :: !Pact.ExecutionMode
    , _blockHandlerAtTip :: Bool
    }

instance HasChainId (BlockHandlerEnv logger) where
    _chainId = _blockHandlerChainId

instance HasChainwebVersion (BlockHandlerEnv logger) where
    _chainwebVersion = _blockHandlerVersion

-- | The state used by database operations.
-- Includes both the state re: the whole block, and the state re: a transaction in progress.
data BlockState = BlockState
    { _bsBlockHandle :: !BlockHandle
    , _bsPendingTxWrites :: !SQLitePendingData
    , _bsPendingTxLog :: !(Maybe (DList (Pact.TxLog ByteString)))
    }

makeLenses ''BlockState
makeLenses ''BlockHandlerEnv

getPendingTxLogOrError :: Text -> BlockHandler logger (DList (Pact.TxLog ByteString))
getPendingTxLogOrError msg = do
    use bsPendingTxLog >>= \case
        Nothing -> liftGas $ Pact.throwDbOpErrorGasM (Pact.NotInTx msg)
        Just t -> return t

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

domainTableName :: Pact.Domain k v b i -> SQ3.Utf8
domainTableName = toUtf8 . Pact.renderDomain

tableNameToSQL :: Pact.TableName -> SQ3.Utf8
tableNameToSQL = toUtf8 . Pact.renderTableName

convKeySetName :: Pact.KeySetName -> Text
convKeySetName = Pact.renderKeySetName

convModuleName :: Pact.ModuleName -> Text
convModuleName = Pact.renderModuleName

convNamespaceName :: Pact.NamespaceName -> Text
convNamespaceName (Pact.NamespaceName name) = name

convRowKey :: Pact.RowKey -> Text
convRowKey (Pact.RowKey name) = name

-- to match legacy keys
convPactId :: Pact.DefPactId -> Text
convPactId pid = "PactId \"" <> Pact.renderDefPactId pid <> "\""

convHashedModuleName :: Pact.HashedModuleName -> Text
convHashedModuleName = Pact.renderHashedModuleName

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

chainwebPactBlockDb :: (Logger logger) => BlockHandlerEnv logger -> ChainwebPactDb
chainwebPactBlockDb env = ChainwebPactDb
    { doChainwebPactDbTransaction = \blockHandle maybeRequestKey kont -> do
        stateVar <- newMVar $ BlockState blockHandle (_blockHandlePending blockHandle) Nothing
        let pactDb = Pact.PactDb
                { Pact._pdbPurity = Pact.PImpure
                , Pact._pdbRead = \d k -> runOnBlockGassed env stateVar $ doReadRow d k
                , Pact._pdbWrite = \wt d k v ->
                    runOnBlockGassed env stateVar $ doWriteRow wt d k v
                , Pact._pdbKeys = \d ->
                    runOnBlockGassed env stateVar $ doKeys d
                , Pact._pdbCreateUserTable = \tn ->
                    runOnBlockGassed env stateVar $ doCreateUserTable tn
                , Pact._pdbBeginTx = \m ->
                    runOnBlockGassed env stateVar $ doBegin m
                , Pact._pdbCommitTx =
                    runOnBlockGassed env stateVar doCommit
                , Pact._pdbRollbackTx =
                    runOnBlockGassed env stateVar doRollback
                }
        let headerOracle = HeaderOracle
                { chain = _chainId env
                , consult = throwOnDbError . lookupParentBlockHash (_blockHandlerDb env)
                }
        let spv = pactSPV headerOracle
        r <- kont pactDb spv
        finalState <- readMVar stateVar
        -- Register a successful transaction in the pending data for the block
        let registerRequestKey = case maybeRequestKey of
                Just requestKey -> HashSet.insert (SB.fromShort $ unHash $ unRequestKey requestKey)
                Nothing -> id
        let finalHandle =
                _bsBlockHandle finalState
                    & blockHandlePending . pendingSuccessfulTxs %~ registerRequestKey

        return (r, finalHandle)
    , lookupPactTransactions =
        fmap (HM.mapKeys (RequestKey . Hash)) .
        doLookupSuccessful (_blockHandlerDb env) (_blockHandlerBlockHeight env) .
        fmap (unHash . unRequestKey)
    }

doReadRow
    :: forall k v logger
    -- ^ the highest block we should be reading writes from
    . Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> BlockHandler logger (Maybe v)
doReadRow d k = do
    runMaybeT (MaybeT lookupInMem <|> MaybeT lookupInDb) >>= \case
        Nothing -> return Nothing
        Just (encodedValueLength, decodedValue) -> do
            case d of
                Pact.DModules -> do
                    BlockHandler $ lift $ lift
                        $ Pact.chargeGasM (Pact.GModuleOp (Pact.MOpLoadModule encodedValueLength))
                _ -> return ()
            bsPendingTxWrites . pendingWrites %=
                InMemDb.insert d k (InMemDb.ReadEntry encodedValueLength decodedValue)
            return (Just decodedValue)
    where
    codec :: BlockHandler logger (ByteString -> Maybe (Pact.Document v), Text)
    codec = do
        serialiser <- getSerialiser
        return $ case d of
            Pact.DKeySets ->
                (Pact._decodeKeySet serialiser, convKeySetName k)
            Pact.DModules ->
                (Pact._decodeModuleData serialiser, convModuleName k)
            Pact.DNamespaces ->
                (Pact._decodeNamespace serialiser, convNamespaceName k)
            Pact.DUserTables _ ->
                (Pact._decodeRowData serialiser, convRowKey k)
            Pact.DDefPacts ->
                (Pact._decodeDefPactExec serialiser, convPactId k)
            Pact.DModuleSource ->
                (Pact._decodeModuleCode serialiser, convHashedModuleName k)

    lookupInMem :: BlockHandler logger (Maybe (Int, v))
    lookupInMem = do
            store <- use (bsPendingTxWrites . pendingWrites)
            return $ InMemDb.lookup d k store <&> \case
                (InMemDb.ReadEntry len a) -> (len, a)
                (InMemDb.WriteEntry _ bs a) -> (BS.length bs, a)

    lookupInDb :: BlockHandler logger (Maybe (Int, v))
    lookupInDb = do
        case d of
            Pact.DUserTables pactTableName -> do
                -- if the table is pending creation, we also return Nothing
                fmap join $ withTableExistenceCheck pactTableName fetchRowFromDb
            _ -> throwOnDbError $ fetchRowFromDb
        where
        fetchRowFromDb :: ExceptT SQ3.Error (BlockHandler logger) (Maybe (Int, v))
        fetchRowFromDb = do
            (decodeValueDoc, encodedKey) <- lift codec
            let decodeValue = fmap (view Pact.document) . decodeValueDoc
            let encodedKeyUtf8 = toUtf8 encodedKey
            Pact.TxId txIdUpperBoundWord64 <- view blockHandlerUpperBoundTxId
            let tablename = domainTableName d
            let queryStmt =
                    "SELECT rowdata FROM " <> tbl tablename <> " WHERE rowkey = ? AND txid < ?"
                    <> " ORDER BY txid DESC LIMIT 1;"
            db <- view blockHandlerDb
            result <- mapExceptT liftIO $
                qry db queryStmt [SText encodedKeyUtf8, SInt (fromIntegral txIdUpperBoundWord64)] [RBlob]
            case result of
                [] -> return Nothing
                [[SBlob a]] -> return $ (BS.length a,) <$> decodeValue a
                err -> internalDbError $
                    "doReadRow: Expected (at most) a single result, but got: " <>
                    sshow err

data TableStatus
    = TableCreationPending
    | TableExists
    | TableDoesNotExist

checkTableStatus :: Pact.TableName -> BlockHandler logger TableStatus
checkTableStatus tableName = do
    pds <- use bsPendingTxWrites
    if
        Pact.renderTableName tableName
        `HashSet.member`
        _pendingTableCreation pds
    then return TableCreationPending

    else if
        InMemDb.checkTableSeen tableName (_pendingWrites pds)
    then
        return TableExists

    else do
        exists <- checkTableExistsInDb
        when exists $
            bsPendingTxWrites . pendingWrites
                %= InMemDb.markTableSeen tableName
        return $
            if exists then TableExists else TableDoesNotExist

    where
    checkTableExistsInDb :: BlockHandler logger Bool
    checkTableExistsInDb = do
        bh <- view blockHandlerBlockHeight
        db <- view blockHandlerDb
        tableExistsResult <- liftIO $ throwOnDbError $
            qry db tableExistsStmt
                [SInt $ max 0 (fromIntegral bh), SText $ tableNameToSQL tableName]
                [RText]
        tableExists <- case tableExistsResult of
            [] -> return False
            _ -> return True
        return tableExists
        where
        tableExistsStmt =
            -- table names are case-insensitive
            "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight < ? AND lower(tablename) = lower(?)"

-- we ideally produce `NoSuchTable` errors for accesses to user tables that
-- don't exist, so when doing such accesses, wrap them with
-- `withTableExistenceCheck`.  we cache knowledge of tables' existence in
-- `checkTableStatus`, too.  returns `Nothing` if the table is pending creation
-- in this block; usually, this means that we halt before accessing the db to
-- look in the table.
withTableExistenceCheck :: HasCallStack => Pact.TableName -> ExceptT SQ3.Error (BlockHandler logger) a -> BlockHandler logger (Maybe a)
withTableExistenceCheck tableName action = do
    atTip <- view blockHandlerAtTip
    if atTip
    -- at tip, speculatively execute the statement, and only check if the table
    -- was missing if the statement threw an error
    then runExceptT action >>= \case
        Left err@SQ3.ErrorError -> do
            tableStatus <- checkTableStatus tableName
            case tableStatus of
                TableDoesNotExist -> liftGas $ throwDbOpErrorGasM $ Pact.NoSuchTable tableName
                TableCreationPending -> return Nothing
                TableExists -> internalDbError (sshow err)
        Left err -> internalDbError (sshow err)
        Right result -> return (Just result)
    else do
        -- if we're rewound, we just check if the table exists first
        tableStatus <- checkTableStatus tableName
        case tableStatus of
            TableDoesNotExist -> liftGas $ throwDbOpErrorGasM $ Pact.NoSuchTable tableName
            TableCreationPending -> return Nothing
            TableExists -> throwOnDbError (Just <$> action)

latestTxId :: Lens' BlockState Pact.TxId
latestTxId = bsBlockHandle . blockHandleTxId . coerced

writeSys
    :: Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> v
    -> BlockHandler logger ()
writeSys d k v = do
    txid <- use latestTxId
    serialiser <- getSerialiser
    let !(!encodedKey, !encodedValue) = case d of
            Pact.DKeySets -> (convKeySetName k, Pact._encodeKeySet serialiser v)
            Pact.DModules ->  (convModuleName k, Pact._encodeModuleData serialiser v)
            Pact.DNamespaces -> (convNamespaceName k, Pact._encodeNamespace serialiser v)
            Pact.DDefPacts -> (convPactId k, Pact._encodeDefPactExec serialiser v)
            Pact.DUserTables _ -> error "impossible"
            Pact.DModuleSource -> (convHashedModuleName k, Pact._encodeModuleCode serialiser v)
    recordPendingUpdate d k txid (encodedValue, v)
    recordTxLog d encodedKey encodedValue

recordPendingUpdate
    :: Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> Pact.TxId
    -> (ByteString, v)
    -> BlockHandler logger ()
recordPendingUpdate d k txid (encodedValue, decodedValue) =
    bsPendingTxWrites . pendingWrites %=
        InMemDb.insert d k (InMemDb.WriteEntry txid encodedValue decodedValue)

writeUser
    :: Pact.WriteType
    -> Pact.TableName
    -> Pact.RowKey
    -> Pact.RowData
    -> BlockHandler logger ()
writeUser wt tableName k (Pact.RowData newRow) = do
    Pact.TxId txid <- use latestTxId
    maybeExistingValue <- doReadRow (Pact.DUserTables tableName) k
    checkInsertIsOK maybeExistingValue
    finalRow <- case maybeExistingValue of
        Nothing -> return $ Pact.RowData newRow
        Just (Pact.RowData oldRow) -> return $ Pact.RowData $ M.union newRow oldRow
    serialiser <- getSerialiser
    encodedFinalRow <- liftGas (Pact._encodeRowData serialiser finalRow)
    recordTxLog (Pact.DUserTables tableName) (convRowKey k) encodedFinalRow
    recordPendingUpdate (Pact.DUserTables tableName) k (Pact.TxId txid) (encodedFinalRow, finalRow)
    where

    -- only for user tables, we check first if the insertion is legal before doing it.
    checkInsertIsOK :: Maybe Pact.RowData -> BlockHandler logger ()
    checkInsertIsOK olds = do
        case (olds, wt) of
            (Nothing, Pact.Insert) -> return ()
            (Just _, Pact.Insert) -> liftGas $ Pact.throwDbOpErrorGasM (Pact.RowFoundError tableName k)
            (Nothing, Pact.Write) -> return ()
            (Just _, Pact.Write) -> return ()
            (Just _, Pact.Update) -> return ()
            (Nothing, Pact.Update) -> liftGas $ Pact.throwDbOpErrorGasM (Pact.NoRowFound tableName k)

doWriteRow
    -- ^ the highest block we should be reading writes from
    :: Pact.WriteType
    -> Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> v
    -> BlockHandler logger ()
doWriteRow wt d k v = case d of
    Pact.DUserTables tableName -> writeUser wt tableName k v
    _ -> writeSys d k v

doKeys
    :: forall k v logger
    -- ^ the highest block we should be reading writes from
    . Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> BlockHandler logger [k]
doKeys d = do
    dbKeys <- getDbKeys
    mptx <- use bsPendingTxWrites

    let memKeys = collect mptx

    (parsedKeys, ordDict :: Dict (Ord k) ()) <- case d of
        Pact.DKeySets -> do
            let parsed = traverse Pact.parseAnyKeysetName dbKeys
            case parsed of
              Left msg -> internalDbError $ "doKeys.DKeySets: unexpected decoding " <> T.pack msg
              Right v -> pure (v, Dict ())
        Pact.DModules -> do
            let parsed = traverse Pact.parseModuleName dbKeys
            case parsed of
              Nothing -> internalDbError $ "doKeys.DModules: unexpected decoding"
              Just v -> pure (v, Dict ())
        Pact.DNamespaces -> pure (map Pact.NamespaceName dbKeys, Dict ())
        Pact.DDefPacts ->  pure (map Pact.DefPactId dbKeys, Dict ())
        Pact.DUserTables _ -> pure (map Pact.RowKey dbKeys, Dict ())
        Pact.DModuleSource -> do
            let parsed = map Pact.parseHashedModuleName dbKeys
            case sequence parsed of
              Just v -> pure (v, Dict ())
              Nothing -> internalDbError $ "doKeys.DModuleSources: unexpected decoding"
    case ordDict of
        Dict () ->
            return $ sort (memKeys ++ parsedKeys)

    where

    getDbKeys = do
            case d of
                Pact.DUserTables pactTableName -> do
                    fromMaybe [] <$> withTableExistenceCheck pactTableName fetchKeys
                _ -> throwOnDbError fetchKeys
        where
        fetchKeys :: ExceptT SQ3.Error (BlockHandler logger) [Text]
        fetchKeys = do
            Pact.TxId txIdUpperBoundWord64 <- view blockHandlerUpperBoundTxId
            db <- view blockHandlerDb
            ks <- mapExceptT liftIO $ qry db
                ("SELECT DISTINCT rowkey FROM " <> tbl tn <> "WHERE txid < ? ORDER BY rowkey;")
                [SInt (fromIntegral txIdUpperBoundWord64)] [RText]
            forM ks $ \row -> do
                case row of
                    [SText k] -> return $ fromUtf8 k
                    _ -> internalDbError "doKeys: The impossible happened."

    tn = toUtf8 $ Pact.renderDomain d
    collect p = InMemDb.keys d (_pendingWrites p)

recordTxLog
    :: Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> Text
    -> ByteString
    -> BlockHandler logger ()
recordTxLog d k v = recordTxLog' (Pact.renderDomain d) k v

recordTxLog' :: Text -> Text -> ByteString -> BlockHandler logger ()
recordTxLog' d k v = do
    bsPendingTxLog . _Just %= flip DL.snoc newLog
  where
    !newLog = Pact.TxLog d k v

recordTableCreationTxLog :: Pact.TableName -> BlockHandler logger ()
recordTableCreationTxLog tn = do
    recordTxLog' "SYS:usertables" (Pact._tableName tn) (encodeStable uti)
    where
    !uti = Pact.UserTableInfo (Pact._tableModuleName tn)

doCreateUserTable
    -- ^ the highest block we should be seeing tables from
    :: Pact.TableName
    -> BlockHandler logger ()
doCreateUserTable tableName = do
    -- first check if tablename already exists in pending queues
    checkTableStatus tableName >>= \case
        TableCreationPending ->
            liftGas $ Pact.throwDbOpErrorGasM $ Pact.TableAlreadyExists tableName
        TableExists ->
            liftGas $ Pact.throwDbOpErrorGasM $ Pact.TableAlreadyExists tableName
        TableDoesNotExist -> do
            bsPendingTxWrites . pendingTableCreation %=
                HashSet.insert (Pact.renderTableName tableName)
            recordTableCreationTxLog tableName

doRollback :: BlockHandler logger ()
doRollback = do
    blockWrites <- use (bsBlockHandle . blockHandlePending)
    bsPendingTxWrites .= blockWrites
    bsPendingTxLog .= Nothing

-- | Commit a Pact transaction
doCommit :: BlockHandler logger [Pact.TxLog B8.ByteString]
doCommit = view blockHandlerMode >>= \case
    m -> do
        txrs <- if m == Pact.Transactional
        then do
            modify' $ over latestTxId (\(Pact.TxId tid) -> Pact.TxId (succ tid))
            pendingTx <- use bsPendingTxWrites
            txLogs <- getPendingTxLogOrError "doCommit"

            -- merge pending tx into pending block data
            bsBlockHandle . blockHandlePending .= pendingTx
            bsPendingTxLog .= Nothing
            return txLogs
        else doRollback >> return mempty
        return $! DL.toList txrs

-- | Begin a Pact transaction. Note that we don't actually use the ExecutionMode anymore.
doBegin :: (Logger logger) => Pact.ExecutionMode -> BlockHandler logger (Maybe Pact.TxId)
doBegin _m = do
    use bsPendingTxLog >>= \case
        Just _ -> do
            txid <- use latestTxId
            liftGas $ Pact.throwDbOpErrorGasM (Pact.TxAlreadyBegun ("TxId " <> sshow (Pact._txId txid)))
        Nothing -> do
            bsPendingTxLog .= Just mempty
            Just <$> use latestTxId

toTxLog :: MonadThrow m => ChainwebVersion -> ChainId -> BlockHeight -> T.Text -> SQ3.Utf8 -> BS.ByteString -> m (Pact.TxLog Pact.RowData)
toTxLog version cid bh d key value = do
    let serialiser = pact5Serialiser version cid bh
    case fmap (view Pact.document) $ Pact._decodeRowData serialiser value of
        Nothing -> internalDbError $ "toTxLog: Unexpected value, unable to deserialize log: " <> sshow value
        Just v -> return $! Pact.TxLog d (fromUtf8 key) v

commitBlockStateToDatabase :: SQLiteEnv -> EvaluationCtx BlockPayloadHash -> BlockHandle -> IO ()
commitBlockStateToDatabase db evalCtx blockHandle = throwOnDbError $ do
    let newTables = _pendingTableCreation $ _blockHandlePending blockHandle
    mapM_ (\tn -> createUserTable (toUtf8 tn)) newTables
    backendWriteUpdateBatch (_pendingWrites (_blockHandlePending blockHandle))
    indexPendingPactTransactions
    let nextTxId = _blockHandleTxId blockHandle
    blockHistoryInsert nextTxId
    where

    backendWriteUpdateBatch
        :: InMemDb.Store
        -> ExceptT SQ3.Error IO ()
    backendWriteUpdateBatch store = do
        writeTable (domainToTableName Pact.DKeySets)
            $ mapMaybe (uncurry $ prepRow . convKeySetName)
            $ HashMap.toList (InMemDb.keySets store)

        writeTable (domainToTableName Pact.DModules)
            $ mapMaybe (uncurry $ prepRow . convModuleName)
            $ HashMap.toList (InMemDb.modules store)

        writeTable (domainToTableName Pact.DNamespaces)
            $ mapMaybe (uncurry $ prepRow . convNamespaceName)
            $ HashMap.toList (InMemDb.namespaces store)

        writeTable (domainToTableName Pact.DDefPacts)
            $ mapMaybe (uncurry $ prepRow . convPactId)
            $ HashMap.toList (InMemDb.defPacts store)

        writeTable (domainToTableName Pact.DModuleSource)
            $ mapMaybe (uncurry $ prepRow . convHashedModuleName)
            $ HashMap.toList (InMemDb.moduleSources store)

        iforM_ (InMemDb.userTables store) $ \tableName tableContents -> do
            writeTable (domainToTableName (Pact.DUserTables tableName))
                $ mapMaybe (uncurry $ prepRow . convRowKey)
                $ HashMap.toList tableContents

        where
        domainToTableName =
            SQ3.Utf8 . T.encodeUtf8 . Pact.renderDomain
        prepRow rowkey (InMemDb.WriteEntry (Pact.TxId txid) rowdataEncoded _) =
            Just
                [ SText (toUtf8 rowkey)
                , SInt (fromIntegral txid)
                , SBlob rowdataEncoded
                ]
        prepRow _ InMemDb.ReadEntry {} = Nothing

        writeTable :: SQ3.Utf8 -> [[SType]] -> ExceptT SQ3.Error IO ()
        writeTable table writes = when (not (null writes)) $ do
            execMulti db q writes
            markTableMutation table
            where
            q = "INSERT OR REPLACE INTO " <> tbl table <> "(rowkey,txid,rowdata) VALUES(?,?,?)"

        -- Mark the table as being mutated during this block, so that we know
        -- to delete from it if we rewind past this block.
        markTableMutation tablename = do
            exec' db mutq [SText tablename, SInt (fromIntegral $ _evaluationCtxCurrentHeight evalCtx)]
            where
            mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"

    -- | Record a block as being in the history of the checkpointer.
    blockHistoryInsert :: Pact.TxId -> ExceptT SQ3.Error IO ()
    blockHistoryInsert (Pact.TxId t) =
        exec' db stmt
            [ SInt (fromIntegral $ _evaluationCtxCurrentHeight evalCtx)
            , SBlob (runPutS $ encodeBlockHash $ unwrapParent $ _evaluationCtxParentHash evalCtx)
            , SBlob (runPutS $ encodeBlockPayloadHash $ _evaluationCtxPayload evalCtx)
            , SInt (fromIntegral t)
            ]
        where
        stmt = "INSERT INTO BlockHistory ('blockheight', 'parenthash', 'payloadhash', 'endingtxid') VALUES (?,?,?,?);"

    createUserTable :: SQ3.Utf8 -> ExceptT SQ3.Error IO ()
    createUserTable tablename = do
        createVersionedTable tablename db
        markTableCreation tablename

    -- Mark the table as being created during this block, so that we know
    -- to drop it if we rewind past this block.
    markTableCreation tablename =
        exec' db insertstmt insertargs
        where
        insertstmt = "INSERT OR IGNORE INTO VersionedTableCreation VALUES (?,?)"
        insertargs = [SText tablename, SInt (fromIntegral $ _evaluationCtxCurrentHeight evalCtx)]

    -- | Commit the index of pending successful transactions to the database
    indexPendingPactTransactions :: ExceptT SQ3.Error IO ()
    indexPendingPactTransactions = do
        let txs = _pendingSuccessfulTxs $ _blockHandlePending blockHandle
        dbIndexTransactions txs

        where
        toRow b = [SBlob b, SInt (fromIntegral $ _evaluationCtxCurrentHeight evalCtx)]
        dbIndexTransactions txs = do
            let rows = map toRow $ toList txs
            let q = "INSERT INTO TransactionIndex (txhash, blockheight) VALUES (?, ?)"
            execMulti db q rows

createVersionedTable :: SQ3.Utf8 -> SQ3.Database -> ExceptT SQ3.Error IO ()
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

setConsensusState :: SQ3.Database -> ConsensusState -> ExceptT SQ3.Error IO ()
setConsensusState db cs = do
    exec' db
        "INSERT INTO ConsensusState (blockheight, hash, payloadhash, type) VALUES\
        \(?, ?, ?, 'final'), \
        \(?, ?, ?, 'latest'), \
        \(?, ?, ?, 'safe') \
        \ON CONFLICT REPLACE;"
        $ concatMap toRow
        [ _consensusStateFinal cs
        , _consensusStateSafe cs
        , _consensusStateLatest cs
        ]
    where
    toRow SyncState {..} =
        [ SInt $ fromIntegral @BlockHeight @Int64 _syncStateHeight
        , SBlob $ runPutS (encodeBlockHash _syncStateBlockHash)
        , SBlob $ runPutS (encodeBlockPayloadHash _syncStateBlockPayloadHash)
        ]

getConsensusState :: SQ3.Database -> ExceptT SQ3.Error IO ConsensusState
getConsensusState db = do
    qry db "SELECT FROM ConsensusState (blockheight, hash, payloadhash, type) order by type"
        [] [RInt, RBlob, RBlob, RText] >>= \case
        [final, latest, safe] -> return ConsensusState
            { _consensusStateFinal = readRow "final" final
            , _consensusStateLatest = readRow "latest" latest
            , _consensusStateSafe = readRow "safe" safe
            }
        inv -> error $ "invalid contents of the ConsensusState table: " <> sshow inv
    where
    readRow expectedType [SInt height, SBlob hash, SBlob payloadHash, SText type']
        | expectedType == type' = SyncState
            { _syncStateHeight = fromIntegral @Int64 @BlockHeight height
            , _syncStateBlockHash = either error id $ runGetEitherS decodeBlockHash hash
            , _syncStateBlockPayloadHash = either error id $ runGetEitherS decodeBlockPayloadHash payloadHash
            }
        | otherwise = error $ "wrong type; expected " <> sshow expectedType <> " but got " <> sshow type'
    readRow expectedType invalidRow
        = error $ "invalid row: expected " <> sshow expectedType <> " but got row " <> sshow invalidRow

-- | Create all tables that exist pre-genesis
-- TODO: migrate this logic to the checkpointer itself?
initSchema :: SQLiteEnv -> IO ()
initSchema sql =
    withSavepoint sql InitSchemaSavePoint $ throwOnDbError $ do
        createConsensusStateTable
        createBlockHistoryTable
        createTableCreationTable
        createTableMutationTable
        createTransactionIndexTable
        create (toUtf8 $ Pact.renderDomain Pact.DKeySets)
        create (toUtf8 $ Pact.renderDomain Pact.DModules)
        create (toUtf8 $ Pact.renderDomain Pact.DNamespaces)
        create (toUtf8 $ Pact.renderDomain Pact.DDefPacts)
        create (toUtf8 $ Pact.renderDomain Pact.DModuleSource)
    where
    create tablename = do
        createVersionedTable tablename sql

    createConsensusStateTable :: ExceptT SQ3.Error IO ()
    createConsensusStateTable = do
        exec_ sql
            "CREATE TABLE IF NOT EXISTS ConsensusState \
            \(blockheight UNSIGNED BIGINT NOT NULL, \
            \ hash BLOB NOT NULL, \
            \ payloadhash BLOB NOT NULL, \
            \ type VARCHAR NOT NULL, \
            \ CONSTRAINT typeConstraint UNIQUE (type));"

    createBlockHistoryTable :: ExceptT SQ3.Error IO ()
    createBlockHistoryTable = do
        exec_ sql
            "CREATE TABLE IF NOT EXISTS BlockHistory \
            \(blockheight UNSIGNED BIGINT NOT NULL, \
            \ endingtxid UNSIGNED BIGINT NOT NULL, \
            \ parenthash BLOB NOT NULL, \
            \ payloadhash BLOB NOT NULL, \
            \ CONSTRAINT blockHeightConstraint UNIQUE (blockheight), \
            \ CONSTRAINT parentHashConstraint UNIQUE (parenthash), \
            \ CONSTRAINT payloadHashConstraint UNIQUE (payloadHash));"
        -- TODO PP: payload hash should really be NOT NULL but there may exist old databases without it.
        -- making a block hash index at block height 5,658,430 on us-e3 took around 2.5 minutes

    createTableCreationTable :: ExceptT SQ3.Error IO ()
    createTableCreationTable =
        exec_ sql
            "CREATE TABLE IF NOT EXISTS VersionedTableCreation\
            \(tablename TEXT NOT NULL\
            \, createBlockheight UNSIGNED BIGINT NOT NULL\
            \, CONSTRAINT creation_unique UNIQUE(createBlockheight, tablename));"

    createTableMutationTable :: ExceptT SQ3.Error IO ()
    createTableMutationTable =
        exec_ sql
            "CREATE TABLE IF NOT EXISTS VersionedTableMutation\
            \(tablename TEXT NOT NULL\
            \, blockheight UNSIGNED BIGINT NOT NULL\
            \, CONSTRAINT mutation_unique UNIQUE(blockheight, tablename));"

    createTransactionIndexTable :: ExceptT SQ3.Error IO ()
    createTransactionIndexTable = do
        exec_ sql
            "CREATE TABLE IF NOT EXISTS TransactionIndex \
            \ (txhash BLOB NOT NULL, \
            \ blockheight UNSIGNED BIGINT NOT NULL, \
            \ CONSTRAINT transactionIndexConstraint UNIQUE(txhash));"
        exec_ sql
            "CREATE INDEX IF NOT EXISTS \
            \ transactionIndexByBH ON TransactionIndex(blockheight)";

getSerialiser :: BlockHandler logger (Pact.PactSerialise Pact.CoreBuiltin Pact.LineInfo)
getSerialiser = do
    version <- view blockHandlerVersion
    cid <- view blockHandlerChainId
    blockHeight <- view blockHandlerBlockHeight
    return $ pact5Serialiser version cid blockHeight

getPayloadsAfter :: HasCallStack => SQLiteEnv -> Parent BlockHeight -> ExceptT SQ3.Error IO [Ranked BlockPayloadHash]
getPayloadsAfter db parentHeight = do
    qry db "SELECT blockheight, payloadhash FROM BlockHistory WHERE blockheight > ?"
        [SInt (fromIntegral @BlockHeight @Int64 (unwrapParent parentHeight))]
        [RBlob] >>= traverse
        \case
            [SInt bh, SBlob bhash] ->
                return $! Ranked (fromIntegral @Int64 @BlockHeight bh) $ either error id $ runGetEitherS decodeBlockPayloadHash bhash
            _ -> error "incorrect column type"

-- | Get the checkpointer's idea of the earliest block. The block height
-- is the height of the block of the block hash.
getEarliestBlock :: HasCallStack => SQLiteEnv -> ExceptT SQ3.Error IO (Maybe RankedBlockHash)
getEarliestBlock db = do
    r <- qry db qtext [] [RInt, RBlob] >>= mapM go
    case r of
        [] -> return Nothing
        (!o:_) -> return (Just o)
    where
    qtext = "SELECT blockheight, hash FROM BlockHistory ORDER BY blockheight ASC LIMIT 1"

    go [SInt hgt, SBlob blob] =
        let hash = either error id $ runGetEitherS decodeBlockHash blob
        in return (RankedBlockHash (fromIntegral hgt) hash)
    go _ = fail "Chainweb.Pact.Backend.RelationalCheckpointer.doGetEarliest: impossible. This is a bug in chainweb-node."

lookupBlockWithHeight :: SQ3.Database -> BlockHeight -> ExceptT SQ3.Error IO (Maybe (Ranked (Parent BlockHash)))
lookupBlockWithHeight db bheight = do
    qry db qtext [SInt $ fromIntegral bheight] [RInt] >>= \case
        [[SBlob parentHash]] -> return $! Just $!
            Ranked bheight (either error Parent $ runGetEitherS decodeBlockHash parentHash)
        [_] -> error "lookupBlock: output type mismatch"
        _ -> error "Expected single-row result"
    where
    qtext = "SELECT parenthash FROM BlockHistory WHERE blockheight = ?;"

lookupParentBlockHash :: SQ3.Database -> Parent BlockHash -> ExceptT SQ3.Error IO Bool
lookupParentBlockHash db (Parent parentHash) = do
    qry db qtext [SBlob (runPutS (encodeBlockHash parentHash))] [RInt] >>= \case
        [[SInt n]] -> return $! n == 1
        [_] -> error "lookupBlock: output type mismatch"
        _ -> error "Expected single-row result"
    where
    qtext = "SELECT COUNT(*) FROM BlockHistory WHERE parenthash = ?;"

lookupParentBlockRanked :: SQ3.Database -> Ranked (Parent BlockHash) -> ExceptT SQ3.Error IO Bool
lookupParentBlockRanked db (Ranked bheight (Parent parentHash)) = do
    qry db qtext [SInt $ fromIntegral bheight, SBlob (runPutS (encodeBlockHash parentHash))] [RInt] >>= \case
        [[SInt n]] -> return $! n == 1
        [_] -> error "lookupBlock: output type mismatch"
        _ -> error "Expected single-row result"
    where
    qtext = "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND parenthash = ?;"
