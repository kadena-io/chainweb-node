{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , commitBlockStateToDatabase
    ) where

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
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import Data.Default
import qualified Database.SQLite3.Direct as SQ3

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
import qualified Chainweb.Pact.Backend.InMemDb as InMemDb
import Data.Singletons (Dict(..))
import Chainweb.Utils.Serialization (runPutS)
import Data.Foldable

data BlockHandlerEnv logger = BlockHandlerEnv
    { _blockHandlerDb :: !SQLiteEnv
    , _blockHandlerLogger :: !logger
    , _blockHandlerVersion :: !ChainwebVersion
    , _blockHandlerBlockHeight :: !BlockHeight
    , _blockHandlerChainId :: !ChainId
    , _blockHandlerMode :: !Pact.ExecutionMode
    }

-- | The state used by database operations.
-- Includes both the state re: the whole block, and the state re: a transaction in progress.
data BlockState = BlockState
    { _bsBlockHandle :: !(BlockHandle Pact5)
    , _bsPendingTxWrites :: !(SQLitePendingData InMemDb.Store)
    , _bsPendingTxLog :: !(Maybe (DList (Pact.TxLog ByteString)))
    }

makeLenses ''BlockState
makeLensesWith
    (lensRulesFor
        [ ("_blockHandlerDb", "blockHandlerDb")
        , ("_blockHandlerLogger", "blockHandlerLogger")
        , ("_blockHandlerMode", "blockHandlerMode")
        ])
    ''BlockHandlerEnv

getPendingTxLogOrError :: Text -> BlockHandler logger (DList (Pact.TxLog ByteString))
getPendingTxLogOrError msg = do
    use bsPendingTxLog >>= \case
        Nothing -> liftGas $ Pact.throwDbOpErrorGasM (Pact.NotInTx msg)
        Just t -> return t

-- | The Pact 5 database as it's provided by the checkpointer.
data Pact5Db = Pact5Db
    { doPact5DbTransaction
        :: forall a
        . BlockHandle Pact5
        -> Maybe RequestKey
        -> (Pact.PactDb Pact.CoreBuiltin Pact.Info -> IO a)
        -> IO (a, BlockHandle Pact5)
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
        stateVar <- newMVar $ BlockState blockHandle (_blockHandlePending blockHandle) Nothing
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
    :: forall k v logger
    . Maybe (BlockHeight, Pact.TxId)
    -- ^ the highest block we should be reading writes from
    -> Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> BlockHandler logger (Maybe v)
doReadRow mlim d k = do
    pendingData <- use bsPendingTxWrites
    let !(decodeValue, encodedKey, ordDict :: Dict (Ord k) ()) = case d of
            Pact.DKeySets ->
                (Pact._decodeKeySet Pact.serialisePact_lineinfo, convKeySetName k, Dict ())
            -- TODO: This is incomplete (the modules case), due to namespace
            -- resolution concerns
            Pact.DModules ->
                (Pact._decodeModuleData Pact.serialisePact_lineinfo, convModuleName k, Dict ())
            Pact.DNamespaces ->
                (Pact._decodeNamespace Pact.serialisePact_lineinfo, convNamespaceName k, Dict ())
            Pact.DUserTables _ ->
                (Pact._decodeRowData Pact.serialisePact_lineinfo, convRowKey k, Dict ())
            Pact.DDefPacts ->
                (Pact._decodeDefPactExec Pact.serialisePact_lineinfo, convPactId k, Dict ())
            Pact.DModuleSource ->
                (Pact._decodeModuleCode Pact.serialisePact_lineinfo, convHashedModuleName k, Dict ())
    case ordDict of
        Dict () -> do
            lookupWithKey pendingData (toUtf8 encodedKey) (fmap (view Pact.document) . decodeValue) >>= \case
                Nothing -> return Nothing
                Just (encodedValueLength, decodedValue) -> do
                    case d of
                        Pact.DModules -> do
                            BlockHandler $ lift $ lift
                                $ Pact.chargeGasM (Pact.GModuleOp (Pact.MOpLoadModule encodedValueLength))
                        _ -> return ()
                    case d of
                        Pact.DModuleSource -> return ()
                        _ ->
                            bsPendingTxWrites . pendingWrites %=
                                InMemDb.insert d k (InMemDb.ReadEntry encodedValueLength decodedValue)
                    return (Just decodedValue)
  where
    tablename = domainTableName d

    lookupWithKey
        :: Ord k
        => SQLitePendingData InMemDb.Store
        -> SQ3.Utf8
        -> (BS.ByteString -> Maybe v)
        -> BlockHandler logger (Maybe (Int, v))
    lookupWithKey pds key f = do
        let lookPD = lookupInMem pds
        let lookDB = lookupInDb f key
        runMaybeT (lookPD <|> lookDB)

    lookupInMem
        :: Ord k
        => SQLitePendingData InMemDb.Store
        -> MaybeT (BlockHandler logger) (Int, v)
    lookupInMem p = do
        -- we get the latest-written value at this rowkey
        let store = _pendingWrites p
        case InMemDb.lookup d k store of
            Nothing -> empty
            Just (InMemDb.ReadEntry bs a) -> return (bs, a)
            Just (InMemDb.WriteEntry _ bs a) -> return (BS.length bs, a)

    lookupInDb
        :: (BS.ByteString -> Maybe v)
        -> SQ3.Utf8
        -> MaybeT (BlockHandler logger) (Int, v)
    lookupInDb decode rowkey = do
        -- First, check: did we create this table during this block? If so,
        -- there's no point in looking up the key.
        checkDbTablePendingCreation tablename
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
            [[SBlob a]] -> MaybeT $ return $ (BS.length a,) <$> decode a
            err -> internalDbError $
                     "doReadRow: Expected (at most) a single result, but got: " <>
                     T.pack (show err)


checkDbTablePendingCreation :: SQ3.Utf8 -> MaybeT (BlockHandler logger) ()
checkDbTablePendingCreation (SQ3.Utf8 tablename) = do
    pds <- use bsPendingTxWrites
    when (HashSet.member tablename (_pendingTableCreation pds)) mzero

latestTxId :: Lens' BlockState Pact.TxId
latestTxId = bsBlockHandle . blockHandleTxId . coerced

writeSys
    :: Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> v
    -> BlockHandler logger ()
writeSys d k v = do
  txid <- use latestTxId
  let (kk, vv) = case d of
        Pact.DKeySets -> (convKeySetName k, Pact._encodeKeySet Pact.serialisePact_lineinfo v)
        Pact.DModules ->  (convModuleName k, Pact._encodeModuleData Pact.serialisePact_lineinfo v)
        Pact.DNamespaces -> (convNamespaceName k, Pact._encodeNamespace Pact.serialisePact_lineinfo v)
        Pact.DDefPacts -> (convPactId k, Pact._encodeDefPactExec Pact.serialisePact_lineinfo v)
        Pact.DUserTables _ -> error "impossible"
        Pact.DModuleSource -> (convHashedModuleName k, Pact._encodeModuleCode Pact.serialisePact_lineinfo v)
  recordPendingUpdate d k txid (vv, v)
  recordTxLog d kk vv

recordPendingUpdate
    :: Pact.Domain k v Pact.CoreBuiltin Pact.Info
    -> k
    -> Pact.TxId
    -> (ByteString, v)
    -> BlockHandler logger ()
recordPendingUpdate d k txid (encodedValue, decodedValue) =
    bsPendingTxWrites . pendingWrites %=
        InMemDb.insert d k (InMemDb.WriteEntry txid encodedValue decodedValue)

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

  upd txid (Pact.RowData oldrow) = do
      let row' = Pact.RowData (M.union row oldrow)
      liftGas (Pact._encodeRowData Pact.serialisePact_lineinfo row') >>=
          \encoded -> do
              recordPendingUpdate d k (Pact.TxId txid) (encoded, row')
              return row'

  ins txid = do
      liftGas (Pact._encodeRowData Pact.serialisePact_lineinfo rowdata) >>=
          \encoded -> do
              recordPendingUpdate d k (Pact.TxId txid) (encoded, rowdata)
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
            return $ sort (parsedKeys ++ memKeys)

    where
    blockLimitStmt = maybe "" (const " WHERE txid < ?;") mlim
    blockLimitParam = maybe [] (\(Pact.TxId txid) -> [SInt (fromIntegral txid)]) (snd <$> mlim)

    getDbKeys = do
        m <- runMaybeT $ checkDbTablePendingCreation tn
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

    tn = toUtf8 $ Pact.renderDomain d
    collect p = InMemDb.keys d (_pendingWrites p)

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
    :: Maybe BlockHeight
    -- ^ the highest block we should be seeing tables from
    -> Pact.TableName
    -> BlockHandler logger ()
doCreateUserTable mbh tn = do
    -- first check if tablename already exists in pending queues
    m <- runMaybeT $ checkDbTablePendingCreation (tableNameToSQL tn)
    case m of
        Nothing ->
            liftGas $ Pact.throwDbOpErrorGasM $ Pact.TableAlreadyExists tn
        Just () -> do
            -- then check if it is in the db
            cond <- inDb $ SQ3.Utf8 $ T.encodeUtf8 $ Pact.renderTableName tn
            when cond $
                liftGas $ Pact.throwDbOpErrorGasM $ Pact.TableAlreadyExists tn

            bsPendingTxWrites . pendingTableCreation %=
                HashSet.insert (T.encodeUtf8 (Pact.renderTableName tn))
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

toTxLog :: MonadThrow m => T.Text -> SQ3.Utf8 -> BS.ByteString -> m (Pact.TxLog Pact.RowData)
toTxLog d key value =
        case fmap (view Pact.document) $ Pact._decodeRowData Pact.serialisePact_lineinfo value of
            Nothing -> internalDbError $ "toTxLog: Unexpected value, unable to deserialize log: " <> sshow value
            Just v -> return $! Pact.TxLog d (fromUtf8 key) v

toPactTxLog :: Pact.TxLog Pact.RowData -> Pact4.TxLog Pact.RowData
toPactTxLog (Pact.TxLog d k v) = Pact4.TxLog d k v

commitBlockStateToDatabase :: SQLiteEnv -> BlockHash -> BlockHeight -> BlockHandle Pact5 -> IO ()
commitBlockStateToDatabase db hsh bh blockHandle = do
    let newTables = _pendingTableCreation $ _blockHandlePending blockHandle
    mapM_ (\tn -> createUserTable (SQ3.Utf8 tn)) newTables
    backendWriteUpdateBatch (_pendingWrites (_blockHandlePending blockHandle))
    indexPendingPactTransactions
    let nextTxId = _blockHandleTxId blockHandle
    blockHistoryInsert nextTxId
    where

    backendWriteUpdateBatch
        :: InMemDb.Store
        -> IO ()
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
        domainToTableName = SQ3.Utf8 . T.encodeUtf8 . Pact.renderDomain
        prepRow rowkey (InMemDb.WriteEntry (Pact.TxId txid) rowdataEncoded _) =
            Just
                [ SText (toUtf8 rowkey)
                , SInt (fromIntegral txid)
                , SBlob rowdataEncoded
                ]
        prepRow _ InMemDb.ReadEntry {} = Nothing

        writeTable :: SQ3.Utf8 -> [[SType]] -> IO ()
        writeTable table writes = when (not (null writes)) $ do
            execMulti db q writes
            markTableMutation table bh
            where
            q = "INSERT OR REPLACE INTO " <> tbl table <> "(rowkey,txid,rowdata) VALUES(?,?,?)"

        -- Mark the table as being mutated during this block, so that we know
        -- to delete from it if we rewind past this block.
        markTableMutation tablename blockheight = do
            exec' db mutq [SText tablename, SInt (fromIntegral blockheight)]
            where
            mutq = "INSERT OR IGNORE INTO VersionedTableMutation VALUES (?,?);"

    -- | Record a block as being in the history of the checkpointer.
    blockHistoryInsert :: Pact4.TxId -> IO ()
    blockHistoryInsert t =
        exec' db stmt
            [ SInt (fromIntegral bh)
            , SBlob (runPutS (encodeBlockHash hsh))
            , SInt (fromIntegral t)
            ]
        where
        stmt = "INSERT INTO BlockHistory ('blockheight','hash','endingtxid') VALUES (?,?,?);"

    createUserTable :: SQ3.Utf8 -> IO ()
    createUserTable tablename = do
        createVersionedTable tablename db
        markTableCreation tablename

    -- Mark the table as being created during this block, so that we know
    -- to drop it if we rewind past this block.
    markTableCreation tablename =
        exec' db insertstmt insertargs
        where
        insertstmt = "INSERT OR IGNORE INTO VersionedTableCreation VALUES (?,?)"
        insertargs = [SText tablename, SInt (fromIntegral bh)]

    -- | Commit the index of pending successful transactions to the database
    indexPendingPactTransactions :: IO ()
    indexPendingPactTransactions = do
        let txs = _pendingSuccessfulTxs $ _blockHandlePending blockHandle
        dbIndexTransactions txs

        where
        toRow b = [SBlob b, SInt (fromIntegral bh)]
        dbIndexTransactions txs = do
            let rows = map toRow $ toList txs
            let q = "INSERT INTO TransactionIndex (txhash, blockheight) VALUES (?, ?)"
            execMulti db q rows
