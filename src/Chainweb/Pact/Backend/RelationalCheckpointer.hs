{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Chainweb.Pact.Backend.RelationalCheckpointer
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emmanuel Denloye <emmanuel@kadena.io>
-- Stability: experimental
--
-- Pact Checkpointer for Chainweb
module Chainweb.Pact.Backend.RelationalCheckpointer
  ( initRelationalCheckpointer
  , initRelationalCheckpointer'
  , withProdRelationalCheckpointer
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as BS
import Data.Aeson hiding (encode,(.=))
import qualified Data.DList as DL
import Data.Foldable (toList,foldl')
import Data.Int
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as TimSort

import Database.SQLite3.Direct

import Prelude hiding (log)

import System.LogLevel

-- pact

import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Hash (PactHash, TypedHash(..))
import Pact.Types.Logger (Logger(..))
import Pact.Types.Persistence
import Pact.Types.SQLite

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import qualified Chainweb.Logger as C
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.DbCache (updateCacheStats)
import Chainweb.Pact.Service.Types
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version

initRelationalCheckpointer
    :: BlockState
    -> SQLiteEnv
    -> Logger
    -> ChainwebVersion
    -> ChainId
    -> IO CheckpointEnv
initRelationalCheckpointer bstate sqlenv loggr v cid =
    snd <$!> initRelationalCheckpointer' bstate sqlenv loggr v cid

withProdRelationalCheckpointer
    :: C.Logger logger
    => logger
    -> BlockState
    -> SQLiteEnv
    -> Logger
    -> ChainwebVersion
    -> ChainId
    -> (CheckpointEnv -> IO a)
    -> IO a
withProdRelationalCheckpointer logger bstate sqlenv pactLogger v cid inner = do
    (dbenv, cpenv) <- initRelationalCheckpointer' bstate sqlenv pactLogger v cid
    withAsync (logModuleCacheStats dbenv) $ \_ -> inner cpenv
  where
    logFun = C.logFunctionText logger
    logModuleCacheStats e = runForever logFun "ModuleCacheStats" $ do
        stats <- modifyMVar (pdPactDbVar e) $ \db -> do
            let (s, !mc') = updateCacheStats $ _bsModuleCache $ _benvBlockState db
                !db' = set (benvBlockState . bsModuleCache) mc' db
            return (db', s)
        C.logFunctionJson logger Info stats
        threadDelay (60 * 1000000)

-- for testing
initRelationalCheckpointer'
    :: BlockState
    -> SQLiteEnv
    -> Logger
    -> ChainwebVersion
    -> ChainId
    -> IO (PactDbEnv (BlockEnv SQLiteEnv), CheckpointEnv)
initRelationalCheckpointer' bstate sqlenv loggr v cid = do
    let dbenv = BlockDbEnv sqlenv loggr
    db <- newMVar (BlockEnv dbenv bstate)
    runBlockEnv db initSchema
    return
      (PactDbEnv chainwebPactDb db,
       CheckpointEnv
        { _cpeCheckpointer =
            Checkpointer
            {
                _cpRestore = doRestore v cid db
              , _cpSave = doSave db
              , _cpDiscard = doDiscard db
              , _cpGetLatestBlock = doGetLatest db
              , _cpBeginCheckpointerBatch = doBeginBatch db
              , _cpCommitCheckpointerBatch = doCommitBatch db
              , _cpDiscardCheckpointerBatch = doDiscardBatch db
              , _cpLookupBlockInCheckpointer = doLookupBlock db
              , _cpGetBlockParent = doGetBlockParent v cid db
              , _cpRegisterProcessedTx = doRegisterSuccessful db
              , _cpLookupProcessedTx = doLookupSuccessful db
              , _cpGetBlockHistory = doGetBlockHistory db
              , _cpGetHistoricalLookup = doGetHistoricalLookup db
              }
        , _cpeLogger = loggr
        })

type Db = MVar (BlockEnv SQLiteEnv)

doRestore :: ChainwebVersion -> ChainId -> Db -> Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore v cid dbenv (Just (bh, hash)) = runBlockEnv dbenv $ do
    setModuleNameFix
    setSortedKeys
    setLowerCaseTables
    clearPendingTxState
    void $ withSavepoint PreBlock $ handlePossibleRewind v cid bh hash
    beginSavepoint Block
    return $! PactDbEnv' $! PactDbEnv chainwebPactDb dbenv
  where
    -- Module name fix follows the restore call to checkpointer.
    setModuleNameFix = bsModuleNameFix .= enableModuleNameFix v bh
    setSortedKeys = bsSortedKeys .= pact420Upgrade v bh
    setLowerCaseTables = bsLowerCaseTables .= chainweb217Pact After v bh
doRestore _ _ dbenv Nothing = runBlockEnv dbenv $ do
    clearPendingTxState
    withSavepoint DbTransaction $
      callDb "doRestoreInitial: resetting tables" $ \db -> do
        exec_ db "DELETE FROM BlockHistory;"
        exec_ db "DELETE FROM [SYS:KeySets];"
        exec_ db "DELETE FROM [SYS:Modules];"
        exec_ db "DELETE FROM [SYS:Namespaces];"
        exec_ db "DELETE FROM [SYS:Pacts];"
        tblNames <- qry_ db "SELECT tablename FROM VersionedTableCreation;" [RText]
        forM_ tblNames $ \tbl' -> case tbl' of
            [SText t] -> exec_ db ("DROP TABLE [" <> t <> "];")
            _ -> internalError "Something went wrong when resetting tables."
        exec_ db "DELETE FROM VersionedTableCreation;"
        exec_ db "DELETE FROM VersionedTableMutation;"
        exec_ db "DELETE FROM TransactionIndex;"
    beginSavepoint Block
    assign bsTxId 0
    return $! PactDbEnv' $ PactDbEnv chainwebPactDb dbenv

doSave :: Db -> BlockHash -> IO ()
doSave dbenv hash = runBlockEnv dbenv $ do
    height <- gets _bsBlockHeight
    runPending height
    nextTxId <- gets _bsTxId
    blockHistoryInsert height hash nextTxId

    -- FIXME: if any of the above fails with an exception the following isn't
    -- executed and a pending SAVEPOINT is left on the stack.
    commitSavepoint Block
    clearPendingTxState
  where
    runPending :: BlockHeight -> BlockHandler SQLiteEnv ()
    runPending bh = do
        newTables <- use $ bsPendingBlock . pendingTableCreation
        writes <- use $ bsPendingBlock . pendingWrites
        createNewTables bh $ toList newTables
        writeV <- toVectorChunks writes
        callDb "save" $ backendWriteUpdateBatch bh writeV
        indexPendingPactTransactions

    prepChunk [] = error "impossible: empty chunk from groupBy"
    prepChunk chunk@(h:_) = (Utf8 $ _deltaTableName h, V.fromList chunk)

    toVectorChunks writes = liftIO $ do
        mv <- mutableVectorFromList . DL.toList . DL.concat $
              HashMap.elems writes
        TimSort.sort mv
        l' <- V.toList <$> V.unsafeFreeze mv
        let ll = List.groupBy (\a b -> _deltaTableName a == _deltaTableName b) l'
        return $ map prepChunk ll

    createNewTables
        :: BlockHeight
        -> [ByteString]
        -> BlockHandler SQLiteEnv ()
    createNewTables bh = mapM_ (\tn -> createUserTable (Utf8 tn) bh)

-- | Discards all transactions since the most recent @Block@ savepoint and
-- removes the savepoint from the transaction stack.
--
doDiscard :: Db -> IO ()
doDiscard dbenv = runBlockEnv dbenv $ do
    clearPendingTxState
    rollbackSavepoint Block

    -- @ROLLBACK TO n@ only rolls back updates up to @n@ but doesn't remove the
    -- savepoint. In order to also pop the savepoint from the stack we commit it
    -- (as empty transaction). <https://www.sqlite.org/lang_savepoint.html>
    --
    commitSavepoint Block

doGetLatest :: Db -> IO (Maybe (BlockHeight, BlockHash))
doGetLatest dbenv =
    runBlockEnv dbenv $ callDb "getLatestBlock" $ \db -> do
        r <- qry_ db qtext [RInt, RBlob] >>= mapM go
        case r of
          [] -> return Nothing
          (!o:_) -> return (Just o)
  where
    qtext = "SELECT blockheight, hash FROM BlockHistory \
            \ ORDER BY blockheight DESC LIMIT 1"

    go [SInt hgt, SBlob blob] =
        let hash = either error id $ runGetEitherS decodeBlockHash blob
        in return (fromIntegral hgt, hash)
    go _ = fail "impossible"

doBeginBatch :: Db -> IO ()
doBeginBatch db = runBlockEnv db $ beginSavepoint BatchSavepoint

doCommitBatch :: Db -> IO ()
doCommitBatch db = runBlockEnv db $ commitSavepoint BatchSavepoint

-- | Discards all transactions since the most recent @BatchSavepoint@ savepoint
-- and removes the savepoint from the transaction stack.
--
doDiscardBatch :: Db -> IO ()
doDiscardBatch db = runBlockEnv db $ do
    rollbackSavepoint BatchSavepoint

    -- @ROLLBACK TO n@ only rolls back updates up to @n@ but doesn't remove the
    -- savepoint. In order to also pop the savepoint from the stack we commit it
    -- (as empty transaction). <https://www.sqlite.org/lang_savepoint.html>
    --
    commitSavepoint BatchSavepoint

doLookupBlock :: Db -> (BlockHeight, BlockHash) -> IO Bool
doLookupBlock dbenv (bheight, bhash) = runBlockEnv dbenv $ do
    r <- callDb "lookupBlock" $ \db ->
         qry db qtext [SInt $ fromIntegral bheight, SBlob (runPutS (encodeBlockHash bhash))]
                      [RInt]
    liftIO (expectSingle "row" r) >>= \case
        [SInt n] -> return $! n /= 0
        _ -> internalError "doLookupBlock: output mismatch"
  where
    qtext = "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? \
            \ AND hash = ?;"

doGetBlockParent :: ChainwebVersion -> ChainId -> Db -> (BlockHeight, BlockHash) -> IO (Maybe BlockHash)
doGetBlockParent v cid dbenv (bh, hash)
    | bh == genesisHeight v cid = return Nothing
    | otherwise = do
        blockFound <- doLookupBlock dbenv (bh, hash)
        if not blockFound
          then return Nothing
          else runBlockEnv dbenv $ do
            r <- callDb "getBlockParent" $ \db -> qry db qtext [SInt (fromIntegral (pred bh))] [RBlob]
            case r of
              [[SBlob blob]] ->
                either (internalError . T.pack) (return . return) $! runGetEitherS decodeBlockHash blob
              _ -> internalError "doGetBlockParent: output mismatch"
  where
    qtext = "SELECT hash FROM BlockHistory WHERE blockheight = ?"


doRegisterSuccessful :: Db -> PactHash -> IO ()
doRegisterSuccessful dbenv (TypedHash hash) =
    runBlockEnv dbenv (indexPactTransaction $ BS.fromShort hash)


doLookupSuccessful :: Db -> PactHash -> IO (Maybe (T2 BlockHeight BlockHash))
doLookupSuccessful dbenv (TypedHash hash) = runBlockEnv dbenv $ do
    r <- callDb "doLookupSuccessful" $ \db ->
         qry db qtext [ SBlob (BS.fromShort hash) ] [RInt, RBlob] >>= mapM go
    case r of
        [] -> return Nothing
        (!o:_) -> return (Just o)
  where
    qtext = "SELECT blockheight, hash FROM \
            \TransactionIndex INNER JOIN BlockHistory \
            \USING (blockheight) WHERE txhash = ?;"
    go [SInt h, SBlob blob] = do
        !hsh <- either fail return $ runGetEitherS decodeBlockHash blob
        return $! T2 (fromIntegral h) hsh
    go _ = fail "impossible"

doGetBlockHistory :: FromJSON v => Db -> BlockHeader -> Domain k v -> IO BlockTxHistory
doGetBlockHistory dbenv blockHeader d = runBlockEnv dbenv $ do
  callDb "doGetBlockHistory" $ \db -> do
    endTxId <- getEndTxId db bHeight (_blockHash blockHeader)
    startTxId <- if (bHeight == genesisHeight v cid)
      then pure 0  -- genesis block
      else getEndTxId db (pred bHeight) (_blockParent blockHeader)
    let tname = domainTableName d
    history <- queryHistory db tname startTxId endTxId
    let (!hkeys,tmap) = foldl' procTxHist (S.empty,mempty) history
    !prev <- M.fromList . catMaybes <$> mapM (queryPrev db tname startTxId) (S.toList hkeys)
    return $ BlockTxHistory tmap prev
  where
    v = _blockChainwebVersion blockHeader
    cid = _blockChainId blockHeader
    bHeight = _blockHeight blockHeader

    procTxHist
      :: (S.Set Utf8, M.Map TxId [TxLog Value])
      -> (Utf8,TxId,TxLog Value)
      -> (S.Set Utf8,M.Map TxId [TxLog Value])
    procTxHist (ks,r) (uk,t,l) = (S.insert uk ks, M.insertWith (++) t [l] r)

    -- Start index is inclusive, while ending index is not.
    -- `endingtxid` in a block is the beginning txid of the following block.
    queryHistory :: Database -> Utf8 -> Int64 -> Int64 -> IO [(Utf8,TxId,TxLog Value)]
    queryHistory db tableName s e = do
      let sql = "SELECT txid, rowkey, rowdata FROM [" <> tableName <>
                "] WHERE txid >= ? AND txid < ?"
      r <- qry db sql
           [SInt s,SInt e]
           [RInt,RText,RBlob]
      forM r $ \case
        [SInt txid, SText key, SBlob value] -> (key,fromIntegral txid,) <$> toTxLog d key value
        err -> internalError $
               "queryHistory: Expected single row with three columns as the \
               \result, got: " <> T.pack (show err)

    -- Get last tx data, if any, for key before start index.
    queryPrev :: Database -> Utf8 -> Int64 -> Utf8 -> IO (Maybe (RowKey,TxLog Value))
    queryPrev db tableName s k@(Utf8 sk) = do
      let sql = "SELECT rowdata FROM [" <> tableName <>
                "] WHERE rowkey = ? AND txid < ? " <>
                "ORDER BY txid DESC LIMIT 1"
      r <- qry db sql
           [SText k,SInt s]
           [RBlob]
      case r of
        [] -> return Nothing
        [[SBlob value]] -> Just . (RowKey $ T.decodeUtf8 sk,) <$> toTxLog d k value
        _ -> internalError $ "queryPrev: expected 0 or 1 rows, got: " <> T.pack (show r)


getEndTxId :: Database -> BlockHeight -> BlockHash -> IO Int64
getEndTxId db bhi bha = do
  r <- qry db
    "SELECT endingtxid FROM BlockHistory WHERE blockheight = ? and hash = ?;"
    [SInt $ fromIntegral bhi, SBlob $ runPutS (encodeBlockHash bha)]
    [RInt]
  case r of
    [[SInt tid]] -> return tid
    [] -> throwM $ BlockHeaderLookupFailure $ "doGetBlockHistory: not in db: " <>
          sshow (bhi,bha)
    _ -> internalError $ "doGetBlockHistory: expected single-row int result, got " <> sshow r

doGetHistoricalLookup
    :: FromJSON v
    => Db
    -> BlockHeader
    -> Domain k v
    -> RowKey
    -> IO (Maybe (TxLog Value))
doGetHistoricalLookup dbenv blockHeader d k = runBlockEnv dbenv $ do
  callDb "doGetHistoricalLookup" $ \db -> do
    endTxId <- getEndTxId db bHeight (_blockHash blockHeader)
    latestEntry <- queryHistoryLookup db (domainTableName d) endTxId (convRowKey k)
    return $! latestEntry
  where
    bHeight = _blockHeight blockHeader

    queryHistoryLookup :: Database -> Utf8 -> Int64 -> Utf8 -> IO (Maybe (TxLog Value))
    queryHistoryLookup db tableName e rowKeyName = do
      let sql = "SELECT rowKey, rowdata FROM [" <> tableName <>
                "] WHERE txid < ? AND rowkey = ? ORDER BY txid DESC LIMIT 1;"
      r <- qry db sql
           [SInt e, SText rowKeyName]
           [RText, RBlob]
      case r of
        [[SText key, SBlob value]] -> Just <$> toTxLog d key value
        [] -> pure Nothing
        _ -> internalError $ "doGetHistoricalLookup: expected single-row result, got " <> sshow r
