{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
  -- , initRelationalReadCheckpointer'
  , withProdRelationalCheckpointer
  -- , withProdRelationalReadCheckpointer
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State (gets)

import Data.ByteString (ByteString, intercalate)
import qualified Data.ByteString.Short as BS
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
import GHC.Stack (HasCallStack)

import Database.SQLite3.Direct

import Prelude hiding (log)
import Streaming
import qualified Streaming.Prelude as Streaming

import System.LogLevel

-- pact

import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Hash (PactHash, TypedHash(..))
import Pact.Types.Persistence
import Pact.Types.RowData
import Pact.Types.SQLite

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.DbCache (updateCacheStats)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types (defaultModuleCacheLimit)
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Guards

initRelationalCheckpointer
    :: (Logger logger)
    => BlockState
    -> SQLiteEnv
    -> logger
    -> ChainwebVersion
    -> ChainId
    -> IO (Checkpointer logger)
initRelationalCheckpointer bstate sqlenv loggr v cid =
    snd <$!> initRelationalCheckpointer' bstate sqlenv loggr v cid

withProdRelationalCheckpointer
    :: (Logger logger)
    => logger
    -> BlockState
    -> SQLiteEnv
    -> ChainwebVersion
    -> ChainId
    -> (Checkpointer logger -> IO a)
    -> IO a
withProdRelationalCheckpointer logger bstate sqlenv v cid inner = do
    (dbenv, cp) <- initRelationalCheckpointer' bstate sqlenv logger v cid
    withAsync (logModuleCacheStats (_cpPactDbEnv dbenv)) $ \_ -> inner cp
  where
    logFun = logFunctionText logger
    logModuleCacheStats e = runForever logFun "ModuleCacheStats" $ do
        stats <- modifyMVar (pdPactDbVar e) $ \db -> do
            let (s, !mc') = updateCacheStats $ _bsModuleCache $ _benvBlockState db
                !db' = set (benvBlockState . bsModuleCache) mc' db
            return (db', s)
        logFunctionJson logger Info stats
        threadDelay 60_000_000 {- 1 minute -}

-- for testing
-- TODO: why would *any* test want this? "Pact DB" is an undefined notion
-- if the checkpointer hasn't been restored.
initRelationalCheckpointer'
    :: (Logger logger)
    => BlockState
    -> SQLiteEnv
    -> logger
    -> ChainwebVersion
    -> ChainId
    -> IO (CurrentBlockDbEnv logger, Checkpointer logger)
initRelationalCheckpointer' bstate sqlenv loggr v cid = do
    let dbenv = BlockDbEnv sqlenv loggr
    db <- newMVar (BlockEnv dbenv bstate)
    runBlockEnv db initSchema
    let
      blockDbEnv = CurrentBlockDbEnv
          { _cpPactDbEnv = PactDbEnv chainwebPactDb db
          , _cpRegisterProcessedTx =
            \(TypedHash hash) -> runBlockEnv db (indexPactTransaction $ BS.fromShort hash)
          , _cpLookupProcessedTx = \cd hs -> runBlockEnv db $ doLookupSuccessful cd hs
          }
      checkpointer = Checkpointer
          { _cpRewindAndExtend = doRewindAndExtend loggr v cid db
          , _cpReadCp = ReadCheckpointer
              { _cpReadFrom = doReadFrom loggr v cid sqlenv
              , _cpGetBlockHistory = doGetBlockHistory db
              , _cpGetHistoricalLookup = doGetHistoricalLookup db
              , _cpGetEarliestBlock = doGetEarliestBlock db
              , _cpGetLatestBlock = doGetLatestBlock db
              , _cpLookupBlockInCheckpointer = doLookupBlock db
              , _cpGetBlockParent = doGetBlockParent v cid db
              , _cpReadLogger = loggr
              }
          }
    return (blockDbEnv, checkpointer)


type Db logger = MVar (BlockEnv logger SQLiteEnv)

-- TODO: what does this actually do if I'm missing this header?
doReadFrom
  :: (Logger logger, MonadIO m, MonadMask m)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> SQLiteEnv
  -> Maybe ParentContext
  -> (CurrentBlockDbEnv logger -> m a)
  -> m a
doReadFrom logger v cid sql parent doRead = mask $ \resetMask -> do
  let parentHeight = maybe (genesisHeight v cid) _parentContextHeight parent
  -- TODO: grab the real module cache?
  newDbEnv <- liftIO $ newMVar (BlockEnv (BlockDbEnv sql logger) (initBlockState defaultModuleCacheLimit (parentHeight + 1)))
  liftIO $ runBlockEnv newDbEnv $ beginSavepoint BatchSavepoint
  -- NB it's important to do this *after* you start the savepoint (and thus
  -- the db transaction) to make sure that the latestHeader check is up to date.
  latestHeader <- liftIO $ doGetLatestBlock newDbEnv
  let
    parentIsLatestHeader = case (latestHeader, parent) of
      (Nothing, Nothing) -> True
      (Just (_, latestHash), Just (_parentContextHash -> parentHash)) -> parentHash == latestHash
      _ -> False

  (`finally` liftIO (runBlockEnv newDbEnv (rollbackSavepoint BatchSavepoint >> commitSavepoint BatchSavepoint))) $ do
    liftIO $ runBlockEnv newDbEnv $ do
      bsModuleNameFix .= enableModuleNameFix v cid (parentHeight + 1)
      bsSortedKeys .= pact42 v cid (parentHeight + 1)
      bsLowerCaseTables .= chainweb217Pact v cid (parentHeight + 1)
    txid <- liftIO $ runBlockEnv newDbEnv $ case parent of
      Nothing -> do
        withSavepoint DbTransaction $
          callDb "doRestoreInitial: resetting tables" $ \db -> do
            exec_ db "DELETE FROM BlockHistory;"
            exec_ db "DELETE FROM [SYS:KeySets];"
            exec_ db "DELETE FROM [SYS:Modules];"
            exec_ db "DELETE FROM [SYS:Namespaces];"
            exec_ db "DELETE FROM [SYS:Pacts];"
            tblNames <- qry_ db "SELECT tablename FROM VersionedTableCreation;" [RText]
            forM_ tblNames $ \tbl -> case tbl of
                [SText t] -> exec_ db ("DROP TABLE [" <> t <> "];")
                _ -> internalError "Something went wrong when resetting tables."
            exec_ db "DELETE FROM VersionedTableCreation;"
            exec_ db "DELETE FROM VersionedTableMutation;"
            exec_ db "DELETE FROM TransactionIndex;"
        bsTxId .= 0
        return 0
      Just pc -> do
        txid <- handlePossibleRewind v cid (succ $ _parentContextHeight pc) (_parentContextHash pc)
        bsTxId .= txid
        return txid
    let
      pactDb =
        if parentIsLatestHeader
        then chainwebPactDb
        else rewoundPactDb (parentHeight + 1) txid
      curBlockDbEnv = CurrentBlockDbEnv
        { _cpPactDbEnv = PactDbEnv pactDb newDbEnv
        , _cpRegisterProcessedTx =
          \(TypedHash hash) -> runBlockEnv newDbEnv (indexPactTransaction $ BS.fromShort hash)
        , _cpLookupProcessedTx = \cd hs -> do
          rs <- runBlockEnv newDbEnv (doLookupSuccessful cd hs)
          return $! HashMap.filter (\(T2 bh _) -> bh <= parentHeight) rs
        }
    a <- resetMask (doRead curBlockDbEnv)
    return a

-- TODO: log more.
-- see the docs for _cpRewindAndExtend.
doRewindAndExtend
  :: forall logger m blk r q.
  (Logger logger, Monoid q, MonadIO m, MonadMask m, HasCallStack)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> Db logger
  -> Maybe ParentContext
  -> (blk -> ParentContext)
  -> Stream (Of blk) m r
  -> (CurrentBlockDbEnv logger -> ParentContext -> blk -> m q)
  -> m (r, q)
doRewindAndExtend loggr v cid dbenv parent getParentContext blocks execBlock = mask $ \resetMask -> do
    liftIO $ runBlockEnv dbenv clearPendingTxState
    resetMask $ liftIO $ runBlockEnv dbenv $ beginSavepoint BatchSavepoint
    ((q, _) :> r) <- resetMask go `onException` liftIO
      (runBlockEnv dbenv (rollbackSavepoint BatchSavepoint >> commitSavepoint BatchSavepoint))
    liftIO $ runBlockEnv dbenv $ commitSavepoint BatchSavepoint
    return (r, q)
  where
    go = liftIO (runBlockEnv dbenv rewind) >> extend
    rewind :: BlockHandler logger SQLiteEnv ()
    rewind = case parent of
      Nothing -> do
        withSavepoint DbTransaction $
          callDb "doRestoreInitial: resetting tables" $ \db -> do
            exec_ db "DELETE FROM BlockHistory;"
            exec_ db "DELETE FROM [SYS:KeySets];"
            exec_ db "DELETE FROM [SYS:Modules];"
            exec_ db "DELETE FROM [SYS:Namespaces];"
            exec_ db "DELETE FROM [SYS:Pacts];"
            tblNames <- qry_ db "SELECT tablename FROM VersionedTableCreation;" [RText]
            forM_ tblNames $ \tbl -> case tbl of
                [SText t] -> exec_ db ("DROP TABLE [" <> t <> "];")
                _ -> internalError "Something went wrong when resetting tables."
            exec_ db "DELETE FROM VersionedTableCreation;"
            exec_ db "DELETE FROM VersionedTableMutation;"
            exec_ db "DELETE FROM TransactionIndex;"
      Just pc -> do
        void $ withSavepoint PreBlock $
          handlePossibleRewind v cid (succ (_parentContextHeight pc)) (_parentContextHash pc)

    extend :: m (Of (q, ParentContext) r)
    extend = Streaming.foldM
      -- todo: check that each block header is actually a child of the previous.
      (\(m, pc) b -> do
        let
          curPc = getParentContext b
          bh = _parentContextHeight curPc
          curBlockDbEnv = CurrentBlockDbEnv
            { _cpPactDbEnv = PactDbEnv chainwebPactDb dbenv
            , _cpRegisterProcessedTx =
              \(TypedHash hash) -> runBlockEnv dbenv (indexPactTransaction $ BS.fromShort hash)
            , _cpLookupProcessedTx = \cd hs -> runBlockEnv dbenv $ doLookupSuccessful cd hs
            }
        if genesisHeight v cid == bh
        then unless (_parentContextHeight pc == bh) $ throwM $
            PactInternalError "doRewindAndExtend: genesis block should have same height as genesis parent."
        else unless (succ (_parentContextHeight pc) == bh) $ throwM $
            PactInternalError $ "doRewindAndExtend: non-genesis block should be one higher than its parent. parent at " <> sshow (_parentContextHeight pc) <> ", child height " <> sshow bh
        liftIO $ runBlockEnv dbenv $ do
          clearPendingTxState
          assign bsBlockHeight bh
          bsModuleNameFix .= enableModuleNameFix v cid bh
          bsSortedKeys .= pact42 v cid bh
          bsLowerCaseTables .= chainweb217Pact v cid bh
          !txid <-
            if bh == genesisHeight v cid then return 0
            else callDb "getting txid" $ \db ->
              getEndTxId db (_parentContextHeight pc) (_parentContextHash pc)
          assign bsTxId (fromIntegral txid)
        m' <- execBlock curBlockDbEnv pc b
        liftIO $ runBlockEnv dbenv $ do
          newTables <- use $ bsPendingBlock . pendingTableCreation
          createNewTables bh $ toList newTables
          writes <- use $ bsPendingBlock . pendingWrites
          writeV <- toVectorChunks writes
          callDb "save" $ backendWriteUpdateBatch bh writeV
          indexPendingPactTransactions
          nextTxId <- gets _bsTxId
          blockHistoryInsert bh (_parentContextHash curPc) nextTxId
          clearPendingTxState
        let !m'' = m <> m'
        return $ (m'', curPc)
      -- NOTE: we explicitly make the parent of the genesis header also the genesis header.
      -- this is wrong, *but*, we happen to not use this information in any nefarious ways
      -- in PactService.
      ) (return (mempty, fromMaybe (genesisParentContext v cid) parent)) return blocks

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
        -> BlockHandler logger SQLiteEnv ()
    createNewTables bh = mapM_ (\tn -> createUserTable (Utf8 tn) bh)

doGetEarliestBlock :: HasCallStack => Db logger -> IO (Maybe (BlockHeight, BlockHash))
doGetEarliestBlock dbenv =
  runBlockEnv dbenv $ callDb "getEarliestBlock" $ \db -> do
    r <- qry_ db qtext [RInt, RBlob] >>= mapM go
    case r of
      [] -> return Nothing
      (!o:_) -> return (Just o)
  where
    qtext = "SELECT blockheight, hash FROM BlockHistory \
            \ ORDER BY blockheight ASC LIMIT 1"

    go [SInt hgt, SBlob blob] =
        let hash = either error id $ runGetEitherS decodeBlockHash blob
        in return (fromIntegral hgt, hash)
    go _ = fail "Chainweb.Pact.Backend.RelationalCheckpointer.doGetEarliest: impossible. This is a bug in chainweb-node."

doGetLatestBlock :: HasCallStack => Db logger -> IO (Maybe (BlockHeight, BlockHash))
doGetLatestBlock dbenv =
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
    go _ = fail "Chainweb.Pact.Backend.RelationalCheckpointer.doGetLatest: impossible. This is a bug in chainweb-node."

doLookupBlock :: Db logger -> (BlockHeight, BlockHash) -> IO Bool
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

doGetBlockParent :: ChainwebVersion -> ChainId -> Db logger -> (BlockHeight, BlockHash) -> IO (Maybe BlockHash)
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

doLookupSuccessful :: Maybe ConfirmationDepth -> V.Vector PactHash -> BlockHandler logger SQLiteEnv (HashMap.HashMap PactHash (T2 BlockHeight BlockHash))
doLookupSuccessful confDepth hashes = do
    withSavepoint DbTransaction $ do
      r <- callDb "doLookupSuccessful" $ \db -> do
        let
          currentHeightQ = "SELECT blockheight FROM BlockHistory \
              \ ORDER BY blockheight DESC LIMIT 1"

        -- if there is a confirmation depth, we get the current height and calculate
        -- the block height, to look for the transactions in range [0, current block height - confirmation depth]
        blockheight <- case confDepth of
          Nothing -> pure Nothing
          Just (ConfirmationDepth cd) -> do
            currentHeight <- qry_ db currentHeightQ [RInt]
            case currentHeight of
              [[SInt bh]] -> pure $ Just (bh - fromIntegral cd)
              _ -> fail "impossible"

        let
          blockheightval = maybe [] (\bh -> [SInt bh]) blockheight
          qvals = [ SBlob (BS.fromShort hash) | (TypedHash hash) <- V.toList hashes ] ++ blockheightval

        qry db qtext qvals [RInt, RBlob, RBlob] >>= mapM go
      return $ HashMap.fromList (map (\(T3 blockheight blockhash txhash) -> (txhash, T2 blockheight blockhash)) r)
  where
    qtext = "SELECT blockheight, hash, txhash FROM \
            \TransactionIndex INNER JOIN BlockHistory \
            \USING (blockheight) WHERE txhash IN (" <> hashesParams <> ")"
            <> maybe "" (const " AND blockheight <= ?") confDepth
            <> ";"
    hashesParams = Utf8 $ intercalate "," [ "?" | _ <- V.toList hashes]

    go :: [SType] -> IO (T3 BlockHeight BlockHash PactHash)
    go (SInt blockheight:SBlob blockhash:SBlob txhash:_) = do
        !blockhash' <- either fail return $ runGetEitherS decodeBlockHash blockhash
        let !txhash' = TypedHash $ BS.toShort txhash
        return $! T3 (fromIntegral blockheight) blockhash' txhash'
    go _ = fail "impossible"

doGetBlockHistory :: Db logger -> BlockHeader -> Domain RowKey RowData -> IO BlockTxHistory
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
    v = _chainwebVersion blockHeader
    cid = _blockChainId blockHeader
    bHeight = _blockHeight blockHeader

    procTxHist
      :: (S.Set Utf8, M.Map TxId [TxLog RowData])
      -> (Utf8,TxId,TxLog RowData)
      -> (S.Set Utf8,M.Map TxId [TxLog RowData])
    procTxHist (ks,r) (uk,t,l) = (S.insert uk ks, M.insertWith (++) t [l] r)

    -- Start index is inclusive, while ending index is not.
    -- `endingtxid` in a block is the beginning txid of the following block.
    queryHistory :: Database -> Utf8 -> Int64 -> Int64 -> IO [(Utf8,TxId,TxLog RowData)]
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
    queryPrev :: Database -> Utf8 -> Int64 -> Utf8 -> IO (Maybe (RowKey,TxLog RowData))
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
    :: Db logger
    -> BlockHeader
    -> Domain RowKey RowData
    -> RowKey
    -> IO (Maybe (TxLog RowData))
doGetHistoricalLookup dbenv blockHeader d k = runBlockEnv dbenv $ do
  callDb "doGetHistoricalLookup" $ \db -> do
    endTxId <- getEndTxId db bHeight (_blockHash blockHeader)
    latestEntry <- queryHistoryLookup db (domainTableName d) endTxId (convRowKey k)
    return $! latestEntry
  where
    bHeight = _blockHeight blockHeader

    queryHistoryLookup :: Database -> Utf8 -> Int64 -> Utf8 -> IO (Maybe (TxLog RowData))
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
