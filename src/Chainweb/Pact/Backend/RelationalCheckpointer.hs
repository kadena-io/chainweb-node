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
          , _cpLookupProcessedTx = \hs -> runBlockEnv db $ doLookupSuccessful (_bsBlockHeight bstate) hs
          }
      checkpointer = Checkpointer
          { _cpRewindAndExtend = doRewindAndExtend loggr v cid db
          , _cpReadCp = ReadCheckpointer
              { _cpReadFrom = doReadFrom loggr v cid db
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

prepareToPlayBlock
    :: T.Text
    -> ChainwebVersion
    -> ChainId
    -> ParentContext
    -> BlockHandler logger SQLiteEnv ()
prepareToPlayBlock msg v cid pc = do
    let currentHeight = _parentContextCurrentHeight pc
    assign bsBlockHeight currentHeight
    bsModuleNameFix .= enableModuleNameFix v cid currentHeight
    bsSortedKeys .= pact42 v cid currentHeight
    bsLowerCaseTables .= chainweb217Pact v cid currentHeight
    txid <- getEndTxId (msg <> ".prepareToPlayBlock") pc
    assign bsTxId txid
    clearPendingTxState

-- TODO: what does this actually do if I'm missing this header?
-- see the docs for _cpReadFrom
doReadFrom
  :: (Logger logger, MonadIO m, MonadMask m)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> Db logger
  -> ParentContext
  -> (CurrentBlockDbEnv logger -> m a)
  -> m a
doReadFrom logger v cid db parent doRead = mask $ \resetMask -> do
  let currentHeight = _parentContextCurrentHeight parent
  sharedDbEnv <- liftIO $ readMVar db
  let sharedModuleCache = _bsModuleCache $ _benvBlockState sharedDbEnv
  let sql = _bdbenvDb $ _benvDb sharedDbEnv
  -- TODO: grab the real module cache?
  newDbEnv <- liftIO $ newMVar $ BlockEnv
    (BlockDbEnv sql logger)
    (initBlockState defaultModuleCacheLimit currentHeight)
      { _bsModuleCache = sharedModuleCache }
  liftIO $ runBlockEnv newDbEnv $ beginSavepoint BatchSavepoint
  -- NB it's important to do this *after* you start the savepoint (and thus
  -- the db transaction) to make sure that the latestHeader check is up to date.
  latestHeader <- liftIO $ doGetLatestBlock newDbEnv
  let
    -- is the parent the latest header, i.e., can we get away without rewinding?
    parentIsLatestHeader = case (latestHeader, parent) of
      (Nothing, GenesisParentContext {}) -> True
      (Just (_, latestHash), BlockParentContext _ _ parentHash _ _) ->
        parentHash == latestHash
      _ -> False

  (`finally` liftIO (runBlockEnv newDbEnv (rollbackSavepoint BatchSavepoint >> commitSavepoint BatchSavepoint))) $ do
    txid <- liftIO $ runBlockEnv newDbEnv $ do
      prepareToPlayBlock "doReadFrom" v cid parent
      use bsTxId
    let
      pactDb =
        if parentIsLatestHeader
        then chainwebPactDb
        else rewoundPactDb currentHeight txid
      curBlockDbEnv = CurrentBlockDbEnv
        { _cpPactDbEnv = PactDbEnv pactDb newDbEnv
        , _cpRegisterProcessedTx =
          \(TypedHash hash) -> runBlockEnv newDbEnv (indexPactTransaction $ BS.fromShort hash)
        , _cpLookupProcessedTx = \hs ->
          -- TODO: test that we do not see transactions from the
          -- "current block" if we've rewound.
          runBlockEnv newDbEnv (doLookupSuccessful currentHeight hs)
        }
    resetMask (doRead curBlockDbEnv)


-- TODO: log more?
-- see the docs for _cpRewindAndExtend.
doRewindAndExtend
  :: forall logger m blk r q.
  (Logger logger, Monoid q, MonadIO m, MonadMask m, HasCallStack)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> Db logger
  -> ParentContext
  -> Stream (Of blk) m r
  -> (CurrentBlockDbEnv logger -> ParentContext -> blk -> m (q, ParentContext))
  -> m (r, q)
doRewindAndExtend loggr v cid dbenv parent blocks execBlock = mask $ \resetMask -> do
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
      GenesisParentContext {} -> rewindDbToGenesis
      BlockParentContext {} -> rewindDbToBlock v cid parent

    extend :: m (Of (q, ParentContext) r)
    extend = Streaming.foldM
      -- todo: check that each block header is actually a child of the previous.
      (\(m, pc) b -> do
        let
          -- !curPc = getParentContext b
          !bh = _parentContextCurrentHeight pc -- _parentContextHeight curPc
          curBlockDbEnv = CurrentBlockDbEnv
            { _cpPactDbEnv = PactDbEnv chainwebPactDb dbenv
            , _cpRegisterProcessedTx =
              \(TypedHash hash) -> runBlockEnv dbenv (indexPactTransaction $ BS.fromShort hash)
            , _cpLookupProcessedTx = \hs -> runBlockEnv dbenv $ doLookupSuccessful bh hs
            }
        -- TODO: if replacing ParentContext, fix this to work properly.
        -- prepare to execute the block, by setting up the block state.
        liftIO $ runBlockEnv dbenv $ do
          prepareToPlayBlock "doRewindAndExtend" v cid pc
          -- clearPendingTxState
          -- assign bsBlockHeight bh
          -- bsModuleNameFix .= enableModuleNameFix v cid bh
          -- bsSortedKeys .= pact42 v cid bh
          -- bsLowerCaseTables .= chainweb217Pact v cid bh
          -- !txid <- getEndTxId parent
          -- assign bsTxId (fromIntegral txid)
        -- execute the block
        (m', newPc) <- execBlock curBlockDbEnv pc b
        -- compute the accumulator early
        let !m'' = m <> m'
        -- check that the new ParentContext has the right height
        case (pc, newPc) of
          (_, GenesisParentContext {}) -> throwM $ PactInternalError
            "doRewindAndExtend: block reports that it is a pre-genesis block, and those don't exist."
          (GenesisParentContext {}, BlockParentContext {})
            | _parentContextHeight newPc /= _parentContextHeight pc -> throwM $ PactInternalError
            "doRewindAndExtend: genesis block should have same height as genesis parent."
          (BlockParentContext {}, BlockParentContext {})
            | _parentContextHeight newPc /= _parentContextCurrentHeight pc -> throwM $ PactInternalError $
            "doRewindAndExtend: non-genesis block should be one higher than its parent. parent at " <> sshow (_parentContextHeight pc) <> ", child height " <> sshow bh
          _ -> return ()
        -- persist the results to the database
        liftIO $ runBlockEnv dbenv $ do
          newTables <- use $ bsPendingBlock . pendingTableCreation
          createNewTables (_parentContextHeight newPc) $ toList newTables
          writes <- use $ bsPendingBlock . pendingWrites
          writeV <- toVectorChunks writes
          callDb "save" $ backendWriteUpdateBatch (_parentContextHeight newPc) writeV
          indexPendingPactTransactions
          nextTxId <- gets _bsTxId
          blockHistoryInsert (_parentContextHeight newPc) (_parentContextHash newPc) nextTxId
          clearPendingTxState
        return $ (m'', newPc)
      ) (return (mempty, parent)) return blocks

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

doLookupSuccessful :: BlockHeight -> V.Vector PactHash -> BlockHandler logger SQLiteEnv (HashMap.HashMap PactHash (T2 BlockHeight BlockHash))
doLookupSuccessful curHeight hashes = do
    withSavepoint DbTransaction $ do
      r <- callDb "doLookupSuccessful" $ \db -> do
        let
          blockheightval = [SInt $ fromIntegral curHeight - 1]
          qvals = [ SBlob (BS.fromShort hash) | (TypedHash hash) <- V.toList hashes ] ++ blockheightval

        qry db qtext qvals [RInt, RBlob, RBlob] >>= mapM go
      return $ HashMap.fromList (map (\(T3 blockheight blockhash txhash) -> (txhash, T2 blockheight blockhash)) r)
  where
    qtext = "SELECT blockheight, hash, txhash FROM \
            \TransactionIndex INNER JOIN BlockHistory \
            \USING (blockheight) WHERE txhash IN (" <> hashesParams <>
            ") AND blockheight <= ?"
    hashesParams = Utf8 $ intercalate "," [ "?" | _ <- V.toList hashes]

    go :: [SType] -> IO (T3 BlockHeight BlockHash PactHash)
    go (SInt blockheight:SBlob blockhash:SBlob txhash:_) = do
        !blockhash' <- either fail return $ runGetEitherS decodeBlockHash blockhash
        let !txhash' = TypedHash $ BS.toShort txhash
        return $! T3 (fromIntegral blockheight) blockhash' txhash'
    go _ = fail "impossible"

doGetBlockHistory :: Db logger -> BlockHeader -> Domain RowKey RowData -> IO BlockTxHistory
doGetBlockHistory dbenv blockHeader d = runBlockEnv dbenv $ do
  endTxId <- fmap fromIntegral $
    getEndTxId "doGetBlockHistory" (parentToParentContext $ ParentHeader blockHeader)
  startTxId <- fmap fromIntegral $ getEndTxId "doGetBlockHistory" $
    if bHeight == genesisHeight v cid
    then GenesisParentContext v cid
    -- we use the wrong blockCreationTime here, but it's not used by getEndTxId.
    else BlockParentContext v cid (_blockParent blockHeader) (pred (_blockHeight blockHeader)) (_blockCreationTime blockHeader)
  callDb "doGetBlockHistory" $ \db -> do
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


doGetHistoricalLookup
    :: Db logger
    -> BlockHeader
    -> Domain RowKey RowData
    -> RowKey
    -> IO (Maybe (TxLog RowData))
doGetHistoricalLookup dbenv blockHeader d k = runBlockEnv dbenv $ do
  endTxId <- fromIntegral <$> getEndTxId "doGetHistoricalLookup" (parentToParentContext $ ParentHeader blockHeader)
  callDb "doGetHistoricalLookup" $ \db -> do
    latestEntry <- queryHistoryLookup db (domainTableName d) endTxId (convRowKey k)
    return $! latestEntry
  where
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
