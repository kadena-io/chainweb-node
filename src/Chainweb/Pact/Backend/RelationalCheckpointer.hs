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
  , withProdRelationalCheckpointer
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.ByteString (intercalate)
import qualified Data.ByteString.Short as BS
import Data.Foldable (foldl')
import Data.Int
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
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
          { _cpRestoreAndSave = doRestoreAndSave v cid db
          , _cpReadCp = ReadCheckpointer
              { _cpReadFrom = doReadFrom loggr v cid db
              , _cpGetBlockHistory = doGetBlockHistory db
              , _cpGetHistoricalLookup = doGetHistoricalLookup db
              , _cpGetEarliestBlock = doGetEarliestBlock db
              , _cpGetLatestBlock = doGetLatestBlock db
              , _cpLookupBlockInCheckpointer = doLookupBlock db
              , _cpGetBlockParent = doGetBlockParent v cid db
              , _cpLogger = loggr
              }
          }
    return (blockDbEnv, checkpointer)


type Db logger = MVar (BlockEnv logger SQLiteEnv)

-- see the docs for _cpReadFrom
doReadFrom
  :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> Db logger
  -> Maybe ParentHeader
  -> (CurrentBlockDbEnv logger -> IO a)
  -> IO a
doReadFrom logger v cid db parent doRead = mask $ \resetMask -> do
  let currentHeight = maybe (genesisHeight v cid) (succ . _blockHeight . _parentHeader) parent
  sharedDbEnv <- readMVar db
  let sharedModuleCache = _bsModuleCache $ _benvBlockState sharedDbEnv
  let sql = _bdbenvDb $ _benvDb sharedDbEnv
  newDbEnv <- newMVar $ BlockEnv
    (BlockDbEnv sql logger)
    (initBlockState defaultModuleCacheLimit currentHeight)
      { _bsModuleCache = sharedModuleCache }
  runBlockEnv newDbEnv $ beginSavepoint BatchSavepoint
  let abort = runBlockEnv newDbEnv (abortSavepoint BatchSavepoint)
  flip finally abort $ do
    -- NB it's important to do this *after* you start the savepoint (and thus
    -- the db transaction) to make sure that the latestHeader check is up to date.
    latestHeader <- doGetLatestBlock newDbEnv
    let
      -- is the parent the latest header, i.e., can we get away without rewinding?
      parentIsLatestHeader = case (latestHeader, parent) of
        (Nothing, Nothing) -> True
        (Just (_, latestHash), Just (ParentHeader ph)) ->
          _blockHash ph == latestHash
        _ -> False

    txid <- runBlockEnv newDbEnv $ do
      prepareToPlayBlock "doReadFrom" v cid parent
      use bsTxId
    let
      pactDb
        | parentIsLatestHeader = chainwebPactDb
        | otherwise = rewoundPactDb currentHeight txid
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
-- see the docs for _cpRestoreAndSave.
doRestoreAndSave
  :: forall logger r q.
  (Logger logger, Monoid q, HasCallStack)
  => ChainwebVersion
  -> ChainId
  -> Db logger
  -> Maybe ParentHeader
  -> Stream (Of (RunnableBlock logger q)) IO r
  -> IO (r, q)
doRestoreAndSave v cid dbenv parent blocks = mask $ \resetMask -> do
    runBlockEnv dbenv $ do
        clearPendingTxState
        beginSavepoint BatchSavepoint
    let
        abort = runBlockEnv dbenv (abortSavepoint BatchSavepoint)
        rewindAndExtend = resetMask $ do
            runBlockEnv dbenv (rewindDbTo parent)
            extend
    ((q, _) :> r) <- rewindAndExtend `onException` abort
    runBlockEnv dbenv $ commitSavepoint BatchSavepoint
    return (r, q)
  where

    extend :: IO (Of (q, Maybe ParentHeader) r)
    extend = Streaming.foldM
      (\(m, pc) block -> do
        let
          !bh = maybe (genesisHeight v cid) (succ . _blockHeight . _parentHeader) pc
          curBlockDbEnv = CurrentBlockDbEnv
            { _cpPactDbEnv = PactDbEnv chainwebPactDb dbenv
            , _cpRegisterProcessedTx =
              \(TypedHash hash) -> runBlockEnv dbenv (indexPactTransaction $ BS.fromShort hash)
            , _cpLookupProcessedTx = \hs -> runBlockEnv dbenv $ doLookupSuccessful bh hs
            }
        -- prepare the block state
        runBlockEnv dbenv $
          prepareToPlayBlock "doRestoreAndSave" v cid pc
        -- execute the block
        (m', newBh) <- runBlock block curBlockDbEnv pc
        -- compute the accumulator early
        let !m'' = m <> m'
        -- check that the new parent header has the right height for a child
        -- of the previous block
        case pc of
          Nothing
            | genesisHeight v cid /= _blockHeight newBh -> throwM $ PactInternalError
              "doRestoreAndSave: block with no parent, genesis block, should have genesis height but doesn't,"
          Just (ParentHeader ph)
            | succ (_blockHeight ph) /= _blockHeight newBh -> throwM $ PactInternalError $
              "doRestoreAndSave: non-genesis block should be one higher than its parent. parent at "
                <> sshow (_blockHeight ph) <> ", child height " <> sshow (_blockHeight newBh)
          _ -> return ()
        -- persist any changes to the database
        runBlockEnv dbenv $
          commitBlockStateToDatabase (_blockHash newBh)
        return (m'', Just (ParentHeader newBh))
      ) (return (mempty, parent)) return blocks

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
    withSavepoint DbTransaction $
      fmap buildResultMap $ -- swizzle results of query into a HashMap
      callDb "doLookupSuccessful" $ \db -> do
        let
          hss = V.toList hashes
          params = Utf8 $ intercalate "," (map (const "?") hss)
          qtext = "SELECT blockheight, hash, txhash FROM \
                  \TransactionIndex INNER JOIN BlockHistory \
                  \USING (blockheight) WHERE txhash IN (" <> params <> ")"
                  <> " AND blockheight <= ?;"
          qvals
            -- match query params above. first, hashes
            = map (\(TypedHash h) -> SBlob $ BS.fromShort h) hss
            -- then, the block height; we don't want to see txs from the
            -- current block in the db, because they'd show up in pending data
            ++ [SInt $ fromIntegral (pred curHeight)]

        qry db qtext qvals [RInt, RBlob, RBlob] >>= mapM go
  where
    -- NOTE: it's useful to keep the types of 'go' and 'buildResultMap' in sync
    -- for readability but also to ensure the compiler and reader infer the
    -- right result types from the db query.

    buildResultMap :: [T3 PactHash BlockHeight BlockHash] -> HashMap.HashMap PactHash (T2 BlockHeight BlockHash)
    buildResultMap xs = HashMap.fromList $
      map (\(T3 txhash blockheight blockhash) -> (txhash, T2 blockheight blockhash)) xs

    go :: [SType] -> IO (T3 PactHash BlockHeight BlockHash)
    go (SInt blockheight:SBlob blockhash:SBlob txhash:_) = do
        !blockhash' <- either fail return $ runGetEitherS decodeBlockHash blockhash
        let !txhash' = TypedHash $ BS.toShort txhash
        return $! T3 txhash' (fromIntegral blockheight) blockhash'
    go _ = fail "impossible"

doGetBlockHistory :: Db logger -> BlockHeader -> Domain RowKey RowData -> IO BlockTxHistory
doGetBlockHistory dbenv blockHeader d = runBlockEnv dbenv $ do
  endTxId <- fmap fromIntegral $
    getEndTxId "doGetBlockHistory" (Just $ ParentHeader blockHeader)
  startTxId <- fmap fromIntegral $
    if bHeight == genesisHeight v cid
    then return 0
    else getEndTxId' "doGetBlockHistory" (pred bHeight) (_blockParent blockHeader)
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
  endTxId <- fromIntegral <$> getEndTxId "doGetHistoricalLookup" (Just $ ParentHeader blockHeader)
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
