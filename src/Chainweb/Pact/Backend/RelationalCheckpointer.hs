{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Lens (view)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.ByteString.Short as BS
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif
import Data.Int
import Data.Coerce
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Stack (HasCallStack)

import Database.SQLite3.Direct

import Prelude hiding (log)
import Streaming
import qualified Streaming.Prelude as Streaming

import System.LogLevel

-- pact

import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Command(RequestKey(..))
import Pact.Types.Hash (Hash(..))
import Pact.Types.Persistence
import Pact.Types.SQLite
import Pact.Types.Util (AsString(..))

import qualified Pact.Core.Persistence as Pact5

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import qualified Chainweb.Pact4.Backend.ChainwebPactDb as PactDb
import qualified Chainweb.Pact5.Backend.ChainwebPactDb as Pact5

import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Types
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Guards
import qualified Pact.Types.Persistence as Pact4
import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Evaluate as Pact5
import qualified Pact.Core.Names as Pact5

initRelationalCheckpointer
    :: (Logger logger)
    => DbCacheLimitBytes
    -> SQLiteEnv
    -> IntraBlockPersistence
    -> logger
    -> ChainwebVersion
    -> ChainId
    -> IO (Checkpointer logger)
initRelationalCheckpointer dbCacheLimit sqlenv p loggr v cid =
    snd <$!> initRelationalCheckpointer' dbCacheLimit sqlenv p loggr v cid

withProdRelationalCheckpointer
    :: (Logger logger)
    => logger
    -> DbCacheLimitBytes
    -> SQLiteEnv
    -> IntraBlockPersistence
    -> ChainwebVersion
    -> ChainId
    -> (Checkpointer logger -> IO a)
    -> IO a
withProdRelationalCheckpointer logger dbCacheLimit sqlenv p v cid inner = do
    (moduleCacheVar, cp) <- initRelationalCheckpointer' dbCacheLimit sqlenv p logger v cid
    withAsync (logModuleCacheStats moduleCacheVar) $ \_ -> inner cp
  where
    logFun = logFunctionText logger
    logModuleCacheStats e = runForever logFun "ModuleCacheStats" $ do
        stats <- modifyMVar e $ \db -> do
            let (s, !mc') = updateCacheStats db
            return (mc', s)
        logFunctionJson logger Info stats
        threadDelay 60_000_000 {- 1 minute -}

-- for testing
initRelationalCheckpointer'
    :: (Logger logger)
    => DbCacheLimitBytes
    -> SQLiteEnv
    -> IntraBlockPersistence
    -> logger
    -> ChainwebVersion
    -> ChainId
    -> IO (MVar (DbCache PersistModuleData), Checkpointer logger)
initRelationalCheckpointer' dbCacheLimit sqlenv p loggr v cid = do
    PactDb.initSchema loggr sqlenv
    moduleCacheVar <- newMVar (emptyDbCache dbCacheLimit)
    let
        checkpointer = Checkpointer
            { _cpRestoreAndSave = doRestoreAndSave loggr v cid sqlenv p moduleCacheVar
            , _cpReadCp = ReadCheckpointer
                { _cpReadFrom = doReadFrom loggr v cid sqlenv moduleCacheVar
                , _cpGetBlockHistory = doGetBlockHistory sqlenv
                , _cpGetHistoricalLookup = doGetHistoricalLookup sqlenv
                , _cpGetEarliestBlock = doGetEarliestBlock sqlenv
                , _cpGetLatestBlock = doGetLatestBlock sqlenv
                , _cpLookupBlockInCheckpointer = doLookupBlock sqlenv
                , _cpGetBlockParent = doGetBlockParent v cid sqlenv
                , _cpLogger = loggr
                }
            }
    return (moduleCacheVar, checkpointer)

-- see the docs for _cpReadFrom
doReadFrom
  :: forall logger pv a
  . (Logger logger)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> SQLiteEnv
  -> MVar (DbCache PersistModuleData)
  -> Maybe ParentHeader
  -> PactVersionT pv
  -> (PactDbFor logger pv -> BlockHandle -> IO a)
  -> IO (Historical a)
doReadFrom logger v cid sql moduleCacheVar maybeParent pactVersion doRead = do
  let currentHeight = case maybeParent of
        Nothing -> genesisHeight v cid
        Just parent -> succ . view blockHeight . _parentHeader $ parent

  modifyMVar moduleCacheVar $ \sharedModuleCache -> do
    bracket
      (beginSavepoint sql BatchSavepoint)
      (\_ -> abortSavepoint sql BatchSavepoint) \() -> do
      -- NB it's important to do this *after* you start the savepoint (and thus
      -- the db transaction) to make sure that the latestHeader check is up to date.
      latestHeader <- doGetLatestBlock sql
      h <- case pactVersion of
        Pact4T
          | pact5 v cid currentHeight -> internalError $
            "Pact 4 readFrom executed on block height after Pact 5 fork, height: " <> sshow currentHeight
          | otherwise -> PactDb.getEndTxId "doReadFrom" sql maybeParent >>= traverse \startTxId -> do
            newDbEnv <- newMVar $ PactDb.BlockEnv
              (PactDb.mkBlockHandlerEnv v cid currentHeight sql DoNotPersistIntraBlockWrites logger)
              (PactDb.initBlockState defaultModuleCacheLimit startTxId)
                { PactDb._bsModuleCache = sharedModuleCache }
            let
              -- is the parent the latest header, i.e., can we get away without rewinding?
              parentIsLatestHeader = case (latestHeader, maybeParent) of
                (Nothing, Nothing) -> True
                (Just (_, latestHash), Just (ParentHeader ph)) ->
                  view blockHash ph == latestHash
                _ -> False
              mkBlockDbEnv db = PactDb.CurrentBlockDbEnv
                { PactDb._cpPactDbEnv = PactDbEnv db newDbEnv
                , PactDb._cpRegisterProcessedTx = \hash ->
                  PactDb.runBlockEnv newDbEnv (PactDb.indexPactTransaction $ BS.fromShort $ coerce hash)
                , PactDb._cpLookupProcessedTx = \hs ->
                    HashMap.mapKeys coerce <$> doLookupSuccessful sql currentHeight (coerce hs)
                }
              pactDb
                | parentIsLatestHeader = PactDb.chainwebPactDb
                | otherwise = PactDb.rewoundPactDb currentHeight startTxId
            r <- doRead (mkBlockDbEnv pactDb) (emptyBlockHandle startTxId)
            finalCache <- PactDb._bsModuleCache . PactDb._benvBlockState <$> readMVar newDbEnv
            return (r, finalCache)

        Pact5T
          | pact5 v cid currentHeight ->
            Pact5.getEndTxId "doReadFrom" sql maybeParent >>= traverse \startTxId -> do
            let
              -- is the parent the latest header, i.e., can we get away without rewinding?
              -- TODO: just do this inside of the chainwebPactCoreBlockDb function?
              parentIsLatestHeader = case (latestHeader, maybeParent) of
                (Nothing, Nothing) -> True
                (Just (_, latestHash), Just (ParentHeader ph)) ->
                  view blockHash ph == latestHash
                _ -> False
              blockHandlerEnv = Pact5.BlockHandlerEnv
                { Pact5._blockHandlerDb = sql
                , Pact5._blockHandlerLogger = logger
                , Pact5._blockHandlerVersion = v
                , Pact5._blockHandlerChainId = cid
                , Pact5._blockHandlerBlockHeight = currentHeight
                , Pact5._blockHandlerMode = Pact5.Transactional
                , Pact5._blockHandlerPersistIntraBlockWrites = DoNotPersistIntraBlockWrites
                }
            let upperBound
                  | parentIsLatestHeader = Nothing
                  | otherwise = Just (currentHeight, startTxId)
            let pactDb
                  = Pact5.chainwebPactCoreBlockDb upperBound blockHandlerEnv
            r <- doRead pactDb (emptyBlockHandle (coerce @Pact5.TxId @Pact4.TxId startTxId))
            return (r, sharedModuleCache)
          | otherwise ->
            internalError $
              "Pact 5 readFrom executed on block height before Pact 5 fork, height: " <> sshow currentHeight
      case h of
        NoHistory -> return (sharedModuleCache, NoHistory)
        Historical (r, finalCache) -> return (finalCache, Historical r)



-- TODO: log more?
-- see the docs for _cpRestoreAndSave.
doRestoreAndSave
  :: forall logger r q.
  (Logger logger, Monoid q, HasCallStack)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> SQLiteEnv
  -> IntraBlockPersistence
  -> MVar (DbCache PersistModuleData)
  -> Maybe ParentHeader
  -> Stream (Of (RunnableBlock logger q)) IO r
  -> IO (r, q)
doRestoreAndSave logger v cid sql p moduleCacheVar rewindParent blocks = do
    modifyMVar moduleCacheVar $ \moduleCache -> do
      fmap fst $ generalBracket
        (beginSavepoint sql BatchSavepoint)
        (\_ -> \case
          ExitCaseSuccess {} -> commitSavepoint sql BatchSavepoint
          _ -> abortSavepoint sql BatchSavepoint
        ) $ \_ -> do
          startTxId <- PactDb.rewindDbTo sql rewindParent
          ((q, _, _, finalModuleCache) :> r) <- extend startTxId moduleCache
          return (finalModuleCache, (r, q))
  where

    extend
      :: TxId -> DbCache PersistModuleData
      -> IO (Of (q, Maybe ParentHeader, TxId, DbCache PersistModuleData) r)
    extend startTxId startModuleCache = Streaming.foldM
      (\(m, maybeParent, txid, moduleCache) block -> do
        let
          !bh = case maybeParent of
            Nothing -> genesisHeight v cid
            Just parent -> (succ . view blockHeight . _parentHeader) parent
        case block of
          Pact4RunnableBlock runBlock
            | pact5 v cid bh ->
              internalError $
                "Pact 4 block executed on block height after Pact 5 fork, height: " <> sshow bh
            | otherwise -> do
              -- prepare the block state
              let handlerEnv = PactDb.mkBlockHandlerEnv v cid bh sql p logger
              let state = (PactDb.initBlockState defaultModuleCacheLimit txid)
                    { PactDb._bsModuleCache = moduleCache }
              dbMVar <- newMVar PactDb.BlockEnv
                { PactDb._blockHandlerEnv = handlerEnv
                , PactDb._benvBlockState = state
                }

              let
                mkBlockDbEnv db = PactDb.CurrentBlockDbEnv
                  { PactDb._cpPactDbEnv = db
                  , PactDb._cpRegisterProcessedTx = \hash ->
                    PactDb.runBlockEnv dbMVar (PactDb.indexPactTransaction $ BS.fromShort $ coerce hash)
                  , PactDb._cpLookupProcessedTx = \hs ->
                      fmap (HashMap.mapKeys coerce) $
                      doLookupSuccessful sql bh $
                      coerce hs
                  }

              -- execute the block
              let pact4Db = PactDbEnv PactDb.chainwebPactDb dbMVar
              (m', newBh) <- runBlock (mkBlockDbEnv pact4Db) maybeParent


              -- grab any resulting state that we're interested in keeping
              nextState <- PactDb._benvBlockState <$> takeMVar dbMVar
              let !nextTxId = PactDb._bsTxId nextState
              let !nextModuleCache = PactDb._bsModuleCache nextState
              when (isJust (PactDb._bsPendingTx nextState)) $
                internalError "tx still in progress at the end of block"
              -- compute the accumulator early
              let !m'' = m <> m'
              -- check that the new parent header has the right height for a child
              -- of the previous block
              case maybeParent of
                Nothing
                  | genesisHeight v cid /= view blockHeight newBh -> internalError
                    "doRestoreAndSave: block with no parent, genesis block, should have genesis height but doesn't,"
                Just (ParentHeader ph)
                  | succ (view blockHeight ph) /= view blockHeight newBh -> internalError $
                    "doRestoreAndSave: non-genesis block should be one higher than its parent. parent at "
                      <> sshow (view blockHeight ph) <> ", child height " <> sshow (view blockHeight newBh)
                _ -> return ()
              -- persist any changes to the database
              PactDb.commitBlockStateToDatabase sql
                (view blockHash newBh) (view blockHeight newBh)
                (BlockHandle (PactDb._bsTxId nextState) (PactDb._bsPendingBlock nextState))
              return (m'', Just (ParentHeader newBh), nextTxId, nextModuleCache)
          Pact5RunnableBlock runBlock
            | pact5 v cid bh -> do
              let
                blockEnv = Pact5.BlockHandlerEnv
                  { Pact5._blockHandlerDb = sql
                  , Pact5._blockHandlerLogger = logger
                  , Pact5._blockHandlerVersion = v
                  , Pact5._blockHandlerBlockHeight = bh
                  , Pact5._blockHandlerChainId = cid
                  , Pact5._blockHandlerMode = Pact5.Transactional
                  , Pact5._blockHandlerPersistIntraBlockWrites = p
                  }
                pactDb = Pact5.chainwebPactCoreBlockDb Nothing blockEnv
              -- run the block
              ((m', nextBlockHeader), blockHandle) <- runBlock pactDb maybeParent (emptyBlockHandle txid)
              -- compute the accumulator early
              let !m'' = m <> m'
              case maybeParent of
                Nothing
                  | genesisHeight v cid /= view blockHeight nextBlockHeader -> internalError
                    "doRestoreAndSave: block with no parent, genesis block, should have genesis height but doesn't,"
                Just (ParentHeader ph)
                  | succ (view blockHeight ph) /= view blockHeight nextBlockHeader -> internalError $
                    "doRestoreAndSave: non-genesis block should be one higher than its parent. parent at "
                      <> sshow (view blockHeight ph) <> ", child height " <> sshow (view blockHeight nextBlockHeader)
                _ -> return ()
              PactDb.commitBlockStateToDatabase sql
                (view blockHash nextBlockHeader) (view blockHeight nextBlockHeader)
                blockHandle

              return (m'', Just (ParentHeader nextBlockHeader), _blockHandleTxId blockHandle, moduleCache)

            | otherwise -> internalError $
                "Pact 5 block executed on block height before Pact 5 fork, height: " <> sshow bh
      )
      (return (mempty, rewindParent, startTxId, startModuleCache))
      return
      blocks

doGetEarliestBlock :: HasCallStack => SQLiteEnv -> IO (Maybe (BlockHeight, BlockHash))
doGetEarliestBlock db = do
  r <- qry_ db qtext [RInt, RBlob] >>= mapM go
  case r of
    [] -> return Nothing
    (!o:_) -> return (Just o)
  where
    qtext = "SELECT blockheight, hash FROM BlockHistory ORDER BY blockheight ASC LIMIT 1"

    go [SInt hgt, SBlob blob] =
        let hash = either error id $ runGetEitherS decodeBlockHash blob
        in return (fromIntegral hgt, hash)
    go _ = fail "Chainweb.Pact.Backend.RelationalCheckpointer.doGetEarliest: impossible. This is a bug in chainweb-node."

doGetLatestBlock :: HasCallStack => SQLiteEnv -> IO (Maybe (BlockHeight, BlockHash))
doGetLatestBlock db = do
  r <- qry_ db qtext [RInt, RBlob] >>= mapM go
  case r of
    [] -> return Nothing
    (!o:_) -> return (Just o)
  where
    qtext = "SELECT blockheight, hash FROM BlockHistory ORDER BY blockheight DESC LIMIT 1"

    go [SInt hgt, SBlob blob] =
        let hash = either error id $ runGetEitherS decodeBlockHash blob
        in return (fromIntegral hgt, hash)
    go _ = fail "Chainweb.Pact.Backend.RelationalCheckpointer.doGetLatest: impossible. This is a bug in chainweb-node."

doLookupBlock :: SQLiteEnv -> (BlockHeight, BlockHash) -> IO Bool
doLookupBlock db (bheight, bhash) = do
    r <- qry db qtext [SInt $ fromIntegral bheight, SBlob (runPutS (encodeBlockHash bhash))]
                      [RInt]
    liftIO (expectSingle "row" r) >>= \case
        [SInt n] -> return $! n == 1
        _ -> internalError "doLookupBlock: output type mismatch"
  where
    qtext = "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"

doGetBlockParent :: ChainwebVersion -> ChainId -> SQLiteEnv -> (BlockHeight, BlockHash) -> IO (Maybe BlockHash)
doGetBlockParent v cid db (bh, hash)
    | bh == genesisHeight v cid = return Nothing
    | otherwise = do
        blockFound <- doLookupBlock db (bh, hash)
        if not blockFound
          then return Nothing
          else do
            r <- qry db qtext [SInt (fromIntegral (pred bh))] [RBlob]
            case r of
              [[SBlob blob]] ->
                either (internalError . T.pack) (return . return) $! runGetEitherS decodeBlockHash blob
              [] -> internalError "doGetBlockParent: block was found but its parent couldn't be found"
              _ -> error "doGetBlockParent: output type mismatch"
  where
    qtext = "SELECT hash FROM BlockHistory WHERE blockheight = ?"


doGetBlockHistory
  :: SQLiteEnv
  -> BlockHeader
  -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
  -> IO (Historical BlockTxHistory)
doGetBlockHistory db blockHeader d = do
  historicalEndTxId <-
      fmap fromIntegral
      <$> PactDb.getEndTxId "doGetBlockHistory" db (Just $ ParentHeader blockHeader)
  forM historicalEndTxId $ \endTxId -> do
    startTxId <-
      if bHeight == genesisHeight v cid
      then return 0
      else PactDb.getEndTxId' "doGetBlockHistory" db (pred bHeight) (view blockParent blockHeader) >>= \case
        NoHistory ->
          internalError $ "doGetBlockHistory: missing parent for: " <> sshow blockHeader
        Historical startTxId ->
          return $ fromIntegral startTxId

    let tname = domainTableNameCore d
    history <- queryHistory tname startTxId endTxId
    let (!hkeys,tmap) = foldl' procTxHist (S.empty,mempty) history
    !prev <- M.fromList . catMaybes <$> mapM (queryPrev tname startTxId) (S.toList hkeys)
    return $ BlockTxHistory tmap prev
  where
    v = _chainwebVersion blockHeader
    cid = view blockChainId blockHeader
    bHeight = view blockHeight blockHeader

    procTxHist
      :: (S.Set Utf8, M.Map TxId [Pact5.TxLog Pact5.RowData])
      -> (Utf8,TxId,Pact5.TxLog Pact5.RowData)
      -> (S.Set Utf8,M.Map TxId [Pact5.TxLog Pact5.RowData])
    procTxHist (ks,r) (uk,t,l) = (S.insert uk ks, M.insertWith (++) t [l] r)

    -- Start index is inclusive, while ending index is not.
    -- `endingtxid` in a block is the beginning txid of the following block.
    queryHistory :: Utf8 -> Int64 -> Int64 -> IO [(Utf8,TxId,Pact5.TxLog Pact5.RowData)]
    queryHistory tableName s e = do
      let sql = "SELECT txid, rowkey, rowdata FROM [" <> tableName <>
                "] WHERE txid >= ? AND txid < ?"
      r <- qry db sql
            [SInt s,SInt e]
            [RInt,RText,RBlob]
      forM r $ \case
        [SInt txid, SText key, SBlob value] -> (key,fromIntegral txid,) <$> Pact5.toTxLog (asString d) key value
        err -> internalError $
          "queryHistory: Expected single row with three columns as the \
          \result, got: " <> T.pack (show err)

    -- Get last tx data, if any, for key before start index.
    queryPrev :: Utf8 -> Int64 -> Utf8 -> IO (Maybe (RowKey,Pact5.TxLog Pact5.RowData))
    queryPrev tableName s k@(Utf8 sk) = do
      let sql = "SELECT rowdata FROM [" <> tableName <>
                "] WHERE rowkey = ? AND txid < ? " <>
                "ORDER BY txid DESC LIMIT 1"
      r <- qry db sql
        [SText k,SInt s]
        [RBlob]
      case r of
        [] -> return Nothing
        [[SBlob value]] -> Just . (RowKey $ T.decodeUtf8 sk,) <$> Pact5.toTxLog (asString d) k value
        _ -> internalError $ "queryPrev: expected 0 or 1 rows, got: " <> T.pack (show r)

doGetHistoricalLookup
    :: SQLiteEnv
    -> BlockHeader
    -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
    -> Pact5.RowKey
    -> IO (Historical (Maybe (Pact5.TxLog Pact5.RowData)))
doGetHistoricalLookup db blockHeader d k = do
  historicalEndTxId <-
    PactDb.getEndTxId "doGetHistoricalLookup" db (Just $ ParentHeader blockHeader)
  forM historicalEndTxId (queryHistoryLookup . fromIntegral)
  where
    queryHistoryLookup :: Int64 -> IO (Maybe (Pact5.TxLog Pact5.RowData))
    queryHistoryLookup e = do
      let sql = "SELECT rowKey, rowdata FROM [" <> domainTableNameCore d <>
                "] WHERE txid < ? AND rowkey = ? ORDER BY txid DESC LIMIT 1;"
      r <- qry db sql
        [SInt e, SText (convRowKeyCore k)]
        [RText, RBlob]
      case r of
        [[SText key, SBlob value]] -> Just <$> Pact5.toTxLog (asString d) key value
        [] -> pure Nothing
        _ -> internalError $ "doGetHistoricalLookup: expected single-row result, got " <> sshow r
