{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
module Chainweb.Pact.PactService.Checkpointer.Internal
  ( Checkpointer(..)
  , initCheckpointerResources
  , withCheckpointerResources
  , rewindTo
  , readFrom
  , restoreAndSave
  , lookupBlock
  , getEarliestBlock
  , getLatestBlock
  , getBlockHistory
  , getBlockParent
  , lookupHistorical
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

import qualified Pact.Core.Persistence as Pact5

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import qualified Chainweb.Pact4.Backend.ChainwebPactDb as Pact4
import qualified Chainweb.Pact5.Backend.ChainwebPactDb as Pact5

import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Types
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Guards
import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Evaluate as Pact5
import qualified Pact.Core.Names as Pact5
import qualified Chainweb.Pact.Backend.Utils as PactDb

withCheckpointerResources
    :: (Logger logger)
    => logger
    -> DbCacheLimitBytes
    -> SQLiteEnv
    -> IntraBlockPersistence
    -> ChainwebVersion
    -> ChainId
    -> (Checkpointer logger -> IO a)
    -> IO a
withCheckpointerResources logger dbCacheLimit sqlenv p v cid inner = do
    cp <- initCheckpointerResources dbCacheLimit sqlenv p logger v cid
    withAsync (logModuleCacheStats (cpModuleCacheVar cp)) $ \_ -> inner cp
    where
    logFun = logFunctionText logger
    logModuleCacheStats e = runForever logFun "ModuleCacheStats" $ do
        stats <- modifyMVar e $ \db -> do
            let (s, !mc') = updateCacheStats db
            return (mc', s)
        logFunctionJson logger Info stats
        threadDelay 60_000_000 {- 1 minute -}

initCheckpointerResources
    :: (Logger logger)
    => DbCacheLimitBytes
    -> SQLiteEnv
    -> IntraBlockPersistence
    -> logger
    -> ChainwebVersion
    -> ChainId
    -> IO (Checkpointer logger)
initCheckpointerResources dbCacheLimit sql p loggr v cid = do
    Pact5.initSchema sql
    moduleCacheVar <- newMVar (emptyDbCache dbCacheLimit)
    return Checkpointer
        { cpLogger = loggr
        , cpCwVersion = v
        , cpChainId = cid
        , cpSql = sql
        , cpIntraBlockPersistence = p
        , cpModuleCacheVar = moduleCacheVar
        }

-- | Rewind to a particular block *in-memory*, producing a read-write snapshot
-- of the database at that block to compute some value, after which the snapshot
-- is discarded and nothing is saved to the database.
--
-- prerequisite: ParentHeader is an ancestor of the "latest block";
-- if that isn't the case, NoHistory is returned.
readFrom
  :: forall logger pv a
  . (Logger logger)
  => Checkpointer logger
  -> Maybe ParentHeader
  -> PactVersionT pv
  -> (PactDbFor logger pv -> BlockHandle pv -> IO a)
  -> IO (Historical a)
readFrom res maybeParent pactVersion doRead = do
  let currentHeight = case maybeParent of
        Nothing -> genesisHeight res.cpCwVersion res.cpChainId
        Just parent -> succ . view blockHeight . _parentHeader $ parent

  modifyMVar res.cpModuleCacheVar $ \sharedModuleCache -> do
    bracket
      (beginSavepoint res.cpSql BatchSavepoint)
      (\_ -> abortSavepoint res.cpSql BatchSavepoint) \() -> do
      -- NB it's important to do this *after* you start the savepoint (and thus
      -- the db transaction) to make sure that the latestHeader check is up to date.
      latestHeader <- getLatestBlock res.cpSql
      h <- case pactVersion of
        Pact4T
          | pact5 res.cpCwVersion res.cpChainId currentHeight -> internalError $
            "Pact 4 readFrom executed on block height after Pact 5 fork, height: " <> sshow currentHeight
          | otherwise -> PactDb.getEndTxId "doReadFrom" res.cpSql maybeParent >>= traverse \startTxId -> do
            newDbEnv <- newMVar $ Pact4.BlockEnv
              (Pact4.mkBlockHandlerEnv res.cpCwVersion res.cpChainId currentHeight res.cpSql DoNotPersistIntraBlockWrites res.cpLogger)
              (Pact4.initBlockState defaultModuleCacheLimit startTxId)
                { Pact4._bsModuleCache = sharedModuleCache }
            let
              -- is the parent the latest header, i.e., can we get away without rewinding?
              parentIsLatestHeader = case (latestHeader, maybeParent) of
                (Nothing, Nothing) -> True
                (Just (_, latestHash), Just (ParentHeader ph)) ->
                  view blockHash ph == latestHash
                _ -> False
              mkBlockDbEnv db = Pact4.CurrentBlockDbEnv
                { Pact4._cpPactDbEnv = PactDbEnv db newDbEnv
                , Pact4._cpRegisterProcessedTx = \hash ->
                  Pact4.runBlockEnv newDbEnv (Pact4.indexPactTransaction $ BS.fromShort $ coerce hash)
                , Pact4._cpLookupProcessedTx = \hs ->
                    HashMap.mapKeys coerce <$> doLookupSuccessful res.cpSql currentHeight (coerce hs)
                }
              pactDb
                | parentIsLatestHeader = Pact4.chainwebPactDb
                | otherwise = Pact4.rewoundPactDb currentHeight startTxId
            r <- doRead (mkBlockDbEnv pactDb) (emptyPact4BlockHandle startTxId)
            finalCache <- Pact4._bsModuleCache . Pact4._benvBlockState <$> readMVar newDbEnv
            return (r, finalCache)

        Pact5T
          | pact5 res.cpCwVersion res.cpChainId currentHeight ->
            PactDb.getEndTxId "doReadFrom" res.cpSql maybeParent >>= traverse \startTxId -> do
            let
              -- is the parent the latest header, i.e., can we get away without rewinding?
              parentIsLatestHeader = case (latestHeader, maybeParent) of
                (Nothing, Nothing) -> True
                (Just (_, latestHash), Just (ParentHeader ph)) ->
                  view blockHash ph == latestHash
                _ -> False
              blockHandlerEnv = Pact5.BlockHandlerEnv
                { Pact5._blockHandlerDb = res.cpSql
                , Pact5._blockHandlerLogger = res.cpLogger
                , Pact5._blockHandlerVersion = res.cpCwVersion
                , Pact5._blockHandlerChainId = res.cpChainId
                , Pact5._blockHandlerBlockHeight = currentHeight
                , Pact5._blockHandlerMode = Pact5.Transactional
                , Pact5._blockHandlerUpperBoundTxId = Pact5.TxId $ fromIntegral startTxId
                , Pact5._blockHandlerAtTip = parentIsLatestHeader
                }
            let pactDb = Pact5.chainwebPactBlockDb blockHandlerEnv
            r <- doRead pactDb (emptyPact5BlockHandle startTxId)
            return (r, sharedModuleCache)
          | otherwise ->
            internalError $
              "Pact 5 readFrom executed on block height before Pact 5 fork, height: " <> sshow currentHeight
      case h of
        NoHistory -> return (sharedModuleCache, NoHistory)
        Historical (r, finalCache) -> return (finalCache, Historical r)



-- the special case where one doesn't want to extend the chain, just rewind it.
rewindTo :: Logger logger => Checkpointer logger -> Maybe ParentHeader -> IO ()
rewindTo cp ancestor = void $ restoreAndSave cp
    ancestor
    (pure () :: Stream (Of (RunnableBlock logger ())) IO ())

-- TODO: log more?
-- | Rewind to a particular block, and play a stream of blocks afterward,
-- extending the chain and saving the result persistently. for example,
-- to validate a block `vb`, we rewind to the common ancestor of `vb` and
-- the latest block, and extend the chain with all of the blocks on `vb`'s
-- fork, including `vb`.
-- this function takes care of making sure that this is done *atomically*.
-- TODO: fix the below w.r.t. its latest type
-- promises:
--   - excluding the fact that each _cpRestoreAndSave call is atomic, the
--     following two expressions should be equivalent:
--     do
--       _cpRestoreAndSave cp p1 x
--         ((,) <$> (bs1 <* Stream.yield p2) <*> bs2) runBlk
--     do
--       (r1, q1) <- _cpRestoreAndSave cp p1 x (bs1 <* Stream.yield p2) runBlk
--       (r2, q2) <- _cpRestoreAndSave cp (Just (x p2)) x bs2 runBlk
--       return ((r1, r2), q1 <> q2)
--     i.e. rewinding, extending, then rewinding to the point you extended
--     to and extending some more, should give the same result as rewinding
--     once and extending to the same final point.
--   - no block in the stream is used more than once.
-- prerequisites:
--   - the parent being rewound to must be a direct ancestor
--     of the latest block, i.e. what's returned by _cpLatestBlock.
--   - the stream must start with a block that is a child of the rewind
--     target and each block after must be the child of the previous block.
restoreAndSave
  :: forall logger r q.
  (Logger logger, Monoid q, HasCallStack)
  => Checkpointer logger
  -> Maybe ParentHeader
  -> Stream (Of (RunnableBlock logger q)) IO r
  -> IO (r, q)
restoreAndSave res rewindParent blocks = do
    modifyMVar res.cpModuleCacheVar $ \moduleCache -> do
      fmap fst $ generalBracket
        (beginSavepoint res.cpSql BatchSavepoint)
        (\_ -> \case
          ExitCaseSuccess {} -> commitSavepoint res.cpSql BatchSavepoint
          _ -> abortSavepoint res.cpSql BatchSavepoint
        ) $ \_ -> do
          startTxId <- PactDb.rewindDbTo res.cpSql rewindParent
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
            Nothing -> genesisHeight res.cpCwVersion res.cpChainId
            Just parent -> (succ . view blockHeight . _parentHeader) parent
        case block of
          Pact4RunnableBlock runBlock
            | pact5 res.cpCwVersion res.cpChainId bh ->
              internalError $
                "Pact 4 block executed on block height after Pact 5 fork, height: " <> sshow bh
            | otherwise -> do
              -- prepare a fresh block state
              let handlerEnv = Pact4.mkBlockHandlerEnv res.cpCwVersion res.cpChainId bh res.cpSql res.cpIntraBlockPersistence res.cpLogger
              let state = (Pact4.initBlockState defaultModuleCacheLimit txid)
                    { Pact4._bsModuleCache = moduleCache }
              dbMVar <- newMVar Pact4.BlockEnv
                { Pact4._blockHandlerEnv = handlerEnv
                , Pact4._benvBlockState = state
                }

              let
                mkBlockDbEnv db = Pact4.CurrentBlockDbEnv
                  { Pact4._cpPactDbEnv = db
                  , Pact4._cpRegisterProcessedTx = \hash ->
                    Pact4.runBlockEnv dbMVar (Pact4.indexPactTransaction $ BS.fromShort $ coerce hash)
                  , Pact4._cpLookupProcessedTx = \hs ->
                      fmap (HashMap.mapKeys coerce) $
                      doLookupSuccessful res.cpSql bh $
                      coerce hs
                  }

              -- execute the block
              let pact4Db = PactDbEnv Pact4.chainwebPactDb dbMVar
              (m', newBh) <- runBlock (mkBlockDbEnv pact4Db) maybeParent

              -- grab any resulting state that we're interested in keeping
              nextState <- Pact4._benvBlockState <$> takeMVar dbMVar
              let !nextTxId = Pact4._bsTxId nextState
              let !nextModuleCache = Pact4._bsModuleCache nextState
              when (isJust (Pact4._bsPendingTx nextState)) $
                internalError "tx still in progress at the end of block"
              -- compute the accumulator early
              let !m'' = m <> m'
              -- check that the new parent header has the right height for a child
              -- of the previous block
              case maybeParent of
                Nothing
                  | genesisHeight res.cpCwVersion res.cpChainId /= view blockHeight newBh -> internalError
                    "doRestoreAndSave: block with no parent, genesis block, should have genesis height but doesn't,"
                Just (ParentHeader ph)
                  | succ (view blockHeight ph) /= view blockHeight newBh -> internalError $
                    "doRestoreAndSave: non-genesis block should be one higher than its parent. parent at "
                      <> sshow (view blockHeight ph) <> ", child height " <> sshow (view blockHeight newBh)
                _ -> return ()
              -- persist any changes to the database
              Pact4.commitBlockStateToDatabase res.cpSql
                (view blockHash newBh) (view blockHeight newBh)
                (BlockHandle (Pact4._bsTxId nextState) (Pact4._bsPendingBlock nextState))
              return (m'', Just (ParentHeader newBh), nextTxId, nextModuleCache)
          Pact5RunnableBlock runBlock
            | pact5 res.cpCwVersion res.cpChainId bh -> do
              let
                blockEnv = Pact5.BlockHandlerEnv
                  { Pact5._blockHandlerDb = res.cpSql
                  , Pact5._blockHandlerLogger = res.cpLogger
                  , Pact5._blockHandlerVersion = res.cpCwVersion
                  , Pact5._blockHandlerBlockHeight = bh
                  , Pact5._blockHandlerChainId = res.cpChainId
                  , Pact5._blockHandlerMode = Pact5.Transactional
                  , Pact5._blockHandlerUpperBoundTxId = Pact5.TxId $ fromIntegral txid
                  , Pact5._blockHandlerAtTip = True
                  }
                pactDb = Pact5.chainwebPactBlockDb blockEnv
              -- run the block
              ((m', nextBlockHeader), blockHandle) <- runBlock pactDb maybeParent (emptyPact5BlockHandle txid)
              -- compute the accumulator early
              let !m'' = m <> m'
              case maybeParent of
                Nothing
                  | genesisHeight res.cpCwVersion res.cpChainId /= view blockHeight nextBlockHeader -> internalError
                    "doRestoreAndSave: block with no parent, genesis block, should have genesis height but doesn't,"
                Just (ParentHeader ph)
                  | succ (view blockHeight ph) /= view blockHeight nextBlockHeader -> internalError $
                    "doRestoreAndSave: non-genesis block should be one higher than its parent. parent at "
                      <> sshow (view blockHeight ph) <> ", child height " <> sshow (view blockHeight nextBlockHeader)
                _ -> return ()
              Pact5.commitBlockStateToDatabase res.cpSql
                (view blockHash nextBlockHeader) (view blockHeight nextBlockHeader)
                blockHandle

              return (m'', Just (ParentHeader nextBlockHeader), _blockHandleTxId blockHandle, moduleCache)

            | otherwise -> internalError $
                "Pact 5 block executed on block height before Pact 5 fork, height: " <> sshow bh
      )
      (return (mempty, rewindParent, startTxId, startModuleCache))
      return
      blocks

-- | Get the checkpointer's idea of the earliest block. The block height
-- is the height of the block of the block hash.
getEarliestBlock :: HasCallStack => SQLiteEnv -> IO (Maybe (BlockHeight, BlockHash))
getEarliestBlock db = do
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

-- | Get the checkpointer's idea of the latest block. The block height is
-- is the height of the block of the block hash.
--
-- TODO: Under which circumstances does this return 'Nothing'?
getLatestBlock :: HasCallStack => SQLiteEnv -> IO (Maybe (BlockHeight, BlockHash))
getLatestBlock db = do
  r <- qry_ db qtext [RInt, RBlob] >>= mapM go
  case r of
    [] -> return Nothing
    (!o:_) -> return (Just o)
  where
    qtext = "SELECT blockheight, hash FROM BlockHistory ORDER BY blockheight DESC LIMIT 1"

    go [SInt hgt, SBlob blob] =
        let hash = either error id $ runGetEitherS decodeBlockHash blob
        in return (fromIntegral hgt, hash)
    go _ = fail "Chainweb.Pact.Backend.RelationalCheckpointer.getLatest: impossible. This is a bug in chainweb-node."

-- | Ask: is the checkpointer aware of the given block?
lookupBlock :: SQLiteEnv -> (BlockHeight, BlockHash) -> IO Bool
lookupBlock db (bheight, bhash) = do
    r <- qry db qtext [SInt $ fromIntegral bheight, SBlob (runPutS (encodeBlockHash bhash))]
                      [RInt]
    liftIO (expectSingle "row" r) >>= \case
        [SInt n] -> return $! n == 1
        _ -> internalError "lookupBlock: output type mismatch"
  where
    qtext = "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"

getBlockParent :: ChainwebVersion -> ChainId -> SQLiteEnv -> (BlockHeight, BlockHash) -> IO (Maybe BlockHash)
getBlockParent v cid db (bh, hash)
    | bh == genesisHeight v cid = return Nothing
    | otherwise = do
        blockFound <- lookupBlock db (bh, hash)
        if not blockFound
          then return Nothing
          else do
            r <- qry db qtext [SInt (fromIntegral (pred bh))] [RBlob]
            case r of
              [[SBlob blob]] ->
                either (internalError . T.pack) (return . return) $! runGetEitherS decodeBlockHash blob
              [] -> internalError "getBlockParent: block was found but its parent couldn't be found"
              _ -> error "getBlockParent: output type mismatch"
  where
    qtext = "SELECT hash FROM BlockHistory WHERE blockheight = ?"


-- TODO: do this in ChainwebPactDb instead?
getBlockHistory
  :: SQLiteEnv
  -> BlockHeader
  -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
  -> IO (Historical BlockTxHistory)
getBlockHistory db blockHeader d = do
  historicalEndTxId <-
      fmap fromIntegral
      <$> PactDb.getEndTxId "getBlockHistory" db (Just $ ParentHeader blockHeader)
  forM historicalEndTxId $ \endTxId -> do
    startTxId <-
      if bHeight == genesisHeight v cid
      then return 0
      else PactDb.getEndTxId' "getBlockHistory" db (pred bHeight) (view blockParent blockHeader) >>= \case
        NoHistory ->
          internalError $ "getBlockHistory: missing parent for: " <> sshow blockHeader
        Historical startTxId ->
          return $ fromIntegral startTxId

    let tname = Pact5.domainTableName d
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
        [SInt txid, SText key, SBlob value] ->
            (key,fromIntegral txid,)
            <$> Pact5.toTxLog (_chainwebVersion blockHeader) (_chainId blockHeader) (view blockHeight blockHeader) (Pact5.renderDomain d) key value
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
        [[SBlob value]] ->
            Just . (RowKey $ T.decodeUtf8 sk,)
            <$> Pact5.toTxLog (_chainwebVersion blockHeader) (_chainId blockHeader) (view blockHeight blockHeader) (Pact5.renderDomain d) k value
        _ -> internalError $ "queryPrev: expected 0 or 1 rows, got: " <> T.pack (show r)

-- TODO: do this in ChainwebPactDb instead?
lookupHistorical
    :: SQLiteEnv
    -> BlockHeader
    -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
    -> Pact5.RowKey
    -> IO (Historical (Maybe (Pact5.TxLog Pact5.RowData)))
lookupHistorical db blockHeader d k = do
  historicalEndTxId <-
    PactDb.getEndTxId "lookupHistorical" db (Just $ ParentHeader blockHeader)
  forM historicalEndTxId (queryHistoryLookup . fromIntegral)
  where
    queryHistoryLookup :: Int64 -> IO (Maybe (Pact5.TxLog Pact5.RowData))
    queryHistoryLookup e = do
      let sql = "SELECT rowKey, rowdata FROM [" <> Pact5.domainTableName d <>
                "] WHERE txid < ? AND rowkey = ? ORDER BY txid DESC LIMIT 1;"
      r <- qry db sql
        [SInt e, SText (toUtf8 $ Pact5.convRowKey k)]
        [RText, RBlob]
      case r of
        [[SText key, SBlob value]] ->
            Just <$> Pact5.toTxLog (_chainwebVersion blockHeader) (_chainId blockHeader) (view blockHeight blockHeader) (Pact5.renderDomain d) key value
        [] -> pure Nothing
        _ -> internalError $ "lookupHistorical: expected single-row result, got " <> sshow r
