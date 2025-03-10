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
  , getBlockParent
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
import qualified Chainweb.Pact.Backend.ChainwebPactDb as Pact4
import qualified Chainweb.Pact.Backend.ChainwebPactDb as Pact5

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
    -> SQLiteEnv
    -> IntraBlockPersistence
    -> ChainwebVersion
    -> ChainId
    -> (Checkpointer logger -> IO a)
    -> IO a
withCheckpointerResources logger sqlenv p v cid inner = do
    inner =<< initCheckpointerResources sqlenv p logger v cid

initCheckpointerResources
    :: (Logger logger)
    => SQLiteEnv
    -> IntraBlockPersistence
    -> logger
    -> ChainwebVersion
    -> ChainId
    -> IO (Checkpointer logger)
initCheckpointerResources sql p loggr v cid = do
    Pact5.initSchema sql
    return Checkpointer
        { cpLogger = loggr
        , cpCwVersion = v
        , cpChainId = cid
        , cpSql = sql
        , cpIntraBlockPersistence = p
        }

-- | Rewind to a particular block *in-memory*, producing a read-write snapshot
-- of the database at that block to compute some value, after which the snapshot
-- is discarded and nothing is saved to the database.
--
-- prerequisite: ParentHeader is an ancestor of the "latest block";
-- if that isn't the case, NoHistory is returned.
readFrom
  :: forall logger a
  . (Logger logger)
  => Checkpointer logger
  -> Maybe (Parent RankedBlockHash)
  -> (ChainwebPactDb -> BlockHandle -> IO a)
  -> IO (Historical a)
readFrom res maybeParent doRead = do
  let currentHeight = case maybeParent of
        Nothing -> genesisHeight res.cpCwVersion res.cpChainId
        Just parent -> succ $ _rankedBlockHashHeight $ unwrapParent parent

  modifyMVar res.cpModuleCacheVar $ \sharedModuleCache -> do
    bracket
      (beginSavepoint res.cpSql BatchSavepoint)
      (\_ -> abortSavepoint res.cpSql BatchSavepoint) \() -> do
      -- NB it's important to do this *after* you start the savepoint (and thus
      -- the db transaction) to make sure that the latestHeader check is up to date.
      latestHeader <- getLatestBlock res.cpSql
      -- is the parent the latest header, i.e., can we get away without rewinding?
      let parentIsLatestHeader = fmap Parent latestHeader == maybeParent
      h <- if
          | pact5 res.cpCwVersion res.cpChainId currentHeight ->
            PactDb.getEndTxId "doReadFrom" res.cpSql maybeParent >>= traverse \startTxId -> do
            let
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
            r <- doRead pactDb (emptyBlockHandle startTxId)
            return (r, sharedModuleCache)
          | otherwise ->
            error $
              "Pact 5 readFrom executed on block height before Pact 5 fork, height: " <> sshow currentHeight
      case h of
        NoHistory -> return (sharedModuleCache, NoHistory)
        Historical (r, finalCache) -> return (finalCache, Historical r)



-- the special case where one doesn't want to extend the chain, just rewind it.
rewindTo :: Logger logger => Checkpointer logger -> Maybe (Parent RankedBlockHash) -> IO ()
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
  -> Maybe (Parent RankedBlockHash)
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
      -> IO (Of (q, Maybe (Parent RankedBlockHash), TxId, DbCache PersistModuleData) r)
    extend startTxId startModuleCache = Streaming.foldM
      (\(m, maybeParent, txid, moduleCache) block -> do
        let
          !bh = case maybeParent of
            Nothing -> genesisHeight res.cpCwVersion res.cpChainId
            Just (Parent parent) -> succ $ _rankedBlockHashHeight parent
        case block of
          RunnableBlock runBlock
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
              ((m', nextBlockHeader), blockHandle) <- runBlock pactDb maybeParent (emptyBlockHandle txid)
              -- compute the accumulator early
              let !m'' = m <> m'
              case maybeParent of
                Nothing
                  | genesisHeight res.cpCwVersion res.cpChainId /= succ (_rankedBlockHashHeight nextBlockHeader) -> error
                    "doRestoreAndSave: block with no parent, genesis block, should have genesis height but doesn't,"
                Just (Parent ph)
                  | bh /= _rankedBlockHashHeight nextBlockHeader -> error $
                    "doRestoreAndSave: non-genesis block should be one higher than its parent. parent at "
                      <> sshow (_rankedBlockHashHeight ph) <> ", child height " <> sshow (_rankedBlockHashHeight nextBlockHeader)
                _ -> return ()
              Pact5.commitBlockStateToDatabase res.cpSql
                (_rankedBlockHashHash nextBlockHeader) (_rankedBlockHashHeight nextBlockHeader)
                blockHandle

              return (m'', Just (Parent nextBlockHeader), _blockHandleTxId blockHandle, moduleCache)

            | otherwise -> error $
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
getLatestBlock :: HasCallStack => SQLiteEnv -> IO (Maybe RankedBlockHash)
getLatestBlock db = do
  r <- qry_ db qtext [RInt, RBlob] >>= mapM go
  case r of
    [] -> return Nothing
    (!o:_) -> return (Just o)
  where
    qtext = "SELECT blockheight, hash FROM BlockHistory ORDER BY blockheight DESC LIMIT 1"

    go [SInt hgt, SBlob blob] =
        let hash = either error id $ runGetEitherS decodeBlockHash blob
        in return $ RankedBlockHash (fromIntegral hgt) hash
    go _ = fail "Chainweb.Pact.Backend.RelationalCheckpointer.getLatest: impossible. This is a bug in chainweb-node."

-- | Ask: is the checkpointer aware of the given block?
lookupBlock :: SQLiteEnv -> RankedBlockHash -> IO Bool
lookupBlock db (RankedBlockHash bheight bhash) = do
    r <- qry db qtext [SInt $ fromIntegral bheight, SBlob (runPutS (encodeBlockHash bhash))]
                      [RInt]
    case r of
      [[SInt n]] -> return $! n == 1
      [_] -> error "lookupBlock: output type mismatch"
      _ -> error "Expected single-row result"
  where
    qtext = "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"

getBlockParent :: ChainwebVersion -> ChainId -> SQLiteEnv -> (BlockHeight, BlockHash) -> IO (Maybe BlockHash)
getBlockParent v cid db (bh, hash)
    | bh == genesisHeight v cid = return Nothing
    | otherwise = do
        blockFound <- lookupBlock db (RankedBlockHash bh hash)
        if not blockFound
          then return Nothing
          else do
            r <- qry db qtext [SInt (fromIntegral (pred bh))] [RBlob]
            case r of
              [[SBlob blob]] ->
                either error (return . return) $! runGetEitherS decodeBlockHash blob
              [] -> error "getBlockParent: block was found but its parent couldn't be found"
              _ -> error "getBlockParent: output type mismatch"
  where
    qtext = "SELECT hash FROM BlockHistory WHERE blockheight = ?"
