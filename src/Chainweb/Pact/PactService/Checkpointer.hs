{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.Pact.PactService.Checkpointer
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Stuart Popejoy, Lars Kuhtz, Emily Pillmore
-- Stability: experimental
--
-- Checkpointer interaction for Pact service.
--
module Chainweb.Pact.PactService.Checkpointer
    ( mkFakeParentCreationTime
    , readFromLatest
    , readFromNthParent
    , readFrom
    , restoreAndSave
    , rewindTo
    -- , findLatestValidBlockHeader'
    -- , findLatestValidBlockHeader
    -- , exitOnRewindLimitExceeded
    , getEarliestBlock
    -- , lookupBlock
    , lookupRankedBlockHash
    , lookupParentBlockHash
    , lookupBlockWithHeight
    , getPayloadsAfter
    , getConsensusState
    , setConsensusState
    ) where

import Control.Lens hiding ((:>), (:<))
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid hiding (Product (..))
import GHC.Stack
import Prelude hiding (lookup)

import qualified Pact.Core.Persistence.Types as Pact

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.MinerReward
import qualified Chainweb.Pact.Backend.ChainwebPactDb as ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import qualified Chainweb.Pact.Backend.Utils as PactDb
import Chainweb.Pact.Types
import Chainweb.Parent
import Chainweb.PayloadProvider
import Chainweb.Ranked
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Guards (pact5)
import Control.Exception.Safe (MonadMask)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Chainweb.Pact.Backend.ChainwebPactDb (lookupRankedBlockHash)
import Control.Monad.State.Strict

-- readFrom demands to know this context in case it's mining a block.
-- But it's not really used by /local, or polling, so we fabricate it
-- for those users.
mkFakeParentCreationTime :: IO (Parent BlockCreationTime)
mkFakeParentCreationTime = do
    Parent . BlockCreationTime <$> getCurrentTimeIntegral

-- read-only rewind to the latest block.
-- note: because there is a race between getting the latest header
-- and doing the rewind, there's a chance that the latest header
-- will be unavailable when we do the rewind. in that case
-- we just keep grabbing the new "latest header" until we succeed.
-- note: this function will never rewind before genesis.
readFromLatest
    :: Logger logger
    => logger
    -> ChainwebVersion
    -> ChainId
    -> SQLiteEnv
    -> Parent BlockCreationTime
    -> (BlockEnv -> BlockHandle -> IO a)
    -> IO a
readFromLatest logger v cid sql parentCreationTime doRead =
    readFromNthParent logger v cid sql parentCreationTime 0 doRead >>= \case
        NoHistory -> error "readFromLatest: failed to grab the latest header, this is a bug in chainweb"
        Historical a -> return a

-- read-only rewind to the nth parent before the latest block.
-- note: this function will never rewind before genesis.
readFromNthParent
    :: Logger logger
    => logger
    -> ChainwebVersion
    -> ChainId
    -> SQLiteEnv
    -> Parent BlockCreationTime
    -> Word
    -> (BlockEnv -> BlockHandle -> IO a)
    -> IO (Historical a)
readFromNthParent logger v cid sql parentCreationTime n doRead = do
    withSavepoint sql ReadFromNSavepoint $ do
        latest <- _consensusStateLatest . fromJuste <$> getConsensusState sql
        if genesisHeight v cid + fromIntegral @Word @BlockHeight n < _syncStateHeight latest
        then return NoHistory
        else do
            let targetHeight = _syncStateHeight latest - fromIntegral @Word @BlockHeight n
            lookupBlockWithHeight sql targetHeight >>= \case
                    -- this case for shallow nodes without enough history
                    Nothing -> return NoHistory
                    Just nthBlock ->
                        readFrom logger v cid sql parentCreationTime (parentBlockHeight v cid nthBlock) doRead

-- read-only rewind to a target block.
-- if that target block is missing, return Nothing.
readFrom
    :: Logger logger
    => logger
    -> ChainwebVersion
    -> ChainId
    -> SQLiteEnv
    -> Parent BlockCreationTime
    -- ^ you can fake this if you're not making a new block
    -> Parent RankedBlockHash
    -> (BlockEnv -> BlockHandle -> IO a)
    -> IO (Historical a)
readFrom logger v cid sql parentCreationTime parent doRead = do
    let blockCtx = BlockCtx
            { _bctxParentCreationTime = parentCreationTime
            , _bctxParentHash = _rankedBlockHashHash <$> parent
            , _bctxParentHeight = _rankedBlockHashHeight <$> parent
            , _bctxMinerReward = blockMinerReward v (childBlockHeight v cid parent)
            , _bctxChainwebVersion = v
            , _bctxChainId = cid
            }
    liftIO $ withSavepoint sql ReadFromSavepoint $ do
        !latestHeader <- maybe (genesisRankedParentBlockHash v cid) (Parent . _syncStateRankedBlockHash . _consensusStateLatest) <$>
            ChainwebPactDb.throwOnDbError (ChainwebPactDb.getConsensusState sql)
        -- is the parent the latest header, i.e., can we get away without rewinding?
        let parentIsLatestHeader = latestHeader == parent
        let currentHeight = _bctxCurrentBlockHeight blockCtx
        if pact5 v cid currentHeight
        then PactDb.getEndTxId v cid sql parent >>= traverse \startTxId -> do
            let
                blockHandlerEnv = ChainwebPactDb.BlockHandlerEnv
                    { ChainwebPactDb._blockHandlerDb = sql
                    , ChainwebPactDb._blockHandlerLogger = logger
                    , ChainwebPactDb._blockHandlerVersion = v
                    , ChainwebPactDb._blockHandlerChainId = cid
                    , ChainwebPactDb._blockHandlerBlockHeight = currentHeight
                    , ChainwebPactDb._blockHandlerMode = Pact.Transactional
                    , ChainwebPactDb._blockHandlerUpperBoundTxId = startTxId
                    , ChainwebPactDb._blockHandlerAtTip = parentIsLatestHeader
                    }
            let pactDb = ChainwebPactDb.chainwebPactBlockDb blockHandlerEnv
            let blockEnv = BlockEnv blockCtx pactDb
            doRead blockEnv (emptyBlockHandle startTxId)
        else error "Pact 4 blocks are not playable anymore"

-- the special case where one doesn't want to extend the chain, just rewind it.
rewindTo
    :: ChainwebVersion -> ChainId -> SQLiteEnv
    -> RankedBlockHash -> IO ()
rewindTo v cid sql ancestor = do
    withSavepoint sql RewindSavePoint $
        void $ PactDb.rewindDbTo v cid sql ancestor

-- TODO: log more?
-- | Given a list of blocks in ascending order, rewind to the first
-- of them and play all of the subsequent blocks, saving the result persistently.
-- this function takes care of making sure that this is done *atomically*.
-- The two following expressions should be equivalent:
-- restoreAndSave cp v cid blocks1 <> restoreAndSave cp v cid blocks2
-- restoreAndSave cp v cid (blocks1 <> blocks2)
--
-- prerequisites:
--   - the first block's parent must be an ancestor of the latest block in the database.
--   - each subsequent block must be the direct child of the previous block.
restoreAndSave
    :: forall logger m q.
    (Logger logger, MonadIO m, MonadMask m, Semigroup q, HasCallStack)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> SQLiteEnv
    -> NonEmpty (BlockCtx, BlockEnv -> StateT BlockHandle m (q, (BlockHash, BlockPayloadHash)))
    -> m q
restoreAndSave logger v cid sql blocks = do
    withSavepoint sql RestoreAndSaveSavePoint $ do
        -- TODO PP: check first if we're rewinding past "final" point? same with rewindTo above.
        startTxId <- liftIO $ PactDb.rewindDbTo
            v
            cid
            sql
            (unwrapParent $ _bctxParentRankedBlockHash $ fst $ NE.head blocks)

        (mStart, startTxId') <- executeBlock startTxId (NE.head blocks)
        extend mStart startTxId' (NE.tail blocks)

    where

    extend !acc !startTxId (blk:blks) = do
        (acc', endTxId) <- executeBlock startTxId blk
        -- compute the accumulator strictly
        let !acc'' = acc <> acc'
        extend acc'' endTxId blks
    extend !acc !_ [] = return acc

    executeBlock startTxId (blockCtx, blockAction) = do
        let
            !bh = _bctxCurrentBlockHeight blockCtx
            blockEnv = ChainwebPactDb.BlockHandlerEnv
                { ChainwebPactDb._blockHandlerDb = sql
                , ChainwebPactDb._blockHandlerLogger = logger
                , ChainwebPactDb._blockHandlerVersion = v
                , ChainwebPactDb._blockHandlerBlockHeight = bh
                , ChainwebPactDb._blockHandlerChainId = cid
                , ChainwebPactDb._blockHandlerMode = Pact.Transactional
                , ChainwebPactDb._blockHandlerUpperBoundTxId = startTxId
                , ChainwebPactDb._blockHandlerAtTip = True
                }
            pactDb = ChainwebPactDb.chainwebPactBlockDb blockEnv
            in if pact5 v cid bh then do

                -- run the block
                ((m, blockInfo), blockHandle) <-
                    runStateT (blockAction (BlockEnv blockCtx pactDb)) (emptyBlockHandle startTxId)
                -- TODO PP: also check the child matches the parent height
                -- case maybeParent of
                --   Nothing
                --     | genesisHeight res.cpCwVersion res.cpChainId /= succ (_rankedBlockHashHeight nextBlock) -> error
                --       "doRestoreAndSave: block with no parent, genesis block, should have genesis height but doesn't,"
                --   Just (Parent ph)
                --     | bh /= _rankedBlockHashHeight nextBlock -> error $
                --       "doRestoreAndSave: non-genesis block should be one higher than its parent. parent at "
                --         <> sshow (_rankedBlockHashHeight ph) <> ", child height " <> sshow (_rankedBlockHashHeight nextBlock)
                --   _ -> return ()
                liftIO $ ChainwebPactDb.commitBlockStateToDatabase
                    sql
                    (Ranked bh blockInfo)
                    blockHandle

                return (m, _blockHandleTxId blockHandle)

            else error $
                "Pact 5 block executed on block height before Pact 5 fork, height: " <> sshow bh

getEarliestBlock :: SQLiteEnv -> IO (Maybe RankedBlockHash)
getEarliestBlock sql = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getEarliestBlock sql

getConsensusState :: SQLiteEnv -> IO (Maybe ConsensusState)
getConsensusState sql = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getConsensusState sql

setConsensusState :: SQLiteEnv -> ConsensusState -> IO ()
setConsensusState sql cs = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.setConsensusState sql cs

lookupBlockWithHeight :: SQLiteEnv -> BlockHeight -> IO (Maybe (Ranked (Parent BlockHash)))
lookupBlockWithHeight sql bh = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.lookupBlockWithHeight sql bh

-- lookupBlockByHash :: SQLiteEnv -> EvaluationCtx p -> IO Bool
-- lookupBlockByHash sql rpbh = do
--     ChainwebPactDb.throwOnDbError $ ChainwebPactDb.lookupBlockByHash sql rpbh

lookupParentBlockHash :: SQLiteEnv -> Parent BlockHash -> IO Bool
lookupParentBlockHash sql pbh = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.lookupParentBlockHash sql pbh

getPayloadsAfter :: SQLiteEnv -> Parent BlockHeight -> IO [Ranked BlockPayloadHash]
getPayloadsAfter sql b = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getPayloadsAfter sql b

-- -------------------------------------------------------------------------- --
-- Utils

-- heightProgress :: BlockHeight -> IORef BlockHeight -> (Text -> IO ()) -> IO ()
-- heightProgress initialHeight ref logFun = forever $ do
--     threadDelay (20 * 1_000_000)
--     h <- readIORef ref
--     logFun
--         $ "processed blocks: " <> sshow (h - initialHeight)
--         <> ", current height: " <> sshow h
