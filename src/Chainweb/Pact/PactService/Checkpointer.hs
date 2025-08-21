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
    , lookupBlockHash
    , lookupBlockWithHeight
    , getPayloadsAfter
    , getConsensusState
    , setConsensusState
    , RunnableBlock(..)
    , PactRead(..)
    , readPact5
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
import System.LogLevel
import qualified Chainweb.Pact4.Types as Pact4
import qualified Chainweb.Pact4.Backend.ChainwebPactDb as Pact4
import Control.Concurrent.MVar
import qualified Pact.Types.Persistence as Pact4
import Chainweb.Pact.Backend.DbCache (defaultModuleCacheLimit)
import qualified Pact.Interpreter as Pact4
import qualified Data.ByteString.Short as BS
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Pact.Types.Command as Pact4
import qualified Pact.Types.Hash as Pact4

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
    :: (HasCallStack, HasVersion, Logger logger)
    => logger
    -> ChainId
    -> SQLiteEnv
    -> Parent BlockCreationTime
    -> PactRead a
    -> IO a
readFromLatest logger cid sql parentCreationTime doRead =
    readFromNthParent logger cid sql parentCreationTime 0 doRead >>= \case
        NoHistory -> error "readFromLatest: failed to grab the latest header, this is a bug in chainweb"
        Historical a -> return a

-- read-only rewind to the nth parent before the latest block.
-- note: this function will never rewind before genesis.
readFromNthParent
    :: (HasCallStack, HasVersion, Logger logger)
    => logger
    -> ChainId
    -> SQLiteEnv
    -> Parent BlockCreationTime
    -> Word
    -> PactRead a
    -> IO (Historical a)
readFromNthParent logger cid sql parentCreationTime n doRead = do
    withSavepoint sql ReadFromNSavepoint $ do
        latest <-
            _consensusStateLatest . fromMaybe (error "readFromNthParent is illegal to call before genesis")
            <$> getConsensusState sql
        if genesisHeight cid + fromIntegral @Word @BlockHeight n > _syncStateHeight latest
        then do
            logFunctionText logger Warn $ "readFromNthParent asked to rewind beyond genesis, to "
                <> sshow (int (_syncStateHeight latest) - int n :: Integer)
            return NoHistory
        else do
            let targetHeight = _syncStateHeight latest - fromIntegral @Word @BlockHeight n
            lookupBlockWithHeight sql targetHeight >>= \case
                    -- this case for shallow nodes without enough history
                    Nothing -> do
                        logFunctionText logger Warn "readFromNthParent asked to rewind beyond known blocks"
                        return NoHistory
                    Just nthBlock ->
                        readFrom logger cid sql parentCreationTime (Parent nthBlock) doRead

-- read-only rewind to a target block.
-- if that target block is missing, return Nothing.
readFrom
    :: (HasCallStack, HasVersion, Logger logger)
    => logger
    -> ChainId
    -> SQLiteEnv
    -> Parent BlockCreationTime
    -- ^ you can fake this if you're not making a new block
    -> Parent RankedBlockHash
    -> PactRead a
    -> IO (Historical a)
readFrom logger cid sql parentCreationTime parent pactRead = do
    let blockCtx = BlockCtx
            { _bctxParentCreationTime = parentCreationTime
            , _bctxParentHash = _rankedBlockHashHash <$> parent
            , _bctxParentHeight = _rankedBlockHashHeight <$> parent
            , _bctxMinerReward = blockMinerReward (childBlockHeight cid parent)
            , _bctxChainId = cid
            }
    liftIO $ withSavepoint sql ReadFromSavepoint $ do
        !latestHeader <- maybe (genesisRankedParentBlockHash cid) (Parent . _syncStateRankedBlockHash . _consensusStateLatest) <$>
            ChainwebPactDb.throwOnDbError (ChainwebPactDb.getConsensusState sql)
        -- is the parent the latest header, i.e., can we get away without rewinding?
        let parentIsLatestHeader = latestHeader == parent
        let currentHeight = _bctxCurrentBlockHeight blockCtx
        PactDb.getEndTxId cid sql parent >>= traverse \startTxId ->
            if pact5 cid currentHeight then do
                let
                    blockHandlerEnv = ChainwebPactDb.BlockHandlerEnv
                        { ChainwebPactDb._blockHandlerDb = sql
                        , ChainwebPactDb._blockHandlerLogger = logger
                        , ChainwebPactDb._blockHandlerChainId = cid
                        , ChainwebPactDb._blockHandlerBlockHeight = currentHeight
                        , ChainwebPactDb._blockHandlerMode = Pact.Transactional
                        , ChainwebPactDb._blockHandlerUpperBoundTxId = startTxId
                        , ChainwebPactDb._blockHandlerAtTip = parentIsLatestHeader
                        }
                let pactDb = ChainwebPactDb.chainwebPactBlockDb blockHandlerEnv
                let blockEnv = BlockEnv blockCtx pactDb
                pact5Read pactRead blockEnv (emptyBlockHandle startTxId)
            else do
                let pact4TxId = Pact4.TxId (coerce startTxId)
                let blockHandlerEnv = Pact4.mkBlockHandlerEnv cid currentHeight sql logger
                newBlockDbEnv <- liftIO $ newMVar $ Pact4.BlockDbEnv
                    blockHandlerEnv
                    -- FIXME not sharing the cache
                    (Pact4.initBlockState defaultModuleCacheLimit pact4TxId)
                let pactDb = Pact4.rewoundPactDb currentHeight pact4TxId

                let pact4DbEnv = Pact4.CurrentBlockDbEnv
                        { _cpPactDbEnv = Pact4.PactDbEnv pactDb newBlockDbEnv
                        , _cpRegisterProcessedTx = \hash ->
                            Pact4.runBlockDbEnv newBlockDbEnv (Pact4.indexPactTransaction $ BS.fromShort $ Pact4.unHash $ Pact4.unRequestKey hash)
                        , _cpLookupProcessedTx = \hs -> do
                            res <- doLookupSuccessful sql currentHeight (coerce hs)
                            return
                                $ HashMap.mapKeys coerce
                                $ HashMap.map
                                    (\(T3 height _payloadhash bhash) -> T2 height bhash)
                                    res
                        , _cpHeaderOracle = Pact4.headerOracleForBlock blockHandlerEnv
                        }
                pact4Read pactRead (Pact4.BlockEnv blockCtx pact4DbEnv)

-- the special case where one doesn't want to extend the chain, just rewind it.
rewindTo
    :: HasVersion
    => ChainId
    -> SQLiteEnv
    -> RankedBlockHash
    -> IO ()
rewindTo cid sql ancestor = do
    withSavepoint sql RewindSavePoint $
        void $ PactDb.rewindDbTo cid sql ancestor

data PactRead a
    = PactRead
        { pact4Read :: (forall logger. Logger logger => Pact4.BlockEnv logger -> IO a)
        , pact5Read :: BlockEnv -> BlockHandle -> IO a
        }

readPact5 :: String -> (BlockEnv -> BlockHandle -> IO a) -> PactRead a
readPact5 msg act = PactRead
    { pact4Read = \_ -> error msg
    , pact5Read = act
    }

data RunnableBlock m q
    = Pact4RunnableBlock
            (forall logger. Logger logger => Pact4.CurrentBlockDbEnv logger -> m (q, (BlockHash, BlockPayloadHash)))
    | Pact5RunnableBlock (BlockEnv -> StateT BlockHandle m (q, (BlockHash, BlockPayloadHash)))

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
    (Logger logger, MonadIO m, MonadMask m, Semigroup q, HasCallStack, HasVersion)
    => logger
    -> ChainId
    -> SQLiteEnv
    -> NonEmpty (BlockCtx, RunnableBlock m q)
    -> m q
restoreAndSave logger cid sql blocks = do
    withSavepoint sql RestoreAndSaveSavePoint $ do
        -- TODO PP: check first if we're rewinding past "final" point? same with rewindTo above.
        startTxId <- liftIO $ PactDb.rewindDbTo
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
        case blockAction of
            Pact4RunnableBlock block | not (pact5 cid bh) -> do
                let pact4TxId = Pact4.TxId (coerce startTxId)
                let blockHandlerEnv = Pact4.mkBlockHandlerEnv cid bh sql logger
                newBlockDbEnv <- liftIO $ newMVar $ Pact4.BlockDbEnv
                    blockHandlerEnv
                    -- FIXME not sharing the cache
                    (Pact4.initBlockState defaultModuleCacheLimit pact4TxId)
                let pactDb = Pact4.chainwebPactDb

                let pact4DbEnv = Pact4.CurrentBlockDbEnv
                        { _cpPactDbEnv = Pact4.PactDbEnv pactDb newBlockDbEnv
                        , _cpRegisterProcessedTx = \hash ->
                            Pact4.runBlockDbEnv newBlockDbEnv (Pact4.indexPactTransaction $ BS.fromShort $ Pact4.unHash $ Pact4.unRequestKey hash)
                        , _cpLookupProcessedTx = \hs -> do
                            res <- doLookupSuccessful sql bh (coerce hs)
                            return
                                $ HashMap.mapKeys coerce
                                $ HashMap.map
                                    (\(T3 height _payloadhash bhash) -> T2 height bhash)
                                    res
                        , _cpHeaderOracle = Pact4.headerOracleForBlock blockHandlerEnv
                        }
                ((m, blockInfo)) <- block pact4DbEnv
                -- let outputBlockHandle =
                finalBlockState <- liftIO $
                    Pact4._benvBlockState <$> readMVar newBlockDbEnv
                -- TODO Robert: use payload hash here too
                liftIO $ Pact4.commitBlockStateToDatabase sql (fst blockInfo) (snd blockInfo) bh finalBlockState
                return (m, Pact.TxId $ coerce $ Pact4._bsTxId finalBlockState)
            Pact5RunnableBlock block | pact5 cid bh -> do
                let
                    blockEnv = ChainwebPactDb.BlockHandlerEnv
                        { ChainwebPactDb._blockHandlerDb = sql
                        , ChainwebPactDb._blockHandlerLogger = logger
                        , ChainwebPactDb._blockHandlerBlockHeight = bh
                        , ChainwebPactDb._blockHandlerChainId = cid
                        , ChainwebPactDb._blockHandlerMode = Pact.Transactional
                        , ChainwebPactDb._blockHandlerUpperBoundTxId = startTxId
                        , ChainwebPactDb._blockHandlerAtTip = True
                        }
                    pactDb = ChainwebPactDb.chainwebPactBlockDb blockEnv
                -- run the block
                ((m, blockInfo), blockHandle) <-
                    runStateT (block (BlockEnv blockCtx pactDb)) (emptyBlockHandle startTxId)
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

            _ -> do
                error $ "pact 4/5 mismatch: " <> sshow (cid, bh)

getEarliestBlock :: SQLiteEnv -> IO (Maybe RankedBlockHash)
getEarliestBlock sql = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getEarliestBlock sql

getConsensusState :: SQLiteEnv -> IO (Maybe ConsensusState)
getConsensusState sql = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getConsensusState sql

setConsensusState :: SQLiteEnv -> ConsensusState -> IO ()
setConsensusState sql cs = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.setConsensusState sql cs

lookupBlockWithHeight :: HasCallStack => SQLiteEnv -> BlockHeight -> IO (Maybe (Ranked BlockHash))
lookupBlockWithHeight sql bh = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.lookupBlockWithHeight sql bh

lookupBlockHash :: HasCallStack => SQLiteEnv -> BlockHash -> IO (Maybe BlockHeight)
lookupBlockHash sql pbh = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.lookupBlockHash sql pbh

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
