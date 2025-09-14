{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
import Pact.Core.Persistence.Types qualified as Pact
import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.MinerReward
import Chainweb.Pact.Backend.ChainwebPactDb qualified as ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.Utils qualified as PactDb
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
import Chainweb.Pact.Backend.ChainwebPactDb (lookupRankedBlockHash)
import Control.Monad.State.Strict
import System.LogLevel
import Chainweb.Pact4.Backend.ChainwebPactDb qualified as Pact4
import Control.Concurrent.MVar
import Pact.Types.Persistence qualified as Pact4
import Chainweb.Pact.Backend.DbCache (defaultModuleCacheLimit)
import Pact.Interpreter qualified as Pact4
import Data.ByteString.Short qualified as BS
import Data.Coerce
import Data.HashMap.Strict qualified as HashMap
import Pact.Types.Command qualified as Pact4
import Pact.Types.Hash qualified as Pact4
import Data.Foldable1 (foldlMapM1)

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
    -> Parent RankedBlockHash
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
    | Pact5RunnableBlock
        (ChainwebPactDb -> StateT BlockHandle m (q, (BlockHash, BlockPayloadHash)))

foldState1
    :: (Monad m, Semigroup a)
    => NonEmpty (s -> m (a, s))
    -> s
    -> m a
foldState1 bs !initialState = do
    fst <$> foldlMapM1 ($ initialState) g bs
    where
    g (a, s) f = do
        (b, s') <- f s
        let !c = a <> b
        pure (c, s')

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
    -> Parent RankedBlockHash
    -> NonEmpty (RunnableBlock m q)
    -> m q
restoreAndSave logger cid sql parent blocks = do
    withSavepoint sql RestoreAndSaveSavePoint $ do
        -- TODO PP: check first if we're rewinding past "final" point? same with rewindTo above.
        startTxId <- liftIO $ PactDb.rewindDbTo cid sql parent
        let startBlockHeight = childBlockHeight cid parent
        foldState1 (fmap executeBlock blocks) (T2 startBlockHeight startTxId)
    where

    executeBlock :: RunnableBlock m q -> T2 BlockHeight Pact.TxId -> m (q, T2 BlockHeight Pact.TxId)
    executeBlock blockAction (T2 currentBlockHeight startTxId) = do
        case blockAction of
            Pact4RunnableBlock block | not (pact5 cid currentBlockHeight) -> do
                let pact4TxId = Pact4.TxId (coerce startTxId)
                let blockHandlerEnv = Pact4.mkBlockHandlerEnv cid currentBlockHeight sql logger
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
                            res <- doLookupSuccessful sql currentBlockHeight (coerce hs)
                            return
                                $ HashMap.mapKeys coerce
                                $ HashMap.map
                                    (\(T3 height _payloadhash bhash) -> T2 height bhash)
                                    res
                        , _cpHeaderOracle = Pact4.headerOracleForBlock blockHandlerEnv
                        }
                !(!m, !blockInfo) <- block pact4DbEnv
                -- let outputBlockHandle =
                finalBlockState <- liftIO $
                    Pact4._benvBlockState <$> readMVar newBlockDbEnv
                -- TODO Robert: use payload hash here too
                liftIO $ Pact4.commitBlockStateToDatabase sql (fst blockInfo) (snd blockInfo) currentBlockHeight finalBlockState
                let currentRbh = RankedBlockHash currentBlockHeight (fst blockInfo)
                return
                    (m
                    , T2
                        (childBlockHeight cid (Parent currentRbh))
                        (Pact.TxId $ coerce $ Pact4._bsTxId finalBlockState))
            Pact5RunnableBlock block | pact5 cid currentBlockHeight -> do
                let
                    blockEnv = ChainwebPactDb.BlockHandlerEnv
                        { ChainwebPactDb._blockHandlerDb = sql
                        , ChainwebPactDb._blockHandlerLogger = logger
                        , ChainwebPactDb._blockHandlerBlockHeight = currentBlockHeight
                        , ChainwebPactDb._blockHandlerChainId = cid
                        , ChainwebPactDb._blockHandlerMode = Pact.Transactional
                        , ChainwebPactDb._blockHandlerUpperBoundTxId = startTxId
                        , ChainwebPactDb._blockHandlerAtTip = True
                        }
                    pactDb = ChainwebPactDb.chainwebPactBlockDb blockEnv
                -- run the block
                !((!m', !blockInfo), !blockHandle) <-
                    runStateT (block pactDb) (emptyBlockHandle startTxId)
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
                    (Ranked currentBlockHeight blockInfo)
                    blockHandle
                let currentRbh = RankedBlockHash currentBlockHeight (fst blockInfo)
                return
                    (m'
                    , T2
                        (childBlockHeight cid (Parent currentRbh))
                        (_blockHandleTxId blockHandle))

            _ -> do
                error $ "pact 4/5 mismatch: " <> sshow (cid, currentBlockHeight)

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
