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
    ( readFromLatest
    , readFromNthParent
    , readFrom
    , restoreAndSave
    , rewindTo
    , getBlockCtx
    -- , findLatestValidBlockHeader'
    -- , findLatestValidBlockHeader
    -- , exitOnRewindLimitExceeded
    , PactBlockM(..)
    , getEarliestBlock
    -- , lookupBlock
    , lookupParentBlockRanked
    , lookupParentBlockHash
    , lookupBlockWithHeight
    , getPayloadsAfter
    , getConsensusState
    , setConsensusState
    ) where

import Control.Lens hiding ((:>), (:<))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
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
import qualified Chainweb.Pact.Types as Pact
import Chainweb.Parent
import Chainweb.PayloadProvider
import Chainweb.Ranked
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Guards (pact5)

-- read-only rewind to the latest block.
-- note: because there is a race between getting the latest header
-- and doing the rewind, there's a chance that the latest header
-- will be unavailable when we do the rewind. in that case
-- we just keep grabbing the new "latest header" until we succeed.
-- note: this function will never rewind before genesis.
readFromLatest
    :: Logger logger
    => PactBlockM logger tbl a
    -> PactServiceM logger tbl a
readFromLatest doRead = readFromNthParent 0 doRead >>= \case
    NoHistory -> error "readFromLatest: failed to grab the latest header, this is a bug in chainweb"
    Historical a -> return a

-- read-only rewind to the nth parent before the latest block.
-- note: this function will never rewind before genesis.
readFromNthParent
    :: forall logger tbl a
    . Logger logger
    => Word
    -> PactBlockM logger tbl a
    -> PactServiceM logger tbl (Historical a)
readFromNthParent n doRead = do
    sql <- view psReadWriteSql
    v <- view chainwebVersion
    cid <- view chainId
    withSavepoint sql ReadFromNSavepoint $ do
        latest <- _consensusStateLatest <$> getConsensusState
        if genesisHeight v cid + fromIntegral @Word @BlockHeight n < _syncStateHeight latest
        then return NoHistory
        else do
            maybeNthBlock <- lookupBlockWithHeight
                (_syncStateHeight latest - fromIntegral @Word @BlockHeight n)
            case maybeNthBlock of
                -- this case for shallow nodes without enough history
                Nothing -> return NoHistory
                Just nthBlock ->
                    readFrom (parentBlockHeight v cid nthBlock) doRead

-- read-only rewind to a target block.
-- if that target block is missing, return Nothing.
readFrom
    :: Logger logger
    => Parent RankedBlockHash
    -> PactBlockM logger tbl a
    -> PactServiceM logger tbl (Historical a)
readFrom parent doRead = do
    sql <- view psReadWriteSql
    logger <- view psLogger
    v <- view chainwebVersion
    cid <- view chainId
    fakeCreationTime <- liftIO $ Parent . BlockCreationTime <$> getCurrentTimeIntegral
    let blockCtx = BlockCtx
            -- fake, we can't access this field without a block header db
            { _bctxParentCreationTime = fakeCreationTime
            , _bctxParentHash = _rankedBlockHashHash <$> parent
            , _bctxParentHeight = _rankedBlockHashHeight <$> parent
            -- fake, we can't access this field without a block header db
            , _bctxMinerReward = MinerReward 1848
            , _bctxChainwebVersion = v
            , _bctxChainId = cid
            }
    e <- ask
    liftIO $ withSavepoint sql ReadFromSavepoint $ do
        latestHeader <- _syncStateRankedBlockHash . _consensusStateLatest <$>
            ChainwebPactDb.throwOnDbError (ChainwebPactDb.getConsensusState sql)
        -- is the parent the latest header, i.e., can we get away without rewinding?
        let parentIsLatestHeader = latestHeader == unwrapParent parent
        let currentHeight = _bctxCurrentBlockHeight blockCtx
        if pact5 v cid currentHeight
        then PactDb.getEndTxId v cid "doReadFrom" sql parent >>= traverse \startTxId -> do
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
            fmap fst $
                runPactServiceM e $
                    Pact.runPactBlockM blockCtx pactDb (emptyBlockHandle startTxId) doRead
        else error "Pact 4 blocks are not playable anymore"

-- the special case where one doesn't want to extend the chain, just rewind it.
rewindTo :: Logger logger => RankedBlockHash -> PactServiceM logger tbl ()
rewindTo ancestor = do
    sql <- view psReadWriteSql
    v <- view chainwebVersion
    cid <- view chainId
    liftIO $ fmap fst $ generalBracket
        (beginSavepoint sql RewindSavePoint)
        (\_ -> \case
            ExitCaseSuccess {} -> commitSavepoint sql RewindSavePoint
            _ -> abortSavepoint sql RewindSavePoint
        ) $ \_ -> do
            _ <- PactDb.rewindDbTo v cid sql ancestor
            return ()

getBlockCtx :: EvaluationCtx p -> PactServiceM logger tbl BlockCtx
getBlockCtx ec = do
    cid <- view chainId
    v <- view chainwebVersion
    return $! blockCtxOfEvaluationCtx v cid ec

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
    :: forall logger err q tbl.
    (Logger logger, Monoid q, HasCallStack)
    => NonEmpty (ExceptT err (PactBlockM logger tbl) q, EvaluationCtx BlockPayloadHash)
    -> ExceptT err (PactServiceM logger tbl) q
restoreAndSave blocks@((_, forkPoint) :| _) = do
    sql <- view psReadWriteSql
    e <- ask
    fmap fst $ generalBracket
        (liftIO $ beginSavepoint sql RestoreAndSaveSavePoint)
        (\_ -> liftIO . \case
            ExitCaseSuccess {} -> commitSavepoint sql RestoreAndSaveSavePoint
            _ -> abortSavepoint sql RestoreAndSaveSavePoint
        ) $ \_ -> do
            -- TODO PP: check first if we're rewinding past "final" point? same with rewindTo above.
            txid <- liftIO $ PactDb.rewindDbTo
                (_chainwebVersion e)
                (_chainId e)
                sql
                (unwrapParent $ _evaluationCtxRankedParentHash forkPoint)
            mapExceptT liftIO $ extend e (mempty, txid) (NEL.toList blocks)
    where
    extend e (m, startTxId) = \case
        (blockAction, evalCtx) : subsequentBlocks -> let
            !bh = _evaluationCtxCurrentHeight evalCtx
            blockEnv = ChainwebPactDb.BlockHandlerEnv
                { ChainwebPactDb._blockHandlerDb = _psReadWriteSql e
                , ChainwebPactDb._blockHandlerLogger = _psLogger e
                , ChainwebPactDb._blockHandlerVersion = _chainwebVersion e
                , ChainwebPactDb._blockHandlerBlockHeight = bh
                , ChainwebPactDb._blockHandlerChainId = _chainId e
                , ChainwebPactDb._blockHandlerMode = Pact.Transactional
                , ChainwebPactDb._blockHandlerUpperBoundTxId = startTxId
                , ChainwebPactDb._blockHandlerAtTip = True
                }
            pactDb = ChainwebPactDb.chainwebPactBlockDb blockEnv
            in if pact5 (_chainwebVersion e) (_chainId e) bh then do

                let blockCtx = blockCtxOfEvaluationCtx (_chainwebVersion e) (_chainId e) evalCtx

                let runBlock = mapExceptT
                        (\r -> runPactServiceM e $ fmap
                            (\(eitherR, hndl) -> (,hndl) <$> eitherR)
                            (Pact.runPactBlockM blockCtx pactDb (emptyBlockHandle startTxId) r)
                        )

                -- run the block
                (m', blockHandle) <- runBlock blockAction
                -- compute the accumulator early
                let !m'' = m <> m'
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
                    (_psReadWriteSql e)
                    evalCtx
                    blockHandle

                extend e (m'', _blockHandleTxId blockHandle) subsequentBlocks

            else error $
                "Pact 5 block executed on block height before Pact 5 fork, height: " <> sshow bh
        [] -> return m
--
-- -------------------------------------------------------------------------- --
-- Incremental rewindTo

-- | INTERNAL FUNCTION. DON'T USE UNLESS YOU KNOW WHAT YOU DO.
--
-- Used for large incremental rewinds during pact history replay. Unlike
-- @rewindTo@, this version is not transactional but instead incrementally
-- commits the intermediate evaluation state to the pact database.
--
-- Rewinds the pact state to the given parent header.
--
-- If the rewind is deeper than the optionally provided rewind limit, an
-- exception is raised.
--
-- rewindToIncremental
--     :: forall logger tbl
--     . (HasCallStack, CanReadablePayloadCas tbl, Logger logger)
--     => Maybe RewindLimit
--         -- ^ if set, limit rewinds to this delta
--     -> (Parent BlockHeader)
--         -- ^ The parent header which is the rewind target
--     -> PactServiceM logger tbl ()
-- rewindToIncremental rewindLimit (Parent parent) = do

--     latestHeader <- findLatestValidBlockHeader' >>= maybe failNonGenesisOnEmptyDb return

--     failOnTooLowRequestedHeight latestHeader
--     playFork latestHeader

--     where
--     parentHeight = view blockHeight parent


--     -- TODO: PP
--     -- failOnTooLowRequestedHeight lastHeader = case rewindLimit of
--     --     Just limit
--     --         | let limitHeight = BlockHeight $ _rewindLimit limit
--     --         , parentHeight + 1 + limitHeight < lastHeight -> -- need to stick with addition because Word64
--     --             throwM $ RewindLimitExceeded limit (Just lastHeader) (Just parent)
--     --     _ -> return ()
--     --     where
--     --     lastHeight = view blockHeight lastHeader

--     failNonGenesisOnEmptyDb = error "impossible: playing non-genesis block to empty DB"

--     playFork :: BlockHeader -> PactServiceM logger tbl ()
--     playFork lastHeader = do
--         bhdb <- asks _psBlockHeaderDb
--         commonAncestor <- liftIO $ forkEntry bhdb lastHeader parent
--         cp <- view psCheckpointer
--         payloadDb <- view psPdb
--         let ancestorHeight = view blockHeight commonAncestor

--         logger <- view psLogger

--         -- 'getBranchIncreasing' expects an 'IO' callback because it
--         -- maintains an 'TreeDB' iterator. 'withPactState' allows us to call
--         -- pact service actions from the callback.

--         (_ :> c) <- withPactState $ \runPact ->
--             getBranchIncreasing bhdb parent (int ancestorHeight) $ \newBlocks -> do

--                 -- fastforwards all blocks in a chunk in a single database
--                 -- transaction.
--                 -- invariant: when playing a chunk, the checkpointer must
--                 -- already be at `cur`.
--                 let playChunk :: IORef BlockHeight -> BlockHeader -> Stream (Of BlockHeader) IO r -> IO (Of BlockHeader r)
--                     playChunk heightRef cur blockChunk = runPact $ do
--                         (r, Last header) <- restoreAndSave
--                             (Just $ Parent cur)
--                             $ blockChunk & S.map
--                             (\blockHeader -> do
--                                 payload <- liftIO $ lookupPayloadWithHeight payloadDb (Just $ view blockHeight blockHeader) (view blockPayloadHash blockHeader) >>= \case
--                                     Nothing -> internalError
--                                         $ "Checkpointer.rewindTo.fastForward: lookup of payload failed"
--                                         <> ". BlockPayloadHash: " <> encodeToText (view blockPayloadHash blockHeader)
--                                         <> ". Block: "<> encodeToText (ObjectEncoded blockHeader)
--                                     Just x -> return $ payloadWithOutputsToPayloadData x
--                                 liftIO $ writeIORef heightRef (view blockHeight blockHeader)
--                                 PactBlockM $ Pair
--                                     (void $ Pact4.execBlock blockHeader (CheckablePayload payload))
--                                     (void $ Pact.execExistingBlock blockHeader (CheckablePayload payload))
--                                 return (Last (Just blockHeader), blockHeader)
--                             )

--                         return $! fromJuste header :> r

--                 -- This stream is guaranteed to at least contain @e@.
--                 (curHdr, remaining) <- fromJuste <$> S.uncons newBlocks

--                 -- we have to rewind to the current header to start, for
--                 -- playChunk's invariant to be satisfied
--                 Internal.rewindTo cp
--                     (Just $ Parent curHdr)

--                 heightRef <- newIORef (view blockHeight curHdr)
--                 withAsync (heightProgress (view blockHeight curHdr) heightRef (logInfo_ logger)) $ \_ ->
--                     remaining
--                         & S.copy
--                         & S.length_
--                         & S.chunksOf 1000
--                         & foldChunksM (playChunk heightRef) curHdr

--         when (c /= 0) $
--             logInfoPact $ "rewindTo.playFork: replayed " <> sshow c <> " blocks"

getEarliestBlock :: PactServiceM logger tbl (Maybe RankedBlockHash)
getEarliestBlock = do
    sql <- view psReadWriteSql
    liftIO $ ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getEarliestBlock sql

getConsensusState :: PactServiceM logger tbl ConsensusState
getConsensusState = do
    sql <- view psReadWriteSql
    liftIO $ ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getConsensusState sql

setConsensusState :: ConsensusState -> PactServiceM logger tbl ()
setConsensusState cs = do
    sql <- view psReadWriteSql
    liftIO $ ChainwebPactDb.throwOnDbError $ ChainwebPactDb.setConsensusState sql cs

lookupBlockWithHeight :: BlockHeight -> PactServiceM logger tbl (Maybe (Ranked (Parent BlockHash)))
lookupBlockWithHeight bh = do
    sql <- view psReadWriteSql
    liftIO $ ChainwebPactDb.throwOnDbError $ ChainwebPactDb.lookupBlockWithHeight sql bh

lookupParentBlockRanked :: Ranked (Parent BlockHash) -> PactServiceM logger tbl Bool
lookupParentBlockRanked rpbh = do
    sql <- view psReadWriteSql
    liftIO $ ChainwebPactDb.throwOnDbError $ ChainwebPactDb.lookupParentBlockRanked sql rpbh

lookupParentBlockHash :: Parent BlockHash -> PactServiceM logger tbl Bool
lookupParentBlockHash pbh = do
    sql <- view psReadWriteSql
    liftIO $ ChainwebPactDb.throwOnDbError $ ChainwebPactDb.lookupParentBlockHash sql pbh

getPayloadsAfter :: Parent BlockHeight -> PactServiceM logger tbl [Ranked BlockPayloadHash]
getPayloadsAfter b = do
    sql <- view psReadWriteSql
    liftIO $ ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getPayloadsAfter sql b

-- -------------------------------------------------------------------------- --
-- Utils

-- heightProgress :: BlockHeight -> IORef BlockHeight -> (Text -> IO ()) -> IO ()
-- heightProgress initialHeight ref logFun = forever $ do
--     threadDelay (20 * 1_000_000)
--     h <- readIORef ref
--     logFun
--         $ "processed blocks: " <> sshow (h - initialHeight)
--         <> ", current height: " <> sshow h
