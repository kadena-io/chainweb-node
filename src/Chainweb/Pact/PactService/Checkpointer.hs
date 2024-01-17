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
    -- (

      -- * Checkpointer Batches
      --
      -- All usages of the checkpointer must be in the context of a checkpointer
      -- batch, which ensure proper finalization of checkpointer usage by pact serice
      -- calls.


    -- * Pact Service Checkpointer
    --
    -- Within Chainweb Pact code is evaluated in the context of a parent header,
    -- which the parent block, which is the latest block that was commit to the
    -- block chain before the new transaction code is evaluated.
    --
    -- There are two function for restoring the checkpointer for evaluation of back
    -- code:
    --
    -- * 'withCheckPointerRewind' and
    -- * 'withCurrentCheckpointer'.
    --
    -- 'withCheckPointerRewind' rewinds the checkpointer to the provided parent
    -- header. 'withCurrentCheckpointer' evaluates the pact transaction within the
    -- context of the current checkpointer state. Both functions update the value of
    -- '_psParentHeader' at the beginning and the end of each call.
    --
    -- The result of the evaluation indicates whether the result of the evaluation
    -- is persisted, i.e. is commited as a new block, or is discarded, i.e.
    -- subsequent evaluation are performed the same context as the current one.
    --
    -- , withCheckpointerRewind
    -- , withReadCheckpointerRewind
    -- , withCurrentCheckpointer
    -- , withReadCurrentCheckpointer
    -- , WithCheckpointerResult(..)

    -- * Low Level Pact Service Checkpointer Tools

    ( readFromLatest
    , readFromNthParent
    , readFrom
    , findLatestValidBlock
    -- , setParentHeader
    , syncLatestHeader
    , getCheckpointer
    , exitOnRewindLimitExceeded
    , rewindToIncremental

    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader

import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import GHC.Stack

import qualified Pact.JSON.Encode as J

import Prelude hiding (lookup)

import Streaming
import qualified Streaming.Prelude as S

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.TreeDB (getBranchIncreasing, forkEntry, lookup, seekAncestor)
import Chainweb.Utils hiding (check)
import Chainweb.Version

import Chainweb.Storage.Table

exitOnRewindLimitExceeded :: PactServiceM logger tbl a -> PactServiceM logger tbl a
exitOnRewindLimitExceeded = handle $ \case
    e@RewindLimitExceeded{} -> do
        killFunction <- asks (\x -> _psOnFatalError x)
        liftIO $ killFunction e (J.encodeText $ msg e)
    e -> throwM e
  where
    msg e = J.object
        [ "details" J..= e
        , "message" J..= J.text "Your node is part of a losing fork longer than your\
            \ reorg-limit, which is a situation that requires manual\
            \ intervention.\
            \ For information on recovering from this, please consult:\
            \ https://github.com/kadena-io/chainweb-node/blob/master/\
            \ docs/RecoveringFromDeepForks.md"
        ]


-- | INTERNAL FUNCTION. USE 'withCheckpointer' instead.
--
-- TODO: The performance overhead is relatively low if there is no fork. We
-- should consider merging it with 'restoreCheckpointer' and always rewind.
--
-- Rewinds the pact state to the given parent in a single database transactions.
-- Rewinds to the genesis block if the parent is 'Nothing'.
--
-- If the rewind is deeper than the optionally provided rewind limit, an
-- exception is raised.
--
-- rewindTo
--     :: forall logger tbl
--     . (HasCallStack, CanReadablePayloadCas tbl, Logger logger)
--     => Maybe RewindLimit
--         -- ^ if set, limit rewinds to this delta
--     -> Maybe ParentHeader
--         -- ^ The parent header which is the rewind target
--     -> PactServiceM logger tbl ()
-- rewindTo = rewindTo' ReadWriteCheckpointer

readFromLatest
  :: Logger logger
  => PactBlockM logger tbl a
  -> PactServiceM logger tbl a
readFromLatest doRead = do
  latestBlockHeader <- syncLatestHeader
  readFrom (parentToParentContext latestBlockHeader) doRead

readFromNthParent
  :: Logger logger
  => Word
  -> PactBlockM logger tbl a
  -> PactServiceM logger tbl a
readFromNthParent n doRead = do
    bhdb <- view psBlockHeaderDb
    ParentHeader latest <- syncLatestHeader
    v <- view chainwebVersion
    cid <- view chainId
    nthParent <- liftIO $
        if _blockHeight latest <= fromIntegral n
        then return $ genesisParentContext v cid
        else seekAncestor bhdb latest (fromIntegral (_blockHeight latest - fromIntegral n)) >>= \case
            Nothing -> throwM $ PactInternalError
                $ "readFromNthParent: Failed to lookup nth ancestor, block " <> sshow latest
                <> ", depth " <> sshow n
            Just nthParentHeader ->
                return $ parentToParentContext $ ParentHeader nthParentHeader
    readFrom nthParent doRead

readFrom :: ParentContext -> PactBlockM logger tbl a -> PactServiceM logger tbl a
readFrom pc doRead = do
    cp <- getCheckpointer
    _cpReadFrom (_cpReadCp cp) pc $
        (\dbenv -> runPactBlockM pc dbenv doRead)

-- | Find the latest block stored in the checkpointer for which the respective
-- block header is available in the block header database. A block header may be in
-- the checkpointer but not in the block header database during deep catch-up
-- or reorgs. Why?
--
-- First the result of '_cpGetLatestBlock' is checked. If the respective block
-- header isn't available, the function recursively checks the result of
-- '_cpGetBlockParent'.
--
findLatestValidBlock :: (Logger logger) => PactServiceM logger tbl (Maybe BlockHeader)
findLatestValidBlock = getCheckpointer >>= liftIO . _cpGetLatestBlock . _cpReadCp >>= \case
    Nothing -> return Nothing
    Just (height, hash) -> go height hash
  where

    go height hash = do
        bhdb <- view psBlockHeaderDb
        liftIO (lookup bhdb hash) >>= \case
            Nothing -> do
                logInfo $ "Latest block isn't valid."
                    <> " Failed to lookup hash " <> sshow (height, hash) <> " in block header db."
                    <> " Continuing with parent."
                cp <- getCheckpointer
                liftIO (_cpGetBlockParent (_cpReadCp cp) (height, hash)) >>= \case
                    Nothing -> throwM $ PactInternalError
                        $ "missing block parent of last hash " <> sshow (height, hash)
                    Just predHash -> go (pred height) predHash
            x -> return x

syncLatestHeader :: (Logger logger) => PactServiceM logger tbl ParentHeader
syncLatestHeader = do
    findLatestValidBlock >>=
        maybe (throwM NoBlockValidatedYet) (return . ParentHeader)

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
rewindToIncremental
    :: forall logger tbl
    . (HasCallStack, CanReadablePayloadCas tbl, Logger logger)
    => Maybe RewindLimit
        -- ^ if set, limit rewinds to this delta
    -> ParentHeader
        -- ^ The parent header which is the rewind target
    -> PactServiceM logger tbl ()
rewindToIncremental rewindLimit (ParentHeader parent) = do

    lastHeader <- findLatestValidBlock >>= maybe failNonGenesisOnEmptyDb return
    logInfo $ "rewind from last to checkpointer target"
        <> ". last height: " <> sshow (_blockHeight lastHeader)
        <> "; last hash: " <> blockHashToText (_blockHash lastHeader)
        <> "; target height: " <> sshow parentHeight
        <> "; target hash: " <> blockHashToText parentHash

    failOnTooLowRequestedHeight parent rewindLimit lastHeader
    playFork lastHeader

  where
    parentHeight = _blockHeight parent
    parentHash = _blockHash parent

    failNonGenesisOnEmptyDb = error "impossible: playing non-genesis block to empty DB"

    playFork :: BlockHeader -> PactServiceM logger tbl ()
    playFork lastHeader = do
        bhdb <- asks _psBlockHeaderDb
        commonAncestor <- liftIO $ forkEntry bhdb lastHeader parent
        cp <- view psCheckpointer
        payloadDb <- view psPdb
        let ancestorHeight = _blockHeight commonAncestor

        logInfo $ "rewindTo.playFork"
            <> ": checkpointer is at height: " <> sshow (_blockHeight lastHeader)
            <> ", target height: " <> sshow (_blockHeight parent)
            <> ", common ancestor height " <> sshow ancestorHeight

        logger <- view psLogger

        -- 'getBranchIncreasing' expects an 'IO' callback because it
        -- maintains an 'TreeDB' iterator. 'withPactState' allows us to call
        -- pact service actions from the callback.

        (_ S.:> c) <- withPactState $ \runPact ->
            getBranchIncreasing bhdb parent (int ancestorHeight) $ \newBlocks -> do

                -- fastforwards all blocks in a chunk in a single database
                -- transactions (withBatchIO).
                let playChunk :: IORef BlockHeight -> BlockHeader -> Stream (Of BlockHeader) IO r -> IO (Of BlockHeader r)
                    playChunk heightRef cur s = do
                        (r, Last header) <- _cpRewindAndExtend
                            cp
                            (parentToParentContext $ ParentHeader cur)
                            s
                            $ \dbEnv pc bh -> do
                                payload <- tableLookup payloadDb (_blockPayloadHash bh) >>= \case
                                    Nothing -> throwM $ PactInternalError
                                        $ "Checkpointer.rewindTo.fastForward: lookup of payload failed"
                                        <> ". BlockPayloadHash: " <> encodeToText (_blockPayloadHash bh)
                                        <> ". Block: "<> encodeToText (ObjectEncoded bh)
                                    Just x -> return $ payloadWithOutputsToPayloadData x
                                writeIORef heightRef (_blockHeight bh)
                                void $ runPact $ runPactBlockM pc dbEnv $ execBlock bh payload
                                let pc' = parentToParentContext (ParentHeader bh)
                                return (Last (Just bh), pc')
                                -- double check output hash here?

                        return $ fromMaybe cur header :> r

                -- This stream is guaranteed to at least contain @e@.
                (curHdr, remaining) <- fromJuste <$> S.uncons newBlocks
                --
                -- we have to rewind to the current header to start.
                _cpRewindTo cp
                    (parentToParentContext $ ParentHeader curHdr)

                heightRef <- newIORef (_blockHeight curHdr)
                withAsync (heightProgress (_blockHeight curHdr) heightRef (logInfo_ logger)) $ \_ ->
                  remaining
                      & S.copy
                      & S.length_
                      & chunksOf 1000
                      & foldChunksM (playChunk heightRef) curHdr

        logInfo $ "rewindTo.playFork: replayed " <> sshow c <> " blocks"

-- -------------------------------------------------------------------------- --
-- Utils

heightProgress :: BlockHeight -> IORef BlockHeight -> (Text -> IO ()) -> IO ()
heightProgress initialHeight ref logFun = forever $ do
    h <- readIORef ref
    logFun
      $ "processed blocks: " <> sshow (h - initialHeight)
      <> ", current height: " <> sshow h
    threadDelay (20 * 1_000_000)

failOnTooLowRequestedHeight
    :: forall logger tbl
    . (HasCallStack, Logger logger)
    => CanReadablePayloadCas tbl
    => BlockHeader
    -> Maybe RewindLimit
    -> BlockHeader
    -> PactServiceM logger tbl ()
failOnTooLowRequestedHeight parent (Just limit) lastHeader
    | parentHeight + 1 + limitHeight < lastHeight = -- need to stick with addition because Word64
        throwM $ RewindLimitExceeded limit (Just lastHeader) (Just parent)
  where
    limitHeight = BlockHeight $ _rewindLimit limit
    parentHeight = _blockHeight parent
    lastHeight = _blockHeight lastHeader
failOnTooLowRequestedHeight _ _ _ = return ()
