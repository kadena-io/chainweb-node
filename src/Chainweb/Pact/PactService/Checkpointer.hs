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
    ( readFromLatest
    , readFromNthParent
    , readFrom
    , restoreAndSave
    , findLatestValidBlockHeader'
    , findLatestValidBlockHeader
    , exitOnRewindLimitExceeded
    , rewindToIncremental
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import GHC.Stack

import qualified Pact.JSON.Encode as J

import Prelude hiding (lookup)

import Streaming (Stream, Of(..))
import qualified Streaming as S
import qualified Streaming.Prelude as S

-- internal modules

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
readFromLatest doRead = readFromNthParent 0 doRead

-- read-only rewind to the nth parent before the latest block.
-- note: this function will never rewind before genesis.
readFromNthParent
  :: forall logger tbl a
  . Logger logger
  => Word
  -> PactBlockM logger tbl a
  -> PactServiceM logger tbl a
readFromNthParent n doRead = go 0
    where
    go :: Int -> PactServiceM logger tbl a
    go retryCount = do
        bhdb <- view psBlockHeaderDb
        ParentHeader latest <- findLatestValidBlockHeader
        v <- view chainwebVersion
        cid <- view chainId
        let parentHeight =
                -- guarantees that the subtraction doesn't overflow
                -- this will never give us before genesis
                fromIntegral $ max (genesisHeight v cid + fromIntegral n) (view blockHeight latest) - fromIntegral n
        nthParent <- liftIO $
            seekAncestor bhdb latest parentHeight >>= \case
                Nothing -> internalError
                    $ "readFromNthParent: Failed to lookup nth ancestor, block " <> sshow latest
                    <> ", depth " <> sshow n
                Just nthParentHeader ->
                    return $ ParentHeader nthParentHeader
        readFrom (Just nthParent) doRead >>= \case
          -- note: because there is a race between getting the nth header
          -- and doing the rewind, there's a chance that the nth header
          -- will be unavailable when we do the rewind. in that case
          -- we just keep grabbing the new "nth header" until we succeed.
            NoHistory
                | retryCount < 10 ->
                    go (retryCount + 1)
                | otherwise -> internalError "readFromNthParent: failed after 10 retries"
            Historical r -> return r

-- read-only rewind to a target block.
-- if that target block is missing, return Nothing.
readFrom
    :: Logger logger
    => Maybe ParentHeader -> PactBlockM logger tbl a -> PactServiceM logger tbl (Historical a)
readFrom ph doRead = do
    cp <- view psCheckpointer
    pactParent <- getPactParent ph
    s <- get
    e <- ask
    liftIO $ _cpReadFrom (_cpReadCp cp) ph $ \dbenv ->
      evalPactServiceM s e $ runPactBlockM pactParent dbenv doRead

-- here we cheat, making the genesis block header's parent the genesis
-- block header, only for Pact's information, *not* for the checkpointer;
-- the checkpointer knows the difference between rewinding to before the
-- genesis block and rewinding to just after the genesis block.
getPactParent :: Maybe ParentHeader -> PactServiceM logger tbl ParentHeader
getPactParent ph = do
    case ph of
        Nothing -> do
            bh <- genesisBlockHeader <$> view chainwebVersion <*> view chainId
            return (ParentHeader bh)
        Just h -> return h

-- play multiple blocks starting at a given parent header.
restoreAndSave
  :: (CanReadablePayloadCas tbl, Logger logger, Monoid q)
  => Maybe ParentHeader
  -> Stream (Of (PactBlockM logger tbl (q, BlockHeader))) IO r
  -> PactServiceM logger tbl (r, q)
restoreAndSave ph blocks = do
    cp <- view psCheckpointer
    withPactState $ \runPact ->
        _cpRestoreAndSave cp ph
            (blocks & S.map (\block -> RunnableBlock $ \dbEnv mph -> runPact $ do
                pactParent <- getPactParent mph
                runPactBlockM pactParent dbEnv block
            ))

-- | Find the latest block stored in the checkpointer for which the respective
-- block header is available in the block header database. A block header may be in
-- the checkpointer but not in the block header database during deep catch-up
-- or reorgs. Why? It appears to be a race, where a thread's insert into rocksdb
-- may not be seen by another thread immediately.
--
-- First the result of '_cpGetLatestBlock' is checked. If the respective block
-- header isn't available, the function recursively checks the result of
-- '_cpGetBlockParent'.
--
findLatestValidBlockHeader' :: (Logger logger) => PactServiceM logger tbl (Maybe BlockHeader)
findLatestValidBlockHeader' = do
    cp <- view psCheckpointer
    latestInCheckpointer <- liftIO (_cpGetLatestBlock (_cpReadCp cp))
    case latestInCheckpointer of
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
                cp <- view psCheckpointer
                liftIO (_cpGetBlockParent (_cpReadCp cp) (height, hash)) >>= \case
                    Nothing -> internalError
                        $ "missing block parent of last hash " <> sshow (height, hash)
                    Just predHash -> go (pred height) predHash
            x -> return x

findLatestValidBlockHeader :: (Logger logger) => PactServiceM logger tbl ParentHeader
findLatestValidBlockHeader = do
    findLatestValidBlockHeader' >>=
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

    latestHeader <- findLatestValidBlockHeader' >>= maybe failNonGenesisOnEmptyDb return

    failOnTooLowRequestedHeight latestHeader
    playFork latestHeader

  where
    parentHeight = view blockHeight parent


    failOnTooLowRequestedHeight lastHeader = case rewindLimit of
        Just limit
            | let limitHeight = BlockHeight $ _rewindLimit limit
            , parentHeight + 1 + limitHeight < lastHeight -> -- need to stick with addition because Word64
                throwM $ RewindLimitExceeded limit (Just lastHeader) (Just parent)
        _ -> return ()
      where
        lastHeight = view blockHeight lastHeader

    failNonGenesisOnEmptyDb = error "impossible: playing non-genesis block to empty DB"

    playFork :: BlockHeader -> PactServiceM logger tbl ()
    playFork lastHeader = do
        bhdb <- asks _psBlockHeaderDb
        commonAncestor <- liftIO $ forkEntry bhdb lastHeader parent
        cp <- view psCheckpointer
        payloadDb <- view psPdb
        let ancestorHeight = view blockHeight commonAncestor

        logger <- view psLogger

        -- 'getBranchIncreasing' expects an 'IO' callback because it
        -- maintains an 'TreeDB' iterator. 'withPactState' allows us to call
        -- pact service actions from the callback.

        (_ :> c) <- withPactState $ \runPact ->
            getBranchIncreasing bhdb parent (int ancestorHeight) $ \newBlocks -> do

                -- fastforwards all blocks in a chunk in a single database
                -- transaction.
                -- invariant: when playing a chunk, the checkpointer must
                -- already be at `cur`.
                let playChunk :: IORef BlockHeight -> BlockHeader -> Stream (Of BlockHeader) IO r -> IO (Of BlockHeader r)
                    playChunk heightRef cur blockChunk = runPact $ do
                        (r, Last header) <- restoreAndSave
                            (Just $ ParentHeader cur)
                            $ blockChunk & S.map
                            (\blockHeader -> do

                                payload <- liftIO $ lookupPayloadWithHeight payloadDb (Just $ view blockHeight blockHeader) (view blockPayloadHash blockHeader) >>= \case
                                    Nothing -> internalError
                                        $ "Checkpointer.rewindTo.fastForward: lookup of payload failed"
                                        <> ". BlockPayloadHash: " <> encodeToText (view blockPayloadHash blockHeader)
                                        <> ". Block: "<> encodeToText (ObjectEncoded blockHeader)
                                    Just x -> return $ payloadWithOutputsToPayloadData x
                                liftIO $ writeIORef heightRef (view blockHeight blockHeader)
                                void $ execBlock blockHeader (CheckablePayload payload)
                                return (Last (Just blockHeader), blockHeader)
                                -- double check output hash here?
                            )

                        return $! fromJuste header :> r

                -- This stream is guaranteed to at least contain @e@.
                (curHdr, remaining) <- fromJuste <$> S.uncons newBlocks

                -- we have to rewind to the current header to start, for
                -- playChunk's invariant to be satisfied
                _cpRewindTo cp
                    (Just $ ParentHeader curHdr)

                heightRef <- newIORef (view blockHeight curHdr)
                withAsync (heightProgress (view blockHeight curHdr) heightRef (logInfo_ logger)) $ \_ ->
                  remaining
                      & S.copy
                      & S.length_
                      & S.chunksOf 1000
                      & foldChunksM (playChunk heightRef) curHdr

        when (c /= 0) $
            logInfo $ "rewindTo.playFork: replayed " <> sshow c <> " blocks"

-- -------------------------------------------------------------------------- --
-- Utils

heightProgress :: BlockHeight -> IORef BlockHeight -> (Text -> IO ()) -> IO ()
heightProgress initialHeight ref logFun = forever $ do
    threadDelay (20 * 1_000_000)
    h <- readIORef ref
    logFun
      $ "processed blocks: " <> sshow (h - initialHeight)
      <> ", current height: " <> sshow h
