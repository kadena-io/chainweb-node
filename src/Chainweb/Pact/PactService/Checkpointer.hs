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
    , SomeBlockM(..)
    , getEarliestBlock
    , getLatestBlock
    , lookupHistorical
    , getBlockHistory
    , Internal.withCheckpointerResources
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor.Product
import Data.IORef
import Data.Maybe
import Data.Monoid hiding (Product(..))
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

import qualified Chainweb.Pact.PactService.Pact4.ExecBlock as Pact4
import qualified Chainweb.Pact.PactService.Pact5.ExecBlock as Pact5
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.TreeDB (getBranchIncreasing, forkEntry, lookup, seekAncestor)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import qualified Chainweb.Pact4.Types as Pact4
import qualified Chainweb.Pact5.Types as Pact5
import Chainweb.Version.Guards (pact5)
import qualified Chainweb.Pact.PactService.Checkpointer.Internal as Internal
import Chainweb.BlockHash
import qualified Pact.Core.Names as Pact5
import qualified Pact.Core.Persistence.Types as Pact5
import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Evaluate as Pact5

exitOnRewindLimitExceeded :: PactServiceM logger tbl a -> PactServiceM logger tbl a
exitOnRewindLimitExceeded = handle $ \case
    e@RewindLimitExceeded{..} -> do
        killFunction <- asks (\x -> _psOnFatalError x)
        liftIO $ killFunction e (J.encodeText $ msg _rewindExceededLimit _rewindExceededLast _rewindExceededTarget)
    e -> throwM e
    where
    msg (RewindLimit limit) lastHeader targetHeader = J.object
        [ "details" J..= J.object
            [ "limit" J..= J.number (fromIntegral limit)
            , "last" J..= fmap (J.text . blockHeaderShortDescription) lastHeader
            , "target" J..= fmap (J.text . blockHeaderShortDescription) targetHeader
            ]
        , "message" J..= J.text "Your node is part of a losing fork longer than your\
            \ reorg-limit, which is a situation that requires manual\
            \ intervention.\
            \ For information on recovering from this, please consult:\
            \ https://github.com/kadena-io/chainweb-node/blob/master/\
            \ docs/RecoveringFromDeepForks.md"
        ]

newtype SomeBlockM logger tbl a = SomeBlockM (Product (Pact4.PactBlockM logger tbl) (Pact5.PactBlockM logger tbl) a)
    deriving newtype (Functor, Applicative, Monad)
instance MonadIO (SomeBlockM logger tbl) where
    liftIO a = SomeBlockM $ Pair (liftIO a) (liftIO a)

-- read-only rewind to the latest block.
-- note: because there is a race between getting the latest header
-- and doing the rewind, there's a chance that the latest header
-- will be unavailable when we do the rewind. in that case
-- we just keep grabbing the new "latest header" until we succeed.
-- note: this function will never rewind before genesis.
readFromLatest
    :: Logger logger
    => SomeBlockM logger tbl a
    -> PactServiceM logger tbl a
readFromLatest doRead = readFromNthParent 0 doRead

-- read-only rewind to the nth parent before the latest block.
-- note: this function will never rewind before genesis.
readFromNthParent
    :: forall logger tbl a
    . Logger logger
    => Word
    -> SomeBlockM logger tbl a
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
    => Maybe ParentHeader
    -> SomeBlockM logger tbl a
    -> PactServiceM logger tbl (Historical a)
readFrom ph doRead = do
    cp <- view psCheckpointer
    pactParent <- getPactParent ph
    let bh = _parentHeader <$> ph
    s <- get
    e <- ask
    v <- view chainwebVersion
    cid <- view chainId
    let currentHeight = maybe (genesisHeight v cid) (succ . view blockHeight) bh
    let execPact4 act =
            liftIO $ Internal.readFrom cp ph Pact4T $ \dbenv _ ->
                evalPactServiceM s e $
                    Pact4.runPactBlockM pactParent (isNothing ph) dbenv act
    let execPact5 act =
            liftIO $ Internal.readFrom cp ph Pact5T $ \dbenv blockHandle ->
                evalPactServiceM s e $ do
                    fst <$> Pact5.runPactBlockM pactParent (isNothing ph) dbenv blockHandle act
    case doRead of
        SomeBlockM (Pair forPact4 forPact5)
            | pact5 v cid currentHeight -> execPact5 forPact5
            | otherwise -> execPact4 forPact4

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
    -> Stream (Of (SomeBlockM logger tbl (q, BlockHeader))) IO r
    -> PactServiceM logger tbl (r, q)
restoreAndSave ph blocks = do
    cp <- view psCheckpointer
    v <- view chainwebVersion
    cid <- view chainId
    -- the height of the first block in the stream
    let firstBlockHeight = case ph of
            Nothing -> genesisHeight v cid
            Just (ParentHeader bh) -> succ (bh ^. blockHeight)
    withPactState $ \runPact ->
        Internal.restoreAndSave cp ph
            $ blocks & S.zip (S.iterate succ firstBlockHeight) & S.map
                (\case
                    (height, SomeBlockM (Pair pact4Block pact5Block))
                        | pact5 v cid height ->
                            Pact5RunnableBlock $ \dbEnv mph blockHandle -> runPact $ do
                                pactParent <- getPactParent mph
                                let isGenesis = isNothing mph
                                Pact5.runPactBlockM pactParent isGenesis dbEnv blockHandle pact5Block
                        | otherwise ->
                            Pact4RunnableBlock $ \dbEnv mph -> runPact $ do
                                pactParent <- getPactParent mph
                                let isGenesis = isNothing mph
                                Pact4.runPactBlockM pactParent isGenesis dbEnv pact4Block
                )

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
    latestInCheckpointer <- liftIO (Internal.getLatestBlock cp.cpSql)
    case latestInCheckpointer of
        Nothing -> return Nothing
        Just (height, hash) -> Just <$> go height hash
    where
    go height hash = do
        bhdb <- view psBlockHeaderDb
        liftIO (lookup bhdb hash) >>= \case
            Nothing -> do
                logInfoPact $ "Latest block isn't valid."
                    <> " Failed to lookup hash " <> sshow (height, hash) <> " in block header db."
                    <> " Continuing with parent."
                cp <- view psCheckpointer
                liftIO (Internal.getBlockParent cp.cpCwVersion cp.cpChainId cp.cpSql (height, hash)) >>= \case
                    Nothing -> internalError
                        $ "missing block parent of last hash " <> sshow (height, hash)
                    Just predHash -> go (pred height) predHash
            Just x -> return x

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
                                SomeBlockM $ Pair
                                    (void $ Pact4.execBlock blockHeader (CheckablePayload payload))
                                    (void $ Pact5.execExistingBlock blockHeader (CheckablePayload payload))
                                return (Last (Just blockHeader), blockHeader)
                            )

                        return $! fromJuste header :> r

                -- This stream is guaranteed to at least contain @e@.
                (curHdr, remaining) <- fromJuste <$> S.uncons newBlocks

                -- we have to rewind to the current header to start, for
                -- playChunk's invariant to be satisfied
                Internal.rewindTo cp
                    (Just $ ParentHeader curHdr)

                heightRef <- newIORef (view blockHeight curHdr)
                withAsync (heightProgress (view blockHeight curHdr) heightRef (logInfo_ logger)) $ \_ ->
                    remaining
                        & S.copy
                        & S.length_
                        & S.chunksOf 1000
                        & foldChunksM (playChunk heightRef) curHdr

        when (c /= 0) $
            logInfoPact $ "rewindTo.playFork: replayed " <> sshow c <> " blocks"

getEarliestBlock :: PactServiceM logger tbl (Maybe (BlockHeight, BlockHash))
getEarliestBlock = do
    cp <- view psCheckpointer
    liftIO $ Internal.getEarliestBlock cp.cpSql

getLatestBlock :: PactServiceM logger tbl (Maybe (BlockHeight, BlockHash))
getLatestBlock = do
    cp <- view psCheckpointer
    liftIO $ Internal.getLatestBlock cp.cpSql

lookupHistorical
    :: BlockHeader
    -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
    -> Pact5.RowKey
    -> PactServiceM logger tbl (Historical (Maybe (Pact5.TxLog Pact5.RowData)))
lookupHistorical blockHeader d k = do
    cp <- view psCheckpointer
    liftIO $ Internal.lookupHistorical cp.cpSql blockHeader d k

getBlockHistory
    :: BlockHeader
    -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
    -> PactServiceM logger tbl (Historical BlockTxHistory)
getBlockHistory blockHeader d = do
    cp <- view psCheckpointer
    liftIO $ Internal.getBlockHistory cp.cpSql blockHeader d

-- -------------------------------------------------------------------------- --
-- Utils

heightProgress :: BlockHeight -> IORef BlockHeight -> (Text -> IO ()) -> IO ()
heightProgress initialHeight ref logFun = forever $ do
    threadDelay (20 * 1_000_000)
    h <- readIORef ref
    logFun
        $ "processed blocks: " <> sshow (h - initialHeight)
        <> ", current height: " <> sshow h
