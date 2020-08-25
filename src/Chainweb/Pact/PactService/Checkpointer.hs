{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
    (

      -- * Checkpointer Batches
      --
      -- All usages of the checkpointer must be in the context of a checkpointer
      -- batch, which ensure proper finalization of checkpointer usage by pact serice
      -- calls.

      withBatch
    , withDiscardedBatch

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
    , withCheckpointerRewind
    , withCurrentCheckpointer
    , WithCheckpointerResult(..)

    -- * Low Level Pact Service Checkpointer Tools

    , withCheckpointerWithoutRewind
    , rewindTo
    , findLatestValidBlock
    , setParentHeader
    , syncParentHeader
    , getCheckpointer
    , exitOnRewindLimitExceeded

    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Aeson as A
import Data.Either
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.TreeDB (getBranchIncreasing, forkEntry, lookup, lookupM)
import Chainweb.Utils hiding (check)
import Data.CAS (casLookup)

-- | Support lifting bracket style continuations in 'IO' into 'PactServiceM' by
-- providing a function that allows unwrapping pact actions in IO while
-- threading through the pact service state.
--
-- /NOTE:/ This must not be used to access the pact service state from another
-- thread.
--
withPactState
    :: forall cas a b
    . PayloadCasLookup cas
    => ((PactServiceM cas a -> IO a) -> IO b)
    -> PactServiceM cas b
withPactState inner = bracket captureState releaseState $ \ref -> do
    e <- ask
    liftIO $ inner $ \act -> mask $ \umask -> do
        s <- readIORef ref
        T2 r s' <- umask $ runPactServiceM s e act
        writeIORef ref s'
        return r
  where
    captureState = liftIO . newIORef =<< get
    releaseState = liftIO . readIORef >=> put

exitOnRewindLimitExceeded :: PactServiceM cas a -> PactServiceM cas a
exitOnRewindLimitExceeded = handle $ \case
    e@RewindLimitExceeded{} -> do
        killFunction <- asks _psOnFatalError
        liftIO $ killFunction e (encodeToText $ msg e)
    e -> throwM e
  where
    msg e = A.object
        [ "details" A..= e
        , "message" A..= id @T.Text "Your node is part of a losing fork longer than your\
            \ reorg-limit, which is a situation that requires manual\
            \ intervention.\
            \ For information on recovering from this, please consult:\
            \ https://github.com/kadena-io/chainweb-node/blob/master/\
            \ docs/RecoveringFromDeepForks.md"
        ]


-- | When using the checkpointer for evaluating pact code the caller must
-- indicate whether the result of the evaluation should be persisted or
-- discarded.
--
data WithCheckpointerResult a
    = Discard !a
    | Save BlockHeader !a

-- | INTERNAL FUNCTION. USE WITH CAUTION!
--
-- Same as 'withCheckpointer' but doesn't rewinds the checkpointer state to
-- the provided target. Note that it still restores the state to the target.
--
-- In other words, this method can move the checkpointer back in time to a state
-- in the current history. It doesn't resolve forks or fast forwards the
-- checkpointer.
--
-- /Assumption:/
--
-- This function assumes that '_psParentHeader' has been updated to match the
-- latest block in the checkpointers. This is guaranteed to be the case after
-- calling any of 'rewindTo', 'syncParentHeader', 'withCheckPointerRewind',
-- 'withCheckPointerWithoutRewind', or 'withCurrentCheckpointer'.
--
-- /NOTE:/
--
-- In most use cases one needs to rewind the checkpointer first to the target
-- and function 'withCheckpointer' should be preferred.
--
-- Only use this function when
--
-- 1. you need the extra performance from skipping the call to 'rewindTo' and
-- 2. you know exactly what you are doing.
--
withCheckpointerWithoutRewind
    :: PayloadCasLookup cas
    => Maybe ParentHeader
        -- ^ block height and hash of the parent header
    -> String
        -- ^ Putative caller
    -> (PactDbEnv' -> PactServiceM cas (WithCheckpointerResult a))
    -> PactServiceM cas a
withCheckpointerWithoutRewind target caller act = do
    checkPointer <- getCheckpointer
    logInfo $ "restoring (with caller " <> caller <> ") " <> sshow target

    -- check requirement that this must be called within a batch
    unlessM (asks _psIsBatch) $
        error $ "Code invariant violation: withCheckpointerRewind called by " <> caller <> " outside of batch. Please report this as a bug."
    -- we allow exactly one nested call of 'withCheckpointer', which is used
    -- during fastforward in 'rewindTo'.
    unlessM ((<= 1) <$> asks _psCheckpointerDepth) $ do
        error $ "Code invariant violation: to many nested calls of withCheckpointerRewind. Please report this as a bug."

    case target of
        Just h -> setParentHeader (caller <> ".withCheckpointerWithoutRewind") h
        Nothing -> return ()

    local (over psCheckpointerDepth succ) $ mask $ \restore -> do
        cenv <- restore $ do
            r <- liftIO $! _cpRestore checkPointer checkpointerTarget
            return r

        try (restore (act cenv)) >>= \case
            Left !e -> discardTx checkPointer >> throwM @_ @SomeException e
            Right (Discard !result) -> discardTx checkPointer >> return result
            Right (Save header !result) -> saveTx checkPointer header >> return result
  where
    checkpointerTarget = case target of
        Nothing -> Nothing
        Just (ParentHeader h) -> Just (_blockHeight h + 1, _blockHash h)
            -- the second argument of _cpRestore expects the hash of the parent
            -- and the height of the parent plus one.

    discardTx checkPointer = liftIO $! _cpDiscard checkPointer

    saveTx checkPointer !header = do
        -- TODO: _cpSave is a complex call. If any thing in there throws
        -- an exception it would result in a pending tx.
        liftIO $! _cpSave checkPointer $ _blockHash header
        modify' $ set psStateValidated (Just header)
        setParentHeader (caller <> ".withCheckpointerWithoutRewind.saveTx") (ParentHeader header)

-- | 'withCheckpointer' but using the cached parent header for target.
--
withCurrentCheckpointer
    :: PayloadCasLookup cas
    => String
    -> (PactDbEnv' -> PactServiceM cas (WithCheckpointerResult a))
    -> PactServiceM cas a
withCurrentCheckpointer caller act = do
    ph <- syncParentHeader "withCurrentCheckpointer"
        -- discover the header for the latest block that is stored in the
        -- checkpointer.
    withCheckpointerRewind (Just 0) (Just ph) caller act

-- | Execute an action in the context of an @Block@ that is provided by the
-- checkpointer. The checkpointer is rewinded and restored to the state to the
-- provided target.
--
-- The result of the inner action indicates whether the resulting checkpointer
-- state should be discarded or saved.
--
-- If the inner action throws an exception the checkpointer state is discarded.
--
withCheckpointerRewind
    :: PayloadCasLookup cas
    => Maybe BlockHeight
        -- ^ if set, limit rewinds to this delta
    -> Maybe ParentHeader
        -- ^ The parent header to which the checkpointer is restored
        --
        -- 'Nothing' restores the checkpointer for evaluating the genesis block.
        --
    -> String
    -> (PactDbEnv' -> PactServiceM cas (WithCheckpointerResult a))
    -> PactServiceM cas a
withCheckpointerRewind rewindLimit p caller act = do
    rewindTo rewindLimit p
        -- This updates '_psParentHeader'
    withCheckpointerWithoutRewind p caller act

-- | Run a batch of checkpointer operations, possibly involving the evaluation
-- transactions accross several blocks using more than a single call of
-- 'withCheckPointerRewind' or 'withCurrentCheckpointer', and persist the final
-- state. In case of an failure, the checkpointer is reverted to the initial
-- state.
--
withBatch :: PactServiceM cas a -> PactServiceM cas a
withBatch act = do
    cp <- getCheckpointer
    local (set psIsBatch True) $ mask $ \r -> do
        liftIO $ _cpBeginCheckpointerBatch cp
        v <- r act `onException` (liftIO $ _cpDiscardCheckpointerBatch cp)
        liftIO $ _cpCommitCheckpointerBatch cp
        return v

-- | Run a batch of checkpointer operations, possibly involving the evaluation
-- transactions accross several blocks using more than a single call of
-- 'withCheckPointerRewind' or 'withCurrentCheckpointer', and discard the final
-- state at the end.
--
withDiscardedBatch :: PactServiceM cas a -> PactServiceM cas a
withDiscardedBatch act = do
    cp <- getCheckpointer
    local (set psIsBatch True) $ bracket_
        (liftIO $ _cpBeginCheckpointerBatch cp)
        (liftIO $ _cpDiscardCheckpointerBatch cp)
        act


-- | INTERNAL FUNCTION. USE 'withCheckpointer' instead.
--
-- TODO: The performance overhead is relatively low if there is no fork. We
-- should consider merging it with 'restoreCheckpointer' and always rewind.
--
-- Rewinds the pact state to @mb@.
--
-- If @mb@ is 'Nothing', it rewinds to the genesis block. If the rewind is
-- deeper than the optionally provided rewind limit, an exception is raised.
--
rewindTo
    :: forall cas . PayloadCasLookup cas
    => Maybe BlockHeight
        -- ^ if set, limit rewinds to this delta
    -> Maybe ParentHeader
        -- ^ The parent header which is the rewind target
    -> PactServiceM cas ()
rewindTo _ Nothing = return ()
rewindTo rewindLimit (Just (ParentHeader parent)) = do

    -- skip if the checkpointer is already at the target.
    (_, lastHash) <- getCheckpointer >>= liftIO . _cpGetLatestBlock >>= \case
        Nothing -> throwM NoBlockValidatedYet
        Just p -> return p

    if lastHash == parentHash
      then
        -- We want to guarantee that '_psParentHeader' is in sync with the
        -- latest block of the checkpointer at the end of and call to
        -- 'rewindTo'. In the @else@ branch this is taken care of by the call to
        -- 'withCheckPointerWithoutRewind'.
        setParentHeader "rewindTo" (ParentHeader parent)
      else do
        lastHeader <- findLatestValidBlock >>= maybe failNonGenesisOnEmptyDb return
        logInfo $ T.unpack $ "rewind from last to checkpointer target"
            <> ". last height: " <> sshow (_blockHeight lastHeader)
            <> "; last hash: " <> blockHashToText (_blockHash lastHeader)
            <> "; target height: " <> sshow parentHeight
            <> "; target hash: " <> blockHashToText parentHash

        failOnTooLowRequestedHeight rewindLimit lastHeader
        playFork lastHeader

  where
    parentHeight = _blockHeight parent
    parentHash = _blockHash parent

    failOnTooLowRequestedHeight (Just limit) lastHeader
        | parentHeight + 1 + limit < lastHeight = -- need to stick with addition because Word64
            throwM $ RewindLimitExceeded (int limit) parentHeight lastHeight parent
      where
        lastHeight = _blockHeight lastHeader
    failOnTooLowRequestedHeight _ _ = return ()

    failNonGenesisOnEmptyDb = error "impossible: playing non-genesis block to empty DB"

    playFork lastHeader = do
        bhdb <- asks _psBlockHeaderDb
        commonAncestor <- liftIO $ forkEntry bhdb lastHeader parent
        let ancestorHeight = _blockHeight commonAncestor

        if commonAncestor == parent
          then
            -- If no blocks got replayed the checkpointer isn't restored via
            -- 'fastForward'. So we do an empty 'withCheckPointerWithoutRewind'.
            withCheckpointerWithoutRewind (Just $ ParentHeader commonAncestor) "rewindTo" $ \_ ->
                return $! Save commonAncestor ()
          else do
            logInfo $ "rewindTo.playFork"
                <> ": checkpointer is at height: " <> sshow (_blockHeight lastHeader)
                <> ", target height: " <> sshow (_blockHeight parent)
                <> ", common ancestor height " <> sshow ancestorHeight

            -- 'getBranchIncreasing' expects an 'IO' callback because it maintains an 'TreeDB'
            -- iterator. 'withPactState' allows us to call pact service actions
            -- from the callback.
            c <- withPactState $ \runPact ->
                getBranchIncreasing bhdb parent (int ancestorHeight) $ \newBlocks -> do
                    -- This stream is guaranteed to at least contain @e@.
                    (h, s) <- fromJuste <$> S.uncons newBlocks
                    s
                        & S.scanM
                            (\(!p) (!c) -> runPact (fastForward (ParentHeader p, c)) >> return c)
                            (return h) -- initial parent
                            return
                        & S.length_
            logInfo $ "rewindTo.playFork: replayed " <> sshow c <> " blocks"

    -- This provides some progress feedback in case of long pact replays
    -- (e.g. when revalidating the pact history)
    progress block = do
        let h = _blockHeight block
        when (h `rem` 10000 == 0) $
            logInfo $ "rewindTo.fastForward: replay block at height " <> sshow (_blockHeight block)

    fastForward
        :: forall c
        . PayloadCasLookup c
        => (ParentHeader, BlockHeader)
        -> PactServiceM c ()
    fastForward (target, block) = do
        progress block
        payloadDb <- asks _psPdb
        let bpHash = _blockPayloadHash block

        -- This does a restore, i.e. it rewinds the checkpointer back in
        -- history, if needed.
        withCheckpointerWithoutRewind (Just target) "fastForward" $ \pdbenv -> do
            payload <- liftIO $ casLookup payloadDb bpHash >>= \case
                Nothing -> throwM $ PactInternalError
                    $ "Checkpointer.rewindTo.fastForward: lookup of payload failed"
                    <> ". BlockPayloadHash: " <> encodeToText bpHash
                    <> ". Block: "<> encodeToText (ObjectEncoded block)
                Just x -> return $ payloadWithOutputsToPayloadData x
            void $ execBlock block payload pdbenv
            return $! Save block ()
        -- double check output hash here?

-- | Find the latest block stored in the checkpointer for which the respective
-- block header is available in the block header database.
--
-- NOTE: the function doesn't take into consideration the block header stored in
-- '_psParentHeader' in 'PactServiceM'. It is meant to be used during re-orgs
-- (recovering from forks).
--
-- First the result of '_cpGetLatestBlock' is checked. If the respective block
-- header isn't available, the function recursively checks the result of
-- '_cpGetBlockParent'.
--
findLatestValidBlock :: PactServiceM cas (Maybe BlockHeader)
findLatestValidBlock = getCheckpointer >>= liftIO . _cpGetLatestBlock >>= \case
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
                liftIO (_cpGetBlockParent cp (height, hash)) >>= \case
                    Nothing -> throwM $ PactInternalError
                        $ "missing block parent of last hash " <> sshow (height, hash)
                    Just predHash -> go (pred height) predHash
            x -> return x



-- | Synchronizes the parent header with the latest block of the checkpointer.
--
-- Ideally, this function would not be needed. It
--
-- 1. does a best effort to recover from a situation where '_cpGetLatestBlock'
--    '_psParentHeader' got out of sync, and
-- 2. provides visibility into the state of that invariant via the info log
--    message.
--
-- This call would fail if the latest block that is stored in the checkpointer
-- got orphaned and the '_psParentHeader' got reset to some other block
-- /without/ rewinding the checkpointer, too. That must not be possible. Hence,
-- the result of '_cpGetLatestBlock' and '_psParentHeader' must be kept in sync.
--
-- The previously described scenario /is/ possible if the service gets restarted
-- and the latest block got orphaned after the service stopped. This is
-- prevented by rewinding on pact service startup to the latest available header
-- in the block header db.
--
syncParentHeader :: String -> PactServiceM cas ParentHeader
syncParentHeader caller = do
    cp <- getCheckpointer
    liftIO (_cpGetLatestBlock cp) >>= \case
        Nothing -> throwM NoBlockValidatedYet
        Just (h, ph) -> do
            cur <- _parentHeader <$> use psParentHeader
            unless (_blockHash cur == ph) $
                logInfo $ T.unpack
                    $ T.pack caller <> ".syncParentHeader"
                    <> "; current hash: " <> blockHashToText (_blockHash cur)
                    <> "; current height: " <>  sshow (_blockHeight cur)
                    <> "; checkpointer hash: " <> blockHashToText ph
                    <> "; checkpointer height: " <>  sshow h

            parent <- ParentHeader
                <$!> lookupBlockHeader ph (T.pack caller <> ".syncParentHeader")
            setParentHeader (caller <> ".syncParentHeader") parent
            return parent

-- | Lookup a block header.
--
-- The block header is expected to be either in the block header database or to
-- be the the currently stored '_psParentHeader'. The latter addresses the case
-- when a block has already been validate with 'execValidateBlock' but isn't (yet)
-- available in the block header database. If that's the case two things can
-- happen:
--
-- 1. the header becomes available before the next 'execValidateBlock' call, or
-- 2. the header gets orphaned and the next 'execValidateBlock' call would cause
--    a rewind to an ancestor, which is available in the db.
--
lookupBlockHeader :: BlockHash -> Text -> PactServiceM cas BlockHeader
lookupBlockHeader bhash ctx = do
    ParentHeader cur <- use psParentHeader
    if (bhash == _blockHash cur)
      then return cur
      else do
        bhdb <- asks _psBlockHeaderDb
        liftIO $! lookupM bhdb bhash `catchAllSynchronous` \e ->
            throwM $ BlockHeaderLookupFailure $
                "failed lookup of parent header in " <> ctx <> ": " <> sshow e
