{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Chainweb.Sync.ForkInfo
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Sync.ForkInfo
( resolveForkInfo
, resolveForkInfoForProviderState
) where

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Core.Brief
import Chainweb.Parent
import Chainweb.PayloadProvider
import Chainweb.Ranked
import Chainweb.Storage.Table
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version
import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Data.List qualified as L
import Data.LogMessage
import Data.Maybe
import Data.Text qualified as T
import Data.These (partitionHereThere)
import GHC.Generics (Generic)
import GHC.Stack
import P2P.TaskQueue (TaskException(TaskFailed))
import P2P.Utils
import Streaming.Prelude qualified as S
import System.LogLevel
import Control.Monad

-- -------------------------------------------------------------------------- --

newtype ForkInfoSyncFailure = ForkInfoSyncFailure T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception ForkInfoSyncFailure

-- -------------------------------------------------------------------------- --
-- Synchronize Payload Provider
--
-- 1. Submit initial ForkInfo
-- 2. If result is not as requested: let ph be the height of the payload provider state.
-- 3. Efficiently compute fork point (starting at min of ph and target height)
-- 4. Get chunk size prefix of branch from fork point to target block
-- 5. Submit new ForkInfo with that prefix
-- 6. If result height is smaller than fork point retry from start and eventually fail.
-- 7. If result is on prefix continue with next prefix chunk from result.
-- 8. If result is larger than fork point and not on branch compute new fork
--    point. If that forkpoint is on prefix continue with next prefix chunk form
--    forkpoint. Otherwise retry from start and eventually fail.
--
-- We do not include a new block context for chunked forkinfo resolutions.
-- Mining should be performed only on a fully caught up payload provider.

forkInfoChunkSize :: Int
forkInfoChunkSize = 1000

-- -------------------------------------------------------------------------- --

-- | Resolve a ForkInfo for an existing block header against a PayloadProvider.
--
-- Typically an initial forkInfo is either for a block that has been validated
-- before or for a single new block header. In the former case the trace is
-- typically empty, in the latter case it contains a single entry.
--
-- TODO: trace this operation!
-- TODO: check the size of trace and if needed take appropriate measures
--
resolveForkInfo
    :: HasCallStack
    => HasVersion
    => ReadableBlockHeaderCas hdrCas
    => PayloadProvider p
    => LogFunctionText
    -> BlockHeaderDb -- FIXME use RankedBlockHeaderDb
    -> hdrCas
    -> p
    -> Maybe Hints
    -> ForkInfo
    -> IO ()
resolveForkInfo logg bhdb candidateHdrs provider hints finfo = do

    logg Info $ "resolveForkInfo: starting resolution"
            <> "; target state: " <> brief (_forkInfoTargetState finfo)
            <> "; base payload: " <> brief (_forkInfoBasePayloadHash finfo)
            <> "; trace length: " <> sshow (length $ _forkInfoTrace finfo)
            <> "; payload requested: " <> sshow (isJust $ _forkInfoNewBlockCtx finfo)

    -- Attempt payload validation for the given ForkInfo
    --
    -- We assume that this forkInfo has a trace of "reasonable" size
    --
    r <- syncToBlock provider hints finfo `catch` \(e :: SomeException) -> do
        logg Warn $ "getBlockHeaderInternal payload validation for "
            <> toText h <> " failed with: " <> T.pack (displayException e)
        throwM e

    -- Check result of syncToBlock
    --
    if r == _forkInfoTargetState finfo

      -- We are done
      --
      then return ()

      -- We are not in sync and need to resolve (could be a fork a
      -- payload provider that is not caught up yet)
      --
      else do
        resolveForkInfoForProviderState logg bhdb candidateHdrs provider hints finfo (_consensusStateLatest r)
  where
    h = _latestRankedBlockHash . _forkInfoTargetState $ finfo

-- | Resolve a ForkInfo with respect to the current (or some past) state of the
-- payload provider.
--
-- If necessarily resolution is done iteratively in chunks. FIXME chunking
-- is not yet implemented.
--
-- The operation fails if no progress is made in a chunk.
--
resolveForkInfoForProviderState
    :: HasCallStack
    => HasVersion
    => ReadableBlockHeaderCas hdrCas
    => PayloadProvider p
    => LogFunctionText
    -> BlockHeaderDb -- FIXME use RankedBlockHeaderDb
    -> hdrCas
    -> p
    -> Maybe Hints
    -> ForkInfo
    -> SyncState
    -> IO ()
resolveForkInfoForProviderState logg bhdb candidateHdrs provider hints finfo ppKnownState
    | ppRBH == trgHash = do
        -- nothing to do, we are at the target
        logg Info "resolveForkInfo: payload provider is at target block"
        return ()
    | otherwise = do
        logg Info $ "resolveForkInfo: payload provider state: " <> brief ppKnownState
            <> "; target state: " <> brief (_forkInfoTargetState finfo)

        hdr :: BlockHeader <- runMaybeT
            (MaybeT (tableLookup candidateHdrs (_ranked trgHash))
                <|> MaybeT (lookupRanked bhdb (int $ _rankedHeight trgHash) (_ranked trgHash)))
                >>= \case
                    Nothing -> do
                        throwM $ InternalInvariantViolation $ "validatePayload: target block is missing: " <> brief trgHash
                    Just hdr -> return hdr

        -- Lookup the state of the Payload Provider and query the trace
        -- from the fork point to the target block.
        --
        -- This really should only happen if the payload provider db comes from an
        -- external source or the payload provider db is outdated and resides on a
        -- fork that got already pruned. In this case we can only guess where a
        -- common fork point might be -- which, in worst case, might be the
        -- genesis. We should either do an exponential search or just fail.
        --

        -- Before we do the potentially expensive branch diff call, we check
        -- whether the ppKnownState is in the trace of the finfo.
        -- TODO this could be done more efficiently, but for now it is fine.
        let idx = L.elemIndex ppRBH
                $ unwrapParent . _evaluationCtxRankedParentHash <$> _forkInfoTrace finfo

        newForkInfo <- case idx of
            Just i -> do
                logg Info $ "resolveForkInfo: found payload provider state in trace at index " <> sshow i
                return finfo
                    { _forkInfoTrace = drop (i + 1) (_forkInfoTrace finfo)
                    , _forkInfoBasePayloadHash = Parent $ _ranked $ _syncStateRankedBlockPayloadHash ppKnownState
                    }
            Nothing -> do
                logg Info "resolveForkInfo: payload provider state not in trace, computing fork point"
                ppBlock <- lookupRanked bhdb (int $ _rankedHeight ppRBH) (_ranked ppRBH) >>= \case
                    Nothing -> throwM $ InternalInvariantViolation $ "Payload provider block is missing: " <> brief ppRBH
                    Just ppBlock -> return ppBlock

                -- FIXME: this stream can be very long if the payload provider
                -- is out of sync. We should limit it and proceed iteratively if
                -- necessary. Ideally, we check the length based on block
                -- heights before we compute the full trace below. We also have
                -- to be careful, that we compute the fork point only once and
                -- iterate from there, otherwise the complexity would become
                -- quadratic.
                --
                -- Note that as long as no other CL is interfering with us we
                -- can proceed in chunks starting form the fork point. According
                -- to the payload provider protocol, the payload provider may
                -- process the ForkInfo only partially, in which case the result
                -- is our branch and the forkpoint is forwarded. If we find that
                -- the result is on a fork it means that there is interference.
                -- We may check whether we still made progress, in which case we
                -- are fine. If we find that we do not make progress we raise an
                -- error.

                (forkBlocksDescendingStream S.:> forkPoint) <- S.toList

                    -- Note that this assumes that the parent of hdr is in the
                    -- block header DB. With the current cut pipeline that is
                    -- always the case, because we never validate a header for
                    -- which the parent hasn't already been validated. In case
                    -- that this changes in the future we need to adjust the
                    -- branch diff to start from parent of the lowest processed
                    -- block. That block would certainly be on the canonical
                    -- chain, since we wouldn't validate block that aren't
                    -- extending the current head.
                    --
                    $ branchDiff_ bhdb ppBlock hdr

                let forkBlocksAscending = reverse
                        $ snd
                        $ partitionHereThere forkBlocksDescendingStream

                let l = length forkBlocksAscending
                logg Info $ "resolveForkInfo: fork point: " <> brief forkPoint
                    <> "; fork length: " <> sshow l
                    <> "; fork blocks: " <> brief forkBlocksAscending

                let newTrace = zipWith
                        (\prent child ->
                            ConsensusPayload (view blockPayloadHash child) Nothing <$
                                blockHeaderToEvaluationCtx (Parent prent)
                        )
                        (forkPoint : forkBlocksAscending)
                        forkBlocksAscending

                    -- If the original forkinfo trace contained extra payload data
                    -- we must include it into the new trace as well.
                    --
                    -- We can expect that the original trace is a suffix of the
                    -- new trace and we can replace the common part with the
                    -- original trace in order to preserve the extra data.
                    --
                    mergedTrace = go newTrace (_forkInfoTrace finfo)
                      where
                        go xs [] = xs
                        go [] _ = []
                        go (x:xs) (y:ys)
                            | _evaluationCtxRankedParentHash x == _evaluationCtxRankedParentHash y
                                = y:ys
                            | otherwise
                                = x : go xs (y:ys)

                unless (length mergedTrace == length newTrace) $
                    error "Invariant violation: merged trace is not of expected length"

                return finfo
                    { _forkInfoTrace = mergedTrace
                    , _forkInfoBasePayloadHash =
                        Parent (view blockPayloadHash forkPoint)
                    }

        -- FXIME: when we limit the trace we still need to adjust
        -- the target. We also need to make sure that we eventually
        -- reach the target. For that we should remember the
        -- remaining trace. After each chunk we concatenate the
        -- remaining trace form the chunk with the overall remaining
        -- trace and compute the next chunk.
        -- Also, do not request payload creation on non-final chunks.
        --
        -- let (nextChunk, remainingTrace) = splitAt forkInfoChunkSize forkBlocksAscending

        -- Retry payload validation with the new ForkInfo
        --
        newState <- syncToBlock provider hints newForkInfo `catch` \(e :: SomeException) -> do
            logg Warn $ "getBlockHeaderInternal payload validation retry for "
                <> brief trgHash <> " failed with: " <> renderException
                    [ Renderer $ \(TaskFailed es) -> encodeToText
                        $ renderExceptionJson [Renderer clientErrorValue] <$> es
                    ]
                    e
            throwM e

        -- check if we made progress
        --
        let delta :: Int = int (_rankedHeight (_latestRankedBlockHash newState))
                - int (_rankedHeight (_syncStateRankedBlockHash ppKnownState))

        -- TODO: when this function is incremental, we will manage this more correctly.
        if ppKnownState /= _consensusStateLatest newState
          then do
            logg Info $ "resolveForkInfo: made progress"
                <> "; delta: " <> sshow delta
                <> "; previous payload provider state: " <> brief ppKnownState
                <> "; new payload provider state: " <> brief newState
                <> "; target state: " <> brief (_forkInfoTargetState newForkInfo)
            -- continue.
            -- TODO compute the new fork info here.
            resolveForkInfoForProviderState logg bhdb candidateHdrs provider hints newForkInfo (_consensusStateLatest newState)
          else do
            logg Warn $ "resolveForkInfo: no progress"
                <> "; delta: " <> sshow delta
                <> "; previous payload provider state: " <> brief ppKnownState
                <> "; new payload provider state: " <> brief newState
                <> "; target state: " <> brief (_forkInfoTargetState newForkInfo)

            -- If this fails, there is no way for the payload provider to
            -- sync to the block without using the ordinary cut pipeline.
            -- so, we raise an exception.
            --
            throwM $ ForkInfoSyncFailure $ "unexpected result state"
                <> "; delta: " <> sshow delta
                <> "; previous payload provider state: " <> brief ppKnownState
                <> "; new payload provider state: " <> brief newState
                <> "; target state: " <> brief (_forkInfoTargetState newForkInfo)
  where
    trgHash = _latestRankedBlockHash . _forkInfoTargetState $ finfo
    ppRBH = _syncStateRankedBlockHash ppKnownState
