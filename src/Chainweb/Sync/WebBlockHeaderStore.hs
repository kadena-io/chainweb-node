{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Chainweb.Sync.WebBlockHeaderStore
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A handle that provides tools obtaining and validating block headers within
-- the Chainweb network and making them available in the local database.
--
module Chainweb.Sync.WebBlockHeaderStore
( WebBlockHeaderStore(..)
, newWebBlockHeaderStore
, getBlockHeader
, forkInfoForHeader

-- *
, WebBlockPayloadStore(..)
-- , newEmptyWebPayloadStore
, newWebPayloadStore

-- * Utils
, memoInsert
) where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RemoteDB
import Chainweb.ChainId
import Chainweb.ChainValue
import Chainweb.Difficulty (WindowWidth(..))
import Chainweb.MinerReward (blockMinerReward)
import Chainweb.Pact.Payload
import Chainweb.Pact.Payload.PayloadStore
import Chainweb.PayloadProvider
import Chainweb.Ranked
import Chainweb.Storage.Table
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.TreeDB qualified as TDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils (diameterAt)
import Chainweb.WebBlockHeaderDB
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Foldable
import Data.Hashable
import Data.LogMessage
import Data.PQueue
import Data.TaskMap
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Stack
import Network.HTTP.Client qualified as HTTP
import P2P.Peer
import P2P.TaskQueue
import Servant.Client
import System.LogLevel
import Utils.Logging.Trace
import Chainweb.Parent
import Streaming.Prelude qualified as S
import Data.These (partitionHereThere, These (..))
import Chainweb.Core.Brief

-- -------------------------------------------------------------------------- --
-- Response Timeout Constants

-- | For P2P queries we lower the response timeout to 500ms. This is tight
-- but guarantees that the task isn't blocked for too long and retries
-- another node quickly in case the first node isn't available. If no node
-- in the network is able to serve that quickly the local node is out of
-- luck. However, 500ms should be sufficient for a well-connected node
-- anywhere on the world. Otherwise, the node would have trouble anyways.
--
taskResponseTimeout :: HTTP.ResponseTimeout
taskResponseTimeout = HTTP.responseTimeoutMicro 500_000

pullOriginResponseTimeout :: HTTP.ResponseTimeout
pullOriginResponseTimeout = HTTP.responseTimeoutMicro 1_000_000

-- | Set the response timeout on all requests made with the ClientEnv This
-- overwrites the default response timeout of the connection manager.
--
setResponseTimeout :: HTTP.ResponseTimeout -> ClientEnv -> ClientEnv
setResponseTimeout t env =  env
    { makeClientRequest = \u r -> defaultMakeClientRequest u r <&> \req -> req
        { HTTP.responseTimeout = t
        }
    }

-- -------------------------------------------------------------------------- --
-- Append Only CAS for WebBlockHeaderDb

newtype WebBlockHeaderCas = WebBlockHeaderCas WebBlockHeaderDb

-- -------------------------------------------------------------------------- --
-- Obtain and Validate Block Payloads

data WebBlockPayloadStore tbl = WebBlockPayloadStore
    { _webBlockPayloadStoreCas :: !(PayloadDb tbl)
        -- ^ Cas for storing complete payload data including outputs.
    , _webBlockPayloadStoreMemo :: !(TaskMap BlockPayloadHash PayloadData)
        -- ^ Internal memo table for active tasks
    , _webBlockPayloadStoreQueue :: !(PQueue (Task ClientEnv PayloadData))
        -- ^ task queue for scheduling tasks with the task server
    , _webBlockPayloadStoreLogFunction :: !LogFunction
        -- ^ LogFunction
    , _webBlockPayloadStoreMgr :: !HTTP.Manager
        -- ^ Manager object for making HTTP requests
    }

-- -------------------------------------------------------------------------- --
-- WebBlockHeaderStore

-- | In order to use this a processor for the queue is needed.
--
-- The module P2P.TaskQueue provides a P2P session that serves the queue.
--
-- TODO
--
-- * Find a better name
-- * Parameterize in cas
-- * This is currently based on TreeDB (for API) and BlockHeaderDB, it
--   would be possible to run this on top of any CAS and API that offers
--   a simple GET.
--
data WebBlockHeaderStore = WebBlockHeaderStore
    { _webBlockHeaderStoreCas :: !WebBlockHeaderDb
    , _webBlockHeaderStoreMemo :: !(TaskMap (ChainValue BlockHash) (ChainValue BlockHeader))
    , _webBlockHeaderStoreQueue :: !(PQueue (Task ClientEnv (ChainValue BlockHeader)))
    , _webBlockHeaderStoreLogFunction :: !LogFunction
    , _webBlockHeaderStoreMgr :: !HTTP.Manager
    }

-- -------------------------------------------------------------------------- --
-- Overlay CAS with asynchronous weak HashMap

memoInsert
    :: (Table t (CasKeyType v) v, IsCasValue v)
    => Hashable (CasKeyType v)
    => t
    -> TaskMap (CasKeyType v) v
    -> CasKeyType v
    -> (CasKeyType v -> IO v)
    -> IO v
memoInsert cas m k a = tableLookup cas k >>= \case
    Nothing -> memo m k $ \k' -> do
        -- there is the chance of a race here. At this time some task may just
        -- have finished updating the CAS with the key we are looking for. We
        -- could solve this by doing a another CAS lookup here. But, depending
        -- on the CAS, that could be expensive, too. For now we except a few
        -- duplicate tasks due to races instead of adding an extra CAS lookup to
        -- every task.
        !v <- a k'
        casInsert cas v
        return v
    (Just !x) -> return x

-- | For a given latest BlockHeader return the state of consensus
--
-- THIS IS NOT FINAL!
--
-- TODO:
-- This should eventually become a consensus component on its own. It probably
-- needs to depend on the cut db (and it is not clear how that would fit in this
-- module).
--
-- It should take into consideration the current hash power, weight, and
-- difficulty. It should probably also take into consideration historical hash
-- power, like the all time max.
--
-- For instance, the confirmation depth should approach infinite as the honest
-- hash power degrades towards 50% (of the all time max).
--
-- For now we use
--
-- - safe: 6 times of the graph diameter block heights, ~ 9 min
-- - final: 4 epochs, 120 * 4 block heights, ~ 4 hours
--
consensusState
    :: HasCallStack
    => HasVersion
    => WebBlockHeaderDb
    -> BlockHeader
    -> IO ConsensusState
consensusState wdb hdr
    | isGenesisBlockHeader hdr = return ConsensusState
        { _consensusStateLatest = syncStateOfBlockHeader hdr
        , _consensusStateSafe = syncStateOfBlockHeader hdr
        , _consensusStateFinal = syncStateOfBlockHeader hdr
        }
    | otherwise = do
        db <- getWebBlockHeaderDb wdb hdr
        Parent phdr <- lookupParentHeader wdb hdr
        safeHdr <- fromJuste <$> seekAncestor db phdr safeHeight
        finalHdr <- fromJuste <$> seekAncestor db phdr finalHeight
        return ConsensusState
            { _consensusStateLatest = syncStateOfBlockHeader hdr
            , _consensusStateSafe = syncStateOfBlockHeader safeHdr
            , _consensusStateFinal = syncStateOfBlockHeader finalHdr
            }
  where
    WindowWidth w = _versionWindow implicitVersion
    cid = _chainId hdr
    finalHeight = int @Int @_ $ max (int $ genesisHeight cid) (int height - int w * 4)
    safeHeight = int @Int @_ $ max (int $ genesisHeight cid) (int height - 6 * (int diam + 1))
    height = view blockHeight hdr
    diam = diameterAt height

-- -------------------------------------------------------------------------- --
-- ForkInfo For Header

-- | Compute ForkInfo Object for a single newly added BlockHeader
--
forkInfoForHeader
    :: HasVersion
    => WebBlockHeaderDb
    -> BlockHeader
    -> Maybe EncodedPayloadData
    -> Maybe (Parent BlockHeader)
    -> IO ForkInfo
forkInfoForHeader wdb hdr pldData parentHdr

    -- FIXME do we need this case??? We never add genesis headers...
    | isGenesisBlockHeader hdr = do
        state <- consensusState wdb hdr
        return $ ForkInfo
            { _forkInfoTrace = []
            , _forkInfoBasePayloadHash = Parent $ _latestPayloadHash state
            , _forkInfoTargetState = state
            , _forkInfoNewBlockCtx = Just nbctx
            }

    | otherwise = do
        phdr <- maybe (lookupParentHeader wdb hdr) return parentHdr
        let consensusPayload = ConsensusPayload
                { _consensusPayloadHash = pld
                , _consensusPayloadData = pldData
                }

        -- TargetState
        state <- consensusState wdb hdr
        return $ ForkInfo
            { _forkInfoTrace = [consensusPayload <$ blockHeaderToEvaluationCtx phdr]
            , _forkInfoBasePayloadHash = view blockPayloadHash <$> phdr
            , _forkInfoTargetState = state
            , _forkInfoNewBlockCtx = Just nbctx
            }
  where
    pld = view blockPayloadHash hdr

    nbctx = NewBlockCtx
        { _newBlockCtxMinerReward = blockMinerReward (height + 1)
        , _newBlockCtxParentCreationTime = Parent $ view blockCreationTime hdr
        }
    height = view blockHeight hdr

-- -------------------------------------------------------------------------- --
-- Obtain, Validate, and Store BlockHeaders

newtype GetBlockHeaderFailure = GetBlockHeaderFailure T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception GetBlockHeaderFailure

-- | Run an action to obtain and validate a BlockHeader along with all of it's
-- dependencies. Dependencies are computed asynchronously. Asynchronous
-- computations are memoized and shared. The results are stored in the provided
-- CAS storage.
--
-- NOTE: This fetches all prerequesites of a block recursively. It works best
-- for relatively shallow queries. For synchronizing longer/deeper forks an
-- iterative algorithm is preferable.
--
getBlockHeaderInternal
    :: HasVersion
    => BlockHeaderCas candidateHeaderCas
        -- ^ CandidateHeaderCas is a content addressed store for BlockHeaders
    => ReadableTable candidatePldTbl BlockPayloadHash EncodedPayloadData
    => WebBlockHeaderStore
        -- ^ Block Header Store for all Chains
    -> candidateHeaderCas
        -- ^ Ephemeral store for block headers under consideration
    -> candidatePldTbl
    -> ChainMap ConfiguredPayloadProvider
    -> Maybe (BlockPayloadHash, EncodedPayloadOutputs)
        -- ^ Payload and Header data for the block, in case that it is
        -- available.
    -> Priority
        -- ^ Priority of P2P tasks for querying headers and payloads
    -> Maybe PeerInfo
        -- ^ Origin hint for the block header and related data.
    -> ChainValue BlockHash
        -- ^ The hash of the block that is processed, tagged by the chain
    -> IO (ChainValue BlockHeader)
        -- ^ If validation is successful the added block header is returned
getBlockHeaderInternal
    headerStore
    candidateHeaderCas
    candidatePldTbl
    providers
    localPayload
    priority
    maybeOrigin
    h
  = do
    logg Debug $ "getBlockHeaderInternal: " <> sshow h
    !bh <- memoInsert cas memoMap h $ \k@(ChainValue cid k') -> do

        -- query BlockHeader via
        --
        -- - header store,
        -- - candidates header cache,
        -- - local database (we may have validated this header before)
        -- - cut origin, or
        -- - task queue of P2P network
        --
        (maybeOrigin', header) <- tableLookup candidateHeaderCas k' >>= \case
            Just !x -> return (maybeOrigin, x)
            Nothing -> pullOrigin k maybeOrigin >>= \case
                Nothing -> do
                    t <- queryBlockHeaderTask k
                    pQueueInsert queue t
                    (ChainValue _ !x) <- awaitTask t
                    return (Nothing, x)
                Just !x -> return (maybeOrigin, x)

        -- Check that the chain id is correct. The candidate cas is indexed just
        -- by the block hash. So, if this fails it is most likely a bug in code
        -- that uses or populates the candidateHeaderCas.
        --
        unless (_chainId header == cid) $ throwM $ GetBlockHeaderFailure
            $ "chain id of block header doesn't match expected chain id. "
            <> "Most likely, this is a bug in Chainweb.Sync.WebBlockHeaderStore."

        -- Perform intrinsic validations on the block header. There's another
        -- complete pass of block header validations after payload validation
        -- when the header is finally added to the db.
        --
        now <- getCurrentTimeIntegral
        validateIntrinsicM now header

        -- Query Prerequesits recursively. If there is already a job for this
        -- prerequesite in the memo-table it is awaited, otherwise a new job is
        -- created.
        --
        let isGenesisParentHash p = _chainValueValue p == genesisParentBlockHash p
            queryAdjacentParent p = Concurrently $ unless (isGenesisParentHash p) $ void $ do
                logg Debug $ taskMsg k
                    $ "getBlockHeaderInternal.getPrerequisiteHeader (adjacent) for " <> sshow h
                    <> ": " <> sshow p
                getBlockHeaderInternal
                    headerStore
                    candidateHeaderCas
                    candidatePldTbl
                    providers
                    localPayload
                    priority
                    maybeOrigin'
                    (unwrapParent <$> p)

            -- Perform inductive (involving the parent) validations on the block
            -- header. There's another complete pass of block header validations
            -- after payload validation when the header is finally added to the db.
            --
            queryParent p = Concurrently $ do
                logg Debug $ taskMsg k
                    $ "getBlockHeaderInternal.getPrerequisiteHeader (parent) for " <> sshow h
                    <> ": " <> sshow p
                parentHdr <- Parent <$> getBlockHeaderInternal
                    headerStore
                    candidateHeaderCas
                    candidatePldTbl
                    providers
                    localPayload
                    priority
                    maybeOrigin'
                    (unwrapParent <$> p)
                chainDb <- getWebBlockHeaderDb wdb header
                validateInductiveChainM (tableLookup chainDb) header
                return $ fmap _chainValueValue parentHdr

        -- Get the Payload Provider and
        let hints = Hints <$> maybeOrigin'
        pld <- tableLookup candidatePldTbl (view blockPayloadHash header)
        let prefetchProviderPayloads = case providers ^?! atChain cid of
                ConfiguredPayloadProvider _provider -> return ()
                    -- TODO PP
                    -- prefetchPayloads provider hints
                    --     [flip ConsensusPayload Nothing <$> view rankedBlockPayloadHash header]
                DisabledPayloadProvider -> return ()

        parentHdr <- runConcurrently
            -- instruct the payload provider to fetch payload data and prepare
            -- validation.
            $ Concurrently prefetchProviderPayloads

            -- query parent (recursively)
            --
            *> queryParent (view blockParent <$> chainValue header)

            -- query adjacent parents (recursively)
            <* mconcat (queryAdjacentParent <$> adjParents header)

            -- TODO Above recursive calls are potentially long running
            -- computations. In particular pact validation can take significant
            -- amounts of time. We may try make these calls tail recursive by
            -- providing a continuation. This would allow earlier garbage
            -- collection of some stack resources.
            --
            -- This requires to provide a CPS version of memoInsert.

        logg Debug $ taskMsg k $ "getBlockHeaderInternal got pre-requesites for " <> sshow h

        -- ------------------------------------------------------------------ --
        -- Validation

        -- 1. Validate Parents and Adjacent Parents
        --
        -- Existence and validity of parents and adjacent parents is guaranteed
        -- in the dependency resolution code above.

        -- 2. Validate BlockHeader
        --
        -- Single chain properties are currently validated when the block header
        -- is inserted into the block header db.

        -- 3. Validate Braiding
        --
        -- Currently, we allow blocks here that are not part of a valid
        -- braiding. However, those block won't make it into cuts, because the
        -- cut processor uses 'joinIntoHeavier' to combine an external cut with
        -- the local cut, which guarantees that only blocks with valid braiding
        -- are referenced by local cuts.
        --
        -- TODO: check braiding and reject blocks without valid braiding here.

        -- 4. Validate block payload
        --
        -- Pact validation is done in the context of a particular header. Just
        -- because the payload does already exist in the store doesn't mean that
        -- validation succeeds in the context of a particular block header.
        --
        -- If we reach this point in the code we are certain that the header
        -- isn't yet in the block header database and thus we still must
        -- validate the payload for this block header.
        --

        -- Do not produce payloads at this point; we may not stick around at
        -- this block.
        finfo <- forkInfoForHeader wdb header pld (Just parentHdr) <&> forkInfoNewBlockCtx .~ Nothing

        logg Debug $ taskMsg k $
            "getBlockHeaderInternal validate payload for " <> sshow h
        case providers ^?! atChain cid of
            ConfiguredPayloadProvider provider -> do
                r <- syncToBlock provider hints finfo `catch` \(e :: SomeException) -> do
                    logg Warn $ taskMsg k $ "getBlockHeaderInternal payload validation for " <> sshow h <> " failed with : " <> sshow e
                    throwM e
                if r /= _forkInfoTargetState finfo
                then do
                    bhdb <- getWebBlockHeaderDb wdb cid
                    let ppRBH = _syncStateRankedBlockHash $ _consensusStateLatest r
                    ppBlock <- lookupRankedM bhdb (int $ _rankedHeight ppRBH) (_ranked ppRBH) `catch` \case
                        e@(TreeDbKeyNotFound {} :: TreeDbException BlockHeaderDb) -> do
                            logfun Warn $ "PP block is missing: " <> brief ppRBH
                            throwM e
                        e -> throwM e

                    (forkBlocksDescendingStream S.:> forkPoint) <-
                            S.toList $ branchDiff_ bhdb ppBlock (unwrapParent parentHdr)
                    let forkBlocksAscending = reverse $ snd $ partitionHereThere (That header : forkBlocksDescendingStream)
                    let newTrace =
                            zipWith
                                (\prent child ->
                                    ConsensusPayload (view blockPayloadHash child) Nothing <$
                                        blockHeaderToEvaluationCtx (Parent prent))
                                (forkPoint : forkBlocksAscending)
                                forkBlocksAscending
                    let newForkInfo = finfo
                            { _forkInfoTrace = newTrace
                            , _forkInfoBasePayloadHash =
                                Parent $ view blockPayloadHash forkPoint
                            }
                    r' <- syncToBlock provider hints newForkInfo `catch` \(e :: SomeException) -> do
                        logg Warn $ taskMsg k $ "getBlockHeaderInternal payload validation retry for " <> sshow h <> " failed with: " <> sshow e
                        throwM e
                    unless (r' == _forkInfoTargetState finfo) $ do
                        throwM $ GetBlockHeaderFailure $ "unexpected result state"
                            <> "; expected: " <> brief (_forkInfoTargetState finfo)
                            <> "; actual: " <> brief r
                            <> "; PP latest block: " <> brief ppBlock
                            <> "; target block: " <> brief header
                            <> "; target block parent: " <> brief (unwrapParent parentHdr)
                            <> "; fork blocks: " <> brief forkBlocksAscending
                else
                    return ()
            DisabledPayloadProvider -> do
                logg Debug $ taskMsg k $ "getBlockHeaderInternal payload provider disabled"

        logg Debug $ taskMsg k "getBlockHeaderInternal pact validation succeeded"

        logg Debug $ taskMsg k $ "getBlockHeaderInternal return header " <> sshow h
        return $! chainValue header
    logg Debug $ "getBlockHeaderInternal: got block header for " <> sshow h
    return bh

  where

    mgr = _webBlockHeaderStoreMgr headerStore
    cas = WebBlockHeaderCas $ _webBlockHeaderStoreCas headerStore
    memoMap = _webBlockHeaderStoreMemo headerStore
    queue = _webBlockHeaderStoreQueue headerStore
    wdb = _webBlockHeaderStoreCas headerStore

    logfun :: LogFunction
    logfun = _webBlockHeaderStoreLogFunction headerStore

    logg :: LogFunctionText
    logg = logfun @T.Text

    taskMsg k msg = "header task " <> sshow k <> ": " <> msg

    traceLabel subfun =
        "Chainweb.Sync.WebBlockHeaderStore.getBlockHeaderInternal." <> subfun

    -- pact = _pactValidateBlock
    --     $ _webPactExecutionService
    --     $ _webBlockPayloadStorePact payloadStore

    -- validateAndInsertPayload :: BlockHeader -> PayloadData -> IO ()
    -- validateAndInsertPayload hdr p = do
    --     let payload = case localPayload of
    --             Just (hsh, pwo)
    --                 | hsh == view blockPayloadHash hdr
    --                     -> CheckablePayloadWithOutputs pwo
    --             _ -> CheckablePayload p
    --     outs <- trace logfun
    --         (traceLabel "pact")
    --         (view blockHash hdr)
    --         (length (view payloadDataTransactions p))
    --         $ pact hdr payload
    --     addNewPayload (_webBlockPayloadStoreCas payloadStore) (view blockHeight hdr) outs

    queryBlockHeaderTask ck@(ChainValue cid k)
        = newTask (sshow ck) priority $ \l env -> chainValue <$> do
            l @T.Text Debug $ taskMsg ck "query remote block header"
            let taskEnv = setResponseTimeout taskResponseTimeout env
            !r <- trace l (traceLabel "queryBlockHeaderTask") k (let Priority i = priority in i)
                $ TDB.lookupM (rDb cid taskEnv) k `catchAllSynchronous` \e -> do
                    l @T.Text Debug $ taskMsg ck $ "failed: " <> sshow e
                    throwM e
            l @T.Text Debug $ taskMsg ck "received remote block header"
            return r

    rDb :: ChainId -> ClientEnv -> RemoteDb
    rDb cid env = RemoteDb
        { _remoteEnv = env
        , _remoteLogFunction = ALogFunction logfun
        , _remoteChainId = cid
        }

    adjParents = toList . imap ChainValue . _getBlockHashRecord . view blockAdjacentHashes

    pullOrigin
        :: ChainValue BlockHash
        -> Maybe PeerInfo
        -> IO (Maybe BlockHeader)
    pullOrigin ck Nothing = do
        logg Debug $ taskMsg ck "no origin"
        return Nothing
    pullOrigin ck@(ChainValue cid k) (Just origin) = do
        let originEnv = setResponseTimeout pullOriginResponseTimeout $ peerInfoClientEnv mgr origin
        logg Debug $ taskMsg ck "lookup origin"
        !r <- trace logfun (traceLabel "pullOrigin") k 0
            $ TDB.lookup (rDb cid originEnv) k
        case r of
            Nothing -> do
                logg Warn $ taskMsg k
                    $ "failed to pull from origin " <> sshow origin <> " key " <> sshow k
                return Nothing
            Just !v -> do
                logg Debug $ taskMsg ck "received from origin"
                return $ Just v

    -- pullOriginDeps _ Nothing = return ()
    -- pullOriginDeps ck@(ChainValue cid k) (Just origin) = do
    --     let originEnv = peerInfoClientEnv mgr origin
    --     curRank <- liftIO $ do
    --         cdb <- give (_webBlockHeaderStoreCas headerStore) (getWebBlockHeaderDb cid)
    --         maxRank cdb
    --     (l, _) <- TDB.branchEntries (rDb cid originEnv)
    --         Nothing (Just 1000)
    --         (Just $ int curRank) Nothing
    --         mempty (HS.singleton (UpperBound k))
    --     liftIO $ logg Info $ taskMsg ck $ "pre-fetched " <> sshow l <> " block headers"
    --     return ()

newWebBlockHeaderStore
    :: HTTP.Manager
    -> WebBlockHeaderDb
    -> LogFunction
    -> IO WebBlockHeaderStore
newWebBlockHeaderStore mgr wdb logfun = do
    m <- new
    queue <- newEmptyPQueue
    return $! WebBlockHeaderStore wdb m queue logfun mgr

newWebPayloadStore
    :: HTTP.Manager
    -> PayloadDb tbl
    -> LogFunction
    -> IO (WebBlockPayloadStore tbl)
newWebPayloadStore mgr payloadDb logfun = do
    payloadTaskQueue <- newEmptyPQueue
    payloadMemo <- new
    return $! WebBlockPayloadStore
        payloadDb payloadMemo payloadTaskQueue logfun mgr

getBlockHeader
    :: HasVersion
    => BlockHeaderCas candidateHeaderCas
    => ReadableTable candidatePldTbl BlockPayloadHash EncodedPayloadData
    => WebBlockHeaderStore
    -> candidateHeaderCas
    -> candidatePldTbl
    -> ChainMap ConfiguredPayloadProvider
    -> Maybe (BlockPayloadHash, EncodedPayloadOutputs)
    -> ChainId
    -> Priority
    -> Maybe PeerInfo
    -> BlockHash
    -> IO BlockHeader
getBlockHeader headerStore candidateHeaderCas candidatePldTbl providers localPayload cid priority maybeOrigin h
    = ((\(ChainValue _ b) -> b) <$> go)
        `catch` \(TaskFailed es) -> throwM $ TreeDbKeyNotFound @BlockHeaderDb h (sshow es)
  where
    go = getBlockHeaderInternal
        headerStore
        candidateHeaderCas
        candidatePldTbl
        providers
        localPayload
        priority
        maybeOrigin
        (ChainValue cid h)
{-# INLINE getBlockHeader #-}

instance (HasVersion, CasKeyType (ChainValue BlockHeader) ~ k) => ReadableTable WebBlockHeaderCas k (ChainValue BlockHeader) where
    tableLookup (WebBlockHeaderCas db) (ChainValue cid h) =
        (Just . ChainValue cid <$> lookupWebBlockHeaderDb db cid h)
            `catch` \e -> case e of
                TDB.TreeDbKeyNotFound _ _ -> return Nothing
                _ -> throwM @_ @(TDB.TreeDbException BlockHeaderDb) e
    {-# INLINE tableLookup #-}

instance (HasVersion, CasKeyType (ChainValue BlockHeader) ~ k) => Table WebBlockHeaderCas k (ChainValue BlockHeader) where
    tableInsert (WebBlockHeaderCas db) _ (ChainValue _ h)
        = insertWebBlockHeaderDb db h
    {-# INLINE tableInsert #-}

    tableDelete = error "not implemented"

    -- This is fine since the type 'WebBlockHeaderCas' is not exported. So the
    -- instance is available only locally.
    --
    -- The instance requires that memoCache doesn't delete from the cas.
