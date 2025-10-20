{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.CutDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.CutDB
(
-- * CutConfig
  CutDbParams(..)
, cutDbParamsInitialCutFile
, cutDbParamsBufferSize
, cutDbParamsLogLevel
, cutDbParamsTelemetryLevel
, cutDbParamsFastForwardHeightLimit
, cutDbParamsInitialHeightLimit
, cutDbParamsFetchTimeout
, cutDbParamsAvgBlockHeightPruningDepth
, cutDbParamsReadOnly
, defaultCutDbParams
, farAheadThreshold

-- * Cut Hashes Table
, cutHashesTable
, readHighestCutHeaders

-- * CutDb
, CutDb
, pruneCuts
, cutDbWebBlockHeaderDb
, cutDbBlockHeaderDb
, cutDbPayloadProviders
, cut
, _cut
, _cutStm
, cutStm
, awaitNewCut
, awaitNewCutStm
, awaitNewCutByChainId
, awaitNewBlock
, awaitNewBlockStm
, awaitNewCutByChainIdStm
, cutStream
, addCutHashes
, withCutDb
, startCutDb
, fastForwardCutDb
, stopCutDb
, cutDbQueueSize
, blockStream
, blockDiffStream
, cutStreamToHeaderStream
, cutStreamToHeaderDiffStream

-- * Membership Queries
, member
, memberOfHeader

-- * Some CutDb
, CutDbT(..)
, SomeCutDb(..)
, someCutDbVal

-- * Queue Statistics
, QueueStats(..)
, getQueueStats
) where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Cut.Create
import Chainweb.Cut.CutHashes
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.PayloadProvider
import Chainweb.Ranked
import Chainweb.Storage.Table
import Chainweb.Storage.Table.HashMap
import Chainweb.Storage.Table.RocksDB
import Chainweb.Sync.ForkInfo
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec, check)
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Exception (asyncExceptionFromException, asyncExceptionToException)
import Control.Exception.Safe
import Control.Lens hiding ((:>), (.=))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.STM
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource hiding (throwM)
import Data.Aeson (ToJSON, Value, object, (.=))
import Data.ByteString.Lazy qualified as BS
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Function
import Data.Functor.Of
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.LogMessage
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.PQueue
import Data.TaskMap qualified as TM
import Data.Text (Text)
import Data.Text qualified as T
import Data.These
import GHC.Generics hiding (to)
import Numeric.Natural
import P2P.TaskQueue
import Prelude hiding (lookup)
import Streaming.Prelude qualified as S
import System.LogLevel
import System.Timeout
import Utils.Logging.Trace
import Chainweb.ChainValue
import P2P.Utils

-- -------------------------------------------------------------------------- --
-- Cut DB Configuration

data CutDbParams = CutDbParams
    { _cutDbParamsInitialCutFile :: !(Maybe FilePath)
    , _cutDbParamsBufferSize :: !Natural
    , _cutDbParamsLogLevel :: !LogLevel
    , _cutDbParamsTelemetryLevel :: !LogLevel
    , _cutDbParamsFetchTimeout :: !Int
    , _cutDbParamsInitialHeightLimit :: !(Maybe BlockHeight)
    , _cutDbParamsFastForwardHeightLimit :: !(Maybe BlockHeight)
    , _cutDbParamsAvgBlockHeightPruningDepth :: !BlockHeight
    -- ^ How many block heights' worth of cuts should we keep around?
    -- (how far back do we expect that a fork can happen)
    , _cutDbParamsReadOnly :: !Bool
    -- ^ Should the cut store be read-only?
    -- Enabled during replay-only mode.
    --
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''CutDbParams

defaultCutDbParams :: HasVersion => Int -> CutDbParams
defaultCutDbParams ft = CutDbParams
    { _cutDbParamsInitialCutFile = Nothing
    , _cutDbParamsBufferSize = (order g ^ (2 :: Int)) * diameter g
    , _cutDbParamsLogLevel = Warn
    , _cutDbParamsTelemetryLevel = Warn
    , _cutDbParamsFetchTimeout = ft
    , _cutDbParamsInitialHeightLimit = Nothing
    , _cutDbParamsFastForwardHeightLimit = Nothing
    -- really this is 2500 minutes, or ~1.7 days, which gives us some time to
    -- recover from long-lived forks.
    , _cutDbParamsAvgBlockHeightPruningDepth = 5000
    , _cutDbParamsReadOnly = False
    }
  where
    g = chainGraphAt (maxBound @BlockHeight)

newtype CutPruningState = CutPruningState
    { cutPruningStateLatestWrittenHeight :: BlockHeight
    }

initialCutPruningState :: Cut -> CutPruningState
initialCutPruningState initialCut = CutPruningState
    { cutPruningStateLatestWrittenHeight = view cutMaxHeight initialCut
    }

-- | We ignore cuts that are two far ahead of the current best cut that we have.
-- There are two reasons for this:
--
-- 1. It limits the effect of a DOS attack where an attack sends large fake cuts
--    that would consume a lot of resources before we notice that the cut was
--    fake.
--
-- 2. It helps a node catching up with the overall consensus of the network by
--    doing the catchup via several moderately fixed size steps instead of one
--    giant step. The latter would consume an unlimited amount of resources,
--    possibly leading to resource exhaustion on the local system. Also if the
--    node is restarted during the catch-up process it resumes from the
--    intermediate steps instead of starting all over again.
--
-- For the latter to work it is important that the local node only pulls cuts
-- that are at most 'forAheadThreshold' blocks ahead. Otherwise the pulled
-- blocks would be immediately rejected by the cut processing pipeline
-- 'processCuts' below in the module and the node would never be able to join
-- the consensus of the network.
--
-- NOTE: this number multiplied by the (current) number of chains must always be
-- STRICTLY LARGER THAN 'catchupStepSize' in "Chainweb.CutDB.Sync".
--
-- This number should also be sufficiently large to accomodate for parallel
-- mining. It should be at least the diameter of the chain graph.
--
farAheadThreshold :: BlockHeight
farAheadThreshold = 20

-- -------------------------------------------------------------------------- --
-- CutHashes Table

cutHashesTable :: HasVersion => RocksDb -> Casify RocksDbTable CutHashes
cutHashesTable rdb = Casify $ newTable rdb valueCodec keyCodec ["CutHashes"]
  where
    keyCodec = Codec
        (\(a,b,c) -> runPutS $ encodeCutHeightBe a >> encodeBlockWeightBe b >> encodeCutId c)
        (runGetS $ (,,) <$> decodeCutHeightBe <*> decodeBlockWeightBe <*> decodeCutId)
    valueCodec = Codec encodeToByteString decodeStrictOrThrow'

-- -------------------------------------------------------------------------- --
-- Exceptions

data CutDbStopped = CutDbStopped
    deriving (Eq, Show, Generic)

instance Exception CutDbStopped where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

-- -------------------------------------------------------------------------- --
-- Cut DB

-- | This is a singleton DB that contains the latest chainweb cut as only entry.
--
data CutDb l = CutDb
    { _cutDbCut :: !(TVar Cut)
    , _cutDbQueue :: !(PQueue (Down CutHashes))
    , _cutDbAsync :: !(Async ())
    , _cutDbLogFunction :: !LogFunction
    , _cutDbHeaderStore :: !(WebBlockHeaderStore l)
    , _cutDbPayloadProviders :: !(ChainMap ConfiguredPayloadProvider)
    , _cutDbCutStore :: !(Casify RocksDbTable CutHashes)
    , _cutDbQueueSize :: !Natural
    , _cutDbReadOnly :: !Bool
    , _cutDbFastForwardHeightLimit :: !(Maybe BlockHeight)
    }

cutDbPayloadProviders :: Getter (CutDb l) (ChainMap ConfiguredPayloadProvider)
cutDbPayloadProviders = to _cutDbPayloadProviders
{-# INLINE cutDbPayloadProviders #-}

-- We export the 'WebBlockHeaderDb' read-only
--
cutDbWebBlockHeaderDb :: Getter (CutDb l) WebBlockHeaderDb
cutDbWebBlockHeaderDb = to $ _webBlockHeaderStoreCas . _cutDbHeaderStore
{-# INLINE cutDbWebBlockHeaderDb #-}

cutDbWebBlockHeaderStore :: Getter (CutDb l) (WebBlockHeaderStore l)
cutDbWebBlockHeaderStore = to _cutDbHeaderStore
{-# INLINE cutDbWebBlockHeaderStore #-}

-- | Access the blockerheaderdb via the cutdb for a given chain id
--
cutDbBlockHeaderDb :: HasChainId cid => cid -> Fold (CutDb l) BlockHeaderDb
cutDbBlockHeaderDb cid = cutDbWebBlockHeaderDb . ixg (_chainId cid)

-- | Get the current 'Cut', which represent the latest chainweb state.
--
-- This the main API method of chainweb-consensus.
--
_cut :: CutDb l -> IO Cut
_cut = readTVarIO . _cutDbCut
{-# INLINE _cut #-}

-- | Get the current 'Cut', which represent the latest chainweb state.
--
-- This the main API method of chainweb-consensus.
--
cut :: Getter (CutDb l) (IO Cut)
cut = to _cut

addCutHashes :: CutDb l -> CutHashes -> IO ()
addCutHashes db = pQueueInsertLimit (_cutDbQueue db) (_cutDbQueueSize db) . Down

-- | An 'STM' version of '_cut'.
--
-- @_cut db@ is generally more efficient than as @atomically (_cut db)@.
--
_cutStm :: CutDb l -> STM Cut
_cutStm = readTVar . _cutDbCut

-- | An 'STM' version of 'cut'.
--
-- @_cut db@ is generally more efficient than as @atomically (_cut db)@.
--
cutStm :: Getter (CutDb l) (STM Cut)
cutStm = to _cutStm

-- | A common idiom to spin while waiting for a guaranteed new `Cut`, different
-- from the given one.
--
awaitNewCutStm :: CutDb l -> Cut -> STM Cut
awaitNewCutStm cdb c = do
    c' <- _cutStm cdb
    when (c' == c) retry
    return c'

-- | A common idiom to spin while waiting for a guaranteed new `Cut`, different
-- from the given one.
--
awaitNewCut :: CutDb l -> Cut -> IO Cut
awaitNewCut cdb c = atomically $ awaitNewCutStm cdb c

-- | As in `awaitNewCut`, but only updates when the specified `ChainId` has
-- grown.
--
awaitNewCutByChainId :: CutDb l -> ChainId -> Cut -> IO Cut
awaitNewCutByChainId cdb cid c = atomically $ awaitNewCutByChainIdStm cdb cid c
{-# INLINE awaitNewCutByChainId #-}

-- | As in `awaitNewCut`, but only updates when the header at the specified
-- `ChainId` has changed, and only returns that new header.
awaitNewBlock :: CutDb l -> ChainId -> BlockHash -> IO BlockHeader
awaitNewBlock cdb cid bHash = atomically $ awaitNewBlockStm cdb cid bHash

-- | As in `awaitNewCut`, but only updates when the header at the specified
-- `ChainId` has changed, and only returns that new header.
awaitNewBlockStm :: CutDb l -> ChainId -> BlockHash -> STM BlockHeader
awaitNewBlockStm cdb cid bHash = do
    c <- _cutStm cdb
    case HM.lookup cid (_cutMap c) of
        Just bh' | view blockHash bh' /= bHash -> return bh'
        _ -> retry

-- | As in `awaitNewCut`, but only updates when the specified `ChainId` has
-- grown.
--
awaitNewCutByChainIdStm :: CutDb l -> ChainId -> Cut -> STM Cut
awaitNewCutByChainIdStm cdb cid c = do
    c' <- _cutStm cdb
    let !b0 = HM.lookup cid $ _cutMap c
        !b1 = HM.lookup cid $ _cutMap c'
    when (b1 == b0) retry
    return c'

pruneCuts
    :: HasVersion
    => LogFunction
    -> CutDbParams
    -> BlockHeight
    -> Casify RocksDbTable CutHashes
    -> IO ()
pruneCuts logfun conf curAvgBlockHeight cutHashesStore = do
    let avgBlockHeightPruningDepth = _cutDbParamsAvgBlockHeightPruningDepth conf
    let pruneCutHeight =
            avgCutHeightAt (curAvgBlockHeight - min curAvgBlockHeight avgBlockHeightPruningDepth)
    logfun @T.Text Info $ "pruning CutDB before cut height " <> T.pack (show pruneCutHeight)
    -- deleteRange is constant time in rocksdb, it just inserts a tombstone.
    -- either way, we will use constant space for cuts.
    deleteRangeRocksDb (unCasify cutHashesStore)
        (Nothing, Just (pruneCutHeight, 0, maxBound :: CutId))

cutDbQueueSize :: CutDb l -> IO Natural
cutDbQueueSize = pQueueSize . _cutDbQueue

withCutDb
    :: HasVersion
    => Logger logger
    => CutDbParams
    -> logger
    -> WebBlockHeaderStore logger
    -> ChainMap ConfiguredPayloadProvider
    -> Casify RocksDbTable CutHashes
    -> ResourceT IO (Either Cut (CutDb logger))
withCutDb config logger headerStore providers cutHashesStore
    = snd <$> allocate
        (startCutDb config logger headerStore providers cutHashesStore)
        (traverse_ stopCutDb)

-- | Start a CutDB. This loads the initial cut from the database (falling back
-- to the configured initial cut loading fails) and starts the cut validation
-- pipeline.
--
-- If possible use 'withCutDb' instead of this function. This function exposes
-- the type of the the payload store via the 'cas' parameter. 'withCutDb'
-- provides a 'CutDB' that abstracts over the cas type and only exposes a
-- read-only version of the payload store.
--
startCutDb
    :: Logger logger
    => HasVersion
    => CutDbParams
    -> logger
    -> WebBlockHeaderStore logger
    -> ChainMap ConfiguredPayloadProvider
    -> Casify RocksDbTable CutHashes
    -> IO (Either Cut (CutDb logger))
startCutDb config logger headerStore providers cutHashesStore = mask_ $ do
    logg Debug "obtain initial cut"
    initialCut <- readInitialCut
    recoveryCut <- synchronizeProviders logger wbhdb providers initialCut
    unless (_cutDbParamsReadOnly config) $
        deleteRangeRocksDb
            (unCasify cutHashesStore)
            -- intentionally don't delete up to recovery cut, the initial cut could be useful later
            (Just $ over _1 succ $ casKey $ cutToCutHashes Nothing initialCut, Nothing)
    if isNothing (_cutDbParamsInitialHeightLimit config)
        && isNothing (_cutDbParamsInitialCutFile config)
    then do
        cutVar <- newTVarIO recoveryCut
        -- use the recovery cut for the pruning state, so that we write cuts more quickly after recovering
        cutPruningStateVar <- newTVarIO $ initialCutPruningState recoveryCut
        c <- readTVarIO cutVar
        logg Info $ T.unlines $
            "got initial cut:" : ["    " <> block | block <- cutToTextShort c]
        queue <- newEmptyPQueue
        cutAsync <- asyncWithUnmask $ \u -> u $ processor queue cutVar cutPruningStateVar
        return $ Right CutDb
            { _cutDbCut = cutVar
            , _cutDbQueue = queue
            , _cutDbAsync = cutAsync
            , _cutDbLogFunction = logFunction logger
            , _cutDbHeaderStore = headerStore
            , _cutDbPayloadProviders = providers
            , _cutDbQueueSize = _cutDbParamsBufferSize config
            , _cutDbCutStore = cutHashesStore
            , _cutDbReadOnly = _cutDbParamsReadOnly config
            , _cutDbFastForwardHeightLimit = _cutDbParamsFastForwardHeightLimit config
            }
    else
        return (Left recoveryCut)
  where
    logg = logFunctionText logger
    wbhdb = _webBlockHeaderStoreCas headerStore

    processor :: PQueue (Down CutHashes) -> TVar Cut -> TVar CutPruningState -> IO ()
    processor queue cutVar cutPruningStateVar = runForever (logFunction logger) "CutDB" $
        processCuts config logger headerStore providers cutHashesStore queue cutVar cutPruningStateVar

    readInitialCut :: IO Cut
    readInitialCut = do
        case _cutDbParamsInitialCutFile config of
            Nothing -> do
                unsafeMkCut <$> do
                    hm <- readHighestCutHeaders logg wbhdb cutHashesStore
                    case _cutDbParamsInitialHeightLimit config of
                        Nothing -> return hm
                        Just h -> do
                            limitedCutHeaders <- limitCutHeaders wbhdb h hm
                            let limitedCut = unsafeMkCut limitedCutHeaders
                            unless (_cutDbParamsReadOnly config) $
                                casInsert cutHashesStore (cutToCutHashes Nothing limitedCut)
                            return limitedCutHeaders
            Just f -> do
                rankedBlockHashes :: HM.HashMap ChainId RankedBlockHash <- decodeOrThrow =<< BS.readFile f
                blockHeaders <- iforM rankedBlockHashes (lookupRankedWebBlockHeaderDb wbhdb)
                return $ unsafeMkCut blockHeaders

-- | Sync all configured payload providers with consensus.
--
synchronizeProviders
    :: (Logger logger, HasVersion)
    => logger
    -> WebBlockHeaderDb
    -> ChainMap ConfiguredPayloadProvider
    -> Cut
    -> IO Cut
synchronizeProviders logger wbh providers c = do
    let startHeaders = HM.union
            (_cutHeaders c)
            (imap (\cid () -> genesisBlockHeader cid) (HS.toMap chainIds))
    syncsSuccessful <- mapConcurrently (runMaybeT . syncOne) startHeaders

    logFunctionText logger Info $ "finished synchronizing all payload providers"
        <> "; failed: " <> sshow (length (HM.filter isNothing syncsSuccessful))

    if all isJust syncsSuccessful
      then do
        return c
      else do
        -- try to recover from the fork automatically by removing ~`diameter`
        -- blocks from the cut
        let recoveryHeight =
                max (int (diameter (chainGraphAt maxBound))) (_cutMinHeight c) - int (diameter (chainGraphAt maxBound))
        recoveryCut <- limitCut wbh recoveryHeight c
        let recoveryHeaders = HM.union
                (_cutHeaders recoveryCut)
                (imap (\cid () -> genesisBlockHeader cid) (HS.toMap chainIds))
        mapConcurrently_ (runMaybeT . syncOne) recoveryHeaders
        return recoveryCut
  where
    syncOne :: BlockHeader -> MaybeT IO ()
    syncOne hdr = case providers ^?! atChain (_chainId hdr) of
        ConfiguredPayloadProvider provider -> do
            let pLogger = providerLogger provider . chainLogger hdr $ logger
            let pLog :: MonadIO m => LogLevel -> Text -> m ()
                pLog l = liftIO . logFunctionText pLogger l
            pLog Info $ "syncing payload provider to "
                    <> sshow (view blockHeight hdr)
                    <> "." <> toText (view blockHash hdr)
            finfo <- liftIO $ forkInfoForHeader wbh hdr Nothing Nothing True
            pLog Debug $ "syncToBlock with fork info " <> encodeToText finfo

            bhdb <- liftIO $ getWebBlockHeaderDb wbh cid
            liftIO (resolveForkInfo pLog bhdb NullCas provider Nothing finfo) `catch` \(e :: SomeException) -> do
                pLog Warn $ "resolveFork failed"
                    <> "; finfo: " <> encodeToText finfo
                    <> "; failure: " <> T.pack (displayException e)
                empty
            pLog Info $ "payload provider synced to "
                <> sshow (view blockHeight hdr)
                <> "." <> toText (view blockHash hdr)

        DisabledPayloadProvider -> do
            liftIO $ logFunctionText logger Info $
                "payload provider disabled, not synced, on chain: " <> toText (_chainId hdr)
      where
        cid = _chainId hdr


readHighestCutHeaders
    :: HasVersion
    => LogFunctionText -> WebBlockHeaderDb -> Casify RocksDbTable CutHashes -> IO (HM.HashMap ChainId BlockHeader)
readHighestCutHeaders logg wbhdb cutHashesStore = withTableIterator (unCasify cutHashesStore) $ \it -> do
    iterLast it
    go it
  where
    -- TODO: should we limit the search to a certain number of attempts
    -- or iterate in increasinly larger steps?
    go it = iterValue it >>= \case
        Nothing -> do
            logg Info "No initial cut found in database or configuration, falling back to genesis cut"
            return $ view cutMap genesisCut
        Just ch -> try (lookupCutHashes wbhdb ch) >>= \case
            Left (e@TreeDbKeyNotFound {} :: TreeDbException BlockHeaderDb) -> do
                logg Warn
                    $ "Unable to load cut at height " <>  sshow (_cutHashesHeight ch)
                    <> " from database."
                    <> " Error: " <> T.pack (displayException e) <> "."
                    <> " The database might be corrupted. Falling back to previous cut."
                iterPrev it
                go it
            Left e -> throwM e
            Right hm -> return hm

fastForwardCutDb :: HasVersion => CutDb l -> IO ()
fastForwardCutDb cutDb = do
    highestCutHeaders <-
        readHighestCutHeaders (_cutDbLogFunction cutDb) wbhdb (_cutDbCutStore cutDb)
    limitedCutHeaders <-
        limitCutHeaders wbhdb (fromMaybe maxBound (_cutDbFastForwardHeightLimit cutDb)) highestCutHeaders
    let limitedCut = unsafeMkCut limitedCutHeaders
    atomically $ writeTVar (_cutDbCut cutDb) limitedCut
  where
    wbhdb = _webBlockHeaderStoreCas $ _cutDbHeaderStore cutDb

-- | Stop the cut validation pipeline.
--
stopCutDb :: CutDb l -> IO ()
stopCutDb db = do
    currentCut <- readTVarIO (_cutDbCut db)
    unless (_cutDbReadOnly db) $
        casInsert (_cutDbCutStore db) (cutToCutHashes Nothing currentCut)
    cancelWith (_cutDbAsync db) CutDbStopped

-- | Lookup the BlockHeaders for a CutHashes structure. Throws an exception if
-- the lookup for some BlockHash in the input CutHashes.
--
lookupCutHashes
    :: HasVersion
    => WebBlockHeaderDb
    -> CutHashes
    -> IO (HM.HashMap ChainId BlockHeader)
lookupCutHashes wbhdb hs =
    flip itraverse (_cutHashes hs) $ \cid (Ranked _ h) ->
        lookupWebBlockHeaderDb wbhdb cid h

cutAvgBlockHeight :: HasVersion => Cut -> BlockHeight
cutAvgBlockHeight = BlockHeight . round . avgBlockHeightAtCutHeight . _cutHeight

-- | This is at the heart of 'Chainweb' POW: Deciding the current "longest" cut
-- among the incoming candiates.
--
-- Going forward this should probably be the main scheduler for most operations,
-- in particular it should drive (or least preempt) synchronzations of block
-- headers on indiviual chains.
--
-- After initialization, this function is the only writer to the 'TVar Cut' that
-- stores the longest cut.
--
processCuts
    :: HasVersion
    => Logger l
    => CutDbParams
    -> l
    -> WebBlockHeaderStore l
    -> ChainMap ConfiguredPayloadProvider
    -> Casify RocksDbTable CutHashes
    -> PQueue (Down CutHashes)
    -> TVar Cut
    -> TVar CutPruningState
    -> IO ()
processCuts conf logger headerStore providers cutHashesStore queue cutVar cutPruningStateVar =
    flip onException writeLatestCutOnExit $ do
        pQueueToStream queue
            & S.map getDown
            & S.chain (\c -> cutIdLogFunctionText c logger Debug "start processing")
            & S.filterM (fmap not . isVeryOld)
            & S.filterM (fmap not . farAhead)
            & S.filterM (fmap not . isOld)
            & S.filterM (fmap not . isCurrent)

            & S.chain (\c -> cutIdLogFunctionText c logger Info "fetching all prerequisites")
            & S.mapM (cutHashesToBlockHeaderMap conf logger headerStore providers)
            & S.catMaybes
            -- ignore unsuccessful values for now

            -- using S.scanM would be slightly more efficient (one pointer dereference)
            -- by keeping the value of cutVar in memory. We use the S.mapM variant with
            -- an redundant 'readTVarIO' because it is easier to read.
            & S.mapM_ (\newCut -> do
                let clogger = cutLogger newCut logger
                let loggCutId :: LogFunction
                    loggCutId = logFunction clogger
                curCut <- readTVarIO cutVar
                !resultCut <- trace loggCutId "Chainweb.CutDB.processCuts._joinIntoHeavier" () 1
                    $ joinIntoHeavier_ hdrStore (_cutMap curCut) newCut
                -- write the cut to storage for later use when the node restarts
                -- or if the operator has to do manual fork resolution later.
                --
                -- we don't write *all* cuts. in the worst case if the node
                -- receives each block one at a time, we have one cut for each
                -- block. each cut has one block hash per chain, so that would
                -- be quadratic storage in the number of chains.
                -- we also prune old cuts for this reason.

                -- however, we do want to write them at least *once in a while*,
                -- to ensure that the node doesn't have to replay too much
                -- history on restart, and to ensure that we can do manual fork
                -- recovery without more complicated cut rediscovery mechanisms.
                unless (_cutDbParamsReadOnly conf) $ do
                    let resultCutMaxHeight = view cutMaxHeight resultCut
                    shouldWriteAndPrune <- atomically $ do
                        latestPruningState <- readTVar cutPruningStateVar
                        -- we write one cut each time the max height advances by
                        -- a block ahead of the previously written cut.
                        -- note that we are not that smart here; this code isn't
                        -- aware of reorgs. reorgs, especially rewinds, would
                        -- probably make some sense to write; but we assume that
                        -- this is good enough, because reorgs are small, one
                        -- block is a fast thing to wait for, and we
                        -- regardless write our current cut on a healthy
                        -- shutdown.
                        let lastWriteHeight = cutPruningStateLatestWrittenHeight latestPruningState
                        let nextWriteHeight = succ lastWriteHeight
                        if all (\bh -> view blockHeight bh >= nextWriteHeight) (view cutMap resultCut)
                        then do
                            writeTVar cutPruningStateVar
                                latestPruningState { cutPruningStateLatestWrittenHeight = resultCutMaxHeight }
                            return True
                        else
                            return False
                    when shouldWriteAndPrune $ do
                        pruneCuts logFun conf (cutAvgBlockHeight curCut) cutHashesStore
                        loggCutId @Text Info $ "writing cut at bh " <> sshow resultCutMaxHeight
                        casInsert cutHashesStore (cutToCutHashes Nothing resultCut)

                -- ensure that payload providers are in sync with the *merged*
                -- cut, so that they produce payloads on the correct parents.
                forConcurrently_ (HM.toList (_cutMap resultCut)) $ \(cid, bh) -> do
                    -- avoid asking for syncToBlock when we know that we're already
                    -- in sync, otherwise some payload providers misbehave.
                    when (Just bh /= curCut ^? ixg cid) $
                        case providers ^?! atChain cid of
                            ConfiguredPayloadProvider provider -> do
                                let cidLogger = chainLogger cid $ providerLogger  provider clogger
                                let clog = logFunction cidLogger

                                -- During this final sync we also enable payload production.
                                finfo <- forkInfoForHeader hdrStore bh Nothing Nothing True

                                -- Note, that this sync really should be super quick and
                                -- should never fail.
                                -- TODO: It would be nicer to go to the merge cut directly.
                                clog Info "Syncing paylooad provider with merged cut"
                                resolveForkInfo clog (hdrStore ^?! ixg cid) NullCas provider Nothing finfo `catch`
                                    \(e :: SomeException) -> do
                                        clog Error
                                            $ "Failed to sync payload provider to the merge cut."
                                            <> " This should never happen. It may indicated a broken payload provider or a corrupted database."
                                            <> " Fork info: " <> encodeToText finfo
                                            <> " Failure: " <> T.pack (displayException e)
                                        throwM $ InternalInvariantViolation
                                            $ "Failed to sync payload provider to the merge cut."
                                            <> " This should never happen. It may indicated a broken payload provider or a corrupted database."
                                            <> " Failure: " <> T.pack (displayException e)
                            _ -> return ()
                let cutDiff = cutDiffToTextShort curCut resultCut
                let currentCutIdMsg = T.unwords
                        [ "current cut is now"
                        , cutIdToTextShort (_cutId resultCut) <> ","
                        , "diff:" -- ???
                        ]
                let catOverflowing x xs =
                        if length xs == 1
                        then T.unwords (x : xs)
                        else T.intercalate "\n" (x : map ("    " <>) xs)
                loggCutId Info $ catOverflowing currentCutIdMsg cutDiff
                atomically $ writeTVar cutVar resultCut
                )
  where
    logFun :: LogFunction
    logFun = logFunction logger

    writeLatestCutOnExit = do
        latestCut <- readTVarIO cutVar
        casInsert cutHashesStore (cutToCutHashes Nothing latestCut)

    hdrStore = _webBlockHeaderStoreCas headerStore

    -- FIXME: this is problematic. We should drop these much earlier before they
    -- are even added to the queue, to prevent the queue from becoming stale.
    -- Although its probably low risk, since the processing pipeline is probably
    -- faster in removing these from the queue than the network stack can add
    -- them.
    --
    -- Note that _cutHashesMaxHeight is linear in the number of chains. If
    -- that's not acceptable any more, it would also be fine to just pick the
    -- height from some arbitrary chain, e.g. 0, in which case the result would
    -- be off by at most the diameter of the graph.
    --
    farAhead x = do
        curMax <- _cutMaxHeight <$> readTVarIO cutVar
        let newMax = _cutHashesMaxHeight x
        let r = newMax >= curMax + farAheadThreshold
        when r $ cutIdLogFunctionText x logger Debug
            $ "skip far ahead cut. Current maximum block height: " <> sshow curMax
            <> ", got: " <> sshow newMax
            -- log at debug level because this is a common case during catchup
        return r

    -- This could be problematic if there is a very lightweight fork that is far
    -- ahead. Otherwise, there should be no fork that has 2*diam less blocks and
    -- exceeds the current fork in weight.
    --
    -- Note that _cutHashesMaxHeight is linear in the number of chains. If
    -- that's not acceptable any more, it would also be fine to just pick the
    -- height from some arbitrary chain, e.g. 0, in which case the result would
    -- be off by at most the diameter of the graph.
    --
    isVeryOld x = do
        curMin <- _cutMinHeight <$> readTVarIO cutVar
        let diam = diameter $ chainGraphAt curMin
            newMin = _cutHashesMinHeight x
        let r = newMin + 2 * (1 + int diam) <= curMin
        when r $ cutIdLogFunctionText x logger Debug "skip very old cut"
            -- log at debug level because this is a common case during catchup
        return r

    -- This should be based on weight
    --
    isOld x = do
        curHashes <- cutToCutHashes Nothing <$> readTVarIO cutVar
        let r = all (>= (0 :: Int)) $ (HM.unionWith (-) `on` (fmap (int . _rankedHeight) . _cutHashes)) curHashes x
        when r $ cutIdLogFunctionText x logger Debug "skip old cut"
        return r

    isCurrent x = do
        curHashes <- cutToCutHashes Nothing <$> readTVarIO cutVar
        let r = _cutHashes curHashes == _cutHashes x
        when r $ cutIdLogFunctionText x logger Debug "skip current cut"
        return r

-- | Stream of most recent cuts. This stream does not generally include the full
-- history of cuts. When no cuts are demanded from the stream or new cuts are
-- produced faster than they are consumed from the stream, the stream skips over
-- cuts and always returns the latest cut in the db.
--
cutStream :: MonadIO m => CutDb l -> S.Stream (Of Cut) m r
cutStream db = liftIO (_cut db) >>= \c -> S.yield c >> go c
  where
    go cur = do
        new <- liftIO $ atomically $ do
            c' <- _cutStm db
            check (c' /= cur)
            return c'
        S.yield new
        go new

-- | Given a stream of cuts, produce a stream of all blocks included in those
-- cuts. Blocks of the same chain are sorted by block height.
--
cutStreamToHeaderStream
    :: forall m r l
    . MonadIO m
    => HasVersion
    => CutDb l
    -> S.Stream (Of Cut) m r
    -> S.Stream (Of BlockHeader) m r
cutStreamToHeaderStream db s = S.for (go Nothing s) $ \(T2 p n) ->
    S.foldrT
        (\(cid, a, b) x -> void $ S.mergeOn uniqueBlockNumber x (branch cid a b))
        (iforM_ (HM.intersectionWith (,) (_cutMap p) (_cutMap n)) $ \cid (x, y) ->
            S.yield (cid, x, y)
        )
  where
    go :: Maybe Cut -> S.Stream (Of Cut) m r -> S.Stream (Of (T2 Cut Cut)) m r
    go c st = lift (S.next st) >>= \case
        Left r -> return r
        Right (x, t) -> case c of
            Nothing -> go (Just x) t
            Just a -> S.yield (T2 a x) >> go (Just x) t

    branch :: ChainId -> BlockHeader -> BlockHeader -> S.Stream (Of BlockHeader) m ()
    branch cid p n = hoist liftIO $ getBranch
        (db ^?! cutDbBlockHeaderDb cid)
        (HS.singleton $ LowerBound $ view blockHash p)
        (HS.singleton $ UpperBound $ view blockHash n)

-- | Given a stream of cuts, produce a stream of all blocks included in those
-- cuts. Blocks of the same chain are sorted by block height.
--
cutStreamToHeaderDiffStream
    :: forall m r l
    . MonadIO m
    => HasVersion
    => CutDb l
    -> S.Stream (Of Cut) m r
    -> S.Stream (Of (Either BlockHeader BlockHeader)) m r
cutStreamToHeaderDiffStream db s = S.for (cutUpdates Nothing s) $ \(T2 p n) ->
    S.foldrT
        (\(cid, a, b) x -> void $ S.mergeOn toOrd x (branch cid a b))
        (iforM_ (HM.intersectionWith (,) (_cutMap p) (_cutMap n)) $ \cid (x, y) ->
            S.yield (cid, x, y)
        )
  where
    cutUpdates :: Maybe Cut -> S.Stream (Of Cut) m r -> S.Stream (Of (T2 Cut Cut)) m r
    cutUpdates c st = lift (S.next st) >>= \case
        Left r -> return r
        Right (x, t) -> case c of
            Nothing -> cutUpdates (Just x) t
            Just a -> S.yield (T2 a x) >> cutUpdates (Just x) t

    branch :: ChainId -> BlockHeader -> BlockHeader -> S.Stream (Of (Either BlockHeader BlockHeader)) m ()
    branch cid p n = hoist liftIO
        $ branchDiff_ (db ^?! cutDbBlockHeaderDb cid) p n
        & these2Either
        & void

    these2Either = flip S.for $ \case
        This a -> S.each [Left a]
        That a -> S.each [Right a]
        These a b -> S.each [Left a, Right b]

    toOrd :: Either BlockHeader BlockHeader -> Int
    toOrd (Right a) = int $ uniqueBlockNumber a
    toOrd (Left a) = negate $ int (uniqueBlockNumber a)

-- | Assign each block a unique number that respects the order of blocks in a
-- chain:
--
-- @chainId + blockHeight * graphOrder@
--
uniqueBlockNumber :: HasVersion => BlockHeader -> Natural
uniqueBlockNumber bh
    = chainIdInt (_chainId bh) + int (view blockHeight bh) * order (_chainGraph bh)

blockStream :: (MonadIO m, HasVersion) => CutDb l -> S.Stream (Of BlockHeader) m r
blockStream db = cutStreamToHeaderStream db $ cutStream db

blockDiffStream :: (MonadIO m, HasVersion) => CutDb l -> S.Stream (Of (Either BlockHeader BlockHeader)) m r
blockDiffStream db = cutStreamToHeaderDiffStream db $ cutStream db

cutHashesToBlockHeaderMap
    :: HasVersion
    => Logger logger
    => CutDbParams
    -> logger
    -> WebBlockHeaderStore logger
    -> ChainMap ConfiguredPayloadProvider
    -> CutHashes
    -> IO (Maybe (HM.HashMap ChainId BlockHeader))
        -- ^ The 'Left' value holds missing hashes, the 'Right' value holds
        -- a 'Cut'.
cutHashesToBlockHeaderMap conf logfun headerStore providers hs =
    trace logg "Chainweb.CutDB.cutHashesToBlockHeaderMap" hsid 1 $ do
        timeout (_cutDbParamsFetchTimeout conf) go >>= \case
            Nothing -> do
                let cutOriginText = case _cutHashesLocalPayload hs of
                        Nothing -> "from " <> maybe "unknown origin" (\p -> "origin " <> toText p) origin
                        Just _ -> "which was locally mined - the mining loop will stall until unstuck by another miner"

                logg (maybe Warn (const Error) (_cutHashesLocalPayload hs))
                    $ "Timeout while processing cut "
                        <> cutIdToTextShort hsid
                        <> " at height " <> sshow (_cutHashesHeight hs)
                        <> " from origin " <> cutOriginText
                return Nothing
            Just (Left missing) -> do
                logg Warn $ "Failed to get prerequisites for some blocks. Missing: "
                    <> renderMissing missing
                return Nothing
            Just (Right headers) -> do
                return (Just headers)
  where
    logg :: LogFunction
    logg = cutIdLogFunction hs logfun

    renderMissing :: HM.HashMap (ChainValue RankedBlockHash) [SomeException] -> T.Text
    renderMissing m = encodeToText $ HM.mapWithKey renderMiss m
    renderMiss :: ChainValue RankedBlockHash -> [SomeException] -> Value
    renderMiss cv es = object
        [ "blockHash" .= toText cv
        , "error" .= (renderExceptionJson [Renderer clientErrorValue] <$> es)
        ]

    hsid = _cutId hs
    go = do

        -- We collect candidate payloads locally in a table and provide them to
        -- the payload provider by inserting them in the evluation contexts for
        -- the respective blocks
        --
        plds <- emptyTable
        tableInsertBatch plds $ HM.toList $ _cutHashesPayloads hs

        hdrs <- emptyTable
        casInsertBatch hdrs $ HM.elems $ _cutHashesHeaders hs

        -- for better error messages on validation failure
        let localPayload = _cutHashesLocalPayload hs

        (missing, headers) <- fmap partitionEithers
            $ forConcurrently (uncurry ChainValue <$> HM.toList (_cutHashes hs))
            $ tryGetBlockHeader hdrs plds localPayload
        if null missing
          then
            return $ Right $! HM.fromList $ (\hdr -> (_chainId hdr, hdr)) <$> headers
          else do
            when (isJust $ _cutHashesLocalPayload hs) $
                logg @T.Text Error
                    "error validating locally mined cut; the mining loop will stall until unstuck by another mining node"
            return $ Left $! HM.fromList missing

    origin = _cutOrigin hs
    priority = Priority (- int (_cutHashesHeight hs))

    -- tryGetBlockHeader hdrs localPayload cv@(cid, _) =
    --     (Right <$> mapM
    --         (getBlockHeader minerInfo headerStore hdrs providers localPayload cid priority origin) cv)
    --             `catch` \case
    --                 (TreeDbKeyNotFound{} :: TreeDbException BlockHeaderDb) ->
    --                     return (Left cv)
    --                 e -> throwM e

    tryGetBlockHeader
        :: HashMapTable BlockHash BlockHeader
        -> HashMapTable BlockPayloadHash EncodedPayloadData
        -> Maybe (BlockPayloadHash, EncodedPayloadOutputs)
        -> ChainValue RankedBlockHash
        -> IO (Either (ChainValue RankedBlockHash, [SomeException]) BlockHeader)
    tryGetBlockHeader hdrs plds localPayload cv = do
        Right <$> getBlockHeader
            headerStore
            hdrs
            plds
            providers
            localPayload
            priority
            origin
            cv
        `catch` \(TaskFailed es) -> return (Left (cv, es))

-- -------------------------------------------------------------------------- --
-- Membership Queries

memberOfHeader
    :: HasVersion
    => CutDb l
    -> ChainId
    -> BlockHash
        -- ^ the block hash to look up (the member)
    -> BlockHeader
        -- ^ the context, i.e. the branch of the chain that contains the member
    -> IO Bool
memberOfHeader db cid h ctx = do
    lookup chainDb h >>= \case
        Nothing -> return False
        Just !lh -> seekAncestor chainDb ctx (int $ view blockHeight lh) >>= \case
            Nothing -> return False
            Just x -> return $ view blockHash x == h
  where
    chainDb = db ^?! cutDbWebBlockHeaderDb . ixg cid

member
    :: HasVersion
    => CutDb l
    -> ChainId
    -> BlockHash
    -> IO Bool
member db cid h = do
    th <- maxEntry chainDb
    memberOfHeader db cid h th
  where
    chainDb = db ^?! cutDbWebBlockHeaderDb . ixg cid

-- -------------------------------------------------------------------------- --
-- Some CutDB

-- | 'CutDb' with type level 'ChainwebVersionName'
--
newtype CutDbT l (v :: ChainwebVersionT) = CutDbT (CutDb l)
    deriving (Generic)

data SomeCutDb = forall l v . KnownChainwebVersionSymbol v => SomeCutDb (CutDbT l v)

someCutDbVal :: HasVersion => CutDb l -> SomeCutDb
someCutDbVal db = case implicitVersion of
    FromSingChainwebVersion (SChainwebVersion :: Sing v) ->
        SomeCutDb $ CutDbT @_ @v db

-- -------------------------------------------------------------------------- --
-- Queue Stats

data QueueStats = QueueStats
    { _queueStatsCutQueueSize :: !Natural
    , _queueStatsBlockHeaderQueueSize :: !Natural
    , _queueStatsBlockHeaderTaskMapSize :: !Natural
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, ToJSON)

getQueueStats :: CutDb l -> IO QueueStats
getQueueStats db = QueueStats
    <$> cutDbQueueSize db
    <*> pQueueSize (_webBlockHeaderStoreQueue $ view cutDbWebBlockHeaderStore db)
    <*> (int <$> TM.size (_webBlockHeaderStoreMemo $ view cutDbWebBlockHeaderStore db))

-- -------------------------------------------------------------------------- --
-- Logging Utils

chainLogger :: Logger logger => HasChainId c => c -> logger -> logger
chainLogger cid = addLabel ("chain", toText (_chainId cid))

providerLogger :: Logger logger => HasPayloadProviderType p => p -> logger -> logger
providerLogger p = addLabel ("provider", toText (_payloadProviderType p))

cutLogger :: Logger logger => HasCutId c => c -> logger -> logger
cutLogger c = addLabel ("cut", cutIdToTextShort (_cutId c))

cutIdLogFunction
    :: HasCutId c
    => Logger logger
    => c
    -> logger
    -> LogFunction
cutIdLogFunction c = logFunction . cutLogger c

cutIdLogFunctionText
    :: HasCutId c
    => Logger logger
    => c
    -> logger
    -> LogFunctionText
cutIdLogFunctionText = cutIdLogFunction
