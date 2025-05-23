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
, cutDbParamsPruningFrequency
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
, memberOfM

-- * Some CutDb
, CutDbT(..)
, SomeCutDb(..)
, someCutDbVal

-- * Queue Statistics
, QueueStats(..)
, getQueueStats
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.STM
import Control.Monad.Trans.Resource

import Data.Aeson (ToJSON)
import Data.Foldable
import Data.Function
import Data.Functor.Of
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.LogMessage
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Data.These

import GHC.Generics hiding (to)

import Numeric.Natural

import Prelude hiding (lookup)

import Streaming.Prelude qualified as S

import System.LogLevel
import System.Random.MWC qualified as Prob
import System.Timeout

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.Cut.Create
import Chainweb.Graph
import Chainweb.PayloadProvider
import Chainweb.Storage.Table
import Chainweb.Storage.Table.HashMap
import Chainweb.Storage.Table.RocksDB
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec, check)
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB

import Data.PQueue
import Data.TaskMap qualified as TM

import P2P.TaskQueue

import Utils.Logging.Trace

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
    , _cutDbParamsPruningFrequency :: !BlockHeight
    -- ^ After how many blocks do we prune cuts (on average)?
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
    , _cutDbParamsAvgBlockHeightPruningDepth = 5000
    , _cutDbParamsPruningFrequency = 10000
    , _cutDbParamsReadOnly = False
    }
  where
    g = chainGraphAt (maxBound @BlockHeight)

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
-- that are at most 'farAheadThreshold' blocks ahead. Otherwise the pulled
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
farAheadThreshold = 2

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
data CutDb = CutDb
    { _cutDbCut :: !(TVar Cut)
    , _cutDbQueue :: !(PQueue (Down CutHashes))
    , _cutDbAsync :: !(Async ())
    , _cutDbLogFunction :: !LogFunction
    , _cutDbHeaderStore :: !WebBlockHeaderStore
    , _cutDbPayloadProviders :: !(ChainMap ConfiguredPayloadProvider)
    , _cutDbCutStore :: !(Casify RocksDbTable CutHashes)
    , _cutDbQueueSize :: !Natural
    , _cutDbReadOnly :: !Bool
    , _cutDbFastForwardHeightLimit :: !(Maybe BlockHeight)
    }

cutDbPayloadProviders :: Getter CutDb (ChainMap ConfiguredPayloadProvider)
cutDbPayloadProviders = to _cutDbPayloadProviders
{-# INLINE cutDbPayloadProviders #-}

-- We export the 'WebBlockHeaderDb' read-only
--
cutDbWebBlockHeaderDb :: Getter CutDb WebBlockHeaderDb
cutDbWebBlockHeaderDb = to $ _webBlockHeaderStoreCas . _cutDbHeaderStore
{-# INLINE cutDbWebBlockHeaderDb #-}

cutDbWebBlockHeaderStore :: Getter CutDb WebBlockHeaderStore
cutDbWebBlockHeaderStore = to _cutDbHeaderStore
{-# INLINE cutDbWebBlockHeaderStore #-}

-- | Access the blockerheaderdb via the cutdb for a given chain id
--
cutDbBlockHeaderDb :: HasChainId cid => cid -> Fold CutDb BlockHeaderDb
cutDbBlockHeaderDb cid = cutDbWebBlockHeaderDb . ixg (_chainId cid)

-- | Get the current 'Cut', which represent the latest chainweb state.
--
-- This the main API method of chainweb-consensus.
--
_cut :: CutDb -> IO Cut
_cut = readTVarIO . _cutDbCut
{-# INLINE _cut #-}

-- | Get the current 'Cut', which represent the latest chainweb state.
--
-- This the main API method of chainweb-consensus.
--
cut :: Getter CutDb (IO Cut)
cut = to _cut

addCutHashes :: CutDb -> CutHashes -> IO ()
addCutHashes db = pQueueInsertLimit (_cutDbQueue db) (_cutDbQueueSize db) . Down

-- | An 'STM' version of '_cut'.
--
-- @_cut db@ is generally more efficient than as @atomically (_cut db)@.
--
_cutStm :: CutDb -> STM Cut
_cutStm = readTVar . _cutDbCut

-- | An 'STM' version of 'cut'.
--
-- @_cut db@ is generally more efficient than as @atomically (_cut db)@.
--
cutStm :: Getter CutDb (STM Cut)
cutStm = to _cutStm

-- | A common idiom to spin while waiting for a guaranteed new `Cut`, different
-- from the given one.
--
awaitNewCutStm :: CutDb -> Cut -> STM Cut
awaitNewCutStm cdb c = do
    c' <- _cutStm cdb
    when (c' == c) retry
    return c'

-- | A common idiom to spin while waiting for a guaranteed new `Cut`, different
-- from the given one.
--
awaitNewCut :: CutDb -> Cut -> IO Cut
awaitNewCut cdb c = atomically $ awaitNewCutStm cdb c

-- | As in `awaitNewCut`, but only updates when the specified `ChainId` has
-- grown.
--
awaitNewCutByChainId :: CutDb -> ChainId -> Cut -> IO Cut
awaitNewCutByChainId cdb cid c = atomically $ awaitNewCutByChainIdStm cdb cid c
{-# INLINE awaitNewCutByChainId #-}

-- | As in `awaitNewCut`, but only updates when the header at the specified
-- `ChainId` has changed, and only returns that new header.
awaitNewBlock :: CutDb -> ChainId -> BlockHash -> IO BlockHeader
awaitNewBlock cdb cid bHash = atomically $ awaitNewBlockStm cdb cid bHash

-- | As in `awaitNewCut`, but only updates when the header at the specified
-- `ChainId` has changed, and only returns that new header.
awaitNewBlockStm :: CutDb -> ChainId -> BlockHash -> STM BlockHeader
awaitNewBlockStm cdb cid bHash = do
    c <- _cutStm cdb
    case HM.lookup cid (_cutMap c) of
        Just bh' | view blockHash bh' /= bHash -> return bh'
        _ -> retry

-- | As in `awaitNewCut`, but only updates when the specified `ChainId` has
-- grown.
--
awaitNewCutByChainIdStm :: CutDb -> ChainId -> Cut -> STM Cut
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
    deleteRangeRocksDb (unCasify cutHashesStore)
        (Nothing, Just (pruneCutHeight, 0, maxBound :: CutId))
    -- compactRangeRocksDb waits for compaction to complete which takes a while
    void $ async $
        compactRangeRocksDb (unCasify cutHashesStore)
            (Nothing, Just (pruneCutHeight, 0, maxBound :: CutId))

cutDbQueueSize :: CutDb -> IO Natural
cutDbQueueSize = pQueueSize . _cutDbQueue

withCutDb
    :: HasVersion
    => CutDbParams
    -> LogFunction
    -> WebBlockHeaderStore
    -> ChainMap ConfiguredPayloadProvider
    -> Casify RocksDbTable CutHashes
    -> ResourceT IO CutDb
withCutDb config logfun headerStore providers cutHashesStore
    = snd <$> allocate
        (startCutDb config logfun headerStore providers cutHashesStore)
        stopCutDb

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
    :: HasVersion
    => CutDbParams
    -> LogFunction
    -> WebBlockHeaderStore
    -> ChainMap ConfiguredPayloadProvider
    -> Casify RocksDbTable CutHashes
    -> IO CutDb
startCutDb config logfun headerStore providers cutHashesStore = mask_ $ do
    logg Debug "obtain initial cut"
    initialCut <- readInitialCut
    unless (_cutDbParamsReadOnly config) $
        deleteRangeRocksDb
            (unCasify cutHashesStore)
            (Just $ over _1 succ $ casKey $ cutToCutHashes Nothing initialCut, Nothing)
    cutVar <- newTVarIO initialCut
    c <- readTVarIO cutVar
    logg Info $ T.unlines $
        "got initial cut:" : ["    " <> block | block <- cutToTextShort c]
    queue <- newEmptyPQueue
    cutAsync <- asyncWithUnmask $ \u -> u $ processor queue cutVar
    return CutDb
        { _cutDbCut = cutVar
        , _cutDbQueue = queue
        , _cutDbAsync = cutAsync
        , _cutDbLogFunction = logfun
        , _cutDbHeaderStore = headerStore
        , _cutDbPayloadProviders = providers
        , _cutDbQueueSize = _cutDbParamsBufferSize config
        , _cutDbCutStore = cutHashesStore
        , _cutDbReadOnly = _cutDbParamsReadOnly config
        , _cutDbFastForwardHeightLimit = _cutDbParamsFastForwardHeightLimit config
        }
  where
    logg = logfun @T.Text
    wbhdb = _webBlockHeaderStoreCas headerStore

    processor :: PQueue (Down CutHashes) -> TVar Cut -> IO ()
    processor queue cutVar = runForever logfun "CutDB" $
        processCuts config logfun headerStore providers cutHashesStore queue cutVar

    readInitialCut :: IO Cut
    readInitialCut = do
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
            Left (e@(TreeDbKeyNotFound _) :: TreeDbException BlockHeaderDb) -> do
                logg Warn
                    $ "Unable to load cut at height " <>  sshow (_cutHashesHeight ch)
                    <> " from database."
                    <> " Error: " <> sshow e <> "."
                    <> " The database might be corrupted. Falling back to previous cut."
                iterPrev it
                go it
            Left e -> throwM e
            Right hm -> return hm

fastForwardCutDb :: HasVersion => CutDb -> IO ()
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
stopCutDb :: CutDb -> IO ()
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
    flip itraverse (_cutHashes hs) $ \cid (BlockHashWithHeight _ h) ->
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
    => CutDbParams
    -> LogFunction
    -> WebBlockHeaderStore
    -> ChainMap ConfiguredPayloadProvider
    -> Casify RocksDbTable CutHashes
    -> PQueue (Down CutHashes)
    -> TVar Cut
    -> IO ()
processCuts conf logFun headerStore providers cutHashesStore queue cutVar = do
    rng <- Prob.createSystemRandom
    queueToStream
        & S.chain (\c -> loggCutId logFun Debug c "start processing")
        & S.filterM (fmap not . isVeryOld)
        & S.filterM (fmap not . farAhead)
        & S.filterM (fmap not . isOld)
        & S.filterM (fmap not . isCurrent)

        & S.chain (\c -> loggCutId logFun Info c "fetching all prerequisites")
        & S.mapM (cutHashesToBlockHeaderMap conf logFun headerStore providers)
        & S.catMaybes
        -- ignore unsuccessful values for now

        -- using S.scanM would be slightly more efficient (one pointer dereference)
        -- by keeping the value of cutVar in memory. We use the S.mapM variant with
        -- an redundant 'readTVarIO' because it is easier to read.
        & S.mapM_ (\newCut -> do
            curCut <- readTVarIO cutVar
            !resultCut <- trace logFun "Chainweb.CutDB.processCuts._joinIntoHeavier" () 1
                $ joinIntoHeavier_ hdrStore (_cutMap curCut) newCut
            unless (_cutDbParamsReadOnly conf) $ do
                maybePrune rng (cutAvgBlockHeight curCut)
                loggCutId logFun Debug newCut "writing cut"
                casInsert cutHashesStore (cutToCutHashes Nothing resultCut)
            -- ensure that payload providers are in sync with the *merged*
            -- cut, so that they produce payloads on the correct parents.
            iforM_ (_cutMap resultCut) $ \cid bh -> do
                case providers ^?! atChain cid of
                    ConfiguredPayloadProvider provider -> do
                        finfo <- forkInfoForHeader hdrStore bh Nothing Nothing
                        r <- syncToBlock provider Nothing finfo
                        unless (r == _forkInfoTargetState finfo) $ do
                            error $ "unexpected result state"
                                <> "; expected: " <> sshow (_forkInfoTargetState finfo)
                                <> "; actual: " <> sshow r
                    _ -> return ()
            let cutDiff = cutDiffToTextShort curCut resultCut
            let currentCutIdMsg = T.unwords
                    [ "current cut is now"
                    , cutIdToTextShort (_cutId resultCut) <> ","
                    , "diff:"
                    ]
            let catOverflowing x xs =
                    if length xs == 1
                    then T.unwords (x : xs)
                    else T.intercalate "\n" (x : (map ("    " <>) xs))
            logFun @T.Text Info $ catOverflowing currentCutIdMsg cutDiff
            atomically $ writeTVar cutVar resultCut
            )
  where

    maybePrune rng curCutAvgBlockHeight = do
        r :: Double <- Prob.uniform rng
        when (r < 1 / int (int (_cutDbParamsPruningFrequency conf) * chainCountAt maxBound)) $
            pruneCuts logFun conf curCutAvgBlockHeight cutHashesStore

    hdrStore = _webBlockHeaderStoreCas headerStore

    queueToStream = do
        Down a <- liftIO (pQueueRemove queue)
        S.yield a
        queueToStream

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
        when r $ loggCutId logFun Debug x
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
        when r $ loggCutId logFun Debug x "skip very old cut"
            -- log at debug level because this is a common case during catchup
        return r

    -- This should be based on weight
    --
    isOld x = do
        curHashes <- cutToCutHashes Nothing <$> readTVarIO cutVar
        let r = all (>= (0 :: Int)) $ (HM.unionWith (-) `on` (fmap (int . _bhwhHeight) . _cutHashes)) curHashes x
        when r $ loggCutId logFun Debug x "skip old cut"
        return r

    isCurrent x = do
        curHashes <- cutToCutHashes Nothing <$> readTVarIO cutVar
        let r = _cutHashes curHashes == _cutHashes x
        when r $ loggCutId logFun Debug x "skip current cut"
        return r

-- | Stream of most recent cuts. This stream does not generally include the full
-- history of cuts. When no cuts are demanded from the stream or new cuts are
-- produced faster than they are consumed from the stream, the stream skips over
-- cuts and always returns the latest cut in the db.
--
cutStream :: MonadIO m => CutDb -> S.Stream (Of Cut) m r
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
    :: forall m r
    . MonadIO m
    => HasVersion
    => CutDb
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
    :: forall m r
    . MonadIO m
    => HasVersion
    => CutDb
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

blockStream :: (MonadIO m, HasVersion) => CutDb -> S.Stream (Of BlockHeader) m r
blockStream db = cutStreamToHeaderStream db $ cutStream db

blockDiffStream :: (MonadIO m, HasVersion) => CutDb -> S.Stream (Of (Either BlockHeader BlockHeader)) m r
blockDiffStream db = cutStreamToHeaderDiffStream db $ cutStream db

cutHashesToBlockHeaderMap
    :: HasVersion
    => CutDbParams
    -> LogFunction
    -> WebBlockHeaderStore
    -> ChainMap ConfiguredPayloadProvider
    -> CutHashes
    -> IO (Maybe (HM.HashMap ChainId BlockHeader))
        -- ^ The 'Left' value holds missing hashes, the 'Right' value holds
        -- a 'Cut'.
cutHashesToBlockHeaderMap conf logfun headerStore providers hs =
    trace logfun "Chainweb.CutDB.cutHashesToBlockHeaderMap" hsid 1 $ do
        timeout (_cutDbParamsFetchTimeout conf) go >>= \case
            Nothing -> do
                let cutOriginText = case _cutHashesLocalPayload hs of
                        Nothing -> "from " <> maybe "unknown origin" (\p -> "origin " <> toText p) origin
                        Just _ -> "which was locally mined - the mining loop will stall until unstuck by another miner"

                logfun (maybe Warn (\_ -> Error) (_cutHashesLocalPayload hs))
                    $ "Timeout while processing cut "
                        <> cutIdToTextShort hsid
                        <> " at height " <> sshow (_cutHashesHeight hs)
                        <> " from origin " <> cutOriginText
                return Nothing
            Just (Left missing) -> do
                loggCutId logfun Warn hs $ "Failed to get prerequisites for some blocks. Missing: " <> encodeToText missing
                return Nothing
            Just (Right headers) -> do
                return (Just headers)
  where
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

        (headers :> missing) <- S.each (HM.toList $ _cutHashes hs)
            & S.map (fmap _bhwhHash)
            & S.mapM (tryGetBlockHeader hdrs plds localPayload)
            & S.partitionEithers
            & S.fold_ (\x (cid, h) -> HM.insert cid h x) mempty id
            & S.fold (\x (cid, h) -> HM.insert cid h x) mempty id
        if null missing
        then return $! Right headers
        else do
            when (isJust $ _cutHashesLocalPayload hs) $
                logfun @Text Error
                    "error validating locally mined cut; the mining loop will stall until unstuck by another mining node"
            return $! Left missing

    origin = _cutOrigin hs
    priority = Priority (- int (_cutHashesHeight hs))

    -- tryGetBlockHeader hdrs localPayload cv@(cid, _) =
    --     (Right <$> mapM
    --         (getBlockHeader minerInfo headerStore hdrs providers localPayload cid priority origin) cv)
    --             `catch` \case
    --                 (TreeDbKeyNotFound{} :: TreeDbException BlockHeaderDb) ->
    --                     return (Left cv)
    --                 e -> throwM e

    tryGetBlockHeader hdrs plds localPayload cv@(cid, _) = do
        fmap Right $ forM cv $ getBlockHeader
            headerStore
            hdrs
            plds
            providers
            localPayload
            cid
            priority
            origin
        `catch` \case
            (TreeDbKeyNotFound{} :: TreeDbException BlockHeaderDb) ->
                return (Left cv)
            e -> throwM e

-- -------------------------------------------------------------------------- --
-- Membership Queries

memberOfHeader
    :: HasVersion
    => CutDb
    -> ChainId
    -> BlockHash
        -- ^ the block hash to look up (the member)
    -> BlockHeader
        -- ^ the context, i.e. the branch of the chain that contains the member
    -> IO Bool
memberOfHeader db cid h ctx = do
    lookup chainDb h >>= \case
        Nothing -> return False
        Just lh -> seekAncestor chainDb ctx (int $ view blockHeight lh) >>= \case
            Nothing -> return False
            Just x -> return $ view blockHash x == h
  where
    chainDb = db ^?! cutDbWebBlockHeaderDb . ixg cid

memberOfM
    :: HasVersion
    => CutDb
    -> ChainId
    -> BlockHash
        -- ^ the block hash to look up (the member)
    -> BlockHash
        -- ^ the context, i.e. the branch of the chain that contains the member
    -> IO Bool
memberOfM db cid h ctx = do
    th <- lookupM chainDb ctx
    memberOfHeader db cid h th
  where
    chainDb = db ^?! cutDbWebBlockHeaderDb . ixg cid

member
    :: HasVersion
    => CutDb
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
newtype CutDbT (v :: ChainwebVersionT) = CutDbT CutDb
    deriving (Generic)

data SomeCutDb = forall v . KnownChainwebVersionSymbol v => SomeCutDb (CutDbT v)

someCutDbVal :: HasVersion => CutDb -> SomeCutDb
someCutDbVal db = case implicitVersion of
    FromSingChainwebVersion (SChainwebVersion :: Sing v) ->
        SomeCutDb $ CutDbT @v db

-- -------------------------------------------------------------------------- --
-- Queue Stats

data QueueStats = QueueStats
    { _queueStatsCutQueueSize :: !Natural
    , _queueStatsBlockHeaderQueueSize :: !Natural
    , _queueStatsBlockHeaderTaskMapSize :: !Natural
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, ToJSON)

getQueueStats :: CutDb -> IO QueueStats
getQueueStats db = QueueStats
    <$> cutDbQueueSize db
    <*> pQueueSize (_webBlockHeaderStoreQueue $ view cutDbWebBlockHeaderStore db)
    <*> (int <$> TM.size (_webBlockHeaderStoreMemo $ view cutDbWebBlockHeaderStore db))

-- Logging
loggCutId :: HasCutId c => LogFunction -> LogLevel -> c -> T.Text -> IO ()
loggCutId logFun l c msg = logFun @T.Text l $  "cut " <> cutIdToTextShort (_cutId c) <> ": " <> msg
