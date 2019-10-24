{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.CutDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.CutDB
(
-- * CutConfig
  CutDbConfig(..)
, cutDbConfigInitialCut
, cutDbConfigInitialCutFile
, cutDbConfigBufferSize
, cutDbConfigLogLevel
, cutDbConfigTelemetryLevel
, cutDbConfigUseOrigin
, defaultCutDbConfig
, farAheadThreshold

-- * Cut Hashes Table
, cutHashesTable

-- * CutDb
, CutDb
, cutDbWebBlockHeaderDb
, cutDbWebBlockHeaderStore
, cutDbBlockHeaderDb
, cutDbPayloadCas
, cutDbPayloadStore
, cutDbStore
, member
, cut
, _cut
, _cutStm
, cutStm
, awaitNewCut
, awaitNewCutByChainId
, cutStream
, addCutHashes
, withCutDb
, startCutDb
, stopCutDb
, cutDbQueueSize
, blockStream
, blockDiffStream
, cutStreamToHeaderStream
, cutStreamToHeaderDiffStream

-- * Some CutDb
, CutDbT(..)
, SomeCutDb(..)
, someCutDbVal
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Lens hiding ((:>))
import Control.Monad hiding (join)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.STM

import Data.CAS.HashMap
import Data.Foldable
import Data.Function
import Data.Functor.Of
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.LogMessage
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Reflection hiding (int)
import qualified Data.Text as T
import Data.These
import Data.Tuple.Strict
import qualified Data.Vector as V

import GHC.Generics hiding (to)

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB.Types
import Chainweb.Graph
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec, check)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.CAS
import Data.CAS.RocksDB
import Data.PQueue
import Data.Singletons

import P2P.TaskQueue

import Utils.Logging.Trace

-- -------------------------------------------------------------------------- --
-- Cut DB Configuration

data CutDbConfig = CutDbConfig
    { _cutDbConfigInitialCut :: !Cut
    , _cutDbConfigInitialCutFile :: !(Maybe FilePath)
    , _cutDbConfigBufferSize :: !Natural
    , _cutDbConfigLogLevel :: !LogLevel
    , _cutDbConfigTelemetryLevel :: !LogLevel
    , _cutDbConfigUseOrigin :: !Bool
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''CutDbConfig

defaultCutDbConfig :: ChainwebVersion -> CutDbConfig
defaultCutDbConfig v = CutDbConfig
    { _cutDbConfigInitialCut = genesisCut v
    , _cutDbConfigInitialCutFile = Nothing
    , _cutDbConfigBufferSize = (order g ^ (2 :: Int)) * diameter g
    , _cutDbConfigLogLevel = Warn
    , _cutDbConfigTelemetryLevel = Warn
    , _cutDbConfigUseOrigin = True
    }
  where
    g = _chainGraph v

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
-- that are at most 'forAheadThreshold' blocks in the ahead. Otherwise the
-- pulled blocks would be immediately rejected by the cut processing pipeline
-- 'processCuts' below in the module and the node would never be able to join
-- the consensus of the network.
--
-- NOTE: THIS NUMBER MUST BE STRICTLY LARGER THAN THE RESPECTIVE LIMIT IN
-- 'CutDB.Sync'
--
farAheadThreshold :: Int
farAheadThreshold = 2000

-- -------------------------------------------------------------------------- --
-- CutHashes Table

cutHashesTable :: RocksDb -> RocksDbCas CutHashes
cutHashesTable rdb = newCas rdb valueCodec keyCodec ["CutHashes"]
  where
    keyCodec = Codec
        (\(a,b,c) -> runPut $ encodeBlockHeightBe a >> encodeBlockWeightBe b >> encodeCutId c)
        (runGet $ (,,) <$> decodeBlockHeightBe <*> decodeBlockWeightBe <*> decodeCutId)
    valueCodec = Codec encodeToByteString decodeStrictOrThrow'

cutDbPayloadCas :: Getter (CutDb cas) (PayloadDb cas)
cutDbPayloadCas = to $ _webBlockPayloadStoreCas . _cutDbPayloadStore
{-# INLINE cutDbPayloadCas #-}

cutDbPayloadStore :: Getter (CutDb cas) (WebBlockPayloadStore cas)
cutDbPayloadStore = to _cutDbPayloadStore
{-# INLINE cutDbPayloadStore #-}

cutDbStore :: Getter (CutDb cas) (RocksDbCas CutHashes)
cutDbStore = to _cutDbStore
{-# INLINE cutDbStore #-}

-- We export the 'WebBlockHeaderDb' read-only
--
cutDbWebBlockHeaderDb :: Getter (CutDb cas) WebBlockHeaderDb
cutDbWebBlockHeaderDb = to $ _webBlockHeaderStoreCas . _cutDbHeaderStore
{-# INLINE cutDbWebBlockHeaderDb #-}

cutDbWebBlockHeaderStore :: Getter (CutDb cas) WebBlockHeaderStore
cutDbWebBlockHeaderStore = to _cutDbHeaderStore
{-# INLINE cutDbWebBlockHeaderStore #-}

-- | Access the blockerheaderdb via the cutdb for a given chain id
--
cutDbBlockHeaderDb :: HasChainId cid => cid -> Fold (CutDb cas) BlockHeaderDb
cutDbBlockHeaderDb cid = cutDbWebBlockHeaderDb . ixg (_chainId cid)

-- | Get the current 'Cut', which represent the latest chainweb state.
--
-- This the main API method of chainweb-consensus.
--
_cut :: CutDb cas -> IO Cut
_cut = readTVarIO . _cutDbCut
{-# INLINE _cut #-}

-- | Get the current 'Cut', which represent the latest chainweb state.
--
-- This the main API method of chainweb-consensus.
--
cut :: Getter (CutDb cas) (IO Cut)
cut = to _cut

addCutHashes :: CutDb cas -> CutHashes -> IO ()
addCutHashes db = pQueueInsertLimit (_cutDbQueue db) (_cutDbQueueSize db) . Down

-- | An 'STM' version of '_cut'.
--
-- @_cut db@ is generally more efficient than as @atomically (_cut db)@.
--
_cutStm :: CutDb cas -> STM Cut
_cutStm = readTVar . _cutDbCut

-- | An 'STM' version of 'cut'.
--
-- @_cut db@ is generally more efficient than as @atomically (_cut db)@.
--
cutStm :: Getter (CutDb cas) (STM Cut)
cutStm = to _cutStm

-- | A common idiom to spin while waiting for a guaranteed new `Cut`, different
-- from the given one.
--
awaitNewCut :: CutDb cas -> Cut -> IO Cut
awaitNewCut cdb c = atomically $ do
    c' <- _cutStm cdb
    when (c' == c) retry
    return c'

-- | As in `awaitNewCut`, but only updates when the specified `ChainId` has
-- grown.
--
awaitNewCutByChainId :: CutDb cas -> ChainId -> Cut -> IO Cut
awaitNewCutByChainId cdb cid c = atomically $ do
    c' <- _cutStm cdb
    let !b0 = HM.lookup cid $ _cutMap c
        !b1 = HM.lookup cid $ _cutMap c'
    when (b1 == b0) retry
    return c'

member :: CutDb cas -> ChainId -> BlockHash -> IO Bool
member db cid h = do
    th <- maxEntry chainDb
    lookup chainDb h >>= \case
        Nothing -> return False
        Just lh -> do
            fh <- forkEntry chainDb th lh
            return $! fh == lh
  where
    chainDb = db ^?! cutDbWebBlockHeaderDb . ixg cid

cutDbQueueSize :: CutDb cas -> IO Natural
cutDbQueueSize = pQueueSize . _cutDbQueue

withCutDb
    :: PayloadCas cas
    => CutDbConfig
    -> LogFunction
    -> WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> RocksDbCas CutHashes
    -> (CutDb cas -> IO a)
    -> IO a
withCutDb config logfun headerStore payloadStore cutHashesStore
    = bracket
        (startCutDb config logfun headerStore payloadStore cutHashesStore)
        stopCutDb

-- | Start a CutDB. This loads the initial cut from the database (falling back
-- to the configured initial cut loading fails) and starts the cut validation
-- pipeline.
--
-- TODO: Instead of falling back to the configured initial cut, which usually is
-- the genesis cut, we could instead try to walk back in history until we find a
-- cut that succeed to load.
--
startCutDb
    :: PayloadCas cas
    => CutDbConfig
    -> LogFunction
    -> WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> RocksDbCas CutHashes
    -> IO (CutDb cas)
startCutDb config logfun headerStore payloadStore cutHashesStore = mask_ $ do
    logg Debug "obtain initial cut"
    cutVar <- newTVarIO =<< initialCut
    c <- readTVarIO cutVar
    logg Info $ "got initial cut: " <> sshow c
    queue <- newEmptyPQueue
    cutAsync <- asyncWithUnmask $ \u -> u $ processor queue cutVar
    logg Info "CutDB started"
    return $! CutDb
        { _cutDbCut = cutVar
        , _cutDbQueue = queue
        , _cutDbAsync = cutAsync
        , _cutDbLogFunction = logfun
        , _cutDbHeaderStore = headerStore
        , _cutDbPayloadStore = payloadStore
        , _cutDbQueueSize = _cutDbConfigBufferSize config
        , _cutDbStore = cutHashesStore
        }
  where
    logg = logfun @T.Text
    wbhdb = _webBlockHeaderStoreCas headerStore
    processor :: PQueue (Down CutHashes) -> TVar Cut -> IO ()
    processor queue cutVar = runForever logfun "CutDB" $
        processCuts logfun headerStore payloadStore cutHashesStore queue cutVar

    -- TODO: The following code doesn't perform any validation of the cut.
    -- 'joinIntoHeavier_' may stil be slow on large dbs. Eventually, we should
    -- support different levels of validation:
    --
    -- 1. nothing
    -- 2. braiding
    -- 3. exitence of dependencies
    -- 4. full validation
    --
    initialCut = withTableIter (_getRocksDbCas cutHashesStore) $ \it -> do
        tableIterLast it
        go it
      where
        -- TODO: should we limit the search to a certain number of attempts
        -- or iterate in increasinly larger steps?
        go it = tableIterValue it >>= \case
            Nothing -> do
                logg Debug "using intial cut from cut db configuration"
                return $! _cutDbConfigInitialCut config
            Just ch -> try (lookupCutHashes wbhdb ch) >>= \case
                Left (e@(TreeDbKeyNotFound _) :: TreeDbException BlockHeaderDb) -> do
                    logfun @T.Text Warn
                        $ "Unable to load cut at height " <>  sshow (_cutHashesHeight ch)
                        <> " from database."
                        <> " Error: " <> sshow e <> "."
                        <> " The database might be corrupted. Falling back to previous cut."
                    tableIterPrev it
                    go it
                Left e -> throwM e
                Right hm -> do
                    logg Debug $ "joining intial cut from cut db configuration with cut that was loaded from the database " <> sshow hm
                    joinIntoHeavier_
                        (_webBlockHeaderStoreCas headerStore)
                        hm
                        (_cutMap $ _cutDbConfigInitialCut config)

-- | Stop the cut validation pipeline.
--
stopCutDb :: CutDb cas -> IO ()
stopCutDb db = cancel (_cutDbAsync db)

-- | Lookup the BlockHeaders for a CutHashes structure. Throws an exception if
-- the lookup for some BlockHash in the input CutHashes.
--
lookupCutHashes
    :: WebBlockHeaderDb
    -> CutHashes
    -> IO (HM.HashMap ChainId BlockHeader)
lookupCutHashes wbhdb hs = do
    flip itraverse (_cutHashes hs) $ \cid (_, h) -> do
        give wbhdb $ lookupWebBlockHeaderDb cid h

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
    :: PayloadCas cas
    => LogFunction
    -> WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> RocksDbCas CutHashes
    -> PQueue (Down CutHashes)
    -> TVar Cut
    -> IO ()
processCuts logFun headerStore payloadStore cutHashesStore queue cutVar = queueToStream
    & S.chain (\c -> loggc Debug c $ "start processing")
    & S.filterM (fmap not . isVeryOld)
    & S.filterM (fmap not . farAhead)
    & S.filterM (fmap not . isOld)
    & S.filterM (fmap not . isCurrent)
    & S.chain (\c -> loggc Debug c $ "fetch all prerequesites")
    & S.mapM (cutHashesToBlockHeaderMap logFun headerStore payloadStore)
    & S.chain (either
        (\c -> loggc Warn c "failed to get prerequesites for some blocks")
        (\c -> loggc Debug c "got all prerequesites")
        )
    & S.concat
        -- ignore left values for now

    -- using S.scanM would be slightly more efficient (one pointer dereference)
    -- by keeping the value of cutVar in memory. We use the S.mapM variant with
    -- an redundant 'readTVarIO' because it is eaiser to read.
    & S.mapM_ (\newCut -> do
        curCut <- readTVarIO cutVar
        !resultCut <- trace logFun "Chainweb.CutDB.processCuts._joinIntoHeavier" () 1
            $ joinIntoHeavier_ hdrStore (_cutMap curCut) newCut
        casInsert cutHashesStore (cutToCutHashes Nothing resultCut)
        atomically $ writeTVar cutVar resultCut
        loggc Info resultCut "published cut"
        )
  where
    loggc :: HasCutId c => LogLevel -> c -> T.Text -> IO ()
    loggc l c msg = logFun @T.Text l $  "cut " <> cutIdToTextShort (_cutId c) <> ": " <> msg

    hdrStore = _webBlockHeaderStoreCas headerStore

    graph = _chainGraph headerStore

    threshold :: Int
    threshold = int $ 2 * diameter graph * order graph

    queueToStream = do
        Down a <- liftIO (pQueueRemove queue)
        S.yield a
        queueToStream

    -- FIXME: this is problematic. We should drop these before they are
    -- added to the queue, to prevent the queue becoming stale.
    farAhead x = do
        !h <- _cutHeight <$> readTVarIO cutVar
        let r = (int (_cutHashesHeight x) - farAheadThreshold) >= int h
        when r $ loggc Debug x
            $ "skip far ahead cut. Current height: " <> sshow h
            <> ", got: " <> sshow (_cutHashesHeight x)
        return r

    isVeryOld x = do
        !h <- _cutHeight <$> readTVarIO cutVar
        let r = int (_cutHashesHeight x) <= (int h - threshold)
        when r $ loggc Debug x "skip very old cut"
        return r

    isOld x = do
        curHashes <- cutToCutHashes Nothing <$> readTVarIO cutVar
        let r = all (>= (0 :: Int)) $ (HM.unionWith (-) `on` (fmap (int . fst) . _cutHashes)) curHashes x
        when r $ loggc Debug x "skip old cut"
        return r

    isCurrent x = do
        curHashes <- cutToCutHashes Nothing <$> readTVarIO cutVar
        let r = _cutHashes curHashes == _cutHashes x
        when r $ loggc Debug x "skip current cut"
        return r

-- | Stream of most recent cuts. This stream does not generally include the full
-- history of cuts. When no cuts are demanded from the stream or new cuts are
-- produced faster than they are consumed from the stream, the stream skips over
-- cuts and always returns the latest cut in the db.
--
cutStream :: MonadIO m => CutDb cas -> S.Stream (Of Cut) m r
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
    :: forall m cas r
    . MonadIO m
    => CutDb cas
    -> S.Stream (Of Cut) m r
    -> S.Stream (Of BlockHeader) m r
cutStreamToHeaderStream db s = S.for (go Nothing s) $ \(T2 p n) ->
    S.foldrT
        (\(cid, a, b) x -> void $ S.mergeOn uniqueBlockNumber x (branch cid a b))
        (S.each $ zipCuts p n)
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
        (HS.singleton $ LowerBound $ _blockHash p)
        (HS.singleton $ UpperBound $ _blockHash n)

-- | Given a stream of cuts, produce a stream of all blocks included in those
-- cuts. Blocks of the same chain are sorted by block height.
--
cutStreamToHeaderDiffStream
    :: forall m cas r
    . MonadIO m
    => CutDb cas
    -> S.Stream (Of Cut) m r
    -> S.Stream (Of (Either BlockHeader BlockHeader)) m r
cutStreamToHeaderDiffStream db s = S.for (cutUpdates Nothing s) $ \(T2 p n) ->
    S.foldrT
        (\(cid, a, b) x -> void $ S.mergeOn toOrd x (branch cid a b))
        (S.each $ zipCuts p n)
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
    toOrd (Left a) = 0 - int (uniqueBlockNumber a)

-- | Assign each block a unique number that respects the order of blocks in a
-- chain:
--
-- @chainId + blockHeight * graphOrder@
--
uniqueBlockNumber :: BlockHeader -> Natural
uniqueBlockNumber bh
    = chainIdInt (_chainId bh) + (int $ _blockHeight bh) * (order $ _chainGraph bh)

blockStream :: MonadIO m => CutDb cas -> S.Stream (Of BlockHeader) m r
blockStream db = cutStreamToHeaderStream db $ cutStream db

blockDiffStream :: MonadIO m => CutDb cas -> S.Stream (Of (Either BlockHeader BlockHeader)) m r
blockDiffStream db = cutStreamToHeaderDiffStream db $ cutStream db

cutHashesToBlockHeaderMap
    :: PayloadCas cas
    => LogFunction
    -> WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> CutHashes
    -> IO (Either (HM.HashMap ChainId BlockHash) (HM.HashMap ChainId BlockHeader))
        -- ^ The 'Left' value holds missing hashes, the 'Right' value holds
        -- a 'Cut'.
cutHashesToBlockHeaderMap logfun headerStore payloadStore hs =
    trace logfun "Chainweb.CutDB.cutHashesToBlockHeaderMap" (_cutId hs) 1 $ do
        plds <- emptyCas
        casInsertBatch plds $ V.fromList $ HM.elems $ _cutHashesPayloads hs

        hdrs <- emptyCas
        casInsertBatch hdrs $ V.fromList $ HM.elems $ _cutHashesHeaders hs

        (headers :> missing) <- S.each (HM.toList $ _cutHashes hs)
            & S.map (fmap snd)
            & S.mapM (tryGetBlockHeader hdrs plds)
            & S.partitionEithers
            & S.fold_ (\x (cid, h) -> HM.insert cid h x) mempty id
            & S.fold (\x (cid, h) -> HM.insert cid h x) mempty id
        if null missing
            then return $! Right headers
            else return $! Left missing
  where
    origin = _cutOrigin hs
    priority = Priority (- int (_cutHashesHeight hs))

    tryGetBlockHeader hdrs plds cv@(cid, _) =
        (Right <$> mapM (getBlockHeader headerStore payloadStore hdrs plds cid priority origin) cv)
            `catch` \case
                (TreeDbKeyNotFound{} :: TreeDbException BlockHeaderDb) ->
                    return $! Left cv
                e -> throwM e

-- -------------------------------------------------------------------------- --
-- Some CutDB

-- | 'CutDb' with type level 'ChainwebVersion'
--
newtype CutDbT cas (v :: ChainwebVersionT) = CutDbT (CutDb cas)
    deriving (Generic)

data SomeCutDb cas = forall v . KnownChainwebVersionSymbol v => SomeCutDb (CutDbT cas v)

someCutDbVal :: ChainwebVersion -> CutDb cas -> SomeCutDb cas
someCutDbVal (FromSing (SChainwebVersion :: Sing v)) db = SomeCutDb $ CutDbT @_ @v db
