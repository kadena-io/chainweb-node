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

-- * CutDb
, CutDb
, cutDbWebBlockHeaderDb
, cutDbWebBlockHeaderStore
, cutDbPayloadCas
, cutDbPayloadStore
, member
, cut
, _cut
, _cutStm
, cutStm
, consensusCut
, cutStream
, addCutHashes
, withCutDb
, startCutDb
, stopCutDb
, cutDbQueueSize

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
import Control.Monad.STM

import Data.Bool (bool)
import Data.Foldable
import Data.Function
import Data.Functor.Of
import qualified Data.HashMap.Strict as HM
import Data.LogMessage
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio ((%))
import qualified Data.Text as T

import qualified Deque.Strict as DQ

import GHC.Exts (IsList(..))
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
import Chainweb.Graph
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.TreeDB
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.PQueue
import Data.Singletons

import P2P.TaskQueue

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
-- Cut DB

-- | A bounded, strict `DQ.Deque` which drops old elements off its right side as
-- new elements are added (`DQ.cons`'d) to the left.
--
-- Not terribly robust.
--
data BDQ a = BDQ { _bdq :: !(DQ.Deque a), _bdqLimit :: !Word }

-- | Create a `BDQ` that contains @n@ copies of some `a`.
--
newBDQ :: a -> Word -> BDQ a
newBDQ a n = BDQ as n
  where
    as = fromList . take (int n) $ repeat a

-- | \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
consBDQ :: a -> BDQ a -> BDQ a
consBDQ a (BDQ q l) = BDQ (DQ.cons a $ DQ.init q) l

-- | A sample of peers and the Cut Heights they report from `Cut`s that they
-- submit to the network. This data is used to extrapolate the current height of
-- the overall network consensus.
--
newtype PeerHeights = PeerHeights (BDQ BlockHeight)

-- | The approximate average Cut Height of the network, at least as accurately
-- as we've seen from Cuts coming in from the network.
--
avgNetworkHeight :: PeerHeights -> BlockHeight
avgNetworkHeight (PeerHeights (BDQ q l)) = BlockHeight $ floor avg
  where
    avg :: Rational
    avg = int (foldl' (+) 0 q) % int l

-- | This is a singleton DB that contains the latest chainweb cut as its only
-- entry.
--
data CutDb cas = CutDb
    { _cutDbCut :: !(TVar Cut)
    , _cutDbQueue :: !(PQueue (Down CutHashes))
    , _cutPeerHeights :: !(TVar PeerHeights)
    , _cutDbAsync :: !(Async ())
    , _cutDbLogFunction :: !LogFunction
    , _cutDbHeaderStore :: !WebBlockHeaderStore
    , _cutDbPayloadStore :: !(WebBlockPayloadStore cas)
    , _cutDbQueueSize :: !Natural
    }

instance HasChainGraph (CutDb cas) where
    _chainGraph = _chainGraph . _cutDbHeaderStore
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion (CutDb cas) where
    _chainwebVersion = _chainwebVersion . _cutDbHeaderStore
    {-# INLINE _chainwebVersion #-}

cutDbPayloadCas :: Getter (CutDb cas) (PayloadDb cas)
cutDbPayloadCas = to $ _webBlockPayloadStoreCas . _cutDbPayloadStore

cutDbPayloadStore :: Getter (CutDb cas) (WebBlockPayloadStore cas)
cutDbPayloadStore = to _cutDbPayloadStore

-- We export the 'WebBlockHeaderDb' read-only
--
cutDbWebBlockHeaderDb :: Getter (CutDb cas) WebBlockHeaderDb
cutDbWebBlockHeaderDb = to $ _webBlockHeaderStoreCas . _cutDbHeaderStore

cutDbWebBlockHeaderStore :: Getter (CutDb cas) WebBlockHeaderStore
cutDbWebBlockHeaderStore = to _cutDbHeaderStore

-- | Get the current 'Cut', which represent the latest chainweb state.
--
-- This the main API method of chainweb-consensus.
--
_cut :: CutDb cas -> IO Cut
_cut = readTVarIO . _cutDbCut

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

member :: CutDb cas -> ChainId -> BlockHash -> IO Bool
member db cid h = do
    th <- maxEntry chainDb
    lookup chainDb h >>= \case
        Nothing -> return False
        Just lh -> do
            fh <- forkEntry chainDb th lh
            return $ fh == lh
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
    -> (CutDb cas -> IO a)
    -> IO a
withCutDb config logfun headerStore payloadStore
    = bracket (startCutDb config logfun headerStore payloadStore) stopCutDb

startCutDb
    :: PayloadCas cas
    => CutDbConfig
    -> LogFunction
    -> WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> IO (CutDb cas)
startCutDb config logfun headerStore payloadStore = mask_ $ do
    cutVar <- newTVarIO (_cutDbConfigInitialCut config)
    queue <- newEmptyPQueue
    peerHeights <- newTVarIO . PeerHeights $ newBDQ (BlockHeight 0) 10
    cutAsync <- asyncWithUnmask $ \u -> u $ processor queue cutVar peerHeights
    logfun @T.Text Info "CutDB started"
    return $ CutDb
        { _cutDbCut = cutVar
        , _cutDbQueue = queue
        , _cutPeerHeights = peerHeights
        , _cutDbAsync = cutAsync
        , _cutDbLogFunction = logfun
        , _cutDbHeaderStore = headerStore
        , _cutDbPayloadStore = payloadStore
        , _cutDbQueueSize = _cutDbConfigBufferSize config
        }
  where
    processor :: PQueue (Down CutHashes) -> TVar Cut -> TVar PeerHeights -> IO ()
    processor queue cutVar peerHeights = do
        processCuts logfun headerStore payloadStore queue cutVar peerHeights `catches`
            [ Handler $ \(e :: SomeAsyncException) -> throwM e
            , Handler $ \(e :: SomeException) ->
                logfun @T.Text Error $ "CutDB failed: " <> sshow e
            ]
        processor queue cutVar peerHeights

stopCutDb :: CutDb cas -> IO ()
stopCutDb db = cancel (_cutDbAsync db)

-- | This is at the heart of 'Chainweb' POW: Deciding the current "longest" cut
-- among the incoming candiates.
--
-- Going forward this should probably be the main scheduler for most operations,
-- in particular it should drive (or least preempt) synchronzations of block
-- headers on indiviual chains.
--
processCuts
    :: PayloadCas cas
    => LogFunction
    -> WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> PQueue (Down CutHashes)
    -> TVar Cut
    -> TVar PeerHeights
    -> IO ()
processCuts logFun headerStore payloadStore queue cutVar peerHeights = queueToStream
    & S.chain (\c -> loggc Info c $ "start processing")
    & S.filterM (fmap not . isVeryOld)
    & S.filterM (fmap not . farAhead)
    & S.filterM (fmap not . isOld)
    & S.filterM (fmap not . isCurrent)
    & S.chain updateNetworkHeight
    & S.chain (\c -> loggc Info c $ "fetch all prerequesites")
    & S.mapM (cutHashesToBlockHeaderMap headerStore payloadStore)
    & S.chain (either
        (\c -> loggc Warn c "failed to get prerequesites for some blocks")
        (\c -> loggc Info c "got all prerequesites")
        )
    & S.concat
        -- ignore left values for now
    & S.scanM
        (\a b -> joinIntoHeavier_ (_webBlockHeaderStoreCas headerStore) (_cutMap a) b
        )
        (readTVarIO cutVar)
        (\c -> do
            atomically (writeTVar cutVar c)
            loggc Info c "published cut"
        )
    & S.effects
  where
    -- | Broadcast the newest estimated `BlockHeight` of the network to other
    -- components of this node.
    --
    -- Ignores any Cuts that came into the pipeline from /this/ node, i.e.
    -- `CutHashes` whose origin field is `Nothing`.
    --
    updateNetworkHeight :: CutHashes -> IO ()
    updateNetworkHeight ch = case _cutOrigin ch of
        Nothing -> pure ()
        Just _  -> atomically . modifyTVar' peerHeights $
            \(PeerHeights q) -> PeerHeights $ consBDQ (_cutHashesHeight ch) q

    loggc :: HasCutId c => LogLevel -> c -> T.Text -> IO ()
    loggc l c msg = logFun @T.Text l $  "cut " <> cutIdToTextShort (_cutId c) <> ": " <> msg

    graph = _chainGraph headerStore

    threshold :: Int
    threshold = int $ 2 * diameter graph * order graph

    queueToStream :: S.Stream (Of CutHashes) IO ()
    queueToStream = do
        Down a <- liftIO (pQueueRemove queue)
        S.yield a
        queueToStream

    -- FIXME: this is problematic. We should drop these before they are
    -- added to the queue, to prevent the queue becoming stale.
    farAhead :: CutHashes -> IO Bool
    farAhead x = do
        h <- _cutHeight <$> readTVarIO cutVar
        let !r = (int (_cutHashesHeight x) - farAheadThreshold) >= int h
        when r $ loggc Info x
            $ "skip far ahead cut. Current height: " <> sshow h
            <> ", got: " <> sshow (_cutHashesHeight x)
        return r

    isVeryOld :: CutHashes -> IO Bool
    isVeryOld x = do
        h <- _cutHeight <$> readTVarIO cutVar
        let !r = int (_cutHashesHeight x) <= (int h - threshold)
        when r $ loggc Info x "skip very old cut"
        return r

    isOld :: CutHashes -> IO Bool
    isOld x = do
        curHashes <- cutToCutHashes Nothing <$> readTVarIO cutVar
        let !r = all (>= (0 :: Int)) $ (HM.unionWith (-) `on` (fmap (int . fst) . _cutHashes)) curHashes x
        when r $ loggc Info x "skip old cut"
        return r

    isCurrent :: CutHashes -> IO Bool
    isCurrent x = do
        curHashes <- cutToCutHashes Nothing <$> readTVarIO cutVar
        let !r = _cutHashes curHashes == _cutHashes x
        when r $ loggc Info x "skip current cut"
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

cutHashesToBlockHeaderMap
    :: PayloadCas cas
    => WebBlockHeaderStore
    -> WebBlockPayloadStore cas
    -> CutHashes
    -> IO (Either (HM.HashMap ChainId BlockHash) (HM.HashMap ChainId BlockHeader))
        -- ^ The 'Left' value holds missing hashes, the 'Right' value holds
        -- a 'Cut'.
cutHashesToBlockHeaderMap headerStore payloadStore hs = do
    (headers :> missing) <- S.each (HM.toList $ _cutHashes hs)
        & S.map (fmap snd)
        & S.mapM tryGetBlockHeader
        & S.partitionEithers
        & S.fold_ (\x (cid, h) -> HM.insert cid h x) mempty id
        & S.fold (\x (cid, h) -> HM.insert cid h x) mempty id
    if null missing
        then return $ Right headers
        else return $ Left missing
  where
    origin = _cutOrigin hs
    priority = Priority (- int (_cutHashesHeight hs))

    tryGetBlockHeader cv@(cid, _) =
        (Right <$> mapM (getBlockHeader headerStore payloadStore cid priority origin) cv)
            `catch` \case
                (TreeDbKeyNotFound{} :: TreeDbException BlockHeaderDb) ->
                    return $ Left cv
                e -> throwM e

-- | Yield a `Cut` only when it is determined that `CutDb` has sync'd with
-- remote peers enough. "Enough" is a measure of "closeness" determined from the
-- `ChainGraph` implied by the given `ChainwebVersion`. In essence, if our
-- current Cut is...:
--
--   * below the threshold: Spin via STM (this has the affect of pausing POW mining).
--   * above the threshold: Yield the `Cut`.
--   * even higher than the network: We are either a "superior" fork, or we are in
--     initial network conditions where there is no real consensus yet. In this
--     case, we yield a `Cut`.
--
consensusCut :: CutDb cas -> IO Cut
consensusCut cutdb = atomically $ do
    cur <- _cutStm cutdb
    nh  <- avgNetworkHeight <$> readTVar (_cutPeerHeights cutdb)
    let !currentHeight = _cutHeight cur
        !thresh = int . catchupThreshold $ _chainwebVersion cutdb
        !mini = bool (nh - thresh) 0 $ thresh > nh
    when (currentHeight < mini) retry
    pure cur

-- | The distance from the true Cut within which the current node could be
-- considered "caught up".
--
catchupThreshold :: ChainwebVersion -> Natural
catchupThreshold = (2 *) . diameter . _chainGraph

-- -------------------------------------------------------------------------- --
-- Some CutDB

-- | 'CutDb' with type level 'ChainwebVersion'
--
newtype CutDbT cas (v :: ChainwebVersionT) = CutDbT (CutDb cas)
    deriving (Generic)

data SomeCutDb cas = forall v . KnownChainwebVersionSymbol v => SomeCutDb (CutDbT cas v)

someCutDbVal :: ChainwebVersion -> CutDb cas -> SomeCutDb cas
someCutDbVal (FromSing (SChainwebVersion :: Sing v)) db = SomeCutDb $ CutDbT @_ @v db
