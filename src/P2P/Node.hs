{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: P2P.Node
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.Node
(
-- * Run Peer Database
  startPeerDb
, stopPeerDb
, withPeerDb

-- * P2P Node
, p2pCreateNode
, p2pStartNode
, p2pStopNode

-- * Logging and Monitoring

, P2pSessionResult(..)
, P2pSessionInfo(..)
, p2pSessionInfoId
, p2pSessionInfoSource
, p2pSessionInfoTarget
, p2pSessionInfoStart
, p2pSessionInfoEnd
, p2pSessionInfoResult

, P2pNodeStats(..)
, p2pStatsSuccessCount
, p2pStatsFailureCount
, p2pStatsExceptionCount
, p2pStatsTimeoutCount
, p2pStatsKnownPeerCount
, p2pStatsActiveLast
, p2pStatsActiveMax
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM

import Data.Aeson hiding (Error)
import Data.Foldable
import Data.Hashable
import qualified Data.HashSet as HS
import Data.Int
import Data.IxSet.Typed (getEQ, getGT, getGTE, getLT, size)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Tuple

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import Servant.Client

import System.LogLevel
import qualified System.Random as R
import System.Timeout

import Test.QuickCheck (Arbitrary(..), oneof)

-- Internal imports

import Chainweb.RestAPI.NetworkID
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.LogMessage

import P2P.Node.Configuration
import P2P.Node.PeerDB
import P2P.Node.RestAPI.Client
import P2P.Peer
import P2P.Session

-- -------------------------------------------------------------------------- --
-- P2pNodeStats

data P2pNodeStats = P2pNodeStats
    { _p2pStatsSuccessCount :: !Natural
    , _p2pStatsFailureCount :: !Natural
    , _p2pStatsTimeoutCount :: !Natural
    , _p2pStatsExceptionCount :: !Natural
    , _p2pStatsKnownPeerCount :: !Natural
    , _p2pStatsActiveLast :: !Natural
    , _p2pStatsActiveMax :: !Natural
    -- , _p2pStatDistinctPeersCount :: !HyperLogLog
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Hashable, NFData)

makeLenses ''P2pNodeStats

emptyP2pNodeStats :: P2pNodeStats
emptyP2pNodeStats = P2pNodeStats
    { _p2pStatsSuccessCount = 0
    , _p2pStatsFailureCount = 0
    , _p2pStatsTimeoutCount = 0
    , _p2pStatsExceptionCount = 0
    , _p2pStatsKnownPeerCount = 0
    , _p2pStatsActiveLast = 0
    , _p2pStatsActiveMax = 0
    }

_p2pStatsSessionCount :: P2pNodeStats -> Natural
_p2pStatsSessionCount s
    = _p2pStatsSuccessCount s
    + _p2pStatsFailureCount s
    + _p2pStatsTimeoutCount s
    + _p2pStatsExceptionCount s

instance Arbitrary P2pNodeStats where
    arbitrary = P2pNodeStats
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary

-- -------------------------------------------------------------------------- --
-- Session Info

data P2pSessionResult
    = P2pSessionResultSuccess
    | P2pSessionResultFailure
    | P2pSessionException T.Text
    | P2pSessionTimeout
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData, ToJSON, FromJSON)

-- | Whether a session was successful. 'P2pSessionTimeout' is considered
-- success.
--
isSuccess :: P2pSessionResult -> Bool
isSuccess P2pSessionResultSuccess = True
isSuccess P2pSessionTimeout = True
isSuccess _ = False

instance Arbitrary P2pSessionResult where
    arbitrary = oneof
        [ pure P2pSessionResultSuccess
        , pure P2pSessionResultFailure
        , P2pSessionException <$> arbitrary
        , pure P2pSessionTimeout
        ]

data P2pSessionInfo = P2pSessionInfo
    { _p2pSessionInfoId :: !T.Text
    , _p2pSessionInfoSource :: !PeerInfo
    , _p2pSessionInfoTarget :: !PeerInfo
    , _p2pSessionInfoStart :: !(Time Int64)
    , _p2pSessionInfoEnd :: !(Maybe (Time Int64))
    , _p2pSessionInfoResult :: !(Maybe P2pSessionResult)
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

makeLenses ''P2pSessionInfo

instance Arbitrary P2pSessionInfo where
    arbitrary = P2pSessionInfo
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary

-- -------------------------------------------------------------------------- --
-- P2P Node State

-- | P2P Node State
--
data P2pNode = P2pNode
    { _p2pNodeNetworkId :: !NetworkId
    , _p2pNodeChainwebVersion :: !ChainwebVersion
    , _p2pNodePeerInfo :: !PeerInfo
    , _p2pNodePeerDb :: !PeerDb
    , _p2pNodeSessions :: !(TVar (M.Map PeerInfo (P2pSessionInfo, Async (Maybe Bool))))
    , _p2pNodeManager :: !HTTP.Manager
    , _p2pNodeLogFunction :: !LogFunction
    , _p2pNodeStats :: !(TVar P2pNodeStats)
    , _p2pNodeClientSession :: !P2pSession
    , _p2pNodeRng :: !(TVar R.StdGen)
    , _p2pNodeActive :: !(TVar Bool)
        -- ^ Wether this node is active. If this is 'False' no new sessions
        -- will be initialized.
    }

showSessionId :: PeerInfo -> Async (Maybe Bool) -> T.Text
showSessionId pinf ses = showInfo pinf <> ":" <> (T.drop 9 . sshow $ asyncThreadId ses)

showInfo :: PeerInfo -> T.Text
showInfo = shortPeerInfo
{-# INLINE showInfo #-}

addSession
    :: P2pNode
    -> PeerInfo
    -> Async (Maybe Bool)
    -> Time Int64
    -> STM P2pSessionInfo
addSession node peer session start = do
    modifyTVar' (_p2pNodeSessions node) $ M.insert peer (info, session)
    return info
  where
    info = P2pSessionInfo
        { _p2pSessionInfoId = showSessionId peer session
        , _p2pSessionInfoSource = _p2pNodePeerInfo node
        , _p2pSessionInfoTarget = peer
        , _p2pSessionInfoStart = start
        , _p2pSessionInfoEnd = Nothing
        , _p2pSessionInfoResult = Nothing
        }

removeSession :: P2pNode -> PeerInfo -> STM ()
removeSession node pinf =
    modifyTVar' (_p2pNodeSessions node) $ M.delete pinf

modifyStats :: (P2pNodeStats -> P2pNodeStats) -> P2pNode -> STM ()
modifyStats f node = modifyTVar (_p2pNodeStats node) f

countSuccess :: P2pNode -> STM ()
countSuccess = modifyStats $ p2pStatsSuccessCount %~ succ

countFailure :: P2pNode -> STM ()
countFailure = modifyStats $ p2pStatsFailureCount %~ succ

countTimeout :: P2pNode -> STM ()
countTimeout = modifyStats $ p2pStatsTimeoutCount %~ succ

countException :: P2pNode -> STM ()
countException = modifyStats $ p2pStatsExceptionCount %~ succ

updateKnownPeerCount :: P2pNode -> STM ()
updateKnownPeerCount node = do
    known <- peerDbSizeSTM (_p2pNodePeerDb node)
    modifyStats (p2pStatsKnownPeerCount .~ known) node

updateActiveCount :: P2pNode -> STM ()
updateActiveCount node = do
    active <- int . M.size <$> readTVar (_p2pNodeSessions node)
    modifyStats (p2pStatsActiveLast .~ active) node
    modifyStats (p2pStatsActiveMax %~ max active) node

-- | Monomorphized LogFunction
--
logg :: P2pNode -> LogLevel -> T.Text -> IO ()
logg = _p2pNodeLogFunction

loggFun :: P2pNode -> LogFunction
loggFun = _p2pNodeLogFunction

randomR :: R.Random a => P2pNode -> (a, a) -> STM a
randomR node range = do
    gen <- readTVar (_p2pNodeRng node)
    let (a, gen') = R.randomR range gen
    a <$ writeTVar (_p2pNodeRng node) gen'

setInactive :: P2pNode -> STM ()
setInactive node = writeTVar (_p2pNodeActive node) False

-- -------------------------------------------------------------------------- --
-- Sync Peers

peerClientEnv :: P2pNode -> PeerInfo -> ClientEnv
peerClientEnv node = peerInfoClientEnv (_p2pNodeManager node)

-- | Synchronize the peer database with the peer database of the remote peer.
--
-- TODO: handle paging
--
syncFromPeer :: P2pNode -> PeerInfo -> IO Bool
syncFromPeer node info = runClientM sync env >>= \case
    Left e -> do
        logg node Warn $ "failed to sync peers from " <> showInfo info <> ": " <> sshow e
        return False
    Right p -> do
        peerDbInsertPeerInfoList
            (_p2pNodeNetworkId node)
            (pageItemsWithoutMe p)
            (_p2pNodePeerDb node)
        return True
  where
    env = peerClientEnv node info
    v = _p2pNodeChainwebVersion node
    nid = _p2pNodeNetworkId node
    sync = do
        p <- peerGetClient v nid Nothing Nothing
        liftIO $ logg node Debug $ "got " <> sshow (_pageLimit p) <> " peers " <> showInfo info
        void $ peerPutClient v nid (_p2pNodePeerInfo node)
        liftIO $ logg node Debug $ "put own peer info to " <> showInfo info
        return p
    myPid = _peerId $ _p2pNodePeerInfo node
    pageItemsWithoutMe = filter (\i -> _peerId i /= myPid) . _pageItems

-- -------------------------------------------------------------------------- --
-- Sample Peer from PeerDb

-- | Sample next active peer. Blocks until a suitable peer is available
--
-- @O(_p2pConfigActivePeerCount conf)@
--
findNextPeer
    :: P2pConfiguration
    -> P2pNode
    -> STM PeerEntry
findNextPeer conf node = do

    -- check if this node is active. If not, don't create new sessions,
    -- but retry until it becomes active.
    --
    active <- readTVar (_p2pNodeActive node)
    check active

    -- get all known peers and all active sessions
    --
    peers <- peerDbSnapshotSTM peerDbVar
    sessions <- readTVar sessionsVar
    let peerCount = length peers
    let sessionCount = length sessions

    -- Retry if there are already more active sessions than known peers.
    --
    check (sessionCount < peerCount)

    -- Create a new sessions with a random peer for which there is no active
    -- sessions:

    let checkPeer (n :: PeerEntry) = do
            let pid = _peerId $ _peerEntryInfo n
            -- can this check be moved out of the fold?
            check (int sessionCount < _p2pConfigMaxSessionCount conf)
            check (M.notMember (_peerEntryInfo n) sessions)
            check (pid /= myPid)
            return n

    -- random circular shift of a set
    let shift i s = uncurry (++)
            $ swap
            $ splitAt (fromIntegral i)
            $ toList
            $ s

        shiftR s = do
            i <- randomR node (0, size s - 1)
            return $ shift i s

    -- Classify the peers by priority
    --
    let base = getEQ (_p2pNodeNetworkId node) peers
#if 0
    searchSpace <- shift base
#else
    -- TODO: how expensive is this? should be cache the classification?
    --
    let p0 = getGT (ActiveSessionCount 0) $ getLT (SuccessiveFailures 2) base
        p1 = getEQ (ActiveSessionCount 0) $ getLT (SuccessiveFailures 2) base
        p2 = getGT (ActiveSessionCount 0) $ getGTE (SuccessiveFailures 2) base
        p3 = getEQ (ActiveSessionCount 0) $ getGTE (SuccessiveFailures 2) base
    searchSpace <- concat <$> traverse shiftR [p0, p1, p2, p3]
#endif

    foldr (orElse . checkPeer) retry searchSpace
  where
    peerDbVar = _p2pNodePeerDb node
    sessionsVar = _p2pNodeSessions node
    myPid = _peerId $ _p2pNodePeerInfo node

-- -------------------------------------------------------------------------- --
-- Manage Sessions

-- | TODO May loop forever. Add proper retry logic and logging
--
newSession :: P2pConfiguration -> P2pNode -> IO ()
newSession conf node = do
    newPeer <- atomically $ findNextPeer conf node
    let newPeerInfo = _peerEntryInfo newPeer
    logg node Debug $ "Selected new peer " <> encodeToText newPeer
    syncFromPeer node newPeerInfo >>= \case
        False -> do
            threadDelay =<< R.randomRIO (400000, 500000)
                -- FIXME there are better ways to prevent the node from spinning
                -- if no suitable (non-failing node) is available.
                -- cf. GitHub issue #117
            newSession conf node
        True -> do
            logg node Debug $ "Connected to new peer " <> showInfo newPeerInfo
            let env = peerClientEnv node newPeerInfo
            (info, newSes) <- mask $ \restore -> do
                now <- getCurrentTimeIntegral
                t <- R.randomRIO
                    ( round (0.9 * timeoutMs)
                    , round (1.1 * timeoutMs)
                    )
                newSes <- async $ restore $ timeout t
                    $ _p2pNodeClientSession node (loggFun node) env newPeerInfo
                incrementActiveSessionCount peerDb newPeerInfo
                info <- atomically $ addSession node newPeerInfo newSes now
                return (info, newSes)
            logg node Debug $ "Started peer session " <> showSessionId newPeerInfo newSes
            loggFun node Info $ JsonLog info
  where
    TimeSpan timeoutMs = secondsToTimeSpan @Double (_p2pConfigSessionTimeout conf)
    peerDb = _p2pNodePeerDb node

-- | Monitor and garbage collect sessions
--
awaitSessions :: P2pNode -> IO ()
awaitSessions node = do
    (pId, info, ses, result) <- atomically $ do
        (p, i, a, r) <- waitAnySession node
        removeSession node p
        result <- case r of
            Right Nothing -> P2pSessionTimeout <$ countTimeout node
            Right (Just True) -> P2pSessionResultSuccess <$ countSuccess node
            Right (Just False) -> P2pSessionResultFailure <$ countFailure node
            Left e -> P2pSessionException (sshow e) <$ countException node
        return (p, i, a, result)

    -- update peer db entry
    --
    -- (Note that there is a chance of a race here, if the peer is used in
    -- new session after the previous session is removed from the node and
    -- before the following db updates are performed. The following updates are
    -- performed under an 'MVar' lock in IO to prevent starvation due to
    -- contention. This comes at the cost of possibly inaccurate values for
    -- the counters and times in the PeerEntry value.)
    --
    decrementActiveSessionCount peerDb pId
    case result of
        P2pSessionTimeout -> do
            resetSuccessiveFailures peerDb pId
            updateLastSuccess peerDb pId
        P2pSessionResultSuccess -> do
            resetSuccessiveFailures peerDb pId
            updateLastSuccess peerDb pId
        P2pSessionResultFailure -> incrementSuccessiveFailures peerDb pId
        P2pSessionException _ -> incrementSuccessiveFailures peerDb pId

    -- logging

    now <- getCurrentTimeIntegral
    let finalInfo = info
            { _p2pSessionInfoEnd = Just now
            , _p2pSessionInfoResult = Just result
            }
    loggFun node Info $ JsonLog finalInfo

    case result of
        P2pSessionException e ->
            logg node Warn
                $ "session " <> showSessionId pId ses <> " failed with " <> sshow e
        _ -> return ()

    logg node Info
        $ "closed session " <> showSessionId pId ses
        <> if isSuccess result then " (success)" else " (failure)"

    stats <- atomically $ do
        updateKnownPeerCount node
        updateActiveCount node
        readTVar (_p2pNodeStats node)
    when (_p2pStatsSessionCount stats `mod` 250 == 0)
        $ loggFun node Info $ JsonLog stats

  where
    peerDb = _p2pNodePeerDb node

waitAnySession
    :: P2pNode
    -> STM (PeerInfo, P2pSessionInfo, Async (Maybe Bool), Either SomeException (Maybe Bool))
waitAnySession node = do
    sessions <- readTVar $ _p2pNodeSessions node
    foldr orElse retry $ waitFor <$> M.toList sessions
  where
    waitFor (k, (i, a)) = (k, i, a,) <$> waitCatchSTM a

-- -------------------------------------------------------------------------- --
-- Run Peer DB

-- | Start a 'PeerDb' for the given set of NetworkIds
--
startPeerDb
    :: HS.HashSet NetworkId
    -> P2pConfiguration
    -> IO PeerDb
startPeerDb nids conf = do
    peerDb <- newEmptyPeerDb
    forM_ nids $ \nid ->
        peerDbInsertPeerInfoList nid (_p2pConfigKnownPeers conf) peerDb
    case _p2pConfigPeerDbFilePath conf of
        Just dbFilePath -> loadIntoPeerDb dbFilePath peerDb
        Nothing -> return ()
    return peerDb

-- | Stop a 'PeerDb', possibly persisting the db to a file.
--
stopPeerDb :: P2pConfiguration -> PeerDb -> IO ()
stopPeerDb conf db = case _p2pConfigPeerDbFilePath conf of
    Just dbFilePath -> storePeerDb dbFilePath db
    Nothing -> return ()

-- | Run a computation with a PeerDb
--
withPeerDb
    :: HS.HashSet NetworkId
    -> P2pConfiguration
    -> (PeerDb -> IO a)
    -> IO a
withPeerDb nids conf = bracket (startPeerDb nids conf) (stopPeerDb conf)

-- -------------------------------------------------------------------------- --
-- Create

p2pCreateNode
    :: ChainwebVersion
    -> NetworkId
    -> Peer
    -> LogFunction
    -> PeerDb
    -> HTTP.Manager
    -> P2pSession
    -> IO P2pNode
p2pCreateNode cv nid peer logfun db mgr session = do
    -- intialize P2P State
    sessionsVar <- newTVarIO mempty
    statsVar <- newTVarIO emptyP2pNodeStats
    rngVar <- newTVarIO =<< R.newStdGen
    activeVar <- newTVarIO True
    let s = P2pNode
                { _p2pNodeNetworkId = nid
                , _p2pNodeChainwebVersion = cv
                , _p2pNodePeerInfo = myInfo
                , _p2pNodePeerDb = db
                , _p2pNodeSessions = sessionsVar
                , _p2pNodeManager = mgr
                , _p2pNodeLogFunction = logfun
                , _p2pNodeStats = statsVar
                , _p2pNodeClientSession = session
                , _p2pNodeRng = rngVar
                , _p2pNodeActive = activeVar
                }

    logfun @T.Text Info "created node"
    return s
  where
    myInfo = _peerInfo peer

-- -------------------------------------------------------------------------- --
-- Run P2P Node

p2pStartNode :: P2pConfiguration -> P2pNode -> IO ()
p2pStartNode conf node = concurrently_
    (runForever (logg node) "P2P.Node.awaitSessions" $ awaitSessions node)
    (runForever (logg node) "P2P.Node.newSessions" $ newSession conf node)

p2pStopNode :: P2P.Node.P2pNode -> IO ()
p2pStopNode node = do
    sessions <- atomically $ do
        setInactive node
        readTVar (_p2pNodeSessions node)
    mapM_ (uninterruptibleCancel . snd) sessions
    logg node Info "stopped node"
