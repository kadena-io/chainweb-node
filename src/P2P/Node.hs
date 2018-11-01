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

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM

import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import Data.Hashable
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import Servant.Client

import System.LogLevel
import qualified System.Random as R

import Test.QuickCheck (Arbitrary(..), oneof)

-- Internal imports

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version

import Data.LogMessage

import P2P.Node.Configuration
import P2P.Node.PeerDB
import P2P.Node.RestAPI.Client
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

instance Arbitrary P2pNodeStats where
    arbitrary = P2pNodeStats
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary

-- -------------------------------------------------------------------------- --
-- Session Info

data P2pSessionResult
    = P2pSessionResult Bool
    | P2pSessionException T.Text
    | P2pSessionTimeout (TimeSpan Int64)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData, ToJSON, FromJSON)

isSuccess :: P2pSessionResult -> Bool
isSuccess (P2pSessionResult True) = True
isSuccess _ = False

instance Arbitrary P2pSessionResult where
    arbitrary = oneof
        [ P2pSessionResult <$> arbitrary
        , P2pSessionException <$> arbitrary
        , P2pSessionTimeout <$> arbitrary
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
-- TODO: add configuration
--
data P2pNode = P2pNode
    { _p2pNodeChainId :: !ChainId
    , _p2pNodeChainwebVersion :: !ChainwebVersion
    , _p2pNodePeerInfo :: !PeerInfo
    , _p2pNodePeerDb :: !PeerDb
    , _p2pNodeSessions :: !(TVar (M.Map PeerId (P2pSessionInfo, Async Bool)))
    , _p2pNodeManager :: !HTTP.Manager
    , _p2pNodeLogFunction :: !LogFunction
    , _p2pNodeStats :: !(TVar P2pNodeStats)
    , _p2pNodeClientSession :: !P2pSession
    , _p2pNodeRng :: !(TVar R.StdGen)
    , _p2pNodeActive :: !(TVar Bool)
    }

showSessionId :: PeerId -> Async Bool -> T.Text
showSessionId pid ses = showPid pid <> ":" <> (T.drop 9 . sshow $ asyncThreadId ses)

showPid :: PeerId -> T.Text
showPid = T.take 8 . toText

addSession :: P2pNode -> PeerInfo -> Async Bool -> Time Int64 -> STM P2pSessionInfo
addSession node peer session start = do
    modifyTVar' (_p2pNodeSessions node) $ M.insert pid (info, session)
    return info
  where
    pid = _peerId peer
    info = P2pSessionInfo
        { _p2pSessionInfoId = showSessionId pid session
        , _p2pSessionInfoSource = _p2pNodePeerInfo node
        , _p2pSessionInfoTarget = peer
        , _p2pSessionInfoStart = start
        , _p2pSessionInfoEnd = Nothing
        , _p2pSessionInfoResult = Nothing
        }

removeSession :: P2pNode -> PeerId -> STM ()
removeSession node pid =
    modifyTVar' (_p2pNodeSessions node) $ M.delete pid

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

peerBaseUrl :: HostAddress -> BaseUrl
peerBaseUrl a = BaseUrl Http
    (B8.unpack . hostnameBytes $ view hostAddressHost a)
    (int $ view hostAddressPort a)
    ""

peerClientEnv :: P2pNode -> PeerInfo -> ClientEnv
peerClientEnv node = mkClientEnv (_p2pNodeManager node) . peerBaseUrl . _peerAddr

-- TODO: handle paging
--
syncFromPeer :: P2pNode -> PeerInfo -> IO Bool
syncFromPeer node info = runClientM sync env >>= \case
    Left e -> do
        logg node Warn $ "failed to sync peers from " <> showPid pid <> ": " <> sshow e
        return False
    Right p -> do
        peerDbInsertList (pageItemsWithoutMe p) (_p2pNodePeerDb node)
        return True
  where
    env = peerClientEnv node info
    v = _p2pNodeChainwebVersion node
    cid = _p2pNodeChainId node
    pid = _peerId info
    sync = do
        p <- peerGetClient v cid Nothing Nothing
        liftIO $ logg node Debug $ "got " <> sshow (_pageLimit p) <> " peers " <> showPid pid
        void $ peerPutClient v cid (_p2pNodePeerInfo node)
        liftIO $ logg node Debug $ "put own peer info to " <> showPid pid
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
    -> STM PeerInfo
findNextPeer conf node = do
    active <- readTVar (_p2pNodeActive node)
    check active
    peers <- peerDbSnapshotSTM peerDbVar
    sessions <- readTVar sessionsVar
    let peerCount = length peers
    let sessionCount = length sessions
    check (sessionCount < peerCount)
    i <- randomR node (0, peerCount - 1)
    let (a, b) = M.splitAt (fromIntegral i) peers
    let checkPeer n = do
            check (int sessionCount < _p2pConfigMaxSessionCount conf)
            check (M.notMember (_peerId n) sessions)
            return n
    foldr (orElse . checkPeer) retry (toList b ++ toList a)
  where
    peerDbVar = _p2pNodePeerDb node
    sessionsVar = _p2pNodeSessions node

-- -------------------------------------------------------------------------- --
-- Manage Sessions

-- | TODO May loop forever. Add proper retry logic and logging
--
newSession :: P2pConfiguration -> P2pNode -> IO ()
newSession conf node = do
    newPeer <- atomically $ findNextPeer conf node
    let newPeerId = _peerId newPeer
    logg node Debug $ "Selected new peer " <> showPid newPeerId
    syncFromPeer node newPeer >>= \case
        False -> do
            logg node Warn $ "Failed to connect new peer " <> showPid newPeerId
            newSession conf node
        True -> do
            logg node Debug $ "Connected to new peer " <> showPid newPeerId
            let env = peerClientEnv node newPeer
            (info, newSes) <- mask $ \restore -> do
                now <- getCurrentTimeIntegral
                newSes <- async $ restore $ _p2pNodeClientSession node (loggFun node) env
                info <- atomically $ addSession node newPeer newSes now
                return (info, newSes)
            logg node Info $ "Started peer session " <> showSessionId newPeerId newSes
            loggFun node Info $ JsonLog info

-- | Monitor and garbage collect sessions
--
awaitSessions :: P2pNode -> IO ()
awaitSessions node = do
    (pId, info, ses, result) <- atomically $ do
        (p, i, a, r) <- waitAnySession node
        removeSession node p
        result <- case r of
            Right True -> P2pSessionResult True <$ countSuccess node
            Right False -> P2pSessionResult False <$ countFailure node
            Left e -> P2pSessionException (sshow e) <$ countException node
        return (p, i, a, result)

    -- logging

    now <- getCurrentTimeIntegral
    let finalInfo = info
            { _p2pSessionInfoEnd = Just now
            , _p2pSessionInfoResult = Just result
            }
    loggFun node Info $ JsonLog finalInfo

    case result of
        P2pSessionException e -> do
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
    loggFun node Info $ JsonLog stats

waitAnySession :: P2pNode -> STM (PeerId, P2pSessionInfo, Async Bool, Either SomeException Bool)
waitAnySession node = do
    sessions <- readTVar $ _p2pNodeSessions node
    foldr orElse retry $ waitFor <$> M.toList sessions
  where
    waitFor (k, (i, a)) = (k, i, a,) <$> waitCatchSTM a

-- -------------------------------------------------------------------------- --
-- Run Peer DB

startPeerDb
    :: P2pConfiguration
    -> IO PeerDb
startPeerDb conf = do
    peerDb <- fromPeerList (_p2pConfigKnownPeers conf)
    case _p2pConfigPeerDbFilePath conf of
        Just dbFilePath -> loadIntoPeerDb dbFilePath peerDb
        Nothing -> return ()
    return peerDb

stopPeerDb :: P2pConfiguration -> PeerDb -> IO ()
stopPeerDb conf db = case _p2pConfigPeerDbFilePath conf of
    Just dbFilePath -> storePeerDb dbFilePath db
    Nothing -> return ()

withPeerDb
    :: P2pConfiguration
    -> (PeerDb -> IO a)
    -> IO a
withPeerDb conf = bracket (startPeerDb conf) (stopPeerDb conf)

-- -------------------------------------------------------------------------- --
-- Create

p2pCreateNode
    :: ChainwebVersion
    -> ChainId
    -> P2pConfiguration
    -> LogFunction
    -> PeerDb
    -> HostAddress
    -> HTTP.Manager
    -> P2pSession
    -> IO P2pNode
p2pCreateNode cv cid conf logfun db addr mgr session = do
    -- get node id
    nid <-  maybe createPeerId return $ _p2pConfigPeerId conf
    let myInfo = PeerInfo
            { _peerId = nid
            , _peerAddr = addr
            }

    -- intialize P2P State
    sessionsVar <- newTVarIO mempty
    statsVar <- newTVarIO emptyP2pNodeStats
    rngVar <- newTVarIO =<< R.newStdGen
    activeVar <- newTVarIO True
    let s = P2pNode
                { _p2pNodeChainId = cid
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

-- -------------------------------------------------------------------------- --
-- Run P2P Node

p2pStartNode :: P2pConfiguration -> P2pNode -> IO ()
p2pStartNode conf node = concurrently_
    (forever $ awaitSessions node)
    (forever $ newSession conf node)

p2pStopNode :: P2P.Node.P2pNode -> IO ()
p2pStopNode node = do
    sessions <- atomically $ do
        setInactive node
        readTVar (_p2pNodeSessions node)
    mapM_ (uninterruptibleCancel . snd) sessions
    logg node Info "stopped node"

