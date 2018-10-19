{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM

import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import qualified Data.Map.Strict as M
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import qualified Data.Text as T

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import Servant.Client

import System.LogLevel
import qualified System.Random as R

-- Internal imports

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.Utils hiding (check)
import Chainweb.Version

import P2P.Node.Configuration
import P2P.Node.PeerDB
import P2P.Node.RestAPI.Client
import P2P.Session

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
    , _p2pNodeSessions :: !(TVar (M.Map PeerId (Async Bool)))
    , _p2pNodeManager :: !HTTP.Manager
    , _p2pNodeLogFunction :: !LogFunction
    , _p2pNodeSuccessCount :: !(TVar Natural)
    , _p2pNodeFailureCount :: !(TVar Natural)
    , _p2pNodeClientSession :: !P2pSession
    , _p2pNodeRng :: !(TVar R.StdGen)
    , _p2pNodeActive :: !(TVar Bool)
    }

addSession :: P2pNode -> PeerId -> Async Bool -> STM ()
addSession node pid session =
    modifyTVar' (_p2pNodeSessions node) $ M.insert pid session

removeSession :: P2pNode -> PeerId -> STM ()
removeSession node pid =
    modifyTVar' (_p2pNodeSessions node) $ M.delete pid

countSuccess :: P2pNode -> STM ()
countSuccess node = modifyTVar' (_p2pNodeSuccessCount node) succ

countFailure :: P2pNode -> STM ()
countFailure node = modifyTVar' (_p2pNodeFailureCount node) succ

logg :: P2pNode -> LogLevel -> T.Text -> IO ()
logg = _p2pNodeLogFunction

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
        peerDbInsertList (_pageItems p) (_p2pNodePeerDb node)
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

showSessionId :: PeerId -> Async Bool -> T.Text
showSessionId pid ses = showPid pid <> ":" <> (T.drop 9 . sshow $ asyncThreadId ses)

showPid :: PeerId -> T.Text
showPid = T.take 8 . toText

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
            newSes <- mask $ \restore -> do
                newSes <- async $ restore $ _p2pNodeClientSession node (logg node) env
                atomically $ addSession node newPeerId newSes
                return newSes
            logg node Info $ "Started peer session " <> showSessionId newPeerId newSes

-- | Monitor and garbage collect sessions
--
awaitSessions :: P2pNode -> IO ()
awaitSessions node = do
    (pId, ses, result) <- atomically $ do
        (p, a, r) <- waitAnySession node
        removeSession node p
        case r of
            Right True -> countSuccess node
            Right False -> countFailure node
            Left{} -> countFailure node
        return (p, a, r)

    -- logging
    r <- case result of
        Right True -> return True
        Right False -> return False
        Left e -> do
            logg node Warn
                $ "session " <> showSessionId pId ses <> " failed with " <> sshow e
            return False

    logg node Info
        $ "closed session " <> showSessionId pId ses
        <> if r then " (success)" else " (failure)"

    successes <- readTVarIO (_p2pNodeSuccessCount node)
    failures <- readTVarIO (_p2pNodeFailureCount node)
    active <- M.size <$> readTVarIO (_p2pNodeSessions node)
    known <- peerDbSize (_p2pNodePeerDb node)
    logg node Info
        $ "known peers: " <> sshow known <> ", "
        <> "successes: " <> sshow successes <> ", "
        <> "failures: " <> sshow failures <> ", "
        <> "active: " <> sshow active

waitAnySession :: P2pNode -> STM (PeerId, Async Bool, Either SomeException Bool)
waitAnySession node = do
    sessions <- readTVar $ _p2pNodeSessions node
    foldr orElse retry $ waitFor <$> M.toList sessions
  where
    waitFor (k, a) = (k, a,) <$> waitCatchSTM a

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
    successVar <- newTVarIO 0
    failureVar <- newTVarIO 0
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
                , _p2pNodeSuccessCount = successVar
                , _p2pNodeFailureCount = failureVar
                , _p2pNodeClientSession = session
                , _p2pNodeRng = rngVar
                , _p2pNodeActive = activeVar
                }

    logfun Info "created node"
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
    mapM_ uninterruptibleCancel sessions
    logg node Info "stopped node"

