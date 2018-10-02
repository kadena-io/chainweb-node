{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
( P2pConfiguration(..)
, InProcessNodeException(..)
, p2pNode
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM

import qualified Data.ByteString as B
import Data.Function
import Data.IORef
import qualified Data.List as L
import Data.Monoid
import Data.String
import qualified Data.Text as T

import Numeric.Natural


import System.IO.Unsafe
import System.LogLevel
import System.Random

-- Internal imports

import P2P.Connection

-- -------------------------------------------------------------------------- --
-- Misc Utils

sshow :: Show a => IsString b => a -> b
sshow = fromString . show

-- -------------------------------------------------------------------------- --
-- Configuration

data P2pConfiguration = P2pConfiguration
    { _p2pConfigSessionCount :: !Natural
        -- ^ number of sessions that a node tries to keep open

    , _p2pConfigMaxSessionCount :: !Natural
        -- ^ maximum number of session that a node accepts

    , _p2pConfigMessageBufferSize :: !Natural

    , _p2pLogFunction :: LogLevel -> T.Text -> IO ()
    }

-- -------------------------------------------------------------------------- --
-- Exceptions

data InProcessNodeException
    = BufferOverflow
    | RemoteSessionEnded
    | NodeFailure
        -- ^ raised if the local node failes to allocate or to run
        -- sessions.

    deriving (Show)

instance Exception InProcessNodeException

-- -------------------------------------------------------------------------- --
-- Connection State

-- | Msg wrapper
--
data Msg
    = Close
    | Failure SomeException
    | Msg [B.ByteString]

-- | Connection State
--
data ConnState = ConnState
    { _sendChan :: !(TBQueue Msg)
        -- ^ Shared with the other end of the connection.
        --
        -- FIXME: Using TBQueue is problematic here because of it's amortized
        -- performance guarantees.
    , _receiveChan :: !(TBQueue Msg)
        -- ^ Shared with the other end of the connection.
    , _failure :: !(TMVar SomeException)
        -- ^ Shared between both ends of the connection.
    , _isClosed :: !(TMVar P2pPeer)
        -- ^ Local to one end of the connection.
    }
    deriving (Eq)

newConnState :: TBQueue Msg -> TBQueue Msg -> TMVar SomeException -> IO ConnState
newConnState s r f = do
    closedVar <- newEmptyTMVarIO
    return $ ConnState
        { _sendChan = s
        , _receiveChan = r
        , _isClosed = closedVar
        , _failure = f
        }

-- -------------------------------------------------------------------------- --
-- Connection Utils

-- | Exception handling for Transactions
--
runP2pStm :: forall m a . MonadIO m => MonadThrow m => STM a -> m a
runP2pStm transaction = do
    eitherResult <- liftIO $ atomically
        $ fmap Right transaction `catchSTM` \e ->
            return $ Left (e :: P2pConnectionException)
    either throwM return eitherResult

checkClosedAndFailure :: ConnState -> STM ()
checkClosedAndFailure s = do
    tryReadTMVar (_isClosed s) >>= \case
        Just peer -> throwSTM $ P2pConnectionClosed peer
        Nothing -> return ()

    tryReadTMVar (_failure s) >>= \case
        Just e -> throwSTM $ P2pConnectionFailed e
        Nothing -> return ()

checkBufferOverflow :: ConnState -> STM ()
checkBufferOverflow s =
    isFullTBQueue (_sendChan s) >>= \case
        True -> do
            let e = toException BufferOverflow
            void $ tryPutTMVar (_failure s) e
            throwSTM $ P2pConnectionFailed e
        False -> return ()

checkReceive :: ConnState -> Msg -> STM [B.ByteString]
checkReceive s msg =  case msg of
    Close -> do
        void $ tryTakeTMVar (_isClosed s)
        putTMVar (_isClosed s) Remote
        throwSTM $ P2pConnectionClosed Remote
    Failure e -> do
        void $ tryPutTMVar (_failure s) e
        throwSTM $ P2pConnectionFailed e
    Msg bytes -> return bytes

-- -------------------------------------------------------------------------- --
-- P2pConnection

send :: MonadIO m => MonadThrow m => ConnState -> P2pMessage -> m ()
send s msg = runP2pStm $ do
    checkClosedAndFailure s
    checkBufferOverflow s
    writeTBQueue (_sendChan s) (Msg msg)

receive :: MonadIO m => MonadThrow m => ConnState -> m P2pMessage
receive s = runP2pStm $ do
    checkClosedAndFailure s
    msg <- readTBQueue (_receiveChan s)
    checkReceive s msg

tryReceive :: MonadIO m => MonadThrow m => ConnState -> m (Maybe P2pMessage)
tryReceive s = runP2pStm $ do
    checkClosedAndFailure s
    maybeMsg <- tryReadTBQueue (_receiveChan s)
    mapM (checkReceive s) maybeMsg

close :: MonadIO m => MonadThrow m => ConnState -> m ()
close s = runP2pStm $ do
    checkClosedAndFailure s
    checkBufferOverflow s
    putTMVar (_isClosed s) Local
    void $ writeTBQueue (_sendChan s) Close

endpoint :: MonadThrow m => MonadIO m => ConnState -> P2pConnection m
endpoint s = P2pConnection
    { p2pSend = send s
    , p2pReceive = receive s
    , p2pTryReceive = tryReceive s
    , p2pClose = close s
    }

-- -------------------------------------------------------------------------- --
-- Node

newtype NodeId = NodeId Int
    deriving (Show, Eq, Ord)

data Node = Node
    { _nodeSessions :: !(TVar [Async Bool])
    , _nodeConfig :: !P2pConfiguration
    , _nodeSession :: !P2pSession
    , _nodeSessionCount :: !(TVar Natural)
    , _nodeSuccessConnectionCount :: !(TVar Natural)
    , _nodeFailedConnectionCount :: !(TVar Natural)
    , _nodeId :: !NodeId
    }

instance Eq Node where
    (==) = (==) `on` _nodeId

instance Show Node where
    show = show . _nodeId

newNode :: P2pConfiguration -> P2pSession -> NodeId -> IO Node
newNode conf session i = do
    sessions <- newTVarIO []
    successCount <- newTVarIO 0
    failureCount <- newTVarIO 0
    sessionCount <- newTVarIO 0

    return $ Node
        { _nodeSessions = sessions
        , _nodeConfig = conf
        , _nodeSession = session
        , _nodeSessionCount = sessionCount
        , _nodeSuccessConnectionCount = successCount
        , _nodeFailedConnectionCount = failureCount
        , _nodeId = i
        }

addSession :: Node -> Async Bool -> STM ()
addSession node s = do
    modifyTVar' (_nodeSessions node) ((:) s)
    modifyTVar' (_nodeSessionCount node) succ

removeSession :: Node -> Async Bool -> STM ()
removeSession node s = do
    modifyTVar' (_nodeSessions node) (L.delete s)
    modifyTVar' (_nodeSessionCount node) pred

-- -------------------------------------------------------------------------- --
-- Global nodes list

nodes :: TVar [Node]
nodes = unsafePerformIO $ newTVarIO []
{-# NOINLINE nodes #-}

addNode :: Node -> STM ()
addNode n = modifyTVar' nodes $ (:) n

removeNode :: Node -> STM ()
removeNode n = modifyTVar' nodes $ L.delete n

sampleNodeIdx :: IO Natural
sampleNodeIdx = do
    ns <- readTVarIO nodes
    fromIntegral <$> randomRIO (0, length ns - 1)

findNextNode :: Node -> Natural -> [Node] -> STM Node
findNextNode cur i ns = foldr (orElse . checkNode) retry ns'
  where
    ns' = let (a,b) = splitAt (fromIntegral i) ns in b ++ a
    checkNode n = do
        check (n /= cur)
        c <- readTVar (_nodeSessionCount n)
        check (c < _p2pConfigMaxSessionCount (_nodeConfig n))
        return n

newNodeIdCounter :: IORef Int
newNodeIdCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE newNodeIdCounter #-}

newNodeId :: IO NodeId
newNodeId = atomicModifyIORef' newNodeIdCounter $ \a -> (succ a, NodeId a)

-- -------------------------------------------------------------------------- --
-- Running a P2P Node

p2pNode
    :: P2pConfiguration
    -> P2pSession
    -> IO ()
p2pNode conf session = bracket createNode deleteNode runNode
  where

    logg = _p2pLogFunction conf

    -- Create new node
    --
    createNode = do
        nid <- newNodeId
        node <- newNode conf session nid
        atomically $ addNode node
        logg Info $ "created node"
        return node

    -- Delete node on exit
    --
    deleteNode node = do
        sessions <- atomically $ do
            removeNode node
            readTVar (_nodeSessions node)
        mapM_ uninterruptibleCancel sessions
        logg Info $ "deleted node"

    -- Run node
    --
    runNode node = concurrently_ (awaitSessions node) (connect node)

    -- Monitor and garbage collect sessions connections.
    --
    awaitSessions node = forever $ do
        (ses, result) <- atomically $ do
            ss <- readTVar (_nodeSessions node)
            (a, r) <- waitAnySTM ss
            removeSession node a
            modifyTVar' (if r then _nodeSuccessConnectionCount node else _nodeFailedConnectionCount node) succ
            return (a, r)

        -- logging
        successes <- readTVarIO (_nodeSuccessConnectionCount node)
        failures <- readTVarIO (_nodeFailedConnectionCount node)
        logg Info
            $ "closed session " <> sshow (_nodeId node) <> ":" <> sshow (asyncThreadId ses)
            <> if result then " (success)" else " (failure)"
        logg Info
            $ "successes: " <> sshow successes <> ", "
            <> "failures: " <> sshow failures

    -- Make new connections
    --
    connect fromNode = forever $ do
        i <- sampleNodeIdx
        toNode <- atomically $ do
            c <- readTVar (_nodeSessionCount fromNode)
            check (c < _p2pConfigSessionCount (_nodeConfig fromNode))

            -- find next available node starting from n
            ns <- readTVar nodes
            findNextNode fromNode i ns

        -- We don't take a lock on fromNode and toNode, so there may be
        -- a race here. We just don't care if we have a few more connections.
        (fromSession, toSession) <- connectNodes fromNode toNode
        logg Info $ "connected"
            <> " from session " <> sshow fromNode <> ":" <> sshow (asyncThreadId fromSession)
            <> " to session " <> sshow toNode <> ":" <> sshow (asyncThreadId toSession)

connectNodes
    :: Node
        -- ^ from node
    -> Node
        -- ^ to node
    -> IO (Async Bool, Async Bool)
connectNodes from to = do
    (fromState, toState) <- createNewConnection from to

    mask_ $ do
        fromSession <- asyncWithUnmask $ runSessionWithConnection (_nodeSession from) fromState
        atomically $ addSession from fromSession

        toSession <- asyncWithUnmask $ runSessionWithConnection (_nodeSession to) toState
        atomically $ addSession to toSession

        -- the return value is only use for logging
        return (fromSession, toSession)

-- | Runs a session.
--
-- Returns 'True' if and only if the session terminated without a failure.
-- Terminating a session without closing the underlying connection raises an
-- failure.
--
runSessionWithConnection :: P2pSession -> ConnState -> (forall a . IO a -> IO a) -> IO Bool
runSessionWithConnection session s unmask = do
    unmask (session $ endpoint s) `catches`
        [ Handler $ \(e :: P2pConnectionException) ->
            void $ atomically $ tryPutTMVar (_failure s) (toException e)

        -- this handler ensures that the remote connection is notified even if
        -- the local connection is killed asynchronously.
        , Handler $ \(e :: SomeException) -> do
            void $ atomically $ tryPutTMVar (_failure s) e
            throwM e
        ]

    atomically $ tryReadTMVar (_isClosed s) >>= \case
        Nothing -> void
            $ tryPutTMVar (_failure s) $ toException RemoteSessionEnded
        Just _ -> return ()

    atomically (tryReadTMVar $ _failure s) >>= \case
        Just _ -> return False
        Nothing -> return True

-- | Creates a new connection between two nodes.
--
createNewConnection
    :: Node
    -> Node
    -> IO (ConnState, ConnState)
createNewConnection n₁ n₂ = do
    receiveChan₁ <- newTBQueueIO (fromIntegral $ _p2pConfigMessageBufferSize c₁)
    receiveChan₂ <- newTBQueueIO (fromIntegral $ _p2pConfigMessageBufferSize c₂)
    failureVar <- newEmptyTMVarIO
    connState₁ <- newConnState receiveChan₂ receiveChan₁ failureVar
    connState₂ <- newConnState receiveChan₁ receiveChan₂ failureVar
    return (connState₁, connState₂)
  where
    c₁ = _nodeConfig n₁
    c₂ = _nodeConfig n₂
