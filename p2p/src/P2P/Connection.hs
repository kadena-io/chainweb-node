{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Modulue: P2P.Connection
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.Connection
(
-- * MonadAsync
  MonadAsync(..)

-- * P2P Connection
, P2pPeer(..)
, P2pConnectionException(..)
, P2pMessage
, P2pConnection(..)

-- * P2P Session
, P2pSession
) where

import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

import qualified Data.ByteString as B

import GHC.Generics (Generic)

import Prelude.Unicode

-- -------------------------------------------------------------------------- --
-- Monad Async

-- | Monads that support asynchronous computations.
--
class Monad m ⇒ MonadAsync m where
    async ∷ m a → m (TMVar (Either SomeException a), ThreadId)

instance MonadAsync IO where
    async action = do
        var ← newEmptyTMVarIO
        t ← action `forkFinally` (atomically ∘ putTMVar var)
        return (var,t)

instance MonadAsync m ⇒ MonadAsync (ReaderT r m) where
    async a = ask >>= lift ∘ async ∘ runReaderT a

instance MonadAsync m ⇒ MonadAsync (IdentityT m) where
    async = lift ∘ async ∘ runIdentityT

-- -------------------------------------------------------------------------- --
-- P2P Connection

data P2pPeer = Local | Remote
    deriving (Show, Eq, Generic)

-- | Exception raised by a P2P connection.
--
-- All exception from the underlying network and transport layer are wrapped
-- into 'P2pConnectionFailed' exceptions.
--
data P2pConnectionException
    = P2pConnectionClosed P2pPeer
        -- ^ Raised synchronously by 'p2pReceive', 'p2pSend', 'p2pTrySend',
        -- 'p2pClose' after 'p2pClosed' was called.

    | P2pConnectionFailed SomeException
        -- ^ Raised synchronously after a connection failure occurred on either
        -- side of the connection.

    deriving (Show, Generic)

instance Exception P2pConnectionException

-- | A P2P message is a chunked ByteString. The chunking on the local and remote
-- side are generally not equal.
--
type P2pMessage = [B.ByteString]

-- | Primitives for a P2P session. All messages are delivered eventually and in
-- order as long no failure occurs or the session is closed by one of the peers.
--
-- Message delivery is asynchronous.
--
-- Implementations can be expected to run on top of any monad transformer stack
-- with IO as a base monad.
--
-- ## Connection Closure
--
-- If a peer closes the session all subsequent 'p2pSend', 'p2pReceive',
-- 'p2pTryReceive', and 'p2pClose' calls of this peer will throw a synchronous
-- 'P2pSessionClosed' exception (or, if a connection failure occurred, a
-- 'P2pSessionFailed' exception) that can be caught using 'catch'.
--
-- Messages that were already sent but haven't yet been received by the remote
-- peer are still delivered to the remote peer. After the last message that was
-- sent before 'p2pClose' was called is received by the remote peer, the
-- connection is closed on the remote peer. This has the same effect as if the
-- remote peer called 'p2pClose' by itself. Messages sent by the remote peer
-- after 'p2pClose' was called and before the connection was closed on the
-- remote peer are discarded.
--
-- ## Connection Failures
--
-- If a connection failure occurs all subsequent calls synchronously throw a
-- 'P2pSessionFailed' exception that can be caught with `catch`. An connection
-- failure on either end eventually will result in an connection failure on the
-- both ends of the connection. A connection failure can occur even after a
-- connection was closed, for instance in the case that the connection closure
-- can't be propagated to the remote peer.
--
-- Generally, after receiving any 'PepConnectionException' the connection can't
-- be recovered.
--
-- ## TODO
--
-- For now, we assume that a P2pConnection is a mutable, thread-safe object.
-- This may change in the future. We may consider placing connections into
-- an ST monad transformer that would enforce single threaded usage of the
-- connection state.
--
data P2pConnection m = P2pConnection
    { p2pSend ∷ P2pMessage → m ()
        -- ^ Send a message to the peer. This call is non-blocking.
        --
        -- Raises a 'P2pConnectionClosed' exception after 'p2pClose' was called
        -- locally or after 'p2pClose' was called by the remote peer and all
        -- messages that were sent before that call have been received. Raises
        -- 'P2pConnectionFailure' if a connection failure occurred.

    , p2pReceive ∷ m P2pMessage
        -- ^ Receive the next message from the message queue. This call blocks
        -- if the receive queue is empty and the connection hasn't been closed
        -- by the remote peer.
        --
        -- Raises a 'P2pConnectionClosed' exception after 'p2pClose' was called
        -- locally or after 'p2pClose' was called by the remote peer and all
        -- messages that were sent before that call have been received. Raises
        -- 'P2pConnectionFailure' if a connection failure occurred.

    , p2pTryReceive ∷ m (Maybe P2pMessage)
        -- ^ Receive the next message from the message queue. Returns 'Nothing'
        -- if the queue is empty.
        --
        -- Raises a 'P2pConnectionClosed' exception after 'p2pClose' was called
        -- locally or after 'p2pClose' was called by the remote peer and all
        -- messages that were sent before that call have been received. Raises
        -- 'P2pConnectionFailure' if a connection failure occurred.

    , p2pClose ∷ m ()
        -- ^ Close this session. The peer may still receive some number of
        -- messages. No message send by the peer after this call is received and
        -- no new message is sent.
        --
        -- Raises a 'P2pConnectionClosed' exception if 'p2pClose' was called
        -- before or after 'p2pClose' was called by the remote peer and all
        -- messages that were sent before that call have been received. Raises
        -- 'P2pConnectionFailure' if a connection failure occurred.
    }

-- -------------------------------------------------------------------------- --
-- P2P Session

type P2pSession = ∀ m
    . MonadIO m
    ⇒ MonadMask m
    ⇒ MonadAsync m
    ⇒ P2pConnection m
    → m ()

