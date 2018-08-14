{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: P2p
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- P2P Network Interfact
--
module P2p
(
-- * P2P Connection
  P2pPeer(..)
, P2pConnectionException(..)
, P2pMessage
, P2pConnection(..)

-- * P2P Session
, P2pSession

-- * P2P Node
, P2pConfiguration
, p2pNode
) where

import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.ByteString as B

import GHC.Generics (Generic)

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
    ⇒ MonadThrow m
    ⇒ MonadCatch m
    ⇒ P2pConnection m
    → m ()

-- -------------------------------------------------------------------------- --
-- P2P Node

-- | Configuration of the Network
--
-- Implementation specific
--
data P2pConfiguration

-- | Runs a node in a peer to peer network. A node establishes and maintains a
-- number of connections with other peers and concurrently runs the given
-- session of type @P2pConnection → m ()@ for each connection. Sessions may be
-- terminating or non-terminating. If a session exits without closing the
-- connection a connection failure is raised at the remote peer.
--
-- There is exactly one connection per session and one session per connection.
-- Sessions may remain active even after a connection failure. The only
-- guaranteed way to kill an active connection externally is by terminating the
-- 'p2pNode' by raising an assynchronous exception. Implementations may
-- implement additional ways like asynchronously killing sessions after a
-- certain time of inactivity.
--
-- Exception that are not connection specific are  propagated to the 'p2pNode'
-- and subsequently to all sessions. Local sessions are termianted
-- asynchronously and an failure is raised in remote sessions.
--
-- It is implementation specific how a 'p2pNode' allocates connections and
-- sessions. Some implementation may try to keep a certain number of sessions
-- active independent of the number of /open/ connections. Some implementations
-- may try to keep a certain number of /open/ connections independent of the
-- number of active sessions. And some implementations my enforce bounds both on
-- connections and /open/ sessions.
--
p2pNode
    ∷ P2pConfiguration
    → P2pSession
    → IO ()
p2pNode configuration session = error "not implemented"

