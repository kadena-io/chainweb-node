# P2P Network API

The P2P network API defines the interface of the P2P layer. It consists of two
main components.

1.  A *P2P Session* is a callback that is given a *P2P.Connection* structure
    that allows the local session to communicate with a remote session.

    ```haskell
    data P2pConnection m = P2pConnection
        { p2pSend ∷ P2pMessage → m ()
        , p2pReceive ∷ m P2pMessage
        , p2pTryReceive ∷ m (Maybe P2pMessage)
        , p2pClose ∷ m ()
        }

    type P2pSession = ∀ m
        . MonadIO m
        ⇒ MonadMask m
        ⇒ MonadAsync m
        ⇒ P2pConnection m
        → m ()
    ```

    The session callback doesn't expose anything about the identity of the
    remote endpoint or about how and when a connection is established.

    Sessions have access to IO and can thus manage and persist local state
    across sessions and fork concurrent threads. However, for a forked thread to
    use communication primitives, it must be forked using `async` primitive of
    the  `MonadAsync` class. The reason for this is to support better testing
    and simulation.

    If a session needs to identify remote endpoints it has to do this within the
    session itself. For instance, a session may implement a protocol that makes
    use of vector clocks that include sharing an identifier among all sessions
    of a node. This identifier would be different from the host-identifiers used
    by the underlying P2P network.

    The `P2pSession` and `Connection` types are defined in
    `p2p/src/P2P/Node/Connection.hs`.

2.  A *P2P Node* is an heavy-weight stateful component that serves *P2P
    sessions*. It listens for incoming connections and makes outgoing requests
    for connections. When a connection is established it runs a session on that
    connection. At any time it tries to maintain a configurable number of active
    sessions. It also implements the topological invariants and other
    requirements of the P2P network.

    ```haskell
    p2pNode
        ∷ P2pConfiguration
        → P2pSession
        → IO ()
    ```

    The interface of a *P2P Node* is defined in `p2p/signatures/P2P/Node.hsig`.
    A prototype (that runs all nodes in a single process) is implemented in
    `p2p/inprocess/P2P/Node/InProcess.hs`.

There is one instance of a `P2P.Node` per single chain. All P2P instances of a
for all chains of a chainweb node share the underlying connection pool layer.

