# Chainweb

## Building from Source

Requirements:

*   Cabal >=2.0
*   GHC >=8.2

For productions builds just run

```
cabal install --enable-tests
```

This will build the chainweb library, the chainweb-node executable, and the main
test suite.

For development builds that include additional examples and tests you have to
pass the `-fdev` flag to cabal:

```bash
cabal configure -fdev --enable-tests
cabal build
```

To just check that all production components comply with their respective
interface specifications run

```bash
cabal configure -fdev --enable-tests
cabal test check-signatures
```

## Running the Examples

A simple end-to-end example for mining and synchronizing nodes for a single
chain is provided in `chaindb-sync-trivial-example`. It demonstrates the
feasibility of the architecture and the APIs by integrating simple prototype
implementations of the P2P, sync, and ChainDB components on top of the
datastructures in the chainweb-types library.

```bash
cabal run chaindb-trivial-sync-example
```

## Component Structure

The production components are:

*   chainweb library: It provides the implementation for the different
    components of a chainweb-node.

*   chainweb-node: An application that runs a Chainweb node. It maintains copies
    of a number of chains from a given Chainweb instance. It provides interfaces
    (command-line and RPC) for directly interacting with the chainweb or for
    implementing applications such as miners and transaction management tools.

*   chainweb-tests: A test suite for the chainweb library and chainweb-node.

The production components depend on the following internal libraries

*   chainweb-types library: defines and implements all basic Chainweb types.
    It doesn't depend on any interfaces.

*   chaindb: a database for storing the block headers for a single chain.
    The current implementation uses `chaindb-hashmap` that uses an in-memory
    hash table for storing the blockheaders.

*   chaindb-sync: a library for synchronizing the remote block header databases
    over a P2P network. The current implementation uses `chaindb-sync-trivial`,
    which uses a very simple synchronization strategy that has complexity that
    is proportional to the size of the databases and thus unbounded. In the
    future it will be replaced by a more efficient implementation.

*   p2p: a P2P network library. It currently uses `p2p-node-inprocess` as
    implementation for the a P2P node. In the future it will be replaced by an
    implementation that uses TCP over a real network.

*   chainweb-test: a library for modules that are shared between different test,
    simulation, and example components.

In addition there are a few development components that provide examples for
using the production components and for advanced testing and simulation.
Currently, these include

*   signatures: a collection of interfaces for different components of the
    chainweb library. These serve as (type-checkable) formal specifications of
    the APIs. They also allow developers to (temporarily) compile against
    chainweb components that are not yet fully implemented. Finally, they allow
    test suites and simulations to the use the chainweb library with mock
    implementations or alternate implementations of certain APIs.

*   check-signatures: An internal library that when build tests that all
    production implementations in the chainweb library comply with their
    respective API specification.

*   chaindb-trivial-sync-example: An simple end-to-end scenario for mining
    and synchronizing nodes for a single chain. It demonstrates the feasibility
    of the `ChainDB` and `P2P` APIs by integrating prototype implementations of
    these APIs on top of the data-structures in `chainweb-types`.

*   chaindb-example: Example for how to use the ChainDB API of chainweb.

*   chaindb-example-int: Example for how to use ChainDB API with a mock
    implementation for `Chainweb.ChainDB.Entry`.

The following indefinite components can be used to switch the underlying
implementations without modifying the source code:

*   chaindb-hashmap-indef: an implementation of the ChainDB signatures that uses
    an in-memory hashmap for storing block headers. It is parameterized over the
    entry type.

*   chaindb-sync-trivial-indef: a prototype implementation of the component
    for synchronizing the blockheaders of a single chain between two nodes. It
    implements a `P2pSession` that can be used with the P2P Api. It works with
    any implementation of the `ChainDB` Api.

*   p2p-indef: P2P network that is parameterized over the `P2P.Node` signature
    that instantiates the underlying network transport.

## Source Code Layout

*   `src`: contains the source code for the chainweb-types library.
*   `node`: contains the source code for the chainweb-node application.
*   `test/chainweb` contains the code for the chainweb-tests test-suite.

*   `signatures`: contains all signatures files
*   `example`: contains example code for using different APIs and components.

*   `docs`: contains documentation files

# Chainweb Types Library

This internal library contains all basic chainweb types. The source code lives
in the `src` directory.

# ChainDB Library

The ChainDB Library is a database for content-addressed, ranked, rooted, trees,
that grow monotonically by adding new leafs. It also includes functionality for
synchronizing remote ChainDB instances over a P2P network.

## Source Code Layout

All code and resources for the ChainDB library live in the `chaindb` directory.

*   `chaindb/src`: contains source code for the modules of ChainDB and Sync
    components.
*   `chaindb/test`: contains source code for the tests-suites for the
    ChainDB library.

In addition to these public production components, the are the following
development components:

*   `chaindb/signatures`: contains all signatures files.
*   `chaindb/example`: contains example code for using the ChainDB and Sync
    APIs.

# P2P Network

The P2P Network layer is developed as an independent library with the chainweb
package. Once its API has stabilized it will be moved into a cabal package on
its own.

## Source Code Layout

All code and resources for the P2P library live in the `p2p` directory.

*   `p2p/src`: contains source code of the P2P library modules.
*   `p2p/test`: contains the source code for the test suites for the P2P
    library.

In addition to these public production components, the are the following
development components:

*   `p2p/signatures`: contains all signatures files.
*   `p2p/inprocess`: contains an simple in-process implementation of the P2P
    library API that can be used for testing and simulation purposes.
*   `p2p/example`: contains example code for using the P2P library API.

# Architecture Overview

There are currently three main APIs that formalize the results of the
architecture meetings from my last visit in NY.

## P2P Network API

This defines the interface of the P2P layer. It consists of two main components.

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

    The session callback doesn't expose any anything about the identity of the
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
    sessions. It also implements the topological invaraints and other
    requirements of the P2P network.

    ```haskell
    p2pNode
        ∷ P2pConfiguration
        → P2pSession
        → IO ()
    ```

    The interface of a *P2P Node* is defined in `p2p/signatures/P2P/Node.hsig`.
    An prototype (that runs all nodes in a single process) is implemented in
    `p2p/inprocess/P2P/Node/InProcess.hs`.

There is one instance of a `P2P.Node` per single chain. All P2P instances of a
for all chains of a chainweb node share the underlying connection pool layer.

## Single Chain Database API

The single chain database API defines the interface to query and update all
block headers for a local copy of single block chain. It uses a data model
where the block headers are organized in a content addressed, monotonically
increasing, rooted, and ranked tree.

The interface provides

*   the list of branches of the tree represented by the keys of the leave
    nodes,
*   a rank function that returns for each node in the tree its distance from
    the root,
*   a parent relation that allows to traverse the tree from the leaves to
    the root,
*   a children relation that allows to traverse the tree in the direction
    from the root to the leaves,
*   an methods to add new children to a node in the tree, and
*   an infinite updates stream, that allows to traverse the tree in an arbitrary
    but deterministic order that is compatible with the rank of the tree nodes
    and that can be used to await newly added tree nodes.

The interface also provides functions for binary encoding and decoding tree
nodes.

The `ChainDB` interface is defined in the file
`signatures/Chainweb/ChainDB.hsig`. A prototype that is based on an in-memory
`HashMap` is implemented in the file `src/Chainweb/ChainDB/HashMap.hs`. An
instantiation of tree entries with the `Chainweb.BlockHeader` type is provided
in the file `src/Chainweb/ChainDB/Entry/BlockHeader.hs`. This separation allows
changing the `BlockHeader` type without affecting components that rely on the
`ChainDB` interface. Once the definition chainweb data-types has stabilized this
level of indirection may be removed.

## Single Chain Block Header Synchronization API

The single chain synchronization API defines the interface for components that
synchronize the local copy of a single chain block header database over a
P2P network with the copies of the block header database of remote chain nodes.

An implementation of this API provides `syncSession` function that, given a logging
function, and a `ChainDB` handle, returns a `P2PSession`:

```haskell
syncSession
    ∷ ∀ m
    . MonadMask m
    ⇒ MonadAsync m
    ⇒ MonadIO m
    ⇒ (LogLevel → T.Text → m ())
    → ChainDb
    → P2pConnection m
    → m ()
```

The `P2PSession` can then be given to an instance of a `P2P.Node` which will
establish connections to other nodes in the P2P network and run the session
which will synchronize the data bases.

The `syncSession` function is defined in the file
`signatures/Chainweb/ChainDB/SyncSession.hsig`. A prototype for a sync session
is implemented in the file `src/Chainweb/ChainDB/Sync/Trivial.hs`.

## Chainweb Types

This API defines the basic data types of a chainweb block chain along with
supporting types and functions. The top-most data types are

*   Chainweb.BlockHeader (defined in `src/Chainweb/BlockHeader.hs`), and
*   Chainweb.Chain (defined in `src/Chainweb/Chain.hs`).

The latter `Chainweb.Chain` module is not yet integrated in an example scenario
and still subject to change.

Other components that are currently work in progress are

*   Chainweb.Braiding, which defines the braiding structure of chainweb,
*   Chainweb.WebChain, which defines a collection of chains in a chainweb, and
*   Chainweb.WebBranch, which defines the notion of a chainweb branch/cut.

The notion of a *web branch* or *cut* provides the basis for defining mining
strategies for chainweb miners. The latter three modules are in the process
of being ported from the haskell-chainweb-simulation repository.

## Single Chain End-To-End Scenario

The file `example/TrivialSync.hs` provides an end-to-end scenario the integrates
the prototype implementations for the P2P network, single chain database, and
single chain synchronization APIs. It proves the feasibility of the architecture
and allows early end-to-end testing while developing more advanced
implementations for these APIs and related components.

The example can be executed via

```bash
cabal run -j1 chaindb-trivial-sync-example
```

It requires GHC >= 8.2 and cabal >= 2.0. It has been tested on Mac OS X
and Ubuntu/Linux.

