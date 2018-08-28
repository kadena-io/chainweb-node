# Chainweb

## Building from Source

Requirements:

*   Cabal >=2.0
*   GHC >=8.2

For productions builds just run

```
cabal install --enable-tests -j1
```

This will build the chainweb library, the chainweb-node executable, and the main
test suite.

(Use `-j1` or `--jobs=1` if `jobs:` is configured in the cabal config file.
Backback doesn't support parallel compilation.)

For development builds that include additional examples and tests you have to
pass the `-fdev` flag to cabal:

```bash
cabal configure -fdev --enable-tests
cabal build --jobs=1
```

To just check that all production components comply with their respective
interface specifications run

```bash
cabal configure -fdev
cabal build --jobs=1 lib:check-signatures
```

## Running the Examples

A simple end-to-end example for mining and synchronizing nodes for a single
chain is provided in `chaindb-sync-trivial-example`. It demonstrates the
feasibility of the architecture and the APIs by integrating simple prototype
implementations of the P2P, sync, and ChainDB components on top of the
datastructures in the chainweb-types library.

```bash
cabal run --jobs=1 chaindb-trivial-sync-example
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

*   chaindb-hashmap-indef: an implementation of the ChainDB signatures that uses
    an in-memory hashmap for storing block headers. It is parameterized over the
    entry type. Currently the chainweb library uses this implementation. In the
    future it will be replaced and moved to the test library.

*   chaindb-sync-trivial-indef: a prototype implementation of the component
    for synchronizing the blockheaders of a single chain between two nodes.
    It implements a `P2pSession` that can be used with the P2P Api. It works
    with any implementation of the `ChainDB` Api. This is a prototype
    implementation that will be replaced with a production implementation in the
    future.

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
    of the `ChainDB` and `P2P` APIs by integrating prototype implementations
    of these APIs on top of the data-structures in `chainweb-types`.

*   chaindb-example: Example for how to use the ChainDB API of chainweb.

*   chaindb-example-int: Example for how to use ChainDB API with a mock
    implementation for `Chainweb.ChainDB.Entry`.

## Source Code Layout

*   `src`: contains the source code for the chainweb library.
*   `node`: contains the source code for the chainweb-node application.
*   `test/chainweb` contains the code for the chainweb-tests test-suite.

*   `signatures`: contains all signatures files
*   `example`: contains example code for using different APIs and components.

*   `docs`: contains documentation files

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

