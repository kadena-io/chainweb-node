# Chainweb

## Building from Source

For productions builds just run

```
cabal install --enable-tests
```

This will build the chainweb library, the chainweb-node executable, and the main
test suite.

For development builds that include additional examples and tests you have to
pass the `-fdev` flag to cabal:

```
cabal configure -fdev --enable-tests
cabal build --jobs=1
```

(Use `--jobs=1` if `jobs:` is configured in the cabal config file. Backback doesn't
support parallel compilation.)

To just check that all production components comply with their respective
interface specifications run

```
cabal configure -fdev
cabal build --jobs=1 lib:check-signatures
```

## Component Structure

The production components are:

*   chainweb library: That provides the implementation for the different
    components of a chainweb-node.

*   chainweb-node: An application that runs a chainweb node. It maintains copies
    of a number of chains from a given chainweb instance. It provides interfaces
    (command-line and RPC) for directly interacting with the chainweb or for
    implementing applications such as miners and transaction management tools.

*   chainweb-tests: A test suite for the chainweb library and chainweb-node.

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

*  chaindb-example: Example for how to use the ChainDB API of chainweb.

## Source Code Layout

*   `src`: contains the source code for the chainweb library.
*   `node`: contains the source code for the chainweb-node application.
*   `test/chainweb` contains the code for the chainweb-tests test-suite.

*   `signatures`: contains all signatures files
*   `example`: contains example code for using different APIs and components.

*   `docs`: contains documentation files

