# Chainweb

A Proof-of-Work Parallel-Chain Architecture for Massive Throughput.
Read [our whitepaper](http://kadena.io/docs/chainweb-v15.pdf).

## Building from Source

Requirements:

- Cabal >=2.0
- GHC >=8.2
- (optional) Stack >=1.6

For productions builds, run one of the following:

```
# To build with cabal
cabal install --enable-tests

# To build with stack
stack install --test
```

This will build the chainweb library, the `chainweb-node` executable, and the main
test suite.

For development builds that include additional examples, you need to
pass the `dev` flag:

```bash
# To build with cabal
cabal configure -fdev --enable-tests
cabal build

# To build with stack
stack build --flag chainweb:dev
```

## Running the Examples

A simple end-to-end example for mining and synchronizing nodes for a single
chain is provided in `chaindb-sync-trivial-example`. It demonstrates simple usage
of the `P2P`, `Sync` and `ChainDB` modules of the `chainweb` library.

Provided that you built `chainweb` with the `dev` flag as shown above, you can
run the sync example with one of:

```bash
cabal run chaindb-trivial-sync-example

stack exec chaindb-trivial-sync-example
```

## Component Structure

The production components are:

*   `chainweb` library: It provides the implementation for the different
    components of a chainweb-node.

*   `chainweb-node`: An application that runs a Chainweb node. It maintains copies
    of a number of chains from a given Chainweb instance. It provides interfaces
    (command-line and RPC) for directly interacting with the chainweb or for
    implementing applications such as miners and transaction management tools.

*   `chainweb-tests`: A test suite for the chainweb library and chainweb-node.

In addition, a number of example executables are also included:

*   `chaindb-trivial-sync-example`: An simple end-to-end scenario for mining
    and synchronizing nodes for a single chain.

*   `chaindb-example`: Example for how to use the ChainDB API of chainweb.

*   `p2p-example-inprocess`: A simple p2p network implementation.

# Architecture Overview

For a detailed description of the `chainweb` architecture,
[see here](docs/Architecture.md).

![Architecture Overview](docs/Overview.png)
