# Chainweb

A Proof-of-Work Parallel-Chain Architecture for Massive Throughput.
Read [our whitepaper](http://kadena.io/docs/chainweb-v15.pdf).

## Building from Source

### Building with Nix

The most reliable way to build chainweb is to use the
[Nix](https://nixos.org/nix/) package manager. Once you've installed Nix, use
the command `nix-build` from the repository root to build chainweb. To do
incremental builds, run `nix-shell` and then `cabal build` or `cabal new-build`
inside the resulting shell.

### Other Build Methods

Requirements:

- Cabal >= 2.2
- GHC >= 8.4
- (optional) Stack >= 1.9

To build the various Chainweb components, run one of the following:

```
# To build with cabal
cabal install --enable-tests

# To build with stack
stack install --test
```

This will build the chainweb library, the `chainweb-node` executable, the main
test suite, and a few extra example executables.

## Running the Examples

A simple end-to-end example for mining and synchronizing nodes for a single
chain is provided in `single-chain-example`. It demonstrates simple usage of the
`P2P`, `Sync` and `BlockHeaderDB` modules of the `chainweb` library.

```bash
cabal run single-chain-example

stack exec single-chain-example
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

In addition, the following example executables are included which demonstrate
the use of individual sub-components:

*   `single-chain-example`: An simple end-to-end scenario for mining
    and synchronizing nodes for a single chain.

*   `blockheaderdb-example`: Example for how to use the BlockHeaderDB API of chainweb.

*   `p2p-example`: A simple p2p network implementation.

# Architecture Overview

For a detailed description of the `chainweb` architecture,
[see here](docs/Architecture.md).

![Architecture Overview](docs/Overview.png)
