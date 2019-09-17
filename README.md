# Chainweb

Read our whitepaper: [Chainweb: A Proof-of-Work Parallel-Chain Architecture for Massive Throughput](http://kadena.io/docs/chainweb-v15.pdf)

With our recent release of Chainweb Testnet v3, we now have a public network to
which anyone can connect, plus the ability to mine (either by using the
chainweb-miner or by running a node). As before, we provide a binary for Linux
and Mac users can follow the instructions to build from source and then run a
Chainweb node.

Below are the addresses of the Bootstrap nodes of the public Chainweb Testnet
network: (insert addresses of the nodes)

## Table of Contents

- [Download Instructions for Linux Users](#linux-users)
- [Download and Build Instructions for Mac Users](#mac-users)
- [Run a Chainweb node](#running-a-chainweb-node)
- [Configure a Chainweb node](#configuring-a-chainweb-node)
- [Mine for a Chainweb Network](#mine-for-a-chainweb-network)
- [Component Structure Details](#component-structure)
- [Architecture Overview](#architecture-overview)

## Linux Users
Download the binary: [chainweb-node-testnet-v2-ubuntu-18.04
168](https://github.com/kadena-io/chainweb-node/releases/download/testnet-v2/chainweb-node-testnet-v2-ubuntu-18.04) (need new link)

You will need to install rocksdb with the following command:

```bash
sudo apt install librocksdb5.8
```

At this point, you are ready to [run a Chainweb node](#running-a-chainweb-node)

## Mac Users
### Building from Source

Chainweb is a [Haskell](https://www.haskell.org/) project, and can be built in
several ways.

#### Getting the Code

##### Dependencies
- Homebrew: `brew install git`
- [Installer](https://git-scm.com/downloads)

To get the code, you can either use the [Chainweb-node Testnet v2 zip](https://github.com/kadena-io/chainweb-node/archive/testnet-v2.zip) or the [Chainweb-node Testnet v2 tar](https://github.com/kadena-io/chainweb-node/archive/testnet-v2.tar.gz)


You have the code, now let's pick a build tool.

#### Building with Nix

The fastest way to build and run chainweb is to use the Nix package manager
which has binary caching capabilities that allow you to download pre-built
binaries for everything needed by Chainweb. For detailed instructions see [our
wiki](https://github.com/kadena-io/pact/wiki/Building-Kadena-Projects).


When the build is finished, you can run chainweb with the following command:

```bash
./result/ghc/chainweb/bin/chainweb-node
```

#### Building with Stack

##### Dependencies

- `stack >= 1.9`
  - Mac (Homebrew): `brew install haskell-stack`
  - General [Linux / Mac](https://docs.haskellstack.org/en/stable/README/)

(You may also need to install `zlib`, `openssl`, and `sqlite`.)

Stack is a Haskell build tool that manages compiler and dependency versions for
you. It's easy to install and use.

To build a `chainweb-node` binary:

```bash
stack build
```

This will compile a runnable version of `chainweb-node`, which you can run via:

```bash
stack exec -- chainweb-node
```

Alternatively, `stack install` will install the binary to `~/.local/bin/`, which
you may need to add to your path. Then, you can call `chainweb-node` as-is.

#### Building with Cabal

##### Dependencies

- `ghc >= 8.4` (Haskell compiler) and `cabal >= 2.2` (Haskell build-tool)
  - [Linux / Mac](https://www.haskell.org/ghcup/)

(You may also need to install `zlib`, `openssl`, and `sqlite`.)

Cabal is the original build tool for Haskell. You will need a version of GHC
installed on your machine to use it.

To build a `chainweb-node` binary:

```bash
# Only necessary if you haven't done this recently.
cabal new-update

# Build the project.
cabal new-build
```

To install a runnable binary to `~/.cabal/bin/`:

```bash
cabal new-install
```



## Running a chainweb node

This section assumes you've installed the `chainweb-node` binary somewhere
sensible, or otherwise have a simple way to refer to it. Please note that by
default, the in-process mining is turned on; for instructions on how to turn it
off, please refer to the [Mining Guide](https://github.com/kadena-io/chainweb-node/blob/master/miner/README.org).

(Below command might need updating)
To run a node:

```bash
chainweb-node --node-id=0 --config-file=./tools/run-nodes/test-bootstrap-node.config
```

This will run a local "bootstrap" node on your machine. Its runtime options - as
well as a hard-coded SSL certificate - are found in
`./tools/run-nodes/test-bootstrap-node.config`. Further nodes can be ran with a simple:

```bash
chainweb-node --node-id=NID
```

`--interface=127.0.0.1` can be used to restrict availability of a node to the
loopback network. The default `--port` value is 0, which causes the node to
request a free port from the operating system.

Alternatively, we provide an additional script - `run-nodes` - for starting a
network of `chainweb-node`s and collecting the logs from each:

```bash
# Create directory for log files.
mkdir -p tmp/run-nodes-logs

# By default, run 10 nodes locally.
run-nodes --exe=path/to/chainweb-node -- --telemetry-log-handle=file:./tmp/run-nodes-logs

# Stop all nodes with Ctrl-C
```

Any option after `--` will be passed as-is to each `chainweb-node` instance.

See `run-nodes --help` for a complete list of its options.

### Details

A chainweb-node has two identifiers:

(below sections needs updating)
*   The node-id is a permanent identifier that is used for the `miner`
    field in the header of newly mined blocks.
    * In its current form, it is a placeholder for an identity, e.g. a public
      key, that in the future will be provided by the Pact layer.
    * If such an identity doesn't exist or isn't needed, the node-id may be
      removed completely or kept only for debugging purposes.
    * The user must provide each node with a unique node-id on startup.

*   The peer-id is used to identify the node in the peer-to-peer network.
    * It is a fingerprint of an ephemeral X509 certificate that, if not provided
      in the configuration, is created automatically and can be dropped and
      recreated at any time.
    * Since the peer-id is used in caches and for reputation management, nodes
      are incentivised to persist and reuse peer-ids.
    * When no peer-id is provided, a node generates a new peer-id on startup.

Upon startup, a `chainweb-node` tries to connect to the P2P network. Each
`chainweb-node` knows about a hard-coded set of bootstrap nodes. For the *Test*
node, this is a single node with host-name `localhost`, and port `1789`.


## Configuring a chainweb node

Alternative or additional bootstrap nodes can be specified at startup, either on
the command line or through a configuration file:

```bash
chainweb-node ... --known-peer-info=<some-ip-address>:<some-port>
```

All available command line options are shown by running:

```bash
chainweb-node --help
```

The configuration of a chainweb-node can be printed by appending
`--print-config` to the command line. Without any additional command line
arguments, `chainweb-node --print-config` shows the default configuration.

Custom configurations can be created by generating a configuration file
with the default configuration:

```bash
chainweb-node --print-config > chainweb-node.config
```

After editing the configuration file `chainweb-node.config` the custom
configuration can be loaded with

```bash
chainweb-node --config-file=chainweb-node.config
```

## Mine for a Chainweb Network

Detailed mining instructions can be found in our [Mining Guide](https://github.com/kadena-io/chainweb-node/blob/master/miner/README.org).

## Component Structure

The production components are:

*   `chainweb` library: It provides the implementation for the different
    components of a chainweb-node.

*   `chainweb-node`: An application that runs a Chainweb node. It maintains copies
    of a number of chains from a given Chainweb instance. It provides interfaces
    (command-line and RPC) for directly interacting with the Chainweb or for
    implementing applications such as miners and transaction management tools.

*   `chainweb-tests`: A test suite for the Chainweb library and chainweb-node.

In addition, the following example executables are included to demonstrate
the use of individual sub-components:

*   `single-chain-example`: A simple end-to-end scenario for mining
    and synchronizing nodes for a single chain.

*   `blockheaderdb-example`: Example for how to use the BlockHeaderDB API of Chainweb.

*   `p2p-example`: A simple p2p network implementation.

## Architecture Overview

For a detailed description of the `chainweb` architecture,
[see here](docs/Architecture.md).

![Architecture Overview](docs/Overview.png)
