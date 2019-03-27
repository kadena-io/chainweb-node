# Chainweb

Read our whitepaper: [Chainweb: A Proof-of-Work Parallel-Chain Architecture for Massive Throughput](http://kadena.io/docs/chainweb-v15.pdf)

## Building from Source

Chainweb is a [Haskell](https://www.haskell.org/) project, and can be built in
several ways.

### Getting the Code

#### Dependencies

- `git`
  - [Linux](https://git-scm.com/download/linux)
  - Mac
    - Homebrew: `brew install git`
    - [Installer](https://git-scm.com/downloads)

To fetch the code:

```bash
git clone https://github.com/kadena-io/chainweb-node.git
```

You have the code, now let's pick a build tool.

### Building with Nix

#### Dependencies

- `nix >= 2.2`
  - [Linux / Mac](https://nixos.org/nix/)

Using Nix is a great option if you just want Chainweb built, and don't
necessarily care about the Haskell toolchain. It will also pull prebuilt
versions of all of Chainweb's dependencies, avoiding the need to compile them
yourself.

Once Nix is installed, if you notice that the `/etc/nix` directory does not yet
exist, create it (as the `root` user). Then put the following lines in your
`/etc/nix/nix.conf` file to connect to Kadena's cache:

```
max-jobs = auto
cores = 0
substituters = http://nixcache.kadena.io https://pact.cachix.org https://nixcache.reflex-frp.org https://cache.nixos.org/
trusted-public-keys = kadena-cache.local-1:8wj8JW8V9tmc5bgNNyPM18DYNA1ws3X/MChXh1AQy/Q= pact.cachix.org-1:cg1bsryGrHnQzqEp52NcHq4mBBL+R25XbR2Q/I/vQ8Y= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

Once you've done this, run the following from the director you cloned Chainweb
to:

```
nix-build
```

This will place a runnable `chainweb-node` binary at a path like
`/nix/store/d2f1ybvrlxj02ikfbgmjgfyybqbxk2s6-chainweb/ghc/chainweb/bin/chainweb-node`.
This path changes with every code update, so a more reliable way to run
`chainweb-node` is to use the symlink created by Nix:

```bash
./result/ghc/chainweb/bin/chainweb-node
```

### Building with Stack

#### Dependencies

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

### Building with Cabal

#### Dependencies

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

## Running a `chainweb-node`

This section assumes you've installed the `chainweb-node` binary somewhere
sensible, or otherwise have a simple way to refer to it.

To run a node:

```bash
chainweb-node --node-id=0 --config-file=./scripts/test-bootstrap-node.config
```

This will run a local "bootstrap" node on your machine. Its runtime options - as
well as a hard-coded SSL certificate - are found in
`./scripts/test-bootstrap-node.config`. Further nodes can be ran with a simple:

```bash
chainweb-node --node-id=NID
```

`--interface=127.0.0.1` can be used to restrict availability of a node to the
loopback network. The default `--port` value is 0, which causes the node to
request a free port from the operating system.

Alternatively, the directory `scripts` contains a shell script for starting a
network of `chainweb-node`s and collecting the logs from all nodes:

```bash
# create directory for log files
mkdir -p tmp/run-nodes-logs

# the first argument is the path to the chainweb-node binary
./scripts/run-nodes.sh ./chainweb-node 10 ./tmp/run-nodes-logs

# stop all nodes with Ctrl-C
```

### Details

A chainweb-node has two identifiers:

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

### A Word on Testnet Bootstrap Nodes

For our first iteration of Testnet, while we do have Bootstrap nodes running
across the web, we haven't revealed these to the public. This is for our own
testing purposes. Eventually these nodes will be revealed and everyone will be
able to participate in the global network.

## Configuring a chainweb-node

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
