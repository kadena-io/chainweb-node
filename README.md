# Chainweb

Read our whitepaper: [Chainweb: A Proof-of-Work Parallel-Chain Architecture for Massive Throughput](http://kadena.io/docs/chainweb-v15.pdf)

## Building from Source

### Building with Nix

#### Dependencies

- `nix >= 2.2`
  - [Linux / Mac](https://nixos.org/nix/)

The most reliable way to build Chainweb is to use the Nix package manager.

Once Nix is installed, if you notice that the `/etc/nix` directory does not yet
exist, create it (as the `root` user). Then put the following lines in your
`/etc/nix/nix.conf` file:

```
max-jobs = auto
cores = 0
substituters = http://nixcache.kadena.io https://pact.cachix.org https://nixcache.reflex-frp.org https://cache.nixos.org/
trusted-public-keys = kadena-cache.local-1:8wj8JW8V9tmc5bgNNyPM18DYNA1ws3X/MChXh1AQy/Q= pact.cachix.org-1:cg1bsryGrHnQzqEp52NcHq4mBBL+R25XbR2Q/I/vQ8Y= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

This will allow you to connect to Kadena's Nix cache, and pull pre-built
dependencies very quickly.

Once you've done this, run the following:

```
git clone https://github.com/kadena-io/chainweb-node.git
cd chainweb-node
nix-build
```

This will place a runnable `chainweb-node` binary at a path like
`/nix/store/d2f1ybvrlxj02ikfbgmjgfyybqbxk2s6-chainweb/ghc/chainweb/bin/chainweb-node`.
This path changes with every code update, so a more reliable way to run
`chainweb-node` is to use the symlink created by Nix:

```bash
./result/ghc/chainweb/bin/chainweb-node
```

### Other Build Methods

#### Dependencies

- `ghc >= 8.4` (Haskell compiler) and `cabal >= 2.2` (Haskell build-tool)
  - [Linux / Mac](https://www.haskell.org/ghcup/)
  - [Windows](https://chocolatey.org/search?q=ghc) (via *chocolatey*)
- (optional) `stack >= 1.9` (Alternate Haskell build-tool, which installs GHC for you)
  - [Linux / Mac / Windows](https://docs.haskellstack.org/en/stable/README/)

To build the various Chainweb components, run one of the following:

```
# To build with cabal
cabal install --enable-tests

# To build with stack
stack install --test
```

This will build the Chainweb library, the `chainweb-node` executable, the main
test suite, and a few extra example executables.

### Running the test suite

There have been some reported issues with the test suite running out of file
descriptors. This may cause test suite failures. If this happens, it can be
fixed by raising ulimits as follows:

On linux add the following line to `/etc/security/limits.conf`:

```
*               soft    nofile            1048576
```

On Mac, follow [these instructions](https://unix.stackexchange.com/questions/108174/how-to-persistently-control-maximum-system-resource-consumption-on-mac).

## Running a chainweb-node

```sh
chainweb-node --node-id=0 --config-file=./scripts/test-bootstrap-node.config
```

This will run a local "bootstrap" node on your machine. Its runtime options - as
well as a hard-coded SSL certificate - are found in
`./scripts/test-bootstrap-node.config`. Further nodes can be ran with a simple:

```sh
chainweb-node --node-id=NID
```

`--interface=127.0.0.1` can be used to restrict availability of a node to the
loopback network. The default `--port` value is 0, which causes the node to
request a free port from the operating system.

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
the command line or through a configuration file.

The available command line options are shown by running:

```sh
chainweb-node --help
```

The configuration of a chainweb-node can be printed by appending
`--print-config` to the command line. Without any additional command line
arguments, `chainweb-node --print-config` shows the default configuration.

Custom configurations can be created by generating a configuration file
with the default configuration:

```sh
chainweb-node --print-config > chainweb-node.config
```

After editing the configuration file `chainweb-node.config` the custom
configuration can be loaded with

```sh
chainweb-node --config-file=chainweb-node.config
```

The directory `scripts` contains a shell script for starting a network of
chainweb-nodes and collecting the logs from all nodes:

```sh
# create directory for log files
mkdir -p tmp/run-nodes-logs

# the first argument is the path to the chainweb-node binary
./scripts/run-nodes.sh ./chainweb-node 10 ./tmp/run-nodes-logs

# stop all nodes with Ctrl-C
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
