<p align="center">
<img src="https://i.imgur.com/bAZFAGF.png" width="450" height="243" alt="Kadena" title="Kadena">
</p>

<p>&nbsp;</p>

# Kadena Public Blockchain


Kadena is a fast, secure, and scalable blockchain using the Chainweb consensus protocol. Chainweb is a braided, parallelized Proof Of Work consensus mechanism that improves throughput and scalability while maintaining the security and integrity found in Bitcoin. 

Read our [whitepapers](https://www.kadena.io/whitepapers):

- [Chainweb: A Proof-of-Work Parallel-Chain Architecture for Massive Throughput](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_029c9991469e4565a7c334dd716345f4.pdf)
- [Agent-based Simulations of Blockchain Protocols illustrated via Kadena's *Chainweb*](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_3b2d0c58179d4edd9df6df4d55d61dda.pdf)

For additional information, press, and development inquires, please refer to the Kadena [website](https://kadena.io)

## Table of Contents

- [Wiki](#chainweb-wiki)
- [Installing Chainweb](#installing-chainweb)
  - [Instructions for Linux Users](#linux-users)
  - [Instructions for Mac Users](#mac-users)
- [Running Chainweb Node](#running-chainweb-node)
  - [Configuring a Chainweb Node](#configuring-a-chainweb-node)
    - [Specifying Bootstrap Nodes](#specifying-bootstrap-nodes)
    - [Specifying your Public Identity](#specifying-your-public-identity)
    - [Specifying your Mining Identity](#specifying-your-mining-identity)
    - [Specifying a Log Level](#specifying-a-log-level)
  - [Mining for a Chainweb Network](#mine-for-a-chainweb-network)
- [Chainweb Design](#chainweb-design)
  - [Component Structure Details](#component-structure)
  - [Architecture Overview](#architecture-overview)

## Wiki

The Chainweb wiki serves as a source of information that receives regular updates. You can find the chainweb Wiki [here](https://github.com/kadena-io/chainweb-node/wiki/), including a list of frequently asked questions regarding  network information, how to explore blocks, diagnosing error messages, and more [here](https://github.com/kadena-io/chainweb-node/wiki/Chainweb-FAQ).

If you have additions or comments, please submit a pull request or raise an issue.


## Installing Chainweb
### Linux Users

The binaries can be found [here](https://github.com/kadena-io/chainweb-node/releases)

#### Apt-based distributions
If you are on Ubuntu, Debian, CentOS or any other Apt-based distribution, you will need to install rocksdb with the following command:

```bash
sudo apt-get update
```

```bash
sudo apt-get install -y librocksdb-dev zlib1g-dev libtinfo-dev libsqlite3-dev libz3-dev z3
```

If this is not available, then please view the [Rocksdb](https://rocksdb.org/) site for alternative modes of installation.

#### Other distributions

For all other distributions not using Apt (RHEL, Gentoo, Arch, etc), please consult your distro's repositories for `librocksdb5.8`, `tinfo`, `zlib`, `z3` and install with its preferred package manager, or follow the alternative modes of installation described in [Rocksdb](https://rocksdb.org/).

At this point, you are ready to [run a Chainweb node](#running-a-chainweb-node)

### Mac Users

#### Getting the Dependencies

Using the `brew` package manager, issue the following commands to download Chainweb's dependencies

```bash
brew update
brew install z3
brew install sqlite
brew install rocksdb
```

#### Building from Source

Chainweb is a [Haskell](https://www.haskell.org/) project, and can be built in
several ways.

##### Getting the Code

###### Dependencies
- Homebrew: `brew install git`
- [Installer](https://git-scm.com/downloads)

To get the code, you can go [here](https://github.com/kadena-io/chainweb-node/releases/tag/1.0)

You have the code, now let's pick a build tool.

##### Building with Nix

The fastest way to build and run chainweb is to use the Nix package manager
which has binary caching capabilities that allow you to download pre-built
binaries for everything needed by Chainweb. For detailed instructions see [our
wiki](https://github.com/kadena-io/pact/wiki/Building-Kadena-Projects).


When the build is finished, you can run chainweb with the following command:

```bash
./result/ghc/chainweb/bin/chainweb-node
```

##### Building with Stack

###### Dependencies

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

##### Building with Cabal

###### Dependencies

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

## Running Chainweb Node

**This section assumes you've installed the `chainweb-node` binary** somewhere
sensible, or otherwise have a simple way to refer to it. Please note that by
default, the in-process mining is turned on; for instructions on how to turn it
off, please refer to the [Mining Guide](https://github.com/kadena-io/chainweb-node/blob/master/miner/README.org).

Chainweb has many configuration options. Although there are command-line flags
for all of them, in practice we use a config file:

```bash
./chainweb-node --print-config > config.yaml
```

Then, to run a node:

```bash
./chainweb-node --config-file=config.yaml
```

This will run a local Node on your machine, and you will see a flurry of
activity. However, your Node won't be connected to the wider network yet. For
that, we must configure Chainweb and change some defaults.

### Configuring a Chainweb Node

All available command-line options are shown by running:

```bash
chainweb-node --help
```

But we recommend working with a configuration file. The following instructions
assume that you have generated such a file, as shown above.

#### Specifying Bootstrap Nodes

To connect to the wider network, we must communiciate with some initial Peer. We
call such a peer a *Bootstrap Node*. We define these in the `peers` list:

```yaml
chainweb:
  ... # other settings
  p2p:
    ... # other settings
    peers: []
```

Let's update it to include one of Kadena's public Bootstraps:

```yaml
peers:
  - address:
      hostname: us1.testnet.chainweb.com
      port: 443
    id: null
```

Since `peers` is a list, you can specify as many as you like, including other
powerful Nodes that you manage.

#### Specifying your Public Identity

You need to inform other Nodes how to talk back to you. This is also the
information that they send along to their neighbours as part of the Peer
Network:

```yaml
chainweb:
  p2p:
    peer:
      ... # other settings
      hostaddress:
        hostname: localhost
        port: 0
```

`localhost` is no good.

```yaml
hostaddress:
  hostname: <your-public-ip-here>
  port: 443
```

Keep in mind that you may have to perform Port Forwarding if your machine is
behind a router.

#### Specifying your Mining Identity

See our [Mining Guide](https://github.com/kadena-io/chainweb-node/blob/master/miner/README.org)
for details. Without a properly defined Mining Identity, your mining effort will
be wasted.

Don't want to mine? Use either `--disable-mining` on the command-line, or set:

```yaml
chainweb:
  miner:
    enable: false
```

#### Specifying a Logging Level

Chainweb runs on `Info` by default. If you'd prefer something quieter, like `Warn`, set:

```yaml
logging:
  logger:
    log_level: warn
```

### Mine for a Chainweb Network

Detailed mining instructions can be found in our [Mining Guide](https://github.com/kadena-io/chainweb-node/blob/master/miner/README.org).

## Chainweb Design

### Component Structure

The production components are:

*   `chainweb` library: It provides the implementation for the different
    components of a chainweb-node.

*   `chainweb-node`: An application that runs a Chainweb node. It maintains copies
    of a number of chains from a given Chainweb instance. It provides interfaces
    (command-line and RPC) for directly interacting with the Chainweb or for
    implementing applications such as miners and transaction management tools.

*   `chainweb-miner`: A stand-alone Mining Client.

*   `chainweb-tests`: A test suite for the Chainweb library and chainweb-node.

### Architecture Overview

For a detailed description of the Kadena architecture,
[see here](docs/Architecture.md).

![Architecture Overview](docs/Overview.png)
