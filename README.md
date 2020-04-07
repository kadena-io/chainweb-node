<p align="center">
<img src="https://i.imgur.com/bAZFAGF.png" width="450" height="243" alt="Kadena" title="Kadena">
</p>

<p>&nbsp;</p>

# Kadena Public Blockchain


Kadena is a fast, secure, and scalable blockchain using the Chainweb consensus
protocol. Chainweb is a braided, parallelized Proof Of Work consensus mechanism
that improves throughput and scalability in executing transactions on the blockchain while maintaining the security and
integrity found in Bitcoin.

Read our [whitepapers](https://www.kadena.io/whitepapers):

- [Chainweb: A Proof-of-Work Parallel-Chain Architecture for Massive Throughput](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_029c9991469e4565a7c334dd716345f4.pdf)
- [Agent-based Simulations of Blockchain Protocols illustrated via Kadena's *Chainweb*](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_3b2d0c58179d4edd9df6df4d55d61dda.pdf)

For additional information, press, and development inquires, please refer to the Kadena [website](https://kadena.io)

## Table of Contents

- [Kadena Docs Site](#docs)
- [Installing Chainweb](#installing-chainweb)
  - [Instructions for Linux Users](#linux-users)
  - [Instructions for Mac Users](#mac-users)
- [Bootstrap Nodes](#bootstrap-nodes)  
- [Configuring, running, and monitoring the health of a Chainweb Node](#configuring-running-and-monitoring-the-health-of-a-chainweb-node)
- [Mining for a Chainweb Network](#mine-for-a-chainweb-network)
- [Chainweb Design](#chainweb-design)
  - [Component Structure Details](#component-structure)
  - [Architecture Overview](#architecture-overview)

## Docs

The Kadena Docs site, which can be found [here](https://kadena-io.github.io/kadena-docs/) serves as a source of information about Kadena. You can find information about how to interact with the public chain, including how to get keys, view network activity, explore blocks, etc. [here](https://kadena-io.github.io/kadena-docs/Public-Chain-Docs/).

If you have additions or comments, please submit a pull request or raise an issue - the GitHub project can be found [here](https://github.com/kadena-io/kadena-docs)


## Installing Chainweb

### Linux Users

The binaries can be found [here](https://github.com/kadena-io/chainweb-node/releases).

#### Apt-based distributions

If you are on Ubuntu, Debian, CentOS or any other Apt-based distribution, you
will need to install rocksdb with the following command:

```bash
sudo apt-get update
```

```bash
sudo apt-get install -y librocksdb-dev zlib1g-dev libtinfo-dev libsqlite3-dev libz3-dev z3
```

If this is not available, then please view the [Rocksdb](https://rocksdb.org/)
site for alternative modes of installation.

#### Other distributions

For all other distributions not using Apt (RHEL, Gentoo, Arch, etc), please
consult your distro's repositories for `librocksdb5.8`, `tinfo`, `zlib`, `z3`
and install with its preferred package manager, or follow the alternative modes
of installation described in [Rocksdb](https://rocksdb.org/).

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

## Bootstrap Nodes

### Testnet Nodes

- us1.testnet.chainweb.com
- us2.testnet.chainweb.com
- eu1.testnet.chainweb.com
- eu2.testnet.chainweb.com
- ap1.testnet.chainweb.com
- ap2.testnet.chainweb.com

### Mainnet Nodes

All bootstrap nodes are running on port 443.

- us-e1.chainweb.com
- us-e2.chainweb.com
- us-e3.chainweb.com
- us-w1.chainweb.com
- us-w2.chainweb.com
- us-w3.chainweb.com
- jp1.chainweb.com
- jp2.chainweb.com
- jp3.chainweb.com
- fr1.chainweb.com
- fr2.chainweb.com
- fr3.chainweb.com

## Configuring, running, and monitoring the health of a Chainweb Node

**This section assumes you've installed the `chainweb-node` binary** somewhere
sensible, or otherwise have a simple way to refer to it.

To configure your node, please use our [minimal node
configuration](./minimal-config.yaml). You need to update only one section,
`hostaddress`:

```yaml
hostaddress:
  hostname: your-public-ip-or-domain
  port: 443
```

**Note:** You will have to perform Port Forwarding if your machine is behind a
router.

Then, to run your node:

```bash
chainweb-node --config-file=minimal-config.yaml
```

### Monitoring the health of a Chainweb Node


The following outlines how you can check that your `chainweb-node` is healthy

`chainweb-node` should be running from the public IP address and a port that is open to the other chainweb nodes. 

If you're behind a NAT, it is **VERY IMPORTANT** that your network allows external nodes to connect to the node you are running. If you provide us with your ip address and port number in our [Discord mining channel](https://discord.io/kadena), we can verify whether your node is reachable to the rest of the network.

When running the chainweb-node binary, you can indicate your hostname and port number directly on the config-file, or you can set it via command line flags like such:
```
$ chainweb-node --config-file <path-to-config-file> --hostname <public-ip> --port <port> --log-level <desired-log-level>
```

Once you're node is running, go through the following checks to verify that you have a healthy node:
* run the command in your terminal:  
```
$ curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/cut"
```
* navigate to this website on your browser: [https://yourPublicIp:port/chainweb/0.0/mainnet01/cut](https://yourPublicIp:port/chainweb/0.0/mainnet01/cut)
* check logs for whether services are started
* check if the node is receiving cuts
* look for errors in the logs
* look for warnings in the logs

Usually, when a node is receiving and publishing cuts (i.e. block heights at every chain), it's working correctly. 

The `/cut` endpoint will return the latest cut that your node has. It's possible that your node is falling behind, so make sure to compare its cut height with the cut heights of the bootstrap nodes. It's also possible that you are mining to a node that is catching up to the rest of the network. Before you start mining to a node, you SHOULD verify that this node has the most up-to-date cut. 

You can get the cut height of any node by running the following:
```
$ https://<bootstrap-node-url>/chainweb/0.0/mainnet01/cut | python -m json.tool | jq '.height'
```

### Miscellaneous
To find your public ip:
```
$ dig +short myip.opendns.com @resolver1.opendns.com
$ dig TXT +short o-o.myaddr.l.google.com @ns1.google.com
```

## Mine for a Chainweb Network

Detailed mining instructions can be found in our [Mining Guide](https://github.com/kadena-io/chainweb-miner/blob/master/README.org).

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
