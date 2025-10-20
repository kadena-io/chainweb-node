<div align="center">
     <img src="./docs/Kadena_logo-black.png" width="450" height="243" alt="Kadena" title="Kadena">
</div>

<p>&nbsp;</p>

# Kadena Chainweb Protocol for a Scalable Blockchain

The Kadena public blockchain network is a fast, secure, and scalable blockchain platform built using the **Chainweb** consensus protocol. 
Chainweb is a braided, parallelized Proof-of-Work consensus mechanism that improves throughput and scalability for executing transactions on the blockchain while maintaining the security, decentralization, and data integrity found in Bitcoin.

For technical information about the protocol, read the following [whitepapers](https://www.kadena.io/whitepapers):
- [Chainweb: A Proof-of-Work Parallel-Chain Architecture for Massive Throughput](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_029c9991469e4565a7c334dd716345f4.pdf)
- [Agent-based Simulations of Blockchain Protocols illustrated via Kadena's *Chainweb*](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_3b2d0c58179d4edd9df6df4d55d61dda.pdf)

For information about Kadena as the blockchain for business, the Kadena ecosystem, or the Kadena leadership team, see the [Kadena website](https://kadena.io).

## Documentation and key resources

You can find guides, tutorials, and technical reference information for Chainweb and the Pact smart contract language in [Kadena Developer Docs](https://docs.kadena.io). 

For information about getting keys and setting up a wallet, see the Discord [wallet](https://discord.com/channels/502858632178958377/631965054778212362) channel, the [Kadena wallets](https://discord.com/channels/502858632178958377/1217831353983303701) pinned message, or the following resources:

- [@kadena/kadena-cli](https://www.npmjs.com/package/@kadena/kadena-cli)
- [Chainweaver wallet](https://chainweaver.kadena.network/)

To create and fund accounts for testing purposes or explore contracts that are deployed on the Kadena test or production networks, see the [Kadena Developer Tools](https://tools.kadena.io).

To view network activity and explore blocks, see [Kadscan](https://kadscan.io/), [Kadena Block Explorer](https://explorer.kadena.io/mainnet) or [Kadena GraphQL](https://graph.kadena.network/graphql).

If you would like to contribute to the project, you can submit a [pull request](https://github.com/kadena-io/chainweb-node/pulls) or raise an [issue](https://github.com/kadena-io/chainweb-node/issues) in the GitHub repository.

## Minimum system requirements

- CPU: Minimum of eight (8) CPU cores.
- RAM: Minimum of sixteen (16) GB of RAM.
- Storage: Minimum 2 TB using a solid state drive (SSD) or fast hard disk drive (HDD).
- Network: Publicly-accessible IP address and a port that allows incoming traffic from the internet.
- Operating system: Linux AMD64 architecture.

If you plan to use the node for mining, to handle RPC requests, or to query historical blockchain data, you should consider upgrading the number of CPU cores, available RAM, and storage above the minimum system requirements.

## Installation options

There are several ways you can deploy Chainweb node in a physical or virtual environment. 
For example, you can choose to set up a node by using one of the following installation options:

- Download and install Chainweb release binaries directly on a physical server or on infrastructure from a cloud services provider.
- Run a Chainweb node image in a container.
- Build Chainweb binaries yourself from the source code.

## Run the node in a container

You can run a Chainweb node inside of a container by downloading an image with platform-specific dependencies already installed.
The `chainweb-node` image is available at `ghcr.io/kadena-io/chainweb-node/ubuntu`.

To get the image for the most recent release, run:

```shell
docker pull ghcr.io/kadena-io/chainweb-node/ubuntu:latest
```

To view command-line options for the node, run:

```shell
docker run --publish 1789:1789 --publish 80:80 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:latest --help
```

To display the default configuration settings, run:

```shell
docker run --publish 1789:1789 --publish 80:80 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:latest --print-config
```

To save the default configuration file settings to a file, run:

```shell
docker run --publish 1789:1789 --publish 80:80 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:latest --print-config > default-config.yaml
```

The command you use to start the node from the image in the container depends on whether you want to run the node with output in the terminal or as a detached process in the background and on the command-line or configuration file settings that you want to modify.

To start the node in the background as a detached process, run:

```shell
docker run --detach --publish 1789:1789 --publish 1848:1848 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:latest
```

If you want to modify configuration settings for the node running in a container, you should specify the appropriate settings as command-line options.
For example:

```shell
docker run --publish 1789:1789 --publish 1848:1848 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:latest --enable-backup-api --backup-directory /tmp/my-backups
```

To interact with the node files and directories in the container:

1. Run `docker ps` to get the container identifier:
   
   ```shell
   docker ps
   CONTAINER ID   IMAGE  ...
   0584cc2cd54e   ghcr.io/kadena-io/chainweb-node/ubuntu:latest   "/chainweb/chainweb-…"  ...
   ```

1. Run `docker exec` to open an interactive terminal shell in the container:
   
   ```shell
   docker exec --interactive --tty 0584cc2cd54e /bin/bash
   ```

1. Run shell commands to add files or directories to the image:
   
   ```shell
   root@0584cc2cd54e:/chainweb# ./chainweb-node --print-config > image-config.yaml
   root@0584cc2cd54e:/chainweb# mkdir my-backups
   root@0584cc2cd54e:/chainweb# touch my-log-file
   ```

Note that the container doesn't include a text editor for modifying the configuration file. 
You can add an editor to the container.
However, you must persistent the container data for the modified configuration file to be available when you restart the container.

## Install node binaries

To install node binaries directly on a physical or virtual server:

1. Update the system with the latest software:
    
    ```shell
    sudo apt update && sudo apt upgrade
    ```

2. Install the required packages:
   
   ```shell
   apt-get install ca-certificates libmpfr6 libgmp10 libssl1.1 libsnappy1v5 zlib1g liblz4-1 libbz2-1.0 libgflags2.2 zstd
   ```

3. Download the latest compressed archive from [Releases](https://github.com/kadena-io/chainweb-node/releases).

4. Unzip and extract the archive into a directory where you have permission to run programs.

7. Verify the node is ready to use and review command-line configuration options by running the following command:
    
   ```shell
   ./chainweb-node --help
   ```
    
8. Save the default configuration settings in a configuration file for the node by running the following command:
    
   ```shell
   ./chainweb-node --print-config > default-config.yaml
   ```
    
9. Start the node using the default configuration settings and an empty database by running the following command:
    
   ```bash
   ./chainweb-node
   ```
    
   By default, the node tries to connect to bootstrap nodes and start synchronizing its database with the other nodes in the network. 
   During the synchronization process, the node replays all of the transactions that have been successfully executed until the node catches up to the current state of the other nodes.
   For a node connecting to the Kadena main public network, this process can take a significant period of time.

## Build from source

*IMPORTANT NOTE: We recommend the use of officially released chainweb-node
binaries or docker images, which can be found in the
[release section of this
repository](https://github.com/kadena-io/chainweb-node/releases).
If you decide to build your own binaries, please make sure to only use
officially released and tagged versions of the code. Those versions are
extensively tested to ensure that they are compatible with all other nodes in
the chainweb network. It is generally not safe to run arbitrary builds of the
master branch in the Kadena mainnet.*

Chainweb is a [Haskell](https://www.haskell.org/) project. After cloning the
code with git from this GitHub repository the chainweb-node application can be
built as follows.

### Building with Cabal

In order to build with `cabal` you have to install `ghc-8.10.7` (Haskell compiler)
and `cabal >= 3.4` (Haskell build-tool)

*   [Linux / Mac](https://www.haskell.org/ghcup/)

You need to install the development versions of the following libraries:
`gflags`, `snappy`, `zlib`, `lz4`, `bz2`, `zstd`.

On apt based distribution these can be installed as follows:

```
apt-get install ca-certificates libssl-dev libmpfr-dev libgmp-dev libsnappy-dev zlib1g-dev liblz4-dev libbz2-dev libgflags-dev libzstd-dev
```

To build a `chainweb-node` binary:

```bash
# Only necessary if you haven't done this recently.
cabal update

# Build the project.
#
# After this, a runnable binary can be found by running `cabal list-bin chainweb-node`.
cabal build
```

### Building with Nix

Another way to build and run chainweb is to use the Nix package manager which
has binary caching capabilities that allow you to download pre-built binaries
for everything needed by Chainweb. For detailed instructions see [our
    wiki](https://github.com/kadena-io/pact/wiki/Building-Kadena-Projects).

When the build is finished, you can run chainweb with the following command:

```bash
./result/ghc/chainweb/bin/chainweb-node
```

## Bootstrap Nodes

Bootstrap nodes are used by chainweb-nodes on startup in order to discover other
nodes in the network. At least one of the bootstrap nodes must be trusted.

Chainweb node operators can configure additional bootstrap nodes by using the
`--known-peer-info` command line option or in a configuration file. It is also
possible to ignore the builtin bootstrap nodes by using the
`--enable-ignore-bootstrap-nodes` option or the respective configuration file
setting.

Bootstrap nodes must have public DNS names and a corresponding TLS certificate
that is issued by a widely accepted CA (a minimum requirement is acceptance by
the OpenSSL library).

Operators of bootstrap nodes are expected to guarantee long-term availability of
the nodes. The list of builtin bootstrap nodes should be kept up-to-date and
concise for each chainweb-node release.

If you like to have your node included as a bootstrap node please make a pull
request that adds your node to [P2P.BootstrapNodes module](src/P2P/BootstrapNodes.hs).

### Current Testnet Bootstrap Nodes

- us1.testnet.chainweb.com
- us2.testnet.chainweb.com
- eu1.testnet.chainweb.com
- eu2.testnet.chainweb.com
- ap1.testnet.chainweb.com
- ap2.testnet.chainweb.com

### Current Mainnet Bootstrap Nodes

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
sensible, or otherwise have a simple way to refer to it. For running
`chainweb-node` via docker, please see the instruction above in this document or
visit our [docker repository](https://hub.docker.com/r/kadena/chainweb-node).

**Note:** Your node needs to be reachable from the public internet. You will
have to perform Port Forwarding if your machine is behind a router (by default
port 1789 is used by the node).

**NOTE**: When you start chainweb-node for the first time it creates a new
empty database and starts to synchronize and catch up with other nodes in the
Kadena network. This process takes a long time -- several days. It is much
faster (depending on hardware one to a few hours) to just synchronize the chain
database or get a snapshot of it and only rebuild the pact databases from the
chain-database. Please, consult the documentation of the docker images for
chainweb-node about details on how to obtain an initial chain database.

Run your node:

```bash
chainweb-node
```

The node will communicate with other nodes in a P2P network. By default it uses
port 1789 for the P2P communication.

Node services are exposed via the service API, by default on port 1848. The
service API includes `/info`, `/health-check`, Pact endpoints, the mining API
endpoints, GET endpoints for on-chain data (headers, payloads, cuts), and an
HTTP event stream of block header updates. Some of these are disabled by default
(e.g. mining API, and header updates).

While the P2P endpoint must be directly available from the public internet, it
is highly recommended to expose the service API only on a private network. When
service API endpoints are made available publicly it is recommended to use a
reverse proxy setup things like rate limiting, authentication, and CORS.

### Configuration

No particular configuration is needed for running Chainweb node on the Kadena
mainnet.

Use `chainweb-node --help` to show a help message that includes a brief
description of all available command line options.

A complete configuration file with the default settings can be created with

```sh
chainweb-node --print-config > config.yaml
```

This file can then be edited in order to change configuration values.

The command `chainweb-node --help` also provides descriptions of these
configuration values.

Given a configuration file or a set of command line options it is possible to
print out only those configuration values that are different from their
respective default:

```
chainweb-node --config-file=config.yaml --some-command-line-options --print-config-as=minimal
```


### Monitoring the health of a Chainweb Node

The following outlines how you can check that your `chainweb-node` is healthy

`chainweb-node` should be running from the public IP address and a port that is open to the other Chainweb nodes.

If you're behind a NAT, it is **VERY IMPORTANT** that your network allows
external nodes to connect to the node you are running.

```
$ chainweb-node --log-level <desired-log-level>
```

For production scenarios we recommend that you use log-level `warn` or `error`.
For troubleshooting or improved monitoring you can also use `info`.

Once your node is running, go through the following checks to verify that you have a healthy node:
*   run the command in your terminal:
    ```
    $ curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/cut"
    ```
*   navigate to this website on your browser: [https://yourPublicIp:port/chainweb/0.0/mainnet01/cut](https://yourPublicIp:port/chainweb/0.0/mainnet01/cut)
*   check logs for whether services are started
*   check if the node is receiving cuts
*   look for errors in the logs
*   look for warnings in the logs

Usually, when a node is receiving and publishing cuts (i.e. block heights at every chain), it's working correctly.

The `/cut` endpoint will return the latest cut that your node has. It's possible that your node is falling behind, so make sure to compare its cut height with the cut heights of the bootstrap nodes. It's also possible that you are mining to a node that is catching up to the rest of the network. Before you start mining to a node, you SHOULD verify that this node has the most up-to-date cut.

You can get the cut height of any node by running the following:

```
$ curl -sk https://<bootstrap-node-url>/chainweb/0.0/mainnet01/cut | jq '.height'
```

## Mine for a Chainweb Network

Successful mining on mainnet requires specialized hardware (ASIC). The setup for solo mining involves running a chainweb-node with a configuration that enables mining and a [chainweb-mining-client](https://github.com/kadena-io/chainweb-mining-client/) that connects to the mining API of a chainweb-node and provides a Stratum API for the mining hardware (ASIC).

Detailed instructions for setting up all the infrastructure needed to start
mining using `docker compose` can be found in the documentation of [docker-compose-chainweb-node/mining-node](https://github.com/kadena-io/docker-compose-chainweb-node/tree/main/mining-node).

For example, to set up a chainweb node for mining, see [this](https://github.com/kadena-io/docker-compose-chainweb-node/blob/main/mining-node/docker-compose.yaml#L126) section of the docker-compose file.

Detailed mining client instructions can be found in the documentation of
[chainweb-mining-client](https://github.com/kadena-io/chainweb-mining-client/)

## Chainweb Design

### Component Structure

The Chainweb package contains the following buildable components:

*   `chainweb` library: It provides the implementation for the different
    components of a chainweb-node.

*   `chainweb-node`: An application that runs a Chainweb node. It maintains copies
    of a number of chains from a given Chainweb instance. It provides interfaces
    (command-line and RPC) for directly interacting with the Chainweb or for
    implementing applications such as miners and transaction management tools.

*   `chainweb-tests`: A test suite for the Chainweb library and chainweb-node.

*   `cwtool`: A collection of tools that are helpful for maintaining, testing,
    and debugging Chainweb.

*   `bench`: a collection of benchmarks

### Architecture Overview

![Architecture Overview](docs/Overview.png)
