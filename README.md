<div align="center">
     <img src="static/img/kadena-github-view.png">
</div>

<p>&nbsp;</p>

# Kadena Chainweb Protocol for a Scalable Blockchain

The Kadena public blockchain network is a fast, secure, and scalable blockchain platform built using the **Chainweb** consensus protocol. 
Chainweb is a braided, parallelized Proof-of-Work consensus mechanism that improves throughput and scalability for executing transactions on the blockchain while maintaining the security, decentralization, and data integrity found in Bitcoin.

For technical information about the protocol, read the following [whitepapers](https://www.kadena.io/whitepapers):

- [Chainweb: A Proof-of-Work Parallel-Chain Architecture for Massive Throughput](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_029c9991469e4565a7c334dd716345f4.pdf)
- [Agent-based Simulations of Blockchain Protocols illustrated via Kadena's *Chainweb*](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_3b2d0c58179d4edd9df6df4d55d61dda.pdf)

For information about Kadena as the blockchain for business, the Kadena ecosystem, or the Kadena leadership team, see the [Kadena website](https://kadena.io).

## Table of contents

- [Kadena Chainweb Protocol for a Scalable Blockchain](#kadena-chainweb-protocol-for-a-scalable-blockchain)
  - [Table of contents](#table-of-contents)
  - [Documentation and key resources](#documentation-and-key-resources)
  - [Minimum system requirements](#minimum-system-requirements)
  - [Installing Chainweb](#installing-chainweb)
    - [Install release binaries](#install-release-binaries)
    - [Run in a Docker container](#run-in-a-docker-container)
    - [Build from source code](#build-from-source-code)
      - [Build using the Haskell toolchain](#build-using-the-haskell-toolchain)
      - [Build using Nix](#build-using-nix)
  - [Starting and stopping a Chainweb node](#starting-and-stopping-a-chainweb-node)
    - [Synchronizing the node database](#synchronizing-the-node-database)
    - [Default ports and bootstrap nodes](#default-ports-and-bootstrap-nodes)
      - [Current testnet bootstrap nodes](#current-testnet-bootstrap-nodes)
      - [Current mainnet bootstrap nodes](#current-mainnet-bootstrap-nodes)
      - [Node service API](#node-service-api)
    - [Becoming a bootstrap node](#becoming-a-bootstrap-node)
  - [Configure features and settings](#configure-features-and-settings)
  - [Monitor node status and operations](#monitor-node-status-and-operations)
    - [Check the public IP address](#check-the-public-ip-address)
    - [Set the logging level](#set-the-logging-level)
    - [Check cut height](#check-cut-height)
  - [Enable mining for a node](#enable-mining-for-a-node)
  - [Chainweb component structure](#chainweb-component-structure)
    - [Architecture overview](#architecture-overview)

## Documentation and key resources

You can find guides, tutorials, and technical reference information for Chainweb and the Pact smart contract language on the [Kadena Developer Docs](https://docs.kadena.io). 

For information about getting keys and setting up a wallet, see the Discord [wallet](https://discord.com/channels/502858632178958377/631965054778212362) channel, the [Kadena wallets](https://discord.com/channels/502858632178958377/1217831353983303701) pinned message, or the following resources:

- [@kadena/kadena-cli](https://www.npmjs.com/package/@kadena/kadena-cli)
- [Chainweaver wallet](https://chainweaver.kadena.network/)

To create and fund accounts for testing purposes or explore contracts that are deployed on the Kadena test or production networks, see the [Kadena Developer Tools](https://tools.kadena.io).

To view network activity and explore blocks, see [Kadscan](https://kadscan.io/), [Kadena Block Explorer](https://explorer.kadena.io/mainnet) or [Kadena GraphQL](https://graph.kadena.network/graphql?query=%23%0A%23+Welcome+to+Kadena+GraphQL%0A%23%0A%23+Kadena+GraphQL+is+an+in-browser+tool+for+writing%2C+validating%2C+and%0A%23+testing+GraphQL+queries.%0A%23%0A%23+Type+queries+into+this+side+of+the+screen%2C+and+you+will+see+intelligent%0A%23+typeaheads+aware+of+the+current+GraphQL+type+schema+and+live+syntax+and%0A%23+validation+errors+highlighted+within+the+text.%0A%23%0A%23+GraphQL+queries+typically+start+with+a+%22%7B%22+character.+Lines+that+start%0A%23+with+a+%23+are+ignored.%0A%23%0A%23+An+example+GraphQL+query+might+look+like%3A%0A%23%0A%23+++++%7B%0A%23+++++++field%28arg%3A+%22value%22%29+%7B%0A%23+++++++++subField%0A%23+++++++%7D%0A%23+++++%7D%0A%23%0A%23+Keyboard+shortcuts%3A%0A%23%0A%23++Prettify+Query%3A++Shift-Ctrl-P+%28or+press+the+prettify+button+above%29%0A%23%0A%23+++++Merge+Query%3A++Shift-Ctrl-M+%28or+press+the+merge+button+above%29%0A%23%0A%23+++++++Run+Query%3A++Ctrl-Enter+%28or+press+the+play+button+above%29%0A%23%0A%23+++Auto+Complete%3A++Ctrl-Space+%28or+just+start+typing%29%0A%23%0A).

If you would like to contribute to the project, you can submit a [pull request](https://github.com/kadena-io/chainweb-node/pulls) or raise an [issue](https://github.com/kadena-io/chainweb-node/issues) in the GitHub repository.

## Minimum system requirements

Before installing Chainweb, you should verify that your computer meets the following minimum system requirements for running a node:

- CPU: Minimum of eight (8) CPU cores.
- RAM: Minimum of sixteen (16) GB of RAM.
- Storage: Minimum 2 TB using a solid state drive (SSD) or fast hard disk drive (HDD).
- Network: Publicly-accessible IP address and a port that allows incoming traffic from the internet.
- Operating system: Linux AMD64 architecture.

If you plan to use the node for mining, to handle RPC requests, or to query historical blockchain data, you should consider upgrading the number of CPU cores, available RAM, and storage above the minimum system requirements.

## Installing Chainweb

There are several ways you can set up a Chainweb node in a physical or virtual environment. 
For example, you can choose to install by using one of the following installation options:

- Download and install Chainweb release binaries directly on a physical server or on infrastructure from a cloud services provider.
- Run a Chainweb node image in a Docker container. 
- Build Chainweb binaries yourself from the source code.

### Install release binaries

You can download a compressed archive file with `chainweb-node` release binaries for Ubuntu Linux directly from the [Releases](https://github.com/kadena-io/chainweb-node/releases) page in the [chainweb-node](https://github.com/kadena-io/chainweb-node/) repository.
If you have a supported version of Ubuntu Linux or an equivalent operating system on a physical or virtual machine, downloading the binary is the most straightforward installation path.

To install from a compressed release archive:

1. Open a terminal shell on the physical or virtual host with the Ubuntu Linux operating system.

2. Update the system with the latest software by running the following command:
   
   ```bash
   sudo apt update && sudo apt upgrade
   ```

3. Install the required packages by running the following command:
   
   ```bash
   sudo apt-get install ca-certificates libgmp10 libssl3 libsnappy1v5 zlib1g liblz4-1 libbz2-1.0 libgflags2.2 zstd
   ```

4. Copy the URL for the latest release from the Release **Assets** by hovering over the compressed archive file name, right-click, then select **Copy Link Address**.

5. Download the Chainweb release from the URL by running a command similar to the following:
   
   ```shell
   wget https://github.com/<path>/chainweb-<release>.tar.gz
   ```
   
   Note that the archive file includes the `chainweb-node` version, compiler version, Ubuntu version, and a commit hash identifier.
   For example, you would run the following command to download the `chainweb-node` archive file for Ubuntu 22.04 and `chainweb-node` version 2.30:
   
   ```shell
   wget https://github.com/kadena-io/chainweb-node/releases/download/2.30/chainweb-2.30.ghc-9.8.2.ubuntu-22.04.e0acda0.tar.gz
   ```

6. Unzip and extract the compressed archive by running a command similar to the following:
   
   ```shell
   tar -xvzf chainweb-2.30.ghc-9.6.5.ubuntu-22.04.89b0ac3.tar.gz
   ```

7. Verify the binary is ready to use and review command-line configuration options by running the following command:
   
   ```shell
   ./chainweb-node --help
   ```

   You should see usage information about the configuration settings you can specify as command-line options similar to the following truncated output:

   ```shell
   Usage: chainweb-node [--info] [--long-info] [-v|--version] [--license] 
                        [-?|-h|--help] 
                        [--print-config-as full|minimal|diff | --print-config] 
                        [--config-file FILE] 
   ```

   From the usage information, you can see that there are a large number of configuration options that you can use to control the operation and behavior of the Chainweb node. 
   Before you start the node, you should review the configuration options and the default values to determine whether you want to make any changes to the configuration of the node.

8. Save the default configuration settings in a configuration file for the node by running the following command:
   
   ```shell
   ./chainweb-node --print-config > default-config.yaml
   ```

### Run in a Docker container

If you have Docker installed, you can run a Chainweb node by pulling the latest [kadena/chainweb-node](https://hub.docker.com/r/kadena/chainweb-node) image hosted on the [Docker Hub](https://hub.docker.com) or [chainweb-node/ubuntu](https://github.com/kadena-io/chainweb-node/pkgs/container/chainweb-node%2Fubuntu) image located in the `chainweb-node` repository.

The [kadena/chainweb-node](https://hub.docker.com/r/kadena/chainweb-node) image includes a script to initialize the Chainweb database from a compressed backup file. 
Initializing the Chainweb database from a database backup file is an optional step and is less secure than starting the node with a genesis state.
If you start a node without initializing the database, the node will replay all of the transactions that have been executed until its state is synchronized with the other nodes in the network.
The synchronization process can take a significant amount of time and system resources, but it's the most secure way to bring a node into consensus with its peers.

If you want to run the node in a Docker container but don't have Docker installed, see the instructions provided in [Install Docker Engine](https://docs.docker.com/engine/install/) to download and install Docker for your operating system.
After you have Docker installed, use the following instructions to pull the Docker image and run `chainweb-node` in a Docker container.

To run a Chainweb node in a Docker container:

1. (Optional) Get a database backup file from a trusted node or other source.
   
   ```shell
   export DB_SNAPSHOT_URL="https://chainweb-data.kda.kaddex.xyz/chainweb-node-data-2025-10-14T20_00_01_Z0200.tar.gz"
   ```

2. (Optional) Initialize the database as a persistent volume for the Docker container.
   
   ```shell
   docker run --tty --interactive --rm \
     --mount type=volume,source=chainweb-data,target=/data \
     --env DBURL=$DB_SNAPSHOT_URL \
     kadena/chainweb-node /chainweb/initialize-db.sh
   ```
   
   Alternatively, you can initialize the database inside of a Docker container, then create a new `chainweb-node-with-db` image from it.

   ```shell
   docker run --tty --interactive --name initialize-chainweb-db \
     --env DBURL=$DB_SNAPSHOT_URL kadena/chainweb-node /chainweb/initialize-db.sh

   docker commit `docker ps -a -f 'name=initialize-chainweb-db' -q` chainweb-node-with-db
   
   docker rm initialize-chainweb-db
   ```

3. Start the Chainweb node in a detached container using the persistent database volume.
   
   ```shell
   docker run --detach --publish 1789:1789 --publish 1848:1848 --volume chainweb-data:/data kadena/chainweb-node
   ```
   
   If you initialized the database inside of a Docker container, you can use the `chainweb-node-with-db` image to run the node:

   ```shell
   docker run \
     --detach \
     --publish 1848:1848 \
     --publish 1789:1789 \
     --name chainweb-node \
     chainweb-node-with-db \
     /chainweb/run-chainweb-node.sh
   ```

4. Check the status of the Chainweb node using the `docker ps` command.
   
   The command should display output similar to the following:
   
   ```shell                                                                           NAMES
   539c7cb27b8d   kadena/chainweb-node   "/bin/sh -c ./run-ch…"   4 seconds ago   Up 3 seconds (health: starting)   0.0.0.0:1789->1789/tcp, :::1789->1789/tcp, 0.0.0.0:1848->1848/tcp, :::1848->1848/tcp   chainweb-node
   ```

5. Connect to the node using API endpoints to verify that the node is running and the cut height is increasing.
   
   For example, if the IP address for the node is 52.207.207.113, you can run a command similar to the following:
   
   ```shell
   curl http://52.207.207.113:1848/health-check
   Health check OK.
   ```
   
   To get general information about the node, you can run a command similar to the following:

   ```shell
   curl http://52.207.207.113:1848/info
   {"nodeApiVersion":"0.0","nodeBlockDelay":30000000,"nodeChains":["9","0","6","4","1","2","3","5","8","7"],"nodeGenesisHeights":[["16",852054],["9",0],["0",0],["6",0],["4",0],["1",0],["2",0],["18",852054],["14",852054],["10",852054],["3",0],["12",852054],["5",0],["17",852054],["11",852054],["8",0],["7",0],["15",852054],["13",852054],["19",852054]],"nodeGraphHistory":[[852054,[[16,[1,17,15]],[9,[6,4,7]],[0,[10,5,15]],[6,[9,1,8]],[4,[9,14,19]],[1,[16,6,11]],[2,[12,17,7]],[18,[3,17,19]],[14,[4,15,13]],[10,[0,11,19]],[3,[18,8,13]],[12,[2,11,13]],[5,[0,8,7]],[17,[16,2,18]],[11,[1,10,12]],[8,[6,3,5]],[7,[9,2,5]],[15,[16,0,14]],[13,[14,3,12]],[19,[4,18,10]]]],[0,[[9,[4,5,8]],[0,[2,3,5]],[6,[1,5,7]],[4,[9,1,2]],[1,[6,4,3]],[2,[0,4,7]],[3,[0,1,8]],[5,[9,0,6]],[8,[9,3,7]],[7,[6,2,8]]]]],"nodeHistoricalChains":[[852054,[[16,[1,17,15]],[9,[6,4,7]],[0,[10,5,15]],[6,[9,1,8]],[4,[9,14,19]],[1,[16,6,11]],[2,[12,17,7]],[18,[3,17,19]],[14,[4,15,13]],[10,[0,11,19]],[3,[18,8,13]],[12,[2,11,13]],[5,[0,8,7]],[17,[16,2,18]],[11,[1,10,12]],[8,[6,3,5]],[7,[9,2,5]],[15,[16,0,14]],[13,[14,3,12]],[19,[4,18,10]]]],[0,[[9,[4,5,8]],[0,[2,3,5]],[6,[1,5,7]],[4,[9,1,2]],[1,[6,4,3]],[2,[0,4,7]],[3,[0,1,8]],[5,[9,0,6]],[8,[9,3,7]],[7,[6,2,8]]]]],"nodeLatestBehaviorHeight":6027617,"nodeNumberOfChains":10,"nodePackageVersion":"2.30","nodeServiceDate":"2025-10-15T00:00:00Z","nodeVersion":"mainnet01"}
   ```
   
   To see the current cut height, you can submit a GET request similar to the following using Postman:
   
   ```postman
   GET https://52.207.207.113:1789/chainweb/0.0/mainnet01/cut
   ```
  
   You should see the block height and hashes for multiple chains similar to the following:

   ```json
   {
       "hashes": {
           "9": {
               "height": 60,
               "hash": "fwLGKjAX9kL0HVDUvxZx0r47dH5EqoMXPScgU69NG5o"
           },
           "0": {
               "height": 60,
               "hash": "UQQkrkIsdml6wMz80pl3ur_ZPAaXlPtCxvXbqUY02pw"
           },
           "6": {
               "height": 60,
               "hash": "9WWpuzM2mvF9h-ujQSA8Br-6dult3ylhfMGfdn9nDXM"
           },
           "4": {
               "height": 60,
               "hash": "r4ffJebqiMY90ShDvQL2QpTznelduUI-CnXNcdsF8_k"
           },
           "1": {
               "height": 60,
               "hash": "9mxqIdKKCe1nS--cpvyALh3oDRUsvExGf8-DdwXQFIg"
           },
           "2": {
               "height": 60,
               "hash": "L5J_8GzEhoUAYgzUtH4Uit6x7GNJDA742PH0W4Sonic"
           },
           "3": {
               "height": 60,
               "hash": "Gu7tNbaOQsoAcJ5adaM1MnH_5XtS5hHoCz_uqaabID4"
           },
           "5": {
               "height": 60,
               "hash": "xo1sBuWSK4BlYVo9B9GlAUOvby8xb6jUeV7LGcU0jXk"
           },
           "8": {
               "height": 60,
               "hash": "POylOXr0OFNkZakfxvPyZXH-dhGQZD9TlWF7HXCIXKk"
           },
           "7": {
               "height": 60,
               "hash": "8B2NgIqdanXSsMonnJ565TkxyonVUK5b1FjvSDCVYMQ"
           }
       },
       "origin": null,
       "weight": "tKEBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
       "height": 600,
       "instance": "mainnet01",
       "id": "O4JwtvipeuxT0RRxCgmDoDuUiQpMA_iHXU71jnyE1Pc"
   }
   ```

6. Display configuration options interactively by running a command similar to the following;
    
   ```shell
   docker run --publish 1789:1789 --publish 80:80 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:2.30 --help
   ```

7. Save the default configuration settings to a file by running a command similar to the following;
    
   ```shell
   docker run --publish 1789:1789 --publish 80:80 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:2.30 --print-config > default-config.yaml
   ```

   For more examples of running a Chainweb node using `docker compose` for mining, see the 
[docker-compose-chainweb-node repository](https://github.com/kadena-io/docker-compose-chainweb-node).

### Build from source code

In most cases, you should only run Chainweb nodes using officially released `chainweb-node` binaries or from the binary packaged in officially released Docker images.
However, if you choose to build `chainweb-node` from the source, you should first ensure that you have an officially released and tagged version of the source code. 
Tagged versions of the source code are tested extensively to ensure that they are compatible with all nodes in the Chainweb network.

You should never build `chainweb-node` from the `master` branch if you plan to run the node as part of any Kadena public network.

Chainweb is a [Haskell](https://www.haskell.org/) project. 
You can build from the source code using the native Haskell toolchain or, alternatively, using the Nix package manager.

#### Build using the Haskell toolchain

Before you can build with the native Haskell toolchain, you must have the following native Haskell tools installed:
   
   - [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/), `ghc-9.6.5`, or later.
   - [Haskell build tool CABAL](https://www.haskell.org/cabal/), `cabal`, version 3.4, or later.

To build with the native Haskell toolchain:

1. Open a terminal shell on your computer.
2. Download [ghcup](https://www.haskell.org/ghcup/) and start the installation script by running the following command:
   
   ```shell
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```
   
   Follow the prompts displayed to configure the toolchain.
  

3. Install the development versions of the required libraries using either `apt` on Linux or `brew` on macOS.: 

   If the host uses `apt`, install the libraries by running the following command:
   
   ```bash
   apt-get install ca-certificates libssl-dev libgmp-dev libsnappy-dev zlib1g-dev liblz4-dev libbz2-dev libgflags-dev libzstd-dev
   ```

   On macOS, install the libraries by running the following command:
   
   ```bash
   brew install ca-certificates libgmp-dev libsnappy-dev zlib1g-dev liblz4-dev libbz2-dev libgflags-dev libzstd-dev
   ```

4. Ensure the `cabal` build tool is up-to-date by running the following command:
   
   ```bash
   cabal update
   ```

5. Build `chainweb-node` by running the following command:
   
   ```bash
   cabal build
   ```

6. Locate the `chainweb-node` executable binary by running the following command:
   
   ```bash
   cabal list-bin chainweb-node
   ```

7. Verify that `chainweb-node` is ready to use and review command-line configuration options by running the following command:
   
   ```bash
   ./chainweb-node --help
   ```

   You should see usage information about the configuration settings you can specify as command-line options similar to the following truncated output:

   ```bash
   Usage: chainweb-node [--info] [--long-info] [-v|--version] [--license] 
                        [-?|-h|--help] 
                        [--print-config-as full|minimal|diff | --print-config] 
                        [--config-file FILE] 
   ```

   Note that there are a large number of configuration options that you can use to control the operation and behavior of the Chainweb node. 
   Before you start the node, you should review the configuration options and the default values to determine whether you want to make any changes to the configuration of the node.
   For more information about this step, see [Review the default configuration](#review-the-default-configuration).

#### Build using Nix

The Nix package manager caches binary dependencies, so you can download pre-built binaries for for the libraries and packages that Chainweb requires.

To build with the Nix package manager:

1. Download and install [Nix](https://nixos.org/nix/) by clicking **Get Nix**, then follow the instructions.

2. Open the shell startup profile for the shell you use in a text editor.
   
   For example, if you are using the `bash` shell, open the `.bash_profile` file.
   If you are using the `zsh` shell, open the `.zsh_profile` file.

3. Add the following line to the startup profile:

   ```text
   . $HOME/.nix-profile/etc/profile.d/nix.sh
   ```

   If you don't want to edit the profile directly using a text editor or don't have a text editor installed, you can run a command similar to the following:
   
   ```bash
   echo ". $HOME/.nix-profile/etc/profile.d/nix.sh" >> ~/.bash_profile
   ```
   
   If you are using a different shell, change `.bash_profile` to the appropriate startup script for your shell.

4. Open the `/etc/nix/nix.conf` file in a text editor and add the following lines to the file:

   ```text
   substituters = https://nixcache.chainweb.com https://cache.nixos.org/
   trusted-public-keys = nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
   experimental-features = nix-command flakes
   ```

   If the `/etc/nix` directory doesn't exist, you should switch to the root user and create it, then add the `nix.conf` file to the directory.

5. Restart the `nix-daemon` process.
   
   Most installations of Nix are multi-user and require you to restart the nix daemon to make your `nix.conf` changes take effect. 
   
   You can check whether Nix is running in multi-user or single-user mode by running the following command:
   
   ```bash
   ps aux | grep nix-daemon
   ```
   
   If you see only the `grep` command, then you are using single-user mode and you don't have to do anything else. 
   If you see a `nix-daemon` process, then you are using multi-user mode and you need to restart the process
   
   On macOS, run the following commands:
   
   ```bash
   sudo launchctl stop org.nixos.nix-daemon
   sudo launchctl start org.nixos.nix-daemon
   ```
   
   On Linux, run the following command:
   
   ```bash
   sudo systemctl restart nix-daemon.service
   ```
   
6. Build `chainweb-node` by running a command similar to the following for a specific release `tag`, such as 2.31:

   ```bash
   nix build github:kadena-io/chainweb-node/<tag>
   ```
   
   After starting the build, you should see messages similar to the following:
   
   ```text
   copying path '/nix/store/8dyrf48cwvyqhvks5adxlk675qgm7pql-haskell-project-plan-to-nix-pkgs' from 'https://nixcache.chainweb.com'
   ...
   ```
   
   These messages indicate that the pre-built artifacts are being successfully downloaded from the cache. 
   
   When the build is finished, the directory with the `chainweb-node` source code contains a `result` subdirectory with a symbolic link to the Nix cache.

7. Verify that `chainweb-node` is ready to use and review command-line configuration options by running the following command:
   
   ```bash
   ./result/bin/chainweb-node --help
   ```

   You should see usage information about the configuration settings you can specify as command-line options similar to the following truncated output:

   ```bash
   Usage: chainweb-node [--info] [--long-info] [-v|--version] [--license] 
                        [-?|-h|--help] 
                        [--print-config-as full|minimal|diff | --print-config] 
                        [--config-file FILE] 
   ```

   From the usage information, you can see that there are a large number of configuration options that you can use to control the operation and behavior of the Chainweb node. 
   Before you start the node, you should review the configuration options and the default values to determine whether you want to make any changes to the configuration of the node.
   For information about this next step, see [Review the default configuration](#review-the-default-configuration).

## Starting and stopping a Chainweb node

As previously noted, a node must be reachable from the public internet to participate in peer-to-peer networking. 
Before you start the node, you should verify that either the default port 1789 or another designated port is open. 
If your node is behind a firewall or network router, verify that the network is configured with port forwarding to allow incoming connections on the port used for peer-to-peer communication.

To stop a running Chainweb node, press Control-c or sending a terminate signal to the process.
For example, run the `ps` program to view running processes, then `pkill` to terminate the process:

```shell
$ ps -a
    PID TTY          TIME CMD
2194388 pts/0    00:00:24 chainweb-node
2194527 pts/1    00:00:00 ps
$ pkill chainweb-node
```

### Synchronizing the node database

By default, the first time you start `chainweb-node`, the node creates a new **empty database** and starts to synchronize state with the other nodes in the Kadena network. 
During the synchronization process, the node being brought online replays all of the transactions that have been successfully executed since the network started first until the node catches up to the current state of the other nodes.
For a node connecting to the Kadena main public network, this process can take a significant period of time.

Optionally, you can start a Chainweb node with a backup snapshot of the chain database from another node to reduce the time required to synchronize the state.
It's important to note that initializing a Chainweb node with a database backup file that's been copied from another source is less secure than starting the node with an empty database.
Starting a node with an empty database takes significant time and system resources, but ensures that the node has the canonical view of blockchain history and is the most secure way to bring a node into consensus with its peers.

To start Chainweb node outside of a Docker container without using a backup database, you can run the following command:

```bash
./chainweb-node
```

### Default ports and bootstrap nodes

After you start a Chainweb node, the node communicates with other nodes in a peer-to-peer (P2P) network. 
By default, the node listens on port 1789 for the P2P communication.
There are several default **bootstrap nodes** in the network.
Bootstrap nodes help other Chainweb nodes discover information about each other in the network. 

Bootstrap nodes must have public DNS names and a corresponding TLS certificate
that is issued by a widely-accepted certificate authority (CA). 
The minimum requirement is acceptance by the OpenSSL library.
To participate in the public network, your node must trust at least one of the bootstrap nodes.

You can configure additional bootstrap nodes by using the `--known-peer-info` command-line option or by settings options in a configuration file. 
You can also ignore the default bootstrap nodes by using the
`--enable-ignore-bootstrap-nodes` command-line option or by settings options in a configuration file
setting.

#### Current testnet bootstrap nodes

- us1.testnet.chainweb.com
- us2.testnet.chainweb.com
- eu1.testnet.chainweb.com
- eu2.testnet.chainweb.com
- ap1.testnet.chainweb.com
- ap2.testnet.chainweb.com

#### Current mainnet bootstrap nodes

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

#### Node service API

The Chainweb node service API exposes endpoints to get information about node and network health, Pact transactions, mining, and on-chain data, including block headers, payloads, and cut heights. 
Because the service API endpoints provide access to potentially sensitive information, you should only expose the Chainweb service API on a private network. 
If you make any service API endpoints available publicly, you should use a reverse proxy to set restrictions—such as rate limiting, authentication, and CORS—to limit access to the endpoints you expose.

### Becoming a bootstrap node

If you like to have your node included as a bootstrap node, you can create a pull
request that adds your node to [P2P.BootstrapNodes module](src/P2P/BootstrapNodes.hs).

However, you should keep in mind that bootstrap nodes are expected to stable with high availability and node operators are expected too guarantee long-term availability and reasonable maintenance of bootstrap nodes.

## Configure features and settings

There are no specific configuration settings required to run a Chainweb node that connects to the Kadena public test or main network.
However, there are many features and options that you might want to enable or modify in different scenarios.
For example, the database backup API and mining are disabled by default.
If you want to use either of those features, you must modify the default configuration by specifying command-line options that enable the feature or by editing the configuration file settings that enable the feature.

You can run display a brief description of all available command-line options by running a command similar to the following on a node with `chainweb-node` installed:

```sh
./chainweb-node --help
```

You can save all of the current configuration settings in a file by running a command similar to the following on a node with `chainweb-node` installed:

```shell
./chainweb-node --print-config > default-config.yaml
```

After you generate the configuration file, you can then edit it or replace it with a configuration file that has the changes you want your node to use.

To only display the configuration values that you've modified with values that are different from their default values, you can run a command similar to the following:

```shell
./chainweb-node --config-file=modified-config.yaml --database-directory=/tmp/testdb --print-config-as=minimal
```
In this example, the modified configuration includes the following settings that aren't using the default values:

```shell
chainweb:
  allowReadsInLocal: true
  backup:
    api:
      enabled: true
    directory: ./chainweb-node-logs/my-backups
databaseDirectory: /tmp/testdb
logging:
  backend:
    handle: file:chainweb-node-logs/cw-log
```

For additional information about configuration file settings, see [Configure node settings](https://docs.kadena.io/guides/nodes/configure).
For additional information about using the chainweb-node command-line options, see [chainweb-node command-line](https://docs.kadena.io/reference/chainweb-cli).

## Monitor node status and operations

After you start your node, there are several checks you should perform to verify that the node is healthy node.
The following examples suggest some of the ways you can check the status of your Chainweb node and monitor network operations.

### Check the public IP address

Verify that `chainweb-node` has a publicly-accessible IP address and a port that is open to the other Chainweb nodes.
   
You can check whether you have a public IP address by looking up the address using [What is My IP Address?](https://whatismyipaddress.com/ip-lookup) or by attempting to send a `ping` request to the address.

### Set the logging level

For production operation, you should configure your node to use the log level of `warn` or `error`.
For example, run a command similar to the following on a node with `chainweb-node` installed:

```shell
./chainweb-node --log-level warn
```

For troubleshooting or improved monitoring, you can set the logging level to `info`.

### Check cut height

After you start a node, you should verify that the node is receiving blocks and catching up with other nodes by checking that the node is your node is receiving and publishing cuts representing the block height for every chain.

Open a new terminal and run a command similar to the following for your node IP address and port number:

```shell
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/cut"
```

For example:

```shell
curl -sk "https://52.207.207.113:1789/chainweb/0.0/mainnet01/cut"
```

You should see the command return the height and hash for multiple chains similar to the following output:

```shell
{"hashes":{"9":{"height":222110,"hash":"yxYbgtla3mnYAi8lTCmnAI6ECaLPpO0mvcvMv4I0V_k"},"0":{"height":222110,"hash":"YIyWRTC-1q08hECJMghFmz4QjYBfpKR08spynK2UNJY"},"6":{"height":222110,"hash":"kA84PgB5l2QT4fBi37GS2CoR6cRSHOjuN9NMh3Vw3zg"},"4":{"height":222110,"hash":"DTpbrhu7LSWDWUg99ryhyNE6JCj4JL5CwEb4v3HPZmk"},"1":{"height":222110,"hash":"QN2XNt9szsXd3Uz5V2fqcOR_w9X5T_D3j4QJM7P_ImQ"},"2":{"height":222110,"hash":"QMI-F_6oaWTGfe5TCMZ0OA2gfrZlcDuQZ2MA1S1UE0k"},"3":{"height":222110,"hash":"0uaGHsd-CJiGIs65I0qrnhiNni8CupGB9aXfn86xke4"},"5":{"height":222110,"hash":"sPPO4UY875aUowqZ5-fJssp9uvMQtwJ_bDOpL0_o4pQ"},"8":{"height":222110,"hash":"VM-lTFylNyEzyn9WupZukTIp6L0bPVygUxYI9J0qnLs"},"7":{"height":222110,"hash":"mReE4wZk77VPpZw2XRQFgNla0mDFjMTxD8K0ihbh_ts"}},"origin":null,"weight":"cphsDY2mePUFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA","height":2221100,"instance":"mainnet01","id":"p0lQi2y96Fe6DkDLjoMJA1PY3isT8FJKodgWWMLgxY4"}
```

You can also navigate to the `/cut` endpoint in your browser.
For example, navigate to a URL similar to the following for your node IP address and port: 

```shell
open https://yourPublicIp:port/chainweb/0.0/mainnet01/cut
```

The `/cut` endpoint returns the latest cut that your node has.
In most cases, if the node is receiving and publishing cuts for every chain, it's working correctly.
However, it's possible that your node could fall behind other nodes, so you should also compare the cut height on your node with the cut heights for one or more bootstrap nodes.
You can get the cut height of any node by running the following command:

```shell
$ curl -sk https://<bootstrap-node-url>/chainweb/0.0/mainnet01/cut | jq '.height'
```

If you intend to use a node for mining, you should verify that the the most up-to-date cut height and that it isn't still in the process for catching up to the rest of the network.

## Enable mining for a node

Successful mining for the Kadena main production network (mainnet01) requires specialized Application-Specific Integrated Circuit (ASIC) hardware. 
Mining also requires you to run `chainweb-node` with the appropriate configuration settings to enable mining and a [`chainweb-mining-client`](https://github.com/kadena-io/chainweb-mining-client/).
The `chainweb-mining-client` is responsible for connecting to the mining API of a chainweb-node and provides a Stratum API for the mining hardware (ASIC).
You can find detailed instructions for setting up the mining client in the [chainweb-mining-client](https://github.com/kadena-io/chainweb-mining-client/) repository.


You can find detailed instructions for setting up the mining infrastructure using `docker compose` in the [docker-compose-chainweb-node/mining-node](https://github.com/kadena-io/docker-compose-chainweb-node/tree/main/mining-node) repository.
For example, you can find information about how to set up a chainweb node for mining in this [docker-compose](https://github.com/kadena-io/docker-compose-chainweb-node/blob/main/mining-node/docker-compose.yaml#L126) file.

## Chainweb component structure

The Chainweb package contains the following buildable components:

* `chainweb` library provides the implementation for the different components of a Chainweb consensus node.

* `chainweb-node` is the application that runs a Chainweb node. 
  It maintains copies
    of a number of chains from a given Chainweb instance. It provides interfaces
    (command-line and RPC) for directly interacting with the Chainweb or for
    implementing applications such as miners and transaction management tools.

* `chainweb-tests` is a test suite for the `chainweb` library and the `chainweb-node` application.

* `cwtool` is a collection of tools that are helpful for maintaining, testing, and debugging Chainweb.

* `bench` is a collection of benchmarks.

### Architecture overview

![Architecture overview](docs/Overview.png)
