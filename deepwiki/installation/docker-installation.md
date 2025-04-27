# Docker Installation

Chainweb Node can be installed and run using Docker, which provides a convenient containerized environment.

## Docker (All Batteries Included)

A complete Docker image is available from [Docker Hub](https://hub.docker.com/r/kadena/chainweb-node) and can be used with the following commands:

### Initialize the Database (Optional)

This step is optional but recommended as it avoids several hours of initial database synchronization:

```bash
docker run -ti --rm -v chainweb-db:/root/.local/share/chainweb-node/mainnet01/0/ kadena/chainweb-node /chainweb/initialize-db.sh
```

### Run a Chainweb Node in Kadena's Mainnet

```bash
docker run -d -p 443:443 -v chainweb-db:target=/root/.local/share/chainweb-node/mainnet01/0/ kadena/chainweb-node
```

## Docker (Bare Metal)

A docker image with just the chainweb-node binary and its dependencies is available at `ghcr.io/kadena-io/chainweb-node/ubuntu:latest`. With this approach, you need to set up and manage the database and configure the node yourself.

```bash
docker run -p 1789:1789 -p 80:80 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:latest --help
docker run -p 1789:1789 -p 80:80 --entrypoint=/chainweb/chainweb-node ghcr.io/kadena-io/chainweb-node/ubuntu:latest --print-config
```

## Docker Compose Setup

Examples for running Docker Compose setups for chainweb-node for different usage scenarios can be found in [this repository](https://github.com/kadena-io/docker-compose-chainweb-node).
