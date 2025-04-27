# Ubuntu Installation

This guide provides instructions for installing Chainweb Node on Ubuntu Linux.

## Required Packages

### Ubuntu 20.04

```bash
apt-get install ca-certificates libmpfr6 libgmp10 libssl1.1 libsnappy1v5 zlib1g liblz4-1 libbz2-1.0 libgflags2.2 zstd
```

### Ubuntu 22.04

```bash
apt-get install ca-certificates libmpfr6 libgmp10 libssl1.1 libsnappy1v5 zlib1g liblz4-1 libbz2-1.0 libgflags2.2 zstd
```

## Download and Install Binary

1. Download the appropriate binary for your Ubuntu version from the [Chainweb Node releases page](https://github.com/kadena-io/chainweb-node/releases).

2. Extract the binary:

```bash
tar -xzf chainweb-node-ubuntu-*.tar.gz
```

3. Move the binary to a directory in your PATH:

```bash
sudo mv chainweb-node /usr/local/bin/
```

4. Make the binary executable:

```bash
sudo chmod +x /usr/local/bin/chainweb-node
```

## Next Steps

After installation, proceed to the [Configuration](../configuration/README.md) section to set up your node.
