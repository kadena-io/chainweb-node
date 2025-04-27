# Building from Source

**IMPORTANT NOTE**: We recommend using officially released chainweb-node binaries or docker images, which can be found in the [release section of the repository](https://github.com/kadena-io/chainweb-node/releases). If you decide to build your own binaries, please make sure to only use officially released and tagged versions of the code.

## Building with Cabal

### Prerequisites

Install GHC 8.10.7 (Haskell compiler) and Cabal >= 3.4 (Haskell build tool):

#### Linux / Mac

Follow the instructions at [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/)

Install the development versions of required libraries:

```bash
apt-get install ca-certificates libssl-dev libmpfr-dev libgmp-dev libsnappy-dev zlib1g-dev liblz4-dev libbz2-dev libgflags-dev libzstd-dev
```

### Build Process

1. Update Cabal package database:

```bash
cabal update
```

2. Build the project:

```bash
cabal build
```

After this, a runnable binary can be found by running:

```bash
cabal list-bin chainweb-node
```

## Building with Nix

Nix package manager provides binary caching capabilities that allow you to download pre-built binaries for everything needed by Chainweb.

For detailed instructions, see [the wiki](https://github.com/kadena-io/pact/wiki/Building-Kadena-Projects).

When the build is finished, you can run chainweb with:

```bash
./result/ghc/chainweb/bin/chainweb-node
```
