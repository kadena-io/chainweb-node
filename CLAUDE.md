# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Chainweb-node is the implementation of Kadena's Chainweb consensus protocol - a braided, parallelized Proof of Work blockchain architecture. The project is written in Haskell and uses Cabal as the build system.

## Build Commands

### Building the project
```bash
# Update cabal package index (do this occasionally)
cabal update

# Build all components
cabal build all

# Build specific components
cabal build chainweb         # Main library
cabal build chainweb-node    # Node executable  
cabal build cwtools          # Command-line tools
```

### Running tests
```bash
# Run all tests
cabal test chainweb-tests

# Run tests with specific patterns
cabal run chainweb-tests -- --hide-successes -p '!/chainweb216Test/'

# Run a single test module
cabal test chainweb-tests --test-options="-p 'Chainweb.Test.BlockHeaderDB'"
```

### Common development tasks
```bash
# Run the chainweb node
cabal run chainweb-node

# Get help and configuration options
cabal run chainweb-node -- --help
cabal run chainweb-node -- --print-config > config.yaml

# Build with optimizations
cabal build -O2 chainweb-node

# Clean build artifacts
cabal clean
```

## Architecture Overview

### Core Module Structure

The codebase is organized into several key areas:

- **src/Chainweb/**: Core blockchain implementation
  - `BlockHeader/`: Block header management and genesis blocks
  - `BlockHeaderDB/`: Block header database operations
  - `CutDB/`: Cut (blockchain state) management
  - `Mempool/`: Transaction mempool implementation
  - `Miner/`: Mining-related functionality
  - `Pact/`: Smart contract (Pact) integration
  - `SPV/`: Simple Payment Verification proofs
  - `Payload/`: Block payload management
  - `RestAPI/`: REST API endpoints
  - `VerifierPlugin/`: Pluggable verification systems (e.g., Hyperlane)

- **src/P2P/**: Peer-to-peer networking layer
  - Bootstrap nodes configuration
  - Peer discovery and communication

- **node/**: Chainweb-node executable
  - Main entry point is `node/src/ChainwebNode.hs`

- **cwtools/**: Command-line utilities for debugging and maintenance

- **test/**: Test suites
  - `unit/`: Unit tests
  - `pact/`: Pact smart contract tests
  - `multinode/`: Multi-node integration tests

### Key Architectural Concepts

1. **Chainweb Protocol**: Multiple parallel chains (20 chains in mainnet) that reference each other's block headers, creating a braided structure

2. **Cut**: The current state across all chains - essentially a snapshot of the latest block header on each chain

3. **Pact Integration**: Each chain runs its own Pact interpreter for smart contract execution

4. **P2P Network**: Nodes communicate via a custom P2P protocol for block propagation and synchronization

5. **Mining API**: Provides endpoints for external miners to get work and submit solutions

## Working with the Codebase

### Language Extensions
The project uses many GHC language extensions. Common ones include:
- OverloadedStrings
- LambdaCase  
- TypeApplications
- ScopedTypeVariables
- FlexibleContexts

### External Dependencies
Key external dependencies managed via Cabal:
- pact (smart contract language)
- rocksdb-haskell (database backend)
- servant (REST API framework)
- yet-another-logger (logging)

Custom forks are specified in `cabal.project` as source-repository-packages.

### Database Backends
- RocksDB for block headers and payloads
- SQLite for Pact state

### Testing Approach
- Unit tests use HUnit and QuickCheck
- Integration tests use custom multi-node test framework
- Test data files in `test/pact/` and `test/golden/`
- CI runs tests on multiple GHC versions (9.6, 9.8, 9.10)

## Network Configuration

### Networks
- **mainnet01**: Production network
- **testnet04**: Test network  
- **development**: Local development network

### Bootstrap Nodes
Hardcoded in `src/P2P/BootstrapNodes.hs`
- Mainnet: us-[e|w][1-3].chainweb.com, jp[1-3].chainweb.com, fr[1-3].chainweb.com
- Testnet: us[1-2].testnet.chainweb.com, eu[1-2].testnet.chainweb.com, ap[1-2].testnet.chainweb.com

## Important Files

- `cabal.project`: Cabal configuration with dependency overrides
- `chainweb.cabal`: Main library package definition
- `node/chainweb-node.cabal`: Node executable package
- `.github/workflows/`: CI/CD workflows
- `src/Chainweb/Version.hs`: Network version definitions