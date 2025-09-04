# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Chainweb-node is the implementation of Kadena's public blockchain - a fast, secure, and scalable blockchain using the Chainweb consensus protocol. Chainweb is a braided, parallelized Proof-of-Work consensus mechanism that improves throughput and scalability while maintaining Bitcoin-level security.

This is a **Haskell project** using Cabal for build management.

## Development Commands

### Building the Project

```bash
# Update package index
cabal update

# Build the project
cabal build

# Build specific target
cabal build chainweb-node
cabal build cwtools
```

### Testing

```bash
# Run all tests
cabal test

# Run specific test suites
cabal test chainweb-tests           # Unit tests
cabal test compaction-tests         # Compaction tests  
cabal test multi-node-network-tests # Multi-node network tests
cabal test remote-tests             # Remote tests
```

### Running the Node

```bash
# Run chainweb-node directly
chainweb-node

# Print default configuration
chainweb-node --print-config

# Run with custom config
chainweb-node --config-file=config.yaml

# Check help
chainweb-node --help
```

### Benchmarking

```bash
# Run benchmarks
cabal bench
```

### Development with Nix (Alternative)

```bash
# Build with Nix
nix build

# Enter development shell
nix develop

# Run chainweb-node through Nix
./result/ghc/chainweb/bin/chainweb-node
```

## Architecture Overview

### Core Components

**Chainweb Library** (`src/Chainweb/`):
- `Chainweb.hs` - Main orchestration module managing component lifecycle
- `Cut/` - Cut management (global state across all chains)  
- `BlockHeaderDB/` - Block header database and validation
- `Payload/` - Transaction payload storage and management
- `Mempool/` - Transaction mempool for pending transactions
- `Miner/` - Mining coordination and API
- `P2P/` - Peer-to-peer networking layer
- `Pact4/`, `Pact5/` - Smart contract execution engines (Pact versions)
- `SPV/` - Simple Payment Verification proofs
- `RestAPI/` - HTTP API endpoints

**Applications**:
- `chainweb-node` (`node/`) - Main blockchain node application
- `cwtools` (`cwtools/`) - Collection of utility tools for maintenance and debugging

**Key Data Flow**:
1. P2P layer handles node discovery and communication
2. Mempool collects and validates transactions
3. Miner creates new blocks with transactions from mempool
4. Cut represents the current state across all parallel chains
5. Pact engines execute smart contracts within blocks
6. BlockHeaderDB stores and validates the blockchain structure

### Multi-Chain Architecture

Chainweb implements a braided multi-chain architecture where:
- Multiple parallel chains run simultaneously (mainnet has 20 chains)
- Chains are connected in a graph structure for cross-chain communication
- Each chain has its own block headers and transactions
- A "cut" represents the current state across all chains

### Database Architecture

- **RocksDB**: Primary storage backend for persistent data
- **SQLite**: Used for Pact state database
- Supports database checkpointing and compaction
- Backup and restore functionality for node operators

## Testing Strategy

**Test Organization**:
- `test/unit/` - Fast unit tests for individual components
- `test/compaction/` - Database compaction testing
- `test/multinode/` - Multi-node network scenario tests
- `test/remote/` - Tests requiring network resources
- `test/lib/` - Shared testing utilities and helper functions

**Pact Testing**:
- Separate test suites for Pact4 and Pact5 smart contract engines
- Transaction execution testing
- SPV proof validation testing

## Configuration

- Default config generated with `chainweb-node --print-config`
- YAML configuration format
- Bootstrap nodes pre-configured for mainnet and testnet
- Port 1789 for P2P communication (must be publicly accessible)
- Port 1848 for service API (should be private/firewalled)

## Hardware Requirements

**Minimal**:
- 2 CPU cores
- 4 GB RAM  
- 250 GB SSD storage
- Public IP address

**Recommended** (for API server/mining):
- 4+ CPU cores
- 8+ GB RAM

## Important File Patterns

- `*.cabal` - Haskell package definitions
- `cabal.project*` - Multi-package project configuration
- `*.nix` - Nix build configuration
- `.github/workflows/` - CI/CD pipeline definitions
- `pact/` - Pact smart contract code and genesis data
- `docs/` - Additional technical documentation

## Haskell Development Guide

### GHC Configuration & Language Extensions

**GHC Versions Supported**: GHC 9.6, 9.8, 9.10 (primary)

**Key Cabal Configuration**:
```bash
# Common GHC flags used across the project
ghc-options: -Wall -Werror -Wcompat -Wpartial-fields 
             -Wincomplete-record-updates -Wincomplete-uni-patterns 
             -Widentities -funclutter-valid-hole-fits
             -fmax-relevant-binds=0 -Wno-gadt-mono-local-binds
```

**Commonly Used Language Extensions**:
- `BangPatterns` - Strict evaluation patterns for performance
- `DataKinds` - Type-level data for type safety
- `DeriveAnyClass` / `DeriveGeneric` - Automatic instance derivation
- `DerivingStrategies` - Explicit derivation strategies 
- `FlexibleContexts` / `FlexibleInstances` - Type class flexibility
- `GADTs` - Generalized Algebraic Data Types for type safety
- `GeneralizedNewtypeDeriving` - Efficient newtype wrappers
- `LambdaCase` - Concise pattern matching
- `OverloadedStrings` - String literal polymorphism
- `RankNTypes` - Higher-rank polymorphism for resource management
- `ScopedTypeVariables` - Type variable scoping
- `TemplateHaskell` - Compile-time metaprogramming
- `TypeApplications` - Explicit type application
- `TypeFamilies` - Type-level functions

### Architecture Patterns

**Resource Management Pattern**:
The project uses a consistent `with*` / `run*` pattern for resource lifecycle:
```haskell
-- Resource acquisition and initialization
withChainweb :: ChainwebConfiguration -> logger -> RocksDb -> FilePath -> FilePath -> Bool -> (StartedChainweb logger -> IO ()) -> IO ()

-- Component execution
runChainweb :: Chainweb logger tbl -> ((NowServing -> NowServing) -> IO ()) -> IO ()
```

**Lens-Based Configuration**:
Extensive use of `lens` library for nested configuration access:
```haskell
-- Common lens patterns
makeLenses ''Chainweb  -- Generate lenses automatically
conf & set (configServiceApi . serviceApiConfigPort) newPort  -- Configuration updates
```

**Type-Level Safety**:
- ChainId and other domain types wrapped in newtypes for type safety
- Phantom type parameters to track resource states
- GADTs for type-safe protocol definitions

### Key Libraries and Dependencies

**Core Blockchain Libraries**:
- `rocksdb-haskell-kadena` - Persistent storage backend
- `merkle-log` - Merkle tree implementations
- `pact` / `pact-tng` - Smart contract execution engines
- `chainweb-storage` - Storage abstractions

**Networking & Serialization**:
- `servant` / `servant-server` / `servant-client` - REST API framework
- `wai` / `warp` - HTTP server infrastructure  
- `aeson` - JSON serialization
- `binary` - Binary serialization

**Concurrency & Performance**:
- `async` - Structured concurrency
- `stm` - Software Transactional Memory
- `unliftio` - Safe exception handling with ResourceT
- `streaming` - Memory-efficient data processing

**Cryptography**:
- `crypton` - Cryptographic primitives (replaces deprecated cryptonite)
- `ethereum` - Ethereum cryptography for bridge functionality

### Module Organization

**Core Architecture Modules**:
```
src/Chainweb/
├── Chainweb.hs              # Main orchestration and resource management
├── Cut/                     # Global state management across chains
├── BlockHeaderDB/           # Block header storage and validation  
├── Payload/                 # Transaction payload management
├── Mempool/                 # Transaction pool management
├── Miner/                   # Mining coordination
├── P2P/                     # Peer-to-peer networking
├── Pact4/ & Pact5/         # Smart contract execution engines
├── SPV/                     # Simple Payment Verification
└── RestAPI/                 # HTTP API endpoints
```

**Key Type Definitions**:
- `Cut` - Global state snapshot across all chains
- `BlockHeader` - Block metadata and proof-of-work
- `ChainId` - Type-safe chain identifier
- `ChainResources` - Per-chain resource bundle

### Testing Framework

**Test Organization** (using Tasty framework):
```
test/
├── unit/           # Fast unit tests (parallel execution)
├── compaction/     # Database compaction testing  
├── multinode/      # Multi-node network scenarios
├── remote/         # Tests requiring network resources
└── lib/            # Shared test utilities and fixtures
```

**Testing Libraries**:
- `tasty` - Main test framework
- `tasty-hunit` - Unit test support
- `tasty-quickcheck` - Property-based testing
- `tasty-hedgehog` - Alternative property testing  
- `QuickCheck` - Property-based test generation
- `property-matchers` - Advanced property matching

**Test Utilities**:
```haskell
-- Common test patterns
withToyDB :: (RocksDb -> IO a) -> IO a  -- Temporary test database
independentSequentialTestGroup  -- Sequential test execution for stateful tests  
```

### Performance & Benchmarking

**Criterion Benchmarks**:
```bash
cabal bench  # Run all performance benchmarks
```

**Key Performance Areas**:
- `Chainweb.MempoolBench` - Transaction pool performance
- `Chainweb.Pact.Backend.*` - Smart contract execution benchmarks
- JSON encoding/decoding optimizations

**RTS Options for Testing**:
```bash
# Multi-node tests use specific RTS settings
ghc-options: -threaded -rtsopts "-with-rtsopts=-N -H1G -A64M"
```

### Development Workflow

**Building**:
```bash
# Standard development build
cabal build

# With specific GHC flags for debugging
cabal configure --ghc-options="-g"
cabal build

# Parallel builds (faster)
cabal build -j
```

**Testing Workflow**:
```bash
# Fast feedback loop - run unit tests only
cabal test chainweb-tests

# Full test suite (includes network tests)
cabal test

# Specific test modules
cabal test chainweb-tests --test-options="-p '/BlockHeader/'"
```

**Debugging & Profiling**:
```bash
# Enable debugging symbols  
cabal configure --enable-profiling --ghc-options="-g"

# Heap profiling
cabal run exe:chainweb-node -- +RTS -h -p

# Time profiling
cabal run exe:chainweb-node -- +RTS -p

# Thread activity  
cabal run exe:chainweb-node -- +RTS -N -ls
```

### Performance Considerations

**Critical Performance Patterns**:
- Strict evaluation with BangPatterns for hot paths
- Efficient serialization with `binary` for network protocols
- STM for concurrent data structures (Cut management)
- Streaming for large data processing (block synchronization)
- RocksDB tuning for persistent storage performance

**Memory Management**:
- Resource bracketing with `bracket` / `finally`
- `ResourceT` for automatic cleanup
- Compact regions for large persistent data structures
- NFData instances for deep evaluation control

**Common Performance Pitfalls**:
- Lazy evaluation in accumulating functions (use strict folds)
- String concatenation (prefer Text or ByteString)
- Inefficient JSON parsing (use strict parsing)
- Memory leaks from unevaluated thunks in long-running processes

### Code Quality & Style

**Linting & Formatting**:
- Strict compiler warnings enabled (`-Wall -Werror`)
- Consistent use of explicit imports  
- Prefer qualified imports for common modules (Data.Map, Data.Set)
- Use `-Wno-missing-home-modules` for GHCI compatibility

**Type Safety Practices**:
- Extensive use of newtypes for domain modeling
- Type families for compile-time guarantees
- Smart constructors for invariant enforcement
- Phantom types for state tracking

**Error Handling**:
- `UnliftIO.Exception` for async-safe exception handling
- `Either` for recoverable errors
- `Maybe` for optional values  
- Custom exception types for domain-specific errors

### Integration Points

**Smart Contract Integration** (Pact):
- Dual Pact4/Pact5 engine support
- SQLite backend for Pact state storage
- JSON-RPC interface for contract execution
- Gas metering and transaction validation

**Database Integration**:
- RocksDB for primary blockchain storage
- SQLite for Pact smart contract state
- Configurable compaction strategies
- Backup and restore functionality

**Network Protocols**:
- Custom P2P protocol over HTTP/2
- REST API for external integrations
- WebSocket support for real-time updates
- SPV proofs for light client support