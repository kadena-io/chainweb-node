# Blockchain Core

The blockchain core is responsible for maintaining the chain of blocks, validating new blocks, and ensuring consensus across the network.

## Key Components

### BlockHeader

The `BlockHeader` is a central data structure that contains metadata about a block, including:

- Nonce
- Creation time
- Parent hash
- Adjacent chain hashes
- Target difficulty
- Payload hash
- Chain ID
- Block weight
- Block height
- Chainweb version

### Cut

A `Cut` is a set of block headers (one per chain) representing the current blockchain state. It's a fundamental concept in Chainweb that captures a cross-section of all chains at a specific point.

### CutDB

The `CutDB` is a database that stores and synchronizes the latest cut. It's responsible for:

- Storing and retrieving cuts
- Validating new cuts
- Broadcasting cut updates to peers
- Managing cut history

### Version

The `Version` module defines protocol parameters and fork schedules. It ensures that all nodes on the network operate with compatible rules.

## Block Validation Process

When a new block is received:

1. The header is validated against the chain's rules
2. The block's payload is verified
3. If valid, the block is added to the chain
4. A new cut is computed and propagated to peers

## Consensus Mechanism

Chainweb achieves consensus through:

1. Proof-of-work mining to create valid blocks
2. Block header verification to ensure blocks follow protocol rules
3. Cut-based synchronization to maintain a consistent view of the network
4. Difficulty adjustment to regulate block production
