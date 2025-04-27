# Glossary

This glossary provides definitions for key terms used in the Chainweb Node documentation.

## BlockHeader
Central data structure containing metadata about a block, including nonce, creation time, parent hash, etc.

## Cut
A set of block headers (one per chain) representing the current blockchain state.

## CutDB
Database that stores and synchronizes the latest cut.

## ChainwebVersion
Data type defining protocol parameters and fork schedules.

## PactService
Service for executing and validating Pact smart contracts.

## Checkpointer
Manages persistence of Pact state with rewind capability.

## MiningCoordination
Manages mining work distribution and solution processing.

## P2pNode
Manages peer discovery and communication in the P2P network.

## PeerDb
Database storing information about known peers.

## MempoolBackend
Interface for storing and retrieving pending transactions.

## InMemoryMempool
In-memory implementation of the MempoolBackend.

## TreeDb
Interface for tree-structured databases like BlockHeaderDb.

## BlockHeaderDb
Database for storing and retrieving block headers.

## Pact4/Pact5
Different versions of the Pact smart contract language supported by the system.

## PrimedWork
Cache of pre-computed payloads for miners.

## ChainId
Identifier for a specific chain in the Chainweb network.

## HashTarget
Represents mining difficulty target.

## PayloadWithOutputs
Block payload with transaction outputs.

## Fork
Change to blockchain behavior activated at specific heights.

## P2pSession
Action executed on behalf of local peer with remote peer.

## SQLiteEnv
Persistence environment for Pact state.

## CutHashes
Lightweight representation of a Cut for network transfer.

## ChainGraph
Structure defining connections between chains.

## BlockCreationTime
Timestamp when a block was created.

## ValidationFailure
Error during block header validation.
