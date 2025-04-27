# Chainweb Overview

Chainweb is a proof-of-work, parallel-chain architecture designed for high transaction throughput and security. It was created by Kadena to address the scalability challenges of traditional blockchain systems while maintaining the security properties of Bitcoin's consensus mechanism.

## Key Features

- **Parallel Processing**: Multiple chains operating in parallel allow for higher transaction throughput.
- **Braided Consensus**: Chains are "braided" together through a consensus mechanism that references blocks from neighboring chains.
- **Proof of Work**: Security is maintained through a proven and robust consensus mechanism.
- **Smart Contract Platform**: Executes Pact smart contracts with high performance.

## Core Components

![Architecture Overview](./images/Overview.png)

Chainweb consists of the following core components:

1. **Blockchain Core**: Manages the chain of blocks and maintains consensus.
2. **Pact Smart Contract System**: Executes and validates smart contracts.
3. **Mining System**: Coordinates mining work and processes solutions.
4. **P2P Network**: Enables communication between nodes in the network.
5. **Mempool Management**: Manages pending transactions waiting to be included in blocks.

## Design Philosophy

Chainweb was designed with the following principles in mind:

- **Scalability**: Ability to process thousands of transactions per second.
- **Security**: Maintains the security properties of Bitcoin through proof-of-work.
- **Decentralization**: No central authority or privileged nodes.
- **Interoperability**: Chains can interact with each other through SPV proofs.
