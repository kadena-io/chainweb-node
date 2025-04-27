# Pact Smart Contract System

The Pact Smart Contract System executes and validates Pact code on the Chainweb blockchain.

## Key Components

### PactService

The `PactService` is the main service responsible for:

- Executing and validating smart contracts
- Creating and validating blocks
- Managing the Pact database
- Handling transaction requests

### Backend Types

The `Backend/Types` module provides the persistence layer for smart contract state, including:

- Transaction results
- Contract state
- Module definitions
- Keysets

### TransactionExec

The `TransactionExec` modules handle the execution of transactions, including:

- Transaction validation
- Gas metering
- Execution of Pact code
- State updates

## Pact Versions

Chainweb supports multiple versions of the Pact language:

- **Pact4**: The previous version of the Pact language
- **Pact5**: The current version of the Pact language with improved performance and features

## Transaction Lifecycle

1. Transactions are submitted to the mempool
2. When included in a block, transactions are executed by the Pact service
3. State changes are persisted to the Pact database
4. Transaction results are included in the block
