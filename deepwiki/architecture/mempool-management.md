# Mempool Management

The mempool contains pending transactions waiting to be included in blocks.

## Key Components

### InMem

The `InMem` provides an in-memory implementation of the mempool, including:

- Priority queue for pending transactions
- Recent modifications log
- Transaction validation
- Gas price ordering

### Mempool

The `Mempool` defines the core mempool interface, including:

- Transaction insertion
- Transaction lookup
- Block candidate selection
- Transaction validation

### RestAPI

The `RestAPI` provides a REST API for interacting with the mempool, including:

- Submitting transactions
- Checking transaction status
- Querying pending transactions
- Mempool statistics

## Transaction Lifecycle

1. Transactions are submitted by users via the `/send` endpoint
2. Transactions are validated and inserted into the mempool
3. Transactions are gossiped to peers
4. Miners select transactions for inclusion in blocks
5. Once included in a block, transactions are removed from the mempool

## Transaction Ordering

Transactions in the mempool are ordered by:

1. Gas price (highest first)
2. Gas limit (lowest first, as a tie-breaker)

This ordering ensures that transactions offering higher fees are processed first, while preferring smaller transactions when fees are equal.
