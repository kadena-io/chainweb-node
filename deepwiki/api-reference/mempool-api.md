# Mempool API

The Mempool API allows users to interact with the mempool, which contains pending transactions waiting to be included in blocks.

## Overview

The Mempool API provides endpoints for:

- Submitting transactions to the mempool
- Checking transaction status
- Retrieving pending transactions
- Querying mempool statistics

## Endpoints

### `/chainweb/0.0/{network}/chain/{chain-id}/mempool/submit`

Submits a transaction to the mempool.

**Method**: POST

**Request Body**: A JSON object containing the transaction.

**Response**: A JSON object indicating whether the transaction was accepted.

**Example**:
```bash
curl -sk -X POST "https://<public-ip>:<port>/chainweb/0.0/mainnet01/chain/0/mempool/submit" \
  -H "Content-Type: application/json" \
  -d '{"transaction": {...}}'
```

### `/chainweb/0.0/{network}/chain/{chain-id}/mempool/lookup`

Looks up a transaction in the mempool by hash.

**Method**: GET

**Parameters**:
- `txhash`: Transaction hash to look up

**Response**: A JSON object containing the transaction status.

**Example**:
```bash
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/chain/0/mempool/lookup?txhash=..."
```

### `/chainweb/0.0/{network}/chain/{chain-id}/mempool/pending`

Retrieves pending transactions from the mempool.

**Method**: GET

**Parameters**:
- `limit`: Maximum number of transactions to return

**Response**: A JSON array of pending transactions.

**Example**:
```bash
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/chain/0/mempool/pending?limit=10"
```

## Mempool Configuration

The mempool can be configured using several options:

- `--mempool-max-gas-limit`: Maximum gas limit for transactions in the mempool
- `--mempool-min-gas-price`: Minimum gas price for transactions to be accepted
- `--mempool-max-tx-size`: Maximum size of transactions in the mempool
- `--mempool-check-interval`: Interval for checking and pruning expired transactions
