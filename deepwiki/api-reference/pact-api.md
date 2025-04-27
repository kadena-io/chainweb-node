# Pact API

The Pact API allows users to interact with the Pact smart contract system on Chainweb.

## Overview

The Pact API provides endpoints for:

- Sending transactions
- Querying contract state
- Executing local queries
- Retrieving transaction results

## Endpoints

### `/chainweb/0.0/{network}/chain/{chain-id}/pact/api/v1/send`

Sends a signed transaction to the network.

**Method**: POST

**Request Body**: A JSON object containing the signed Pact command.

**Response**: A JSON object containing the request key.

**Example**:
```bash
curl -sk -X POST "https://<public-ip>:<port>/chainweb/0.0/mainnet01/chain/0/pact/api/v1/send" \
  -H "Content-Type: application/json" \
  -d '{"cmds":[{"cmd":"...","sigs":[...],"hash":"..."}]}'
```

### `/chainweb/0.0/{network}/chain/{chain-id}/pact/api/v1/local`

Executes a Pact command locally without sending it to the blockchain.

**Method**: POST

**Request Body**: A JSON object containing the Pact command.

**Response**: A JSON object containing the execution result.

**Example**:
```bash
curl -sk -X POST "https://<public-ip>:<port>/chainweb/0.0/mainnet01/chain/0/pact/api/v1/local" \
  -H "Content-Type: application/json" \
  -d '{"code":"(+ 1 2)","data":{},"keyPairs":[]}'
```

### `/chainweb/0.0/{network}/chain/{chain-id}/pact/api/v1/poll`

Polls for transaction results.

**Method**: POST

**Request Body**: A JSON object containing the request keys to poll.

**Response**: A JSON object containing the transaction results.

**Example**:
```bash
curl -sk -X POST "https://<public-ip>:<port>/chainweb/0.0/mainnet01/chain/0/pact/api/v1/poll" \
  -H "Content-Type: application/json" \
  -d '{"requestKeys":["..."]}'
```

### `/chainweb/0.0/{network}/chain/{chain-id}/pact/api/v1/listen`

Listens for transaction results.

**Method**: POST

**Request Body**: A JSON object containing the request keys to listen for.

**Response**: A JSON object containing the transaction results.

**Example**:
```bash
curl -sk -X POST "https://<public-ip>:<port>/chainweb/0.0/mainnet01/chain/0/pact/api/v1/listen" \
  -H "Content-Type: application/json" \
  -d '{"requestKeys":["..."]}'
```

## Pact Versions

Chainweb supports multiple versions of the Pact language:

- **Pact4**: The previous version of the Pact language
- **Pact5**: The current version of the Pact language with improved performance and features
