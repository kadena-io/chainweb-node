# Mining API

The Mining API allows miners to interact with the Chainweb node to request work and submit solutions.

## Overview

The Mining API provides endpoints for:

- Requesting mining work
- Submitting mining solutions
- Checking mining status
- Retrieving mining-related information

## Endpoints

### `/chainweb/0.0/{network}/mining/work`

Requests mining work from the node.

**Method**: GET

**Parameters**:
- `chain`: Chain ID to mine on
- `miner`: Miner account that will receive the reward
- `limit`: Maximum number of work items to return

**Response**: A JSON object containing mining work information.

**Example**:
```bash
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/mining/work?chain=0&miner=k:account&limit=1"
```

### `/chainweb/0.0/{network}/mining/solve`

Submits a mining solution to the node.

**Method**: POST

**Request Body**: A JSON object containing the mining solution.

**Response**: A JSON object indicating whether the solution was accepted.

**Example**:
```bash
curl -sk -X POST "https://<public-ip>:<port>/chainweb/0.0/mainnet01/mining/solve" \
  -H "Content-Type: application/json" \
  -d '{"solution": {...}}'
```

### `/chainweb/0.0/{network}/mining/updates`

Provides a stream of mining work updates.

**Method**: GET

**Response**: An HTTP event stream of mining work updates.

## Mining Configuration

The mining system can be configured using several options:

- `--mining-public-key`: Public key for the mining account
- `--mining-disable`: Disable mining functionality
- `--mining-coordination-enabled`: Enable mining coordination
- `--mining-coordination-chain-id`: Chain ID for mining coordination
