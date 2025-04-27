# Service API

The Service API provides general information about the Chainweb node and its status.

## Overview

The Service API provides endpoints for:

- Retrieving node information
- Checking node health
- Accessing configuration
- Monitoring node metrics

## Endpoints

### `/chainweb/0.0/{network}/info`

Returns general information about the node.

**Method**: GET

**Response**: A JSON object containing node information.

**Example**:
```bash
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/info"
```

### `/chainweb/0.0/{network}/health-check`

Checks the health of the node.

**Method**: GET

**Response**: A JSON object indicating the node's health status.

**Example**:
```bash
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/health-check"
```

### `/chainweb/0.0/{network}/config`

Returns the node's configuration.

**Method**: GET

**Response**: A JSON object containing the node's configuration.

**Example**:
```bash
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/config"
```

### `/chainweb/0.0/{network}/cut`

Returns the current cut of the node.

**Method**: GET

**Response**: A JSON object representing the current cut.

**Example**:
```bash
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/cut"
```

## Service Configuration

The service can be configured using several options:

- `--service-port`: Port for the service API (default: 1848)
- `--service-interface`: Network interface to bind to
- `--service-disable`: Disable the service API
- `--service-log-level`: Log level for the service
