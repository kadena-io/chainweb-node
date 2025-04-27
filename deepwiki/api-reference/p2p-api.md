# P2P API

The P2P API enables communication between Chainweb nodes in the network.

## Overview

Chainweb nodes communicate with each other using a peer-to-peer network protocol. The P2P API is used for:

- Peer discovery
- Cut synchronization
- Block header exchange
- Payload retrieval
- Transaction gossip

## Endpoints

### `/chainweb/0.0/{network}/cut`

Returns the latest cut that the node has.

**Method**: GET

**Response**: A JSON object representing the current cut.

**Example**:
```bash
curl -sk "https://<public-ip>:<port>/chainweb/0.0/mainnet01/cut"
```

### `/chainweb/0.0/{network}/peer`

Returns information about the node's peers.

**Method**: GET

**Response**: A JSON object containing peer information.

### `/chainweb/0.0/{network}/header/updates`

Provides a stream of block header updates.

**Method**: GET

**Response**: An HTTP event stream of block header updates.

## P2P Configuration

The P2P network can be configured using several options:

- `--known-peer-info`: Specify additional bootstrap nodes
- `--enable-ignore-bootstrap-nodes`: Ignore the builtin bootstrap nodes
- `--p2p-port`: Specify the port for P2P communication (default: 1789)
- `--p2p-private`: Disallow nodes not in the bootstrap list from communicating

## Bootstrap Nodes

Bootstrap nodes are used by chainweb-nodes on startup to discover other nodes in the network. The default bootstrap nodes for mainnet are:

- us-e1.chainweb.com:443
- us-e2.chainweb.com:443
- us-e3.chainweb.com:443
- us-w1.chainweb.com:443
- us-w2.chainweb.com:443
- us-w3.chainweb.com:443
- jp1.chainweb.com:443
- jp2.chainweb.com:443
- jp3.chainweb.com:443
- fr1.chainweb.com:443
- fr2.chainweb.com:443
- fr3.chainweb.com:443
