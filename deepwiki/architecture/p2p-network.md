# P2P Network

The P2P Network enables communication between nodes in the Chainweb network.

## Key Components

### Node

The `Node` implements the peer-to-peer node functionality, including:

- Peer discovery
- Connection management
- Message handling
- Network service endpoints

### PeerDB

The `PeerDB` maintains a database of known peers, including:

- Peer addresses
- Connection status
- Trust metrics
- Discovery information

### PeerResources

The `PeerResources` manages resources for P2P networking, including:

- Network connections
- Bandwidth allocation
- Peer session management
- Network configuration

## Network Protocols

Chainweb nodes communicate using several protocols:

- **Peer Discovery**: Finding other nodes in the network
- **Cut Synchronization**: Exchanging cut information
- **Transaction Gossip**: Propagating pending transactions
- **Block Header Exchange**: Sharing new block headers
- **Payload Retrieval**: Requesting and serving block payloads

## Bootstrap Nodes

Bootstrap nodes are used by new nodes to discover other peers in the network. The default bootstrap nodes are:

### Mainnet Bootstrap Nodes

All bootstrap nodes run on port 443:

- us-e1.chainweb.com
- us-e2.chainweb.com
- us-e3.chainweb.com
- us-w1.chainweb.com
- us-w2.chainweb.com
- us-w3.chainweb.com
- jp1.chainweb.com
- jp2.chainweb.com
- jp3.chainweb.com
- fr1.chainweb.com
- fr2.chainweb.com
- fr3.chainweb.com

Custom bootstrap nodes can be configured using the `--known-peer-info` option.
