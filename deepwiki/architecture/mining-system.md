# Mining System

The Mining System coordinates mining work and processes solutions to create new blocks.

## Key Components

### Coordinator

The `Coordinator` is responsible for:

- Distributing mining work to miners
- Validating mining solutions
- Creating new blocks when valid solutions are found
- Managing the mining work queue

### MinerResources

The `MinerResources` manages resources for mining, including:

- Connection to miners
- Work generation
- Reward distribution
- Mining configuration

### RestAPI/Server

The `RestAPI/Server` provides a REST API for miners to:

- Request mining work
- Submit mining solutions
- Check mining status
- Access mining statistics

## Mining Process

1. A miner requests work from the mining coordinator
2. The coordinator creates a work item with a specific target difficulty
3. The miner attempts to find a nonce that produces a hash below the target
4. When a solution is found, it's submitted to the coordinator
5. The coordinator validates the solution and creates a new block
6. The block is propagated to the network

## Mining Rewards

Mining rewards are distributed according to the protocol rules, which include:

- Base block reward
- Transaction fees
- Distribution to the mining address specified in the coinbase transaction
