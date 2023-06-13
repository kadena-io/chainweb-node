# Rosetta Kadena

## Testing with rosetta-cli
To validate Kadena's rosetta implementation, install [rosetta-cli](https://github.com/coinbase/rosetta-cli#install) and run one of the following commands:

1. This command validates that the Data API information in the testnet network is correct. It also ensures that the implementation does not miss any balance-changing operations.
```
rosetta-cli check:data --configuration-file rosetta-cli-conf/testnet/chain0/configTestnetChain0.json
```

2. This command validates the blockchain’s construction, signing, and broadcasting.
See `rosetta-cli-conf/testnet/chain0/testnet-chain00.ros` for an example of the types of operations supported.
_Only k:accounts transfers are currently available._
```
rosetta-cli check:construction --configuration-file rosetta-cli-conf/testnet/chain0/configTestnetChain0.json
```

3. This command validates that the Data API information in the mainnet network is correct. It also ensures that the implementation does not miss any balance-changing operations.
```
rosetta-cli check:data --configuration-file rosetta-cli-conf/mainnet/chain0/configMainnetChain0.json
```


A couple of things to note:
- The configuration files included here assume testing on chain “0”.
- To run these `rosetta-cli` commands on another chain (valid chains are chains “0” through “19”), change sub_network_identifier.network from “0” to the chain id of your choice. Chain id is expected as a string not a number.
- Replace `localhost` in the `online_url` and `offline_url` fields with the IP address of the testing node that has rosetta enabled for the network (e.g. testnet or mainnet) you’re testing.
- `testnet04` refers to tesnet, and `mainnet01` refers to mainnet.

## Funding Accounts
As part of the testing workflow of the Construction API, accounts need to be funded.

The testnet faucet for chain 1 can be found here: https://faucet.testnet.chainweb.com/

In order to fund accounts using these pre funded accounts, we suggest using https://github.com/kadena-io/kda-exchange-integration and to follow the withdrawal example.

Some clarifications on this example: 
```
processWithdraw('coin', EXCHANGE_KACCOUNT, EXCHANGE_PRIVKEY, customerAddress, 10, "13").then((res) => console.log(res))
```
- `10` refers to the amount to be transferred
- `“13”` refers to the chain id where the “customerAddress” is located
