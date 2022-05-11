Upgrading the coin contract

- v2: https://github.com/kadena-io/chainweb-node/pull/809
- v3: https://github.com/kadena-io/chainweb-node/pull/1218
- v4: https://github.com/kadena-io/chainweb-node/pull/1397

--- 

## Coin-v* Upgrades

- Blessing
- Updating code
- Repl files

## Updating Genesis 

- load yaml
- run Ea
- Add modules to chainweb-node.cabal
- add transactions to 
- UpgradeTransactions.hs
- Update Rosetta (see Linda's note)

## Chainweb Version 

- At or After block heights
- Add disabling flags

## Updating TxExec.hs

- add flags in upgrade at applyUpgrades

## Testing 

- PactInProcApi
  - Make sure v* is backcompatible across 2 forks so (see: pact4coin3UpgradeTest)
    - https://github.com/kadena-io/chainweb-node/pull/1416
    - https://github.com/kadena-io/chainweb-node/pull/1413
    
  - It should be the case that updates are backcompatible if users start from scratch
  - Events need to be backcompatible
