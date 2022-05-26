Upgrading the coin contract

---

For previous work, see past coin upgrade efforts:

- v2: https://github.com/kadena-io/chainweb-node/pull/809
- v3: https://github.com/kadena-io/chainweb-node/pull/1218
- v4: https://github.com/kadena-io/chainweb-node/pull/1397


Upgrading the coin contract is a delicate procedure that requires touching many moving parts. In particular, it touches raw pact source files, Ea (genesis and upgrade tx generation), Rosetta, the transaction execution layer, the chainweb versioning layer, and requires tests that span multiple upgrades in order to prove the correctness of upgrade code. Below is a detailed rundown of the procedure and what code may need to be modified in order to correctly upgrade the code contract.

## Coin-v*.pact upgrades

In order to upgrade the coin contract source code, the following are needed:

- A pact version bump: obviously, if changes to Pact are involved, the pact source repository reference in the `cabal.project` file must be modified with a reference to the correct git tag or commit.
- [Bless](https://pact-language.readthedocs.io/en/stable/pact-reference.html?highlight=bless#blessing-hashes) previous coin versions: this allows for Kadena, as an upstream provider, to phase in v(n+1) upgrades without breaking downstream code that depends on older versions of the coin contract.
- Make code changes as needed.
- Repl test files: the repl files should be labeled `coin-vN.repl` (for some N), and should be a copy-over of the previous `coin-v(N-1).repl`, with additional tests for modified code. When a pull request is made to the repository, someone should check the diff between the two repl files to make sure nothing has changed needlessly. We should try to keep test semantics as stable as possible unless there's a good reason.
- Create a `load-coin-contract-v*.yaml` script in the immediate directory with the following:

  ```yaml
  codeFile: coin-v*.pact
  nonce: coin-contract-v*
  keyPairs: []
  ```

  This will be used during the `Ea` upgrade transaction generation process.

## Updating Genesis + Upgrade transactions

In order to generate upgrade or new genesis transactions, the `ea` tool must be run on an updated script with the new transaction information added to the list of generated transactions. The following steps must be taken in order to generate the new modules that will reference these transactions for use in `chainweb-node` Haskell source:

- Update `Ea`: this is a four step process.
  1. `tools/ea/Ea/Genesis.hs` must be updated to reference the load YAML script defined in the previous section for coin contract changes.
  2. `tools/ea/Ea.hs` must be updated to call `genTxModule` on the new coin contract reference (see: `genCoinV4Payloads` etc.).
  3. the newly updated `genCoinV*Payloads` function must be added to the `main` function. Keep in mind that if any additional upgrades are present, the transactions are generated in sequence, therefore are order-sensitive. If new code has dependencies, then those dependencies need to occur earlier in the input list to `genTxModule`.
  4. The `ea` tool must be run by issuing the following command:
  ```shell
  cabal run cwtool -- ea
  ```
  This will produce new modules in the `src/Chainweb/Pact/Transactions` directory.
- Add the newly generated transaction modules to chainweb-node.cabal
- Export the new transaction values via `src/Chainweb/Pact/Transactions/UpgradeTransactions.hs`.
- Update Rosetta if need be (see Linda's note in `src/Chainweb/Pact/Transactions/UpgradeTransactions.hs`).

This procedure will allow the new transactions to be referenced from Haskell code as values. From here, the new transactions need to be applied at a particular block height via the transaction layer. The trigger for such a change is canonically located in `src/Chainweb/Version.hs`, while the application of the upgrades themselves are canonically located in `src/Chainweb/Pact/TransactionExec.hs`.

## Updating TxExec.hs

- add flags in upgrade at applyUpgrades

## Chainweb Version

In order to make sure the new coin contract upgrades are applied in the future, a blockheight must be chosen at which to apply the upgrades. These block heights are calculated to be at some date in the near future which coincide with the killswitch dates decided by the chainweb leads. They are defined in `src/Chainweb/Version.hs`. The naming convention for pact upgrades as of the date of this document is of the form `chainweb<\new chainweb version number>Pact` (e.g. `chainweb214Pact`). Within these functions, we use the `AtOrAfter` construct to define behaviors across chainweb for what to do at or after a particular block height. Keep in mind that these upgrades interact with each other, and therefore the choice of blockheights for the testing and devnet environments will need to be amended strictly monotonically.

## Testing

- PactInProcApi
  - Make sure v* is backcompatible across 2 forks so (see: pact4coin3UpgradeTest)
    - https://github.com/kadena-io/chainweb-node/pull/1416
    - https://github.com/kadena-io/chainweb-node/pull/1413

  - It should be the case that updates are backcompatible if users start from scratch
  - Events need to be backcompatible
