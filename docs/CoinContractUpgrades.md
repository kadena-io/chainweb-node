# Upgrading the coin contract

---

For previous work, see past coin upgrade efforts:

- v2: https://github.com/kadena-io/chainweb-node/pull/809
- v3: https://github.com/kadena-io/chainweb-node/pull/1218
- v4: https://github.com/kadena-io/chainweb-node/pull/1397


Upgrading the coin contract is a delicate procedure that requires touching many moving parts. In particular, it touches raw pact source files, Ea (genesis and upgrade tx generation), the transaction execution layer, the chainweb versioning layer, and requires tests that span multiple upgrades in order to prove the correctness of upgrade code. Below is a detailed rundown of the procedure and what code may need to be modified in order to correctly upgrade the code contract.

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
  2. `tools/ea/Ea.hs` must be updated to call `genTxModule` on the new coin contract reference (see: `genCoinV4Payloads` etc.). Example:
  ```haskell
    genCoinV4Payloads :: IO ()
    genCoinV4Payloads = genTxModule "CoinV4"
      [ fungibleXChainV1
      , coinContractV4
      ]

    genCoinV5Payloads :: IO ()
    genCoinV5Payloads = genTxModule "CoinV5"
      [ coinContractV5
      ]
  ```
  3. the newly updated `genCoinV*Payloads` function must be added to the `main` function. Keep in mind that if any additional upgrades are present, the transactions are generated in sequence, therefore are order-sensitive. If new code has dependencies, then those dependencies need to occur earlier in the input list to `genTxModule`.
  4. The `ea` tool must be run by issuing the following command:
  ```shell
  cabal run cwtool -- ea
  ```
  This will produce new modules in the `src/Chainweb/Pact/Transactions` directory.
- Add the newly generated transaction modules to chainweb-node.cabal
- Export the new transaction values via `src/Chainweb/Pact/Transactions/UpgradeTransactions.hs`.

This procedure will allow the new transactions to be referenced from Haskell code as values. From here, the new transactions need to be applied at a particular block height via the transaction layer. The trigger for such a change is canonically located in `src/Chainweb/Version.hs`, while the application of the upgrades themselves are canonically located in `src/Chainweb/Pact/TransactionExec.hs`.

## Updating TxExec.hs

`/src/Chainweb/Pact/TransactionExec.hs` is where upgrades are applied during the coinbase phase of mining blocks, via the `applyUpgrades` function. During this phase, the flags defined in `src/Chainweb/Version.hs` are used to branch on the particular upgrade transactions that are inserted into the block, and the resulting updates to the pact db are incorporated as transactions in the block, and the pact service module cache is updated as needed.

### Pact flags

Depending on the type of upgrades, code changes may or may not require applying certain execution environment flags during the `applyTx` portion of the `applyUpgrade` call. These flags mask new features during pact code execution when loading the contracts, and are primarily used for backwards compatibility with older contracts. If upgrades require a breaking change to pact code, you must add these flags in during the `applyTx` phase in order for older code depending on older versions of pact to run properly. Flags look like this (from `applyUpgrades`):

```haskell
    applyCoinV2 = applyTxs (upgradeTransactions v cid) [FlagDisableInlineMemCheck, FlagDisablePact43]

    applyCoinV3 = applyTxs coinV3Transactions [FlagDisableInlineMemCheck, FlagDisablePact43]

    applyCoinV4 = applyTxs coinV4Transactions []
```

### Module caching

Historically, the module cache has had trouble upon replay, due to the tricky nature of rolling back the cache across block heights when it crosses the upgrade/rewind threshold for specific upgrades. For example:

- https://github.com/kadena-io/chainweb-node/pull/1413
- https://github.com/kadena-io/chainweb-node/pull/1236
- https://github.com/kadena-io/chainweb-node/pull/1169
- https://github.com/kadena-io/chainweb-node/pull/1029
- https://github.com/kadena-io/chainweb-node/pull/821
- https://github.com/kadena-io/chainweb-node/pull/722

In #1413, we note that in the case of loading interfaces along with other upgrades, there is additional work that needs to be done in order to make the interface available, since module caching relies on module load in Pact to update the cache. This results in us having to hack interface references into the module cache after checking to see if we're at the correct block height where we expect such interfaces to be available. This is tricky. Please remember to update the tests and see the prior art listed above. Such checks are located in the `readInitModules` function in `TransactionExec.hs`:

```haskell
    go :: TransactionM p ModuleCache
    go = do

      -- see if fungible-v2 is there
      checkCmd <- liftIO $ mkCmd "(contains \"fungible-v2\" (list-modules))"
      checkFv2 <- run "check fungible-v2" checkCmd
      hasFv2 <- case checkFv2 of
        (PLiteral (LBool b)) -> return b
        t -> die $ "got non-bool result from module read: " <> T.pack (showPretty t)

      -- see if fungible-xchain-v1 is there
      checkCmdx <- liftIO $ mkCmd "(contains \"fungible-xchain-v1\" (list-modules))"
      checkFx <- run "check fungible-xchain-v1" checkCmdx
      hasFx <- case checkFx of
        (PLiteral (LBool b)) -> return b
        t -> die $ "got non-bool result from module read: " <> T.pack (showPretty t)

      -- load modules by referencing members
      refModsCmd <- liftIO $ mkCmd $ T.intercalate " " $
        [ "coin.MINIMUM_PRECISION"
        , "ns.GUARD_SUCCESS"
        , "gas-payer-v1.GAS_PAYER"
        , "fungible-v1.account-details"] ++
        [ "fungible-v2.account-details" | hasFv2 ] ++
        [ "(let ((m:module{fungible-xchain-v1} coin)) 1)" | hasFx ]
      void $ run "load modules" refModsCmd
```

## Chainweb Version

In order to make sure the new coin contract upgrades are applied in the future, a blockheight must be chosen at which to apply the upgrades. These block heights are calculated to be at some date in the near future which coincide with the service dates decided by the chainweb leads. They are defined in `src/Chainweb/Version.hs`. The naming convention for pact upgrades as of the date of this document is of the form `chainweb<\new chainweb version number>Pact` (e.g. `chainweb214Pact`). Within these functions, we use the `AtOrAfter` construct to define behaviors across chainweb for what to do at or after a particular block height. Keep in mind that these upgrades interact with each other, and therefore the choice of blockheights for the testing and devnet environments will need to be amended strictly monotonically. Example:

```haskell
-- | Pact and coin contract changes for Chainweb 2.14
--
chainweb214Pact
    :: AtOrAfter
    -> ChainwebVersion
    -> BlockHeight
    -> Bool
chainweb214Pact aoa v h = case aoa of
    At -> go (==) v h
    After -> go (flip (>)) v h
  where
    go f Mainnet01 = f 2605663 -- 2022-04-22T00:00:00Z (service date + 24 hours)
    go f Testnet04 = f 2134331 -- 2022-04-21T12:00:00Z (service date + 12 hours)
    go f Development = f 115
    go f (FastTimedCPM g) | g == petersenChainGraph = f 30
    go f _ = f 5
```

## Testing

There are four main sites for testing coin upgrades and assessing fork compatibility: `TransactionTests`, `PactInProcApi`, `ModuleCacheOnRestart`, and live DevNet testing.

### Unit Tests

The former tests the `coin-v*.repl` test and reports a boolean pass/fail if there are any failures in the repl script. The second tests Pact execution using an in-memory Chainweb instance, and the third tests for module caching compatibility across forks and upgrades. In general, if coin upgrades and/or new interfaces are added to the Pact Service cache, `ModuleCacheOnRestart` will need a test. However, if only coin upgrades are involved, it's possible that only `PactInProcApi` needs a test. Consider the following:

- `PactInProcApi`:
  - Make sure `coin-v*` is backcompatible across 2 forks so (see: pact4coin3UpgradeTest)
    - See: https://github.com/kadena-io/chainweb-node/pull/1416
    - See: https://github.com/kadena-io/chainweb-node/pull/1413
  - It should be the case that updates are backcompatible if users start from scratch
  - Events need to be backcompatible as well, and requires a cross-fork test in order to establish backcompatibility
    - See: https://github.com/kadena-io/chainweb-node/pull/1444

- `ModuleCacheOnRestart`:
  - For examples of testing versioned module caches across forks, see: https://github.com/kadena-io/chainweb-node/pull/1433

### Live Devnet

It's good practice to assess outputs using a live chainweb instance. We have [devnet](https://github.com/kadena-io/devnet) for this purpose. Using Devnet, one may stand up a miner and chainweb node instance locally and test forks in a live environment. Instructions exist in the linked repository.
