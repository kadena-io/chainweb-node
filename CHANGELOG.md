# `chainweb-node` Changelog

## 1.1

This is a major release that turns on coin transfers. All node operators should
upgrade ASAP. If you do not upgrade, your node will stop working on December 5,
2019.

* Improve logging for orphan blocks.
  [#752](https://github.com/kadena-io/chainweb-node/pull/752)

* Finalize gas model.
  [#744](https://github.com/kadena-io/chainweb-node/pull/744)

* Updates to staged transaction rollout behavior.
  [#743](https://github.com/kadena-io/chainweb-node/pull/743)
  [#755](https://github.com/kadena-io/chainweb-node/pull/755)
  [#759](https://github.com/kadena-io/chainweb-node/pull/759)
  [#766](https://github.com/kadena-io/chainweb-node/pull/766)
  [#768](https://github.com/kadena-io/chainweb-node/pull/768)
  [#769](https://github.com/kadena-io/chainweb-node/pull/769)
  [#771](https://github.com/kadena-io/chainweb-node/pull/771)

* Improve cut DB membership queries.
  [#756](https://github.com/kadena-io/chainweb-node/pull/756)

* Better error handling for /mining/solved endpoint.
  [#762](https://github.com/kadena-io/chainweb-node/pull/762)

* Increase mempool session timeout.
  [#761](https://github.com/kadena-io/chainweb-node/pull/761)

* Mempool performance improvements
  [#732](https://github.com/kadena-io/chainweb-node/pull/732)
  [#742](https://github.com/kadena-io/chainweb-node/pull/742)

* Upgrade the testnet version from `testnet03` to `testnet04`.
  [#765](https://github.com/kadena-io/chainweb-node/pull/765)

* Better error message when gas limit exceeded.
  [#748](https://github.com/kadena-io/chainweb-node/pull/748)

* Refactor Pact service state handling and chain data.
  [#767](https://github.com/kadena-io/chainweb-node/pull/767)

* Fix bug in miner redeem on failed payloads.
  [#773](https://github.com/kadena-io/chainweb-node/pull/773)

* Set default block gas limit of 6000.
  [#776](https://github.com/kadena-io/chainweb-node/pull/776)

## 1.0.6 (2019-11-26)

This is a minor release that provides stability and performance improvements. It also
upgrades the testnet version.

Miners are advised to also upgrade to the most recent version of the mining
application. Older versions of `chainweb-miner` may experience occasional delays
of work update notifications when used with this `chainweb-node` version.

* Improves the stability of Chainweb nodes, by closing a TCP connection leak on
  nodes that had mining coordination enabled.
  [#735](https://github.com/kadena-io/chainweb-node/pull/735)

* Improve performance of Chainweb nodes, by changing the default memory
  allocation parameters of the Haskell runtime. The default, built in RTS
  settings are now `+RTS -N -A64M -H1G`.
  [#737](https://github.com/kadena-io/chainweb-node/pull/737)

* Upgrade the testnet version from `testnet02` to `testnet03`.
 [#736](https://github.com/kadena-io/chainweb-node/pull/736)


## 1.0.5 (2019-11-21)

This version changes the rules for Difficulty Adjustment in scenarios where
there is a sudden loss of over 99% of the network hash power. While this is very
unlikely to happen, this altered logic could result in a fork if not applied by
the majority of the network. **Node administrators are advised to upgrade to
this version as soon as possible.**

### Significant Changes

- Emergency difficulty adjustment is disabled from chain height 80,000 onward.
  This is technically a fork, although it will hopefully never become relevant.
  [#671](https://github.com/kadena-io/chainweb-node/pull/671)

### Configuration Changes

- Configuration options for logging blocks to Amberdata was removed.
  [#717](https://github.com/kadena-io/chainweb-node/pull/717)

### Bug Fixes

- Parsing of the configuration file properties `p2p.peer.certificateChainFile`
  and `p2p.peer.keyFile` was fixed.
  [#703](https://github.com/kadena-io/chainweb-node/pull/703)

## 1.0.4 (2019-11-13)

#### Improved Mining Configuration

Mining configuration has been consolidated into a single section:

```yaml
mining:
  coordination:
    enabled: false
    mode: private
    limit: 1200
    miners: []
```

Please update your config to the [new minimal configuration file](./minimal-config.yaml).

If you compare to the old config, you will notice that the `miningCoordination`
field has been moved.

#### Private Mining

When `enabled: true` and `mode: private`, you must provide a list of account
names into the `miners` field. Only remote clients that declare they are mining
to these blessed accounts will be able to receive work - all others will be
rejected. You can use this to protect your node from unwanted visitors.

#### Configurable Work Request Limits

The `limit` field can be set to restrict the number of mining work requests that
occur over a 5 minute period. Requests over this limit are rejected with a `503`
error code.
