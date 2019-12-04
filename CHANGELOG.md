# `chainweb-node` Changelog

## 1.1 (2019-12-02)

This is a major release that activates coin transfers on December 5 2019 16:00 UTC.
All node operators should upgrade ASAP.
1.0.x nodes will stop mining and fail to launch after December 5 2019 00:00 UTC.

* Improve logging for orphan blocks. [#752]

* Finalize gas model. [#744]

* Updates to staged transaction rollout behavior. Coin transactions will be
  enabled on December 5, and the ability to deploy smart contracts will be
  enabled on January 15. [#743,#755,#759,#769]

* Improve cut DB membership queries. [#756]

* Better error handling for /mining/solved endpoint. [#762]

* Increase mempool session timeout. [#761]

* Improve mempool performance. [#732, #742]

* Better error message when gas limit exceeded. [#748]

* Refactor Pact service state handling and chain data. [#767]

* Fix bug in miner redeem on failed payloads. [#773]

* Set default block gas limit of 6000. [#776]

* Tx compilation failure messages from mempool [#768]

* Pre-compiled templates for gas buy/gas redeem/coinbase operations [#771]

* Introduce configurable Pact execution parameters [#766]

## 1.0.6 (2019-11-26)

This is a minor release that provides stability and performance improvements. It also
upgrades the testnet version.

Miners are advised to also upgrade to the most recent version of the mining
application. Older versions of `chainweb-miner` may experience occasional delays
of work update notifications when used with this `chainweb-node` version.

* Improves the stability of Chainweb nodes, by closing a TCP connection leak on
  nodes that had mining coordination enabled.
  [#735]

* Improve performance of Chainweb nodes, by changing the default memory
  allocation parameters of the Haskell runtime. The default, built in RTS
  settings are now `+RTS -N -A64M -H1G`.
  [#737]

* Upgrade the testnet version from `testnet02` to `testnet03`.
 [#736]


## 1.0.5 (2019-11-21)

This version changes the rules for Difficulty Adjustment in scenarios where
there is a sudden loss of over 99% of the network hash power. While this is very
unlikely to happen, this altered logic could result in a fork if not applied by
the majority of the network. **Node administrators are advised to upgrade to
this version as soon as possible.**

### Significant Changes

- Emergency difficulty adjustment is disabled from chain height 80,000 onward.
  This is technically a fork, although it will hopefully never become relevant.
  [#671]

### Configuration Changes

- Configuration options for logging blocks to Amberdata was removed.
  [#717]

### Bug Fixes

- Parsing of the configuration file properties `p2p.peer.certificateChainFile`
  and `p2p.peer.keyFile` was fixed.
  [#703]

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
