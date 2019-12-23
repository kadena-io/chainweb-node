# `chainweb-node` Changelog

## 1.4.x

#### New Mining API

A new endpoint `/mining/stream` provides a long-living flow of new work to
compatible mining clients. The full path is as follows:

```
GET /chainweb/0.0/mainnet01/mining/stream/<chain-id>/<miner-account>
```

This is the most efficient way for a mining client or pool software to receive
new, valid work from their node. Overall strain on the node is now even less.

##### Notes for Pool Administrators and Mining Client Authors

Notice that this is a per-chain subscription. To receive new work from all 10
chains, please open a concurrent thread for each chain, and keep them open. New
work will appear in a stream a few hundred milliseconds after your node learns
about new a block from the network.

The endpoint is an HTTP **Event Stream**, and each data block comes in the
following format:

```
event:New Work
data:00000000e4501217cef33e53f85e8b306dc50b7e67b22fe73a619b4c89cca8eb6f03030000000000000000006ec5bdc0669a0500c741ad2907c1c46e5f14a29a0e0fd8cd674c2dcc1d2ded2fb4144d67e133655b030002000000d4d3e008af14b0bf9c7fd18873f2d154e50c9c9d372759f2830ae2417f93d8d503000000e7baba04e6af33161788e4af66e18470ce0e743f3256aa98425f39e3e536b6720500000059b300025dc1688810acae9597619924198ddca6536bcea4df35b72196bbfd21e4501217cef33e53f85e8b306dc50b7e67b22fe73a619b4c89cca8eb6f03030082721f0a1729038e5890d922d6bb54bc1598c9d17a5faf858ce74eb00f7e390b00000000ff62e703000000000000000000000000000000000000000000000000000000008a0000000000000005000000494d9ee4619a05000000000000000000
```

The `data` itself is Hex-encoded, to comply with the Event Stream protocol. Once
decoded, the data has the same format as given by
[`/mining/work`](https://github.com/kadena-io/chainweb-miner/blob/37019600f9187aad6ff50c8f397800bd48c7fdc8/README.org#work-requests).

For the time being, you must call this endpoint with `HTTP 1.1` or the stream
may cut off early.

The old mining endpoints `/mining/work` and `/mining/updates` still work, but
may be removed in a future release.

## 1.4 (2019-12-14)

This version replaces all previous versions. Any prior version will stop working
on **2019-12-17T00:00:00Z**. Note administrators must upgrade to this version
before that date.

This version will stop working on **2020-01-15T00:00:00Z**. Node administrators
must update to the next version before that date.

* All nodes in the miners list in the configuration file are served cached work
  [#819]

* Correct account balances that where compromised by vulnerability #797.
  The adjustment will occur in the first block with a creation time after
  2019-12-17T15:00:00Z on the respective chains. [#830]

* Avoid opening and closing the pact SQLite database after pact validation
  errors. [#817]

## 1.3.1 (2019-12-09)

* [SECURITY] fix issue with date comparison triggering block validation of fix for #797 [#810]

* Don't vacuum SQLite databases on startup [#803]

## 1.3 (2019-12-08)

CRITICAL SECURITY UPDATE [2 of 3]: addressing vulnerability #797.
All node operators need to update no later than 2019-12-10T20:00:00.

* [SECURITY] Address vulnerability 797 via precompiled statements [#798]

* Enforce lower bound on acceptable node versions [#793]

* Prune peer db and update peer selection [#788]

* Limit checkpointer rewind depth [#795]

* Improved mining coordination efficiency [#791]

Note that this change involves a breaking change to the config file.

What was previously:
```yaml
      miners:
      - foobar  # just an account name
      - cfd7816f15bd9413e5163308e18bf1b13925f3182aeac9b30ed303e8571ce997
```
Must now be:

```yaml
      miners:
      - account: foobar
        predicate: keys-all
        public-keys:
        - 3438e5bcfd086c5eeee1a2f227b7624df889773e00bd623babf5fc72c8f9aa63
      - account: cfd7816f15bd9413e5163308e18bf1b13925f3182aeac9b30ed303e8571ce997
        predicate: keys-all
        public-keys:
        - cfd7816f15bd9413e5163308e18bf1b13925f3182aeac9b30ed303e8571ce997
```

## 1.2 (2019-12-04)

CRITICAL SECURITY UPDATE [1 of 3]: postponing transfers in order to address a late-breaking vulnerability #797.
All node operators need to update by 2019-12-05T00:00:00.

* [SECURITY] Postpone transfers to 2019-12-17T01:00:00:00 UTC [#789]

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
