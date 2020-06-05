# `chainweb-node` Changelog

## 1.9 (2020-06-07)

This version replaces all previous versions. Any prior version will stop working
on **2020-06-11T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-07-16T00:00:00Z**.

This release doesn't introduce new features. It contains many small bug fixes,
along with some performance and stability improvements. It also introduces some
larger behind the scenes changes in preparation of upcoming major new
functionality.

## 1.8 (2020-04-27)

This version replaces all previous versions. Any prior version will stop working
on **2020-04-30T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-06-11T00:00:00Z**.

*   Starting with block height 530500 the first eight bytes of each block must be
    set to zero. (#974)

*   The option to persist the peer database is removed. The functionality
    was rarely used and can be simulated by using the peer REST API endpoints
    along with the know peers configuration option. (#1010)

*   Removed the PUT endpoints from the `/chain/header` APIs. (#1002).

*   Make block header validation aware of adjacent parents. (#1004).

*   Increased the default block gas limit by 10x to 150,000. Note that if you
    have manually specified a block gas limit in your node config file, this
    change will not take effect. We recommend removing it and using the
    chainweb-node default. (#1019).

## 1.7 (2020-03-26)

This version replaces all previous versions. Any prior version will stop working
on **2020-04-02T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-04-30T00:00:00Z**.

The use of the first eight bytes of the mining jobs bytes as nonce is now
considered deprecated. Those bytes should be set to 0x0. Only the last eight
bytes must be used as nonce.  Miners and pools should start upgrading to the new
behavior. The new behavior will be enforced in a future version.

* Compute epoch time based on parent header. This change won't affect any
  any users of Chainweb. The change will become effective at block height
  452820. (#977)

* Validate transaction creation time and expiration with respect to creation
  of the parent block header. This change brings the checks for transaction
  validity in line with the time used during Pact validation. (#935, #942)

  There is a 90 second leniency applied for transactions to become valid. This
  change will become effective at chain block height 449940. In the time before
  the change becomes active users may experience that new nodes reject their
  transactions. This can be mitigated by using a negative offset for the
  creation of about 2 minutes. (#973)

* A new flag `--allowReadsInLocal` and configuration option was added that
  enables direct database reads of smart contract tables in local queries.
  (#938)

* Fixed a rare bug that could affect nodes during history rewinds. (#940)

* Internal CPU mining use the last eight bytes of the mining jobs bytes as nonce
  (#957)

* Add missing `cut` API to the swagger documentation of the Chainweb API (#980)

* Fixes a bug that caused Chainweb nodes to crash during startup due to running
  out of RAM. (#982)

* Beside several stability and performance improvements this release
  also greatly improves test coverage and the quality of the code base to
  support long term maintainability of the Chainweb codebase.

## 1.6 (2019-02-18)

This version replaces all previous versions. Any prior version will stop working
on **2020-02-20T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-04-02T00:00:00Z**.

* Adds enriched message body to gas payer execution (#912)

* Pact fix for enforce-one, guards runtime TC (#916)

* Gas perf improvements (#871)

* Remove custom `systemd` usage (#908)

* Mine an additional cut height in SPV tests (#889)

* Add configuration options for checking payload hashes during pact replay (#903)

* pact: poll: report when a transaction is on the bad list. (#842)

* More detailed errors when attempt buy gas fails (#893)

* Fix performance of applyRank (#892)

* improve performance of branchEntries (#885)


## 1.5 (2019-01-13)

This version replaces all previous versions. Any prior version will stop working
on **2020-01-15T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-02-20T00:00:00Z**.

* Full launch of Pact smart contract functionality (begins at
  2020-01-15T16:00:00Z)

* Increased block gas limit to 15000 (#870)

* Minimal SPV proofs are created by default (affects cross-chain transfer proof size) (#860)

* Enriched results at /poll and /listen endpoints (#866)

* Caching improved for coinbase transactions (#861)

* Improvements to node stability and node stalling issues (#844, #845)

## 1.4 (2019-12-14)

This version replaces all previous versions. Any prior version will stop working
on **2019-12-17T00:00:00Z**. Note administrators must upgrade to this version
before that date.

This version will stop working on **2020-01-15T00:00:00Z**.

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
