# `chainweb-node` Changelog

## 2.22 (2023-11-24)
This version replaces all previous versions. Any prior version will stop working
on **2023-12-13T:00:00Z**. Node administrators must upgrade to this version before
that date.

This version will expire on **2023-03-06T:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the node.

Changes:

* Updated to Pact 4.10 (numerous, see [Pact
  changelog](https://github.com/kadena-io/pact/releases/tag/v4.10))
* Node support for webauthn signers, scoped signatures, and webauthn keyset formats in Pact (#1779)
* Block endpoint added to Service API (#1720)
* Fix batch /polling so that it no longer omits results (#1775)
* Add block header to validation failure message (#1752)
* Halt block fill algorithm constructively if we exceeded the tx fetch limit (#1762)
* Be more careful not to write the results of invalid blocks to the pact state (#1740)
* Fix Mac M2 compatibility with older blocks (#1782)

Internal Changes:
* Support aeson-2.2 (#1750)
* Fix benchmarks for block creation and validation (#1767)

## 2.21 (2023-10-05)

This version replaces all previous versions. Any prior version will stop working
on **2023-10-19T00:00:00Z**. Node administrators must upgrade to this version before
that date.

This version will expire on **2023-12-13T:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the node.

Changes:

* Support for WebAuthN signatures in Pact keyset guards. (#1729, see [https://github.com/kadena-io/pact](Pact) #1139)
* Updated to Pact 4.9. (numerous, see [Pact
  changelog](https://github.com/kadena-io/pact/releases/tag/v4.9))

Internal Changes:
* Updated from tls package version 1.7.1 to 1.9. (#1734)
* Updated from base64-bytestring package version 1.0.0.3 to 1.2.1.0. (#1729)

## 2.20 (2023-08-28)

This version replaces all previous versions. Any prior version will stop working
on **2023-09-07T00:00:00Z**. Node administrators must upgrade to this version before
that date.

This version will expire on **2023-10-19T00:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the node.

Changes:

* A new chainwebVersion called fast-development, intended for use by Pact
  developers. See #1627 for more details.
* Updated to Pact 4.8. (numerous, see [Pact
  changelog](https://github.com/kadena-io/pact/releases/tag/v4.8))
* Fixed an issue where /local calls that rewind to a previous block could have
  the wrong behavior or gas usage if rewinding crosses fork boundaries. (#1700)

Internal Changes:
* Updated from GHC 8.10.7 to GHC 9.6.2. (#1565)
* PactService now emits significantly more structured logs. (#1699)

## 2.19.2 (2023-07-17)

**NOTE: THIS VERSION SUPERSEDES 2.19.1. PLEASE UPDATE AS SOON AS POSSIBLE.**

This version replaces all previous versions.

This version will expire on 2023-09-07.

To upgrade, pull the latest docker image or download the binary and restart the node.

Changes:

*   Add rewind support to /poll and /local. (#1653, #1686)
*   Add some leniency to mempool creation time checks. (#1255)
*   Return metadata from /local with preflight set to true. (#1612)
*   Optimize new block creation. (#1691)

## 2.19.1 (2023-05-22)

**NOTE: THIS VERSION SUPERSEDES 2.19. PLEASE UPDATE AS SOON AS POSSIBLE.**

This version replaces all previous versions. Node administrators must upgrade to
this version before 2023-06-01T00:00:00Z.

This version will expire on 2023-09-07.

To upgrade, pull the latest docker image or download the binary and restart the node.

Changes:

*   Disable user function return value typechecking (#1661)
*   Add typechecking option to tx-sim. (#1656)

## 2.19 (2023-05-17)

**NOTE: THIS VERSION IS OBSOLETE. IT IS REPLACED BY 2.19.1. PLEASE UPDATE AS SOON
AS POSSIBLE.**

This version replaces all previous versions. Any prior version will stop working
on 2023-06-01T00:00:00Z. Node administrators must upgrade to this version before
that date.

This version will expire on 2023-09-07.

To upgrade, pull the latest docker image or download the binary and restart the node.

Changes:

*   Support for Pact 4.7 (#1649, #1645, #1633, #1639):
    *   Pact errors are now displayed to users of the Pact /poll endpoint. Some
        Pact errors have changed and been made shorter.

Bug fixes:

*   API endpoints now more strictly comply to the API specification at
    api.chainweb.com. (#1434)
*   A small memory leak has been fixed. (#1635)

## 2.18.1 (2023-03-06)

This is a feature and bug-fix release. Upgrading is optional but recommended.

To upgrade, pull the latest docker image or download the binary and restart the
node.

All 2.18.* versions will expire on **2023-06-01T00:00:00Z**.

[Changes](https://github.com/kadena-io/chainweb-node/compare/2.18...2.18.1):

Performance Improvements:

*   Optimize JSON+base64 encoding. (#1611)
*   Use `application/octet-stream` encoding for P2P header queries. (#1619)

Miscellaneous:

*   Remove unused rate limiting configuration settings. (#1616)
*   Remove CORS support from the P2P API. (#1616)
*   Remove unused hashes and SPV endpoints from the P2P API. (#1616)
*   Tighten default P2P rate limits. (#1616)
*   Add dedicated rate limiter for Mempool requests. (#1616)
*   Disable unused `application/json;blockheader-encoding=object` in responses
    from the P2P API. (#1619)

## 2.18 (2023-03-01)

This version replaces all previous versions. Any prior version will stop working
on **2023-03-02T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will expire on **2023-06-01T00:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the
node.

[Changes](https://github.com/kadena-io/chainweb-node/compare/2.17.2...2.18):

*   New /local endpoint preflight simulation API. (#1585, #1600)
*   Support for Pact 4.6: (#1602)
    *    New ZK native function support.
    *    Better gas estimation.
*   Warning deprecation system support in /local:
    *    Allows the node to provide warnings for upcoming feature deprecations in Pact.
    *    The same warnings are generated by the repl in Pact 4.6.
*   Internal changes to support future chain database schema changes.
*   Remove libtbb as a dependency.

Bug fixes:

*   Filter Module Cache for just `coin` contract. (#1548)
*   Prevent table name clashes in module. (#1556)
*   Full chain replay is now possible on Intel Mac & Linux, and M1 Mac.
When upgrading directly from chainweb-node version 2.17, please, also take a look at the changes in versions 2.17.1 and 2.17.2 below.

## 2.17.2 (2022-12-22)

This is a feature and bug-fix release. Upgrading is optional but recommended.

To upgrade, pull the latest docker image or download the binary and restart the
node.

All 2.17* versions expire on **2023-03-02T00:00:00Z**.

[Changes](https://github.com/kadena-io/chainweb-node/compare/2.17.1...2.17.2):

Logging and Telemetry Changes:

*   Add telemetry logging for Database size. (#1330)
*   Make Pact service log asynchronous exceptions with log-level `warn` and not
    `error`. (#1562)
*   Log replay height based on time, not blocks. (#1563)
*   Add telemetry logging for node top-level status. (#1561)

Performance Improvements:

*   Add module cache to checkpointer. (#1577)

Bug Fixes:

*   `withSavepoint` now catches `SomeAsyncException`. (#1576)
*   Fix transfer cost for Rosetta transaction generator. (#1579)

Miscellaneous:

*   Censor `BackupConfig` from `config` endpoint. (#1569)
*   Simulate whole block(s) in transaction simulator. (#1573)
*   Fix some command line help messages. (#1574)

## 2.17.1 (2022-12-02)

This is a feature and bug-fix release. Upgrading is optional but recommended.

To upgrade, pull the latest docker image or download the binary and restart the
node.

All 2.17* versions expire on **2023-03-02T00:00:00Z**.

[Changes](https://github.com/kadena-io/chainweb-node/compare/2.17...2.17.1):


Bug fixes:

*   Fix `initialHeightLimit` CLI argument parsing to not override config file.
    (#1566)
*   Fix cut GET endpoint height limiting. (#1571)

Miscellaneous:

*   Add transaction simulator to cwtools. (#1558)

## 2.17 (2022-11-17)

This version replaces all previous versions. Any prior version will stop working
on **2022-12-01T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will expire on **2023-03-02T00:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the
node.

[Changes](https://github.com/kadena-io/chainweb-node/compare/2.16.1...2.17):

*   Remove error messages from pact output for on-chain transactions. Dapps can
    still retrieve transaction error messages from the `local` endpoint. (#1543)
*   Implement a per-tx timeout during creation of new blocks to prevent mining
    nodes from stalling when block creation takes too long. (#1546)
*   Miscellaneous changes for fine tuning of the gas model. (#1554)

Bug fixes:

*   Filter Module Cache for just `coin` contract. (#1548)
*   Prevent table name clashes in module. (#1556)

## 2.16.1 (2022-11-07)

This is a feature and bug-fix release. Upgrading is optional but recommended.

To upgrade, pull the latest docker image or download the binary and restart the
node.

**NOTE**: This release upgrades the version of RocksDB. After upgrading to this
version previous versions of chainweb-node will not be able to open the
database. Additionally, when using the ubuntu binaries the set of required
system dependencies changes. For details please see the [release
nodes](https://github.com/kadena-io/chainweb-node/releases/tag/2.16.1).

Changes:

*   Upgrade RocksDB version (#1394)
*   Support for partial replays (#1524)
*   Allow enabling gas logs from configuration (#1525)
*   Reduce volume of info level logging (#1526)
*   Check file descriptor rlimit on startup (#1532)

## 2.16 (2022-08-23)

This version replaces all previous versions. Any prior version will stop working
on **2022-09-01T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will expire on **2022-12-01T00:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the
node.

[Changes](https://github.com/kadena-io/chainweb-node/compare/2.15...2.16):

*   Upgrade to Pact 4.4. This release brings namespaced keysets. For further changes check the [Pact Changelog](https://github.com/kadena-io/pact/blob/master/CHANGELOG.md).
*   Improve performance during catchup. (#1473, #1474, #1476)
*   Improve P2P protocol performance. (#1502)
*   Improve TLS session management. (#1489)
*   Faster and safer binary serialization across the board. (#1494)

Bug Fixes:

*   Fix some potential race conditions in SQLite interface code. (#1477)

## 2.15 (2022-06-09)

This version replaces all previous versions. Any prior version will stop working
on **2022-06-16T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will expire on **2022-09-01T00:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the
node.

Changes:

*   Upgrade to Pact 4.3.1. The release contains miscellaneous performance
    improvements and bug fixes. (#1448)
*   Upgrade to Coin V5. This adds the new `TRANSFER_XCHAIN_RECD` event that is
    emitted when the funds of a cross-chain transfer are redeemed on the target
    chain. (#1444)
*   Support resetting chainweb-node to a lower block height at startup. (#1344)

Bug Fixes:

*   Fix a pact module cache issue that could occasionally result in corrupted
    databases. (#1430)

## 2.14.1 (2022-05-16)

This is a feature and bug-fix release. Upgrading is optional but recommended.

Unlike mandatory service releases, optional releases can be rolled back in case
of an issue with the release. Optional releases are therefore well suited for
early integration and testing of new chainweb-node versions.

To upgrade, pull the latest docker image or download the binary and restart the
node.

Changes:

*   Improve performance of branch queries in some cases (#1431)
*   Make upper and lower bounds in branch APIs optional (#1432)
*   Make the payload API batch limit configurable on the service API.
    Restrict the payload batch limit on the P2P API to 50 items. (#1433)
*   More robust block refill logic (#1437)

## 2.14 (2022-04-11)

This version replaces all previous versions. Any prior version will stop working
on **2022-04-21T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2022-06-16T00:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the node.

Changes:

*   Improve Mempool to fill blocks more efficiently (#1399)

*   Pact Changes (#1382, #1396, #1397, #1399, #1407, #1409,#1410, #1413, #1414, #1417):

    *   Gas changes for integer and decimal operations.
    *   `NaN` and `+/- Infinity` throw errors now.
    *   Several other nonsensical arithmetic expressions and operations throw
        errors now.
    *   Support of nested Defpacts and native `continue`.
    *   New natives `create-principal` and `validate-principal`
    *   Add support for principals`r:`, `m:`, `u:`, `p:`, and `w:` in `coin`.
    *   Addition of `fungible-xchain-v1` interface, which provides a
        `TRANSFER_XCHAIN` managed capability.
    *   Implement `fungible-xchain-v1` in `coin`.
    *   Miscellaneous bug fixes
    *   Various performance improvements, in particular for deeply nested
        function calls.

## 2.13.1 (2022-04-01)

This is a feature and bug-fix release. Upgrading is optional but recommended.

Unlike mandatory service releases, optional releases can be rolled back in case
of an issue with the release. Optional releases are therefore well suited for
early integration and testing of new chainweb-node versions.

To upgrade, pull the latest docker image or download the binary and restart the
node.

Changes:

*   Restrict HTTP request body sizes for all API endpoints to 2MB. (#1385)
*   Periodically prune old cuts from the RocksDb database and store current cuts
    less often. This saves up to 30% disk space. (#1342, #1388)
*   Set default P2P port to 1789. (#1389)
*   Add the telemetry/logger type to log messages. (#1401)
*   Add new optional endpoints `/make-backup` and `/check-backup` to the service
    API. When enabled these endpoints can be used to trigger the creation of
    backups of the chainweb-node databases. Further details can be found
    in the [Chainweb API documentation](https://api.chainweb.com). (#1359, #1387)

Bug fixes:

*   Remove spurious warning when the hostname is configured as `0.0.0.0`. (#1389)
*   Fix typo in list of reserved IP addresses. (#1398)

## 2.13 (2022-02-18)

This version replaces all previous versions. Any prior version will stop working
on **2022-02-24T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2022-04-21T00:00:00Z**.

To upgrade, pull the latest docker image or download the binary and restart the node.

Changes:

*   [Pact] Enforce miner keyset formats. (#1314)
*   [Pact] Fix Pact parser to always consume all input. (#1358)
*   [Pact] Improve gas cost calculations. (#1361,#1369)

*   [Rosetta] Chainweb Rosetta API 2.0.0 (#1145)

    New Features
    *   Adds Construction API endpoints for Rosetta version 1.4.4 as defined
        here: https://www.rosetta-api.org/docs/1.4.4/ConstructionApi.html.
       *   For this version of the Rosetta Construction API , only transfer operations with k:accounts are
        supported.

    Backwards Incompatibility
    *   A Rosetta Operation's metadata no longer returns the Operation's "tx-id" or its
        account's "total-balance". The Construction API declares intended
        Operations and looks for these operations in blocks. There is no way for
        the Construction API to know the "tx-id" and "total-balance" when
        creating the intended operations.

## 2.12.2 (2022-02-04)

This is a new feature and bug fix release for the `Chainweb Rosetta API 1.0.0`
([#1135](https://github.com/kadena-io/chainweb-node/pull/1135)).

To upgrade, pull the latest docker image or download the binary and restart the
node.

[Rosetta] Bug Fixes
*   After the fork to Pact 4.2.0, the rosetta /block and /account/balance
*   endpoints returned `TxLogs not parsable`. This version fixes this.

[Rosetta] Backwards Incompatibility
*   Adds related transaction (continuations) information to the /block
    endpoints.
*   Adds related operation information to /block endpoints. Gas operations are
    now linked together, while coin-table operations are linked to each other in
    the order they appear.
*   Improved parsing of remediation logs for blocks containing coin v2 and 20
    chain forking transactions. Previously, the logs in the coin v2 block were
    all grouped together into a single request key, but now it will show the
    different request keys of the remediation transactions.
*   Re-enabled metadatas. Rosetta testing tooling used to error out when
    submitting unstructured JSON. This bug has seen been fixed.

[Rosetta] Minor Improvments
*   Introduces the `rosettaImplementationVersion` value to denote changes in the
    internal implementation of the Rosetta API.
*   Adds internal metadata data types to facilitate documenting and expanding
    the metadatas used by the Rosetta endpoints.

## 2.12.1 (2022-01-22)

This is a bug fix release. Upgrading is highly encouraged. In particular, if you
experience issues with chainweb-node version 2.12, upgrading to version 2.12.1
will likely resolve those issues.

To upgrade, pull the latest docker image or download the binary and restart the
node.

This version will expire on 2022-02-24T00:00:00Z.

Changes:

* Correct mempool tx persistence, check tx details in validate (#1348)

* Remove deprecated configuration options (#1345)

## 2.12 (2022-01-07)

This version replaces all previous versions. Any prior version will stop working
on **2022-01-13T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2022-02-24T00:00:00Z**.

Changes:

*   Upgrade Pact version to 4.2.0. (#1323)

*   The default setting for pruning the chainweb database has been changed to
    "none" to reduce unnecessary work and speed up node start times. (#1332)

*   Nodes will now take a snapshot of the chainweb database (RocksDB) if sent
    the SIGUSR1 signal. These snapshots can be used for backups and do not take
    significant space until the database has diverged significantly from the
    snapshot. Also, SIGUSR2 no longer terminates the node. (#1328)

*   Nodes will log their progress while pruning the database. (#1315)

*   The Pact queue has been instrumented to log its utilization level and
    latency. (#1284)

## 2.11.1 (2021-11-23)

This is a minor feature release. It is compatible with version 2.11. Upgrading
optional. Please, check the list of changes before upgrading.

Changes:

*   New command line options for configuring mining coordination. The options
    `--enable-mining-coordination --mining-public-key=<PUBLIC_KEY>` enable the
    mining API of a node and configure `k:<PUBLIC_KEY>` as the account that
    receives mining rewards. (#1311)

*   Include GET endpoints for cuts, headers, branches, and payloads
    into the service API. (#1309)

*   Add configuration for stopping the node after synchronizing the
    Pact state to the chain database and before starting connecting to
    the P2P network. This is useful to initializing the Pact database
    for a new node or validating an existing database. (#1312)

*   Remove rate limiting support for endpoints of the service API.
    Rate limiting for the service API should be done by using an external
    reverse proxy. (#1300)

*   Log filter rules allow fine grained support for controlling
    which messages are actually submitted. This version adds the ability to
    specify for a filter rule a probability with which a log messages passes the
    respective filter rule. This allows to emit only a certain percentage of
    message of some kind to the backend. (#1300)

## 2.11 (2021-11-09)

This version replaces all previous versions. Any prior version will stop working
on **2021-11-18T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2022-01-13T00:00:00Z**.

Changes:

*    Add priorities to the pact services queue. This gives consensus and
     new block requests priority over requests from the service APIs and the
     mempool. It makes nodes more resilient under load. (#1280)

*    Upgrade Pact version to 4.1.2. (#1286, #1287)

*    Enforce keyset formats. (#1287)

*    A new configuration option `chainweb.minGasPrice` (`--min-gas-price`) is
     added that configures a minimum gas price for transactions. The mempool
     will reject any transactions that doesn't pay at least this amount for
     gas. This allows node operators to enforce mindful usage of resources
     even when the majority of blocks isn't full. (#1282)

     The default minimum gas limit is raised from 1e-12 to 1e-8.

*    Chainweb node now depends on the OpenSSL library being installed on
     the system. (Before it already depended on the OpenSSL root certificates
     being available.)

## 2.10 (2021-10-07)

This version replaces all previous versions. Any prior version will stop working
on **2021-10-14T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2021-11-18T00:00:00Z**.

There are no changes in this version.

## 2.9.2 (2021-09-16)

This is a bug fix release. It is recommended that node operators upgrade their
nodes.

This version is fully compatible with previous versions.

Changes:

*   Fix a bug where API requests return result pages with more than the upper
    limit of items. (#1271)

## 2.9.1 (2021-08-27)

This is a bug fix release. It is recommended that node operators
upgrade their nodes.

This version is fully compatible with previous versions.

Changes:

*   Fix a bug that causes mempools to ignore new transactions after receiving
    10000 transactions on a chain. (#1267)

## 2.9 (2021-08-12)

This version replaces all previous versions. Any prior version will stop working
on **2021-08-19T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2021-10-14T00:00:00Z**.

Changes:

This is a maintenance release without breaking changes.

*   Use `0.0.0.0` as default P2P host address, which enables auto-detection of
    the IP address of the node. (#1245)
*   Build and link Pact without CLI tools support. (#1246)
*   Limit batch size of payload REST API requests 1000 items. (#1258)
*   Removed several external dependencies from the code base.

## 2.8 (2021-06-05)

This version replaces all previous versions. Any prior version will stop working
on **2021-06-17T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2021-08-19T00:00:00Z**.

Changes:

* `coin` v3 and Pact 4.0 upgrade (#1218, #1237, #1236, #1234)
  * Emits `coin.TRANSFER` events for all balance changing operations.
    Burns/creates/allocations are indicating using the _null account_ (`""`).
    Miner rewards, gas payments, allocations, and cross-chain.
  * Chainweb account protocols: reserves new account names with the format
    `c:data` where `c` is a single-char protocol identifier and `data`
    protocol-specified data.
  * Introduces the Chainweb single-key protocol `k`
    where `data` must match a single ED-25519 public key.
  * Leverages Pact 4.0
    * `X_YIELD` and `X_RESUME` event emission.
    * `bless`es previous module hash so that in-progress cross-chain
      transfers can succeed.
  * Transactional module init cache. (#1236)

* P2P API endpoint to get node config (#1226)

* Bugfixes and cleanups (#1235, #1228, #1227, #1225)



## 2.7 (2021-04-29)

This version replaces all previous versions. Any prior version will stop working
on **2021-05-06T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2021-06-17T00:00:00Z**.

Changes:

*   Improve P2P networking configuration. (#1174)
    *   Re-add builtin bootstrap nodes. This also means that default bootstrap
        nodes will always be used as long as `--ignore-boostrap-nodes` (or the
        respective configuration file setting) is not enabled.
    *   Add `X-Peer-Addr` response header that allows nodes to auto-discover
        their external network configuration.
    *   Enable chainweb-node to auto-configure the hostname. This eliminates the
        need to use a (centralized) third party service for that.
    *   Validate P2P configuration on startup.
    *   Validate peer configuration on startup.
    *   Check that a chainweb-node can connect with a configurable portion of
        the known-peers and bootstrap nodes at startup. The portion can be
        configured via the `--bootstrap-reachability` option or the
        `chainweb.p2p.bootstrapReachability` setting. The value is a number
        between 0 and 1. If it is 0 the reachability test is disabled.

*   Remove deprecated mining coordination code. (#1177)
    *   Removes support for public mining.
    *   Fix two race conditions in the mining API that may have slightly increased
        the number blocks that got orphaned before being included on the chain.

*   Internal infrastructure to support bridging KDA to other networks (#1210)

*   New OpenAPI 3.0 specification of the chainweb-node API. The API
    documentation is maintained in the git repository
    https://github.com/kadena-io/chainweb-openapi. Is published at
    https://api.chainweb.com/openapi.

## 2.6 (2021-03-18)

This version replaces all previous versions. Any prior version will stop working
on **2021-03-25T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2021-05-06T00:00:00Z**.

Changes:

*   Increase default listen timeout to 3 minutes (#1208)
*   Additional verification for coin contract (#1200)

Additional changes support the build of fully statically linked binaries that
can be used with Alpine Linux.

## 2.5 (2021-02-17)

This version replaces all previous versions. Any prior version will stop working
on **2021-02-25T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2021-03-25T00:00:00Z**.

If chainweb-node-2.5 is started with existing configuration files, the P2P
API remains unchanged. The service API endpoints will be available via HTTP on
port 1848. Please read the description below for details about this change.

Breaking changes:

*   Publish service APIs on separate HTTP port (#1063)

    The API of the chainweb-node is split into two separate sub-APIs which are
    served on two different ports:

    *   **P2P API**: this API includes all endpoint that are used in the P2P
        inter-node communication. It is exported using HTTPS and must be
        available on a publicly reachable port.

        The command line option for the this APIs are prefixed with `p2p`:

        `--p2p-host`, `--p2p-port`, `--p2p-interface`, and the respective
        certificate related options.

        The respective properties in in the configuration file are unchanged.

    *   **Service API**: this API includes all endpoints that are not directly
        used for P2P inter-node communication. These include the `pact`, `rosetta`,
        `mining`, `header-stream`, `info`, and `health-check` endpoints. This
        API is exported as plain HTTP and can thus be easily used along with a
        reverse proxy. There is no need to make this API publicly available.

        This API is configured with the configuration options `--service-port`
        (default 1848) and `--service-interface` (default '*'). The respective
        properties in the configuration file are `chainweb.serviceApi.port` and
        `chainweb.serviceApi.interface`.

    IMPORTANT: The previously used options `--hostname`, `--port`,
    `--interface`, and the related certificate options got removed. Please, use
    the new variants instead which are prefixed with the respective API as
    described above.

New features:

*   New command line option `--print-config-as=[full|minimal|diff]`. `full`
    prints a complete configuration file with all options; `minimal` prints a
    file that includes only those options that differ from the default values;
    `diff` prints the difference between the default configuration and the
    actual configuration (#1193)
*   Add payload batch APIs (#1192)

Miscellaneous changes:

*   Use TLS session manager (#1173)
*   Upgrade to a new version 0.7.5 of the http-client library (#1191)
*   Build TLS with pclmulqdq support switched on (#1193)

## 2.4 (2021-01-11)

This version replaces all previous versions. Any prior version will stop working
on **2021-01-14T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2021-02-25T00:00:00Z**.

 *  Ethereum bridge support
   *  add endpoint for creating eth-receipt-proofs (#1181)
   *  ethereum receipt SPV (#1179)
 *  Improve TXOUT SPV to include events (#1178)
 *  Pact replay bug fix (#1172,#1169,#1168)

## 2.3 (2020-11-11)

This version replaces all previous versions. Any prior version will stop working
on **2020-11-19T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2021-01-14T00:00:00Z**.

*   Support for Pact Events (#1157)
*   **Upgrade to Pact 3.7.** This includes an API change to the `pact` endpoints for events. For further information see the [Pact readthedocs](https://pact-language.readthedocs.io/en/latest/pact-reference.html#events-1). (#1157,#1158)

## 2.2 (2020-10-08)

This version replaces all previous versions. Any prior version will stop working
on **2020-10-15T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-11-19T00:00:00Z**.

*   Upgrade to Rosetta version 1.4.4 (#1149)

*   Adjust the default API request rate limits to better match the expected
    networking loads for 20 chains. This reduces overhead due to HTTP responses
    with status 429 and improves catchup performance. (#1152)

## 2.1.1 (2020-09-17)

This release provides performance improvements and bug fixes.

*   More rigorously check the correct length of request keys,
    transactions ids, and hashes in API requests. (#1139, #1140)

*   Fix a bug in the Mempool where transactions that have been included in a
    block are still marked as pending. Those transactions occupied memory in the
    Mempool and got synchronized between nodes until they finally expired.
    (#1138)

*   The database pruning at node startup is more efficient and can now
    operate in four different modes, which can be used on the command line with
    the `--prune-chain-database` option and in the configuration file with the
    property `chainweb.cuts.pruneChainDatabase`.

    *   `none` no database pruning is performed
    *   `headers` only block headers but no payloads are pruned (10-30 seconds)
    *   `headers-checked` like `headers` but also validates all block headers and
        checks the complete chain Merkle tree including payload hashes.
        (5-20 minutes)
    *   `full` prunes block headers and payloads. (10-20 minutes)

    The default is `headers`. For backward compatibility it is possible to also
    use Boolean values for setting `chainweb.cuts.prunChainDatabase` in the
    configuration file, where `false` is equivalent with `none` and `true` is
    equivalent with `headers`. (#1132)

*   Improved performance of rebuilding the pact database from the
    chain data. (#1137)

## 2.1 (2020-08-11)

This version replaces all previous versions. Any prior version will stop working
on **2020-08-13T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-10-15T00:00:00Z**.

*Forks:*

This version includes the fork that adds 10 additional chains to Chainweb
resulting in 20 chains in total. The fork will occur on

*   Mainnet at block height 852,054, which is expected to happen around
    2020-08-20 16:55:14 UTC.

The mining API of chainweb-node will start serving work items for the new chains
starting at above block heights for the respective network. The mempool and pact
service APIs will accept requests already some time before the transition. POW
difficulties will re-adjust within a few hours after the transition. During that
time block rates may be slightly higher or lower than usual.

*Possibly Breaking Changes:*

*   Fix the database location and layout when a custom location is configured.
    (#1128)

    This only affects users who configured custom database locations.

    This is a potentially breaking change. The chainweb node tries hard to
    adjust the database location by itself without user intervention. If you
    have tooling that depends on a custom database location, you may want check
    the logs at first start up and double check that everything works as expected.

*   Deprecate the use of the node-id configuration. (#1128)

    This only affects users who configured the node-id either on the command line or
    in the configuration files.

    Any node-id settings are now ignored. In particular the database locations
    doesn't include the node-id any more and just defaults to `0/rocksDb` and
    `0/sqlitedb`.

*Other Changes*:

*   Fix a bug where Chainweb node would fail to start when it stopped after
    the last block that pact evaluated got orphaned. (#1123)
*   Improve failure response for invalid solved work. (#1126)
*   Fix mainnet coin v2 upgrade for new chains. (#1130)

## 2.0 (2020-07-11)

This version replaces all previous versions. Any prior version will stop working
on **2020-07-16T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-08-13T00:00:00Z**.

This version includes the fork that adds 10 additional chains to Chainweb
resulting in 20 chains in total. The fork will occur on

*   Testnet at block height 332,604, which is expect to happen around
    2020-07-28 16:00:00 UTC, and
*   Mainnet at block height 852,054, which is expected to happen around
    2020-08-20 16:00:00 UTC.

The mining API of chainweb-node will start serving work items for the new chains
starting at above block heights for the respective network. The mempool and pact
service APIs will accept requests already some time before the transition. POW
difficulties will re-adjust within a few hours after the transition. During that
time block rates may be slightly higher or lower than usual. Node operators and
miners are encouraged to participate in the transition of Testnet as a dress
rehearsal for the mainnet transition.

Other changes:

*   Full support for [Rosetta](https://www.rosetta-api.org).
    (#1050, #1077, #1079, #1093)
*   Support for Pact continuations in `local` queries. (#1080)
*   Make Pact service more stable and fix a memory leak. (#1104, #1100)
*   Fix a Pact bug related to module namespaces. (#1107)
*   Make `info` endpoint show currently in use chains. (#1099)
*   An improved difficulty adjustment algorithm to support the graph transition
    and improve overall performance. Some research background regarding the new
    algorithm can be found [in this document](https://github.com/larskuhtz/ChainwebSimulations/blob/master/Results.ipynb).
    (#1075)

Beside these major changes, the release includes several smaller bug fixes and
performance improvements.


## 1.9 (2020-06-07)

This version replaces all previous versions. Any prior version will stop working
on **2020-06-11T00:00:00Z**. Node administrators must upgrade to this version
before that date.

This version will stop working on **2020-07-16T00:00:00Z**.

This release doesn't introduce new features. It contains many small bug fixes,
along with some performance and stability improvements. It also introduces some
larger behind the scenes changes in preparation of upcoming major new
functionality. User-facing bug fixes and improvements are listed below.
* Improve reliability of `local` API calls by using cached header data instead of a potentially failing lookup causing spurious `TreeDbKeyNotFoundException` failures (#1062)
* Provide clean shutdown via SIGTERM (#1052)
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
