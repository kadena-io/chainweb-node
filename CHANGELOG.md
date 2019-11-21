# `chainweb-node` Changelog

## 1.0.5

### Configuration Changes

*   Configuration options for logging blocks to Amberdata got removed.
    [#717](https://github.com/kadena-io/chainweb-node/pull/717)

### Bug Fixes

*   Parsing of the configuration file properties
    `p2p.peer.certificateChainFile` and `p2p.peer.keyFile` got fixed.
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
