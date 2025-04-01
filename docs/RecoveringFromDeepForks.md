# Recovering from deep forks

Help! My node has failed with the following fatal error:

```
Fatal error: Requested rewind exceeds limit (4). Our previous cut block height: 1, fork ancestor's block height: 6.
Offending new block:
BlockHeader {_blockNonce = Nonce 0, _blockCreationTime = BlockCreationTime {_bct = Time (TimeSpan (Micros 1575672858613932))}, _blockParent = "ugAOff6YkKmvp_pJIHqx6HLTRPwyHEHD2c2Olm6L2Fg", _blockAdjacentHashes = BlockHashRecord {_getBlockHashRecord = fromList []}, _blockTarget = HashTarget (PowHashNat 115792089237316195423570985008687907853269984665640564039457584007913129639935), _blockPayloadHash = BlockPayloadHash (MerkleLogHash U-3paUKNKQUkggFmNOnGBEAJnjpy4K_-7t_dGyV0lhw), _blockChainId = ChainId 8, _blockWeight = BlockWeight (HashDifficulty (PowHashNat 1)), _blockHeight = 1, _blockChainwebVersion = fastfork-CPM-petersen, _blockEpochStart = EpochStartTime (Time (TimeSpan (Micros 0))), _blockFlags = FeatureFlags 0, _blockHash = "SKZ0mTWp08ooXsWz_yyT7v_MSrRdyKkmCyWJBTK8u3g"}

Your node is part of a losing fork longer than your reorg-limit, which
is a situation that requires manual intervention.
For information on recovering from this, please consult:
    https://github.com/kadena-io/chainweb-node/blob/master/docs/RecoveringFromDeepForks.md
```

This means that a peer sent our node a new cut, weightier than our current one,
that has a greatest common ancestor of our current cut that is more than
`reorg-limit` blocks behind:


```
                     (htA, hashA)
   ... history ... - o - o - o - ... - o
                     |                 (htOld, hashOld)
                     |
                     ` - o - o - ... - o
                                       (htNew, hashNew)
```

Here the node will fail with a fatal error if `(htOld - htA) >= reorg-limit`,
because huge forks like this are very unlikely absent some software problem or
attack situation, both requiring operator intervention.

## Recovery method A: get DB files from a friendly peer

If you can get up-to-date database files for the blockchain from a friendly
peer or from Kadena support, this may be the fastest way to catch up a lagging
or forked node.

```
our-node# ssh root@remote-node
remote-node# systemctl stop chainweb
remote-node# tar cvzf chainweb-db.tar.gz .local/share/chainweb-node
remote-node# systemctl start chainweb
remote-node# exit
our-node# systemctl stop chainweb
our-node# rm -Rf ~/.local/share/chainweb-node
our-node# ssh root@remote-node 'cat chainweb.db.tar.gz' | tar xvzf -
our-node# systemctl start chainweb
```

## Recovery method B: restart node with a cut height limit

This option is slower, but probably safer/easier. The error message will report
an ancestor blockheight on a particular chain; your goal is to select a
cumulative cut height that will rewind us behind this block. Select:

```
cut_height_limit = (height_ancestor - 50) * num_chains
```

And restart the node with the following changes to `chainweb-config.yaml`:

```
chainweb:
  cuts:
    pruneChainDatabase: false
    initialCutHeightLimit: $cut_height_limit
```

(Here replace `$cut_height_limit` with the value computed above).

Watch the logs. If the node joins the correct fork, revert your changes to
`chainweb-config.yaml` (it should not be necessary to restart the node).


### Using `p2p.private = true`

If you want to make sure you catch up to a specific node only, you can set the
`private` flag in `chainweb-config.yaml`:

```
chainweb:
  p2p:
    private: true
```

This will disallow nodes that are not in your bootstrap list from communicating
with you, so that you can catch up faster.


### Using `chainweb-forks`

The [chainweb-forks](https://github.com/kadena-community/chainweb-forks) tool
may also be used to debug/diagnose the fork between two nodes.

```
TODO: chainweb-forks example
```
