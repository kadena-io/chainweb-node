# Recovery Procedures

This document provides guidance on recovering from various issues that may affect your Chainweb Node.

## Recovering from Deep Forks

If your node fails with a fatal error related to reorg limits, it means your node is part of a losing fork longer than your reorg-limit, which requires manual intervention.

### Error Example

```
Fatal error: Requested rewind exceeds limit (4). Our previous cut block height: 1, fork ancestor's block height: 6.
Offending new block:
BlockHeader {_blockNonce = Nonce 0, _blockCreationTime = BlockCreationTime {_bct = Time (TimeSpan (Micros 1575672858613932))}, _blockParent = "ugAOff6YkKmvp_pJIHqx6HLTRPwyHEHD2c2Olm6L2Fg", _blockAdjacentHashes = BlockHashRecord {_getBlockHashRecord = fromList []}, _blockTarget = HashTarget (PowHashNat 115792089237316195423570985008687907853269984665640564039457584007913129639935), _blockPayloadHash = BlockPayloadHash (MerkleLogHash U-3paUKNKQUkggFmNOnGBEAJnjpy4K_-7t_dGyV0lhw), _blockChainId = ChainId 8, _blockWeight = BlockWeight (HashDifficulty (PowHashNat 1)), _blockHeight = 1, _blockChainwebVersion = fastfork-CPM-peterson, _blockEpochStart = EpochStartTime (Time (TimeSpan (Micros 0))), _blockFlags = FeatureFlags 0, _blockHash = "SKZ0mTWp08ooXsWz_yyT7v_MSrRdyKkmCyWJBTK8u3g"}

Your node is part of a losing fork longer than your reorg-limit, which
is a situation that requires manual intervention.
```

### Recovery Method A: Get DB Files from a Friendly Peer

If you can get up-to-date database files from a friendly peer or from Kadena support, this is the fastest way to catch up:

```bash
# On the remote node
ssh root@remote-node
systemctl stop chainweb
tar cvzf chainweb-db.tar.gz .local/share/chainweb-node
systemctl start chainweb
exit

# On your node
systemctl stop chainweb
rm -Rf ~/.local/share/chainweb-node
ssh root@remote-node 'cat chainweb.db.tar.gz' | tar xvzf -
systemctl start chainweb
```

### Recovery Method B: Restart Node with a Cut Height Limit

This option is slower but easier:

1. Calculate the cut height limit:
```
cut_height_limit = (height_ancestor - 50) * num_chains
```

2. Modify `chainweb-config.yaml`:
```yaml
chainweb:
  cuts:
    pruneChainDatabase: false
    initialCutHeightLimit: $cut_height_limit
```

3. Restart the node and monitor the logs.

4. If the node joins the correct fork, revert your changes to `chainweb-config.yaml`.

### Using Private P2P Mode

To ensure you catch up to a specific node only:

```yaml
chainweb:
  p2p:
    private: true
```

This disallows communication with nodes not in your bootstrap list.

## Other Recovery Procedures

For other recovery procedures, please consult the [Chainweb community](https://discord.gg/kadena) or [Kadena support](https://kadena.io/contact).
