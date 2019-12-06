# Recovering from deep forks

Help! My node has failed with the following fatal error:

```
Insert error message here
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

And restart the node with the following changes to `chainweb.yaml`:

```
chainweb:
  cuts:
    pruneChainDatabase: false
    initialCutHeightLimit: $cut_height_limit
```

(Here replace `$cut_height_limit` with the value computed above).

Watch the logs. If the node joins the correct fork, revert your changes to
`chainweb.yaml` (it should not be necessary to restart the node).
