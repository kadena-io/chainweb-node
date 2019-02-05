# Git storage scheme

## Objects and their types

### Block representation

Block headers and payloads are represented as git blob objects. This means that
we can look up a block header based on its content-addressed block-hash.

### Leaf representation

Each block is represented by a "leaf tree" that contains a "sketch" of the
block's ancestors, indexed by block height + block-hash:

```
040000 tree 27de12feda6ab7fa896055cf39ff2128f0766241	0000000000000020.Hc-ixI8lSPr39uzlsO_qbFv52veiJhlL_ozLY1F_2hY
040000 tree e741ab27aaadea065522fb551d33c291d3d56673	0000000000000040.8lqxArVy0fw_mv5LPv94Qrek0DT8zL5miH_LF9mCvRM
040000 tree 0e3eaf97d334f56ba541fe2f25762dab2eeaa2cc	0000000000000080.H_hTtC-oj8KpiY5weioJj6Tkn4pI-6EnIvwelfrQWO8
040000 tree 01d9937902fe60bf659c24470a65fb0f73d26ea8	0000000000000100.PwntUkxmI2pTGlI0N9Gdcwlhb94WSdO9HY_CQXtyaoU
040000 tree 1a0b81b63fcbb4ddc11fe26a7011f6c26f9a37de	0000000000000200.283narJfQAlan8rxS6AbjdZNzUGw_gB7i6cx1fubxoc
040000 tree f058d1eeaae84b330548e294bddc544af06df1ac	0000000000000223.Y6kaaCaqjH4GPVa11fqxQy4ExAArhFJa-Mn8WH-TO2Q
040000 tree 408a361f16b4646ba2a90a3b0e37ce81c431b273	0000000000000224.ZNmQ_B2aoNzSVlKuveTDaqiiBbV0LMO3W8fFLCo3JRk
040000 tree d9a6ae67f3901e4897e1479c8b7301f40db74341	0000000000000225.anBlI3nJdvXj6CtX9Hck5sLm4EzL0A_EUiFkcUzu3L8
040000 tree 186102a53e8237a69b79b2b8681d95f3f5521cbc	0000000000000226.hS1iKaOJescYa_N7v9STKiVUDgJfOP6ZHW0Mo7dpWV0
100644 blob 3c5e7929245e1fb0d31478ee0dc43100c75bde21	0000000000000227.KXfCOA1XzVcNVERuu0yRE8TxXii6FudWYGjWaADDZ6A
```

Here each of the linked `tree` objects points to another block's leaf tree.
These trees will be delta-compressed by `git pack` -- at each addition of a
block header, we will only be adding or removing a small number of entries to
the sketch table. Ancestor search should be `O(log(n))` if we keep K recent
blocks plus this spectrum. (FIXME: PROOF)

### Refs

Refs (tags) identify leaf tree objects.

| Tag Type     | Locations         |
|--------------|-------------------|
| Leaf         | `refs/tags/leaf/` |
| Block Header | `refs/tags/bh/`   |

### Garbage collection

Periodically the refs must be walked to remove leaf links that are not going to
make it onto the longest chain.
