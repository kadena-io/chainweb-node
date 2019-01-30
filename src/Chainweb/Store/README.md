# Git storage scheme

## Objects and their types

### Block representation

Block headers and payloads are represented as git blob objects. This means that
we can look up a block header based on its content-addressed block-hash.

### Leaf representation

Each block is represented by a "leaf tree" that contains a "sketch" of the
block's ancestors, indexed by block height + block-hash:

```
040000 tree 58851ae2166b3f1454193e0a7e821402dd1c1a91  32.04dd4d85f5cbf4b7d34bff444a296f89efc2d30c33d396fb6c25757e4b87d9bb
040000 tree 44abe502de17131ca4f64bcea88cc58b5d251050  64.c739918a22764c87ac849e5c5ab4a681ca6d19c2748e434e1817df436b671f81
040000 tree ade94d0b0d0204d298d96abb101f8e20e60e8e70  128.9f767b0b078d25ab01b7e887c3abe03df60943d2b7f4a6fb218e99685db3b30a
040000 tree e2043d336a4d37b18bad7f26a8f8f4a16180545c  256.e1b34673f09374bb3b6c70a5e4ca4c7bd9151a0364a2dc9fc11c58a8512e939c
....
040000 tree f0e9eef642bdde66546dafd0b2a7b75184cb18d6  1023495.5e4fb6e0605385aee583035ae0db732e485715c8d26888d2a3571a26291fb58e
040000 tree d4489ec3cc068916ef41074b304cd0247363ed31  1023496.eb24c5d08b9225976f854c34a284514785bafe85dcc12644f0299c541da0fe8d
040000 tree 9fb30655b40d036a70cf09db48e4935abc5c26ec  1023497.37f67b120e9b9f85f12ee88f013288fcf2eb059b4bc5488795071e6e7a517f74
040000 tree 391fe0b23fa795caf352d2aa71b5c95f726b7c18  1023498.902ff1dd633c66aef1853ced67cbd749716d8a388b59de5a662e14bcdfe40cd6
040000 blob 391fe0b23fa795caf352d2aa71b5c95f726b7c18  header
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
