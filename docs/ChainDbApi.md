# Single Chain Database API

The single chain database API defines the interface to query and update all
block headers for a local copy of single block chain. It uses a data model
where the block headers are organized in a content addressed, monotonically
increasing, rooted, and ranked tree.

The interface provides

*   the list of branches of the tree represented by the keys of the leave
    nodes,
*   a rank function that returns for each node in the tree its distance from
    the root,
*   a parent relation that allows to traverse the tree from the leaves to
    the root,
*   a children relation that allows to traverse the tree in the direction
    from the root to the leaves,
*   an methods to add new children to a node in the tree, and
*   an infinite updates stream, that allows to traverse the tree in an arbitrary
    but deterministic order that is compatible with the rank of the tree nodes
    and that can be used to await newly added tree nodes.

The interface also provides functions for binary encoding and decoding tree
nodes.

The `ChainDB` interface is defined in the file
`chaindb/signatures/Chainweb/ChainDB.hsig`. A prototype that is based on an in-memory
`HashMap` is implemented in the file `chaindb/src/Chainweb/ChainDB/HashMap.hs`. An
instantiation of tree entries with the `Chainweb.BlockHeader` type is provided
in the file `chaindb/src/Chainweb/ChainDB/Entry/BlockHeader.hs`. This separation allows
changing the `BlockHeader` type without affecting components that rely on the
`ChainDB` interface. Once the definition chainweb data-types has stabilized this
level of indirection may be removed.

