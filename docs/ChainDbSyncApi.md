# Single Chain Block Header Synchronization API

The single chain synchronization API defines the interface for components that
synchronize the local copy of a single chain block header database over a
P2P network with the copies of the block header database of remote chain nodes.

An implementation of this API provides `syncSession` function that, given a logging
function, and a `ChainDB` handle, returns a `P2PSession`:

```haskell
syncSession
    ∷ ∀ m
    . MonadMask m
    ⇒ MonadAsync m
    ⇒ MonadIO m
    ⇒ (LogLevel → T.Text → m ())
    → ChainDb
    → P2pConnection m
    → m ()
```

The `P2PSession` can then be given to an instance of a `P2P.Node` which will
establish connections to other nodes in the P2P network and run the session
which will synchronize the data bases.

The `syncSession` function is defined in the file
`chaindb/signatures/Chainweb/ChainDB/SyncSession.hsig`. A prototype for a sync
session is implemented in the file
`chaindb/src/Chainweb/ChainDB/Sync/Trivial.hs`.


