# Architecture Overview

![Architecture Overview](Overview.png)

Component Colors:

*   **blue**: software components that are part of the chainweb consesus
    implementation
*   **grey**: chainweb components that are not part of the consensus layer
*   **red**, **orange**, **yellow**, **green**: represent instances of
    components that belong to different chains

Arrows represent, in a semiformal way, the flow of control. Control flow can be
synchronous or asynchronous (via callbacks and awaitable shared variables).

Bold borders indicate that a component drives control by triggering events. The
main event, that drive the global behavior, is the arrival of new block headers.
New block headers are introduced by *Miner* components and *P2P Network*
connections. Another source of external triggers is the (external) transaction
system that assembles and interprets block payloads.

## Description of APIs and Components

*   [P2P Network API](P2pNetworkApi.md)
*   [ChainDB API](ChainDbApi.md)
*   [ChainDB Synchronization API](ChainDbSyncApi.md)
*   [Transport Layer Architecture](Transport.md)
