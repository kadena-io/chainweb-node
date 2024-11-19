-- |
-- Module: Chainweb.PayloadProvider.Initialization
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.Initialization
( Resources(..)
) where

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Version
import Chainweb.Mempool.Mempool
import Chainweb.Payload.PayloadStore

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Globally initialized resources that are needed by payload providers.
--
-- Note that not all payload providers require all of these resources.
--
data Resources logger = Resources
    { _resourceChainwebVersion :: !ChainwebVersion
    , _resourceChainId :: !ChainId
    , _resourceLogger :: !logger
    , _resourceMempoolAccess :: !MempoolAccess
        -- ^ The mempool is own completely by the payload provider. So, maybe
        -- we should let it initialize the mempool by itself? At the momen
        -- all network components are initialized centrally.
    , _resourcePayloadDb :: !PayloadDb
        -- ^ For the EVM this is not needed, because it manages its own payload
        -- database and synchronization. Pact relies on the consensus header for\
        -- synchronizing the payloads and populating the payload database.
    , _resourceHeaderDb :: !BlockHeaderDb
        -- ^ this is unfortunate. We don't want to give the payload provider
        -- access to the header db. For EVM this is not needed, because the
        -- Ethereum EL is a blockchain on its own. For Pact, however, the
        -- consensus headers are needed for discovering fork points for reorgs.
    , _resourceMinerInfo :: !MinerInfo
        -- ^ Should this be on a per block basis?
    }

