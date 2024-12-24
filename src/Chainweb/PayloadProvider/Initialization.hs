{-# LANGUAGE ImportQualifiedPost #-}

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
import Chainweb.Payload.PayloadStore
import Chainweb.PayloadProvider
import Chainweb.Pact.Types (MemPoolAccess)
import Data.Text qualified as T

-- -------------------------------------------------------------------------- --
-- Initialization

-- | FIXME: this should be refined
--
newtype MinerInfo = MinerInfo T.Text

-- | Globally initialized resources that are needed by payload providers.
--
-- Note that not all payload providers require all of these resources.
--
data Resources tbl logger = Resources
    { _resourceChainwebVersion :: !ChainwebVersion
    , _resourceChainId :: !ChainId
    , _resourceLogger :: !logger
    , _resourceMempoolAccess :: !MemPoolAccess
        -- ^ The mempool is owned completely by the payload provider. So, maybe
        -- we should let it initialize the mempool by itself? At the moment all
        -- network components are initialized centrally.
    , _resourcePayloadDb :: !(PayloadDb tbl)
        -- ^ For the EVM this is not needed, because it manages its own payload
        -- database and synchronization. Pact relies on the consensus header for\
        -- synchronizing the payloads and populating the payload database.
    , _resourceMinerInfo :: !MinerInfo
    }
