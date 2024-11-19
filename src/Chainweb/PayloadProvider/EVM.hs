-- |
-- Module: Chainweb.PayloadProvider.EVM
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM
(
) where

import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.Initialization

-- -------------------------------------------------------------------------- --
-- EVM Payload Provider

-- | EVM Payload Provider
--
-- The EVM EL has to perform the following Chainweb specific validation tasks
-- on each EL header:
--
-- 1. The miner reward in coinbase is correct
-- 2. The payload hash is the Merkle root of the EL header.
-- 3. EL time = CL parent time
--
-- TODO:
-- * is the third point really necessary?
-- * How is the expected payload hash determined?
--
data EvmPayloadProvider = EvmPayloadProvider
    { _evmState :: !SyncState
        -- ^ The current sync state of the EVM EL.
    }

-- | Synchronize the EL to the given block.
--
-- We *must* validate that the EL header is valid.
--
-- To evaluate the EL header it can be queried from the EL. Or the CL can store
-- the EL headers in the payload database and provide them to the EL. In that
-- case the the EL would be obligated to validate all headers that are provided.
--
evmSyncToBlock
    :: EvmPayloadProvider
    -> ForkInfo
    -> IO SyncState
evmSyncToBlock p forkInfo
    | _forkInfoTargetHash forkInfo == _syncStateBlockHash (_evmState p) =
        -- The local sync state of the EVM EL is already at the target block.
        return (_evmState p)
    | otherwise = case _forkInfoTrace forkInfo of
        -- Nothing to validate.
        --
        -- NOTE that this behavior uses the assumption that validation is
        -- monotonic, which is not be true in the context of Chainweb. We deal
        -- with this by accepting that the EVM EL may be temporarily ahead of
        -- the CL state on individual chains. In practice, this means that
        -- reorgs are applied lazily and may be delayed on individual chains.
        --
        -- TODO: while this behavior is rare it is not exceptional and it
        -- may confront clients with more reorgs than they expect.
        --
        -- This also means that this function may permanently return a sync
        -- state that is ahead of the requested state. The client must detect
        -- this behavior as successful outcome.
        --
        [] -> return (_evmState p)
        l -> do
            -- Strictly, we only need to validate the evaluation context for
            -- blocks that haven't been validate before. However, the EVM EL
            -- does not provide this information on the engine API. Therefore,
            -- we validate provided contexts.
            --
            -- FIXME:
            -- If the validation fails, we set the sync state to the last
            -- successful block and return and failure.
            -- However, if we validate the context only afer requesting a sync
            -- from the EVM. If the sync was successful, we need a way to
            -- "reset" the EVM EL to the actual sync state. This is not possible
            -- through the engine API.
            -- An alterative is to require that users trust the lastest EVM
            -- block only if it is in a CL cut, which is not practical.
            -- Another option would be to disable the EVM API altogether until
            -- the EVM is in sync again. It is not clear how this could be
            -- achieved via existing APIs.
            -- TODO: investigate the use of engine_getPayloadBodiesByRange or
            -- engine_getPayloadBodiesByHash or engine_newPayload to validate
            -- evaluation contexts before committing to a new state.
            -- Another options is to not marke any block as safe of finalized
            -- before the context is evaluated.

            -- validate the EL header
            -- if the validation fails, return the current state
            -- if the validation succeeds, update the state and continue
            -- with the next header
            evmSyncToBlock p { _evmState = x } forkInfo { _forkInfoTrace = xs }
