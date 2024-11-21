{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Module: Chainweb.PayloadProvider.EVM
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM
( EvmPayloadProvider(..)
) where

import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.Initialization
import qualified Chainweb.PayloadProvider.EVM.Header as EVM
import Chainweb.PayloadProvider.EVM.JsonRPC (JsonRpcHttpCtx)
import Chainweb.PayloadProvider.EVM.EngineAPI
import Chainweb.PayloadProvider.EVM.EthRpcAPI
import qualified Chainweb.PayloadProvider.EVM.JsonRPC as JsonRpc
import qualified Ethereum.Misc as Ethereum
import Numeric.Natural (Natural)
import Chainweb.BlockHeight
import Chainweb.Storage.Table (Table (tableInsert), ReadableTable (tableLookup))
import Chainweb.BlockPayloadHash
import qualified Data.List.NonEmpty as NonEmpty
import Data.IORef
import Control.Concurrent.Async
import Control.Concurrent.STM
import Chainweb.Utils
import Data.Maybe
import qualified Chainweb.PayloadProvider.EVM.Utils as Utils
import Data.Functor
import qualified Chainweb.RankedBlockHash as Chainweb
import qualified Chainweb.BlockPayloadHash as Chainweb

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
data EvmPayloadProvider payloadDb = EvmPayloadProvider
    { _evmState :: !(TVar Chainweb.RankedBlockHash)
    -- ^ The current sync state of the EVM EL.
    , _evmPayloadSub :: !(NewPayload -> IO ())
    , _evmPayloadListener :: !(Async ())
    , _evmPayloadId :: !(TMVar PayloadId)
    , _evmPayloadVar :: !(TMVar NewPayload)
    , _evmEngineCtx :: !JsonRpcHttpCtx
    -- ^ The Json RPC context allowing us access to the EVM Engine API.
    , _evmPayloadDb :: !payloadDb
    , _evmMinerInfo :: !(Maybe Ethereum.Address)
    }

-- TODO: should we have
instance Table payloadDb Chainweb.BlockPayloadHash EVM.Header => PayloadProvider (EvmPayloadProvider payloadDb) where
    syncToBlock evm maybeOrigin forkInfo = do
        currentSyncState <- readTVarIO evm._evmState
        if currentSyncState == targetSyncState
        then return (Just currentSyncState)
        else do
            ps <- traverse fetchAndVerifyEvmHeader forkInfo._forkInfoTrace
            let maybeTargetCtx = NonEmpty.last <$> NonEmpty.nonEmpty forkInfo._forkInfoTrace
            -- TODO: check EvaluationCtx against EVM headers
            -- TODO: also check that the ctxs form a chain ending in the target
            case maybeTargetCtx of
                -- there was not enough information in the trace to change blocks
                Nothing -> return (Just currentSyncState)
                Just targetCtx -> do
                    tableLookup evm._evmPayloadDb targetSyncState >>= \case
                        -- TODO: fetch EVM headers here later?
                        Nothing -> error "missing EVM header in payloaddb"
                        Just evmHeader -> do
                            let evmHash = EVM.blockHash evmHeader
                            resp <- JsonRpc.callMethodHttp
                                @"engine_forkchoiceUpdatedV3" evm._evmEngineCtx (forkChoiceRequest evmHash)

                            case resp._forkchoiceUpdatedV1ResponsePayloadStatus._payloadStatusV1Status  of
                                Valid -> do
                                    listenToPayloadId resp._forkchoiceUpdatedV1ResponsePayloadId
                                    return (Just targetSyncState)
                                Invalid -> return Nothing
                                Syncing -> waitForSync evmHash
                                Accepted -> error "invalid"
                                InvalidBlockHash -> return Nothing
        where
        targetSyncState = forkInfo._forkInfoTarget
        forkChoiceRequest evmHash = ForkchoiceUpdatedV3Request
            { _forkchoiceUpdatedV3RequestState = ForkchoiceStateV1 evmHash evmHash evmHash
            , _forkchoiceUpdatedV3RequestPayloadAttributes =
                evm._evmMinerInfo <&> \minerAddress -> PayloadAttributesV3
                    { _payloadAttributesV3parentBeaconBlockRoot =
                        EVM.chainwebBlockHashToBeaconBlockRoot targetSyncState._rankedBlockHash
                    , _payloadAttributesV2 = PayloadAttributesV2
                        -- TODO: add withdrawals for paying miners block rewards?
                        { _payloadAttributesV2Withdrawals = []
                        , _payloadAttributesV1 = PayloadAttributesV1
                            -- TODO: add real timestamp?
                            { _payloadAttributesV1Timestamp = Ethereum.Timestamp 0
                            -- TODO: use random number derived from PoW hash?
                            , _payloadAttributesV1PrevRandao = Utils.Randao (Ethereum.encodeLeN 0)
                            -- TODO: does this suffice to pay miners gas fees?
                            , _payloadAttributesV1SuggestedFeeRecipient = minerAddress
                            }
                        }
                    }
            }
        fetchAndVerifyEvmHeader :: EvaluationCtx -> IO EVM.Header
        fetchAndVerifyEvmHeader evalCtx = do
            tableLookup evm.payloadDb (Just $ view blockHeight h) payloadHash >>= \case
                Nothing -> do
                    p <- fetchPayloadFromRemote
                    validateCtx evalCtx p
                    return p
                Just p -> return p
        waitForSync evmHash = JsonRpc.callMethodHttp @"eth_syncing" evm._evmEngineCtx Nothing >>= \case
            SyncingStatusFalse -> do
                resp <- JsonRpc.callMethodHttp @"engine_forkchoiceUpdatedV3" evm._evmEngineCtx (forkChoiceRequest evmHash)
                case resp._forkchoiceUpdatedV1ResponsePayloadStatus._payloadStatusV1Status of
                    Valid -> do
                        listenToPayloadId resp._forkchoiceUpdatedV1ResponsePayloadId
                        return $ Just forkInfo._forkInfoTarget
                    Invalid -> return Nothing
                    Syncing -> error "that syncing feeling..."
                    Accepted -> error "invalid"
                    InvalidBlockHash -> return Nothing

            SyncingStatus
                { _syncingStatusStartingBlock
                , _syncingStatusCurrentBlock = Ethereum.BlockNumber current
                , _syncingStatusHighestBlock = Ethereum.BlockNumber highest
                } -> do
                -- todo: use the current speed to make an estimate here
                approximateThreadDelay (max 10000 (int highest - int current * 10000))
                waitForSync evmHash
        listenToPayloadId = \case
            Just x | isJust evm._evmMinerInfo ->
                atomically $ writeTMVar evm._evmPayloadId x
            Nothing | isNothing evm._evmMinerInfo ->
                return ()
            pid -> error
                $ "mining is: " <> maybe "disabled" (\_ -> "enabled") evm._evmMinerInfo
                <> ", but payload ID is " <> maybe "not present" (\_ -> "present") pid

-- | Synchronize the EL to the given block.
--
-- We *must* validate that the EL header is valid.
--
-- To evaluate the EL header it can be queried from the EL. Or the CL can store
-- the EL headers in the payload database and provide them to the EL. In that
-- case the the EL would be obligated to validate all headers that are provided.
--
-- evmSyncToBlock
--     :: EvmPayloadProvider payloadDb
--     -> ForkInfo
--     -> IO SyncState
-- evmSyncToBlock p forkInfo
--     | _forkInfoTargetHash forkInfo == _syncStateBlockHash (_evmState p) =
--         -- The local sync state of the EVM EL is already at the target block.
--         return (_evmState p)
--     | otherwise = case _forkInfoTrace forkInfo of
--         -- Nothing to validate.
--         --
--         -- NOTE that this behavior uses the assumption that validation is
--         -- monotonic, which is not be true in the context of Chainweb. We deal
--         -- with this by accepting that the EVM EL may be temporarily ahead of
--         -- the CL state on individual chains. In practice, this means that
--         -- reorgs are applied lazily and may be delayed on individual chains.
--         --
--         -- TODO: while this behavior is rare it is not exceptional and it
--         -- may confront clients with more reorgs than they expect.
--         --
--         -- This also means that this function may permanently return a sync
--         -- state that is ahead of the requested state. The client must detect
--         -- this behavior as successful outcome.
--         --
--         [] -> return (_evmState p)
--         l -> do
--             -- Strictly, we only need to validate the evaluation context for
--             -- blocks that haven't been validate before. However, the EVM EL
--             -- does not provide this information on the engine API. Therefore,
--             -- we validate provided contexts.
--             --
--             -- FIXME:
--             -- If the validation fails, we set the sync state to the last
--             -- successful block and return and failure.
--             -- However, if we validate the context only afer requesting a sync
--             -- from the EVM. If the sync was successful, we need a way to
--             -- "reset" the EVM EL to the actual sync state. This is not possible
--             -- through the engine API.
--             -- An alterative is to require that users trust the lastest EVM
--             -- block only if it is in a CL cut, which is not practical.
--             -- Another option would be to disable the EVM API altogether until
--             -- the EVM is in sync again. It is not clear how this could be
--             -- achieved via existing APIs.
--             -- TODO: investigate the use of engine_getPayloadBodiesByRange or
--             -- engine_getPayloadBodiesByHash or engine_newPayload to validate
--             -- evaluation contexts before committing to a new state.
--             -- * Store EVM EL headers in the payload database and use them pre
--             --   check the context
--             -- * Another options is to not marke any block as safe of finalized
--             --   before the context is evaluated.

--             -- validate the EL header
--             -- if the validation fails, return the current state
--             -- if the validation succeeds, update the state and continue
--             -- with the next header
--             undefined
--             -- evmSyncToBlock p { _evmState = x } forkInfo { _forkInfoTrace = xs }
