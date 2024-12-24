{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_GHC -Wprepositive-qualified-module #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM
( EvmPayloadProvider(..)
, evmSyncToBlock
) where

import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.Initialization
import Chainweb.PayloadProvider.EVM.JsonRPC qualified as JsonRpc
import Ethereum.Misc qualified as Ethereum
import Data.List.NonEmpty qualified as NonEmpty
import Data.IORef
import Control.Concurrent.Async
import Control.Concurrent.STM
import Chainweb.Utils
import Data.Maybe
import Chainweb.PayloadProvider.EVM.Utils qualified as Utils
import Data.Functor
import Chainweb.BlockPayloadHash

import Ethereum.Misc qualified as EVM
import Chainweb.PayloadProvider.EVM.Header qualified as EVM
import Chainweb.PayloadProvider.EVM.HeaderDB qualified as EVM
import Chainweb.PayloadProvider.EVM.JsonRPC (JsonRpcHttpCtx)
import Chainweb.PayloadProvider.EVM.EngineAPI
import Chainweb.PayloadProvider.EVM.EthRpcAPI
import Chainweb.BlockHeight
import Control.Monad.Catch
import GHC.Generics
import Control.Monad.IO.Class
import Data.Text qualified as T
import Control.Concurrent
import Chainweb.Time
import Chainweb.BlockPayloadHash (BlockPayloadHash)
import Control.Monad

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
data EvmPayloadProvider payloadDb = EvmPayloadProvider
    { _evmState :: !(TVar ConsensusState)
        -- ^ The current sync state of the EVM EL.

    , _evmPayloadDb :: !payloadDb
        -- ^ The BlockPayloadHash in the ConsensusState is different from the
        -- EVM BlockHash that the EVM knows about.
        --
        -- For new blocks that are not yet known to the EVM we need to provide
        -- the EVM BlockHash. We compute this hash from the respective EVM
        -- Header. The header is queried via the Chainweb consensus P2P (which
        -- is also used to synchronize Pact payloads).
        --
        -- After validating the EVM header against the corresponding evaluation
        -- context we call engine_forkchoiceUpdated on it which synchronizes the
        -- EVM to the respective block. After that we can obtain the EVM header
        -- of the canonical chain from the EVM via JSON RPC API via the block
        -- number.
        --
        -- However, it is not clear how we can obtain non-canonical EVM headers
        -- without knowing their EVM block hashes. This is needed to resolve
        -- reorgs without having the synchronize the headers again from the P2P
        -- network. For that reason we also stored the EVM headers (redundantly)
        -- in the Payload Provider.
        --
        -- Strictly, just storing the mapping from Chainweb BlockPayloadHash to
        -- the EVM BlockHash would be sufficient to resolve reorgs (for headers
        -- which had been validated against the evaluation context before). But
        -- for simplicity we store the full EVM Header indexed by block height
        -- and block payload hash.
        --
        -- In the future we may consider teaching the EVM about Chainweb
        -- BlockPayloadHashes.

    , _evmEngineCtx :: !JsonRpcHttpCtx
        -- ^ The JSON RPC context that provides the connection the Engine API of
        -- the EVM.

    -- Mining:
    , _evmMinerInfo :: !(Maybe Ethereum.Address)
    , _evmPayloadListener :: !(Async ())
    , _evmPayloadId :: !(TMVar PayloadId)
    , _evmPayloadVar :: !(TMVar NewPayload)
    }

stateIO :: EvmPayloadProvider pd -> IO ConsensusState
stateIO = readTVarIO . _evmState

-- -------------------------------------------------------------------------- --
-- Exceptions

data EvmHeaderNotFoundException
    = EvmHeaderNotFoundByNumber DefaultBlockParameter
    | EvmHeaderNotFoundByHash EVM.BlockHash
    | EvmHeaderNotFoundByPayloadHash BlockPayloadHash
    deriving (Eq, Show, Generic)
instance Exception EvmHeaderNotFoundException

data InvalidEvmState
    = EvmGenesisHeaderNotFound
    deriving (Eq, Show, Generic)
instance Exception InvalidEvmState

newtype UnexpectedForkchoiceUpdatedResponseException
    = UnexpectedForkchoiceUpdatedResponseException PayloadStatusV1
    deriving (Eq, Show, Generic)
instance Exception UnexpectedForkchoiceUpdatedResponseException

newtype ForkchoiceUpdatedTimeoutException = ForkchoiceUpdatedTimeoutException Micros
    deriving (Eq, Show, Generic)
instance Exception ForkchoiceUpdatedTimeoutException

-- | Thrown on an invalid payload status.
--
newtype InvalidPayloadException = InvalidPayloadException T.Text
    deriving (Eq, Show, Generic)
instance Exception InvalidPayloadException

-- -------------------------------------------------------------------------- --
-- Engine Calls

-- | Obtains a block by number. It is an exception if the block can not be
-- found.
--
-- Returns: EVM Header
--
-- Throws:
--
-- * EvmHeaderNotFoundException
-- * Eth RPC failures
-- * JSON RPC failures
--
getBlockAtNumber
    :: MonadThrow m
    => EvmPayloadProvider pdf
    -> EVM.BlockNumber
    -> m EVM.Header
getBlockAtNumber p n = do
    r <- liftIO $ JsonRpc.callMethodHttp
        @Eth_GetBlockByNumber (_evmEngineCtx p) (DefaultBlockNumber n, False)
    case r of
        Just h -> return h
        Nothing -> throwM $ EvmHeaderNotFoundByNumber (DefaultBlockNumber n)

-- | Returns the EVM genesis block.
--
-- Returns: EVM Header
--
-- Throws:
--
-- * InvalidEvmState
-- * Eth RPC failures
-- * JSON RPC failures
--
getGenesisHeader
    :: MonadThrow m
    => EvmPayloadProvider pdf
    -> m EVM.Header
getGenesisHeader p = try (getBlockAtNumber p 0) >>= \case
    Left (EvmHeaderNotFoundByNumber _) -> throwM EvmGenesisHeaderNotFound
    Left e -> throwM e
    Right x -> return x

-- | Obtains a block by EVM block hash. It is an exception if the block can not
-- be found.
--
-- Returns: EVM Header
--
-- Throws:
--
-- * EvmHeaderNotFoundException
-- * Eth RPC failures
-- * JSON RPC failures
--
getBlockByHash
    :: MonadThrow m
    => EvmPayloadProvider pdf
    -> EVM.BlockHash
    -> m EVM.Header
getBlockByHash p h = do
    r <- liftIO $ JsonRpc.callMethodHttp
        @Eth_GetBlockByHash (_evmEngineCtx p) (h, False)
    case r of
        Just hdr -> return hdr
        Nothing -> throwM $ EvmHeaderNotFoundByHash h

-- | Updates the forkchoice state of the EVM. Does not initiate payload
-- production.
--
-- Returns: new EVM BlockHash
--
-- Throws:
--
-- * ForkchoiceUpdatedTimeoutException
-- * UnexpectedForkchoiceUpdatedResponseException
-- * InvalidPayloadException
--
-- * Engine RPC failures
-- * Eth RPC failures
-- * JSON RPC failure
--
forkchoiceUpdate
    :: MonadThrow m
    => EvmPayloadProvider pdb
    -> Micros
        -- ^ Timeout in microseconds
    -> EVM.BlockHash
        -- ^ the new forkchoice update
    -> BlockHeight
        -- ^ safe block height
    -> BlockHeight
        -- ^ finalized block height
    -> m EVM.Header
forkchoiceUpdate p t h safeH finalH = do
    -- FIXME we should be a bit smarter here: in particular make sure that
    -- we don't revert final blocks (or safe blocks). It if happens we
    -- definitely should warn or fail.
    safeBlock <- getBlockAtNumber p (EVM.heightToNumber safeH)
    finalBlock <- getBlockAtNumber p (EVM.heightToNumber finalH)
    let newState = ForkchoiceStateV1
            { _forkchoiceHeadBlockHash = h
            , _forkchoiceSafeBlockHash = EVM._hdrHash safeBlock
            , _forkchoiceFinalizedBlockHash = EVM._hdrHash finalBlock
            }
    h' <- go t safeBlock finalBlock newState
    getBlockByHash p h'
  where
    waitTime = Micros 500_000
    go remaining safeBlock finalBlock newState
        | remaining <= 0 = throwM $ ForkchoiceUpdatedTimeoutException t
        | otherwise = do
            r <- liftIO $ JsonRpc.callMethodHttp @Engine_ForkchoiceUpdatedV3 (_evmEngineCtx p)
                (ForkchoiceUpdatedV3Request newState Nothing)
            let payloadStatus = _forkchoiceUpdatedV1ResponsePayloadStatus r
            case payloadStatus of
                PayloadStatusV1 Valid (Just x) Nothing -> return x
                PayloadStatusV1 Invalid Nothing (Just e) -> throwM $ InvalidPayloadException e
                PayloadStatusV1 Syncing Nothing Nothing -> do
                    -- wait 500ms
                    liftIO $ threadDelay $ int waitTime
                    go (remaining - waitTime) safeBlock finalBlock newState
                e -> throwM UnexpectedForkchoiceUpdatedResponseException e

-- -------------------------------------------------------------------------- --

-- fetchAndVerifyEvmHeader
--     :: EvmPayloadProvider pdb
--     -> EvaluationCtx
--     -> IO EVM.Header
-- fetchAndVerifyEvmHeader p evalCtx = do
--   where

-- | Obtain the EVM header for all entries of the Evaluation Ctx.
--
-- The header is first looked up in the local payload. If it is not available
-- locally it is fetched in the P2P network, validated against the evaluation
-- context, and inserted into the database.
--
getEvmHeader
    :: EvmPayloadProvider pdb
    -> RankedBlockPayloadHash
    -> IO EVM.Header
getEvmHeader p h =
    tableLookup (_evmPayloadDb evm) (Just $ EVM._blockHeight h) payloadHash >>= \case
        Nothing -> do
            p <- fetchPayloadFromRemote
            validateCtx evalCtx p
            return p
        Just p -> return p

-- -------------------------------------------------------------------------- --
-- Sync To Block

-- | Timeout for evmSyncToBlock
--
forkchoiceUpdatedTimeout :: Micros
forkchoiceUpdatedTimeout = 3_000_000

-- | Synchronize the EL to the given block.
--
-- This function must *must* validate that all EL-headers up to the target block
-- are valid.
--
evmSyncToBlock
    :: EvmPayloadProvider payloadDb
    -> ForkInfo
    -> IO ConsensusState
evmSyncToBlock p forkInfo = do
    curState <- stateIO p
    if trgState == curState
      then
        -- The local sync state of the EVM EL is already at the target block.
        return curState
      else
        -- Otherwise we'll take a look at the forkinfo trace
        case trace of
            -- For now, lets focus on what can be done on an empty trace:
            [] -> try (getLatestHash p trgState) >>= \case

                -- EvmHeaderNotFoundByTooOldNumber -> _ -- fatal

                Left (EvmHeaderNotFoundByPayloadHash ph) -> _
                    -- The target number is smaller then the current number
                    -- but the block is on a fork.
                    --
                    -- TODO: how can we sync to that block without knowing
                    -- the eth block hash? the validation context is not enough
                    -- for this!
                    return curState


                Right h -> do
                    -- We evaluated the block before on the canonical chain.
                    -- (note that this must be supported by the Engine)

                    -- These are guaranteed to succeed under nominal
                    -- circumstances.
                    trgSafeHash <- getSafeHash p trgState
                    trgFinalHash <- getFinalHash p trgState

                    resultHdr <- forkchoiceUpdate p forkchoiceUpdatedTimeout (EVM.blockHash hdr) 12 480
                    return ConsensusState
                        { _consensusStateLatest = SyncState
                            { _syncStateBlockHash = _
                            , _syncStateHeight = _
                            , _syncStateBlockPayloadHash = EVM.blockPayloadHash resultHdr
                            }
                        , _consensusStateSafe = _
                        , _consensusStateFinal = _
                        }


                Left (EvmHeaderNotFoundByNumber _) ->
                    -- The target number is larger then the current number
                    --
                    -- The target is ahead, either on the canonical chain or another
                    -- chain. (There exist the possibility that we've seen it before but
                    -- rewound. Not sure if the Eth-node would remember it.) Let's
                    -- assume we need to evaluated it for the first time. For that we
                    -- need the validation context. Assuming that the context is
                    -- we do nothing and return the current state.
                    return curState

            l -> do
              _
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
              -- * Store EVM EL headers in the payload database and use them pre
              --   check the context
              -- * Another options is to not marke any block as safe of finalized
              --   before the context is evaluated.

              -- validate the EL header
              -- if the validation fails, return the current state
              -- if the validation succeeds, update the state and continue
              -- with the next header

            -- evmSyncToBlock p { _evmState = x } forkInfo { _forkInfoTrace = xs }
  where
    trgState = _forkInfoTargetState forkInfo
    trgLatest = _consensusStateLatest trgState
    trgLatestHeight = _syncStateHeight trgLatest
    trgLatestNumber = EVM.heightToNumber trgLatestHeight
    trgLatestPayloadHash = _syncStateBlockPayloadHash trgLatest
    trace = _forkInfoTrace forkInfo

-- -------------------------------------------------------------------------- --
-- Utils

latestPayloadHash :: ConsensusState -> BlockPayloadHash
latestPayloadHash = _syncStateBlockPayloadHash . _consensusStateLatest

latestHeight :: ConsensusState -> BlockHeight
latestHeight = _syncStateHeight . _consensusStateLatest

latestNumber :: ConsensusState -> EVM.BlockNumber
latestNumber = EVM.heightToNumber . latestHeight

safePayloadHash :: ConsensusState -> BlockPayloadHash
safePayloadHash = _syncStateBlockPayloadHash . _consensusStateSafe

safeHeight :: ConsensusState -> BlockHeight
safeHeight = _syncStateHeight . _consensusStateSafe

safeNumber :: ConsensusState -> EVM.BlockNumber
safeNumber = EVM.heightToNumber . safeHeight

finalPayloadHash :: ConsensusState -> BlockPayloadHash
finalPayloadHash = _syncStateBlockPayloadHash . _consensusStateFinal

finalHeight :: ConsensusState -> BlockHeight
finalHeight = _syncStateHeight . _consensusStateFinal

finalNumber :: ConsensusState -> EVM.BlockNumber
finalNumber = EVM.heightToNumber . finalHeight

-- -------------------------------------------------------------------------- --
-- Utils for querying EVM block details for a consensus state

-- | Get the header for the latest block in a consensus state.
--
-- Throws:
--
-- * EvmHeaderNotFoundException with
--   * EvmHeaderNotFoundByNumber if there is no block for the given number. This
--     usually means that consensus state references a block that has not been
--     validated before.
--   * EvmHeaderNotFoundByPayloadHash if the block on the given number does not
--     match the block in the payload state. This usually means that the
--     referenced block is on different fork.
--
getLatestHdr
    :: MonadThrow m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.Header
getLatestHdr p s = do
    hdr <- getBlockAtNumber p (latestNumber s)
    unless (EVM.blockPayloadHash hdr == latestPayloadHash s) $
        throwM $ EvmHeaderNotFoundByPayloadHash (latestPayloadHash s)
    return hdr

-- | Get the header for the safe block in a consensus state.
--
-- Throws:
--
-- * EvmHeaderNotFoundException with
--   * EvmHeaderNotFoundByNumber if there is no block for the given number. This
--     usually means that consensus state references a block that has not been
--     validated before. This can happen only if the latest block has not been
--     evaluted yet neither.
--   * EvmHeaderNotFoundByPayloadHash if the block on the given number does not
--     match the block in the payload state. This usually means that the
--     referenced block is on different fork. For this to happen the latest
--     block must be on a different fork, too.
--
getSafeHdr
    :: MonadThrow m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.Header
getSafeHdr p s = do
    hdr <- getBlockAtNumber p (safeNumber s)
    unless (EVM.blockPayloadHash hdr == safePayloadHash s) $
        throwM $ EvmHeaderNotFoundByPayloadHash (safePayloadHash s)
    return hdr

-- | Get the header for the final block in a consensus state.
--
-- Throws:
--
-- * EvmHeaderNotFoundException with
--   * EvmHeaderNotFoundByNumber if there is no block for the given number. This
--     usually means that consensus state references a block that has not been
--     validated before. This can happen only if the latest and safe blocks have
--     not been evaluted yet neither.
--   * EvmHeaderNotFoundByPayloadHash if the block on the given number does not
--     match the block in the payload state. This usually means that the
--     referenced block is on different fork. For this to happen the latest and
--     safe block must be on a different fork, too.
--
getFinalHdr
    :: MonadThrow m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.Header
getFinalHdr p s = do
    hdr <- getBlockAtNumber p (finalNumber s)
    unless (EVM.blockPayloadHash hdr == finalPayloadHash s) $
        throwM $ EvmHeaderNotFoundByPayloadHash (finalPayloadHash s)
    return hdr

-- | Get the hash for the latest block in a consensus state.
--
-- Cf. 'getLatestHeader' for details.
--
getLatestHash
    :: MonadThrow m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.BlockHash
getLatestHash p = fmap EVM.blockHash . getLatestHdr p

-- | Get the hash for the safe block in a consensus state.
--
-- Cf. 'getSafeHeader' for details.
--
getSafeHash
    :: MonadThrow m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.BlockHash
getSafeHash p = fmap EVM.blockHash . getSafeHdr p

-- | Get the hash for the final block in a consensus state.
--
-- Cf. 'getFinalHeader' for details.
--
getFinalHash
    :: MonadThrow m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.BlockHash
getFinalHash p = fmap EVM.blockHash . getFinalHdr p

-- -------------------------------------------------------------------------- --
-- Attic

-- instance PayloadProvider (EvmPayloadProvider payloadDb) where
--
--     syncToBlock evm maybeOrigin forkInfo = do
--         currentSyncState <- readTVarIO evm._evmState
--         if currentSyncState == targetSyncState
--         then return (Just currentSyncState)
--         else do
--             ps <- traverse fetchAndVerifyEvmHeader forkInfo._forkInfoTrace
--             let maybeTargetCtx = NonEmpty.last <$> NonEmpty.nonEmpty forkInfo._forkInfoTrace
--             -- TODO: check EvaluationCtx against EVM headers
--             -- TODO: also check that the ctxs form a chain ending in the target
--             case maybeTargetCtx of
--                 -- there was not enough information in the trace to change blocks
--                 Nothing -> return (Just currentSyncState)
--                 Just targetCtx -> do
--                     tableLookup evm._evmPayloadDb targetSyncState >>= \case
--                         -- TODO: fetch EVM headers here later?
--                         Nothing -> error "missing EVM header in payloaddb"
--                         Just evmHeader -> do
--                             let evmHash = EVM.blockHash evmHeader
--                             resp <- JsonRpc.callMethodHttp
--                                 @"engine_forkchoiceUpdatedV3" evm._evmEngineCtx (forkChoiceRequest evmHash)
--
--                             case resp._forkchoiceUpdatedV1ResponsePayloadStatus._payloadStatusV1Status  of
--                                 Valid -> do
--                                     listenToPayloadId resp._forkchoiceUpdatedV1ResponsePayloadId
--                                     return (Just targetSyncState)
--                                 Invalid -> return Nothing
--                                 Syncing -> waitForSync evmHash
--                                 Accepted -> error "invalid"
--                                 InvalidBlockHash -> return Nothing
--         where
--         targetSyncState = forkInfo._forkInfoTarget
--         forkChoiceRequest evmHash = ForkchoiceUpdatedV3Request
--             { _forkchoiceUpdatedV3RequestState = ForkchoiceStateV1 evmHash evmHash evmHash
--             , _forkchoiceUpdatedV3RequestPayloadAttributes =
--                 evm._evmMinerInfo <&> \minerAddress -> PayloadAttributesV3
--                     { _payloadAttributesV3parentBeaconBlockRoot =
--                         EVM.chainwebBlockHashToBeaconBlockRoot targetSyncState._rankedBlockHash
--                     , _payloadAttributesV2 = PayloadAttributesV2
--                         -- TODO: add withdrawals for paying miners block rewards?
--                         { _payloadAttributesV2Withdrawals = []
--                         , _payloadAttributesV1 = PayloadAttributesV1
--                             -- TODO: add real timestamp?
--                             { _payloadAttributesV1Timestamp = Ethereum.Timestamp 0
--                             -- TODO: use random number derived from PoW hash?
--                             , _payloadAttributesV1PrevRandao = Utils.Randao (Ethereum.encodeLeN 0)
--                             -- TODO: does this suffice to pay miners gas fees?
--                             , _payloadAttributesV1SuggestedFeeRecipient = minerAddress
--                             }
--                         }
--                     }
--             }
--         fetchAndVerifyEvmHeader :: EvaluationCtx -> IO EVM.Header
--         fetchAndVerifyEvmHeader evalCtx = do
--             tableLookup evm.payloadDb (Just $ view blockHeight h) payloadHash >>= \case
--                 Nothing -> do
--                     p <- fetchPayloadFromRemote
--                     validateCtx evalCtx p
--                     return p
--                 Just p -> return p
--         waitForSync evmHash = JsonRpc.callMethodHttp @"eth_syncing" evm._evmEngineCtx Nothing >>= \case
--             SyncingStatusFalse -> do
--                 resp <- JsonRpc.callMethodHttp @"engine_forkchoiceUpdatedV3" evm._evmEngineCtx (forkChoiceRequest evmHash)
--                 case resp._forkchoiceUpdatedV1ResponsePayloadStatus._payloadStatusV1Status of
--                     Valid -> do
--                         listenToPayloadId resp._forkchoiceUpdatedV1ResponsePayloadId
--                         return $ Just forkInfo._forkInfoTarget
--                     Invalid -> return Nothing
--                     Syncing -> error "that syncing feeling..."
--                     Accepted -> error "invalid"
--                     InvalidBlockHash -> return Nothing
--
--             SyncingStatus
--                 { _syncingStatusStartingBlock
--                 , _syncingStatusCurrentBlock = Ethereum.BlockNumber current
--                 , _syncingStatusHighestBlock = Ethereum.BlockNumber highest
--                 } -> do
--                 -- todo: use the current speed to make an estimate here
--                 approximateThreadDelay (max 10000 (int highest - int current * 10000))
--                 waitForSync evmHash
--         listenToPayloadId = \case
--             Just x | isJust evm._evmMinerInfo ->
--                 atomically $ writeTMVar evm._evmPayloadId x
--             Nothing | isNothing evm._evmMinerInfo ->
--                 return ()
--             pid -> error
--                 $ "mining is: " <> maybe "disabled" (\_ -> "enabled") evm._evmMinerInfo
--                 <> ", but payload ID is " <> maybe "not present" (\_ -> "present") pid
--
