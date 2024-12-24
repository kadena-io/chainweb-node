{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wprepositive-qualified-module #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM
( EvmProviderConfig(..)
, defaultEvmProviderConfig

-- * EVM Payload Provider Implementation
, EvmPayloadProvider(..)
, withEvmPayloadProvider
, evmPayloadDb
, evmPayloadQueue

-- * Payload Provider API
, evmSyncToBlock

) where

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHash qualified as Chainweb
import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.BlockPayloadHash (BlockPayloadHash)
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.MinerReward
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.EVM.EngineAPI
import Chainweb.PayloadProvider.EVM.EthRpcAPI
import Chainweb.PayloadProvider.EVM.Header qualified as EVM
import Chainweb.PayloadProvider.EVM.HeaderDB qualified as EvmDB
import Chainweb.PayloadProvider.EVM.JsonRPC (JsonRpcHttpCtx, callMethodHttp)
import Chainweb.PayloadProvider.EVM.JsonRPC qualified as JsonRpc
import Chainweb.PayloadProvider.EVM.Utils qualified as EVM
import Chainweb.PayloadProvider.EVM.Utils qualified as Utils
import Chainweb.PayloadProvider.Initialization
import Chainweb.PayloadProvider.P2P
import Chainweb.PayloadProvider.P2P.RestAPI.Client qualified as Rest
import Chainweb.Storage.Table
import Chainweb.Storage.Table.HashMap
import Chainweb.Storage.Table.RocksDB
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils
import Chainweb.Version
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.ByteString.Short qualified as BS
import Data.Functor
import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.PQueue
import Data.Singletons
import Data.Text qualified as T
import Ethereum.Misc
import Ethereum.Misc qualified as EVM
import Ethereum.Misc qualified as Ethereum
import Ethereum.RLP
import Ethereum.Trie
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeNats (fromSNat)
import Network.HTTP.Client qualified as HTTP
import Network.URI
import Network.URI.Static
import Numeric.Natural
import P2P.Session (ClientEnv)
import P2P.TaskQueue
import Servant.Client (ClientM)
import System.LogLevel
import Data.LogMessage
import Chainweb.PayloadProvider.P2P.RestAPI.Client
import Chainweb.PayloadProvider.P2P
import Chainweb.PayloadProvider.EVM.Utils (decodeRlpM)

-- -------------------------------------------------------------------------- --
-- Types (to keep the code clean and avoid confusion)

-- Might be a usecase for backpack, if we want to go down that route ...
--
-- Though doing it directly is fine, too.

type Payload = EVM.Header
type PayloadDb tbl = EvmDB.HeaderDb tbl

initPayloadDb :: EvmDB.Configuration -> IO (EvmDB.HeaderDb_ a RocksDbTable)
initPayloadDb = EvmDB.initHeaderDb

payloadDbConfiguration
    :: HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> RocksDb
    -> Payload
    -> EvmDB.Configuration
payloadDbConfiguration = EvmDB.configuration

-- Or maybe we could bring into scope the IsPayloadProviderClass? That would
-- help with other issues, too.
--
-- type Payload = PayloadType EvmPayloadProvider

-- -------------------------------------------------------------------------- --

-- FIXME
--
data EvmProviderConfig = EvmProviderConfig
    { _evmConfEngineUri :: !URI
    , _evmConfEngineJwtSecret :: !JwtSecret
    , _evmConfMinerInfo :: !(Maybe EVM.Address)
    }
    deriving (Show, Eq, Generic)

defaultEvmProviderConfig :: EvmProviderConfig
defaultEvmProviderConfig = EvmProviderConfig
    { _evmConfEngineUri = [uri|http://localhost:8551|]
    , _evmConfEngineJwtSecret = unsafeFromText "2d706f02122fb7bf29d48977851b1e30e305499991454374c4bfbc60189086be" -- FIXME
    , _evmConfMinerInfo = Nothing
    }

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
data EvmPayloadProvider logger = EvmPayloadProvider
    { _evmChainwebVersion :: !ChainwebVersion
    , _evmChainId :: !ChainId
    , _evmLogger :: !logger
    , _evmState :: !(TVar ConsensusState)
        -- ^ The current sync state of the EVM EL.

    , _evmPayloadStore :: !(PayloadStore (PayloadDb RocksDbTable) Payload)
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
    , _evmCandidatePayloads :: !(HashMapTable RankedBlockPayloadHash Payload)
        -- ^ FIXME: should this be moved into the Payload Store?
        --
        -- For now we just prune after each successful syncToBlock. Consensus
        -- has its own candidate cache with a broader (cut processing) scope.

    , _evmEngineCtx :: !JsonRpcHttpCtx
        -- ^ The JSON RPC context that provides the connection the Engine API of
        -- the EVM.

    -- Mining:
    , _evmMinerInfo :: !(Maybe Ethereum.Address)
    , _evmPayloadId :: !(TMVar (T2 SyncState PayloadId))
    , _evmPayloadVar :: !(TMVar (T2 EVM.BlockHash NewPayload))
    }

stateIO :: EvmPayloadProvider logger -> IO ConsensusState
stateIO = readTVarIO . _evmState

evmPayloadDb :: Getter (EvmPayloadProvider l) (PayloadDb RocksDbTable)
evmPayloadDb = to (_payloadStoreTable . _evmPayloadStore)

evmPayloadQueue :: Getter (EvmPayloadProvider l) (PQueue (Task ClientEnv Payload))
evmPayloadQueue = to (_payloadStoreQueue . _evmPayloadStore)

instance HasChainwebVersion (EvmPayloadProvider logger) where
    _chainwebVersion = _evmChainwebVersion

instance HasChainId (EvmPayloadProvider logger) where
    _chainId = _evmChainId

-- | Expose the /local/ payload table.
--
-- NOTE, this does /not/ look for payloads in P2P network. Data available in
-- this table is guaranteed to be fully validated, both by the execution client
-- as well as against the evaluation context.
--
instance
    ReadableTable (EvmPayloadProvider logger) RankedBlockPayloadHash Payload
  where
    tableLookup = tableLookup . _evmPayloadStore
    tableLookupBatch' s = tableLookupBatch' (_evmPayloadStore s)
    tableMember = tableMember . _evmPayloadStore

-- | Expose the /local/ payload table.
--
-- NOTE, this does /not/ look for payloads in P2P network. Data available in
-- this table is guaranteed to be fully validated, both by the execution client
-- as well as against the evaluation context.
--
instance
    Table (EvmPayloadProvider logger) RankedBlockPayloadHash Payload
  where
    tableInsert = tableInsert . _evmPayloadStore
    tableInsertBatch s = tableInsertBatch (_evmPayloadStore s)
    tableDelete s = tableDelete (_evmPayloadStore s)
    tableDeleteBatch s = tableDeleteBatch (_evmPayloadStore s)

lookupConsensusState
    :: ReadableTable tbl RankedBlockPayloadHash Payload
    => tbl
    -> ConsensusState
    -> IO (Maybe ForkchoiceStateV1)
lookupConsensusState p cs = do
    r <- tableLookupBatch p
        [ latestRankedBlockPayloadHash cs
        , safeRankedBlockPayloadHash cs
        , finalRankedBlockPayloadHash cs
        ]
    case r of
        [Nothing, _, _] -> return Nothing
        [Just l, Just s, Just f] -> return $ Just ForkchoiceStateV1
            { _forkchoiceHeadBlockHash = EVM._hdrHash l
            , _forkchoiceSafeBlockHash = EVM._hdrHash s
            , _forkchoiceFinalizedBlockHash = EVM._hdrHash f
            }
        x -> error "corrupted database" -- FIXME throw proper error

            -- FIXME: I guess this can happen with shallow nodes?
            -- The invariant is that if a block is in the db all predecessors
            -- are valid, but they are not necessarily available.

-- -------------------------------------------------------------------------- --
-- Exceptions

data EvmExecutionEngineException
    = EvmChainIdMissmatch (Expected EVM.ChainId) (Actual EVM.ChainId)
    deriving (Show, Eq, Generic)

instance Exception EvmExecutionEngineException

-- | Raised when an EVM header ca nnot be found
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

-- | Initializes a new EVM Payload provider
--
-- Note, that an exception is raised if the execution client is not available.
--
-- The function blocks while trying to connect to the execution client. It is
-- therefor advisable that this function is called asynchronously.
--
-- FIXME:
-- Currently the Genesis Header is pull from the execution client. For
-- production chainweb versions the hash of the genesis header or even the full
-- header should be hard-coded in chainweb node and we should check on startup
-- if it matches with the header from the execution client.
--
withEvmPayloadProvider
    :: Logger logger
    => HasChainwebVersion v
    => HasChainId c
    => logger
    -> v
    -> c
    -> RocksDb
    -> HTTP.Manager
        -- ^ P2P Network manager. This is supposed to be shared among all P2P
        -- network clients.
        --
        -- It is /not/ used for communication with the execution engine client.
    -> EvmProviderConfig
    -> (EvmPayloadProvider logger -> IO a)
    -> IO a
withEvmPayloadProvider logger v c rdb mgr conf f
    | FromSing @_ @p (SEvmProvider ecid) <- payloadProviderTypeForChain v c = do
        engineCtx <- mkEngineCtx (_evmConfEngineJwtSecret conf) (_evmConfEngineUri conf)

        SomeChainwebVersionT @v _ <- return $ someChainwebVersionVal v
        SomeChainIdT @c _ <- return $ someChainIdVal c
        let payloadClient h = Rest.payloadClient @v @c @p h

        genPld <- checkExecutionClient engineCtx (EVM.ChainId (fromSNat ecid))
        pdb <- initPayloadDb $ payloadDbConfiguration v c rdb genPld
        store <- newPayloadStore mgr (logFunction pldStoreLogger) pdb payloadClient
        pldVar <- newEmptyTMVarIO
        pldIdVar <- newEmptyTMVarIO
        candidates <- emptyTable
        stateVar <- newTVarIO (genesisState v c)
        let p = EvmPayloadProvider
                { _evmChainwebVersion = _chainwebVersion v
                , _evmChainId = _chainId c
                , _evmLogger = providerLogger
                , _evmState = stateVar
                , _evmPayloadStore = store
                , _evmCandidatePayloads = candidates
                , _evmEngineCtx = engineCtx
                , _evmMinerInfo = _evmConfMinerInfo conf
                , _evmPayloadId = pldIdVar
                , _evmPayloadVar = pldVar
                }

        result <- race (payloadListener p) $ do
            logFunctionText providerLogger Info $
                "EVM payload provider started for Ethereum network id " <> sshow ecid
            f p
        case result of
            Left () -> error "Chainweb.PayloadProvider.EVM.withEvmPayloadProvider: runForever (payloadListener p) exited unexpectedly"
            Right x -> return x

    | otherwise =
        error "Chainweb.PayloadProvider.Evm.configuration: chain does not use EVM provider"
  where
    providerLogger = setComponent "payload-provider"
        $ addLabel ("provider", "evm") logger
    pldStoreLogger = addLabel ("sub-component", "payloadStore") providerLogger

payloadListener :: Logger logger => EvmPayloadProvider logger -> IO ()
payloadListener p = runForever lf "EVM Provider Payload Listener" $
    awaitNewPayload p
  where
    lf = logFunction (_evmLogger p)

-- | Checks the availability of the Execution Client
--
-- - asserts API availability
-- - asserts the EVM chain id matches the chainweb version and chainweb chain id
-- - asserts that the Genesis header is available
--
-- Returns the genesis header.
--
checkExecutionClient
    :: JsonRpcHttpCtx
    -> EVM.ChainId
        -- ^ expected Ethereum Network ID
    -> IO Payload
checkExecutionClient ctx expectedEcid = do
    ecid <- callMethodHttp @Eth_ChainId ctx Nothing
    unless (expectedEcid == ecid) $
        throwM $ EvmChainIdMissmatch (Expected expectedEcid) (Actual ecid)
    callMethodHttp @Eth_GetBlockByNumber ctx (DefaultBlockNumber 0, False) >>= \case
        Nothing -> throwM EvmGenesisHeaderNotFound
        Just h -> return h

-- -------------------------------------------------------------------------- --

-- queryEngineState :: JsonRpcHttpCtx -> IO (Maybe ForkchoiceStateV1)
-- queryEngineState ctx = runConcurrently $ runMaybeT $ ForkchoiceStateV1
--     <$> MaybeT (Concurrently $ getHashByNumber ctx DefaultBlockLatest)
--     <*> MaybeT (Concurrently $ getHashByNumber ctx DefaultBlockSafe)
--     <*> MaybeT (Concurrently $ getHashByNumber ctx DefaultBlockFinalized)

-- -------------------------------------------------------------------------- --
-- Engine Calls

getHashByNumber
    :: JsonRpcHttpCtx
    -> DefaultBlockParameter
    -> IO (Maybe EVM.BlockHash)
getHashByNumber ctx p = fmap EVM._hdrHash <$>
    callMethodHttp @Eth_GetBlockByNumber ctx (p, False)

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
    => MonadIO m
    => EvmPayloadProvider pdf
    -> EVM.BlockNumber
    -> m Payload
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
    => MonadCatch m
    => MonadIO m
    => EvmPayloadProvider pdf
    -> m Payload
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
    => MonadIO m
    => EvmPayloadProvider pdf
    -> EVM.BlockHash
    -> m Payload
getBlockByHash p h = do
    r <- liftIO $ JsonRpc.callMethodHttp
        @Eth_GetBlockByHash (_evmEngineCtx p) (h, False)
    case r of
        Just hdr -> return hdr
        Nothing -> throwM $ EvmHeaderNotFoundByHash h

-- | Updates the forkchoice state of the EVM. Does not initiate payload
-- production.
--
-- This must be called only after validating the evaluation context for all
-- blocks that lead from the current state to the target state.
--
-- When new payload production was requested the state of the payload id
-- variable is updated. Otherwise the state of the payload id variable is set to
-- empty.
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
    => MonadIO m
    => EvmPayloadProvider pdb
    -> Micros
        -- ^ Timeout in microseconds
    -> Maybe (Chainweb.BlockHash, NewBlockCtx)
        -- ^ Whether to start new payload production on to of the target block.
    -> ForkchoiceStateV1
        -- ^ the requested fork choice state
    -> m Payload
forkchoiceUpdate p t ph state = go t >>= getBlockByHash p
  where
    waitTime = Micros 500_000
    payloadAttributes = case ph of
        Nothing -> Nothing
        Just (h, nctx) -> Just $ mkPayloadAttributes p h nctx
    go remaining
        | remaining <= 0 = throwM $ ForkchoiceUpdatedTimeoutException t
        | otherwise = do
            r <- liftIO $ JsonRpc.callMethodHttp @Engine_ForkchoiceUpdatedV3 (_evmEngineCtx p)
                (ForkchoiceUpdatedV3Request state payloadAttributes)

                -- FIXME: update payload ID variable

            case _forkchoiceUpdatedV1ResponsePayloadStatus r of
                PayloadStatusV1 Valid (Just x) Nothing -> return x
                PayloadStatusV1 Invalid Nothing (Just e) -> throwM $ InvalidPayloadException e
                PayloadStatusV1 Syncing Nothing Nothing -> do
                    -- wait 500ms
                    liftIO $ threadDelay $ int waitTime
                    go (remaining - waitTime)
                e -> throwM $ UnexpectedForkchoiceUpdatedResponseException e

mkPayloadAttributes
    :: EvmPayloadProvider logger
    -> Chainweb.BlockHash
        -- ^ The block hash of the block on which the new payload is created
        -- This is available to the payload provider in the target consensus
        -- state of the fork info of a syncToBlock call.
    -> NewBlockCtx
        -- The new block context from the fork info value.
    -> PayloadAttributesV3
mkPayloadAttributes p ph nctx = PayloadAttributesV3
    { _payloadAttributesV3parentBeaconBlockRoot = EVM.chainwebBlockHashToBeaconBlockRoot ph
    , _payloadAttributesV2 = PayloadAttributesV2
        { _payloadAttributesV2Withdrawals = [withdrawal]
        , _payloadAttributesV1 = PayloadAttributesV1
            { _payloadAttributesV1Timestamp = et
            , _payloadAttributesV1SuggestedFeeRecipient = minerAddress
            , _payloadAttributesV1PrevRandao = randao
            }
        }
    }
  where
    MinerReward reward = _newBlockCtxMinerReward nctx
    Just minerAddress = _evmMinerInfo p
    withdrawal = WithdrawalV1
        { _withdrawalValidatorIndex = 0
        , _withdrawalIndex = 0
        , _withdrawalAmount = int $ reward
        , _withdrawalAddress = minerAddress
        }

    -- FIXME: are there an assumptions about this value?
    randao = Utils.Randao (Ethereum.encodeLeN 0)

    -- Ethereum timestamps are measured in /seconds/ since POSIX epoch. Chainweb
    -- block creation times are measured in /micro-seconds/ since POSIX epoch.
    --
    -- Chainweb requires that block creation times are strictly monotonic.
    -- This means that blocks may be less than one second appart and rounding
    -- would result in two or more consequecutive Ethereum headers having the
    -- same time. Is that legal?
    --
    et = ethTimestamp (_newBlockCtxParentCreationTime nctx)

-- FIXME
ethTimestamp :: BlockCreationTime -> Timestamp
ethTimestamp (BlockCreationTime (Time (TimeSpan (Micros t)))) =
    EVM.Timestamp $ int $ (t `div` 1000000)

-- -------------------------------------------------------------------------- --

-- fetchAndVerifyEvmHeader
--     :: EvmPayloadProvider pdb
--     -> EvaluationCtx
--     -> IO Payload
-- fetchAndVerifyEvmHeader p evalCtx = do
--   where

-- | Obtain the EVM header for all entries of the Evaluation Ctx.
--
-- The header is first looked up in the local payload. If it is not available
-- locally it is fetched in the P2P network, validated against the evaluation
-- context, and inserted into the database.
--
-- getEvmHeader
--     :: EvmPayloadProvider pdb
--     -> RankedBlockPayloadHash
--     -> IO Payload
-- getEvmHeader p h =
--     tableLookup (_evmPayloadDb evm) (Just $ EVM._blockHeight h) payloadHash >>= \case
--         Nothing -> do
--             p <- fetchPayloadFromRemote
--             validateCtx evalCtx p
--             return p
--         Just p -> return p

-- -------------------------------------------------------------------------- --
-- Await New Payload

data EvmNewPayloadExeception
    = BlobsNotSupported
    | InvalidNewPayloadHeight (Expected BlockHeight) (Actual BlockHeight)
    | InconsistentNewPayloadFees
        { _inconsistentPayloadBlockValue :: !BlockValue
        , _inconsistentPayloadFees :: !Stu
        }
    | InconsistenNewPayloadHash (Expected EVM.BlockHash) (Actual EVM.BlockHash)
    deriving (Show, Eq, Generic)

instance Exception EvmNewPayloadExeception

newPayloadRate :: Int
newPayloadRate = 1_000_000

-- | If a payload id is available, new payloads for it.
--
awaitNewPayload :: EvmPayloadProvider logger -> IO ()
awaitNewPayload p = do
    T2 sstate pid <- atomically $ readTMVar (_evmPayloadId p)
    resp <- JsonRpc.callMethodHttp @Engine_GetPayloadV3 ctx pid

    -- Response data
    let v3 = _getPayloadV3ResponseExecutionPayload resp
        v2 = _executionPayloadV2 v3
        v1 = _executionPayloadV1 v2
        h = EVM.numberToHeight $ _executionPayloadV1BlockNumber v1
        newEvmBlockHash = _executionPayloadV1BlockHash v1

    -- check that this is a new payload
    T2 curEvmBlockHash cur <- atomically $ readTMVar (_evmPayloadVar p)
    unless (curEvmBlockHash == newEvmBlockHash) $ do
        -- check that Blobs bundle is empty
        unless (null $ _blobsBundleV1Blobs $ _getPayloadV3ResponseBlobsBundle resp) $
            throwM BlobsNotSupported

        -- Check that the block height matches the expected height
        unless ( (_syncStateHeight sstate + 1) == h) $
            throwM $ InvalidNewPayloadHeight (Expected (_syncStateHeight sstate + 1)) (Actual h)

        -- Check that the fees of the execution paylod match the block value of the
        -- response.
        --
        unless (EVM._blockValueStu (_getPayloadV3ResponseBlockValue resp) == fees v1) $
            throwM InconsistentNewPayloadFees
                { _inconsistentPayloadBlockValue = _getPayloadV3ResponseBlockValue resp
                , _inconsistentPayloadFees = fees v1
                }

        let pld = executionPayloadV3ToHeader (_syncStateBlockHash sstate) v3

        -- Check that the computed block hash matches the hash from the response
        unless (newEvmBlockHash == EVM._hdrHash pld) $
            throwM $ InconsistenNewPayloadHash
                (Expected newEvmBlockHash)
                (Actual (EVM._hdrHash pld))

        -- The actual payload header is included in the NewBlock structure in
        -- as EncodedPayloadData.
        atomically $ writeTMVar (_evmPayloadVar p) $ T2 newEvmBlockHash NewPayload
            { _newPayloadTxCount = int $ length (_executionPayloadV1Transactions v1)
            , _newPayloadSize = int $ sum $ (BS.length . _transactionBytes)
                    <$> (_executionPayloadV1Transactions v1)
            , _newPayloadParentHeight = _syncStateHeight sstate
            , _newPayloadParentHash = _syncStateBlockHash sstate
            , _newPayloadOutputSize = 0
            , _newPayloadNumber = _newPayloadNumber cur + 1
            , _newPayloadFees = fees v1
            , _newPayloadEncodedPayloadOutputs = Nothing
            , _newPayloadEncodedPayloadData = Just (EncodedPayloadData $ putRlpByteString pld)
            , _newPayloadChainwebVersion = v
            , _newPayloadChainId = cid
            , _newPayloadBlockPayloadHash = EVM._hdrPayloadHash pld
            }
    threadDelay newPayloadRate
  where
    ctx = _evmEngineCtx p
    v = _chainwebVersion p
    cid = _chainId p

    fees v1 = Stu $ bf * gu
      where
        EVM.BaseFeePerGas bf = _executionPayloadV1BaseFeePerGas v1
        GasUsed gu = _executionPayloadV1GasUsed v1

executionPayloadV3ToHeader
    :: Chainweb.BlockHash
    -> ExecutionPayloadV3
    -> Payload
executionPayloadV3ToHeader phdr v3 = hdr
    { EVM._hdrHash = EVM.computeBlockHash hdr
    , EVM._hdrPayloadHash = EVM.computeBlockPayloadHash hdr
    }
  where
    v2 = _executionPayloadV2 v3
    v1 = _executionPayloadV1 v2
    hdr = EVM.Header
        { _hdrParentHash = _executionPayloadV1ParentHash v1
        , _hdrOmmersHash = EVM.ommersHash
        , _hdrBeneficiary = _executionPayloadV1FeeRecipient v1
        , _hdrStateRoot = _executionPayloadV1StateRoot v1
        , _hdrTransactionsRoot = transactionsRoot (_executionPayloadV1Transactions v1)
        , _hdrReceiptsRoot = _executionPayloadV1ReceiptsRoot v1
        , _hdrLogsBloom = _executionPayloadV1LogsBloom v1
        , _hdrDifficulty = EVM.difficulty
        , _hdrNumber = _executionPayloadV1BlockNumber v1
        , _hdrGasLimit = _executionPayloadV1GasLimit v1
        , _hdrGasUsed = _executionPayloadV1GasUsed v1
        , _hdrTimestamp = _executionPayloadV1Timestamp v1
        , _hdrExtraData = _executionPayloadV1ExtraData v1
        , _hdrPrevRandao = _executionPayloadV1PrevRandao v1
        , _hdrNonce = EVM.nonce
        , _hdrBaseFeePerGas = _executionPayloadV1BaseFeePerGas v1
        , _hdrWithdrawalsRoot = withdrawlsRoot (_executionPayloadV2Withdrawals v2)
        , _hdrBlobGasUsed = _executionPayloadV3BlobGasUsed v3
        , _hdrExcessBlobGas = _executionPayloadV3ExcessBlobGas v3
        , _hdrParentBeaconBlockRoot = EVM.chainwebBlockHashToBeaconBlockRoot phdr
        , _hdrHash = error "Chainweb.PayloadProvider.EVM.executionPayloadV3ToHeader: _hdrHash"
        , _hdrPayloadHash = error "Chainweb.PayloadProvider.executionPayloadV3ToHeader: _hdrPayloadHash"
        }

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
-- In order to validate any EL header we need to map the Chainweb block hash
-- and/or payload hash to the respective Eth block hash. This requires that the
-- EVM provider must synchronize payload informatin along with the block headers
-- that is sufficient for this lookup.
--
-- Also, the evaluation context is not known to the EVM and thus it can not tell
-- if a block is invalid with respect to the evluation context. It is our job to
-- make sure that those payloads are all consistent with the evluation context.
-- We must do this check /before/ we actually let the EVM update its state, so
-- that (1.) users don't observe invalid states and (2.) we can recover from a
-- bad block (the EVM is monotonic for the canoncial chain).
--
-- The EVM EL header structure provides sufficient information to validate an EL
-- block against the evluation context.
--
-- If we are given a singleton evaluation context along with the respective EL
-- header we
--
-- 1. validate that the contex is consistent with the EL header and
-- 2. request the EVM to sync to that header, which will cause the EVM to query
--    respective full execution payload and transition to the new state.
--
-- If we validate more than a single blokc at a time, the EL queries the
-- respective EL payloads autonomously. We *must* ensure ahead of calling the
-- EVM that any full execution payload satifies the respective evaluation
-- context.
--
-- We can do this as follows:
--
-- 1. Eth block hash of the previous latest valid state is contained in any root
--    of the the target state (either the eth hash or Chainweb block hash or
--    Chainweb payload hash),
-- 2. Each Eth header
--       TODO
--
-- Hard requirements are that
--
-- 1. We keep track what EL blocks have already been validated against their
--    respecitve evaluation context.
-- 2. We never only ask for validation of blocks for which we have the EL header
--    available.
--
evmSyncToBlock
    :: Logger logger
    => EvmPayloadProvider logger
    -> Maybe Hints
    -> ForkInfo
    -> IO ConsensusState
evmSyncToBlock p hints forkInfo = do
    curState <- stateIO p
    newState <- if trgState == curState
      then
        -- The local sync state of the EVM EL is already at the target block.
        return curState
      else do
        -- Otherwise we'll take a look at the forkinfo trace

        -- lookup the fork info base payload hash
        let rankedBaseHash = _forkInfoBaseRankedPayloadHash forkInfo
        tableLookup p rankedBaseHash >>= \case

            -- If we don't know that base, there's nothing we can do
            Nothing -> return curState

            Just basePld -> case trace of
                -- Case of an empty trace:
                --
                -- This succeeds only when we evaluated the target state before.
                -- If it is in our database can attempt to sync to it, which may
                -- either succeed or fail. NOTE we must query the local store
                -- directly and not use the P2P network.
                --
                [] -> do
                    -- Lookup the target hash the local database
                    lookupConsensusState p trgState >>= \case
                        -- This case should not be possible because we looked up
                        -- the base already above. Anyways, we return state.
                        Nothing -> return curState
                        Just fcs -> do
                            -- In the case of an empty context we only care about
                            -- success or failure
                            _ <- liftIO $ forkchoiceUpdate p forkchoiceUpdatedTimeout doNewPld fcs
                            -- FIXME handle the case that the update fails
                            return trgState -- FIXME

                l -> do
                    -- in case of a non-empty trace, we lookup the first entry in
                    -- the trace. (FIXME We could be smarted and search for the
                    -- latest know payload, but we skip that optimization for now)
                    --
                    -- If we don't know the first entry, we fail. Otherwise, we
                    -- validate all headers in the context. If validation succeeds
                    -- we call syncToBlock. If that succeeds we add the headers to
                    -- the database.
                    --
                    -- It could also make sense to check the empty trace case first
                    -- and skip any ctx validation.

                    unknowns' <- dropWhile (isJust . snd) . zip l
                        <$> tableLookupBatch p (_evaluationCtxRankedPayloadHash <$> l)

                    -- assert db invariant
                    unless (all (isNothing . snd) unknowns') $
                        error "Chainweb.PayloadProviders.EVM.syncToBlock: detected corrupted payload database"

                    let unknowns = fst <$> unknowns'

                    -- fetch all unkown payloads
                    --
                    -- FIXME  do the right thing here. Ideally, fetch all
                    -- unknowns in batches without redundant local lookups. Then
                    -- validate all payloads together before sending them to the
                    -- EVM and inserting them into the DB.
                    --

                    plds <- forM unknowns $ \ctx -> do
                        pld <- getPayloadForContext p hints ctx
                        -- FIXME FIXME FIXME
                        validatePayload p pld ctx
                        return (_evaluationCtxRankedPayloadHash ctx, pld)

                    lookupConsensusState p trgState >>= \case
                        Nothing -> return curState
                        Just fcs -> do
                            r <- forkchoiceUpdate p forkchoiceUpdatedTimeout doNewPld fcs
                            if
                                | EVM._hdrPayloadHash r == _latestPayloadHash trgState -> do
                                    mapM_ (uncurry (tableInsert (_evmPayloadStore p))) plds
                                    return trgState
                                | EVM._hdrPayloadHash r == _latestPayloadHash curState ->
                                    return curState
                                | otherwise ->
                                    -- FIXME do something smarter here
                                    error "Chainweb.PayloadProvider.EVM.syncToBlock: failed to update EVM state"

    -- TODO cleeanup. In particular prune candidate store
    resetCandidates p
    return newState
  where
    trgState = _forkInfoTargetState forkInfo
    trgLatest = _consensusStateLatest trgState
    trgLatestHeight = _syncStateHeight trgLatest
    trgLatestNumber = EVM.heightToNumber trgLatestHeight
    trgLatestPayloadHash = _syncStateBlockPayloadHash trgLatest
    trace = _forkInfoTrace forkInfo
    doNewPld = (_latestBlockHash trgState,) <$> _forkInfoNewBlockCtx forkInfo

resetCandidates :: EvmPayloadProvider logger -> IO ()
resetCandidates p = do
    -- FIXME
    return ()


--  | Fetch the payload for a given evaluation context.
--
--  FIXME:
--  This is inefficient for various reasons:
--
--  1. When we call this function we did already establish which payloads are
--     available localy and have been validated before. There is no need to try
--     to look them up locally again. (Although, it is probably pretty cheap)
-- 2.  This function fetches payloads one by one. Instead we should query them
--     in chunks using the existing batch endpoints. (This is usually only
--     relevant during catchup. Otherwise we anyways go block by block.)
-- 3.  With the EVM provider we can validate the payloads with respect to the
--     evaluation contexts in batches before sending them to the execution
--     client.contexts in batches before sending them to the execution client.
--
getPayloadForContext
    :: Logger logger
    => EvmPayloadProvider logger
    -> Maybe Hints
    -> EvaluationCtx
    -> IO Payload
getPayloadForContext p h ctx = do
    mapM_ insertPayloadData (_evaluationCtxPayloadData ctx)
    pld <- getPayload
        (_evmPayloadStore p)
        (_evmCandidatePayloads p)
        (Priority $ negate $ int $ _evaluationCtxParentHeight ctx)
        (_hintsOrigin <$> h)
        (_evaluationCtxRankedPayloadHash ctx)
    tableInsert (_evmCandidatePayloads p) rh pld
    return pld
  where
    rh = _evaluationCtxRankedPayloadHash ctx

    insertPayloadData epld = case decodePayloadData epld of
        Right pld -> tableInsert (_evmCandidatePayloads p) rh pld
        Left e -> do
            lf Warn $ "failed to decode encoded payload from evaluation ctx: " <> sshow e

    lf :: LogFunctionText
    lf = logFunction (_evmLogger p)

-- | FIXME:
--
-- Do the actual validation. In particular, validate the whole chain of payloads
-- from the context in one go.
--
validatePayload
    :: EvmPayloadProvider logger
    -> Payload
    -> EvaluationCtx
    -> IO ()
validatePayload p pld ctx = return ()

-- -------------------------------------------------------------------------- --
-- Utils for querying EVM block details for a consensus state

-- | Get the header for the latest block in a consensus state from the EVM.
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
    => MonadIO m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m Payload
getLatestHdr p s = do
    hdr <- getBlockAtNumber p (_latestNumber s)
    unless (view EVM.hdrPayloadHash hdr == _latestPayloadHash s) $
        throwM $ EvmHeaderNotFoundByPayloadHash (_latestPayloadHash s)
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
    => MonadIO m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m Payload
getSafeHdr p s = do
    hdr <- getBlockAtNumber p (_safeNumber s)
    unless (view EVM.hdrPayloadHash hdr == _safePayloadHash s) $
        throwM $ EvmHeaderNotFoundByPayloadHash (_safePayloadHash s)
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
    => MonadIO m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m Payload
getFinalHdr p s = do
    hdr <- getBlockAtNumber p (_finalNumber s)
    unless (view EVM.hdrPayloadHash hdr == _finalPayloadHash s) $
        throwM $ EvmHeaderNotFoundByPayloadHash (_finalPayloadHash s)
    return hdr

-- | Get the hash for the latest block in a consensus state.
--
-- Cf. 'getLatestHeader' for details.
--
getLatestHash
    :: MonadThrow m
    => MonadIO m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.BlockHash
getLatestHash p = fmap (view EVM.hdrHash) . getLatestHdr p

-- | Get the hash for the safe block in a consensus state.
--
-- Cf. 'getSafeHeader' for details.
--
getSafeHash
    :: MonadThrow m
    => MonadIO m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.BlockHash
getSafeHash p = fmap (view EVM.hdrHash) . getSafeHdr p

-- | Get the hash for the final block in a consensus state.
--
-- Cf. 'getFinalHeader' for details.
--
getFinalHash
    :: MonadThrow m
    => MonadIO m
    => EvmPayloadProvider pdb
    -> ConsensusState
    -> m EVM.BlockHash
getFinalHash p = fmap (view EVM.hdrHash) . getFinalHdr p

-- -------------------------------------------------------------------------- --
-- Payload Provider API Instance

instance PayloadProvider (EvmPayloadProvider logger) where

    -- FIXME

-- -------------------------------------------------------------------------- --
-- Utils

_latestNumber :: ConsensusState -> EVM.BlockNumber
_latestNumber = EVM.heightToNumber . _latestHeight

_safeNumber :: ConsensusState -> EVM.BlockNumber
_safeNumber = EVM.heightToNumber . _safeHeight

_finalNumber :: ConsensusState -> EVM.BlockNumber
_finalNumber = EVM.heightToNumber . _finalHeight

encodedPayloadData :: Payload -> EncodedPayloadData
encodedPayloadData = EncodedPayloadData . putRlpByteString

decodePayloadData :: MonadThrow m => EncodedPayloadData -> m Payload
decodePayloadData (EncodedPayloadData bs) = decodeRlpM bs

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
--         fetchAndVerifyEvmHeader :: EvaluationCtx -> IO Payload
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
