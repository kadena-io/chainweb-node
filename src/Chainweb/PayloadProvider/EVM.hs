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
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wprepositive-qualified-module #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

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
, pEvmProviderConfig

-- * EVM Payload Provider Implementation
, EvmPayloadProvider(..)
, withEvmPayloadProvider
, evmPayloadDb
, evmPayloadQueue

-- * Payload Provider API
, evmSyncToBlock

) where

import Chainweb.BlockHash qualified as Chainweb
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.Core.Brief
import Chainweb.Logger
import Chainweb.MinerReward
import Chainweb.PayloadProvider hiding (TransactionIndex)
import Chainweb.PayloadProvider.EVM.EngineAPI
import Chainweb.PayloadProvider.EVM.EthRpcAPI
import Chainweb.PayloadProvider.EVM.Header qualified as EVM
import Chainweb.PayloadProvider.EVM.HeaderDB qualified as EvmDB
import Chainweb.PayloadProvider.EVM.JsonRPC (JsonRpcHttpCtx, callMethodHttp)
import Chainweb.PayloadProvider.EVM.JsonRPC qualified as RPC
import Chainweb.PayloadProvider.EVM.SPV
import Chainweb.PayloadProvider.EVM.Utils (decodeRlpM)
import Chainweb.PayloadProvider.EVM.Utils qualified as EVM
import Chainweb.PayloadProvider.EVM.Utils qualified as Utils
import Chainweb.PayloadProvider.P2P
import Chainweb.PayloadProvider.P2P.RestAPI.Client qualified as Rest
import Chainweb.Ranked
import Chainweb.Storage.Table
import Chainweb.Storage.Table.Map
import Chainweb.Storage.Table.RocksDB
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Configuration.Utils hiding (Error)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString.Short qualified as BS
import Data.List qualified as L
import Data.LogMessage
import Data.Maybe
import Data.PQueue
import Data.Singletons
import Data.Text qualified as T
import Data.Tuple
import Ethereum.Misc
import Ethereum.Misc qualified as EVM
import Ethereum.Misc qualified as Ethereum
import Ethereum.RLP
import Ethereum.Receipt
import GHC.Generics (Generic)
import GHC.TypeNats (fromSNat)
import Network.HTTP.Client qualified as HTTP
import Network.URI
import Network.URI.Static
import P2P.Session (ClientEnv)
import P2P.TaskQueue
import System.LogLevel
import Data.Function

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
-- Configuration

prefixLongCid :: HasName f => ChainId -> String -> Mod f a
prefixLongCid cid = prefixLong (Just p)
  where
    p = "chain-" <> T.unpack (toText cid)

suffixHelpCid :: ChainId -> String -> Mod f a
suffixHelpCid cid = suffixHelp (Just s)
  where
    s = "chain-" <> T.unpack (toText cid)

pJwtSecret :: ChainId -> OptionParser JwtSecret
pJwtSecret cid = textOption
    % prefixLongCid cid "jwt-secret"
    <> suffixHelpCid cid "JWT secret for the EVM Engine API"

pMinerAddress :: ChainId -> OptionParser EVM.Address
pMinerAddress cid = textOption
    % prefixLongCid cid "miner-address"
    <> suffixHelpCid cid "Miner address for new EVM blocks"

newtype EngineUri = EngineUri { _engineUri :: URI }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (JsonTextRepresentation "EngineUri" EngineUri)

instance HasTextRepresentation EngineUri where
    toText (EngineUri u) = toText u
    fromText = fmap EngineUri . fromText

pEngineUri :: ChainId -> OptionParser EngineUri
pEngineUri cid = textOption
    % prefixLongCid cid "uri"
    <> suffixHelpCid cid "EVM Engine URI"

data EvmProviderConfig = EvmProviderConfig
    { _evmConfEngineUri :: !EngineUri
    , _evmConfEngineJwtSecret :: !JwtSecret
    , _evmConfMinerAddress :: !(Maybe EVM.Address)
    }
    deriving (Show, Eq, Generic)

makeLenses ''EvmProviderConfig

defaultEvmProviderConfig :: EvmProviderConfig
defaultEvmProviderConfig = EvmProviderConfig
    { _evmConfEngineUri = EngineUri [uri|http://localhost:8551|]
    , _evmConfEngineJwtSecret = unsafeFromText "0000000000000000000000000000000000000000000000000000000000000000"
    , _evmConfMinerAddress = Nothing
    }

instance ToJSON EvmProviderConfig where
    toJSON o = object
        [ "engineUri" .= _evmConfEngineUri o
        , "engineJwtSecret" .= _evmConfEngineJwtSecret o
        , "minerAddress" .= _evmConfMinerAddress o
        ]

instance FromJSON EvmProviderConfig where
    parseJSON = withObject "EvmProviderConfig" $ \o -> EvmProviderConfig
        <$> o .: "engineUri"
        <*> o .: "engineJwtSecret"
        <*> o .: "minerAddress"

instance FromJSON (EvmProviderConfig -> EvmProviderConfig) where
    parseJSON = withObject "EvmProviderConfig" $ \o -> id
        <$< evmConfEngineUri ..: "engineUri" % o
        <*< evmConfEngineJwtSecret ..: "engineJwtSecret" % o
        <*< evmConfMinerAddress ..: "minerAddress" % o

pEvmProviderConfig :: ChainId -> MParser EvmProviderConfig
pEvmProviderConfig cid = id
    <$< evmConfEngineUri .:: pEngineUri cid
    <*< evmConfEngineJwtSecret .:: pJwtSecret cid
    <*< evmConfMinerAddress .:: fmap Just % pMinerAddress cid

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
    , _evmCandidatePayloads :: !(MapTable RankedBlockPayloadHash Payload)
        -- ^ FIXME: should this be moved into the Payload Store?
        --
        -- At the moment this is not really used at all.

    , _evmEngineCtx :: !JsonRpcHttpCtx
        -- ^ The JSON RPC context that provides the connection the Engine API of
        -- the EVM.
    , _evmMinerAddress :: !(Maybe Ethereum.Address)
        -- ^ The miner address. If this is Nothing, payload creation is
        -- disabled.

    -- Internal State:

    , _evmState :: !(TVar (T2 ConsensusState (Maybe NewBlockCtx)))
        -- ^ The current consensus state and new block ctx of the EVM.
        --
        -- The new block context is included so that new payload ID can be
        -- requested in case the previous one got lost.

    , _evmPayloadId :: !(TMVar PayloadId)
        -- ^ The payload ID along with the respective SyncState to which it
        -- corresponds. If this is set it always corresponds to the current
        -- consensus state.

    , _evmPayloadVar :: !(TMVar (T2 EVM.BlockHash NewPayload))
        -- ^ The most recent new payload
    , _evmLock :: !(MVar ())
        -- ^ Not sure if we really need this. Users could race, but not sure
        -- whether we actually care...
        --
        -- FIXME: if we actually need or want this, we should probably use a
        -- queue, or even better preempt earlier operations.
    }

stateIO :: EvmPayloadProvider logger -> IO (T2 ConsensusState (Maybe NewBlockCtx))
stateIO = readTVarIO . _evmState

latestStateIO :: EvmPayloadProvider logger -> IO SyncState
latestStateIO = fmap (_consensusStateLatest . sfst) . stateIO

isPayloadRequestedIO :: EvmPayloadProvider logger -> IO Bool
isPayloadRequestedIO p = case _evmMinerAddress p of
    Nothing -> return False
    Just _ -> (isJust . ssnd) <$> stateIO p

newBlockCtxIO :: EvmPayloadProvider logger -> IO (Maybe NewBlockCtx)
newBlockCtxIO p = case _evmMinerAddress p of
    Nothing -> return Nothing
    Just _ -> ssnd <$> stateIO p

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

-- |
--
-- IMPORTANT NOTE: this takes into account the candidate store. This means
-- results can't be trusted to be evaluated.
--
-- Candidates must be inserted in the candidate store only after they are
-- validated against the evaluation context.
--
lookupConsensusState
    :: ReadableTable tbl RankedBlockPayloadHash Payload
    => tbl
    -> ConsensusState
    -> [(RankedBlockPayloadHash, Payload)]
    -> IO (Maybe ForkchoiceStateV1)
lookupConsensusState p cs plds = do
    r0 <- tableLookupBatch p
        [ latestRankedBlockPayloadHash cs
        , safeRankedBlockPayloadHash cs
        , finalRankedBlockPayloadHash cs
        ]

    -- Fill in payloads that are missing in the database from the candidate
    -- payloads.
    --
    case go <$> (zip [lrh, srh, frh] r0) of
        [Nothing, _, _] -> do
            return Nothing
        [Just l, Just s, Just f] -> return $ Just ForkchoiceStateV1
            { _forkchoiceHeadBlockHash = EVM._hdrHash l
            , _forkchoiceSafeBlockHash = EVM._hdrHash s
            , _forkchoiceFinalizedBlockHash = EVM._hdrHash f
            }
        x -> error $ "corrupted database: " <> sshow x
            -- FIXME throw proper error
            --
            -- FIXME: I guess this can happen with shallow nodes?
            -- The invariant is that if a block is in the db all predecessors
            -- are valid, but they are not necessarily available.
  where
    go (k, Nothing) = lookup k plds
    go (_, x) = x
    lrh = latestRankedBlockPayloadHash cs
    srh = safeRankedBlockPayloadHash cs
    frh = finalRankedBlockPayloadHash cs

loggS
    :: Logger logger
    => EvmPayloadProvider logger
    -> T.Text
        -- ^ sub-component name
    -> LogLevel
    -> T.Text
    -> IO ()
loggS p s l t = logFunctionText logger l t
  where
    logger = _evmLogger p & addLabel ("sub-component", s)

logg
    :: Logger logger
    => EvmPayloadProvider logger
    -> LogLevel
    -> T.Text
    -> IO ()
logg p l t = logFunctionText (_evmLogger p) l t

-- -------------------------------------------------------------------------- --
-- Exceptions

data EvmExecutionEngineException
    = EvmChainIdMissmatch (Expected EVM.ChainId) (Actual EVM.ChainId)
    | EvmInvalidGensisHeader (Expected BlockPayloadHash) (Actual BlockPayloadHash)
    deriving (Show, Eq, Generic)

instance Exception EvmExecutionEngineException

-- | Raised when an EVM header ca nnot be found
data EvmHeaderNotFoundException
    = EvmHeaderNotFoundByNumber DefaultBlockParameter
    | EvmHeaderNotFoundByHash EVM.BlockHash
    | EvmHeaderNotFoundByPayloadHash BlockPayloadHash
    deriving (Eq, Show, Generic)
instance Exception EvmHeaderNotFoundException

data LogEntryNotFoundException
    = InvalidTransactionIndex XEventId
    | InvalidEventIndex XEventId
    | AmbiguousLogEntries XEventId [RpcLogEntry]
    deriving (Eq, Show, Generic)
instance Exception LogEntryNotFoundException

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
data InvalidPayloadException = InvalidPayloadException !EVM.BlockHash !(Maybe T.Text)
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
--
-- Verify that the genesis headers form the execution client match what is
-- stored in the chainweb version.
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
        engineCtx <- mkEngineCtx (_evmConfEngineJwtSecret conf) (_engineUri $ _evmConfEngineUri conf)

        SomeChainwebVersionT @v _ <- return $ someChainwebVersionVal v
        SomeChainIdT @c _ <- return $ someChainIdVal c
        let pldCli h = Rest.payloadClient @v @c @p h

        genPld <- checkExecutionClient v c engineCtx (EVM.ChainId (fromSNat ecid))
        logFunctionText logger Info $ "genesis payload block hash: " <> sshow (EVM._hdrPayloadHash genPld)
        logFunctionText logger Debug $ "genesis payload from execution client: " <> sshow genPld
        pdb <- initPayloadDb $ payloadDbConfiguration v c rdb genPld
        store <- newPayloadStore mgr (logFunction pldStoreLogger) pdb pldCli
        pldVar <- newEmptyTMVarIO
        pldIdVar <- newEmptyTMVarIO
        candidates <- emptyTable
        stateVar <- newTVarIO (T2 (genesisState v c) Nothing)
        lock <- newMVar ()
        let p = EvmPayloadProvider
                { _evmChainwebVersion = _chainwebVersion v
                , _evmChainId = _chainId c
                , _evmLogger = logger
                , _evmState = stateVar
                , _evmPayloadStore = store
                , _evmCandidatePayloads = candidates
                , _evmEngineCtx = engineCtx
                , _evmMinerAddress = _evmConfMinerAddress conf
                , _evmPayloadId = pldIdVar
                , _evmPayloadVar = pldVar
                , _evmLock = lock
                }

        result <- race (payloadListener p) $ do
            logg p Info $
                "EVM payload provider started for Ethereum network id " <> sshow ecid
            f p
        case result of
            Left () -> error "Chainweb.PayloadProvider.EVM.withEvmPayloadProvider: runForever (payloadListener p) exited unexpectedly"
            Right x -> return x

    | otherwise =
        error "Chainweb.PayloadProvider.Evm.configuration: chain does not use EVM provider"
  where
    pldStoreLogger = addLabel ("sub-component", "payloadStore") logger

payloadListener :: Logger logger => EvmPayloadProvider logger -> IO ()
payloadListener p = case (_evmMinerAddress p) of
    Nothing -> do
        lf Info "New payload creation is disabled."
        return ()
    Just addr -> runForeverThrottled lf "EVM Provider Payload Listener" 5 10000 $ do
        lf Info $ "Start payload listener with miner address " <> toText addr
        awaitNewPayload p
  where
    lf = loggS p "payloadListener"

-- | Checks the availability of the Execution Client
--
-- - asserts API availability
-- - asserts the EVM chain id matches the chainweb version and chainweb chain id
-- - asserts that the Genesis header is available
--
-- Returns the genesis header.
--
checkExecutionClient
    :: HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> JsonRpcHttpCtx
    -> EVM.ChainId
        -- ^ expected Ethereum Network ID
    -> IO Payload
checkExecutionClient v c ctx expectedEcid = do
    ecid <- callMethodHttp @Eth_ChainId ctx Nothing
    unless (expectedEcid == ecid) $
        throwM $ EvmChainIdMissmatch (Expected expectedEcid) (Actual ecid)
    callMethodHttp @Eth_GetBlockByNumber ctx (DefaultBlockNumber 0, False) >>= \case
        Nothing -> throwM EvmGenesisHeaderNotFound
        Just h -> do
            unless (EVM._hdrPayloadHash h == expectedGenesisHeader) $
                throwM $ EvmInvalidGensisHeader
                    (Expected expectedGenesisHeader)
                    (Actual $ EVM._hdrPayloadHash h)
            return h
  where
    expectedGenesisHeader = genesisBlockPayloadHash (_chainwebVersion v) (_chainId c)

-- -------------------------------------------------------------------------- --
-- Engine Calls

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
    :: Logger logger
    => EvmPayloadProvider logger
    -> Micros
        -- ^ Timeout in microseconds
    -> ForkchoiceStateV1
        -- ^ the requested fork choice state
    -> Maybe PayloadAttributesV3
    -> IO (Maybe PayloadId)
forkchoiceUpdate p t fcs attr = go t
  where
    lf = loggS p "forkchoiceUpdate"
    waitTime = Micros 500_000
    go remaining
        | remaining <= 0 = do
            lf Warn $ "forkchoiceUpdate timed out while EVM is syncing"
            throwM $ ForkchoiceUpdatedTimeoutException t
        | otherwise = do
            lf Info $ briefJson (ForkchoiceUpdatedV3Request fcs attr)
            r <- try @(RPC.Error EngineServerErrors EngineErrors) $
                RPC.callMethodHttp @Engine_ForkchoiceUpdatedV3 (_evmEngineCtx p)
                    (ForkchoiceUpdatedV3Request fcs attr)
            case r of
                Right s -> case _forkchoiceUpdatedV1ResponsePayloadStatus s of

                    -- Syncing: retry
                    PayloadStatusV1 Syncing Nothing Nothing -> do
                        -- wait 500ms
                        lf Warn $ "EVM is SYNCING. Waiting for " <> sshow waitTime <> " microseconds"
                        threadDelay $ int waitTime
                        go (remaining - waitTime)

                    -- If the status is VALID, the latest hash is what was requested.
                    --
                    -- In particular, if the status is VALID the return latest hash
                    -- is never an ancestor of the requested hash.
                    --
                    -- If payload creation was requested the payload id is null only
                    -- if no payload attributes were provided. (If the payload
                    -- attributes are invalid an error is returned, even thought the
                    -- forkchoice state *is* updated.)
                    --
                    -- IMPORTANT NOTE:
                    --
                    -- The Engine API specification demands that, if the requested
                    -- hash was not the latest hash on the (selected) canonical
                    -- chain this returns the latest hash on the canonical chain. It
                    -- will not initiate payload creation. This behavior is not
                    -- acceptable for Chainweb consensus.
                    --
                    -- Therefore will have to patch the EVM client such that the
                    -- latest hash will be the requested hash and payload production
                    -- is initiated if it was requested.
                    --
                    PayloadStatusV1 Valid (Just _) Nothing ->
                        return (_forkchoiceUpdatedV1ResponsePayloadId s)

                    -- FIXME:
                    --
                    -- If the status is INVALID the latest valid hash is always
                    -- returned. The validationError message is optional.
                    --
                    PayloadStatusV1 Invalid (Just h) e ->
                        throwM $ InvalidPayloadException h e

                    -- This includes
                    -- - PayloadStatusV1 Invalid Nothing _
                    e ->
                        throwM $ UnexpectedForkchoiceUpdatedResponseException e

                -- According to Engine Spec [8, point 7]
                -- {error: {code: -38003, message: "Invalid payload attributes"}}
                -- is returned even when forkchoiceState has been updated, but
                -- payload attributes were invalid.
                --
                -- NOTE: we must MUST address this case and update the state!
                --
                Left (RPC.Error (RPC.ApplicationError InvalidPayloadAttributes) msg Nothing) -> do

                    -- FIXME:
                    -- how to we signal this to caller, in particular,
                    -- consensus? If this goes undetected mining may get stuck.
                    -- If this is due to a timestamp being the same as the
                    -- previous block, we'll have to figure out what to do about
                    -- it. Maybe patching reth?
                    --
                    -- For now we log an Error.
                    --
                    T2 cur _ <- stateIO p
                    lf Error $ msg <> ": forkchoiceUpdate succeeded but no payload is produced, because of invalid payload attributes. This is most likely due to a non-increasing timestamp"
                    lf Error $ encodeToText $ object
                        [ "forkchoiceState" .= fcs
                        , "payloadAttributes" .= attr
                        , "previousState" .= cur
                        , "response" .= r
                        ]
                    return Nothing

                Left e -> throwM e

-- | Calls forkchoiceUpdate and update the provider state.
--
-- NOTE: This must be called only for consensus state that have been validated
-- against the evaluation context. In particular, this is the case for the
-- @_evmState@ and for any payload in the payload db.
--
-- NOTE: If the result status is VALID the returned latest valid hash is always
-- the requested for the requested block. A different latest valid hash may be
-- returned if and only if the result status is INVALID.
--
updateEvm
    :: Logger logger
    => EvmPayloadProvider logger
    -> ConsensusState
        -- ^ the requested fork choice state. This state *MUST* have been
        -- validated with respect to the evaluation context.
    -> Maybe NewBlockCtx
    -> [(RankedBlockPayloadHash, Payload)]
        -- ^ Payloads that are added to the database if EVM validation succeeds.
        -- These payloads must already be validated with respect to the
        -- evaluation context.
        --
        -- FIXME: add a newtype wrapper for (Pre-)Validated Payloads and guard
        -- forkchoiceUpdate and database insertion by it.
        --
    -> IO ()
updateEvm p state nctx plds = lookupConsensusState p state plds >>= \case
        Nothing -> do
            lf Info $ "Consensus state lookup returned nothing for" <> briefJson state
            return ()
        Just fcs -> do
            pt <- parentTimestamp
            pid <- forkchoiceUpdate p forkchoiceUpdatedTimeout fcs (attr pt)

            -- forkchoiceUpdate throws if it does not succeed.

            -- add new payloads to payload database
            lf Info $ "new payloads added to database: " <> sshow (length plds)
            mapM_ (uncurry (tableInsert (_evmPayloadStore p))) plds

            -- Update State and Payload Id:
            -- There is a race here: If we fail updating the variable
            -- the EVM is ahead. That could cause payload updates to
            -- fail and possibly stale mining.
            lf Info "update state and payload ID variable"
            void $ atomically $ do
                -- update state
                writeTVar (_evmState p) (T2 state nctx)
                -- update payloadId
                case pid of
                    Nothing -> void $ tryTakeTMVar (_evmPayloadId p)
                    Just x -> writeTMVar (_evmPayloadId p) x
  where
    lf = loggS p "updateEvm"
    attr pt = mkPayloadAttributes (_latestBlockHash state) pt
        <$> _evmMinerAddress p
        <*> nctx

    -- This is available either from the provided new payloads or from the
    -- database. In any case, it /must/ be a source that has been validated with
    -- respect to the evaluation context.
    parentTimestamp = do
        let lrh = latestRankedBlockPayloadHash state
        hdr <- case lookup lrh plds of
            Just x -> return x
            Nothing -> tableLookup p lrh >>= \case
                Nothing -> error $ "Chainweb.PayloadProvider.EVM.updateEvm: failed to find EVM payload header for target " <> sshow lrh
                Just x -> return x
        return $ EVM._hdrTimestamp hdr

mkPayloadAttributes
    :: Chainweb.BlockHash
        -- ^ ParentBeaconBlockRoo, i.e. the Chainweb block hash of the parent of
        -- the new block.
    -> Timestamp
        -- ^ The timestamp of the /parent/ EVM header.
    -> EVM.Address
    -> NewBlockCtx
    -> PayloadAttributesV3
mkPayloadAttributes ph parentTimestamp addr nctx = PayloadAttributesV3
    { _payloadAttributesV3parentBeaconBlockRoot = EVM.chainwebBlockHashToBeaconBlockRoot ph
    , _payloadAttributesV2 = PayloadAttributesV2
        { _payloadAttributesV2Withdrawals = [withdrawal]
        , _payloadAttributesV1 = PayloadAttributesV1
            { _payloadAttributesV1Timestamp = et
            , _payloadAttributesV1SuggestedFeeRecipient = addr
            , _payloadAttributesV1PrevRandao = randao
            }
        }
    }
  where
    MinerReward reward = _newBlockCtxMinerReward nctx
    withdrawal = WithdrawalV1
        { _withdrawalValidatorIndex = 0
        , _withdrawalIndex = 0
        , _withdrawalAmount = int $ reward
        , _withdrawalAddress = addr
        }

    -- FIXME: are there an assumptions about this value?
    -- I guess, for now this fine -- better than something that looks random.
    randao = Utils.Randao (Ethereum.encodeLeN 0)

    et = EVM.timestamp parentTimestamp (_newBlockCtxParentCreationTime nctx)

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

newPayloadTimeout :: Int
newPayloadTimeout = 30_000_000

-- | If a payload id is available, new payloads for it.
--
-- This is called only if payload creation is enabled in the configuration.
--
awaitNewPayload :: Logger logger => EvmPayloadProvider logger -> IO ()
awaitNewPayload p = do
    lf Debug "await new payload ID"
    awaitPid >>= \case
        Nothing -> do
            lf Warn "timeout while waiting for new payloadID"

            -- DEBUG
            T2 state bctx <- stateIO p
            pld <- latestPayloadIO p
            lf Warn $ briefJson state
            lf Warn $ briefJson bctx
            lf Warn $ briefJson pld
            -- FIXME
            -- We are probalby stuck. What can we do? Call forkchoiceUpdate
            -- again? Or we just do nothing? Maybe it should be the job of
            -- consensus to reissue a syncToBlock and get things going again?
            --
            -- Well, let's just give it a try.
            --
            -- Again, there is a race here, but we are already in trouble. So,
            -- let's just ignore it for now.
            -- T2 s nbctx <- readTVarIO (_evmState p)
            -- updateEvm p s nbctx []

            -- FIXME FIXME FIXME but let's also make sure that we actally need
            -- this whole timeout thing.

        Just x -> go x
  where
    lf = loggS p "awaitNewPayload"
    ctx = _evmEngineCtx p
    v = _chainwebVersion p
    cid = _chainId p

    fees v1 = Stu $ bf * gu
      where
        EVM.BaseFeePerGas bf = _executionPayloadV1BaseFeePerGas v1
        GasUsed gu = _executionPayloadV1GasUsed v1

    -- Wait for payload from the execution client
    -- FIXME not sure if the timeout is a good idea...
    awaitPid = do
        timeout <- registerDelay newPayloadTimeout
        atomically $
            Nothing <$ (readTVar timeout >>= guard)
            <|>
            Just <$> takeTMVar (_evmPayloadId p)

    -- process the new payload
    go pid = do

        lf Debug $ "got payload ID " <> encodeToText [pid]
        resp <- RPC.callMethodHttp @Engine_GetPayloadV3 ctx [pid]
        lf Debug $ "got execution payload for payload ID " <> toText pid

        -- FIXME if this fails with unknown payload, we need to issues a new
        -- forkchoiceUpdate in order to obtain a new payload. Otherwise mining gets
        -- stuck.

        -- Response data
        let v3 = _getPayloadV3ResponseExecutionPayload resp
            v2 = _executionPayloadV2 v3
            v1 = _executionPayloadV1 v2
            h = EVM.numberToHeight $ _executionPayloadV1BlockNumber v1
            newEvmBlockHash = _executionPayloadV1BlockHash v1

        -- check that this is a new payload
        atomically (tryReadTMVar (_evmPayloadVar p)) >>= \case
            Just (T2 curEvmBlockHash _)
                | curEvmBlockHash == newEvmBlockHash -> do
                    lf Info $ "the new execution payload is the same as the current payload. No update."
                    return ()
            x -> do
                lf Debug $ "checking new execution payload for " <> toText pid
                    <> "; execution payload: " <> briefJson resp

                sstate <- latestStateIO p

                -- get revision number (zero if this is the first payload)
                let n = maybe 0 (succ . _newPayloadNumber . ssnd) x

                -- check that Blobs bundle is empty
                unless (null $ _blobsBundleV1Blobs $ _getPayloadV3ResponseBlobsBundle resp) $
                    throwM BlobsNotSupported

                -- Check that the block height matches the expected height
                unless ((_syncStateHeight sstate + 1) == h) $
                    throwM $ InvalidNewPayloadHeight (Expected (_syncStateHeight sstate + 1)) (Actual h)

                -- Check that the fees of the execution paylod match the block
                -- value of the response.
                -- FIXME FIXME FIXME
                -- unless (EVM._blockValueStu (_getPayloadV3ResponseBlockValue resp) == fees v1) $
                --     throwM InconsistentNewPayloadFees
                --         { _inconsistentPayloadBlockValue = _getPayloadV3ResponseBlockValue resp
                --         , _inconsistentPayloadFees = fees v1
                --         }

                let pld = executionPayloadV3ToHeader (_syncStateBlockHash sstate) v3

                -- Check that the computed block hash matches the hash from the
                -- response
                unless (newEvmBlockHash == EVM._hdrHash pld) $ do
                    lf Warn $ brief (_syncStateBlockHash sstate, _syncStateHeight sstate)
                    throwM $ InconsistenNewPayloadHash
                        (Expected newEvmBlockHash)
                        (Actual (EVM._hdrHash pld))

                lf Info $ "got new payload " <> briefJson pld

                -- The actual payload header is included in the NewBlock
                -- structure in as EncodedPayloadData.
                lf Debug $ "write new payload to payload var for evm hash " <> briefJson newEvmBlockHash
                atomically $ writeTMVar (_evmPayloadVar p) $ T2 newEvmBlockHash NewPayload
                    { _newPayloadTxCount = int $ length (_executionPayloadV1Transactions v1)
                    , _newPayloadSize = int $ sum $ (BS.length . _transactionBytes)
                            <$> (_executionPayloadV1Transactions v1)
                    , _newPayloadParentHeight = _syncStateHeight sstate
                    , _newPayloadParentHash = _syncStateBlockHash sstate
                    , _newPayloadBlockPayloadHash = EVM._hdrPayloadHash pld
                    , _newPayloadOutputSize = 0
                    , _newPayloadNumber = n
                    , _newPayloadFees = fees v1
                    , _newPayloadEncodedPayloadOutputs = Nothing
                    , _newPayloadEncodedPayloadData = Just (EncodedPayloadData $ putRlpByteString pld)
                    , _newPayloadChainwebVersion = v
                    , _newPayloadChainId = cid
                    }
        threadDelay newPayloadRate

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

withLock :: MVar () -> IO a -> IO a
withLock l a = withMVar l (const a)

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
evmSyncToBlock p hints forkInfo = withLock (_evmLock p) $ do
    T2 curState _ <- stateIO p
    lf Debug $ "current state: " <> briefJson curState <> "; target state: " <> briefJson trgState
    if trgState == curState
      then do

        -- We are are already at the target state, but maybe payload production
        -- is requested.
        --
        -- FIXME do this only when new payload building is requested
        --
        lf Info $ "current state requested: " <> brief (_consensusStateLatest curState)
        updateEvm p curState pctx []

      else do
        -- Otherwise we'll take a look at the forkinfo trace

        -- lookup the fork info base payload hash
        --
        -- NOTE we must query the local store directly and not use the P2P
        -- network.
        --
        let rankedBaseHash = _forkInfoBaseRankedPayloadHash forkInfo
        tableLookup p rankedBaseHash >>= \case

            -- If we don't know that base, there's nothing we can do
            Nothing -> do
                lf Warn $ "unknown base " <> brief rankedBaseHash
                lf Info $ sshow forkInfo

            Just _ -> case trace of
                -- Case of an empty trace:
                --
                -- The base is the same as the target. The table lookup could
                -- only succeed if we evaluated the target state before. If it
                -- is in our database we can attempt to sync to it, which may
                -- either succeed or fail.
                --
                [] -> do
                    lf Debug $ "empty trace"
                    updateEvm p trgState pctx []

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

                    lf Debug $ "unknown blocks in context: " <> sshow (length unknowns)

                    -- fetch all unknown payloads
                    --
                    -- FIXME do the right thing here. Ideally, fetch all
                    -- unknowns in batches without redundant local lookups. Then
                    -- validate all payloads together before sending them to the
                    -- EVM and inserting them into the DB.
                    --

                    plds <- forM unknowns $ \ctx -> do
                        pld <- getPayloadForContext p hints ctx
                        -- FIXME FIXME FIXME
                        validatePayload p pld ctx
                        return (_evaluationCtxRankedPayloadHash ctx, pld)

                    lf Debug $ "fetched payloads for unknowns: " <> sshow (length plds)

                    updateEvm p trgState pctx plds

                    -- remove plds from canidate cache (anything in the db
                    -- shoudl be removed)

                    newState <- sfst <$> stateIO p
                    lf Debug $ "done validating payloads. New state is " <> briefJson newState

    -- TODO cleeanup. In particular prune candidate store
    pruneCandidates p
    sfst <$> stateIO p
  where
    lf = loggS p "syncToBlock"
    trgState = _forkInfoTargetState forkInfo
    trace = _forkInfoTrace forkInfo
    pctx = _forkInfoNewBlockCtx forkInfo

-- | The depth at which the candidate cache is pruned. We prune after each
-- successful syncToBlock call. Most of the time this cache is very small and
-- pruning should be very fast.
--
-- * In normal operation, that just extends the chain, the a pruning depth of 0 is
--   fine
-- * For "normal" reorgs the diameter of the graph is sufficient.
--
-- FIXME  Make this depend on the catchup step size.
--
-- * Remove items from this cache that are in the validated database.
--
-- * For reorgs that are due to network forks/partitions the depth of the
--   catchup would be optimal, which is bounded by the reorg limit. For now we
--   ignore that case. We want a solution where we don't have permanent cost
--   overhead for this exceptional scenario.
--
candidatePruningDepth :: EvmPayloadProvider logger -> BlockHeight -> BlockHeight
candidatePruningDepth p h = int $ diameter (chainGraphAt (_chainwebVersion p) h)

pruneCandidates :: EvmPayloadProvider logger -> IO ()
pruneCandidates p = do
    lrh <- latestRankedBlockPayloadHash . sfst <$> stateIO p
    let h = _rankedHeight lrh
    deleteLt (_evmCandidatePayloads p) lrh
        { _rankedHeight = h - candidatePruningDepth p h
        }

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
    lf = loggS p "getPayloadForContext"

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
-- Payload Provider API Instance

instance Logger logger => PayloadProvider (EvmPayloadProvider logger) where

    -- FIXME
    prefetchPayloads _ _ _ = return ()
    syncToBlock = evmSyncToBlock
    latestPayloadSTM p = ssnd <$> readTMVar (_evmPayloadVar p)
    eventProof = getSpvProof

-- -------------------------------------------------------------------------- --
-- SPV

-- | Obtains all RpcReceipts for a block. It is an exception if the block can
-- not be found.
--
-- Returns: a list of EVM RpcReceipts
--
-- Throws:
--
-- * EvmHeaderNotFoundException
-- * Eth RPC failures
-- * JSON RPC failures
--
-- FIXME: filter for topic
--
getLogEntries
    :: EvmPayloadProvider logger
    -> EVM.BlockNumber
    -> IO [RpcLogEntry]
getLogEntries p n = do
    callMethodHttp
        @Eth_GetLogs (_evmEngineCtx p)
            [ object
                [ "fromBlock" .= DefaultBlockNumber n
                , "toBlock" .= DefaultBlockNumber n
                ]
            ]

getLogEntry
    :: EvmPayloadProvider logger
    -> XEventId
    -> IO LogEntry
getLogEntry p e = do
    -- it would be nice if we could just use the long entry index, but that is
    -- over the whole block and not just the tx
    logs <- getLogEntries p (int $ _xEventBlockHeight e)
    case filter ftx logs of
        [] -> throwM $ InvalidTransactionIndex e
        tx -> case tx L.!? (int $ _xEventEventIndex e) of
            Nothing -> throwM $ InvalidEventIndex e
            Just l -> return $ fromRpcLogEntry l
  where
    ftx RpcLogEntry { _rpcLogEntryTransactionIndex = TransactionIndex l } =
        l == int (_xEventTransactionIndex e)

getSpvProof
    :: Logger logger
    => EvmPayloadProvider logger
    -> XEventId
    -> IO SpvProof
getSpvProof p e = do
    le <- getLogEntry p e
    lf Info $ "got logEntry: " <> encodeToText le
    ld <- parseXLogData (_chainwebVersion p) (_xEventBlockHeight e)  le
    lf Info $ "got logData: " <> sshow ld
    return $ SpvProof $ object
        [ "origin" .= object
            [ "chainId" .= _chainId p
            , "contract" .= _xLogDataSenderAddress ld
            , "height" .= _xEventBlockHeight e
            , "transactionIdx" .= _xEventTransactionIndex e
            , "eventIdx" .= _xEventEventIndex e
            ]
        , "targetChainId" .= _xLogDataTargetChain ld
        , "targetContract" .= _xLogDataTargetContract ld
        , "operationName" .= _xLogDataOperationName ld
        , "data" .= _xLogDataMessage ld
        ]
  where
    lf = loggS p "getSpvProof"

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
-- ATTIC
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Engine Calls

getHashByNumber
    :: JsonRpcHttpCtx
    -> DefaultBlockParameter
    -> IO (Maybe EVM.BlockHash)
getHashByNumber ctx p = fmap EVM._hdrHash <$>
    callMethodHttp @Eth_GetBlockByNumber ctx (p, False)

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
    :: EvmPayloadProvider pdf
    -> EVM.BlockHash
    -> IO Payload
getBlockByHash p h = do
    r <- RPC.callMethodHttp
        @Eth_GetBlockByHash (_evmEngineCtx p) (h, False)
    case r of
        Just hdr -> return hdr
        Nothing -> throwM $ EvmHeaderNotFoundByHash h

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
    :: EvmPayloadProvider pdf
    -> EVM.BlockNumber
    -> IO Payload
getBlockAtNumber p n = do
    r <- RPC.callMethodHttp
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
    :: EvmPayloadProvider pdf
    -> IO Payload
getGenesisHeader p = try (getBlockAtNumber p 0) >>= \case
    Left (EvmHeaderNotFoundByNumber _) -> throwM EvmGenesisHeaderNotFound
    Left e -> throwM e
    Right x -> return x

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
    :: EvmPayloadProvider pdb
    -> ConsensusState
    -> IO Payload
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
    :: EvmPayloadProvider pdb
    -> ConsensusState
    -> IO Payload
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
    :: EvmPayloadProvider pdb
    -> ConsensusState
    -> IO Payload
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
    :: EvmPayloadProvider pdb
    -> ConsensusState
    -> IO EVM.BlockHash
getLatestHash p = fmap (view EVM.hdrHash) . getLatestHdr p

-- | Get the hash for the safe block in a consensus state.
--
-- Cf. 'getSafeHeader' for details.
--
getSafeHash
    :: EvmPayloadProvider pdb
    -> ConsensusState
    -> IO EVM.BlockHash
getSafeHash p = fmap (view EVM.hdrHash) . getSafeHdr p

-- | Get the hash for the final block in a consensus state.
--
-- Cf. 'getFinalHeader' for details.
--
getFinalHash
    :: EvmPayloadProvider pdb
    -> ConsensusState
    -> IO EVM.BlockHash
getFinalHash p = fmap (view EVM.hdrHash) . getFinalHdr p
