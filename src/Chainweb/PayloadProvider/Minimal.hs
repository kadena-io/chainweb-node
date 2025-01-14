{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeAbstractions #-}

-- |
-- Module: Chainweb.IdleProvider
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A minimal payload provider.
--
-- No user provided payload processing is supported.
--
-- Miner reward payout must be recorded in the block payload hash. Ideally, this
-- would be done in a way such that payloads could be validated just from the
-- evaluation context and the payload hash, without the need to persist and
-- synchronize additional payload data. An obvious way to do this would be to
-- use the miner pk account as payload hash. However, that would be unsafe by
-- allowing miner to attack the Chainweb SPV proofs by proving arbitrary facts.
--
-- At the moment, Chainweb SPV proofs do not generally witness provenance with
-- respect to a particular block header. If that information is needed in a
-- particular proof it must be provided on application level in the proof claim,
-- which means that it is hidden behind a Chainweb MerkleLog tag. With the
-- current proof format, there is no way for consumers of SPV proofs to verify
-- that a proof claim was made in the context of a particular payload provider.
-- Consequently, _all_ data in the preimage of a Merkle root must be tagged and
-- preimages of all Merkle roots in the Chainweb Merkle tree must be verified
-- down to the tag level by miners.
--
-- This means that Validation of a block header can be selfcontained only if all
-- data in the preimage of the payload hash is deterministically derived from
-- the header data.
--
-- There is no way to encode the miner account in the block header in a
-- practical way. Therefore, the requirement to include the miner account (or
-- some data that is derived from the miner account) in the preimage of the
-- payload hash implies that header validation cannot be selfcontained and we
-- need to introduce payloads that are persisted and synchronized between nodes.
--
-- Additional Remarks:
--
-- More generally, the above also means that if a node user decides to not
-- validate payloads on all chains, that they blindly trust the miners in the
-- system to validate all payloads on all chains each time they verify an SPV
-- proof. In particular, it means that a majority of miners *must* validate all
-- payloads on all chains!
--
-- This may be actually a strong argument why it is good that over 50% of the
-- chains are blocked most of the time. One can probably establish a Ramsey
-- style argument that a each chain must be covered by a majority of miners or
-- otherwise some miners would risk to have their devices blocked sometimes.
--
module Chainweb.PayloadProvider.Minimal
( MinimalProviderConfig(..)
, defaultMinimalProviderConfig
, validateMinimalProviderConfig
, pMinimalProviderConfig
, MinimalPayloadProvider
, minimalPayloadDb
, minimalPayloadQueue
, newMinimalPayloadProvider
) where

import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.Logger
import Chainweb.MinerReward
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.Minimal.Payload
import Chainweb.PayloadProvider.Minimal.PayloadDB qualified as PDB
import Chainweb.PayloadProvider.P2P
import Chainweb.PayloadProvider.P2P qualified as Rest
import Chainweb.PayloadProvider.P2P.RestAPI.Client qualified as Rest
import Chainweb.Storage.Table
import Chainweb.Storage.Table.HashMap
import Chainweb.Storage.Table.RocksDB
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Configuration.Utils
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except (throwError)
import Data.ByteString qualified as B
import Data.HashSet qualified as HS
import Data.PQueue (PQueue)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Numeric.Natural
import P2P.TaskQueue
import Servant.Client
import System.LogLevel
import Data.LogMessage (LogFunction, LogFunctionText)

-- -------------------------------------------------------------------------- --

data MinimalProviderConfig = MinimalProviderConfig
    { _mpcRedeemChain :: !ChainId
    , _mpcRedeemAccount :: !Account
    }
    deriving (Show, Eq, Generic)

makeLenses ''MinimalProviderConfig

defaultMinimalProviderConfig :: MinimalProviderConfig
defaultMinimalProviderConfig = MinimalProviderConfig
    { _mpcRedeemChain = unsafeChainId 0
    , _mpcRedeemAccount = invalidAccount
        -- Most likely this account is invalid on all providers. If the account
        -- is invalid on the redeem chain, the reward can't be claimed. So, most
        -- likely, this burns the miner rewards.
    }

instance ToJSON MinimalProviderConfig where
    toJSON o = object
        [ "redeemChain" .= _mpcRedeemChain o
        , "redeemAccount" .= _mpcRedeemAccount o
        ]

instance FromJSON MinimalProviderConfig where
    parseJSON = withObject "MinimalProviderConfig" $ \o ->
        MinimalProviderConfig
            <$> o .: "redeemChain"
            <*> o .: "redeemAccount"

instance FromJSON (MinimalProviderConfig -> MinimalProviderConfig) where
    parseJSON = withObject "MinimalProviderConfig" $ \o -> id
        <$< mpcRedeemChain ..: "redeemChain" % o
        <*< mpcRedeemAccount ..: "redeemAccount" % o

pMinimalProviderConfig :: MParser MinimalProviderConfig
pMinimalProviderConfig = id
    <$< mpcRedeemChain .:: pChainId
    <*< mpcRedeemAccount .:: pAccount
  where
    pChainId :: OptionParser ChainId
    pChainId = unsafeChainId <$> option auto
        % long "redeem-chain"
        <> help "chain on which block rewards from the minimal payload provider can be claimed"

    pAccount :: OptionParser Account
    pAccount = option textReader
        % long "redeem-account"
        <> help "a valid account on the redeem chain for the minimal payload provider"

validateMinimalProviderConfig
    :: HasChainwebVersion v
    => v
    -> ConfigValidation MinimalProviderConfig []
validateMinimalProviderConfig v o
    | HS.member rcid (chainIds v) = return ()
    | otherwise = do
        throwError $ mconcat
            [ "The provided redeem chain for the minimal payload provider is not a valid chain in the current chainweb"
            , " Provided value " <> sshow rcid
            ]
  where
    rcid = _mpcRedeemChain o

-- -------------------------------------------------------------------------- --

data MinimalPayloadProvider = MinimalPayloadProvider
    { _minimalChainwebVersion :: !ChainwebVersion
    , _minimalChainId :: !ChainId
    , _minimalPayloadVar :: !(TMVar NewPayload)
    , _minimalRedeemChain :: !ChainId
    , _minimalMinerInfo :: !Account
    , _minimalPayloadStore :: !(PayloadStore (PDB.PayloadDb RocksDbTable) Payload)
    , _minimalCandidatePayloads :: !(HashMapTable RankedBlockPayloadHash Payload)
        -- ^ FIXME: should this be moved into the Payload Store?
        --
        -- For now we just prune after each successful syncToBlock.
        --
        -- FIXME: is pruning actually implemented?
    , _minimalLogger :: LogFunction
    }

minimalPayloadDb :: Getter MinimalPayloadProvider (PDB.PayloadDb RocksDbTable)
minimalPayloadDb = to (_payloadStoreTable . _minimalPayloadStore)

minimalPayloadQueue :: Getter MinimalPayloadProvider (PQueue (Task ClientEnv Payload))
minimalPayloadQueue = to (_payloadStoreQueue . _minimalPayloadStore)

newMinimalPayloadProvider
    :: Logger logger
    => HasChainwebVersion v
    => HasChainId c
    => logger
    -> v
    -> c
    -> RocksDb
    -> HTTP.Manager
    -> MinimalProviderConfig
    -> IO MinimalPayloadProvider
newMinimalPayloadProvider logger v c rdb mgr conf
    | payloadProviderTypeForChain v c /= MinimalProvider =
        error "Chainweb.PayloadProvider.Minimal.configuration: chain does not use minimal provider"
    | otherwise = do
        SomeChainwebVersionT @v _ <- return $ someChainwebVersionVal v
        SomeChainIdT @c _ <- return $ someChainIdVal c
        let payloadClient h = Rest.payloadClient @v @c @'MinimalProvider h

        pdb <- PDB.initPayloadDb $ PDB.configuration v c rdb
        store <- newPayloadStore mgr (logFunction pldStoreLogger) pdb payloadClient
        var <- newEmptyTMVarIO
        candidates <- emptyTable
        logFunctionText providerLogger Info "minimal payload provider started"
        return MinimalPayloadProvider
            { _minimalChainwebVersion = _chainwebVersion v
            , _minimalChainId = _chainId c
            , _minimalPayloadVar = var
            , _minimalRedeemChain = _mpcRedeemChain conf
            , _minimalMinerInfo = _mpcRedeemAccount conf
            , _minimalPayloadStore = store
            , _minimalCandidatePayloads = candidates
            , _minimalLogger = logFunction providerLogger
            }
  where
    providerLogger = setComponent "payload-provider"
        $ addLabel ("provider", "minimal") logger
    pldStoreLogger = addLabel ("sub-component", "payloadStore") providerLogger

instance HasChainwebVersion MinimalPayloadProvider where
    _chainwebVersion = _minimalChainwebVersion

instance HasChainId MinimalPayloadProvider where
    _chainId = _minimalChainId

instance
    ReadableTable MinimalPayloadProvider RankedBlockPayloadHash Payload
  where
    tableLookup = tableLookup . _minimalPayloadStore
    tableLookupBatch' s = tableLookupBatch' (_minimalPayloadStore s)
    tableMember = tableMember . _minimalPayloadStore

instance
    Table MinimalPayloadProvider RankedBlockPayloadHash Payload
  where
    tableInsert = tableInsert . _minimalPayloadStore
    tableInsertBatch s = tableInsertBatch (_minimalPayloadStore s)
    tableDelete s = tableDelete (_minimalPayloadStore s)
    tableDeleteBatch s = tableDeleteBatch (_minimalPayloadStore s)

-- -------------------------------------------------------------------------- --
-- Validate Payload

data PayloadValidationFailure
    = PayloadInvalidChainwebVersion !(Expected ChainwebVersion) !(Actual ChainwebVersion)
    | PayloadInvalidChainId !(Expected ChainId) !(Actual ChainId)
    | PayloadInvalidMinerReward !(Expected MinerReward) !(Actual MinerReward)
    | PayloadInvalidHeight !(Expected BlockHeight) !(Actual BlockHeight)
    | PayloadInvalidHash !(Expected BlockPayloadHash) !(Actual BlockPayloadHash)
    | PayloadInvalidPayloadData !(Expected EncodedPayloadData) !(Actual EncodedPayloadData)
    deriving (Show, Eq, Generic)

instance Exception PayloadValidationFailure

-- | Validate a Payload against a given 'EvaluationCtx'.
--
-- Notes:
--
-- The type witnesses that ChainId values are valid for the latest chainweb
-- graph.
--
-- The Account type witnesses that the size of the account data is within the
-- required limits.
--
-- _evaluationCtxParentCreationTime: there's no notion of time in the minimal
-- payload provider.
--
-- _evaluationCtxParentHash: the parent hash is not reflected in the payload.
-- Payloads uniquly identify a block via version, chainid, and height.
--
validatePayload
    :: forall m
    . MonadThrow m
    => MinimalPayloadProvider
    -> Payload
    -> EvaluationCtx
    -> m ()
validatePayload p pld ctx = do
    checkEq PayloadInvalidChainwebVersion
        (_chainwebVersion p)
        (_chainwebVersion pld)
    checkEq PayloadInvalidChainId
        (_chainId p)
        (_chainId pld)
    checkEq PayloadInvalidHeight
        (_evaluationCtxParentHeight ctx + 1)
        (view payloadBlockHeight pld)
    checkEq PayloadInvalidMinerReward
        (_evaluationCtxMinerReward ctx)
        (view payloadMinerReward pld)
    checkEq PayloadInvalidHash
        (_evaluationCtxPayloadHash ctx)
        (view payloadHash pld)
    case _evaluationCtxPayloadData ctx of
        Nothing -> return ()
        Just x -> checkEq PayloadInvalidPayloadData x (encodedPayloadData pld)
  where
    checkEq
        :: Eq a
        => (Expected a -> Actual a -> PayloadValidationFailure)
        -> a
        -> a
        -> m ()
    checkEq failure expected actual =
        unless (expected == actual) $
            throwM $ failure (Expected expected) (Actual actual)

encodedPayloadData :: Payload -> EncodedPayloadData
encodedPayloadData = EncodedPayloadData . runPutS . encodePayload

encodedPayloadDataSize :: EncodedPayloadData -> Natural
encodedPayloadDataSize (EncodedPayloadData bs) = int $ B.length bs

decodePayloadData :: MonadThrow m => EncodedPayloadData -> m Payload
decodePayloadData (EncodedPayloadData bs) = runGetS decodePayload bs

-- -------------------------------------------------------------------------- --
-- Payload Provider API

instance PayloadProvider MinimalPayloadProvider where
    prefetchPayloads = minimalPrefetchPayloads
    syncToBlock = minimalSyncToBlock
    latestPayloadSTM = minimalLatestPayloadStm
    latestPayloadIO = minimalLatestPayloadIO

-- | Fetch a payload for an evaluation context and insert it into the candidate
-- table.
--
getPayloadForContext
    :: MinimalPayloadProvider
    -> Maybe Hints
    -> EvaluationCtx
    -> IO Payload
getPayloadForContext p h ctx = do
    insertPayloadData (_evaluationCtxPayloadData ctx)
    pld <- Rest.getPayload
        (_minimalPayloadStore p)
        (_minimalCandidatePayloads p)
        (Priority $ negate $ int $ _evaluationCtxParentHeight ctx)
        (_hintsOrigin <$> h)
        (_evaluationCtxRankedPayloadHash ctx)
    casInsert (_minimalCandidatePayloads p) pld
    return pld
  where
    insertPayloadData Nothing = return ()
    insertPayloadData (Just epld) = case decodePayloadData epld of
        Right pld -> casInsert (_minimalCandidatePayloads p) pld
        Left e -> do
            lf Warn $ "failed to decode encoded payload from evaluation ctx: " <> sshow e

    lf :: LogFunctionText
    lf = _minimalLogger p

-- | Concurrently fetch all payloads in an evaluation context and insert them
-- into the candidate table.
--
-- This version blocks until all payloads are fetched (or a timeout occurs).
--
-- Should we also expose a version that is fire-and-forget?
--
minimalPrefetchPayloads
    :: MinimalPayloadProvider
    -> Maybe Hints
    -> ForkInfo
    -> IO ()
minimalPrefetchPayloads p h i = do
    logg p Info "prefetch payloads"
    mapConcurrently_ (getPayloadForContext p h) $ _forkInfoTrace i

-- |
--
-- NOTE:
--
-- The mimimal Payload Provider is special in that it is stateless. Validation
-- of a payload does depend only on the evaluation context and not on the
-- history of the chain. Similarly, new block production does depend only on the
-- block height and not on current latest block.
--
-- This means that 'syncToBlock' does not need to resolve reorgs. It also means
-- that it succeeds on an empty ForkInfo trace. (Otherwise it would succeed only
-- if the requested target state matched the current state.) However, it also
-- means, that the content of the payload db could be incomplete or even
-- inconsistent by containing payloads from non-canonical forks at some block
-- heights.
--
-- If it is a requirement that the payload provider offers access to the full
-- and consistent history via the service API, the implementation of
-- 'syncToBlock' must be adjusted to resolve reorgs and guarantee a seemless
-- history.
--
minimalSyncToBlock
    :: MinimalPayloadProvider
    -> Maybe Hints
    -> ForkInfo
    -> IO ConsensusState
minimalSyncToBlock p h i = do
    logg p Info "syncToBlock called"
    validatePayloads p h i

    -- FIXME: is the right place to prune the candidate store?

    -- Produce new block
    case _forkInfoNewBlockCtx i of
        Nothing -> return ()
        Just ctx -> do
            logg p Info $ "create new payload for sync state: " <> sshow latestState
            atomically
                $ writeTMVar (_minimalPayloadVar p)
                $ makeNewPayload p latestState ctx
    return $ _forkInfoTargetState i
  where
    latestState = _consensusStateLatest $ _forkInfoTargetState i

logg :: MinimalPayloadProvider -> LogLevel -> T.Text -> IO ()
logg p l t = _minimalLogger p l t

makeNewPayload
    :: MinimalPayloadProvider
    -> SyncState
    -> NewBlockCtx
    -> NewPayload
makeNewPayload p latest ctx = NewPayload
    { _newPayloadChainwebVersion = _chainwebVersion p
    , _newPayloadChainId = _chainId p
    , _newPayloadParentHeight = _syncStateHeight latest
    , _newPayloadParentHash = _syncStateBlockHash latest
    , _newPayloadBlockPayloadHash = view payloadHash pld
    , _newPayloadEncodedPayloadData = Just epld
    , _newPayloadEncodedPayloadOutputs = Nothing
    , _newPayloadNumber = 0 -- there will only be a single output per parent
    , _newPayloadTxCount = 0
    , _newPayloadSize = encodedPayloadDataSize epld
    , _newPayloadOutputSize = 0
    , _newPayloadFees = 0
    }
  where
    pld = newPayload p p
        (_syncStateHeight latest + 1)
        (_newBlockCtxMinerReward ctx)
        (_minimalRedeemChain p)
        (_minimalMinerInfo p)
    epld = encodedPayloadData pld

-- | Fetches and validates all missing payloads for a 'ForkInfo' value.
--
-- Throws PayloadValidationFailure if a payload is not valid.
--
-- FIXME: is it fine that we can end up with a payload database where
-- some earlier payloads are missing? This would also mean that the respective
-- payloads have not been validated. This is probably fine with the current cut
-- pipeline, but this might not always be the case in the future.
--
validatePayloads
    :: MinimalPayloadProvider
    -> Maybe Hints
    -> ForkInfo
    -> IO ()
validatePayloads p h i= mapConcurrently_ go (_forkInfoTrace i)
  where
    go ctx = do
        pld <- getPayloadForContext p h ctx
        validatePayload p pld ctx
        casInsert (_minimalPayloadStore p) pld

minimalLatestPayloadIO :: MinimalPayloadProvider -> IO NewPayload
minimalLatestPayloadIO = atomically . readTMVar . _minimalPayloadVar

minimalLatestPayloadStm :: MinimalPayloadProvider -> STM NewPayload
minimalLatestPayloadStm = readTMVar . _minimalPayloadVar

