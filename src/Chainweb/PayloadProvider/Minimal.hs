{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
( MinimalPayloadProvider
) where

import Chainweb.BlockPayloadHash
import Chainweb.Logger
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.Minimal.Payload
import Chainweb.PayloadProvider.Minimal.PayloadDB qualified as PDB
import Chainweb.PayloadProvider.P2P
import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Version
import Control.Concurrent.STM
import Control.Monad.Identity
import Data.Proxy
import GHC.Generics (Generic)
import Servant.Client
import Chainweb.PayloadProvider.P2P qualified as Rest
import Chainweb.PayloadProvider.P2P.RestAPI qualified as Rest
import Chainweb.PayloadProvider.P2P.RestAPI.Client qualified as Rest
import Network.HTTP.Client qualified as HTTP
import Control.Lens
import Control.Monad
import Chainweb.Utils.Serialization

-- -------------------------------------------------------------------------- --

data MinimalPayloadProvider tbl = MinimalPayloadProvider
    { _minimalChainwebVersion :: !ChainwebVersion
    , _minimalChainId :: !ChainId
    , _minimalPayloadVar :: !(TMVar NewPayload)
    , _minimalMinerInfo :: !Account
    , _minimalPayloadStore :: !(PayloadStore tbl Payload)
    }
    deriving (Generic)

newMinimalPayloadProvider
    :: Logger logger
    => HasChainwebVersion v
    => HasChainId c
    => logger
    -> v
    -> c
    -> RocksDb
    -> HTTP.Manager
    -> Account
    -> IO (MinimalPayloadProvider (PDB.PayloadDb RocksDbTable))
newMinimalPayloadProvider logger v c rdb mgr minerAccount
    | payloadProviderTypeForChain v c /= MinimalProvider =
        error "Chainweb.PayloadProvider.Minimal.PayloadDB.configuration: chain does not use minimal provider"
    | otherwise = do
        pdb <- PDB.initPayloadDb $ PDB.configuration v c rdb
        store <- newPayloadStore mgr (logFunction logger) pdb payloadClient
        var <- newEmptyTMVarIO
        return MinimalPayloadProvider
            { _minimalChainwebVersion = _chainwebVersion v
            , _minimalChainId = _chainId c
            , _minimalPayloadVar = var
            , _minimalMinerInfo = minerAccount
            , _minimalPayloadStore = store
            }

instance HasChainwebVersion (MinimalPayloadProvider pld) where
    _chainwebVersion = _minimalChainwebVersion

instance HasChainId (MinimalPayloadProvider pld) where
    _chainId = _minimalChainId

instance
    (ReadableTable pdb RankedBlockPayloadHash Payload)
    => ReadableTable (MinimalPayloadProvider pdb) RankedBlockPayloadHash Payload
  where
    tableLookup = tableLookup . _minimalPayloadStore
    tableLookupBatch' s = tableLookupBatch' (_minimalPayloadStore s)
    tableMember = tableMember . _minimalPayloadStore

instance
    (Table pdb RankedBlockPayloadHash Payload)
    => Table (MinimalPayloadProvider pdb) RankedBlockPayloadHash Payload
  where
    tableInsert = tableInsert . _minimalPayloadStore
    tableInsertBatch s = tableInsertBatch (_minimalPayloadStore s)
    tableDelete s = tableDelete (_minimalPayloadStore s)
    tableDeleteBatch s = tableDeleteBatch (_minimalPayloadStore s)

-- -------------------------------------------------------------------------- --
-- Validate Payload

validatePayload
    :: MinimalPayloadProvider pdb
    -> Payload
    -> EvaluationCtx
    -> IO Payload
validatePayload p pld ctx = do
    guard $ _chainwebVersion p == _chainwebVersion pld
    guard $ _chainId p == _chainId pld

    -- FIXME:
    -- * do we have to check the size of chain ids? Should we enforce that the
    --   chain id exists in the current graph?

    -- guard $ length (view payloadRedeemAccount pld) < maxAccountSize

    -- _evaluationCtxParentCreationTime: there's no notion of time in the
    -- minimal payload provider.

    -- _evaluationCtxParentHash: the parent hash is not reflected in the
    -- payload. Payloads uniquly identify a block via version, chainid, and
    -- height.

    -- _evaluationCtxParentHeight
    guard $ _evaluationCtxParentHeight ctx + 1 == view payloadBlockHeight pld

    guard $ _evaluationCtxMinerReward ctx == view payloadMinerReward pld

    guard $ _evaluationCtxPayloadHash ctx == view payloadHash pld

    case _evaluationCtxPayloaddData ctx of
        Nothing -> return ()
        Just (EncodedPayloadData x) -> guard $ runGetS decodePayload x == Just pld

    return pld

    -- { _evaluationCtxParentCreationTime :: !BlockCreationTime
    --     -- ^ Creation time of the parent block. If transactions in the block
    --     -- have a notion of "current" time, they should use this value.
    -- , _evaluationCtxParentHash :: !BlockHash
    --     -- ^ Block hash of the parent block.
    -- , _evaluationCtxParentHeight :: !BlockHeight
    --     -- ^ Block height of the parent block.
    -- , _evaluationCtxMinerReward :: !MinerReward
    --     -- ^ The miner reward that is assigned to the miner of the block. Miner
    --     -- rewards are not constant and determined by the consensus protocol. It
    --     -- depends on the block height and the respective chain graph.
    --     --
    --     -- The payload provider must validate for each block that the reward is
    --     -- correct.
    --     --
    --     -- On payload provider API level the amount is provided in Stu and
    --     -- encoded as unsigned 64 bit integer value in little endian encoding.
    --     --
    --     -- Internally, encoding and unit is provider specific. For Pact it is a
    --     -- Kda value for the EVM provider it is in Stu. Also the recipient and
    --     -- the mechanism how it is credited is provider specific.
    -- , _evaluationCtxPayloadHash :: !BlockPayloadHash
    --     -- ^ Payload hash of the block that is validated. This is used as a
    --     -- checksum for the payload validation. For the last block of a ForkInfo
    --     -- structure this value must match the respective value in the target
    --     -- sync state.
    --     --
    --     -- The BlockPayloadHash is first computed when the respective payload is
    --     -- created for mining and before it is included in a block.
    -- , _evaluationCtxPayloaddData :: !(Maybe EncodedPayloadData)
    --     -- ^ Optional external payload data. This may be
    --     -- the complete, self contained block payload or it may just contain
    --     -- complementary data that aids with the validation.
    --     --
    --     -- The main purpose of this field is to allow consensus to gossip around
    --     -- payload data along with new cuts in the P2P network, which allows for
    --     -- more efficient synchronization and a reduction of block propagation
    --     -- latencies.

-- -------------------------------------------------------------------------- --

payloadClient
    :: ChainwebVersion
    -> ChainId
    -> RankedBlockPayloadHash
    -> ClientM Payload
payloadClient v c h = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $! Rest.payloadClient @v @c @'MinimalProvider h

-- fetchAndVerifyPayload
--     :: MinimalProvider pdb
--     -> EvaluationCtx
--     -> IO EVM.Header
-- fetchAndVerifyPayload p evalCtx = do
--   where


-- | Obtain payloads for all entries of the Evaluation Ctx.
--
-- The payload is first looked up in the local payload db. If it is not
-- available locally it is fetched in the P2P network, validated against the
-- evaluation context, and inserted into the database.
--

-- FIXME We need to ensure that Payload Store validates context before inserting
-- them into the db:
--         Nothing -> throwM $ Payload
--             pld <- fetchPayloadFromRemote
--             validateCtx evalCtx pld
--             return pld
--         Just pld -> return pld

-- -------------------------------------------------------------------------- --
-- Payload Provider API

-- | FIXME: do something smarter here
--

































































































































































minimalPrefetchPayloads :: MinimalPayloadProvider pdb -> Maybe Hints -> ForkInfo -> IO ()
minimalPrefetchPayloads p h i = return ()

minimalSyncToBlock
    :: MinimalPayloadProvider pld
    -> Maybe Hints
    -> ForkInfo
    -> IO ConsensusState
minimalSyncToBlock p h i = error "Chainweb.PayloadProvider.Minimal.minimalSyncToBlock: TODO"

minimalLatestPayloadIO :: MinimalPayloadProvider pld -> IO NewPayload
minimalLatestPayloadIO = atomically . readTMVar . _minimalPayloadVar

minimalLatestPayloadStm :: MinimalPayloadProvider pld -> STM NewPayload
minimalLatestPayloadStm = readTMVar . _minimalPayloadVar

