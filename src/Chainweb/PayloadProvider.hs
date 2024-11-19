{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module: Chainweb.PayloadProvider
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider
( MinerInfo(..)
, MinerReward(..)
, SyncState(..)
, EvaluationCtx(..)
, ForkInfo(..)
, PayloadProvider(..)
-- , EvmPayloadCtx
-- , PactPayloadCtx
) where

import Data.Decimal

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
-- import Chainweb.Payload qualified as PactPayload

-- -------------------------------------------------------------------------- --

-- | Information that is needed to credit the miner of block with block rewards
-- and other fee payments.
--
-- The format is provider specific. Typically, it includes an account name
-- and/or public key. But some providers may support more complex information,
-- like support for non-trivial key sets.
--
-- The textual format must be sufficiently documented such that miners can
-- provide this value in the configuration of a Chainweb mining node.
--
-- It is up to the payload provider to sanity check the string value (e.g.
-- enforce some safe subset of UTF-8).
--
-- Binary format: UTF-8 serialziation of the string (without BOM) prefixed with
-- the *byte* length of the string as unsigned 32 bit integer number in little
-- endian encoding.
--
data MinerInfo = MinerInfo String
    deriving (Show, Eq, Ord)

-- | The miner reward for a block. This is determined by the consensus protocol
-- and must be validated by the payload provider for each block.
--
-- The encoding of the number is provider specific. It is an open question how
-- we can guarantee that that the precision of the respective KDA value is
-- precise for all providers.
--
newtype MinerReward = MinerReward Decimal
    deriving (Show, Eq, Ord)
    deriving newtype (Enum)

-- | This identifies the block that corresponds to the current state of the
-- payload provider.
--
data SyncState = SyncState
    { _syncStateHeight :: !BlockHeight
    , _syncStateBlockHash :: !BlockHash
    }
    deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------- --
-- Payload Evaluation Context

-- | The block payload evaluation context for a given payload provider.
--
-- It contains all data that allows the provider to evaluate the payload of
-- a single block.
--
-- IMPORTANT NOTE:
-- The context is deterministcally derived from the consensus state of the
-- blockchain. The payload provider must validate that the context is correct.
-- This is particularly important for payload providers that redundantly store
-- some contextual information internally in payload data structure. Some of the
-- contextual information, namely the timestamp and the mining reward, can not
-- be derived from the previous payload. While the mining reward is a constant
-- property of the blockchain, i.e. it is determined at genesis, the timestamp
-- is determined only at mining time. If such a property is stored and remotely
-- synchronized with remote peers, the payload provider must confirm the
-- validity of this information with the local consensus state before committing
-- the the payload evaluation.
--
-- Binary format: The concatenation of the binary serialization of the
-- individual fields in the order as they appear in the data type definition.
--
data EvaluationCtx p = EvaluationCtx
    { _evaluationCtxParentCreationTime :: !BlockCreationTime
        -- ^ Creation time of the parent block. If transactions in the block
        -- have a notion of "current" time, they should use this value.
    , _evaluationCtxParentHash :: !BlockHash
        -- ^ Block hash of the parent block.
    , _evaluationCtxParentHeight :: !BlockHeight
        -- ^ Block height of the parent block.
    , _evaluationCtxMinerReward :: !MinerReward
        -- ^ The miner reward that is assigned to the miner of the block.
        -- Miner rewards are not constant and depend on the block height. It is
        -- set by consensus.
    , _evaluationCtxPayloadHash :: !BlockPayloadHash
        -- ^ Payload hash of the block that is validated. This is used as a
        -- checksum for the payload validation. This value is first computed
        -- when the respective payload is created for mining and before it is
        -- included in a block.
    , _evaluationCtxPayload :: !p
        -- ^ The payload of the block that is going to be evaluated.
    }
    deriving (Show, Eq, Ord)

-- | Synchronize the state of a payload provider to a particular fork of a
-- Chainweb chain.
--
-- This contains all data that is required for a payload provider to evaluate
-- the payloads of a consecutive sequence of blocks on top of some historic
-- provider state.
--
-- In order to create this value the client makes an educated guess about the
-- current state of the payload provider. Under normal circumstances the client
-- is able to make an accurate guess. It can fail on the first initialization of
-- a provider or on re-initialization after an ungracefull shutdown of the
-- payload provider or the client. If that happens and no suitable historic
-- state is known to the payload provider it rejects the `ForkInfo` and returns
-- the current `PayloadProviderState` to the caller. This enables the caller to
-- prepare a new `ForkInfo` value.
--
-- Binary format: the length of `_forkInfoTrace` as unsigned 32 bit integer
-- number in little endian byte order followed by the binary serialization of
-- the entries of `_forkInfoTrace`, followed by the binary serialization of
-- `forkInfoTraceHash`.
--
data ForkInfo p = ForkInfo
    { _forkInfoTrace :: ![EvaluationCtx p]
        -- ^ The payload evaluation contexts for a consecutive sequence of
        -- blocks.  The first entry determines the fork point which must be
        -- known to the payload provider (although it is not necessary that the
        -- payload provider is able to reconstruct the respective state. The
        -- provider is not obligated to replay all blocks as long as the final
        -- state is valid.
        --
        -- If evluation of the full list of payloads fails, the payload provider
        -- may choose to remain in an intermediate state, as long as that state
        -- is consistent with the evaluation of some prefix of this field.
        --
        -- The payload provider is also obligated to validate the correctness of
        -- the evaluation context with respect the payload provider state for
        -- all provided contexts in this field up to the lastest validated
        -- block.  This allows the client to requiest the evaluation and
        -- validation of a series of new blocks. The payload provider does not
        -- need to guarantee the correctness of the validation context for
        -- blocks that had been validated before. This includes all re-validated
        -- blocks that are not included in this field, because it can be assumed
        -- that those have been validated before with their respective contexts
        -- provided.
        --
        -- However, the operation for the respective evaluated prefix must
        -- satisfy the ACID criteria.
        --
    , _forkInfoTargetHash :: !BlockHash
        -- ^ The hash of the the target block. This allows the payload provider
        -- to update its `PayloadProviderState`. Intermediate block hashes are
        -- available in form of `BlockParentHash`s from the `PayloadCtx`
        -- entries.
    }
    deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------- --

-- | Payload Provider API.
--
class PayloadProvider p where

    -- | The Payload of a single block. Typically, this is a list of
    -- transactions, including some system specific transactions for rewarding
    -- miners and paying gas fees. It may also include additional metadata that
    -- affect the evaluation.
    --
    -- The content is completely provider specific. Chainweb only requires that
    -- it has a finite binary representation that can be efficiently stored,
    -- copied, and moved around. In particular, if a provider has internal ways
    -- to materialize payload contents (e.g. using an internal P2P gossip
    -- network), this type may contain only enough information to identify the
    -- payload (e.g. a hash value).
    --
    -- There are some semantic requirements about the evaluation of a block
    -- payload.  Those include the payment of rewards to the miner of a block
    -- and support for SPV proofs (both creation and verification). The details
    -- of those are beyond the scope of this document which is concerned with
    -- the syntactic aspects of the node-internal provider API.
    --
    -- Note, that the use of the term `Payload` is overloaded in the context of
    -- Chainweb. Here it refers only to the input to block validation that is
    -- provided by the payload provider. It does not include the evaluation
    -- context that is provided by the consensus component of Chainweb and also
    -- does not include the evaluation results. It also does not include the
    -- internal state of the payload provider and changes to the state that are
    -- caused by the evaluation of the payload.
    --
    -- In some case this data structure may represent only a commitment to the
    -- payload and the actual payload is synchronized out-off-band (from the
    -- viewpoint of consensus) or not needed at all (in case of ZK
    -- computations).
    --
    -- Some payload providers mix data from the evaluation context and the
    -- payload itself in a single data structure. This API can accomodate this
    -- behavior.
    --
    type Payload p

    -- | Returns the current sync state of the payload provider.
    --
    syncState :: p -> IO SyncState

    -- | Request that the payload provider updates its internal state to
    -- represent the validation of the last block in the provide `ForkInfo`.
    --
    -- If the the first entry in `ForkInfo` is not known to the payload provider
    -- this operation is a no-op and the provider returns its current sync
    -- state.
    --
    -- The payload provider may update the internal state only to a predecessor
    -- of the requested block. This can happen if, for instance, the operation
    -- times out or gets interrupted or an validation error occurs. In any case
    -- the must be valid and the respective `PayloadProviderSyncState` must be
    -- returned.
    --
    -- Independent of the actual final state, the operation must satisify ACID
    -- criteria. In particular, any intermediate state while the operation is
    -- ongoing must not be obseravable and the final state must be consistent
    -- and persistent.
    --
    syncToBlock :: p -> ForkInfo (Payload p) -> IO SyncState

    -- | Create a new block payload on top of the latests block.
    --
    -- This includes validation of the block *on top* of the given `SyncState`.
    -- If the current `SyncState` of the payload provider does not match the
    -- given 'SyncState' the function returns the current 'SyncState'. Otherwise
    -- a new payload along with the corresponding payload hash is returned.
    --
    -- This operation must be *read-only*. It must not change the observable
    -- state of the payload provider.
    --
    newPayload
        :: p
        -> SyncState
            -- ^ The current state of the payload provider.
        -> BlockCreationTime
            -- ^ The creation time of the parent block header.
        -> MinerReward
            -- ^ The miner reward for the new block, which depends on the block
            -- height
        -> MinerInfo
            -- ^ The miner info for the new block.
        -> IO (Either SyncState (Payload p, BlockPayloadHash))

-- type EvmPayloadCtx = EvaluationCtx ()
-- type PactPayloadCtx = EvaluationCtx PactPayload.PayloadData
