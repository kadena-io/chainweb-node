-- |
-- Module: src.Chainweb.PayloadProvider
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- https://www.notion.so/kadenateam/Payload-Providers-for-Chainweb-111e868b687880808cc4c748455bf37b?pvs=4
--
-- Payload Provider Interface for "passive Payload Providers. A passive provider
-- does not actively sync with the P2P network or other external sources. All is
-- provided via the API.
--
-- In contrast active payload providers synchronize required data autonomously,
-- typically via a P2P network. An actively payload provider still implements
-- the passive API by ignoring the content of `_forkInfoTrace`. Therefore it is
-- sufficient to just pass [].
--
module src.Chainweb.PayloadProvider
(
) where

-- -------------------------------------------------------------------------- --
-- EVM Provider Parameters
--
-- Connection:
-- *   host
-- *   port
-- *   jwt secret
--
-- API:
-- *   active
--
-- Payload Hash:
--
-- *   Merkle Tree of EL Header

-- -------------------------------------------------------------------------- --
-- Internal Pact Provider Parameters
--
-- Connection:
-- *   chain id
-- *   pactdb-path
-- *   logger
--
-- API:
-- *   passive
--
-- Payload Hash
-- *   Merkle Tree of PayloadData


-- -------------------------------------------------------------------------- --
-- Administrative

-- Payload Provider Interface
--
-- *   initialize
-- *   release
--
-- Parameters:
--
-- *   payloadDb
-- *   cutDb
-- *   cid
-- *   logger
-- *   historyLimit

-- In order to keep things simple we initialize all payload providers with the
-- same parameters. Payload provider will pick just what they need.
--
--


-- -------------------------------------------------------------------------- --
-- Number Types

-- | The creation time of the block measured in microseconds since POSIX epoch.
-- This is determined during the mining processes and recorded in the block
-- header.
--
-- Binary format: unsigned 64 bit integral number in little endian byte order.
--
newtype BlockTime = BlockTime Word64
    deriving (Eq, Ord, Enum)

-- | The consecutive sequence number of a block since genesis.
--
-- Block height is a global property of a chainweb. All parents, including parents
-- of adjacent chains have the same block height. This means that morally all
-- chains in a chainweb were created at genesis, even those chains that got added
-- later on. And blocks get counted on all chains independent on whether those
-- blocks get mineded and exist physically.
--
-- Binary format: unsigned 64 bit integral number in little endian byte order.
--
newtype BlockHeight = BlockHeight Word64
    deriving (Eq, Ord, Enum)

-- -------------------------------------------------------------------------- --
-- Payload

-- | The Payload of a single block. Typically, this is a list of transactions,
-- including some system specific transactions for rewarding miners and paying
-- gas fees. It may also include additional metadata that affect the evaluation.
--
-- The content is completely provider specific. Chainweb only requires that it has
-- a finite binary representation that can be efficiently stored, copied, and
-- moved around. In particular, if a provider has internal ways to materialize
-- payload contents (e.g. using an internal P2P gossip network), this type may
-- contain only enough information to identify the payload (e.g. a hash value).
--
-- There are some semantic requirements about the evaluation of a block payload.
-- Those include the payment of rewards to the miner of a block and support
-- for SPV proofs (both creation and verification). The details of those are
-- beyond the scope of this document which is concerned with the syntactic
-- aspects of the node-internal provider API.
--
-- Note, that the use of the term `Payload` is overloaded in the context of
-- Chainweb. Here it refers only to the input to block validation that is provided
-- by the payload provider. It does not include the evaluation context that is
-- provided by the consensus component of Chainweb and also does not include the
-- evaluation results. It also does not include the internal state of the payload
-- provider and changes to the state that are caused by the evaluation of the
-- payload.
--
-- Binary format: variable length byte array prefixed with the length of the
-- payload in bytes as an unsigned 32 bit integer number in little endian byte
-- order.
--
newtype Payload = Payload ByteArray
    deriving ()

-- | Information that is needed to credit the miner of block with block rewards and
-- other fee payments.
--
-- The format is provider specific. Typically, it includes an account name and/or
-- public key. But some providers may support more complex information, like
-- support for non-trivial key sets.
--
-- The textual format must be sufficiently documented such that miners can provide
-- this value in the configuration of a Chainweb mining node.
--
-- It is up to the payload provider to sanity check the string value (e.g. enforce
-- some safe subset of UTF-8).
--
-- Binary format: UTF-8 serialziation of the string (without BOM) prefixed with
-- the *byte* length of the string as unsigned 32 bit integer number in little
-- endian encoding.
--
data MinerInfo = MinerInfo String
    deriving ()

-- -------------------------------------------------------------------------- --
-- Merkle Roots

-- | The root of a Merkle tree.
--
-- This is usually a cryptographic hash value for which inclusion of the data from
-- the preimage can be proven. The details are specific for the component that owns
-- the respective data.
--
-- Chainweb only requires that the value can be tested for equality and that the
-- binary represention has exactly 32 bytes.
--
newtype MerkleRoot = MerkleRoot ByteArray
    deriving (Eq)

-- | The Merkle root of a Chainweb block header.
--
-- This is computed after the block is mined. It authenticates all data that is
-- included in the block and all predecessors of the block on the same chain
-- and other chains in the Chainweb. This also includes all payloads and payload
-- evaluation results of those blocks. It also supports the creation and
-- verification of Merkle proofs for that data.
--
-- Binary format: 32 bytes.
--
newtype BlockHash = BlockHash MerkleRoot
    deriving (Eq)

-- | The Merkle root of a block payload evaluation
--
-- NOTE: for historic reasons this is called `PayloadHash`. A more accurate name
-- would be `PayloadEvaluationHash`.
--
-- This is computed by payload provider of the respective block payload. It is
-- treated by Chainweb consensus as the root of a Chainweb Merkle (sub-) tree.
-- It is the responsibility of the payload provider that this interpretation is
-- cryptographically sound.
--
-- Semantically, the hash must completely authenticate the block payload and
-- payload evaluation results, including all updates to the internal state of the
-- payload provider (but not complete state itself).
--
-- It is not required to authenticate the complete internal state of the payload
-- provider. (Although it is strongly recommended that payload providers support
-- this by including a state root into the payload Merkle tree. Pact currently does
-- not support this.)
--
-- Beside of unambiguously authenticating the evaluation of the payload, it is up
-- to the respective payload provider to decide what cryptographic protocol is
-- used to compute this value and what can be proven about the payload.
--
-- Binary format: 32 bytes.
--
newtype PayloadHash = PayloadHash MerkleRoot
    deriving (Eq)

-- -------------------------------------------------------------------------- --
-- Protocol Data Types

-- | The block payload evaluation context for a given payload provider.
--
-- It contains all data that allows the provider to evaluate the paylaod of
-- a single block.
--
-- Binary format: The concatenation of the binary serialization of the
-- individual fields in the order as they appear in the data type definition.
--
data PayloadEvaluationCtx = PayloadEvaluationCtx
    { _payloadEvaluationCtxParentCreationTime :: BlockTime
        -- ^ Creation time of the parent block. If transactions in the block have
        -- a notion of "current" time, they should use this value.
    , _payloadEvaluationCtxParentHash :: BlockHash
        -- ^ Block hash of the parent block.
    , _payloadEvaluationCtxParentHeight :: BlockHeight
        -- ^ Block height of the parent block.
    , _payloadEvaluationCtxPayloadHash :: PayloadHash
        -- ^ Payload hash of the block that is validated. This is used as a
        -- checksum for the payload validation. This value is first computed
        -- when the respective payload is created for mining and before it is
        -- included in a block.
    , _payloadEvaluationCtxPayload :: Payload
        -- ^ The payload of the block that is going to be evaluated.
    }

-- | Synchronize the state of a payload provider to a particular fork of a
-- Chainweb chain.
--
-- This contains all data that is required for a payload provider to evaluate the
-- payloads of a consecutive sequence of blocks on top of some historic provider
-- state.
--
-- In order to create this value the client makes an educated guess about the
-- current state of the payload provider. Under normal circumstances the client
-- is able to make an accurate guess. It can fail on the first initialization of
-- a provider or on re-initialization after an ungracefull shutdown of the
-- payload provider or the client. If that happens and no suitable historic state
-- is known to the payload provider it rejects the `ForkInfo` and returns the
-- current `PayloadProviderState` to the caller. This enables the caller to
-- prepare a new `ForkInfo` value.
--
-- Binary format: the length of `_forkInfoTrace` as unsigned 32 bit integer number
-- in little endian byte order followed by the binary serialization of the
-- entries of `_forkInfoTrace`, followed by the binary serialization of
-- `forkInfoTraceHash`.
--
data ForkInfo = ForkInfo
    { _forkInfoTrace :: [PayloadEvaluationCtx]
        -- ^ The payload evaluation contexts for a consecutive sequence of blocks.
        -- The first entry determines the fork point which must be known to the
        -- payload provider (although it is not necessary that the payload provider
        -- is able to reconstruct the respective state. The provider is not
        -- obligated to replay all blocks as long as the final state is valid.
        --
        -- If evluation of the full list of paylaods fails, the payload provider
        -- may choose to remain in an intermediate state, as long as that state is
        -- consistent with the evaluation of some prefix of this field.
        --
        -- However, the operation for the respective evaluated prefix must satisfy
        -- the ACID criteria.
        --
    , _forkInfoTargetHash :: BlockHash
        -- ^ The hash of the the target block. This allows the payload provider
        -- to update its `PayloadProviderState`. Intermediate block hashes are
        -- available in form of `BlockParentHash`s from the `PayloadCtx` entries.
    }

-- | This identifies the block that corresponds to the current state of the
-- payload provider.
--
data PayloadProviderSyncState = PayloadProviderSyncState
    { _payloadProviderHeight :: BlockHeight
    , _payloadProviderBlockHash :: BlockHash
    }

-- -------------------------------------------------------------------------- --
-- API Methods

-- | Request that the payload provider updates its internal state to represent
-- the validation of the last block in the provide `ForkInfo`.
--
-- If the the first entry in `ForkInfo` is not known to the payload provider this
-- operation is a no-op and the provider returns its current sync state.
--
-- The payload provider may update the internal state only to a predecessor of the
-- requested block. This can happen if, for instance, the operation times out or
-- gets interrupted or an validation error occurs. In any case the must be valid
-- and the respective `PayloadProviderSyncState` must be returned.
--
-- Independent of the actual final state, the operation must satisify ACID
-- criteria. In particular, any intermediate state while the operation is ongoing
-- must not be obseravable and the final state must be consistent and persistent.
--
gotoBlock :: ForkInfo -> PayloadProviderApi PayloadProviderSyncState

-- | Create the payload for a new block.
--
-- This includes validation of the block *on top* of the returned current
-- `PayloadProviderSyncState` in order to compute the `PayloadHash` for the new
-- `Paylaod`.
--
-- This operation must be *read-only*. It must not change the observable state of
-- the payload provider.
--
newBlock :: MinerInfo -> PayloadProviderApi (PayloadProviderSyncState, Payload, PayloadHash)
