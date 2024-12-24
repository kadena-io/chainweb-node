{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.PayloadProvider
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider
(
-- * Hints
  Hints(..)

-- * SyncState
, SyncState(..)
, syncStateOfBlockHeader
, syncStateRankedBlockPayloadHash

-- * ConsensusState
, ConsensusState(..)
, genesisSyncState
, genesisConsensusState
, latestRankedBlockPayloadHash
, safeRankedBlockPayloadHash
, finalRankedBlockPayloadHash
, genesisState

-- * NewBlock Context
, NewBlockCtx(..)

-- * Evaluation Context
, EvaluationCtx(..)
, _evaluationCtxRankedPayloadHash

-- * Fork Info
, ForkInfo(..)
, PayloadProvider(..)
, EncodedPayloadData(..)
, EncodedPayloadOutputs(..)
, assertForkInfoInvariants
, _forkInfoBaseHeight
, _forkInfoBaseBlockHash
, _forkInfoBaseRankedPayloadHash

-- * New Payload
, NewPayload(..)
, _newPayloadRankedParentHash
-- , SyncError(..)
-- , EvmPayloadCtx
-- , PactPayloadCtx
, blockHeaderToEvaluationCtx
, nextPayload
, nextPayloadStm
, payloadStream

-- * Some PayloadProvider
, SomePayloadProvider(..)

-- * Payload Providers for all Chains
, PayloadProviders(..)
, payloadProviders
, withPayloadProvider

-- * Utils

-- ** Consensus State Accessors
, _latestBlockHash
, _latestPayloadHash
, _latestHeight
, _safeBlockHash
, _safePayloadHash
, _safeHeight
, _finalBlockHash
, _finalPayloadHash
, _finalHeight
) where

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.MinerReward
import Chainweb.Utils
import Chainweb.Version
import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString qualified as B
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Numeric.Natural
import P2P.Peer
import Streaming.Prelude qualified as S

-- -------------------------------------------------------------------------- --
-- Exceptions

data PayloadProviderException
    = InvalidForkInfo T.Text
    deriving (Show, Eq, Generic)

instance Exception PayloadProviderException

-- -------------------------------------------------------------------------- --
-- Consensus State

-- | This identifies the block that corresponds to the current state of the
-- payload provider.
--
-- TODO:
-- To accomodate existing providers (Pact and EVM), we include both, block hash
-- and the block payload hash. When future versions of Pact are able to resolve
-- payloads through the payload block hash alone, we may drop the block hash
-- from this structure.
--
-- TODO:
-- Safe and Finalized Block should be identified via the block payload hashes
-- (and possibly number) instead of the height.
--
data SyncState = SyncState
    { _syncStateHeight :: !BlockHeight
        -- ^ The BlockHeight of the lastest Block
    , _syncStateBlockHash :: !BlockHash
        -- ^ The BlockHash of the latest Block
    , _syncStateBlockPayloadHash :: !BlockPayloadHash
        -- ^ The BlockPayloadHash of the latest Block
    }
    deriving (Show, Eq, Ord)

syncStateOfBlockHeader :: BlockHeader -> SyncState
syncStateOfBlockHeader hdr = SyncState
    { _syncStateHeight = view blockHeight hdr
    , _syncStateBlockHash = view blockHash hdr
    , _syncStateBlockPayloadHash = view blockPayloadHash hdr
    }

syncStateRankedBlockPayloadHash :: SyncState -> RankedBlockPayloadHash
syncStateRankedBlockPayloadHash s = RankedBlockPayloadHash
    (_syncStateHeight s) (_syncStateBlockPayloadHash s)

data ConsensusState = ConsensusState
    { _consensusStateLatest :: !SyncState
    , _consensusStateSafe :: !SyncState
        -- ^ The latest block that is generally considered safe. In a Chainweb a
        -- block is considered safe, when it has at least Graph-diameter many
        -- full layers on top of it. However, depending on on current conditions
        -- of the network the required depth can be larger. For testing networks
        -- with low-diameter chain graphs the minimal required depth is at least
        -- 3.
    , _consensusStateFinal :: !SyncState
        -- ^ The latest block that is considered final. Note that this is a
        -- heuristics. There is no definite finality in any PoW/PoH blockchain
        -- system. (And the same should probably be true for PoS blockchains as
        -- well.)
        --
        -- Chainweb uses an very conservative measure of finality. A block that
        -- is considered final can not be reorged automatically. Instead such a
        -- reorg would require manual intervention of node maintainers.
    }
    deriving (Show, Eq, Ord)

genesisSyncState
    :: HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> SyncState
genesisSyncState v c =
    syncStateOfBlockHeader (genesisBlockHeader (_chainwebVersion v) c)

genesisConsensusState
    :: HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> ConsensusState
genesisConsensusState v c = ConsensusState
    { _consensusStateLatest = genesisSyncState v c
    , _consensusStateSafe = genesisSyncState v c
    , _consensusStateFinal = genesisSyncState v c
    }

latestRankedBlockPayloadHash :: ConsensusState -> RankedBlockPayloadHash
latestRankedBlockPayloadHash =
    syncStateRankedBlockPayloadHash . _consensusStateLatest

safeRankedBlockPayloadHash :: ConsensusState -> RankedBlockPayloadHash
safeRankedBlockPayloadHash =
    syncStateRankedBlockPayloadHash . _consensusStateSafe

finalRankedBlockPayloadHash :: ConsensusState -> RankedBlockPayloadHash
finalRankedBlockPayloadHash =
    syncStateRankedBlockPayloadHash . _consensusStateFinal

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
data EvaluationCtx = EvaluationCtx
    { _evaluationCtxParentCreationTime :: !BlockCreationTime
        -- ^ Creation time of the parent block. If transactions in the block
        -- have a notion of "current" time, they should use this value.
    , _evaluationCtxParentHash :: !BlockHash
        -- ^ Block hash of the parent block.
    , _evaluationCtxParentHeight :: !BlockHeight
        -- ^ Block height of the parent block.
    , _evaluationCtxMinerReward :: !MinerReward
        -- ^ The miner reward that is assigned to the miner of the block. Miner
        -- rewards are not constant and determined by the consensus protocol. It
        -- depends on the block height and the respective chain graph.
        --
        -- The payload provider must validate for each block that the reward is
        -- correct.
        --
        -- On payload provider API level the amount is provided in Stu and
        -- encoded as unsigned 64 bit integer value in little endian encoding.
        --
        -- Internally, encoding and unit is provider specific. For Pact it is a
        -- Kda value for the EVM provider it is in Stu. Also the recipient and
        -- the mechanism how it is credited is provider specific.
    , _evaluationCtxPayloadHash :: !BlockPayloadHash
        -- ^ Payload hash of the block that is validated. This is used as a
        -- checksum for the payload validation. For the last block of a ForkInfo
        -- structure this value must match the respective value in the target
        -- sync state.
        --
        -- The BlockPayloadHash is first computed when the respective payload is
        -- created for mining and before it is included in a block.
    , _evaluationCtxPayloadData :: !(Maybe EncodedPayloadData)
        -- ^ Optional external payload data. This may be
        -- the complete, self contained block payload or it may just contain
        -- complementary data that aids with the validation.
        --
        -- The main purpose of this field is to allow consensus to gossip around
        -- payload data along with new cuts in the P2P network, which allows for
        -- more efficient synchronization and a reduction of block propagation
        -- latencies.
    }
    deriving (Show, Eq, Ord)

_evaluationCtxRankedPayloadHash
    :: EvaluationCtx
    -> RankedBlockPayloadHash
_evaluationCtxRankedPayloadHash ctx = RankedBlockPayloadHash
    (_evaluationCtxParentHeight ctx + 1)
    (_evaluationCtxPayloadHash ctx)

-- | Context for creating a new block on top of the latest Block.
--
-- The miner reward depends on global consensus properties like block height and
-- chain graph.
--
-- In the future this structure may be extended with additional fields.
--
-- Note, that the mechanism how the reward is credited as well as the recipient
-- is defined in the scope of the specific payload provider. It is *not* part of
-- the global chainweb consensus protocol. Similarly, gas fees and the recipient
-- of those fees are provider specific. This implies a trust relationship
-- between payload provider and consensus (the miner, which is part of
-- consensus, must trust its payload provider).
--
data NewBlockCtx = NewBlockCtx
    { _newBlockCtxMinerReward :: !MinerReward
        -- ^ the miner reward for the new block.
    , _newBlockCtxParentCreationTime :: !BlockCreationTime
        -- ^ the creation time of the block on which the new is created.
    }
    deriving (Show, Eq, Ord)

-- | Get the evaluation context for given parent header and block payload hash
--
blockHeaderToEvaluationCtx
    :: ParentHeader
    -> BlockPayloadHash
    -> Maybe EncodedPayloadData
    -> EvaluationCtx
blockHeaderToEvaluationCtx (ParentHeader ph) pld pldData = EvaluationCtx
    { _evaluationCtxParentCreationTime = view blockCreationTime ph
    , _evaluationCtxParentHash = view blockHash ph
    , _evaluationCtxParentHeight = parentHeight
    , _evaluationCtxMinerReward = blockMinerReward v height
    , _evaluationCtxPayloadHash = pld
    , _evaluationCtxPayloadData = pldData
    }
  where
    parentHeight = view blockHeight ph
    height = parentHeight + 1
    v = _chainwebVersion ph

-- -------------------------------------------------------------------------- --
-- ForkInfo

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
-- NOTE:
--
-- A special case are stateless payload providers for which the evaluation of a
-- payload depends only on the local evaluation context and not on the history
-- of the chain. Those payload providers do not need to resolve reorgs. However,
-- they cannot offer access to a complete and consistent payload history via the
-- service API. The minimal payload provider is an example for a stateless
-- payload provider.
--
data ForkInfo = ForkInfo
    { _forkInfoTrace :: ![EvaluationCtx]
        -- ^ The payload evaluation contexts for a consecutive sequence of
        -- blocks.  The first entry determines the fork point which must be
        -- known to the payload provider (although it is not necessary that the
        -- payload provider is able to reconstruct the respective state. The
        -- provider is not obligated to replay all blocks as long as the final
        -- state is valid.
        --
        -- If evaluation of the full list of payloads fails, the payload provider
        -- may choose to remain in an intermediate state, as long as that state
        -- is consistent with the evaluation of some prefix of this field.
        --
        -- The payload provider is also obligated to validate the correctness of
        -- the evaluation context with respect the payload provider state for
        -- all provided contexts in this field up to the lastest validated
        -- block.  This allows the client to request the evaluation and
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
        -- FIXME:
        -- It may be more intuitive and convenient to store the trace in reverse
        -- order.
        --
    , _forkInfoBasePayloadHash :: !BlockPayloadHash
        -- ^ The payload hash of the parent block of the first entry in the
        -- fork info trace. If the fork info trace is empty, this is the payload
        -- hash of the latest block in the consensus state.
        --
        -- The payload hash of the parent block is not part of the evaluation
        -- context, because it does need to be validated. However, some payload
        -- providers may require it to identify the base state onto which the
        -- fork info trace is applied.
    , _forkInfoTargetState :: !ConsensusState
        -- ^ The target sync state. This allows the payload provider
        -- to update its `SyncState`. Intermediate block hashes are
        -- available in form of `BlockParentHash`s from the `PayloadCtx`
        -- entries.
    , _forkInfoNewBlockCtx :: !(Maybe NewBlockCtx)
        -- ^ If mining is enabled in the payload provider and this field is not
        -- `Nothing`, the payload provider creates a new block payload with the
        -- given miner info and miner reward.
    }
    deriving (Show, Eq, Ord)

_forkInfoBaseHeight :: ForkInfo -> BlockHeight
_forkInfoBaseHeight fi = case _forkInfoTrace fi of
    [] -> _latestHeight (_forkInfoTargetState fi)
    (h:_) -> _evaluationCtxParentHeight h

_forkInfoBaseBlockHash :: ForkInfo -> BlockHash
_forkInfoBaseBlockHash fi = case _forkInfoTrace fi of
    [] -> _latestBlockHash (_forkInfoTargetState fi)
    (h:_) -> _evaluationCtxParentHash h

_forkInfoBaseRankedPayloadHash :: ForkInfo -> RankedBlockPayloadHash
_forkInfoBaseRankedPayloadHash fi = RankedBlockPayloadHash
    (_forkInfoBaseHeight fi)
    (_forkInfoBasePayloadHash fi)

assertForkInfoInvariants :: MonadThrow m => ForkInfo -> m ()
assertForkInfoInvariants forkInfo = do
    when (null (_forkInfoTrace forkInfo)) $
        unless (trgPayloadHash forkInfo == _forkInfoBasePayloadHash forkInfo) $
            throwM $ InvalidForkInfo
                "The base payload hash must match the target payload hash, when the fork info trace is empty"

  where
    trgPayloadHash = _latestPayloadHash . _forkInfoTargetState

-- -------------------------------------------------------------------------- --
-- Hints for querying Payloads

data Hints = Hints
    { _hintsOrigin :: !PeerInfo
    }

-- -------------------------------------------------------------------------- --
-- Enocoded Payloads

newtype EncodedPayloadData = EncodedPayloadData
    { _encodedPayloadData :: B.ByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance ToJSON EncodedPayloadData where
    toJSON = toJSON . encodeB64UrlNoPaddingText . _encodedPayloadData
    toEncoding = b64UrlNoPaddingTextEncoding . _encodedPayloadData
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON EncodedPayloadData where
    parseJSON = withText "EncodedPayloadData" $ \t ->
        case decodeB64UrlNoPaddingText t of
            Left e -> fail (show e)
            Right x -> return $ EncodedPayloadData x
    {-# INLINE parseJSON #-}

newtype EncodedPayloadOutputs = EncodedPayloadOutputs
    { _encodedPayloadOutputs :: B.ByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance ToJSON EncodedPayloadOutputs where
    toJSON = toJSON . encodeB64UrlNoPaddingText . _encodedPayloadOutputs
    toEncoding = b64UrlNoPaddingTextEncoding . _encodedPayloadOutputs
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON EncodedPayloadOutputs where
    parseJSON = withText "EncodedPayloadOutputs" $ \t ->
        case decodeB64UrlNoPaddingText t of
            Left e -> fail (show e)
            Right x -> return $ EncodedPayloadOutputs x
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- New Payload

-- |
--
-- Some metrics included that maybe used by consensus for performance
-- optimizations and optimal resource allocations and reporting of aggregated
-- metrics.
--
-- TODO: describe encoding
--
data NewPayload = NewPayload
    { _newPayloadChainwebVersion :: !ChainwebVersion
    , _newPayloadChainId :: !ChainId
    , _newPayloadParentHeight :: !BlockHeight
    , _newPayloadParentHash :: !BlockHash
    , _newPayloadBlockPayloadHash :: !BlockPayloadHash
    , _newPayloadEncodedPayloadData :: !(Maybe EncodedPayloadData)
    , _newPayloadEncodedPayloadOutputs :: !(Maybe EncodedPayloadOutputs)
    , _newPayloadNumber :: !Int
        -- ^ a locally monotonically increasing identifier. Its purpose is to
        -- induce an order on payloads that are created for the same parent
        -- header. The miner will give precedence to larger id values.

    -- Informative:
    , _newPayloadTxCount :: !Natural
        -- ^ The number of user transactions in the block. The exact way how
        -- transactions are counted is provider specific.
    , _newPayloadSize :: !Natural
        -- ^ On-chain storage size in bytes. This is not generally the same as
        -- the size of the optional PayloadData. The exact meaning is provider
        -- specific.
    , _newPayloadOutputSize :: !Natural
        -- ^ Size of evaluation outputs bytes. This is not generally the same as
        -- the size of the optional PayloadOuputs. The exact meaning is provider
        -- specific.
    , _newPayloadFees :: !Stu
        -- ^ The total amount of all fees payed for transaction processing in
        -- Stu. The exact meaning is provider specific.
    }
    deriving (Show, Eq, Ord, Generic)

_newPayloadRankedParentHash :: NewPayload -> RankedBlockHash
_newPayloadRankedParentHash np = RankedBlockHash
    (_newPayloadParentHeight np)
    (_newPayloadParentHash np)

instance HasChainwebVersion NewPayload where
    _chainwebVersion = _newPayloadChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainId NewPayload where
    _chainId = _newPayloadChainId
    {-# INLINE _chainId #-}

-- -------------------------------------------------------------------------- --
-- Payload Provider

-- | Payload Provider API.
--
-- Typically, a payload this is a list of transactions, including some system
-- specific transactions for rewarding miners and paying gas fees. It may also
-- include additional metadata that affect the evaluation.
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
class (HasChainwebVersion p, HasChainId p) => PayloadProvider p where

    -- | Returns the current sync state of the payload provider.
    -- Note that this may be ahead of that returned by `prefetchBlock`.
    --
    -- syncState :: p -> IO SyncState

    -- | Tell the PayloadProvider to fetch the block, and do whatever work is
    -- necessary for us to synchronize with a block later that has this payload
    -- hash. This is probably not necessary when compacted headers are added to
    -- catchup.
    --
    -- TODO: is this allowed to fail? Does it return or is it fire and forget?
    --
    prefetchPayloads
        :: p
        -> Maybe Hints
        -> ForkInfo
            -- ^ TODO: do we really want to pass the full ForkInfo here? What is
            -- the purpose of passing the full Consensus State? Maybe resolving
            -- forks? Maybe a list of RankedBlockPayloadHashes (or
            -- EvaluationCtx) would be sufficient here?
        -> IO ()

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
    -- the must be valid and the respective `SyncState` must be returned.
    --
    -- The payload provider may update the internal state to a successor
    -- of the requested block. This can happen if the provider is unable to
    -- rewind blocks on a fork, for example the EVM. In that case 'syncToBlock'
    -- will regardless return a SyncState for the requested block, not the successor.
    --
    -- Independent of the actual final state, the operation must satisify ACID
    -- criteria. In particular, any intermediate state while the operation is
    -- ongoing must not be observable and the final state must be consistent
    -- and persistent.
    --
    syncToBlock
        :: p
            -- ^ Payload provider handle
        -> Maybe Hints
            -- ^ hints for fetching missing payloads
        -> ForkInfo
        -> IO ConsensusState

    -- | Asynchronoulsly yield new block payloads on top of the latest block.
    --
    -- The new payload is identified by the block payload hash.
    --
    -- New payloads only depend on there respective evaluation context and new
    -- block contenxt and are only valid within this context. The context is
    -- fully determined by the parent header, which is always the block of the
    -- latest block of the current consensus state. It is represented by its
    -- respective height and block hash.
    --
    -- The result may include the actual payload and some representation of the
    -- evaluation outputs. This allows for better error reporting and
    -- optimizations in payload synchronizations (e.g. by piggybacking the new
    -- payloads onto the respective new cuts in the P2P gossip network). Beside
    -- of being a sequence of bytes the content of the those payloads are
    -- completely parametetric outside the context of the payload provider.
    --
    -- Payload provider should cache new payloads internally as long they have
    -- not either been integrated into the longest chain or definitely
    -- abandoned. Payload providers may also cache the validation result.
    --
    latestPayloadSTM :: p -> STM NewPayload

    -- If backed by an TVar, this can usually be implemented more efficiently
    -- using 'readTVarIO'
    --
    latestPayloadIO :: p -> IO NewPayload
    latestPayloadIO = atomically . latestPayloadSTM

nextPayloadStm :: PayloadProvider p => p -> NewPayload -> STM NewPayload
nextPayloadStm p cur = do
    new <- latestPayloadSTM p
    when (new == cur) retry
    return new

nextPayload :: PayloadProvider p => p -> NewPayload -> IO NewPayload
nextPayload p = atomically . nextPayloadStm p

payloadStream :: PayloadProvider p => p -> S.Stream (S.Of NewPayload) IO ()
payloadStream p = do
    cur <- liftIO $ latestPayloadIO p
    S.yield cur
    go cur
  where
    go c = do
        n <- liftIO $ nextPayload p c
        S.yield n
        go n

-- -------------------------------------------------------------------------- --
-- Some Payload Provider

data SomePayloadProvider where
    SomePayloadProvider :: PayloadProvider p => p -> SomePayloadProvider

instance HasChainwebVersion SomePayloadProvider where
    _chainwebVersion (SomePayloadProvider p) = _chainwebVersion p

instance HasChainId SomePayloadProvider where
    _chainId (SomePayloadProvider p) = _chainId p

-- -------------------------------------------------------------------------- --
-- Payload Providers

newtype PayloadProviders = PayloadProviders
    { _payloadProviders :: HM.HashMap ChainId SomePayloadProvider
    }

payloadProviders :: Getter PayloadProviders (HM.HashMap ChainId SomePayloadProvider)
payloadProviders = to _payloadProviders

type instance Index PayloadProviders = ChainId
type instance IxValue PayloadProviders = SomePayloadProvider

instance IxedGet PayloadProviders where
    ixg i = to _payloadProviders . ix i

withPayloadProvider
    :: HasCallStack
    => HasChainId c
    => PayloadProviders
    -> c
    -> (forall p . PayloadProvider p => p -> a)
    -> a
withPayloadProvider (PayloadProviders ps) c f = case HM.lookup cid ps of
    Just (SomePayloadProvider p) -> f p
    Nothing -> error $
        "PayloadProviders: unknown ChainId " <> sshow cid <> ". This is a bug"
  where
    cid = _chainId c

-- -------------------------------------------------------------------------- --
-- Utils

_latestBlockHash :: ConsensusState -> BlockHash
_latestBlockHash = _syncStateBlockHash . _consensusStateLatest

_latestPayloadHash :: ConsensusState -> BlockPayloadHash
_latestPayloadHash = _syncStateBlockPayloadHash . _consensusStateLatest

_latestHeight :: ConsensusState -> BlockHeight
_latestHeight = _syncStateHeight . _consensusStateLatest

_safeBlockHash :: ConsensusState -> BlockHash
_safeBlockHash = _syncStateBlockHash . _consensusStateSafe

_safePayloadHash :: ConsensusState -> BlockPayloadHash
_safePayloadHash = _syncStateBlockPayloadHash . _consensusStateSafe

_safeHeight :: ConsensusState -> BlockHeight
_safeHeight = _syncStateHeight . _consensusStateSafe

_finalBlockHash :: ConsensusState -> BlockHash
_finalBlockHash = _syncStateBlockHash . _consensusStateFinal

_finalPayloadHash :: ConsensusState -> BlockPayloadHash
_finalPayloadHash = _syncStateBlockPayloadHash . _consensusStateFinal

_finalHeight :: ConsensusState -> BlockHeight
_finalHeight = _syncStateHeight . _consensusStateFinal

genesisState
    :: HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> ConsensusState
genesisState v c = ConsensusState
    { _consensusStateLatest = s
    , _consensusStateSafe = s
    , _consensusStateFinal = s
    }
  where
    s = SyncState
        { _syncStateHeight = 0
        , _syncStateBlockHash = view blockHash hdr
        , _syncStateBlockPayloadHash = view blockPayloadHash hdr
        }
    hdr = genesisBlockHeader (_chainwebVersion v) c

