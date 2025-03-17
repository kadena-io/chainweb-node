{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeAbstractions #-}

-- |
-- Module: Chainweb.PayloadProvider.SPV
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.SPV
( Argument(..)
, runProof
) where

import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.MinerReward
import Chainweb.Payload
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.EVM.EthRpcAPI qualified as RPC
import Chainweb.PayloadProvider.EVM.JsonRPC
import Chainweb.PayloadProvider.EVM.JsonRPC qualified as RPC
import Chainweb.SPV
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Control.Exception
import Control.Monad
import Control.Monad.Catch qualified as C
import Data.ByteString qualified as B
import Data.ByteString.Short qualified as BS
import Data.Kind
import Data.MerkleLog qualified as ML
import Data.Text qualified as T
import Data.Word
import Ethereum.Misc
import Ethereum.Misc qualified as E
import Ethereum.RLP qualified as E
import Ethereum.Receipt qualified as E
import Ethereum.Receipt.ReceiptProof qualified as E
import Ethereum.Trie qualified as E
import GHC.Generics
import GHC.TypeNats
import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- Alternative Approach

-- Protocol flow:
--
-- 1. Event emission [on-chain]: the user creates the event. The event may be the
--    native event format of the payload provider, a custom language extension,
--    or an agreement to write a particular value to the contract store.
--
-- 2. Proof creation [off-chain]: the user creates a proof (usually through
--    calling an API) that contains the relevant event data along with evidence
--    for its inclusion on the source chain and its provenance.
--
-- 3. Send redeem transaction [off-chain]: the user sends a transaction that
--    includes the proof to the target chain.
--
-- 4. Proof verification [on-chain]: the execution environment on the receiving
--    chain verifies the evidence in the proof and extract the relevant message
--    and provenance data. It performance additional checks on that data and, if
--    successful, notifies the receiver contract.
--
-- 4. Redeem [on-chain]: the receiving contract obtains the notification data.
--    It performs some checks on the data. If the checks succeed it performs the
--    redeem operations.
--
--
-- TOPIC FORMAT:
--
-- 0: event type signature hash (keccak256(<signature>))
-- 1: targetChain [uint32, bigendian]
-- 2: target address [address, last 20 bytes]
-- 3: ignored (optional for indexing)
--
-- LogData:
-- * receiver address B.ByteString (eth address 20 bytes)
-- * amount [uint256]

-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- TODO:
--
-- The types in the Merkle Universe must be available to all payload providers
-- (the alternative would be to define a unique event format across all payload
-- providers and require that all payload providers support direct proofs of
-- those events in their respective payload Merkle tree.
--
-- For instance the EVM provider could collect events from the EVM and transform
-- those to the common format and add those to a new event tree.
--
-- We should create a package that collects and reexports all types from the
-- Chainweb Merkle universe.

-- -------------------------------------------------------------------------- --
-- TagUsage

data TagType
    = LeafTag
    | InnerTag

type family GetTagType (t :: ChainwebHashTag) :: TagType where

    -- GetTagType VoidTag = 'LeafTag
    -- GetTagType MerkleRootTag = 'InnerTag

    -- BlockHeader
    GetTagType ChainIdTag = 'LeafTag
    GetTagType BlockHeightTag = 'LeafTag
    GetTagType BlockWeightTag = 'LeafTag
    GetTagType BlockPayloadHashTag = 'LeafTag
    GetTagType BlockNonceTag = 'LeafTag
    GetTagType BlockCreationTimeTag = 'LeafTag
    GetTagType ChainwebVersionTag = 'LeafTag
    GetTagType PowHashTag = 'LeafTag
    GetTagType BlockHashTag = 'InnerTag
    GetTagType HashTargetTag = 'LeafTag
    GetTagType EpochStartTimeTag = 'LeafTag
    GetTagType FeatureFlagsTag = 'LeafTag

    -- Pact Payloads
    GetTagType TransactionTag = 'LeafTag
    GetTagType TransactionOutputTag = 'LeafTag
    GetTagType BlockTransactionsHashTag = 'InnerTag
    GetTagType BlockOutputsHashTag = 'InnerTag
    GetTagType MinerDataTag = 'LeafTag
    GetTagType CoinbaseOutputTag = 'LeafTag

    -- Pact Event (not Currently Chainweb Merkle Tree)
    GetTagType OutputEventsTag = 'InnerTag
    GetTagType BlockEventsHashTag = 'InnerTag
    GetTagType RequestKeyTag = 'LeafTag
    GetTagType PactEventTag = 'LeafTag

    -- Minimal Payload Provider
    GetTagType MinimalPayloadTag = 'LeafTag

    -- EVM Payload Provider
    GetTagType EthParentHashTag = 'LeafTag
    GetTagType EthOmmersHashTag = 'LeafTag
    GetTagType EthBeneficiaryTag = 'LeafTag
    GetTagType EthStateRootTag = 'LeafTag
    GetTagType EthTransactionsRootTag = 'LeafTag
    GetTagType EthReceiptsRootTag = 'LeafTag
    GetTagType EthBloomTag = 'LeafTag
    GetTagType EthDifficultyTag = 'LeafTag
    GetTagType EthBlockNumberTag = 'LeafTag
    GetTagType EthGasLimitTag = 'LeafTag
    GetTagType EthGasUsedTag = 'LeafTag
    GetTagType EthTimestampTag = 'LeafTag
    GetTagType EthExtraDataTag = 'LeafTag
    GetTagType EthRandaoTag = 'LeafTag
    GetTagType EthNonceTag = 'LeafTag
    GetTagType EthBaseFeePerGasTag = 'LeafTag
    GetTagType EthWithdrawalsRootTag = 'LeafTag
    GetTagType EthBlobGasUsedTag = 'LeafTag
    GetTagType EthExcessBlobGasTag = 'LeafTag
    GetTagType EthParentBeaconBlockRootTag = 'LeafTag

-- --------------------------------------------------------------------------
-- Claims

data Conjunct a b where
    Conjunct :: (InclusionClaim a, InclusionClaim b) => a -> b -> Conjunct a b

class InclusionClaim a
instance {-# OVERLAPPABLE #-}
    (IsMerkleLogEntry ChainwebMerkleHashAlgorithm ChainwebHashTag a)
    => InclusionClaim a
instance {-# OVERLAPPING #-} InclusionClaim (Conjunct a b)

-- class ClaimType a
-- instance {-# OVERLAPPABLE #-} (ChainwebMerkleEntry a, GetTagType (Tag a) ~ 'LeafTag) => ClaimType a
-- instance {-# OVERLAPPING #-} (ClaimType a, ClaimType b) => ClaimType (a, b)
--
-- class RootType a
-- instance {-# OVERLAPPABLE #-} (ChainwebMerkleEntry a, GetTagType (Tag a) ~ 'InnerTag) => RootType a
-- instance {-# OVERLAPPING #-} (RootType a, RootType b) => RootType (a, b)

-- -------------------------------------------------------------------------- --
-- Exceptions

newtype VerificationException = VerificationException T.Text
    deriving (Show, Eq)

instance Exception VerificationException

-- -------------------------------------------------------------------------- --
-- NEW SPV

-- proofProperties
--     :: forall e kv
--     . KeyValue e kv
--     => ChainId
--     -> MerkleProof Sha2_512_256
--     -> [kv]
-- proofProperties cid p =
--     [ "chain" .= cid
--     , "event" .= JsonProofSubject (_getMerkleProofSubject $ _merkleProofSubject p)
--     , "algorithm" .= ("Sha2_512_256" :: T.Text)
--     ]
--   where
--     obj = encodeB64UrlNoPaddingText . encodeMerkleProofObject
--
-- data XChainMessage = XChainMessage
--     { _xChainMsgTargetChain :: !ChainId
--     , _xChainMsgReceiverAccount :: !B.ByteString
--     , _xChainMsgAmount :: !Stu
--     }
--
-- data XChainEvent = XChainEvent
--     { _xChainSourceChain :: !ChainId
-- 	    -- ^ not in the ETH log
--     , _xChainSourceContract :: !B.ByteString
--       -- ^ Added by the system
--     , _xChainSourceBlockHeight :: !Word64
-- 	    -- Not in EHT Log
--     , _xChainSourceTxIdx :: !Word64
-- 	    -- ^ this is not in the event it is provided by the user when they select
-- 	    -- the event
--     , _xChainSourceEventIdx :: !Word64
-- 	    -- ^ this is not authenticated by the proof. It is provided by the user and
-- 	    -- instructs the precompile to limit the proof to just this one event.
--     , _xChainMessage :: !XChainMessage
-- 	    -- ^ The actual user payload. Cf. below for an example for simple ERC-20
-- 	    -- cross chain transfers.
--     }


-- data PayloadProof a = PayloadProof
--     { _payloadProofRootType :: !MerkleRootType
--         -- ^ The type of the Merle root.
--     , _payloadProofBlob :: !(MerkleProof a)
--         -- ^ The Merkle proof blob which coaintains both, the proof object and
--         -- the proof subject.
--     } deriving (Show, Eq, Generic, NFData)
--
-- payloadProofProperties
--     :: forall a e kv
--     . MerkleHashAlgorithmName a
--     => KeyValue e kv
--     => PayloadProof a
--     -> [kv]
-- payloadProofProperties p =
--     [ "rootType" .= _payloadProofRootType p
--     , "object" .= (obj . _merkleProofObject) blob
--     , "subject" .= JsonProofSubject (_getMerkleProofSubject $ _merkleProofSubject blob)
--     , "algorithm" .= merkleHashAlgorithmName @a
--     ]
--   where
--     blob = _payloadProofBlob p
--     obj = encodeB64UrlNoPaddingText . encodeMerkleProofObject
-- {-# INLINE payloadProofProperties #-}
-- -------------------------------------------------------------------------- --

-- TODO: replace concrete types by Tags.
--
-- The types themself are not available to all payload providers, but the tags
-- are.

-- TODO do we need extensible arguments, e.g. as a data family or just class?

-- Data type of Argument
--
-- An argument provides evidence for the statement:
--
-- \(\text{root} \in \text{Chainweb} \rightarrow \text{claim} \in \text{Chainweb}\)
--
-- where `claim` is a value that is stored on chain and has a type that is a
-- member of the Chainweb Merkle universe.
--
-- This statement is established by applying the argument to the claim, which
-- may succeed or fail.
--
data Argument (claim :: Type) (root :: Type) where
    Trivial :: Argument a a
    BlockPayloadHashArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument BlockPayloadHash BlockHash
    ParentHeaderArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument ParentHash BlockHash
    PactOutputArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument TransactionOutput BlockPayloadHash
    EthReceiptArgument
        :: TrieProof
        -> Argument E.Receipt ReceiptsRoot
    EthHeaderArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument ReceiptsRoot BlockPayloadHash

    -- Composed Arguments
    ComposeArgument
        :: InclusionClaim a
        => Argument claim a
        -> Argument a root
        -> Argument claim root
    ComposeArgument2
        :: (InclusionClaim a0, InclusionClaim a1)
        => Argument claim0 a0
        -> Argument claim1 a1
        -> Argument (Conjunct a0 a1) root
        -> Argument (Conjunct claim0 claim1) root

-- What would be the benefit of generic Arguments? Probably not much. When
-- serialized, neither the root nor the claim is included. Therefore the
-- overhead would be just the respective constructor tag.

--     -- Generic Arguments
--     MerkleArgument
--         :: (InclusionClaim claim)
--         => MerkleProof Sha2_512_256
--         -> Argument claim root
--     TrieArgument
--         :: (InclusionClaim claim)
--         => TrieProof
--         -> Argument claim root

compose
    :: InclusionClaim claim
    => InclusionClaim a
    => Argument claim a
    -> Argument a root
    -> Maybe (Argument claim root)
compose Trivial a = Just a
compose a Trivial = Just a
compose (BlockPayloadHashArgument m0) (ParentHeaderArgument m1)
    = MerkleArgument (ML.composeProof m0 m1)


-- | Compute the proof root and provide evidence that the proof claim is
-- included in the root of the respective Chainweb Merkle tree.
--
-- The claim is either a leave and a member of the Chainweb Merkle universe or
-- it is an inner node of the Chainweb Merkle tree. In case of the latter no
-- futher evidence about its type is included.
--
-- IMPORTANT NOTE:
--
-- If the result is an inner tree node (e.g. a 'BlockHash' or a
-- 'BlockPayloadHash') type is not checked and purely informational. There is no
-- evidence for it being correct. (we could add that evidence in the future, but
-- right now it is not provided.)
--
-- It is an important security invariant of Chainweb that the preimage of each
-- root is completely guarded by types values in the Chainweb Merkle universe.
--
runProof
    :: InclusionClaim claim
    => InclusionClaim root
    => Argument claim root
    -> claim
    -> Either VerificationException root
runProof (TransactionOutputProof evidence) claim =
    runTransactionOutputProof evidence claim
runProof (EthReceiptArgument evidence) claim =
    ReceiptsRoot <$> validateTrieProof (Just $ E.putRlpByteString claim) evidence
runProof (ComposeArgument a0 a1) claim = runProof a0 claim >>= runProof a1
runProof (ComposeArgument2 a0 a1 a3) (Conjunct c0 c1) = do
    r0 <- runProof a0 c0
    r1 <- runProof a1 c1
    runProof a3 (Conjunct r0 r1)


-- -- | Compute the proof root and provide evidence that is included as a leave in
-- -- the root of the respective Chainweb Merkle tree. The evidence includes that
-- -- the claim is the correct type in the Chainweb Merkle universe.
-- --
-- -- When running proofs we restrict claims to members of the Merkle universe. It
-- -- is an important security invariant of Chainweb that the preimage of each root
-- -- is completely guarded by types values in the Chainweb Merkle universe.
-- --
-- runLeafProof
--     :: ClaimType claim
--     => RootType root
--     => Argument claim root
--     -> claim
--     -> Either VerificationException root
-- runLeafProof = runProof

-- -------------------------------------------------------------------------- --
-- | Pact Output Proof

instance E.RLP (ML.MerkleProof a) where
    putRlp p = E.putRlp p
    getRlp = E.label "MerkleProof" $ MerkleProof <$> E.getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

createMerkleProof
    :: [(B.ByteString, B.ByteString)]
        -- ^ Key-value pairs that are stroed in the Trie
    -> B.ByteString
        -- ^ the key of the proofs
    -> ML.MerkleProof a
createMerkleProof ps key = TrieProof
    { _trieProofKey = E._proofKey p
    , _trieProofNodes = E._proofNodes p
    , _trieProofRoot = E._proofRoot p
    }
  where
    p = E.createProof ps key

-- -------------------------------------------------------------------------- --
-- | Ethereum Trie Proofs
--
-- Note that trie proofs are "forward": given the root, the key, and the
-- evidence, they produce the claim.
--
-- Whereas Merkle proofs are "backward": given the claim, and some evidence they
-- produce the claim (and possibly the key, i.e. the position in the tree).
--
-- Our implementation of verification turns the argument for trie proofs around
-- to support backward reasoning.
--
data TrieProof = TrieProof
    { _trieProofKey :: !B.ByteString
    , _trieProofNodes :: ![B.ByteString]
    , _trieProofRoot :: !Keccak256Hash
    }
    deriving (Show, Eq)

instance E.RLP TrieProof where
    putRlp p = E.putRlp
        ( _trieProofKey p
        , _trieProofNodes p
        , _trieProofRoot p
        )
    getRlp = E.label "TrieProof" $ E.getRlpL $ TrieProof
        <$> E.label "proofKey" E.getRlp
        <*> E.label "proofNodes" E.getRlp
        <*> E.label "proofRoot" E.getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- | Crate a proof for the value is stored in the trie for the given key.
--
-- If no value is stored for the given key the returned proof includes a
-- '_proofValue' of 'Nothing' and witnesses that the key doesn't exist in the
-- trie.
--
createTrieProof
    :: [(B.ByteString, B.ByteString)]
        -- ^ Key-value pairs that are stroed in the Trie
    -> B.ByteString
        -- ^ the key of the proofs
    -> TrieProof
createTrieProof ps key = TrieProof
    { _trieProofKey = E._proofKey p
    , _trieProofNodes = E._proofNodes p
    , _trieProofRoot = E._proofRoot p
    }
  where
    p = E.createProof ps key

validateTrieProof
    :: Maybe B.ByteString
    -> TrieProof
    -> Either VerificationException Keccak256Hash
validateTrieProof claim p = case E.validateProof q of
    Left _ ->
        Left (VerificationException "Malformed Proof")
    Right False ->
        Left (VerificationException "Invalid Proof")
    Right True ->
        Right $ _trieProofRoot p
  where
    q = E.Proof
        { E._proofKey = _trieProofKey p
        , E._proofNodes = _trieProofNodes p
        , E._proofRoot = _trieProofRoot p
        , E._proofValue = claim
        }

