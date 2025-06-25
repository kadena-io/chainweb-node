{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module: Chainweb.SPV.Argument
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.SPV.Argument
( Argument(..)
, argumentClaim
, SomeArgument(..)
, runArg
, compose
, validateTrieProof
) where

import Chainweb.BlockHash
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Utils.Serialization
import Chainweb.Utils
import Chainweb.Payload
import Chainweb.PayloadProvider.EVM.Header ({- IsMerkleLogEntry instances -})
import Chainweb.PayloadProvider.EVM.Utils qualified as EVM
import Chainweb.PayloadProvider.EVM.Receipt qualified as EVM
import Chainweb.SPV.XChan qualified as X
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Data.ByteString qualified as B
import Data.Coerce
import Data.Kind
import Data.MerkleLog (MerkleRoot)
import Data.MerkleLog qualified as ML
import Data.MerkleLog.V1 qualified as V1
import Data.Text qualified as T
import Data.Type.Equality
import Data.Typeable
import Data.Word
import Ethereum.Misc qualified as E
import Ethereum.RLP (getRlpL)
import Ethereum.RLP qualified as E
import Ethereum.Trie qualified as E
import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- Native X-Chain Overview
--
-- Burn:
-- * XChan
--
-- Redeem:
-- * XChan: version, targetChain, targetAccount, xchanId
--
-- Proof API:
--  * XChan
--  * targetAccount
--  * Maybe confirmation depth
--
-- Proof content:
--  * XChanClaim (version, target chain, target account, amount, block hash)
--  * XChanId (usually derived form evidence)
--  * evidence (xchainid, signature, checksum, Merkle proof, ZK, etc.)
--
-- Redeem contract(proof, amount):
-- * deserialize proof
-- * check evidence (yields xChanId) and amount
-- * Update channel state with amount
-- * sends funds to account
--
-- Types of evidence:
--
-- * xchainId, well...
-- * Hash/Checksum: protects against accidental changes, supports testing of
--   with invalid proofs.
-- * Signature: Centralized proof of authority.
-- * Merkle, ZK, etc. Proof. Decentralized proof based on chain consensus.

-- -------------------------------------------------------------------------- --
-- Overview
--
-- Are build by combining different Merkle proofs.

-- -------------------------------------------------------------------------- --
--
-- Backward compatibility:
--
-- * Proof format:
--   * Existing proof *must* continue to work during reorg and the transition.
--   * Can we skip proof verification during replay?
--
-- Before EVM transition:
-- 1. Introduce support for validating new format
-- 2. Enable creation of new format.

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
-- Eth Account Claim
--
-- TODO move elsewhere?

type EvmAccountClaim = EVM.TrieInclusionClaim EVM.Address32 EVM.Account

deriving via (EVM.RlpMerkleLogEntry 'EthAccountTag EvmAccountClaim)
    instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag EvmAccountClaim

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag X.XChanId where
    type Tag X.XChanId = XChanIdTag
    toMerkleNode = encodeMerkleInputNode X.encodeXChanId
    fromMerkleNode = decodeMerkleInputNode X.decodeXChanId

-- -------------------------------------------------------------------------- --

type ChainwebMerkleLogEntry a =
    ( Eq a
    , Typeable a
    , Show a
    , IsMerkleLogEntry ChainwebMerkleHashAlgorithm ChainwebHashTag a
    )

tagVal' :: forall b . ChainwebMerkleLogEntry b => Word16
tagVal' = tagVal @ChainwebHashTag @(Tag b)

-- --------------------------------------------------------------------------
-- Claims

data Conjunct a b where
    Conjunct
        :: (ChainwebMerkleLogEntry a, ChainwebMerkleLogEntry b)
        => a
        -> b
        -> Conjunct a b

deriving instance (Show a, Show b) => Show (Conjunct a b)
deriving instance (Eq a, Eq b) => Eq (Conjunct a b)

-- FIXME: test the serialziation (even though, it may actually never by used)
instance
    -- (Eq a, Eq b, Typeable a, Typeable b, Show a, Show b)
    (ChainwebMerkleLogEntry a, ChainwebMerkleLogEntry b)
    => IsMerkleLogEntry ChainwebMerkleHashAlgorithm ChainwebHashTag (Conjunct a b)
  where
    type Tag (Conjunct a b) = ConjunctTag
    toMerkleNode = encodeMerkleInputNode $ \(Conjunct a b) -> do
        encodeConjunct (toMerkleNode @ChainwebMerkleHashAlgorithm a)
        encodeConjunct (toMerkleNode @ChainwebMerkleHashAlgorithm b)
      where
        encodeConjunct (ML.InputNode a) =
            putWord8 0 >> putWord16le (int $ B.length a) >> putByteString a
        encodeConjunct (ML.TreeNode b) = do
            let b' = ML.encodeMerkleRoot b
            putWord8 1 >> putWord16le (int $ B.length b') >> putByteString b'
    fromMerkleNode = decodeMerkleInputNode $ Conjunct
        <$> decodeConjunct
        <*> decodeConjunct
      where
        decodeConjunct :: forall x . ChainwebMerkleLogEntry x => Get x
        decodeConjunct = label "Composite Argument Conjunct" $ getWord8 >>= \case
            0 -> do
                l <- getWord16le
                b <- getByteString (int l)
                fromMerkleNodeM @ChainwebMerkleHashAlgorithm (ML.InputNode b)
            1 -> do
                l <- getWord16le
                b <- getByteString (int l)
                r <- ML.decodeMerkleRoot b
                fromMerkleNodeM @ChainwebMerkleHashAlgorithm (ML.TreeNode r)
            t -> fail $ "invalid Conjunct tag: " <> show t
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- class InclusionClaim a
-- instance {-# OVERLAPPABLE #-} (ChainwebMerkleLogEntry a) => InclusionClaim a
-- instance {-# OVERLAPPING #-} InclusionClaim (Conjunct a b)

-- -------------------------------------------------------------------------- --
-- Exceptions

newtype VerificationException = VerificationException T.Text
    deriving (Show, Eq)

instance Exception VerificationException

-- -------------------------------------------------------------------------- --
--
-- Consensus/BlockHeader    ML
--
-- Payload/EL-Header        Pact ML | EVM ML | Minimal ML
--
-- EL                       Pact ML | EMV Trie | Minimal ML
--
-- XChan                             MT

-- -------------------------------------------------------------------------- --
-- Merkle inclusion Arguments
--
-- A claim, in this context, is a statement about some fact being included in
-- the immutable history of the Chainweb Merkle tree. Claims can be more complex
-- than just set theoretic inclusion in the preimage of the Merkle root. In some
-- cases they also involve certain structural properties about the location of
-- the claims in the Merkle tree.
--
-- An argument provides evidence for some claim subject to some assumptions
-- being true, where the assumption are claims themself.
--
-- Reasoning is usually done backwards, by reducing the claim to other claims
-- until final claims can be dischared via some oracle.
--
-- Notation:
--
-- * `a <-| r` provides evidence for claim `a` subject to claim `r` being
--   established as true.
-- * `a ∈ r` denotes a Merkle proof that `a` is included in the preimage of the
--   Merkle root `r`. The respective claims are that `a` is a fact that is
--   recorded somewhere in the history of the block chains and `r` is a Merkle
--   root (inner node) somewhere in the history of the block chain.
-- * `a ∈ₖ r` denotes a Merkle trie proof that `a` is included in the preimage
--   of the Merkle root `r` at key `k`.
-- * `aₖ` denotes that the claim `a` occurs somewhere in the history of the
--   blockchain at key `k`. Note, that tries are used at different levels of the
--   Merkle tree and the key must be typed/tagged properly to avoid ambiguities.
--   By abuse of notion `k` can also be information in the preimagte of the key.
--
-- = Argument Types
--
-- Primitive Arguments:
--
-- * Oracle: claim -> (claim <-| ())
-- * Trivial: r -> (r <-| r)
-- * MerkleTreeInclusion: (a ∈ r) -> (a <-| r)
-- * TrieLookup: (a ∈ₖ r) -> (aₖ <-| r)
--
-- Composite Arguments:
-- * KeyInclusion: (aₖ <-| r) -> (p <-| k) -> (aₚ <-| r)
-- * Compose: (a <-| b) -> (b <-| r) -> (a <-| r)
-- * Conjunct: (a <-| r) -> (b <-| r) -> (a ∧ b) <-| r
--
-- = Primitive Arguments
--
-- There are several primitive arguments in the Chainweb Merkle tree which, in
-- the we identify in the following by their claim type:
--
-- * Local Header Oracle
--   * Local header oracles unconditionallty provide evidence that a block hash
--     exists in the current consensus state of the block chain. It is the only
--     way to discharge assumptions in arguments and obtain an unconditioned
--     claim.
--
-- * MerkleLog
--     * BlockHash: a block hash exists /somewhere/ in the Merkle tree
--     * BlockPayloadHash: block payload hash exists /somewhere/ in the Merkle tree
--     * TransactionOutput: a transaction output exists /somewhere/ in the Merkle tree
--     * Ethereum ReceiptsRoot: an Ethereum receipts root exists /somewhere/ in the Merkle tree
--     * Ethereum State Root: an Ethereum state root exists /somewhere/ in the Merkle tree
--
--     MerkleLog claims have in common that they only claim set theoretic but
--     don't make any cliam about the location of the claim in the Merkle tree.
--     Hence, no assumption must be made about the chain or block in which the
--     data is included beyond what is included in the claim itself.
--     Furthermore, one must be careful about what entity controls possible
--     provencance information in the claim. No guarantee is provided at the
--     level of arguments.
--
--     Roots of these arguments are either BlockHash or BlockPayloadHash.
--     However, in practice these arguments are always composed on primitive
--     level to arguments that have a BlockHash root.
--
-- * PactLegacyProof
--   A pact legacy proof provides evidence that a transaction outputs included
--   in the Merkle tree. This proof format is deprecated and should not be used
--   any more.
--
--   The same considerations as for MerkleLog claims apply with respect to
--   provenance and roots.
--
-- * XChanProof
--   An XChanProof provides evidence for a XChanClaim. These claims are not
--   directly included in the Chainweb Merkle tree. Instead the root of the
--   claim identfies an account in the respective payload provider and are used
--   as indexes for account balance claims.
--
-- * Ethereum Trie Proof
--   * Account proofs (account balance and storage roots for a given account)
--   * Receipt proofs (receipts for a given transaction in a block)
--   * Storage proofs (key-value pairs in the storage trie of an account)
--
--   Trie poofs provide evidence that a key-value pair is included the tree.
--   Care has to be taken because Values are not members of the Chainweb
--   universe and preimages of hashes are not tagged accordingly. Proof
--   verification must ensure that claims that include Ethereum Trie proofs are
--   tight to a value from the Chainweb Merkle universe that guards the values
--   in the claims from the trie proof.
--
--   TODO: we should consider including Ethereum types into the Chainweb Merkle
--   Universe and establish means to cryptographically enforce these via
--   appropriate means other than tagging the preimage explicitly.
--
--  = Composite Arguments
--
-- In some cases primitive arguments support composition directly, so that the
-- componsition results in a new primitive argument. In particular MerkleLog
-- arguments can be composed natively into larger primitive arguments on proof
-- level. Since all MerkleLog claims are also (tagged) members of the Chainweb
-- Universe and the proof structure does not carry any provenance information,
-- this composition is always safe.
--
-- In other cases, in particular when primitive arguments that wrap different
-- proof types are involved, the composition is done on the argument level.
--
-- * KeyInclusionArg: establish a claim about the key of a trie entry. In
--   practice, the claim about the key is na XChan Argument.
-- * ComposeArg: sequentially compose two arguments.
-- * ConjunctArg: combine two arguments that have the same root. Discharging the
--   root establishes both claims simulataneously.
--
-- = Notes for future work:
--
-- * At the moment only linear MerkleLog proofs are supported. It is possible to
--   implement multi-claim proofs, however, the format of proofs would have to
--   change in order to accomodate this.
-- * MerkeLogs use a semi-structured format, which allows to derive some
--   provenance information directly from the proof. The "trace" can be
--   interpreted as a binary key. However, this is unique only for the "header"
--   part of the leafs and not the "body" part.
--   XChan proofs use a different MerkleTree format that does not suffer from
--   this limitation.
--
data Arg (claim :: Type) (root :: Type) where

    -- Primitive Arguments
    OracleArg :: Claim claim => claim -> Arg claim ()
    TrivialArg :: Claim r => r -> Arg r r
    PactLegacyProofArg
        :: V1.MerkleProof ChainwebMerkleHashAlgorithm
        -> Arg TransactionOutput BlockHash
    MerkleLogArg
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Arg claim root
    XChanArg
        :: X.XChanProof
        -> Arg X.XChanClaim X.XChanId
    TrieLookupArg
        :: E.Proof
        -> Arg (EVM.TrieInclusionClaim k v) root

    -- Composite
    KeyInclusionArg
        :: Arg kclaim k
        -> Arg (EVM.TrieInclusionClaim k vclaim) root
        -> Arg (EVM.TrieInclusionClaim kclaim vclaim) root
    ComposeArg
        :: Arg claim a
        -> Arg a root
        -> Arg claim root
    ConjunctArg
        :: Arg claim0 r
        -> Arg claim1 r
        -> Arg (Conjunct claim0 claim1) r

type Claim a =
    ( Eq a
    , Typeable a
    , Show a
    , IsMerkleLogEntry ChainwebMerkleHashAlgorithm ChainwebHashTag a
    )

claimTag :: forall b . ChainwebMerkleLogEntry b => Word16
claimTag = tagVal @ChainwebHashTag @(Tag b)

-- argClaim :: MonadThrow m => Arg claim root -> m claim
-- argClaim (OracleArg c) = return c
-- argClaim (TrivialArg r) = return r
-- argClaim (PactLegacyProofArg p) = proofSubject p
-- argClaim (MerkleLogArg p) = proofClaim p
-- argClaim (XChanArg p) = return $ X.xChanClaim p
-- argClaim (TrieLookupArg p) = case E._proofValue p of
--     Nothing -> error "TrieLookupArg: missing value for the key"
--     Just bytes -> case E.get E.getRlp (E._proofKey p) of
--         Left e -> throwM $ VerificationException  $
--             "Invalid Ethereum Trie proof key encoding: " <> T.pack e
--         Right k -> case E.get E.getRlp bytes of
--             Left e -> throwM $ VerificationException  $
--                 "Invalid Ethereum Trie value encoding: " <> T.pack e
--             Right v -> do
--                 key <- argClaim k
--                 value <- argClaim v
--                 return $ EVM.TrieInclusionClaim
--                     { EVM._trieInclusionClaimKey = key
--                     , EVM._trieInclusionClaimValue = value
--                     }
-- argClaim (KeyInclusionArg a0 a1) = do
--     key <- argClaim a0
--     EVM.TrieInclusionClaim _ value <- argClaim a1
--     return EVM.TrieInclusionClaim
--         { EVM._trieInclusionClaimKey = key
--         , EVM._trieInclusionClaimValue = value
--         }
-- argClaim (ComposeArg a0 _) = argClaim a0
-- argClaim (ConjunctArg a0 a1) = Conjunct
--     <$> argClaim a0
--     <*> argClaim a1

-- -------------------------------------------------------------------------- --

-- TODO: replace concrete types by Tags?
--
-- The types themself are not available to all payload providers, but the tags
-- are.
--
-- TODO do we need extensible arguments, e.g. as a data family or just class?
--
-- For now we try to the set of supported Proofs and claim types small and
-- fixed.
--
-- The claim types are included in the Chainweb core library to which all
-- payload providers have access. Proof verification validates the evidence and
-- also deserializes the claim, which inlcudes verification of the tag.

-- | Arguments
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
    BlockHashOracleArgument :: BlockHash -> Argument BlockHash ()

    TrivialMerkleRootArgument
        ::
        ( Claim a
        , Coercible a (MerkleRoot ChainwebMerkleHashAlgorithm)
        )
        => a
        -> Argument a a

    -- | A header argument provides evidence that a block with a given BlockHash
    -- is included in the Chainweb Merkle tree.
    --
    BlockHashArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument BlockHash BlockHash

    -- | A block payload hash argument provides evidence that a block payload is
    -- included in a block in the Chainweb Merkle tree.
    --
    BlockPayloadHashArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument BlockPayloadHash BlockHash

    -- | A Pact output argument provides evidence that a pact transaction output
    -- is included in some MerkleRoot in the Chainweb Merkle tree.
    --
    PactOutputArgument
        ::
        ( ChainwebMerkleLogEntry root
        , Coercible root (MerkleRoot ChainwebMerkleHashAlgorithm)
        )
        => ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument TransactionOutput root

    -- | A legacy pact argument that provides evidence that a pact transaction
    -- output is included in a block in the Chainweb Merkle tree.
    --
    LegacyPactOutputArgument
        :: V1.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument TransactionOutput BlockHash

    -- | An Ethereum header argument provides evidence that an Ethereum receipt
    -- root is included in some MerkleRoot in the Chainweb Merkle tree.
    --
    EthReceiptsRootArgument
        ::
        ( ChainwebMerkleLogEntry root
        , Coercible root (MerkleRoot ChainwebMerkleHashAlgorithm)
        )
        => ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument EVM.ReceiptsRoot root

    EthStateRootArgument
        ::
        ( ChainwebMerkleLogEntry root
        , Coercible root (MerkleRoot ChainwebMerkleHashAlgorithm)
        )
        => ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument E.StateRoot root

    -- | In Ethereum receipts record transaction ouputs and are represented in
    -- the execution header through the receipts root.
    --
    EthReceiptArgument
        :: E.Proof
        -> Argument EVM.Receipt EVM.ReceiptsRoot

    -- | Ethereum Account arguments represent the balance, the nonce, the code,
    -- and the storage root of an Ethereum account.
    --
    EthAccountArgument
        :: E.Proof
        -> Argument EvmAccountClaim E.StateRoot

    -- | Ethereum Account arguments represent the balance, the nonce, the code,
    -- and the storage root of an Ethereum account.
    --
    -- EthStorageArgument
    --     :: E.Proof
    --     -> Argument EvmStorageClaim EVM.StorageRoot

    -- | Note that the claim of an XChanProof is not in the Chainweb Merkle
    -- Universe. The reason is that the XChanRoot is not a direct member of the
    -- Chainweb Merkle tree.
    --
    -- XChanRoots identify accounts in the respective payload provider and for
    -- which the balance is proven via a state proof against the state root of
    -- the respective payload provider (or alternatively an event). The state
    -- root (or event) are members of the Chainweb Merkle universe.
    --
    -- XChan as well as Payload Provider state form their own local Merkle
    -- universes.
    --
    XChanArgument
        :: X.XChanProof
        -> Argument X.XChanClaim X.XChanId

    -- | Combine a claim about the key of a trie value with a proof of the value.
    --
    TrieKeyValueArgument
        :: Argument kclaim k
        -> Argument (EVM.TrieInclusionClaim k vclaim) root
        -> Argument (EVM.TrieInclusionClaim kclaim vclaim) root

    -- Composed Arguments (used when the underlying proofs can't be composed)
    ComposeArgument
        ::
        ( ChainwebMerkleLogEntry a
        , ChainwebMerkleLogEntry claim
        , ChainwebMerkleLogEntry root
        )
        => Argument claim a
        -> Argument a root
        -> Argument claim root

    ConjunctArgument
        ::
        ( ChainwebMerkleLogEntry a0
        , ChainwebMerkleLogEntry a1
        , ChainwebMerkleLogEntry root
        , ChainwebMerkleLogEntry claim0
        , ChainwebMerkleLogEntry claim1
        )
        => Argument claim0 root
        -> Argument claim1 root
        -> Argument (Conjunct claim0 claim1) root

deriving instance (Show claim, Show root) => Show (Argument claim root)

argumentClaim :: MonadThrow m => Argument claim root -> m claim
argumentClaim (BlockHashOracleArgument c) = return c
argumentClaim (TrivialMerkleRootArgument r) = return r
argumentClaim (BlockHashArgument p) = proofClaim p
argumentClaim (BlockPayloadHashArgument p) = proofClaim p
argumentClaim (PactOutputArgument p) = proofClaim p
argumentClaim (LegacyPactOutputArgument p) = proofSubject p
argumentClaim (EthReceiptsRootArgument p) = proofClaim p
argumentClaim (EthStateRootArgument p) = proofClaim p
argumentClaim (EthReceiptArgument p) = case E._proofValue p of
    Nothing -> throwM $ VerificationException "Invalid Receipt proof: missing claim"
    Just r -> case E.get E.getRlp r of
        Left e -> throwM $ VerificationException $ "Invalid receipt: " <> T.pack e
        Right r' -> return r'

argumentClaim (EthAccountArgument p) = case E._proofValue p of
    Nothing -> throwM $
        VerificationException "Invalid Account proof: missing value for the key"
    Just bytes -> case E.get E.getRlp (E._proofKey p) of
        Left e -> throwM $ VerificationException $ "Invalid Ethereum Account proof key encoding: " <> T.pack e
        Right k -> case E.get E.getRlp bytes of
            Left e -> throwM $ VerificationException $ "Invalid Ethereum Account value encoding " <> T.pack e
            Right (v :: EVM.Account) -> return $ EVM.TrieInclusionClaim
                { EVM._trieInclusionClaimKey = k
                , EVM._trieInclusionClaimValue = v
                }

argumentClaim (XChanArgument p) = return $ X.XChanClaim
    { X._xChanClaimVersion = X._xChanProofVersion p
    , X._xChanClaimTrgChain = X._xChanProofTrgChain p
    , X._xChanClaimPolicy = X._xChanProofPolicy p
    }

argumentClaim (TrieKeyValueArgument a0 a1) = do
    key <- argumentClaim a0
    EVM.TrieInclusionClaim _ value <- argumentClaim a1
    return EVM.TrieInclusionClaim
        { EVM._trieInclusionClaimKey = key
        , EVM._trieInclusionClaimValue = value
        }

argumentClaim (ComposeArgument a0 _) = argumentClaim a0
argumentClaim (ConjunctArgument a0 a1 _) = Conjunct
    <$> argumentClaim a0
    <*> argumentClaim a1

-- -------------------------------------------------------------------------- --
-- Serialization

-- | SomeArgument is a wrapper for an Argument.
--
-- We require that all claims and root are in the Chainweb Merkle universe, even
-- if in some cases it is not strictly required when the types of Merkle tree
-- leafs are guarded by structural invariants on the way how arguments are
-- constructed. We still use the tags from the universe to enforce the correct
-- structure during deserialization of the argument tree itself.
--
data SomeArgument where
    SomeArgument
        :: forall (claim :: Type) (root :: Type)
        . (ChainwebMerkleLogEntry claim, ChainwebMerkleLogEntry root)
        => Argument claim root
        -> SomeArgument

instance MerkleHashAlgorithm a => E.RLP (MerkleRoot a) where
    putRlp r = E.putRlp $ ML.encodeMerkleRoot r
    getRlp = E.label "MerkleRoot " $ do
        bs <- E.getRlp @B.ByteString
        case ML.decodeMerkleRoot bs of
            Left e -> fail $ "Invalid MerkleRoot: " <> show e
            Right r -> return r
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance MerkleHashAlgorithm a => E.RLP (ML.MerkleNodeType a) where
    putRlp (ML.TreeNode h) = E.putRlpL
        [ E.putRlp (0 :: Word8)
        , E.putRlp h
        ]
    putRlp (ML.InputNode b) = E.putRlpL
        [ E.putRlp (1 :: Word8)
        , E.putRlp b
        ]
    getRlp = E.label "MerkleNodeType" $ do
        E.getRlp @Word8 >>= \case
            0 -> E.label "TreeNode" (ML.TreeNode <$> E.getRlp)
            1 -> E.label "InputNode" (ML.InputNode <$> E.getRlp)
            _ -> fail "invalid MerkleNodeType"
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance MerkleHashAlgorithm a => E.RLP (V1.MerkleProof a) where
    putRlp p = E.putRlpL
        [ E.putRlp $ V1._getMerkleProofSubject (V1._merkleProofSubject p)
        , E.putRlp $ V1.encodeMerkleProofObject (V1._merkleProofObject p)
        ]
    getRlp = E.label "V1.MerkleProof" $ V1.MerkleProof
        <$> E.label "MerkleProofSubject" (V1.MerkleProofSubject <$> E.getRlp)
        <*> E.label "MerkleProofObject" do
            bs <- E.getRlp @B.ByteString
            case V1.decodeMerkleProofObject bs of
                Left e -> fail $ "Invalid MerkleProofObject: " <> show e
                Right r -> return r
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance E.RLP (ML.MerkleProof ChainwebMerkleHashAlgorithm) where
    putRlp p = E.putRlpL
        [ E.putRlp (ML._merkleProofClaim p)
        , E.putRlp (fromIntegral @_ @Natural $ ML._merkleProofTrace p)
        , E.putRlp (ML._merkleProofEvidence p)
        ]
    getRlp = E.label "MerkleProof" $
        ML.MerkleProof
            <$> E.label "claim" E.getRlp
            <*> E.label "trace" (fromIntegral @Natural <$> E.getRlp)
            <*> E.label "evidence" E.getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance MerkleHashAlgorithm a => E.RLP (BlockPayloadHash_ a) where
    putRlp = E.putRlp . coerce @_ @(MerkleRoot a)
    getRlp = E.label "BlockPayloadHash" $ coerce <$> E.getRlp @(MerkleRoot a)
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance MerkleHashAlgorithm a => E.RLP (BlockHash_ a) where
    putRlp = E.putRlp . coerce @_ @(MerkleRoot a)
    getRlp = E.label "BlockHash" $ coerce <$> E.getRlp @(MerkleRoot a)
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance E.RLP X.XChanVersion where
    putRlp X.XChainVersion0 = E.putRlp (0 :: Word8)
    getRlp = E.label "XChainVersion" $ do
        tag <- E.getRlp @Word8
        case tag of
            1 -> return X.XChainVersion0
            e -> fail $ "invalid XChainVersion tag: " <> show e
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance E.RLP X.XChanPolicy where
    putRlp (X.TrgAccount a) = E.putRlpL
        [ E.putRlp (0 :: Word8) -- constructor tag
        , E.putRlp a
        ]
    getRlp = E.label "XChanPolicy" $ do
        tag <- E.getRlp @Word8
        case tag of
            0 -> X.TrgAccount <$> E.getRlp
            e -> fail $ "invalid XChanPolicy tag: " <> show e
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance E.RLP X.MerkleRoot where
    putRlp (X.MerkleRoot r) = E.putRlp r
    getRlp = E.label "XChanMerkleRoot" $ X.MerkleRoot <$> E.getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance E.RLP ChainId where
    putRlp (ChainId c) = E.putRlp c
    getRlp = E.label "ChanId" $ unsafeChainId <$> E.getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance E.RLP X.XChanProof where
    putRlp p = E.putRlpL
        [ E.putRlp (X._xChanProofVersion p)
        , E.putRlp (X._xChanProofTrgChain p)
        , E.putRlp (X._xChanProofPolicyIndex p)
        , E.putRlp (X._xChanProofPolicy p)
        , E.putRlp (X._xChanProofRoots p)
        ]
    getRlp = E.label "XChanProof" $ X.XChanProof
        <$> E.getRlp @X.XChanVersion
        <*> E.getRlp @ChainId
        <*> E.getRlp @Natural
        <*> E.getRlp @X.XChanPolicy
        <*> E.getRlp @[X.MerkleRoot]
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance E.RLP SomeArgument where
    putRlp (SomeArgument @claim @root arg) = case arg of
        -- Primitive Arguments
        (Trivial p) -> go 0 (coerce @_ @(MerkleRoot ChainwebMerkleHashAlgorithm) p)
        (PactLegacyProof p) -> go 1 p
        (HeaderArgument p) -> go 2 p
        (ParentHeaderArgument p) -> go 3 p
        (BlockPayloadHashArgument p) -> go 4 p
        (PactOutputArgument p) -> go 5 p
        (EthHeaderArgument p) -> go 6 p
        (EthReceiptArgument p) -> go 7 p
        (EthAccountArgument p) -> go 8 p
        (XChanArgument p) -> go 9 p

        -- Composed Arguments
        (ComposeArgument a0 a1) -> E.putRlpL
            [ E.putRlp (128 :: Word8)
            , E.putRlp (SomeArgument a0)
            , E.putRlp (SomeArgument a1)
            ]
        (ComposeArgument2 a0 a1 a3) -> E.putRlpL
            [ E.putRlp (129 :: Word8)
            , E.putRlp (SomeArgument a0)
            , E.putRlp (SomeArgument a1)
            , E.putRlp (SomeArgument a3)
            ]
      where
        go :: E.RLP p => Word8 -> p -> E.Put
        go tag p = E.putRlpL
            [ E.putRlp (tag :: Word8)
            , E.putRlp (tagVal' @claim)
            , E.putRlp (tagVal' @root)
            , E.putRlp p
            ]

    getRlp = E.label "Argument" $ getRlpL $ do
        tag <- E.label "Argument Tag" (E.getRlp @Word8)
        claim <- E.label "Claim Tag" $ E.getRlp @Word16
        root <- E.label "Root Tag" $ E.getRlp @Word16
        case tag of
            -- Primitive Arguments
            0 -> E.label "Trivial" $ if
                | root == tagVal' @BlockPayloadHash -> SomeArgument <$>
                    enforceType claim root (Trivial @BlockPayloadHash <$> E.getRlp)
                | root == tagVal' @BlockHash -> SomeArgument <$>
                    enforceType claim root (Trivial @BlockHash <$> E.getRlp)
                | otherwise ->
                    fail "invalid root type"
            1 -> E.label "PactLegacyProof" $ SomeArgument <$>
                getTagged (PactLegacyProof <$> E.getRlp)
            2 -> E.label "HeaderArgument" $ SomeArgument <$>
                getTagged (HeaderArgument <$> E.getRlp)
            3 -> E.label "ParentHeaderArgument" $ SomeArgument <$>
                getTagged (ParentHeaderArgument <$> E.getRlp)
            4 -> E.label "BlockPayloadHashArgument" $ SomeArgument <$>
                getTagged (BlockPayloadHashArgument <$> E.getRlp)
            5 -> E.label "PactOutputArgument" $ if
                | root == tagVal' @BlockPayloadHash -> SomeArgument <$>
                    enforceType claim root (PactOutputArgument @BlockPayloadHash <$> E.getRlp)
                | root == tagVal' @BlockHash -> SomeArgument <$>
                    enforceType claim root (PactOutputArgument @BlockHash <$> E.getRlp)
                | otherwise ->
                    fail "invalid root type"
            6 -> E.label "EthHeaderArgument" $ if
                | root == tagVal' @BlockPayloadHash -> SomeArgument <$>
                    enforceType claim root (EthHeaderArgument @BlockPayloadHash <$> E.getRlp)
                | root == tagVal' @BlockHash -> SomeArgument <$>
                    enforceType claim root (EthHeaderArgument @BlockHash <$> E.getRlp)
                | otherwise ->
                    fail "invalid root type"
            7 -> E.label "EthReceiptArgument" $ SomeArgument <$>
                getTagged (EthReceiptArgument <$> E.getRlp)
            8 -> E.label "EthAccountArgument" $ SomeArgument <$>
                getTagged (EthAccountArgument <$> E.getRlp)
            9 -> E.label "XChanArgument" $ SomeArgument <$>
                getTagged (XChanArgument <$> E.getRlp)

            -- Composed Arguments
            128 -> E.label "ComposeArgument" $ do
                SomeArgument @c @a a0 <- E.getRlp
                SomeArgument @a' @r a1 <- E.getRlp
                arg <- getTagged @c @r $ case (eqT @a @a') of
                    Just Refl -> return $ ComposeArgument a0 a1
                    _ -> fail "invalid compose argument"
                return $ SomeArgument arg
            129 -> E.label "ComposeArgument2" $ do
                SomeArgument @c0 @a0 a0 <- E.getRlp
                SomeArgument @c1 @a1 a1 <- E.getRlp
                SomeArgument @a' @r a3 <- E.getRlp
                arg <- getTagged @(Conjunct c0 c1) @r $
                    case eqT @(Conjunct a0 a1) @a' of
                        Just Refl -> return $ ComposeArgument2 a0 a1 a3
                        _ -> fail "invalid compose2 argument"
                return $ SomeArgument arg
            _ -> fail "invalid argument type"
      where
        getTagged
            :: forall claim root
            . ChainwebMerkleLogEntry claim
            => ChainwebMerkleLogEntry root
            => E.Get (Argument claim root)
            -> E.Get (Argument claim root)
        getTagged p = do
            claim <- E.label "Claim Tag" $ E.getRlp @Word16
            root <- E.label "Root Tag" $ E.getRlp @Word16
            enforceType claim root p

        enforceType
            :: forall claim root
            . ChainwebMerkleLogEntry claim
            => ChainwebMerkleLogEntry root
            => Word16
            -> Word16
            -> E.Get (Argument claim root)
            -> E.Get (Argument claim root)
        enforceType claim root p = do
            unless (claim == tagVal' @claim) (fail "invalid claim type")
            unless (root == tagVal' @root) (fail "invalid root type")
            p

-- -------------------------------------------------------------------------- --
-- | Argument composition.
--
-- The implementation balances the composition tree to be right associative.
-- This maximizes the composition on the lower level proofs.
--
-- Nested calls to this can be quadratic in the worst case when associating from
-- the left. So one should avoid that. Instead arguments should be composed from
-- the right.
--
compose
    :: MonadThrow m
    => ChainwebMerkleLogEntry a
    => ChainwebMerkleLogEntry claim
    => ChainwebMerkleLogEntry root
    => Argument claim a
    -> Argument a root
    -> m (Argument claim root)
compose (Trivial _) a = return a -- FXME: check that claims match?
compose a (Trivial _) = return a -- FIXME: check that roots match?
compose (HeaderArgument m0) (HeaderArgument m1)
    = HeaderArgument <$> ML.composeProofs m0 m1
compose (ParentHeaderArgument m0) (HeaderArgument m1)
    = ParentHeaderArgument <$> ML.composeProofs m0 m1
compose (BlockPayloadHashArgument m0) (HeaderArgument m1)
    = BlockPayloadHashArgument <$> ML.composeProofs m0 m1
compose (PactOutputArgument m0) (BlockPayloadHashArgument m1)
    = PactOutputArgument <$> ML.composeProofs m0 m1
compose (PactOutputArgument m0) (ParentHeaderArgument m1)
    = PactOutputArgument <$> ML.composeProofs m0 m1
compose (EthHeaderArgument m0) (BlockPayloadHashArgument m1)
    = EthHeaderArgument <$> ML.composeProofs m0 m1
compose (ComposeArgument a0 a1) b = ComposeArgument a0 <$> compose a1 b
compose a (ComposeArgument b0 b1) = do
    -- First compose a and b0. This may result in a ComposeArgument in which
    -- case we and up with the previous case.
    a' <- compose a b0
    compose a' b1
compose a b = return (ComposeArgument a b)


-- -------------------------------------------------------------------------- --
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
runArg
    :: MonadThrow m
    => Argument claim root
    -> m root
runArg (Trivial r) = return r
runArg (PactLegacyProof evidence) = do
    r <- V1.runMerkleProof evidence
    return (coerce r)
runArg (PactOutputArgument evidence) = do
    return $ coerce $ ML.runProof evidence
runArg (ParentHeaderArgument evidence) = do
    return $ coerce $ ML.runProof evidence
runArg (HeaderArgument evidence) = do
    return $ coerce $ ML.runProof evidence
runArg (BlockPayloadHashArgument evidence) = do
    return $ coerce $ ML.runProof evidence
runArg (EthReceiptArgument evidence) = do
    -- We do not need to check the proof key because all values under this
    -- root are receipts. However, we must make sure that the root has the
    -- correct type, which is done either when checked against an oracle or when
    -- it is used as claim in another proof.
    r <- EVM.ReceiptsRoot <$> validateTrieProof evidence
    return r
runArg (EthHeaderArgument evidence) = do
    return $ coerce $ ML.runProof evidence

-- FIXME: we need to check the key
runArg (EthAccountArgument evidence) = do
    -- It must be checked that the root has the correct type, which is done
    -- either when checked against an oracle or when it is used as claim in
    -- another proof.
    r <- E.StateRoot <$> validateTrieProof evidence
    return r
runArg (XChanArgument p) = do
    -- The XChanProof is not a member of the Chainweb Merkle universe. It must
    -- be proven separately, that the root of the XChan represents an account
    -- with the respective properties in the respective payload provider.
    r <- X.runXChanProof p
    return r

runArg (ComposeArgument a0 a1) = do
    r0 <- runArg a0
    c1 <- argumentClaim a1
    when (r0 /= c1) $ throwM $ VerificationException "Composition failed"
    runArg a1
runArg (ComposeArgument2 a0 a1 a3) = do
    r0 <- runArg a0
    r1 <- runArg a1
    Conjunct c0 c1 <- argumentClaim a3
    when (r0 /= c0 || r1 /= c1) $ throwM $ VerificationException "Composition2 failed"
    runArg a3

-- runProof (Trivial r) = do
--     return r
-- runProof (ComposeArgument2 a0 a1 a3) (Conjunct c0 c1) = do
--     r0 <- runProof a0 c0
--     r1 <- runProof a1 c1
--     runProof a3 (Conjunct r0 r1)

-- -------------------------------------------------------------------------- --
-- | Ethereum Trie Proofs
--

validateTrieProof
    :: MonadThrow m
    => E.Proof
    -> m E.Keccak256Hash
validateTrieProof p = case E.validateProof p of
    Left _ ->
        throwM (VerificationException "Malformed Proof")
    Right False ->
        throwM (VerificationException "Invalid Proof")
    Right True ->
        return $ E._proofRoot p

-- -------------------------------------------------------------------------- --
-- ATTIC

-- -------------------------------------------------------------------------- --
-- | Pact Output Proof

-- instance E.RLP (ML.MerkleProof a) where
--     putRlp p = E.putRlp p
--     getRlp = E.label "MerkleProof" $ MerkleProof <$> E.getRlp
--     {-# INLINE putRlp #-}
--     {-# INLINE getRlp #-}

-- createMerkleProof
--     :: [(B.ByteString, B.ByteString)]
--         -- ^ Key-value pairs that are stroed in the Trie
--     -> B.ByteString
--         -- ^ the key of the proofs
--     -> ML.MerkleProof a
-- createMerkleProof ps key = TrieProof
--     { _trieProofKey = E._proofKey p
--     , _trieProofNodes = E._proofNodes p
--     , _trieProofRoot = E._proofRoot p
--     }
--   where
--     p = E.createProof ps key

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
