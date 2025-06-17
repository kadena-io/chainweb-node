{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Chainweb.PayloadProvider.SPV
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.SPV
( Argument(..)
, SomeArgument(..)
, runArg
, compose
) where

import Chainweb.BlockHash
import Chainweb.BlockPayloadHash
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.PayloadProvider.EVM.Header ({- IsMerkleLogEntry instances -})
import Chainweb.PayloadProvider.EVM.Receipt qualified as EVM
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
--
--

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

class InclusionClaim a
instance {-# OVERLAPPABLE #-} (ChainwebMerkleLogEntry a) => InclusionClaim a
instance {-# OVERLAPPING #-} InclusionClaim (Conjunct a b)

-- -------------------------------------------------------------------------- --
-- Exceptions

newtype VerificationException = VerificationException T.Text
    deriving (Show, Eq)

instance Exception VerificationException

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
    Trivial
        ::
        ( ChainwebMerkleLogEntry a
        , Coercible a (MerkleRoot ChainwebMerkleHashAlgorithm)
        )
        => a
        -> Argument a a
    PactLegacyProof
        :: V1.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument TransactionOutput BlockHash
    HeaderArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument BlockHash BlockHash
    ParentHeaderArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument BlockHash BlockHash
    BlockPayloadHashArgument
        :: ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument BlockPayloadHash BlockHash
    PactOutputArgument
        ::
        ( ChainwebMerkleLogEntry root
        , Coercible root (MerkleRoot ChainwebMerkleHashAlgorithm)
        )
        => ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument TransactionOutput root
    EthHeaderArgument
        ::
        ( ChainwebMerkleLogEntry root
        , Coercible root (MerkleRoot ChainwebMerkleHashAlgorithm)
        )
        => ML.MerkleProof ChainwebMerkleHashAlgorithm
        -> Argument E.ReceiptsRoot root
    EthReceiptArgument
        :: E.Proof
        -> Argument EVM.Receipt E.ReceiptsRoot

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

    -- Future work:
    --
    -- Conjunect a0 a1 is not a MerkleLogEntry. Hence we need create an
    -- abstraction that that covers both MerkleLogEntries and Conjunctions of
    -- MerkleLogEntries.
    --
    -- ComposeArgument2
    --     ::
    --     ( ChainwebMerkleLogEntry a0
    --     , ChainwebMerkleLogEntry a1
    --     , ChainwebMerkleLogEntry root
    --     , ChainwebMerkleLogEntry claim0
    --     , ChainwebMerkleLogEntry claim1
    --     )
    --     => Argument claim0 a0
    --     -> Argument claim1 a1
    --     -> Argument (Conjunct a0 a1) root
    --     -> Argument (Conjunct claim0 claim1) root

deriving instance (Show claim, Show root) => Show (Argument claim root)

argumentClaim :: MonadThrow m => Argument claim root -> m claim
argumentClaim (Trivial r) = return r
argumentClaim (PactLegacyProof p) = fromMerkleNodeM
    $ V1._getMerkleProofSubject
    $ V1._merkleProofSubject p
argumentClaim (HeaderArgument p) = fromMerkleNodeM $ ML._merkleProofClaim p
argumentClaim (ParentHeaderArgument p) = fromMerkleNodeM $ ML._merkleProofClaim p
argumentClaim (BlockPayloadHashArgument p) = fromMerkleNodeM $ ML._merkleProofClaim p
argumentClaim (PactOutputArgument p) = fromMerkleNodeM $ ML._merkleProofClaim p
argumentClaim (EthHeaderArgument p) = fromMerkleNodeM $ ML._merkleProofClaim p
argumentClaim (EthReceiptArgument p) = case E._proofValue p of
    Nothing -> throwM $ VerificationException "Invalid Receipt proof: missing claim"
    Just r -> case E.get E.getRlp r of
        Left e -> throwM $ VerificationException $ "Invalid receipt: " <> T.pack e
        Right r' -> return r'
argumentClaim (ComposeArgument a0 _) = argumentClaim a0
-- argumentClaim (ComposeArgument2 a0 a1 _) = Conjunct
--     <$> argumentClaim a0
--     <*> argumentClaim a1

-- -------------------------------------------------------------------------- --
-- Serialization

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

instance E.RLP SomeArgument where
    putRlp (SomeArgument @claim @root arg) = case arg of
        (Trivial p) -> go 0 (coerce @_ @(MerkleRoot ChainwebMerkleHashAlgorithm) p)
        (PactLegacyProof p) -> go 1 p
        (HeaderArgument p) -> go 2 p
        (ParentHeaderArgument p) -> go 3 p
        (BlockPayloadHashArgument p) -> go 4 p
        (PactOutputArgument p) -> go 5 p
        (EthHeaderArgument p) -> go 6 p
        (EthReceiptArgument p) -> go 7 p
        (ComposeArgument a0 a1) -> E.putRlpL
            [ E.putRlp (8 :: Word8)
            , E.putRlp (SomeArgument a0)
            , E.putRlp (SomeArgument a1)
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
            8 -> E.label "ComposeArgument" $ do
                SomeArgument @c @a a0 <- E.getRlp
                SomeArgument @a' @r a1 <- E.getRlp
                arg <- getTagged @c @r $ case (eqT @a @a') of
                    Just Refl -> return $ ComposeArgument a0 a1
                    _ -> fail "invalid compose argument"
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
    r <- E.ReceiptsRoot <$> validateTrieProof evidence
    return r
runArg (EthHeaderArgument evidence) = do
    return $ coerce $ ML.runProof evidence
runArg (ComposeArgument a0 a1) = do
    r0 <- runArg a0
    c1 <- argumentClaim a1
    when (r0 /= c1) $ throwM $ VerificationException "Composition failed"
    runArg a1

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

-- tests

-- Pact Output Proof script:
--
-- import Chainweb.PayloadProvider.Pact.Genesis
-- import Chainweb.Version.Mainnet01
-- import Chainweb.Utils
-- import Chainweb.Crypto.MerkleLog
-- import Chainweb.MerkleUniverse
-- import Chainweb.Payload
-- import Chainweb.BlockHeader
-- import Data.MerkleLog qualified as ML
-- import Control.Lens
-- script :: IO ()
-- script = do
--     let plds = Chainweb.PayloadProvider.Pact.Genesis.genesisPayload Mainnet01
--     pld0 <- preview (ixg $ unsafeChainId 0) plds
--     let (_, outs) = payloadWithOutputsToBlockObjects pld0
--     p0 <- bodyProofV2 @ChainwebMerkleHashAlgorithm  outs 0
--     let bpld0 = payloadDataToBlockPayload (payloadWithOutputsToPayloadData pld0)
--     x <- headerProofV2 @BlockOutputsHash @ChainwebMerkleHashAlgorithm bpld0
--     p <- ML.composeProofs p0 x
--     ML.runProof p
--     let poarg = PactOutputArgument @BlockPayloadHash p
--     runArg poarg
--
--     let gh = genesisBlockHeader Mainnet01 (unsafeChainId 0)
--     hp <- headerProofV2 @BlockPayloadHash @ChainwebMerkleHashAlgorithm gh
--     ML.runProof hp
--     let harg = BlockPayloadHashArgument hp
--     runArg harg
--
--     arg <- compose poarg harg
--     runArg arg

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
