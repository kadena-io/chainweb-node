{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module: Chainweb.Crypto.MerkleLog
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module provides a framework for hashing structured data and creating
-- inclusion proofs for hashed data.
--
-- An example of how to use this module can be found in the file
-- ./docs/merklelog-example.md. The impatient reader is encouraged to skip right
-- to the example.
--
-- == Background: The Problem of Hashing Structured Data
--
-- When authenticating structured data it is common practice to create hashes
-- by using some binary encoding of the whole structure or of the individual
-- components. In the latter case, the serialized components are usually
-- concatenated and hashed. This ad-hoc approach to hashing binary data is error
-- prone and makes API specifications more complicated.
--
-- Moreover, the resulting hashes can't be used for compact self-contained
-- inclusion proofs. Usually a lot of unrelated additional data has to be
-- included in the proof to make it self-contained. Another way is to define a
-- separate structure as input for a Merkle tree which requires to maintain an
-- additional index for the roots of the trees and possibly also for querying
-- the relevant input in the right order.
--
-- Finally, authenticated values in block chains and values for content-
-- addressed key value stores in general, often include the key in the data
-- structure, which creates a cyclic dependency of a value on itself. When
-- creating such a structure this is usually dealt with by lazily tying a knot,
-- instantiating the key with a default value (e.g. 'Nothing') and replacing it
-- later with the actual key (e.g. 'Just key'), or by defining two functions for
-- creating the hash and another for creating the structure that take the same
-- data as input plus the hash. Often those functions take a large number of
-- parameters and it can be hard to keep both in sync when maintaining the code.
-- It is easy to add a parameter to the structure, but forget to update the hash
-- computation to include the new value, which is difficult to detect in tests and can lead
-- to flawed security.
--
-- == What this Module Offers
--
-- This module defines types and idioms for a unified way to compute hashes on
-- structured data. This makes it easier to maintain the consistency of the hash
-- computations and generates hashes in a way that supports compact Merkle
-- proofs. The module introduces a syntax for defining the structure of hashes
-- that makes it obvious how a hash is created and helps to to make sure that
-- the code complies with the specification.
--
-- The tools in the module also automate some of the tedious and error prone
-- work of creating and verifying inclusion proofs. The precise specification of
-- the construction of these proofs is key for a block chain application, and
-- this module should help to ensure that the specification is uniform across
-- all proofs.
--
-- == Merkle Log
--
-- A merkle log represents a view of a data structure that
--
-- 1. defines how a hash is computed and
-- 2. supports efficient creation of compact, self-contained inclusion proofs.
--
-- Entries in a merkle log must be
--
-- 1. serializable to a binary representation and
-- 2. arranged in a arbitrary but fixed order.
--
-- The types of entries that can be included as leaves of the same MerkleTree must
-- live in a closed universe. Each universe allows fixes the hash algorithm that
-- is used for all Merkle trees over the universe. Using a closed universe
-- prevents attacks that are based on using hashes of data from different
-- domains with equal representation.
--
-- In our implementation we assume that each value for which a Merkle Log is
-- created can be represented as a short, finite length header and a body that
-- is a sequence of values of the same type. The 'HasMerkleLog' class provides
-- the representation of a type as a Merkle log and the functions to
-- transform a value to and from that representation.
--
module Chainweb.Crypto.MerkleLog
(
-- $inputs
--
-- * Merkle Log Universe
  MerkleUniverse(..)
, tagVal
, MerkleHashAlgorithm
, MerkleHashAlgorithmName(..)

-- * Merkle Log Entries
, IsMerkleLogEntry(..)
, MerkleLogEntries(..)
, emptyBody
, mapLogEntries
, entriesHeaderSize
, entriesBody
, fromMerkleNodeM

-- * Merkle Log
, MerkleLog(..)
, _merkleLogTree
, HasMerkleLog(..)
, MkLogType
, merkleLog
, newMerkleLog
, HasHeader
, HasHeader_(..)
, body
, headerSize
, bodySize
, computeMerkleLogRoot

-- * Merkle Log Proofs
, headerProofV1
, headerProofV2
, headerTree
, headerTree_
, bodyProofV1
, bodyProofV2
, bodyTree
, bodyTree_
, proofSubject
, proofClaim

-- * Utils for Defining Instances
, decodeMerkleInputNode
, encodeMerkleInputNode

, decodeMerkleTreeNode
, encodeMerkleTreeNode

-- ** IsMerkleLogEntry instance for use with @deriving via@
, MerkleRootLogEntry(..)

-- * Exceptions
, MerkleLogException(..)
, expectedInputNodeException
, expectedTreeNodeException
) where

import Control.Monad.Catch

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.Coerce
import Data.Foldable
import Data.Kind
import Data.MerkleLog hiding (Expected, Actual)
import Data.MerkleLog.V1 qualified as V1
import Data.Proxy
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word
import Data.Hash.SHA2
import Data.Hash.Keccak

import Foreign.Storable

import GHC.TypeNats

-- internal modules

import Data.Singletons
import Chainweb.Utils
import Chainweb.Utils.Serialization

-- -------------------------------------------------------------------------- --
-- Exceptions

data MerkleLogException
    = MerkleLogWrongNodeTypeException (Expected T.Text) (Actual T.Text)
    | MerkleLogWrongTagException (Expected T.Text) (Actual T.Text)
    | MerkleLogDecodeException T.Text
    deriving (Show)

instance Exception MerkleLogException

expectedInputNodeException :: MerkleLogException
expectedInputNodeException = MerkleLogWrongNodeTypeException
    (Expected "InputNode")
    (Actual "TreeNode")

expectedTreeNodeException :: MerkleLogException
expectedTreeNodeException = MerkleLogWrongNodeTypeException
    (Expected "TreeNode")
    (Actual "InputNode")

-- -------------------------------------------------------------------------- --
-- Internal Utils

toWordBE
    :: forall w m
    . MonadThrow m
    => ByteSwap w
    => Storable w
    => B.ByteString
    -> m w
toWordBE bs = case peekByteString bs of
    Left e -> throwM $ MerkleLogDecodeException e
    Right x -> return $ be x

-- -------------------------------------------------------------------------- --
-- $inputs
-- = Merkle Tree Inputs
--
-- A framework for computing hashes from structures in a well-defined way.
--
-- A structure that can be hashed is called a log. The elements of the structure
-- that are inputs to the hash are called log entries. In order to use a type as
-- a log one has to define two things:
--
-- 1. For each log entry a serialization and deserialization method.
-- 2. A decomposition of a log type into an ordered sequence of log entries.
--
-- A Merkle Universe is created as follows:
--
-- 1. Define a data kind with a nullary type constructor for each type in the
--    Merkle universe.
-- 2. Define an instance for MerkleUniverse for the new kind and assign each
--    type constructor in the universe a typelevel 'Nat' value that represents a
--    'Word16' value.
--
-- A log entry is defined as follows:
--
-- 1. Consider creating a specific newtype wrapper for the entry type, in
--    particular, if the entry is of a generic type, like, for instance, 'Int'
--    or 'B.ByteString'.
-- 2. Define an 'IsMerkleLogEntry' instance or derive it using the 'deriving via'
--    extension if available. For the 'Tag' associated type family pick the
--    value from the Merkle universe type that corresponds to the entry type.
--
-- A log type is defines as follows:
--
-- 1. Define all constructor fields as log entries.
-- 2. Define a 'HasMerkleLogEntry' instance for the type.

-- -------------------------------------------------------------------------- --
-- Merkle Universe

-- | A Kind that represents a closed universe of types that can be included as
-- leaves of the same MerkleTree.
--
-- The 'MerkleLogHash' type family defines the hash function that is used for
-- Merkle trees within the universe.
--
-- The 'MerkleTagVal' type family is used to assing each type-constructor in the
-- universe a type-level 'Nat' that represents a 'Word16' value.
--
class MerkleUniverse k where
    type MerkleTagVal k (a :: k) = (g :: Nat) | g -> a

-- | Term level representation of the 'MerkleTagVal' of a type in a Merkle
-- universe.
--
tagVal :: forall u (t :: u) . KnownNat (MerkleTagVal u t) => Word16
tagVal = fromIntegral $ natVal (Proxy @(MerkleTagVal u t))

-- -------------------------------------------------------------------------- --
-- Hash Algorithms

class MerkleHashAlgorithmName a where
    merkleHashAlgorithmName :: T.Text

instance MerkleHashAlgorithmName Sha2_512_256 where
    merkleHashAlgorithmName = "SHA512t_256"
    {-# INLINE merkleHashAlgorithmName #-}

instance MerkleHashAlgorithmName Keccak256 where
    merkleHashAlgorithmName = "Keccak_256"
    {-# INLINE merkleHashAlgorithmName #-}

-- -------------------------------------------------------------------------- --
-- Merkle Log Entries

-- | A constraint that claims that a type is a Merkle universe and that its
-- 'MerkleTagVal' has a termlevel representation at runtime.
--
type InUniverse u (t :: u) = (MerkleUniverse u, KnownNat (MerkleTagVal u t))

-- | Class of types that can be used as entries in a merkle tree.
--
-- The 'Tag' associated type family tags each type that is an instance of
-- 'IsMerkleLogEntry' with a type from the respective Merkle universe.
--
-- The functions of the type class specify whether the entry corresponds to the
-- root of a nested Merkle tree or corresponds to an input node.
--
class (MerkleHashAlgorithm a, InUniverse u (Tag b)) => IsMerkleLogEntry a u b | b -> u where

    -- | Morralay, this type family is injective. Please, see the comment on
    -- the ChainwebHashTag type for details.
    --
    -- We don't enforce this here to keep the type machinary simple.
    --
    type Tag b :: u

    toMerkleNode
        :: b
        -> MerkleNodeType a

    fromMerkleNode
        :: MerkleNodeType a
        -> Either SomeException b

fromMerkleNodeM
    :: forall a u b m
    . MonadThrow m
    => IsMerkleLogEntry a u b
    => MerkleNodeType a
    -> m b
fromMerkleNodeM = either throwM return . fromMerkleNode @a
{-# INLINE fromMerkleNodeM #-}

-- -------------------------------------------------------------------------- --
-- Merkle Log Entries

-- | A data type that represents the Merkle log for a structure as
--
-- * a fixed size polymorphic list of header values and
-- * a monomorphic sequence of body values.
--
    -- Both the header and the body may possibly be empty. The type of the former is
-- represented by an empty type-level list, while the latter is represented by
-- 'Void'.
--
data MerkleLogEntries
    :: Type
        -- Hash Algorithm
    -> Type
        -- Universe
    -> [Type]
        -- HeaderTypes
    -> Type
        -- BodyType
    -> Type
  where
    MerkleLogBody
        :: IsMerkleLogEntry a u b
        => V.Vector b
        -> MerkleLogEntries a u '[] b

    (:+:)
        :: IsMerkleLogEntry a u h
        => h
        -> MerkleLogEntries a u t b
        -> MerkleLogEntries a u (h ': t) b

    -- TODO: should we support lazy lists/streams in the body or more
    -- generally abstracting a store the may be backed by a persisted
    -- database? All we need is the ability to enumerate items in order. We
    -- may also consider support to lookup the index of an item for creating
    -- proof.

infixr 5 :+:

emptyBody :: IsMerkleLogEntry a u b => MerkleLogEntries a u '[] b
emptyBody = MerkleLogBody mempty
{-# INLINE emptyBody #-}

mapLogEntries
    :: forall a u h s b
    . (forall x . IsMerkleLogEntry a u x => x -> b)
    -> MerkleLogEntries a u h s
    -> V.Vector b
mapLogEntries f m = V.concat $ go m
  where
    go :: forall h' . MerkleLogEntries a u h' s -> [V.Vector b]
    go (MerkleLogBody s) = [V.map f s]
    go (h :+: t) = V.singleton (f h) : go t
{-# INLINE mapLogEntries #-}

entriesHeaderSize :: MerkleLogEntries a u l s -> Int
entriesHeaderSize MerkleLogBody{} = 0
entriesHeaderSize (_ :+: t) = succ $ entriesHeaderSize t

entriesBody :: MerkleLogEntries a u l s -> V.Vector s
entriesBody (MerkleLogBody s) = s
entriesBody (_ :+: t) = entriesBody t
{-# INLINE entriesBody #-}

-- -------------------------------------------------------------------------- --
-- Merkle Log

-- | A merkle log represents values of 'IsMerkleLog' types in a generic way that
-- supports computing the root hash and a merkle tree.
--
data MerkleLog a u (h :: [Type]) (b :: Type) = MerkleLog
    { _merkleLogRoot :: {-# UNPACK #-} !(MerkleRoot a)
        -- ^ The root hash of the Merkle tree of the log.

    , _merkleLogEntries :: !(MerkleLogEntries a u h b)
        -- ^ The entries of the Merkle log.
        --
        -- Note: For creating proofs this isn't needed. Should we make this
        -- lazy, too? For large entries it may be somewhat costly to pull it
        -- from the store.
    }

_merkleLogTree
    :: forall a u h b
    . MerkleHashAlgorithm a
    => MerkleLog a u h b
    -> V1.MerkleTree a
_merkleLogTree = V1.merkleTree . _merkleLogLeafs

_merkleLogLeafs
    :: forall a u h b
    . MerkleHashAlgorithm a
    => MerkleLog a u h b
    -> [MerkleNodeType a]
_merkleLogLeafs = toList
    . mapLogEntries (toMerkleNodeTagged @a)
    . _merkleLogEntries

-- | Class of types which can be represented as a Merkle tree log.
--
-- An instance of 'HasMerkleLog' can be encoded as
--
-- 1. a header that consists of a fixed size polymorphic list 'IsMerkleEntry'
--    instances, and
-- 2. a body that consists of a monomorphic sequence of 'IsMerkleEntry'
--    instances.
--
class (MerkleUniverse u, MerkleHashAlgorithm a) => HasMerkleLog a u b | b -> u where
    type MerkleLogHeader b :: [Type]
        -- The header of the Merkle log representation of the type.

    type MerkleLogBody b :: Type
        -- the body of the Merkle log representation of the type.

    toLog :: b -> MerkleLog a u (MerkleLogHeader b) (MerkleLogBody b)
        -- ^ Transform a value into a Merkle log.
        --
        -- Often the root of the Merkle tree is used as identifier and is stored
        -- as part of the input structure. In those cases the function
        -- 'merkleLog' can be used to create the 'MerkleLog' from the root and
        -- the entries without forcing the computation of the Merkle tree field.

    fromLog :: MerkleLog a u (MerkleLogHeader b) (MerkleLogBody b) -> b
        -- ^ Recover a value from a Merkle log.

type MkLogType a u b = MerkleLog a u (MerkleLogHeader b) (MerkleLogBody b)

-- | Create a 'MerkleLog' from a 'MerkleRoot' and a sequence of 'MerkleLogEntry's.
--
merkleLog
    :: forall (a :: Type) u h b
    . MerkleHashAlgorithm a
    => MerkleRoot a
    -> MerkleLogEntries a u h b
    -> MerkleLog a u h b
merkleLog root entries = MerkleLog
    { _merkleLogRoot = root
    , _merkleLogEntries = entries
    }

-- | /Internal:/ Create a representation of Merkle nodes that are tagged with the
-- respective type from the Merkle universe.
--
toMerkleNodeTagged
    :: forall a u b
    . IsMerkleLogEntry a u b
    => b
    -> MerkleNodeType a
toMerkleNodeTagged b = case toMerkleNode @a @u @b b of
    InputNode bs -> InputNode @a
        $ buildByteString $ BB.word16BE tag <> BB.byteString bs
    TreeNode r -> TreeNode @a r
  where
    tag :: Word16
    tag = tagVal @u @(Tag b)

-- | /Internal:/ Decode Merkle nodes that are tagged with the respective type
-- from the Merkle universe.
--
fromMerkleNodeTagged
    :: forall a u b m
    . MonadThrow m
    => IsMerkleLogEntry a u b
    => MerkleNodeType a
    -> m b
fromMerkleNodeTagged (InputNode bs) = do
    w16 <- toWordBE @Word16 bs
    if w16 /= tag
        then throwM
            $ MerkleLogWrongTagException (Expected (sshow tag)) (Actual (sshow w16))
        else fromMerkleNodeM @a $ InputNode (B.drop 2 bs)
  where
    tag = tagVal @u @(Tag b)
fromMerkleNodeTagged r = fromMerkleNodeM @a r

-- | 'IsMerkleLog' values often include a hash of the value itself, which
-- represents cyclic dependency of a value on itself. This function allows to
-- create such an value from its representation as a sequence of merkle log
-- entries.
--
newMerkleLog
    :: forall a u h b
    . MerkleUniverse u
    => MerkleHashAlgorithm a
    => MerkleLogEntries a u h b
    -> MerkleLog a u h b
newMerkleLog entries = MerkleLog
    { _merkleLogRoot = merkleRoot
        $ toList
        $ mapLogEntries (toMerkleNodeTagged @a)
        $ entries
    , _merkleLogEntries = entries
    }

-- | /Internal:/ Get (first) header entry of given type from a Merkle log.
--
-- TODO:
-- * check that the header value is unique (requires traversing the list until
--   then end after a value is found)
--
class Index c (Hdr b) ~ i => HasHeader_ a u c b i | i b -> c where
    type Hdr b :: [Type]
    header :: b -> c
    headerPos :: Int
    headerDict :: b -> Dict (IsMerkleLogEntry a u c) c

instance HasHeader_ a u c (MerkleLog a u (c ': t) s) 'Z where
    type Hdr (MerkleLog a u (c ': t) s) = (c ': t)
    header (MerkleLog _ (c :+: _)) = c
    headerPos = 0
    headerDict (MerkleLog _ (c :+: _)) = Dict c

-- TODO:
--
-- * recurse only on entries
--
instance
    ( HasHeader_ a u c (MerkleLog a u t s) i
    , 'S i ~ Index c (x ': t) -- this effectively asserts that c and x are different
    )
    => HasHeader_ a u c (MerkleLog a u (x ': t) s) ('S i)
  where
    type Hdr (MerkleLog a u (x ': t) s) = (x ': t)
    header (MerkleLog x (_ :+: t)) = header @a @u (MerkleLog @a x t)
    headerPos = succ $ headerPos @a @u @c @(MerkleLog a u t s)
    headerDict (MerkleLog x (_ :+: t)) = headerDict @a @u (MerkleLog @a x t)

type HasHeader a u c b = HasHeader_ a u c b (Index c (Hdr b))

-- | Get the body sequence of a Merkle log.
--
body :: MerkleLog a u l s -> V.Vector s
body = entriesBody . _merkleLogEntries
{-# INLINE body #-}

-- | Get the number of entries in the header of a Merkle log.
--
headerSize :: MerkleLog a u l s -> Int
headerSize = entriesHeaderSize . _merkleLogEntries
{-# INLINE headerSize #-}

-- | Get the number of entries in the body of a Merkle log.
--
bodySize :: MerkleLog a u l s -> Int
bodySize = V.length . body
{-# INLINE bodySize #-}

-- | Compute the Merkle root hash for an instance of 'HasMerkleLog'.
--
-- This computes the merkle log and forces the merkle tree, which is linear in
-- the size of the @b@. For large logs the hash or the full 'MerkleTree' should
-- be cached.
--
computeMerkleLogRoot
    :: forall a u b
    . HasMerkleLog a u b
    => b
    -> MerkleRoot a
computeMerkleLogRoot = 
    merkleRoot . toList . mapLogEntries (toMerkleNodeTagged @a) . _merkleLogEntries . toLog @a
{-# INLINE computeMerkleLogRoot #-}

-- -------------------------------------------------------------------------- --
-- Proofs

-- | Create an inclusion proof for a value in the header of a Merkle log.
--
-- NOTE: The call to 'toLog' can be potentially expensive if the b body of the
-- log is large and the tree isn't memoized. However, for most applications,
-- either the header is empty or the body is small and recomputing it is cheap.
--
-- We could consider placing the header at the end of the tree. In that case we
-- could cache (all but at most logarithmic many hashes) of the body tree and
-- recompute just the part of the tree that depends on the header. However, for
-- large trees, we store a full cached version of the tree in any case, so we
-- should use that instead. Another option would be to use two separate trees,
-- but, again, our current use case wouldn't justify the overhead.
--
headerProofV1
    :: forall c a u b m
    . MonadThrow m
    => HasHeader a u c (MkLogType a u b)
    => HasMerkleLog a u b
    => b
    -> m (V1.MerkleProof a)
headerProofV1 = uncurry3 (V1.merkleTreeProof @a) . headerTree @c @a
{-# INLINE headerProofV1 #-}

headerProofV2
    :: forall c a u b m
    . MonadThrow m
    => HasHeader a u c (MkLogType a u b)
    => HasMerkleLog a u b
    => b
    -> m (MerkleProof a)
headerProofV2 b = merkleProof @a p (_merkleLogLeafs @a (toLog @a b))
  where
    p = headerPos @a @u @c @(MkLogType a u b)
{-# INLINE headerProofV2 #-}

-- | Create the parameters for creating nested inclusion proofs with the
-- 'merkleProof_' of the merkle-log package.
--
-- This function returns the proof subject, the position of the subject, and the
-- Merkle tree and should be used for the leaf tree in the nested proof.
--
headerTree
    :: forall c a u b
    . HasHeader a u c (MkLogType a u b)
    => HasMerkleLog a u b
    => b
    -> (MerkleNodeType a, Int, V1.MerkleTree a)
headerTree b = (node, p, _merkleLogTree @a mlog)
  where
    mlog = toLog @a b
    p = headerPos @a @u @c @(MkLogType a u b)
    node = case headerDict @a @u @c mlog of
        Dict hdr -> toMerkleNodeTagged @a hdr

-- | Create the parameters for creating nested inclusion proofs with the
-- 'merkleProof_' of the merkle-log package.
--
-- This function returns the position of the input and the Merkle tree, but not
-- the subject. It should be used for inner trees in the nested proof.
--
headerTree_
    :: forall c a u b
    . HasHeader a u c (MkLogType a u b)
    => HasMerkleLog a u b
    => b
    -> (Int, V1.MerkleTree a)
headerTree_ b = (p, _merkleLogTree @a mlog)
  where
    mlog = toLog @a b
    p = headerPos @a @u @c @(MkLogType a u b)

-- | Create an inclusion proof for a value in the body of a Merkle log.
--
-- TODO: it is not clear if the result of 'toLog' (which contains the cached
-- tree) is memoized on the heap. (i.e. depends on where @b@ is coming from). We
-- should either make sure that it is memoized or provide a version that takes a
-- 'MerkleLog' value.
--
bodyProofV1
    :: forall a u b m
    . MonadThrow m
    => HasMerkleLog a u b
    => b
    -> Int
        -- ^ the index in the body of the log
    -> m (V1.MerkleProof a)
bodyProofV1 b = uncurry3 V1.merkleTreeProof . bodyTree @a b
{-# INLINE bodyProofV1 #-}

bodyProofV2
    :: forall a u b m
    . MonadThrow m
    => HasMerkleLog a u b
    => b
    -> Int
        -- ^ the index in the body of the log
    -> m (MerkleProof a)
bodyProofV2 b i = merkleProof @a p (_merkleLogLeafs @a mlog)
  where
    mlog = (toLog @a b)
    p = i + headerSize mlog
{-# INLINE bodyProofV2 #-}

-- | Create the parameters for creating nested inclusion proofs with the
-- 'merkleProof_' of the merkle-log package.
--
-- This function returns the proof subject, the position of the subject, and the
-- Merkle tree and should be used for the leaf tree in the nested proof.
--
bodyTree
    :: forall a u b
    . HasMerkleLog a u b
    => b
    -> Int
        -- ^ the index in the body of the log
    -> (MerkleNodeType a, Int, V1.MerkleTree a)
bodyTree b i = (node, i_, _merkleLogTree @a mlog)
  where
    mlog = toLog @a b
    i_ = i + headerSize mlog
    node = mapLogEntries (toMerkleNodeTagged @a) (_merkleLogEntries mlog) V.! i_

-- | Create the parameters for creating nested inclusion proofs with the
-- 'merkleProof_' of the merkle-log package.
--
-- This function returns the position of the input and the Merkle tree, but not
-- the subject. It should be used for inner trees in the nested proof.
--
bodyTree_
    :: forall a u b
    . HasMerkleLog a u b
    => b
    -> Int
        -- ^ the index in the body of the log
    -> (Int, V1.MerkleTree a)
bodyTree_ b i = (i_, _merkleLogTree @a mlog)
  where
    mlog = toLog @a b
    i_ = i + headerSize mlog

-- | Extract the proof subject from a 'V1.MerkleProof' value.
--
proofSubject
    :: forall a u b m
    . MonadThrow m
    => IsMerkleLogEntry a u b
    => V1.MerkleProof a
    -> m b
proofSubject p = fromMerkleNodeTagged @a subj
  where
    V1.MerkleProofSubject subj = V1._merkleProofSubject p
{-# INLINE proofSubject #-}

-- | Extract the proof claim from a 'MerkleProof' value.
--
proofClaim
    :: forall a u b m
    . MonadThrow m
    => IsMerkleLogEntry a u b
    => MerkleProof a
    -> m b
proofClaim = fromMerkleNodeTagged @a . _merkleProofClaim
{-# INLINE proofClaim #-}

-- -------------------------------------------------------------------------- --
-- Tools Defining Instances

encodeMerkleInputNode
    :: (b -> Put)
    -> b
    -> MerkleNodeType a
encodeMerkleInputNode encode = InputNode . runPutS . encode

decodeMerkleInputNode
    :: MonadThrow m
    => Get b
    -> MerkleNodeType a
    -> m b
decodeMerkleInputNode decode (InputNode bs) = runGetS decode bs
decodeMerkleInputNode _ (TreeNode _) = throwM expectedInputNodeException

encodeMerkleTreeNode :: Coercible a (MerkleRoot alg) => a -> MerkleNodeType alg
encodeMerkleTreeNode = TreeNode . coerce

decodeMerkleTreeNode
    :: MonadThrow m
    => Coercible (MerkleRoot alg) a
    => MerkleNodeType alg
    -> m a
decodeMerkleTreeNode (TreeNode bs) = return $! coerce bs
decodeMerkleTreeNode (InputNode _) = throwM expectedTreeNodeException

-- -------------------------------------------------------------------------- --
-- Support for Deriving Via

-- | Support for deriving IsMerkleLogEntry for types that are newtype wrappers of
-- 'MerkleRoot' via the @DerivingVia@ extension.
--
newtype MerkleRootLogEntry a (t :: u) = MerkleRootLogEntry (MerkleRoot a)

instance (MerkleHashAlgorithm a, InUniverse u t) => IsMerkleLogEntry a u (MerkleRootLogEntry a (t :: u)) where
    type Tag (MerkleRootLogEntry a t) = t
    toMerkleNode (MerkleRootLogEntry r) = TreeNode r
    fromMerkleNode (TreeNode !x) = return $! MerkleRootLogEntry x
    fromMerkleNode (InputNode _) = throwM expectedTreeNodeException
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

