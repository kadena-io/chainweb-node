{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- * Merkle Log
, MerkleLog(..)
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
, headerProof
, headerTree
, headerTree_
, bodyProof
, bodyTree
, bodyTree_
, proofSubject

-- * Utils for Defining Instances
, decodeMerkleInputNode
, encodeMerkleInputNode

, decodeMerkleTreeNode
, encodeMerkleTreeNode

-- ** IsMerkleLogEntry instance for use with @deriving via@
, ByteArrayMerkleLogEntry(..)
, MerkleRootLogEntry(..)
, Word8MerkleLogEntry(..)
, Word16BeMerkleLogEntry(..)
, Word32BeMerkleLogEntry(..)
, Word64BeMerkleLogEntry(..)

-- * Exceptions
, MerkleLogException(..)
, expectedInputNodeException
, expectedTreeNodeException
) where

import Control.Monad.Catch

import Crypto.Hash.Algorithms

import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Coerce
import Data.Foldable
import Data.Kind
import Data.Memory.Endian
import qualified Data.Memory.Endian as BA
import Data.MerkleLog hiding (Expected, Actual)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word

import Foreign.Storable

import GHC.TypeNats

import System.IO.Unsafe

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

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (a,b,c) = f a b c

fromWordBE :: forall w b . BA.ByteArray b => ByteSwap w => w -> b
fromWordBE w = BA.allocAndFreeze (sizeOf (undefined :: w)) $ \ptr -> poke ptr (BA.toBE w)

unsafeToWordBE :: BA.ByteSwap w => BA.ByteArrayAccess b => b -> w
unsafeToWordBE bytes = BA.fromBE . unsafeDupablePerformIO $ BA.withByteArray bytes peek

toWordBE
    :: forall w b m
    . MonadThrow m
    => BA.ByteSwap w
    => BA.ByteArrayAccess b
    => b
    -> m w
toWordBE bytes
    | BA.length bytes < sizeOf (undefined :: w) = throwM
        $ MerkleLogDecodeException "failed to parse Word from bytes: not enough bytes"
    | otherwise = return $! unsafeToWordBE bytes

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
    type MerkleTagVal k (a :: k) :: Nat

-- | Term level representation of the 'MerkleTagVal' of a type in a Merkle
-- universe.
--
tagVal :: forall u (t :: u) . KnownNat (MerkleTagVal u t) => Word16
tagVal = fromIntegral $ natVal (Proxy @(MerkleTagVal u t))

-- -------------------------------------------------------------------------- --
-- Hash Algorithms

type MerkleHashAlgorithm = HashAlgorithm

class MerkleHashAlgorithmName a where
    merkleHashAlgorithmName :: T.Text

instance MerkleHashAlgorithmName SHA512t_256 where
    merkleHashAlgorithmName = "SHA512t_256"
    {-# INLINE merkleHashAlgorithmName #-}

instance MerkleHashAlgorithmName Keccak_256 where
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
    type Tag b :: u

    toMerkleNode
        :: b
        -> MerkleNodeType a B.ByteString

    fromMerkleNode
        :: MerkleNodeType a B.ByteString
        -> Either SomeException b

fromMerkleNodeM
    :: forall a u b m
    . MonadThrow m
    => IsMerkleLogEntry a u b
    => MerkleNodeType a B.ByteString
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

    , _merkleLogTree :: {- Lazy -} MerkleTree a
        -- ^ The merkle tree for the entries of the log. It is required to
        -- compute the root and for creating a merkle proofs.
        --
        -- For some types, computing the merkle tree can be expensive. Therefore
        -- it is instantiated lazily and only forced if actually required.
    }

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
-- The '_merkleLogTree' fields is instantiated lazily.
--
merkleLog
    :: forall a u h b
    . MerkleHashAlgorithm a
    => MerkleRoot a
    -> MerkleLogEntries a u h b
    -> MerkleLog a u h b
merkleLog root entries = MerkleLog
    { _merkleLogRoot = root
    , _merkleLogEntries = entries
    , _merkleLogTree = {- Lazy -} merkleTree
        $ toList
        $ mapLogEntries (toMerkleNodeTagged @a) entries
    }

-- | /Internal:/ Create a representation Merkle nodes that are tagged with the
-- respedtive type from the Merkle universe.
--
toMerkleNodeTagged
    :: forall a u b
    . IsMerkleLogEntry a u b
    => b
    -> MerkleNodeType a B.ByteString
toMerkleNodeTagged b = case toMerkleNode @a @u @b b of
    InputNode bytes -> InputNode @a @B.ByteString
        $ fromWordBE @Word16 tag <> bytes
    TreeNode r -> TreeNode @a r
  where
    tag :: Word16
    tag = tagVal @u @(Tag b)

-- | /Internal:/ Decode Merkle nodes that are tagged with the respedtive type
-- from the Merkle universe.
--
fromMerkleNodeTagged
    :: forall a u b m
    . MonadThrow m
    => IsMerkleLogEntry a u b
    => MerkleNodeType a B.ByteString
    -> m b
fromMerkleNodeTagged (InputNode bytes) = do
    w16 <- toWordBE @Word16 bytes
    if w16 /= tag
        then throwM
            $ MerkleLogWrongTagException (Expected (sshow tag)) (Actual (sshow w16))
        else fromMerkleNodeM @a $ InputNode (B.drop 2 bytes)
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
    { _merkleLogRoot = merkleRoot tree
    , _merkleLogEntries = entries
    , _merkleLogTree = tree
    }
  where
    tree = merkleTree $ toList $ mapLogEntries (toMerkleNodeTagged @a) entries

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
    header (MerkleLog _ (c :+: _) _) = c
    headerPos = 0
    headerDict (MerkleLog _ (c :+: _) _) = Dict c

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
    header (MerkleLog x (_ :+: t) y) = header @a @u (MerkleLog @a x t y)
    headerPos = succ $ headerPos @a @u @c @(MerkleLog a u t s)
    headerDict (MerkleLog x (_ :+: t) y) = headerDict @a @u (MerkleLog @a x t y)

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
computeMerkleLogRoot = merkleRoot . _merkleLogTree . toLog @a
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
headerProof
    :: forall c a u b m
    . MonadThrow m
    => HasHeader a u c (MkLogType a u b)
    => HasMerkleLog a u b
    => b
    -> m (MerkleProof a)
headerProof = uncurry3 merkleProof . headerTree @c @a
{-# INLINE headerProof #-}

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
    -> (MerkleNodeType a B.ByteString, Int, MerkleTree a)
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
    -> (Int, MerkleTree a)
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
bodyProof
    :: forall a u b m
    . MonadThrow m
    => HasMerkleLog a u b
    => b
    -> Int
        -- ^ the index in the body of the log
    -> m (MerkleProof a)
bodyProof b = uncurry3 merkleProof . bodyTree @a b
{-# INLINE bodyProof #-}

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
    -> (MerkleNodeType a B.ByteString, Int, MerkleTree a)
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
    -> (Int, MerkleTree a)
bodyTree_ b i = (i_, _merkleLogTree @a mlog)
  where
    mlog = toLog @a b
    i_ = i + headerSize mlog

-- | Extract the proof subject from a 'MerkleProof' value.
--
proofSubject
    :: forall a u b m
    . MonadThrow m
    => IsMerkleLogEntry a u b
    => MerkleProof a
    -> m b
proofSubject p = fromMerkleNodeTagged @a subj
  where
    MerkleProofSubject subj = _merkleProofSubject p
{-# INLINE proofSubject #-}

-- -------------------------------------------------------------------------- --
-- Tools Defining Instances

encodeMerkleInputNode
    :: (b -> Put)
    -> b
    -> MerkleNodeType a B.ByteString
encodeMerkleInputNode encode = InputNode . runPutS . encode

decodeMerkleInputNode
    :: MonadThrow m
    => Get b
    -> MerkleNodeType a B.ByteString
    -> m b
decodeMerkleInputNode decode (InputNode bytes) = runGetS decode bytes
decodeMerkleInputNode _ (TreeNode _) = throwM expectedInputNodeException

encodeMerkleTreeNode :: Coercible a (MerkleRoot alg) => a -> MerkleNodeType alg x
encodeMerkleTreeNode = TreeNode . coerce

decodeMerkleTreeNode
    :: MonadThrow m
    => Coercible (MerkleRoot alg) a
    => MerkleNodeType alg x
    -> m a
decodeMerkleTreeNode (TreeNode bytes) = return $! coerce bytes
decodeMerkleTreeNode (InputNode _) = throwM expectedTreeNodeException

-- -------------------------------------------------------------------------- --
-- Support for Deriving Via

-- | Support for deriving IsMerkleLogEntry for types that are an instance of
-- 'BA.ByteArray' via the @DerivingVia@ extension.
--
newtype ByteArrayMerkleLogEntry u (t :: u) b = ByteArrayMerkleLogEntry b

instance
    (MerkleHashAlgorithm a, InUniverse u t, BA.ByteArray b)
    => IsMerkleLogEntry a u (ByteArrayMerkleLogEntry u (t :: u) b)
  where
    type Tag (ByteArrayMerkleLogEntry u t b) = t
    toMerkleNode (ByteArrayMerkleLogEntry b) = InputNode $ BA.convert b
    fromMerkleNode (InputNode x) = return $! ByteArrayMerkleLogEntry $! BA.convert x
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

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

-- | Support for deriving IsMerkleLogEntry for types that are newtype wrappers of
-- 'Word8' via the @DerivingVia@ extension.
--
newtype Word8MerkleLogEntry (t :: u) = Word8MerkleLogEntry { _getWord8LogEntry :: Word8 }

instance
    (MerkleHashAlgorithm a, InUniverse u t) => IsMerkleLogEntry a u (Word8MerkleLogEntry (t :: u))
  where
    type Tag (Word8MerkleLogEntry t) = t
    toMerkleNode = InputNode . B.singleton . _getWord8LogEntry
    fromMerkleNode (InputNode x) = case B.uncons x of
        Nothing -> throwM
            $ MerkleLogDecodeException "failed to deserialize Word8 from empty ByteString"
        Just (!c,"") -> return $! Word8MerkleLogEntry c
        Just _ -> throwM
            $ MerkleLogDecodeException "failed to deserialize Word8. Pending bytes in input"
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- | Support for deriving IsMerkleLogEntry for types that are newtype wrappers of
-- 'Word16' via the @DerivingVia@ extension.
--
newtype Word16BeMerkleLogEntry (t :: u) = Word16BeMerkleLogEntry
    { _getWord16BeLogEntry :: Word16 }

instance
    (MerkleHashAlgorithm a, InUniverse u t) => IsMerkleLogEntry a u (Word16BeMerkleLogEntry (t :: u))
  where
    type Tag (Word16BeMerkleLogEntry t) = t
    toMerkleNode = InputNode . fromWordBE . _getWord16BeLogEntry
    fromMerkleNode (InputNode x) = Word16BeMerkleLogEntry <$> toWordBE x
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- | Support for deriving IsMerkleLogEntry for types that are newtype wrappers of
-- 'Word32' via the @DerivingVia@ extension.
--
newtype Word32BeMerkleLogEntry (t :: u) = Word32BeMerkleLogEntry
    { _getWord32BeLogEntry :: Word32 }

instance
    (MerkleHashAlgorithm a, InUniverse u t) => IsMerkleLogEntry a u (Word32BeMerkleLogEntry (t :: u))
  where
    type Tag (Word32BeMerkleLogEntry t) = t
    toMerkleNode = InputNode . fromWordBE . _getWord32BeLogEntry
    fromMerkleNode (InputNode x) = Word32BeMerkleLogEntry <$> toWordBE x
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- | Support for deriving IsMerkleLogEntry for types that are newtype wrappers of
-- 'Word64' via the @DerivingVia@ extension.
--
newtype Word64BeMerkleLogEntry (t :: u) = Word64BeMerkleLogEntry
    { _getWord64BeLogEntry :: Word64 }

instance
    (MerkleHashAlgorithm a, InUniverse u t) => IsMerkleLogEntry a u (Word64BeMerkleLogEntry (t :: u))
  where
    type Tag (Word64BeMerkleLogEntry t) = t
    toMerkleNode = InputNode . fromWordBE . _getWord64BeLogEntry
    fromMerkleNode (InputNode x) = Word64BeMerkleLogEntry <$> toWordBE x
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}
