{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.TreeDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.TreeDB
(
-- * Exceptions
  TreeDbException(..)

-- * Query Parameters
, MinRank(..)
, MaxRank(..)
, Limit(..)
, NextItem(..)
, isExclusive
, isInclusive
, nextItemToText
, nextItemFromText
, LowerBound(..)
, UpperBound(..)
, Bounds(..)

-- * Tree Database
, TreeDbEntry(..)
, type DbKey
, TreeDb(..)

-- * Utils

-- ** Limiting and Seeking a Stream
, Eos(..)
, isEos
, limitStream
, seekStream
, seekStreamSet

-- ** Query branches
, getBranch

-- * Query leaves
, limitLeaves
, descend
, ascend
, ascendIntersect

-- ** Lookups
, lookupM
, lookupStreamM

-- ** Stream a foldable value
, foldableEntries

-- * properties
, properties
) where

import Control.Arrow ((***))
import Control.Lens ((&))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.Trans

import Data.Aeson
import Data.Functor.Of
import Data.Hashable
import qualified Data.HashSet as HS
import Data.Kind
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T
import Data.Typeable

import GHC.Generics

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

import Test.QuickCheck

import Test.QuickCheck.Instances ({- Arbitrary Natural -})

-- internal modules

import Chainweb.Utils hiding ((==>))

-- -------------------------------------------------------------------------- --
-- Exceptions

data TreeDbException db
    = TreeDbParentMissing (DbEntry db)
    | TreeDbKeyNotFound (DbKey db)
    | TreeDbInvalidRank (DbEntry db)

instance (Show (DbEntry db), Show (DbKey db)) => Show (TreeDbException db) where
    show (TreeDbParentMissing e) = "TreeDbParentMissing: " ++ show e
    show (TreeDbKeyNotFound e) = "TreeDbKeyNotFound: " ++ show e
    show (TreeDbInvalidRank e) = "TreeDbInvalidRank: " ++ show e

instance
    ( Show (DbEntry db)
    , Show (DbKey db)
    , Typeable db
    )
    => Exception (TreeDbException db)

-- -------------------------------------------------------------------------- --
-- * Query Parameters

-- -------------------------------------------------------------------------- --
-- ** Rank limits

newtype MinRank = MinRank (Min Natural)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable)
    deriving newtype (Semigroup, Num, Enum, Ord)

_getMinRank :: MinRank -> Natural
_getMinRank (MinRank (Min r)) = r

newtype MaxRank = MaxRank (Max Natural)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable)
    deriving newtype (Semigroup, Num, Enum, Ord)

_getMaxRank :: MaxRank -> Natural
_getMaxRank (MaxRank (Max r)) = r

-- -------------------------------------------------------------------------- --
-- ** Page/Stream Limits

newtype Limit = Limit { _getLimit :: Natural }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable)
    deriving newtype (Num, Real, Integral, Enum, Ord)

instance Arbitrary Limit where
  arbitrary = Limit <$> arbitrary

data NextItem k
    = Inclusive k
    | Exclusive k
    deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

isInclusive :: NextItem k -> Bool
isInclusive Inclusive{} = True
isInclusive _ = False

isExclusive :: NextItem k -> Bool
isExclusive Exclusive{} = True
isExclusive _ = False

nextItemToText :: HasTextRepresentation k => NextItem k -> T.Text
nextItemToText (Inclusive k) = "inclusive:" <> toText k
nextItemToText (Exclusive k) = "exclusive:" <> toText k

nextItemFromText :: MonadThrow m => HasTextRepresentation k => T.Text -> m (NextItem k)
nextItemFromText t = case T.break (== ':') t of
    (a, b)
        | a == "inclusive" -> Inclusive <$> fromText (T.drop 1 b)
        | a == "exclusive" -> Exclusive <$> fromText (T.drop 1 b)
        | T.null b -> throwM . TextFormatException $ "missing ':' in next item: \"" <> t <> "\"."
        | otherwise -> throwM $ TextFormatException $ "unrecognized next item: \"" <> t <> "\"."

instance HasTextRepresentation k => HasTextRepresentation (NextItem k) where
    toText = nextItemToText
    {-# INLINE toText #-}
    fromText = nextItemFromText
    {-# INLINE fromText #-}

instance HasTextRepresentation k => ToJSON (NextItem k) where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance HasTextRepresentation k => FromJSON (NextItem k) where
    parseJSON = parseJsonFromText "NextItem"
    {-# INLINE parseJSON #-}

-- | Data type to indicate end of stream
--
newtype Eos = Eos { _getEos :: Bool }
    deriving stock (Eq, Show, Ord, Generic)
    deriving newtype (Enum, Bounded, FromJSON, ToJSON)

isEos :: Eos -> Bool
isEos = _getEos

-- -------------------------------------------------------------------------- --
-- ** Branch Bounds

newtype LowerBound k = LowerBound { _getLowerBound :: k }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Hashable)

newtype UpperBound k = UpperBound { _getUpperBound :: k }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Hashable)

-- | A simple pair of bounds.
data Bounds k = Bounds { _lower :: !(LowerBound k), _upper :: !(UpperBound k) }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Hashable)

-- -------------------------------------------------------------------------- --
-- * TreeDbEntry

class
    ( Show e
    , Show (Key e)
    , Eq e
    , Eq (Key e)
    , Hashable e
    , Hashable (Key e)
    )
    => TreeDbEntry e
  where
    type family Key e :: Type
    key :: e -> Key e
    rank :: e -> Natural
    parent :: e -> Maybe (Key e)

type DbKey db = Key (DbEntry db)

-- -------------------------------------------------------------------------- --
-- TreeDb

class (Typeable db, TreeDbEntry (DbEntry db)) => TreeDb db where

    {-# MINIMAL
        lookup,
        children,
        (allEntries | entries),
        (leafKeys | leafEntries),
        (insert | insertStream) #-}

    type family DbEntry db :: Type

    -- ---------------------------------------------------------------------- --
    -- * Lookup

    -- | Lookup a single entry by its key.
    --
    -- Implementations are expected to provide \(\mathcal{O}(1)\) performance.
    --
    -- prop> lookup db k == S.head_ (entries db k 1 Nothing Nothing)
    --
    lookup
        :: db
        -> DbKey db
        -> IO (Maybe (DbEntry db))

    -- --------------------------------------------------------------------------
    -- * Children

    -- | All the children of a given node in at some point in time in
    -- some arbitrary order.
    --
    -- The number is expected to be small enough to be returned in a single call
    -- even for remote backends. FIXME: this may be a DOS vulnerability.
    --
    -- TODO: should we give a default implementation? It would be terribly
    -- slow.
    --
    children
        :: db
        -> DbKey db
        -> S.Stream (Of (DbKey db)) IO ()

    -- TODO: add a function that takes a stream instead of a single key.
    -- TODO: implement limits

    -- ---------------------------------------------------------------------- --
    -- * Keys and Entries
    --
    -- The following streams are weakly consistent in the count of items. That
    -- is, different, concurrent or repeated queries may return a different
    -- number of items. However, the streams are strongly consistent with
    -- respect to prefixes. That is the order of returned items never changes.
    --

    -- | This stream returns a prefix of the keys of the nodes in the tree in
    -- ascending order starting from the given key or the genesis block key.
    --
    -- This stream doesn't block indefinitely. If there is no further entry
    -- available it terminates and returns a number of returned items and a
    -- cursor. Implementations should block on IO at most a constant amount of
    -- time.
    --
    -- The default implementation is based on 'entries', which in most cases
    -- doesn't give good performance.
    --
    keys
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> S.Stream (Of (DbKey db)) IO (Natural, Eos)
    keys db k l mir mar = S.map key $ entries db k l mir mar
    {-# INLINEABLE keys #-}

    -- | The infinite stream of all keys in ascending order. The stream may
    -- block indefinitely.
    --
    allKeys
        :: db
        -> Maybe (NextItem (DbKey db))
        -> S.Stream (Of (DbKey db)) IO ()
    allKeys db = go
      where
        go x = do
            n :> (_, eos) <- keys db x Nothing Nothing Nothing
                & S.copy
                & S.last
            case eos of
                Eos True -> error "code invariant violation"
                Eos False -> go (Exclusive <$> n)
    {-# INLINEABLE allKeys #-}

    -- | This stream returns a prefix of the entries of the nodes in the tree in
    -- ascending order starting from the given key or the genesis block.
    --
    -- This stream doesn't block indefinitely. If there is no further entry
    -- available it terminates and returns a number of returned items and a
    -- cursor. Implementations should block on IO at most a constant amount of
    -- time.
    --
    -- The default implementation is based on 'allEntries', which in most cases
    -- doesn't give good performance.
    --
    entries
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> S.Stream (Of (DbEntry db)) IO (Natural, Eos)
    entries db k l mir mar = allEntries db Nothing
        & applyRank mir mar
        & timeoutStream 1000 {- microseconds -}
        & void
        & seekLimitStream key k l
    {-# INLINEABLE entries #-}

    -- | The infinite stream of all entries in ascending order. The stream may
    -- block indefinitely.
    --
    -- The default implementation is based on @entries@ and spins in a busy loop
    -- while waiting for new entries.
    --
    allEntries
        :: db
        -> Maybe (NextItem (DbKey db))
        -> S.Stream (Of (DbEntry db)) IO ()
    allEntries db = go
      where
        go x = do
            n :> (_, eos) <- entries db x Nothing Nothing Nothing
                & S.copy
                & S.last
            case eos of
                Eos True -> error "code invariant violation"
                Eos False -> go (Exclusive . key <$> n)
    {-# INLINEABLE allEntries #-}

    -- ---------------------------------------------------------------------- --
    -- Leaves

    -- | A set of leaves in ascending order.
    --
    -- The semantics are as follows:
    --
    -- A set of nodes is returned such that each node in the tree, that has a
    -- rank within the given range of ranks, is either a successor or a
    -- predecessor of the nodes in the set.
    --
    -- If no minimum rank is given, a minimum rank of zero is assumed. If no
    -- maximum rank is given, a maximum rank if infinity is assumed.
    --
    -- Note that, if the minimum rank is zero, returning the genesis block is
    -- always a legal result, but implementations should try to return a large
    -- set.
    --
    -- The default implementation has poor performance if the maximum rank
    -- parameter is not Nothing.
    --
    leafEntries
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> S.Stream (Of (DbEntry db)) IO (Natural, Eos)
    leafEntries db k l mir mar = leafKeys db k l mir mar
        & lookupStreamM db
    {-# INLINEABLE leafEntries #-}

    leafKeys
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> S.Stream (Of (DbKey db)) IO (Natural, Eos)
    leafKeys db k l mir mar = leafEntries db k l mir mar
        & S.map key
    {-# INLINEABLE leafKeys #-}

    -- ---------------------------------------------------------------------- --
    -- * Branches

    -- | @branchKeys n l mir mar lower upper@ returns all nodes within the given
    -- range of minimum rank @mir@ and maximun rank @mar@ that are predecessors
    -- of nodes in @upper@ and not predecessors of any node in @lower@, starting
    -- at the entry after @n@. The number of itmes in the result is limited by
    -- @l@. Items are returned in descending order.
    --
    -- The result stream doesn't block. It may return less than the requested
    -- number of items.
    --
    -- FIXME: check existence of the given bounds.
    --
    branchKeys
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> HS.HashSet (LowerBound (DbKey db))
            -- Upper limits
        -> HS.HashSet (UpperBound (DbKey db))
            -- Lower limits
        -> S.Stream (Of (DbKey db)) IO (Natural, Eos)
    branchKeys db k l mir mar lower uppper = S.map key
        $ branchEntries db k l mir mar lower uppper
    {-# INLINEABLE branchKeys #-}

    -- | @branchKeys n l mir mar lower upper@ returns all nodes within the given
    -- range of minimum rank @mir@ and maximun rank @mar@ that are predecessors
    -- of nodes in @upper@ and not predecessors of any node in @lower@, starting
    -- at the entry after @n@. The number of itmes in the result is limited by
    -- @l@. Items are returned in descending order.
    --
    -- The result stream doesn't block. It may return less than the requested
    -- number of items.
    --
    branchEntries
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> HS.HashSet (LowerBound (DbKey db))
        -> HS.HashSet (UpperBound (DbKey db))
        -> S.Stream (Of (DbEntry db)) IO (Natural, Eos)
    branchEntries db k l mir mar lower upper = getBranch db lower upper
        & applyRank mir mar
        & seekLimitStream key k l
    {-# INLINEABLE branchEntries #-}

    -- ---------------------------------------------------------------------- --
    -- * Insertion

    insertStream
        :: db
        -> S.Stream (Of (DbEntry db)) IO ()
        -> IO ()
    insertStream = S.mapM_ . insert
    {-# INLINEABLE insertStream #-}

    insert
        :: db
        -> DbEntry db
        -> IO ()
    insert db = insertStream db . S.yield
    {-# INLINEABLE insert #-}

    -- ---------------------------------------------------------------------- --
    -- Misc

    -- | Maximum rank of all entries in the database.
    --
    -- prop> maxRank db == head (branches db Nothing (Just 1))
    --
    --
    maxRank :: db -> IO Natural
    maxRank db = fmap (rank . fromJust)
        $ S.head_
        $ leafEntries db Nothing (Just 1) Nothing Nothing
    {-# INLINEABLE maxRank #-}

-- -------------------------------------------------------------------------- --
-- Utils

-- | Filter the stream of entries for entries in a range of ranks.
--
applyRank
    :: forall e m
    . TreeDbEntry e
    => Monad m
    => Maybe MinRank
        -- ^ Return just the entries that have the given minimum rank.
    -> Maybe MaxRank
        -- ^ Return just the entries that have the given maximum rank.
    -> S.Stream (Of e) m ()
    -> S.Stream (Of e) m ()
applyRank l u
    = maybe id (\x -> S.filter (\e -> rank e <= x)) (_getMaxRank <$> u)
    . maybe id (\x -> S.filter (\e -> rank e >= x)) (_getMinRank <$> l)

-- | @getBranch s a b@ returns all nodes that are predecessors of nodes in @a@
-- and not predecessors of any node in @b@. Entries are returned in descending
-- order.
--
getBranch
    :: forall db
    . TreeDb db
    => db
    -> HS.HashSet (LowerBound (DbKey db))
    -> HS.HashSet (UpperBound (DbKey db))
    -> S.Stream (Of (DbEntry db)) IO ()
getBranch db lowerBounds upperBounds = do
    lowers <- getEntriesHs $ HS.map _getLowerBound lowerBounds
    uppers <- getEntriesHs $ HS.map _getUpperBound upperBounds

    let mar = L.maximum $ HS.map rank (lowers <> uppers)

    go mar (active mar lowers mempty) (active mar uppers mempty)
  where
    getEntriesHs = lift . streamToHashSet_ . lookupStreamM db . S.each
    getParentsHs = lift . streamToHashSet_ . lookupParentStreamM GenesisParentNone db . S.each

    -- prop> all ((==) r . rank) $ snd (active r s c)
    --
    active
        :: Natural
        -> HS.HashSet (DbEntry db)
        -> HS.HashSet (DbEntry db)
        -> (HS.HashSet (DbEntry db), HS.HashSet (DbEntry db))
    active r s c =
        let (a, b) = hsPartition (\x -> rank x < r) s
        in (a, c <> b)

    -- | The size of the input sets decreases monotonically. Space complexity is
    -- linear in the input and constant in the output size.
    --
    go
        :: Natural
        -> (HS.HashSet (DbEntry db), HS.HashSet (DbEntry db))
        -> (HS.HashSet (DbEntry db), HS.HashSet (DbEntry db))
        -> S.Stream (Of (DbEntry db)) IO ()
    go r (ls0, ls1) (us0, us1)
        | HS.null us0 && HS.null us1 = return ()
        | otherwise = do
            let us1' = us1 `HS.difference` ls1
            mapM_ S.yield us1'
            us1p <- getParentsHs us1'
            ls1p <- getParentsHs ls1
            let r' = pred r
            go r' (active r' ls0 ls1p) (active r' us0 us1p)

    hsPartition
        :: Hashable a
        => Eq a
        => (a -> Bool)
        -> HS.HashSet a
        -> (HS.HashSet a, HS.HashSet a)
    hsPartition p = (HS.fromList *** HS.fromList) . L.partition p . HS.toList

-- -------------------------------------------------------------------------- --
-- leaves

limitLeaves
    :: TreeDb db
    => db
    -> Maybe MinRank
    -> Maybe MaxRank
    -> S.Stream (Of (DbEntry db)) IO x
    -> S.Stream (Of (DbEntry db)) IO x
limitLeaves db mir mar s = s
    & maybe id (flip S.for .ascend db) mir
    & maybe id (S.mapM . descend db) mar
    & nub

descend
    :: TreeDb db
    => db
    -> MaxRank
    -> DbEntry db
    -> IO (DbEntry db)
descend db (MaxRank (Max r)) = go
  where
    go e
        | rank e > r = lookupParentM GenesisParentThrow db e >>= go
        | otherwise = return e

ascend
    :: TreeDb db
    => db
    -> MinRank
    -> DbEntry db
    -> S.Stream (Of (DbEntry db)) IO ()
ascend db (MinRank (Min r)) = go
  where
    go e
        | rank e < r = S.for (children db (key e) & lookupStreamM db) go
        | otherwise = S.yield e

-- | @ascencIntersect db s e@ returns the intersection of the successors of
-- @e@ with the set @s@.
--
-- TODO: use rank to prune the search
--
ascendIntersect
    :: TreeDb db
    => db
    -> HS.HashSet (DbKey db)
    -> DbEntry db
    -> S.Stream (Of (DbEntry db)) IO ()
ascendIntersect db s = go
  where
    go e
        | key e `HS.member` s = S.yield e
        | otherwise = S.for (children db (key e) & lookupStreamM db) go

-- -------------------------------------------------------------------------- --
-- Limiting and Seeking a stream

-- | Quick and dirty paging implementation
--
seekLimitStream
    :: Monad m
    => Eq k
    => (a -> k)
    -> Maybe (NextItem k)
    -> Maybe Limit
    -> S.Stream (Of a) m ()
    -> S.Stream (Of a) m (Natural, Eos)
seekLimitStream k n limit = limitStream limit . seekStream k n

limitStream
    :: Monad m
    => Maybe Limit
    -> S.Stream (Of a) m ()
    -> S.Stream (Of a) m (Natural, Eos)
limitStream Nothing s = do
    limit' <- s & S.copy & S.length_
    return (int limit', Eos True)
limitStream (Just limit) s = do
    (limit' :> tailStream) <- s
        & S.splitAt (int limit)
        & S.copy
        & S.length
    -- FIXME this can be expensive for infinite/blocking streams. Skip this if
    -- the underlying stream is known to be infinite.
    --
    eos <- lift (atEos tailStream)
    return (int limit', eos)

seekStream
    :: Monad m
    => Eq k
    => (a -> k)
    -> Maybe (NextItem k)
    -> S.Stream (Of a) m r
    -> S.Stream (Of a) m r
seekStream _ Nothing = id
seekStream k (Just (Exclusive n)) = S.drop 1 . S.dropWhile (\a -> k a /= n)
seekStream k (Just (Inclusive n)) = S.dropWhile (\a -> k a /= n)

seekStreamSet
    :: Monad m
    => Eq k
    => Hashable k
    => (a -> k)
    -> Maybe (NextItem (HS.HashSet k))
    -> S.Stream (Of a) m r
    -> S.Stream (Of a) m r
seekStreamSet _ Nothing = id
seekStreamSet k (Just (Inclusive s)) = S.dropWhile (\a -> not (k a `HS.member` s))
seekStreamSet k (Just (Exclusive s))
    = S.dropWhile (\a -> k a `HS.member` s)
    . S.dropWhile (\a -> not (k a `HS.member` s))

atEos :: Monad m => S.Stream (Of a) m () -> m Eos
atEos = fmap (Eos . isNothing) . S.head_

prop_seekLimitStream_limit :: [Int] -> Natural -> Property
prop_seekLimitStream_limit l i = i <= len l ==> actual === expected
#if MIN_VERSION_QuickCheck(2,12,0)
    & cover 1 (i == len l) "limit == length of stream"
    & cover 1 (i == 0) "limit == 0"
    & cover 1 (length l == 0) "length of stream == 0"
#endif
  where
    actual = runIdentity . S.toList $ seekLimitStream id Nothing (Just (Limit i)) (S.each l)
    expected = take (int i) l :> (i, Eos (i >= len l))

prop_seekLimitStream_id :: [Int] -> Property
prop_seekLimitStream_id l = actual === expected
#if MIN_VERSION_QuickCheck(2,12,0)
    & cover 1 (length l == 0) "len l == 0"
#endif
  where
    actual = runIdentity $ S.toList $ seekLimitStream id Nothing Nothing (S.each l)
    expected = l :> (len l, Eos True)

-- -------------------------------------------------------------------------- --
-- The following functions are based on 'lookup' and are meant for the
-- implementation of base layers in contexts with fast fast local db access.

lookupM
    :: forall db
    . TreeDb db
    => db
    -> DbKey db
    -> IO (DbEntry db)
lookupM db k = lookup db k >>= \case
    Nothing -> throwM $ TreeDbKeyNotFound @db k
    Just x -> return x

-- | Lookup all entries in a stream of database keys and return the stream
-- of entries.
--
lookupStreamM
    :: TreeDb db
    => db
    -> S.Stream (Of (DbKey db)) IO r
    -> S.Stream (Of (DbEntry db)) IO r
lookupStreamM db = S.mapM $ \k -> lookupM db k

data GenesisParent
    = GenesisParentThrow
    | GenesisParentSelf
    | GenesisParentNone
    deriving (Show, Eq, Ord, Generic)

-- | Internal function this throws an 'TreeDbParentMissing' exception if the
-- parent block header is not in the db. It throw an
-- 'InteralCodeInvariantViolation' exception if the block header parent is
-- 'Nothing'.
--
lookupParentM
    :: forall db
    . TreeDb db
    => GenesisParent
    -> db
    -> DbEntry db
    -> IO (DbEntry db)
lookupParentM g db e = case parent e of
    Nothing -> case g of
        GenesisParentSelf -> return e
        _ -> throwM
            $ InternalInvariantViolation "Called getParentEntry on genesis block"
    Just p -> lookup db p >>= \case
        Nothing -> throwM $ TreeDbParentMissing @db e
        Just x -> return x

-- | Replace all entries in the stream by their parent entries.
-- If the genesis block is part of the stream it is replaced by itself.
--
lookupParentStreamM
    :: forall db r
    . TreeDb db
    => GenesisParent
    -> db
    -> S.Stream (Of (DbEntry db)) IO r
    -> S.Stream (Of (DbEntry db)) IO r
lookupParentStreamM g db = S.mapMaybeM $ \e -> case parent e of
    Nothing -> case g of
        GenesisParentSelf -> return $ Just e
        GenesisParentNone -> return Nothing
        GenesisParentThrow -> throwM
            $ InternalInvariantViolation "Called getParentEntry on genesis block"
    Just p -> lookup db p >>= \case
        Nothing -> throwM $ TreeDbParentMissing @db e
        Just x -> return $ Just x

-- | Create a stream from a foldable value
--
foldableEntries
    :: forall e f
    . Foldable f
    => TreeDbEntry e
    => Maybe (NextItem (Key e))
    -> Maybe Limit
    -> Maybe MinRank
    -> Maybe MaxRank
    -> f e
    -> S.Stream (Of e) IO (Natural, Eos)
foldableEntries k l mir mar f = S.each f
    & applyRank mir mar
    & void
    & seekLimitStream key k l

-- -------------------------------------------------------------------------- --
-- Properties

properties :: [(String, Property)]
properties =
    [ ("seekLimitStream_limit", property prop_seekLimitStream_limit)
    , ("seekLimitStream_id", property prop_seekLimitStream_id)
    ]
