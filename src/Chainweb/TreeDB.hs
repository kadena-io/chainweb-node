{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
, _getMinRank
, MaxRank(..)
, _getMaxRank
, LowerBound(..)
, UpperBound(..)
, BranchBounds(..)

-- * Tree Database
, TreeDbEntry(..)
, DbKey
, TreeDb(..)

-- * Utils
, root
, toTree
, descend

-- ** Limiting and Seeking a Stream
, Eos(..)
, isEos
, limitStream
, seekStream
, seekStreamSet

-- ** Query branches
, getBranch

-- ** Lookups
, lookupM
, lookupStreamM

-- ** Stream a foldable value
, foldableEntries

-- * Misc Utils
, forkEntry
, branchDiff
, branchDiff_

-- * properties
, properties
) where

import Control.Arrow ((***))
import Control.Lens (view, (&), _1)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.Trans

import Data.Aeson
import Data.Function
import Data.Functor.Of
import Data.Graph
import Data.Hashable
import qualified Data.HashSet as HS
import Data.Kind
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Typeable

import GHC.Generics

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

import Test.QuickCheck

-- internal modules

import Chainweb.Utils hiding ((==>))
import Chainweb.Utils.Paging hiding (properties)

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

-- -------------------------------------------------------------------------- --
-- ** Branch Bounds

newtype LowerBound k = LowerBound { _getLowerBound :: k }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Hashable)

newtype UpperBound k = UpperBound { _getUpperBound :: k }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Hashable)

data BranchBounds db = BranchBounds
    { _branchBoundsLower :: !(HS.HashSet (LowerBound (DbKey db)))
    , _branchBoundsUpper :: !(HS.HashSet (UpperBound (DbKey db)))
    }
    deriving stock (Generic)

instance
    (Hashable (Key (DbEntry db)), Eq (Key (DbEntry db)), ToJSON (DbKey db))
    => ToJSON (BranchBounds db)
  where
    toJSON b = object
        [ "lower" .= HS.map _getLowerBound (_branchBoundsLower b)
        , "upper" .= HS.map _getUpperBound (_branchBoundsUpper b)
        ]

instance
    (Hashable (Key (DbEntry db)), Eq (Key (DbEntry db)), FromJSON (DbKey db))
    => FromJSON (BranchBounds db)
  where
    parseJSON = withObject "BranchBounds" $ \o -> BranchBounds
        <$> (HS.map LowerBound <$> o .: "lower")
        <*> (HS.map UpperBound <$> o .: "upper")

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

    {-# MINIMAL lookup, entries, (insert | insertStream), maxEntry #-}

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
    -- The stream is provide via continuation passing style. This supports
    -- implementations that allocate resources for the streams that must be
    -- released after the items in the stream are processed. The returned stream
    -- must only be used within the scope of the continuation.
    --
    -- If there is no further entry available it terminates and returns a number
    -- of returned items and a cursor. Implementations should block on IO at
    -- most a constant amount of time.
    --
    -- The default implementation is based on 'entries', which in some cases
    -- doesn't give good performance.
    --
    keys
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> (S.Stream (Of (DbKey db)) IO (Natural, Eos) -> IO a)
        -> IO a
    keys db k l mir mar f = entries db k l mir mar $ f . S.map key
    {-# INLINEABLE keys #-}

    -- | This stream returns a prefix of the entries of the nodes in the tree in
    -- ascending order starting from the given key or the genesis block.
    --
    -- The stream is provide via continuation passing style. This supports
    -- implementations that allocate resources for the streams that must be
    -- released after the items in the stream are processed. The returned stream
    -- must only be used within the scope of the continuation.
    --
    -- If there is no further entry available it terminates and returns a number
    -- of returned items and a cursor. Implementations should block on IO at
    -- most a constant amount of time.
    --
    entries
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> (S.Stream (Of (DbEntry db)) IO (Natural, Eos) -> IO a)
        -> IO a

    -- ---------------------------------------------------------------------- --
    -- * Branches

    -- | @branchKeys n l mir mar lower upper@ returns all nodes within the given
    -- range of minimum rank @mir@ and maximun rank @mar@ that are predecessors
    -- of nodes in @upper@ and not predecessors of any node in @lower@, starting
    -- at the entry @n@. The number of items in the result is limited by @l@.
    -- Items are returned in descending order.
    --
    -- The stream is provide via continuation passing style. This supports
    -- implementations that allocate resources for the streams that must be
    -- released after the items in the stream are processed. The returned stream
    -- must only be used within the scope of the continuation.
    --
    -- The result stream may return less than the requested number of items.
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
        -> (S.Stream (Of (DbKey db)) IO (Natural, Eos) -> IO a)
        -> IO a
    branchKeys db k l mir mar lower upper f =
        branchEntries db k l mir mar lower upper $ f . S.map key
    {-# INLINEABLE branchKeys #-}

    -- | @branchEntries n l mir mar lower upper@ returns all nodes within the
    -- given range of minimum rank @mir@ and maximun rank @mar@ that are
    -- predecessors of nodes in @upper@ and not predecessors of any node in
    -- @lower@, starting at the entry after @n@. The number of items in the
    -- result is limited by @l@. Items are returned in descending order.
    --
    -- The stream is provide via continuation passing style. This supports
    -- implementations that allocate resources for the streams that must be
    -- released after the items in the stream are processed. The returned stream
    -- must only be used within the scope of the continuation.
    --
    -- The result stream may return less than the requested number of items.
    --
    branchEntries
        :: db
        -> Maybe (NextItem (DbKey db))
        -> Maybe Limit
        -> Maybe MinRank
        -> Maybe MaxRank
        -> HS.HashSet (LowerBound (DbKey db))
        -> HS.HashSet (UpperBound (DbKey db))
        -> (S.Stream (Of (DbEntry db)) IO (Natural, Eos) -> IO a)
        -> IO a
    branchEntries db k l mir mar lower upper f = f $
        getBranch db lower upper
            & applyRank mir mar
            & seekLimitStream key k l
    {-# INLINEABLE branchEntries #-}

    -- ---------------------------------------------------------------------- --
    -- * Insertion

    -- FIXME: defining semantics in the presence of insertion failures is
    -- tricky. I think we should replace it either
    --
    -- @
    -- atomicInsertSet
    --     :: db
    --     -> HS.Set (DbEntry db)
    --     -> IO ()
    -- @
    --
    -- where the latter would insert all entries in a single atomic transaction.
    --
    insertStream
        :: db
        -> S.Stream (Of (DbEntry db)) IO a
        -> IO a
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

    -- | The largest entry in the database. This is the last entry in the
    -- 'entries' stream. It is also an entry of maximal rank.
    --
    maxEntry :: db -> IO (DbEntry db)

    -- | Maximum rank of all entries in the database.
    --
    -- prop> maxRank db == head (branches db Nothing (Just 1))
    --
    --
    maxRank :: db -> IO Natural
    maxRank = fmap rank . maxEntry
    {-# INLINEABLE maxRank #-}

-- -------------------------------------------------------------------------- --
-- Utils

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

root :: TreeDb db => db -> IO (DbEntry db)
root db = fmap fromJuste $ entries db Nothing (Just 1) Nothing Nothing S.head_
{-# INLINE root #-}

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
    getEntriesHs = lift . streamToHashSet_ . lookupStream db . S.each
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
-- of entries. Throws if an entry is missing.
--
lookupStreamM
    :: TreeDb db
    => db
    -> S.Stream (Of (DbKey db)) IO r
    -> S.Stream (Of (DbEntry db)) IO r
lookupStreamM db = S.mapM (lookupM db)

-- | Lookup all entries in a stream of database keys and return the stream
-- of entries. Ignores missing entries.
--
lookupStream
    :: TreeDb db
    => db
    -> S.Stream (Of (DbKey db)) IO r
    -> S.Stream (Of (DbEntry db)) IO r
lookupStream db = S.catMaybes . S.mapM (lookup db)

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
            $ InternalInvariantViolation "Chainweb.TreeDB.lookupParentM: Called getParentEntry on genesis block"
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
            $ InternalInvariantViolation "Chainweb.TreeDB.lookupParentStreamM: Called getParentEntry on genesis block"
    Just p -> lookup db p >>= \case
        Nothing -> throwM $ TreeDbParentMissing @db e
        Just x -> return $ Just x

-- | Create a `Stream` from a `Foldable` value.
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

-- | Interpret a given `BlockHeaderDb` as a native Haskell `Tree`. Should be
-- used only for debugging purposes.
--
toTree :: (TreeDb db, Ord (DbKey db)) => db -> IO (Tree (DbEntry db))
toTree db = do
    hs <- entries db Nothing Nothing Nothing Nothing $ \es ->
        S.toList_ $ S.map (\h -> (h, key h, [fromMaybe (key h) $ parent h] )) es
    let (g, vert, _) = graphFromEdges hs
        g' = transposeG g
    pure . fmap (view _1 . vert) . head . dfs g' $ topSort g'

-- -------------------------------------------------------------------------- --
-- Misc Utils

forkEntry
    :: TreeDb db
    => db
    -> DbEntry db
    -> DbEntry db
    -> IO (DbEntry db)
forkEntry db a b = S.effects $ branchDiff_ db a b

branchDiff
    :: TreeDb db
    => db
    -> DbEntry db
    -> DbEntry db
    -> S.Stream (Of (DiffItem (DbEntry db))) IO ()
branchDiff db l r = branchDiff_ db l r >>= \i -> S.yield (BothD i i)

branchDiff_
    :: TreeDb db
    => db
    -> DbEntry db
    -> DbEntry db
    -> S.Stream (Of (DiffItem (DbEntry db))) IO (DbEntry db)
branchDiff_ db = go
  where
    go l r
        | key l == key r = return l
        | rank l > rank r = do
            S.yield (LeftD l)
            lp <- step l
            go lp r
        | rank r > rank l = do
            S.yield (RightD r)
            rp <- step r
            go l rp
        | otherwise = do
            S.yield (BothD l r)
            lp <- step l
            rp <- step r
            go lp rp
    step = lift . lookupParentM GenesisParentThrow db

-- -------------------------------------------------------------------------- --
-- Properties

properties :: [(String, Property)]
properties =
    [ ("seekLimitStream_limit", property prop_seekLimitStream_limit)
    , ("seekLimitStream_id", property prop_seekLimitStream_id)
    ]
