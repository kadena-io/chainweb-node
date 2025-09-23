{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
, minRank
, toTree
, GenesisParent(..)

-- ** Limiting and Seeking a Stream
, Eos(..)
, isEos
, limitStream
, seekStream
, seekStreamSet

-- ** Query branches
, getBranch
, ancestors
, defaultBranchEntries
, chainBranchEntries

-- ** Lookups
, lookupM
, lookupRankedM
, lookupStreamM
, lookupParentM

-- * Misc Utils
, forkEntry
, branchDiff
, branchDiff_
, collectForkBlocks
, seekAncestor
, seekLimitStream
, getBranchIncreasing

-- * Membership Queries
, onLongestBranch
, ancestorOf
, ancestorOfEntry
) where

import Control.Arrow ((***))
import Control.Lens hiding ((:>), (.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans

import Data.Aeson hiding (Key)
import Data.Foldable
import Data.Function
import Data.Functor.Of
import Data.Graph
import Data.Hashable
import qualified Data.HashSet as HS
import Data.Kind
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup
import qualified Data.Text as T
import Data.These
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Generics

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal modules

import Chainweb.Utils hiding ((==>))
import Chainweb.Utils.Paging

-- -------------------------------------------------------------------------- --
-- Exceptions

data TreeDbException db
    = TreeDbParentMissing (DbEntry db)
    | TreeDbKeyNotFound (DbKey db)
    | TreeDbInvalidRank (DbEntry db)
    | TreeDbAncestorMissing (DbEntry db) Natural T.Text

instance (Show (DbEntry db), Show (DbKey db)) => Show (TreeDbException db) where
    show (TreeDbParentMissing e) = "TreeDbParentMissing: " ++ show e
    show (TreeDbKeyNotFound e) = "TreeDbKeyNotFound: " ++ show e
    show (TreeDbInvalidRank e) = "TreeDbInvalidRank: " ++ show e
    show (TreeDbAncestorMissing e r msg) = "TreeDbAncestorMissing: entry "
        ++ show e
        ++ ". Rank " ++ sshow r
        ++ ". " ++ T.unpack msg

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
        <$> (HS.map LowerBound <$> (o .:? "lower" .!= mempty))
        <*> (HS.map UpperBound <$> (o .:? "upper" .!= mempty))

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

    -- {-# MINIMAL lookup, entries, insert, maxEntry #-}
    {-# MINIMAL lookup, entries, maxEntry #-}

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

    -- | Lookup a single entry by its key and rank. For some instances of
    -- the lookup can be implemented more efficiently when the rank is know.
    -- Otherwise the default implementation just ignores the rank parameter
    -- falls back to 'lookup'.
    --
    lookupRanked
        :: db
        -> Natural
        -> DbKey db
        -> IO (Maybe (DbEntry db))
    lookupRanked db _ = lookup db
    {-# INLINEABLE lookupRanked #-}

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
            -- ^ Lower limits
        -> HS.HashSet (UpperBound (DbKey db))
            -- ^ Upper limits
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
    -- Also see 'getBranch' for a less powerful but easier to use variant of
    -- this function.
    --
    branchEntries
        :: db
        -> Maybe (NextItem (DbKey db))
            -- ^ Cursor
        -> Maybe Limit
            -- ^ Maximum number of items that are returned
        -> Maybe MinRank
            -- ^ Minimum rank for returned items
        -> Maybe MaxRank
            -- ^ Maximum rank for returned items
        -> HS.HashSet (LowerBound (DbKey db))
            -- ^ Lower bounds for the returned items
        -> HS.HashSet (UpperBound (DbKey db))
            -- ^ Upper bounds for the returned items
        -> (S.Stream (Of (DbEntry db)) IO (Natural, Eos) -> IO a)
            -- ^ continuation that is provided the stream of result items
        -> IO a
    branchEntries = defaultBranchEntries
    {-# INLINEABLE branchEntries #-}

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

root :: TreeDb db => db -> IO (DbEntry db)
root db = fromJuste <$> entries db Nothing (Just 1) Nothing Nothing S.head_
{-# INLINE root #-}

-- | The rank of the root. Often, but not always, this is 0.
--
-- For chainweb chains that got added during graph changes/extensions, this is
-- strictly larger than 0.
--
minRank :: TreeDb db => db -> IO Natural
minRank = fmap rank . root
{-# INLINE minRank #-}

-- | Filter the stream of entries for entries in a range of ranks.
--
-- This assumes that the entries of the stream are in descending order by rank.
--
applyRankDesc
    :: forall e m
    . TreeDbEntry e
    => Monad m
    => Maybe MinRank
        -- ^ Return just the entries that have the given minimum rank.
    -> Maybe MaxRank
        -- ^ Return just the entries that have the given maximum rank.
    -> S.Stream (Of e) m ()
    -> S.Stream (Of e) m ()
applyRankDesc l u
    = maybe id (\x -> S.dropWhile (\e -> rank e > x)) (_getMaxRank <$> u)
    . maybe id (\x -> S.takeWhile (\e -> rank e >= x)) (_getMinRank <$> l)

-- | Returns the stream of all ancestors of a key, including the entry of the
-- given key.
--
ancestors
    :: forall db
    . TreeDb db
    => db
    -> DbKey db
    -> S.Stream (Of (DbEntry db)) IO ()
ancestors db k = getBranch db mempty (HS.singleton $ UpperBound k)
{-# INLINE ancestors #-}

-- | The default implementation for getBranch. This implementation always starts
-- traversing from the given upper bounds and prunes the result to the possibly
-- provided limits.
--
defaultBranchEntries
    :: TreeDb db
    => db
    -> Maybe (NextItem (DbKey db))
        -- ^ Cursor
    -> Maybe Limit
        -- ^ Maximum number of items that are returned
    -> Maybe MinRank
        -- ^ Minimum rank for returned items
    -> Maybe MaxRank
        -- ^ Maximum rank for returned items
    -> HS.HashSet (LowerBound (DbKey db))
        -- ^ Lower bounds for the returned items
    -> HS.HashSet (UpperBound (DbKey db))
        -- ^ Upper bounds for the returned items
    -> (S.Stream (Of (DbEntry db)) IO (Natural, Eos) -> IO a)
        -- ^ continuation that is provided the stream of result items
    -> IO a
defaultBranchEntries db k l mir mar lower upper f = f $
    getBranch db lower upper
        & applyRankDesc mir mar
        & seekLimitStream key k l
{-# INLINEABLE defaultBranchEntries #-}

-- | An implementation of 'branchEntries' that is optimized for trees that have
-- a single very long trunk and only very short branches. This is usually the
-- case for block chains. If maximum height is provided, the for each upper
-- bound an ancestor at the given height is seeked and search starts from there.
--
-- Seeking of the ancestor is implemented uses 'entries' and the performance
-- depends on an efficient implementation of it.
--
chainBranchEntries
    :: TreeDb db
    => db
    -> Maybe (NextItem (DbKey db))
        -- ^ Cursor
    -> Maybe Limit
        -- ^ Maximum number of items that are returned
    -> Maybe MinRank
        -- ^ Minimum rank for returned items
    -> Maybe MaxRank
        -- ^ Maximum rank for returned items
    -> HS.HashSet (LowerBound (DbKey db))
        -- ^ Lower bounds for the returned items
    -> HS.HashSet (UpperBound (DbKey db))
        -- ^ Upper bounds for the returned items
    -> (S.Stream (Of (DbEntry db)) IO (Natural, Eos) -> IO a)
        -- ^ continuation that is provided the stream of result items
    -> IO a
chainBranchEntries db k l mir Nothing lower upper f
    = defaultBranchEntries db k l mir Nothing lower upper f
chainBranchEntries db k l mir mar@(Just (MaxRank (Max m))) lower upper f = do
    upper' <- foldMap start upper
    defaultBranchEntries db k l mir mar lower upper' f
  where
    start (UpperBound u) = lookup db u >>= \case
        Nothing -> return mempty
        Just e -> seekAncestor db e m >>= \case
            Nothing -> return mempty
            Just x -> return $ HS.singleton (UpperBound $! key x)
{-# INLINEABLE chainBranchEntries #-}

-- | @getBranch db lower upper@ returns all nodes that are predecessors of nodes
-- in @upper@ and not predecessors of any node in @lower@. Entries are returned
-- in descending order.
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
    !eos <- lift (atEos tailStream)
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
    (Just !x) -> return x
{-# INLINEABLE lookupM #-}

lookupRankedM
    :: forall db
    . TreeDb db
    => db
    -> Natural
    -> DbKey db
    -> IO (DbEntry db)
lookupRankedM db r k = lookupRanked db r k >>= \case
    Nothing -> throwM $ TreeDbKeyNotFound @db k
    (Just !x) -> return x
{-# INLINEABLE lookupRankedM #-}

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
        (Just !x) -> return x

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
            $ InternalInvariantViolation "Chainweb.TreeDB.lookupParentStreamM: Called getParentEntry on genesis block. Most likely this means that the genesis headers haven't been generated correctly. If you are using a development or testing chainweb version consider resetting the databases."
    Just p -> lookup db p >>= \case
        Nothing -> throwM $ TreeDbParentMissing @db e
        (Just !x) -> return (Just x)

-- | Interpret a given `BlockHeaderDb` as a native Haskell `Tree`. Should be
-- used only for debugging purposes.
--
toTree :: (TreeDb db, Ord (DbKey db)) => db -> IO (Tree (DbEntry db))
toTree db = do
    hs <- entries db Nothing Nothing Nothing Nothing $ \es ->
        S.toList_ $ S.map (\h -> (h, key h, [fromMaybe (key h) $ parent h] )) es
    let (g, vert, _) = graphFromEdges hs
        g' = transposeG g
    pure . fmap (view _1 . vert) . unsafeHead "Chainweb.TreeDB.toTree: empty DFS" . dfs g' $ topSort g'

-- -------------------------------------------------------------------------- --
-- Misc Utils

-- | Returns the fork entry of two branches of the 'TreeDb'.
--
forkEntry
    :: TreeDb db
    => db
    -> DbEntry db
    -> DbEntry db
    -> IO (DbEntry db)
forkEntry db l r
    | rank l + 20 < rank r = do
        r' <- fromJuste <$> seekAncestor db r (rank l)
        S.effects $ branchDiff_ db l r'
    | rank r + 20 < rank l = do
        l' <- fromJuste <$> seekAncestor db l (rank r)
        S.effects $ branchDiff_ db l' r
    | otherwise = S.effects $ branchDiff_ db l r

-- | Compares two branches of a 'TreeDb'. The fork entry is included as last
-- item in the stream.
--
-- If you only need one branch of the fork you may use 'getBranch' instead.
--
branchDiff
    :: TreeDb db
    => db
    -> DbEntry db
    -> DbEntry db
    -> S.Stream (Of (These (DbEntry db) (DbEntry db))) IO ()
branchDiff db l r = branchDiff_ db l r >>= \i -> S.yield (These i i)

-- | Compares two branches of a 'TreeDb'. The fork entry is returned as the
-- result of the stream computation.
--
-- If you only need one branch of the fork you may use 'getBranch' instead.
--
branchDiff_
    :: TreeDb db
    => db
    -> DbEntry db
    -> DbEntry db
    -> S.Stream (Of (These (DbEntry db) (DbEntry db))) IO (DbEntry db)
branchDiff_ db = go
  where
    go l r
        | key l == key r = return l
        | rank l > rank r = do
            S.yield (This l)
            lp <- step l
            go lp r
        | rank r > rank l = do
            S.yield (That r)
            rp <- step r
            go l rp
        | otherwise = do
            S.yield (These l r)
            lp <- step l
            rp <- step r
            go lp rp
    step = lift . lookupParentM GenesisParentThrow db

-- -------------------------------------------------------------------------- --
-- | Collects the blocks on the old and new branches of a fork. Returns
--   @(commonAncestor, oldBlocks, newBlocks)@.
--
collectForkBlocks
    :: forall db . TreeDb db
    => db
    -> DbEntry db
    -> DbEntry db
    -> IO (DbEntry db, Vector (DbEntry db), Vector (DbEntry db))
collectForkBlocks db lastHeader newHeader = do
    (oldL, newL) <- go (branchDiff db lastHeader newHeader) ([], [])
    when (null oldL) $ throwM $ TreeDbParentMissing @db lastHeader
    let !common = unsafeHead "Chainweb.TreeDB.collectForkBlocks.common" oldL
    let !old = V.fromList $ unsafeTail "Chainweb.TreeDB.collectForkBlocks.old" oldL
    let !new = V.fromList $ unsafeTail "Chainweb.TreeDB.collectForkBlocks.new" newL
    return (common, old, new)
  where
    go !stream (!oldBlocks, !newBlocks) = do
        nxt <- S.next stream
        case nxt of
            -- end of the stream, last item is common branch point of the forks
            -- removing the common branch block with tail -- the lists should never be empty
            Left _ -> return (oldBlocks, newBlocks)

            Right (This blk, strm) ->
                go strm (blk:oldBlocks, newBlocks)
            Right (That blk, strm) ->
                go strm (oldBlocks, blk:newBlocks)
            Right (These lBlk rBlk, strm) ->
                go strm (lBlk:oldBlocks, rBlk:newBlocks)
{-# INLINE collectForkBlocks #-}

-- -------------------------------------------------------------------------- --
-- Query an item with given rank on a branch

-- The following is implemented using branchEntries. Alternatively, we could
-- integrate this into the default implementation of branchEntries.

-- | This has expected constant complexity on tree databases that have very long
-- linear trunks and only short branches, which is the case, for instance, for
-- block chains. Worst case complexity is \(O(n)\), and at least twice as much
-- as calling `getBranch` directly, which is used as fallback.
--
-- The function returns 'Nothing' only if the requested rank is larger than the
-- rank of the given header or smaller than the rank of the root entry.
--
-- It is implemented in terms of 'defaultBranchEntries' and 'entries'.
--
seekAncestor
    :: forall db
    . TreeDb db
    => db
    -> DbEntry db
        -- Branch
    -> Natural
        -- Rank
    -> IO (Maybe (DbEntry db))
seekAncestor db h r
    | r > hh = return Nothing
    | r == hh = return $ Just h
    | otherwise = do
        mr <- minRank db
        if r < mr then return Nothing else fastRoute1
  where
    -- If there is only a single block at the requested block height, it must be
    -- a predecessor of any block of larger height. We first do an efficient
    -- pointwise lookup and return the result if it is unique.
    --
    -- fastRoute1 :: ChainId -> BlockHeader -> IO BlockHeader
    fastRoute1 = do
        a <- S.toList_ & entries db Nothing (Just 2) (Just $ int r) (Just $ int r)
        case a of
            [] -> throwM $ TreeDbAncestorMissing @db h (int r)
                "No entry at this rank in the database"
            [x] -> return $ Just x
            _ -> fastRoute2 1

    hh = rank h

    -- If it turns out that the fastRoute doesn't return a result, we start a
    -- backward traversal with all blocks that are a constant number blocks
    -- above the target block height. With high probability the history will
    -- have narrowed to a single block once we reach the target height.
    --
    fastRoute2 (i :: Int)
        | r + 2 * off < hh = do
            -- get all blocks at height l
            !as <- S.toList_ & entries db Nothing Nothing (Just $ int l) (Just $ int l)

            -- traverse backward to find blocks at height ch
            !bs <- S.toList_ & defaultBranchEntries db
                Nothing (Just 2)
                (Just $ int r) (Just $ int r)
                mempty (HS.fromList $ UpperBound . key <$> as)

            -- check that result is unique
            case bs of
                [] -> throwM $ TreeDbAncestorMissing @db h (int r)
                    $ "Backward search from rank " <> sshow l <> " results"
                [x] -> return $ Just x
                _ -> fastRoute2 (succ i)
        | otherwise = slowRoute
      where
        l = r + off
        off = 2^i

    -- We find the predecessor of requested height of the block in the given cut
    -- by traversing along the parent relation.
    --
    slowRoute = do
        !a <- S.head_ & defaultBranchEntries db
            Nothing (Just 1)
            (Just $ int r) (Just $ int r)
            mempty (HS.singleton $ UpperBound $ key h)
        case a of
            Nothing -> throwM $ TreeDbAncestorMissing @db h (int r)
                "branch traversal yields no result"
            x -> return x

-- | @getBranchIncreasing db e r@ returns a stream of acestors of e sorted by
-- rank in ascending order starting at rank @r@.
--
-- An empty stream is returned if @r@ is larger than @rank e@. Otherwise at least
-- the entry @e@ is in the stream.
--
-- /NOTE:/
--
-- This space complexity of this \(O(1 + f * n)\), where \(f\) is the depth of
-- the largest fork in the database and \(n\) is the number of concurrent forks
-- in the database. The depth of the largest fork is the number of blocks on the
-- shorter branch from the fork point onward.
--
-- The worst case complexity is thus worse than for 'reverse . branchDiff',
-- which is linear in the longest branch. However for a "healthy" block chain
-- database the space complexity is \(O(1)\). Even on a block chain with long
-- forks the worst case complexities are of the same order as long as the number
-- of concurrent forks isn't dominating the the length of the branches in the
-- database.
--
-- A non-healthy block chain database can be fixed by prunning forks before
-- using this function.
--
getBranchIncreasing
    :: forall db a
    . TreeDb db
    => db
    -> DbEntry db
        -- ^ branch
    -> Natural
        -- ^ rank
    -> (S.Stream (Of (DbEntry db)) IO () -> IO a)
    -> IO a
getBranchIncreasing db e r inner
    | r > rank e = inner $ return ()
    | r == rank e = inner $ S.yield e
    | otherwise = go
  where
    go = entries db Nothing Nothing (Just $ int r) (Just $ int (rank e - 1)) $ \s ->
        (s >> S.yield e)
            & S.groupBy ((==) `on` rank)
            & S.mapped S.toList
            & S.scan (goRank . fst) ([], []) snd
            & flip S.for S.each
            & inner

    -- Fold over all ranks of the db: track all branches that exist at
    -- each rank and return entries on the main branch that are yielded
    -- to the result stream.
    --
    -- When an active branch becomes longer than all other active branches
    -- it means that it contains values at ranks for which no other active branch
    -- exists -if there were branches at those levels, those have died off- and the
    -- unique entries at those level are yielded to the result stream.
    goRank
        :: [Branch (DbEntry db)]
            -- ^ currently active branches
        -> [DbEntry db]
            -- ^ entries at current rank
        -> ([Branch (DbEntry db)], [DbEntry db])
            -- ^ new active branches and entries that are yielded to the result stream
    goRank actives new = case extendActives actives new of
        [Branch _ a] -> ([], L.reverse $ toList a)
        Branch l0 a0 : bs@(Branch l1 _ : _)
            -- active branches are sorted by length in decreasing order. If the first branch
            -- is longer than the second branch it is also longer than all other branches.
            -- That means that its entries at the lowest ranks are unique for their rank and
            -- can be yielded to the result stream.
            --
            -- Invariant: `length l1 >= 1` and, thus, length keep >= 1`
            | l0 > l1, (keep, yield) <- NE.splitAt l1 a0 ->
                (Branch l1 (NE.fromList keep) : bs, L.reverse $ toList yield)
        as -> (as, [])

-- | Data type for keeping track of active branches
--
-- For long forks, it may be more efficient to use a stream or a dlist instead
-- of a list that must be reversed. However, it is is not clear (1.) what the
-- overhead is, given that most forks are very short and (2.) how efficient
-- 'splitAt' is for dlist or stream.
--
data Branch a = Branch {-# UNPACK #-} !Int {-# UNPACK #-} !(NE.NonEmpty a)

newBranch :: a -> Branch a
newBranch = Branch 1 . pure

extendBranch :: Branch a -> a -> Branch a
extendBranch (Branch l as) n = Branch (l + 1) (NE.cons n as)

matchBranch :: TreeDbEntry e => Branch e -> e -> Bool
matchBranch (Branch _ (h NE.:| _)) n = parent n == Just (key h)

-- | Extend active branches and sort by length in descending order
--
-- complexity is quadratic in the number active branches, which is acceptable
-- for an healthy chain database. The expected number of branches is small and
-- doesn't justify the use of a more complex set data structure. In most cases
-- @actives@ and @new@ are singleton lists.
--
-- Note that active branches can't be empty
--
extendActives :: TreeDbEntry a => [Branch a] -> [a] -> [Branch a]
extendActives [] new = newBranch <$> new
extendActives actives new = L.sortOn (\(Branch l _) -> negate l)
    [ extendBranch as n | n <- new, as <- actives, matchBranch as n ]

-- -------------------------------------------------------------------------- --
-- Membership Queries

-- | @ancestorOfEntry db h ctx@ returns @True@ if @h@ is an ancestor of @ctx@
-- in @db@.
--
-- If either of @h@ or @ctx@ doesn't exist in @db@, @False@ is returned.
--
ancestorOfEntry
    :: forall db
    . TreeDb db
    => db
    -> DbKey db
        -- ^ the block hash to look up (the member)
    -> DbEntry db
        -- ^ the context, i.e. the branch of the chain that contains the member
    -> IO Bool
ancestorOfEntry db h ctx = lookup db h >>= \case
    Nothing -> return False
    Just lh -> seekAncestor db ctx (rank lh) >>= \case
        Nothing -> return False
        Just x -> return $ key x == h

-- | @ancestorOfEntry db h ctx@ returns @True@ if @h@ is an ancestor of @ctx@
-- in @db@.
--
-- An exception is raised if @th@ doesn't exist in @db@.
--
ancestorOf
    :: forall db
    . TreeDb db
    => db
    -> DbKey db
        -- ^ the block hash to look up (the member)
    -> DbKey db
        -- ^ the context, i.e. the branch of the chain that contains the member
    -> IO Bool
ancestorOf db h ctx = lookup db ctx >>= \case
    Nothing -> return False
    Just lh -> ancestorOfEntry db h lh

onLongestBranch
    :: forall db
    . TreeDb db
    => db
    -> DbKey db
    -> IO Bool
onLongestBranch db h = do
    th <- maxEntry db
    ancestorOfEntry db h th
