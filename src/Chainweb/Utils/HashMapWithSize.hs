{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Utils.HashMapWithSize
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Provides the size in \(O(1)\). For all other operations the performance is
-- assymptotically the same as for 'HashMap'. Some operations, however, have
-- some overhead.
--
module Chainweb.Utils.HashMapWithSize
( HashMapWithSize
, size
, empty
, singleton
, null
, member
, lookup
, insert
, delete
, filter
, filterWithKey
, map
, mapWithKey
, foldr
, foldr'
, foldrWithKey
, foldrWithKey'
, foldl
, foldl'
, foldlWithKey
, foldlWithKey'
, foldMapWithKey
, traverseWithKey
, keys
, elems
, toList
) where

import Control.DeepSeq

import Data.Bifoldable
import qualified Data.Foldable as F
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Traversable

import GHC.Generics

import Prelude hiding (foldr, foldl, lookup, null, map, filter)

data HashMapWithSize k v = HashMapWithSize
    { _hmwsSize :: {-# UNPACK #-} !Int
    , _hmwsMap :: !(HM.HashMap k v)
    }
    deriving (Read, Show, Eq, Ord, Generic, NFData)

instance (Hashable k, Hashable v) => Hashable (HashMapWithSize k v) where
    hash (HashMapWithSize _ m) = hash m
    hashWithSalt s (HashMapWithSize _ m) = hashWithSalt s m
    {-# INLINE hash #-}
    {-# INLINE hashWithSalt #-}

instance Functor (HashMapWithSize k) where
    fmap f (HashMapWithSize s m) = HashMapWithSize s (fmap f m)
    {-# INLINE fmap #-}

instance F.Foldable (HashMapWithSize k) where
    foldMap f = foldMapWithKey (\ _k v -> f v)
    {-# INLINE foldMap #-}
    foldr = foldr
    {-# INLINE foldr #-}
    foldl = foldl
    {-# INLINE foldl #-}
    foldr' = foldr'
    {-# INLINE foldr' #-}
    foldl' = foldl'
    {-# INLINE foldl' #-}
    null = null
    {-# INLINE null #-}
    length = size
    {-# INLINE length #-}

instance Bifoldable HashMapWithSize where
    bifoldMap f g = foldMapWithKey (\k v -> f k `mappend` g v)
    {-# INLINE bifoldMap #-}
    bifoldr f g = foldrWithKey (\k v acc -> k `f` (v `g` acc))
    {-# INLINE bifoldr #-}
    bifoldl f g = foldlWithKey (\acc k v -> (acc `f` k) `g` v)
    {-# INLINE bifoldl #-}

instance Traversable (HashMapWithSize k) where
    traverse f = traverseWithKey (const f)
    {-# INLINABLE traverse #-}

empty :: HashMapWithSize k v
empty = HashMapWithSize 0 HM.empty
{-# INLINE empty #-}

singleton :: Hashable k => k -> v -> HashMapWithSize k v
singleton k v = HashMapWithSize 1 (HM.singleton k v)
{-# INLINE singleton #-}

size :: HashMapWithSize k v -> Int
size (HashMapWithSize s _) = s
{-# INLINE size #-}

null :: HashMapWithSize k v -> Bool
null (HashMapWithSize 0 _) = True
null _ = False
{-# INLINE null #-}

member :: (Eq k, Hashable k) => k -> HashMapWithSize k a -> Bool
member k (HashMapWithSize _ m) = HM.member k m
{-# INLINE member #-}

lookup :: (Eq k, Hashable k) => k -> HashMapWithSize k v -> Maybe v
lookup k (HashMapWithSize _ m) = HM.lookup k m
{-# INLINE lookup #-}

insert
    :: (Eq k, Hashable k)
    => k
    -> v
    -> HashMapWithSize k v
    -> HashMapWithSize k v
insert k v (HashMapWithSize s m) = HashMapWithSize
    (s + if HM.member k m then 0 else 1)
    (HM.insert k v m)
{-# INLINE insert #-}

delete :: (Eq k, Hashable k) => k -> HashMapWithSize k v -> HashMapWithSize k v
delete k (HashMapWithSize s m) = HashMapWithSize
    (s - if HM.member k m then 0 else 1)
    (HM.delete k m)
{-# INLINE delete #-}

toList :: HashMapWithSize k v -> [(k, v)]
toList (HashMapWithSize _ m) = HM.toList m
{-# INLINE toList #-}

keys :: HashMapWithSize k v -> [k]
keys (HashMapWithSize _ m) = HM.keys m
{-# INLINE keys #-}

elems :: HashMapWithSize k v -> [v]
elems (HashMapWithSize _ m) = HM.elems m
{-# INLINE elems #-}

-- | \(O(n)\), note, however, that the result is traversed to obtain the size.
--
filterWithKey
    :: forall k v
    . (k -> v -> Bool)
    -> HashMapWithSize k v
    -> HashMapWithSize k v
filterWithKey p (HashMapWithSize _ m) = HashMapWithSize (HM.size m') m'
  where
    m' = HM.filterWithKey p m
{-# INLINE filterWithKey #-}

-- | \(O(n)\), note, however, that the result is traversed to obtain the size.
--
filter :: (v -> Bool) -> HashMapWithSize k v -> HashMapWithSize k v
filter p (HashMapWithSize _ m) = HashMapWithSize (HM.size m') m'
  where
    m' = HM.filter p m
{-# INLINE filter #-}

mapWithKey :: (k -> v1 -> v2) -> HashMapWithSize k v1 -> HashMapWithSize k v2
mapWithKey f (HashMapWithSize s m) = HashMapWithSize s (HM.mapWithKey f m)
{-# INLINE mapWithKey #-}

map :: (v1 -> v2) -> HashMapWithSize k v1 -> HashMapWithSize k v2
map f (HashMapWithSize s m) = HashMapWithSize s (HM.map f m)
{-# INLINE map #-}

foldr :: (v -> a -> a) -> a -> HashMapWithSize k v -> a
foldr f a (HashMapWithSize _ m) = HM.foldr f a m
{-# INLINE foldr #-}

foldl :: (a -> v -> a) -> a -> HashMapWithSize k v -> a
foldl f a (HashMapWithSize _ m) = HM.foldl f a m
{-# INLINE foldl #-}

foldr' :: (v -> a -> a) -> a -> HashMapWithSize k v -> a
foldr' f a (HashMapWithSize _ m) = HM.foldr' f a m
{-# INLINE foldr' #-}

foldl' :: (a -> v -> a) -> a -> HashMapWithSize k v -> a
foldl' f a (HashMapWithSize _ m) = HM.foldl' f a m
{-# INLINE foldl' #-}

foldrWithKey :: (k -> v -> a -> a) -> a -> HashMapWithSize k v -> a
foldrWithKey f a (HashMapWithSize _ m) = HM.foldrWithKey f a m
{-# INLINE foldrWithKey #-}

foldlWithKey :: (a -> k -> v -> a) -> a -> HashMapWithSize k v -> a
foldlWithKey f a (HashMapWithSize _ m) = HM.foldlWithKey f a m
{-# INLINE foldlWithKey #-}

foldrWithKey' :: (k -> v -> a -> a) -> a -> HashMapWithSize k v -> a
foldrWithKey' f a (HashMapWithSize _ m) = HM.foldrWithKey' f a m
{-# INLINE foldrWithKey' #-}

foldlWithKey' :: (a -> k -> v -> a) -> a -> HashMapWithSize k v -> a
foldlWithKey' f a (HashMapWithSize _ m) = HM.foldlWithKey' f a m
{-# INLINE foldlWithKey' #-}

foldMapWithKey :: Monoid m => (k -> v -> m) -> HashMapWithSize k v -> m
foldMapWithKey f (HashMapWithSize _ m) = HM.foldMapWithKey f m
{-# INLINE foldMapWithKey #-}

traverseWithKey
  :: Applicative f
  => (k -> v1 -> f v2)
  -> HashMapWithSize k v1 -> f (HashMapWithSize k v2)
traverseWithKey f (HashMapWithSize s m)
    = fmap (HashMapWithSize s) (HM.traverseWithKey f m)
{-# INLINE traverseWithKey #-}
