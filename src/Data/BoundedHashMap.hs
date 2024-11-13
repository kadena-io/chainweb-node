{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Data.BoundedHashMap
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Delete is currently not supported. Please let us know if you need it.
--
module Data.BoundedHashMap
( BoundedHashMap
, new
, null
, member
, size
, lookup
, lookupTouch
, insert
, insertWith
, insertWithF
, adjust
, fromHashMap
, toHashMap
) where

import Control.Applicative

import Data.Functor.Compose
import Data.Hashable
import qualified Data.HashMap.Strict as HM

import Numeric.Natural

import Prelude hiding (lookup, null)

-- -------------------------------------------------------------------------- --
-- Bounded HashMap


-- | A HashMap that retains the most recent n items and at any given time
-- contains at most \(2n\) items.
--
data BoundedHashMap k v = BoundedHashMap
    { _boundedHashMap0 :: !(HM.HashMap k v)
    , _boundedHashMap1 :: !(HM.HashMap k v)
    , _boundedHashMapLimit :: !Int
    , _boundedHashMapCount :: !Int
    }
    deriving (Show)

instance Functor (BoundedHashMap k) where
    fmap f m = m
        { _boundedHashMap0 = fmap f (_boundedHashMap0 m)
        , _boundedHashMap1 = fmap f (_boundedHashMap1 m)
        }
    {-# INLINE fmap #-}

new :: Eq k => Hashable k => Natural -> BoundedHashMap k v
new n = BoundedHashMap
    { _boundedHashMap0 = mempty
    , _boundedHashMap1 = mempty
    , _boundedHashMapLimit = fromIntegral n
    , _boundedHashMapCount = 0
    }
{-# INLINE new #-}

-- | The number of entries in the 'BoundedHashMap'.
--
-- This is a somewhat expensive operation and be avoided in production code.
--
-- \(O(n)\)
--
size :: Eq k => Hashable k => BoundedHashMap k v -> Natural
size = fromIntegral . HM.size . toHashMap
{-# INLINE size #-}

-- | Check whether the HashMap is empty.
--
-- \(O(1)\)
--
null :: BoundedHashMap k v -> Bool
null m = HM.null (_boundedHashMap0 m) && HM.null (_boundedHashMap1 m)
{-# INLINE null #-}

-- | Check if a key exists without marking it as recent.
--
-- \(O(log(n))\)
--
member
    :: Eq k
    => Hashable k
    => k
    -> BoundedHashMap k v
    -> Bool
member k m =
    HM.member k (_boundedHashMap0 m) || HM.member k (_boundedHashMap1 m)
{-# INLINE member #-}

-- | Lookup a key without marking it as recent.
--
-- \(O(log(n))\)
--
lookup
    :: Eq k
    => Hashable k
    => k
    -> BoundedHashMap k v
    -> Maybe v
lookup k m = HM.lookup k (_boundedHashMap0 m)
    <|> HM.lookup k (_boundedHashMap1 m)
{-# INLINE lookup #-}

-- | Lookup a key and mark it as recent if it exists.
--
-- \(O(log(n))\)
--
lookupTouch
    :: Eq k
    => Hashable k
    => k
    -> BoundedHashMap k v
    -> Maybe (v, BoundedHashMap k v)
lookupTouch k m
    | _boundedHashMapLimit m == 0 = Nothing
    | _boundedHashMapCount m < _boundedHashMapLimit m =
        case HM.alterF ff k (_boundedHashMap0 m) of
            Compose (Nothing, Nothing) -> Nothing
            Compose (Just v, Nothing) -> Just (v, m)
            Compose (Just v, Just m') -> Just
                (v, m
                    { _boundedHashMap0 = m'
                    , _boundedHashMapCount = _boundedHashMapCount m + 1
                    }
                )
            Compose (Nothing, Just _) -> error "impossible"
    | otherwise = lookupTouch k(swap m)
  where
    ff (Just v) = Compose (Just v, Nothing)
    ff Nothing = case HM.lookup k (_boundedHashMap1 m) of
        Just v -> Compose (Just v, Just (Just v))
        Nothing -> Compose (Nothing, Nothing)

-- | Insert a key and mark it as recent. If the key already exists it is
-- updated.
--
-- \(O(log(n))\)
--
insert
    :: Eq k
    => Hashable k
    => k
    -> v
    -> BoundedHashMap k v
    -> BoundedHashMap k v
insert k v m
    | _boundedHashMapLimit m == 0 = m
    | _boundedHashMapCount m < _boundedHashMapLimit m = m
        { _boundedHashMap0 = m'
        , _boundedHashMapCount = _boundedHashMapCount m + x
        }
    | otherwise = insert k v (swap m)
  where
    (x, m') = HM.alterF ff k (_boundedHashMap0 m)

    ff (Just _) = (0, Just v)
    ff Nothing = (1, Just v)

-- | Insert a value and mark it as recent. If the key already exists the new
-- value is computed via the provided merge function, which takes the new value
-- as first and the old value as second argument.
--
insertWith
    :: Eq k
    => Hashable k
    => (v -> v -> v)
    -> k
    -> v
    -> BoundedHashMap k v
    -> BoundedHashMap k v
insertWith f k v m
    | _boundedHashMapLimit m == 0 = m
    | _boundedHashMapCount m < _boundedHashMapLimit m = m
        { _boundedHashMap0 = m'
        , _boundedHashMapCount = _boundedHashMapCount m + x
        }
    | otherwise = insertWith f k v (swap m)
  where
    (x, m') = HM.alterF ff k (_boundedHashMap0 m)

    ff (Just o) = (0, Just (f v o))
    ff Nothing = case HM.lookup k (_boundedHashMap1 m) of
        (Just o) -> (1, Just (f v o))
        Nothing -> (1, Just v)
{-# INLINE insertWith #-}

insertWithF
    :: forall v k m
    . Eq k
    => Hashable k
    => Monad m
    => (v -> v -> m v)
    -> k
    -> v
    -> BoundedHashMap k v
    -> m (BoundedHashMap k v)
insertWithF f k v m
    | _boundedHashMapLimit m == 0 = return m
    | _boundedHashMapCount m < _boundedHashMapLimit m = do
        (x, m') <- getCompose $ HM.alterF ff k (_boundedHashMap0 m)
        return m
            { _boundedHashMap0 = m'
            , _boundedHashMapCount = _boundedHashMapCount m + x
            }
    | otherwise = insertWithF f k v (swap m)
  where

    ff :: Maybe v -> (Compose m ((,) Int)) (Maybe v)
    ff (Just o) = Compose $ (0,) . Just <$> f v o
    ff Nothing = case HM.lookup k (_boundedHashMap1 m) of
        (Just o) -> Compose $ (1,) . Just <$> f v o
        Nothing -> Compose $ return (1, Just v)
{-# INLINE insertWithF #-}

-- | Adjust a key and mark it as recent if it exists.
--
-- \(O(log(n))\)
--
adjust
    :: Eq k
    => Hashable k
    => (v -> v)
    -> k
    -> BoundedHashMap k v
    -> BoundedHashMap k v
adjust f k m
    | _boundedHashMapLimit m == 0 = m
    | _boundedHashMapCount m < _boundedHashMapLimit m = m
        { _boundedHashMap0 = m'
        , _boundedHashMapCount = _boundedHashMapCount m + x
        }
    | otherwise = adjust f k (swap m)
  where
    (x, m') = HM.alterF ff k (_boundedHashMap0 m)

    ff (Just o) = (0, Just (f o))
    ff Nothing = case HM.lookup k (_boundedHashMap1 m) of
        Nothing -> (1, Nothing)
        Just o -> (1, Just (f o))

-- | Convert the 'BoundedHashMap' into a 'HM.HashMap' with the same content.
--
-- \(O(n)\)
--
toHashMap :: Eq k => Hashable k => BoundedHashMap k v -> HM.HashMap k v
toHashMap m = _boundedHashMap0 m `HM.union` _boundedHashMap1 m
{-# INLINE toHashMap #-}

-- | Create a 'BoundedHashMap' from a 'HM.HashMap'. If the input HashMap has
-- more value than the lower limit of the BoundedHashMap, remaining values are
-- dropped.
--
-- \(O(n+m)\)
--
fromHashMap
    :: Eq k
    => Hashable k
    => Natural
    -> HM.HashMap k v
    -> BoundedHashMap k v
fromHashMap n m
    | s <= fromIntegral n = BoundedHashMap
        { _boundedHashMap0 = m
        , _boundedHashMap1 = mempty
        , _boundedHashMapLimit = s
        , _boundedHashMapCount = 0
        }
    | otherwise =
        fromHashMap n $ HM.fromList $ take (fromIntegral n) $ HM.toList m
  where
    s = HM.size m

-- -------------------------------------------------------------------------- --
-- Utils

swap :: (Eq k, Hashable k) => BoundedHashMap k v -> BoundedHashMap k v
swap x = x
    { _boundedHashMap0 = mempty
    , _boundedHashMap1 = _boundedHashMap0 x
    , _boundedHashMapCount = 0
    }
{-# INLINE swap #-}
