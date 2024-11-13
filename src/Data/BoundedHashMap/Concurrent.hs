{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.BoundedHashMap.Concurrent
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.BoundedHashMap.Concurrent
( BoundedHashMap
, new
, null
, size
, member
, lookup
, lookupTouch
, insert
, insertWith
, adjust
, toHashMap
, fromHashMapIO

-- Tests
, prop_size
, prop_content
) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.Trans.Maybe

import Data.Foldable hiding (null)
import Data.Function
import Data.Functor.Compose
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as L

import GHC.Generics

import Numeric.Natural

import Prelude hiding (lookup, null)

import System.Mem.Weak

import Test.QuickCheck
import Test.QuickCheck.Monadic

-- -------------------------------------------------------------------------- --
-- Effects of mutating the BoundedHashMap

data Effect
    = Insert
    | Update
    | NoOp
    | NotApplicable
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

applyUpdate :: Monad m => Eq v => (v -> m v) -> v -> m (Effect, v)
applyUpdate f o = do
    n <- f o
    return $!
        if n == o then (NoOp, o) else (Update, n)

-- -------------------------------------------------------------------------- --
-- Thread Safe Bounded Hash Maps

-- | A HashMap that retains the most recent n items and at any given time
-- contains at most \(2n\) items.
--
data BoundedHashMap k v = BoundedHashMap
    { _boundedHashMapLimit :: {-# UNPACK #-} !Int
        -- ^ Limit. The hashmap will always contain this many recent
        -- items and at most twice as many items in total.
    , _boundedHashMapGcIntervalSeconds :: {-# UNPACK #-} !Int
        -- ^ Maximum time in seconds before a GC is forced.
    , _boundedHashMapCount :: {-# UNPACK #-} !(TVar Int)
        -- ^ The current count of recent items that are stored in
        -- the hashmap. The total number of items can be twice as
        -- big.
    , _boundedHashMap0 :: {-# UNPACK #-} !(TVar (HM.HashMap k v))
        -- ^ The hash table of recent itmes
    , _boundedHashMap1 :: {-# UNPACK #-} !(TVar (HM.HashMap k v))
        -- ^ The hash table of items that are marked as possibly
        -- not recent.
    -- , _boundedHashMapGcTrigger :: {-# UNPACK #-} !(TVar Bool)
    , _boundedHashMapGcTrigger :: (TVar Bool)
        -- ^ Used internally to trigger GC
    }

new :: Eq k => Hashable k => Natural -> Natural -> IO (BoundedHashMap k v)
new n secs = do
    m <- BoundedHashMap (fromIntegral n) (fromIntegral secs)
        <$> newTVarIO 0
        <*> newTVarIO mempty
        <*> newTVarIO mempty
        <*> newTVarIO False
    wref <- newIORef m >>= flip mkWeakIORef (return ())
    _ <- forkIO $ fix $ \loop -> do
        deRefWeak wref >>= \case
            Nothing -> do
                print "KILLED reaper thread"
                return ()
            Just ref -> do
                print "loop reaper thread"
                threadDelay (1_000_000 * _boundedHashMapGcIntervalSeconds m)
                x <- readIORef ref
                atomically $ writeTVar (_boundedHashMapGcTrigger x) True
                atomically (gc x)
                loop
    return m

{-# INLINE new #-}

-- -------------------------------------------------------------------------- --
-- Basic Operations

size :: Eq k => Hashable k => BoundedHashMap k v -> STM Natural
size m = fromIntegral . HM.size <$> toHashMap m
{-# INLINE size #-}

-- | Check wether the HashMap is empty.
--
-- \(O(1)\)
--
null :: BoundedHashMap k v -> STM Bool
null m = do
    x0 <- HM.null <$> readTVar (_boundedHashMap0 m)
    if x0
      then HM.null <$> readTVar (_boundedHashMap1 m)
      else return False
{-# INLINE null #-}

-- | Check if a key exists without marking it as recent.
--
-- \(O(log(n))\)
--
member
    :: Eq k
    => Hashable k
    => BoundedHashMap k v
    -> k
    -> STM Bool
member m k = do
    x0 <- HM.member k <$> readTVar (_boundedHashMap0 m)
    if x0
      then return True
      else HM.member k <$> readTVar (_boundedHashMap1 m)
{-# INLINE member #-}

-- -------------------------------------------------------------------------- --
-- Lookup

-- | Lookup a key without marking it as recent.
--
-- \(O(log(n))\)
--
lookup
    :: Eq k
    => Hashable k
    => BoundedHashMap k v
    -> k
    -> STM (Maybe v)
lookup m k = runMaybeT
    $ MaybeT (HM.lookup k <$> readTVar (_boundedHashMap0 m))
    <|> MaybeT (HM.lookup k <$> readTVar (_boundedHashMap1 m))
{-# INLINE lookup #-}

-- | Lookup a key and mark it as recent if it exists.
--
-- \(O(log(n))\)
--
lookupTouch
    :: forall k v
    . Eq k
    => Hashable k
    => BoundedHashMap k v
    -> k
    -> STM (Maybe v)
lookupTouch m k
    | _boundedHashMapLimit m == 0 = return Nothing
    | otherwise = do
        gc m
        m0 <- readTVar (_boundedHashMap0 m)
        (x, m0') <- getCompose $ HM.alterF ff k m0
        case x of
            Nothing -> return Nothing
            Just (True, v) -> do
                writeTVar (_boundedHashMap0 m) m0'
                modifyTVar' (_boundedHashMapCount m) (+ 1)
                return $ Just v
            Just (False, v) -> return $ Just v
  where
    ff :: Maybe v -> (Compose STM ((,) (Maybe (Bool, v)))) (Maybe v)
    ff (Just v) = Compose $ return (Just (False, v), Just v)
    ff Nothing = Compose $ do
        m1 <- readTVar (_boundedHashMap1 m)
        case HM.lookup k m1 of
            Just v -> return (Just (True, v), Just v)
            Nothing -> return (Nothing, Nothing)

-- -------------------------------------------------------------------------- --
-- Insert

-- | Insert a key and mark it as recent. If the key already exists it is
-- updated.
--
-- \(O(log(n))\)
--
insert
    :: Eq k
    => Hashable k
    => Eq v
    => BoundedHashMap k v
    -> k
    -> v
    -> STM Effect
insert m k v
    | _boundedHashMapLimit m == 0 = return NotApplicable
    | otherwise = do
        gc m
        m0 <- readTVar (_boundedHashMap0 m)
        case HM.alterF ff k m0 of
            (NoOp, _) -> return NoOp
            (NotApplicable, _) -> error "insertWith: insert can't have effect NotApplicable"
            (Update, m0') -> do
                writeTVar (_boundedHashMap0 m) m0'
                return Update
            (Insert, m0') -> do
                writeTVar (_boundedHashMap0 m) m0'
                modifyTVar' (_boundedHashMapCount m) (+ 1)
                return Insert
  where
    ff (Just e)
        | e == v = (NoOp, Just e)
        | otherwise = (Update, Just v)
    ff Nothing = (Insert, Just v)

-- | Insert a value and mark it as recent. If the key already exists the new
-- value is computed via the provided merge function, which takes the new value
-- as first and the old value as second argument.
--
-- \(O(log(n))\)
--
insertWith
    :: Eq k
    => Hashable k
    => Eq v
    => BoundedHashMap k v
    -> (v -> v -> STM v)
    -> k
    -> v
    -> STM Effect
insertWith m f k v
    | _boundedHashMapLimit m == 0 = return NotApplicable
    | otherwise = do
        gc m
        m0 <- readTVar (_boundedHashMap0 m)
        (eff, m0') <- getCompose $ HM.alterF ff k m0
        case eff of
            NoOp -> return NoOp
            NotApplicable -> error "insertWith: insert can't have effect NotApplicable"
            Update -> do
                writeTVar (_boundedHashMap0 m) m0'
                return Update
            Insert -> do
                writeTVar (_boundedHashMap0 m) m0'
                modifyTVar' (_boundedHashMapCount m) (+ 1)
                return Insert
  where
    ff (Just o) = Compose $ fmap Just <$> applyUpdate (f v) o
    ff Nothing = Compose $ do
        m1 <- readTVar (_boundedHashMap1 m)
        case HM.lookup k m1 of
            (Just o) -> fmap Just <$> applyUpdate (f v) o
            Nothing -> return (Insert, Just v)
{-# INLINE insertWith #-}

-- | Adjust a key and mark it as recent if it exists.
--
-- \(O(log(n))\)
--
adjust
    :: forall k v
    . Eq k
    => Hashable k
    => Eq v
    => BoundedHashMap k v
    -> (v -> STM v)
    -> k
    -> STM Effect
adjust m f k
    | _boundedHashMapLimit m == 0 = return NotApplicable
    | otherwise = do
        gc m
        m0 <- readTVar (_boundedHashMap0 m)
        (eff, m0') <- getCompose $ HM.alterF ff k m0
        case eff of
            NoOp -> return NoOp
            NotApplicable -> return NotApplicable
            Update -> do
                writeTVar (_boundedHashMap0 m) m0'
                return Update
            Insert -> error "adjust: internal error. adjust can't have effect Insert"
  where
    ff :: Maybe v -> Compose STM ((,) Effect) (Maybe v)
    ff (Just o) = Compose $ fmap Just <$> applyUpdate f o
    ff Nothing = Compose $ do
        m1 <- readTVar (_boundedHashMap1 m)
        case HM.lookup k m1 of
            Nothing -> return (NotApplicable, Nothing)
            Just o -> fmap Just <$> applyUpdate f o

-- -------------------------------------------------------------------------- --
-- Converting to and from HashMap

-- | Convert the 'BoundedHashMap' into a 'HM.HashMap' with the same content.
--
-- \(O(n)\)
--
toHashMap :: Eq k => Hashable k => BoundedHashMap k v -> STM (HM.HashMap k v)
toHashMap m = HM.union
    <$> readTVar (_boundedHashMap0 m)
    <*> readTVar (_boundedHashMap1 m)
{-# INLINE toHashMap #-}

-- | Create a 'BoundedHashMap' from a 'HM.HashMap'. If the input HashMap has
-- more value than the lower limit of the BoundedHashMap, remaining values are
-- dropped.
--
-- \(O(n+m)\)
--
fromHashMapIO
    :: Eq k
    => Hashable k
    => Natural
    -> Natural
    -> HM.HashMap k v
    -> IO (BoundedHashMap k v)
fromHashMapIO n secs m
    | s <= n_ = new n secs
    | otherwise =
        fromHashMapIO n secs $ HM.fromList $ take n_ $ HM.toList m
  where
    n_ = fromIntegral n
    s = HM.size m

-- -------------------------------------------------------------------------- --
-- GC

-- | All operations on the BoundedHashMap are guarded by this function, which
-- prevents live locks due to races. The reason is that no thread races against
-- this function. Instead all races are for completing this function. STM
-- guarantees that one thread will win, thus unblocking all other threads tyring
-- to complete 'gc'.
--
-- This check must be fast for the common case. The current implementation
-- does three 'readTVar'.
--
-- The slow case does four 'TVar' reads and four 'TVar' writes.
--
gc :: Eq k => Hashable k => BoundedHashMap k v -> STM ()
gc m = do
    c <- readTVar (_boundedHashMapCount m)
    t <- readTVar (_boundedHashMapGcTrigger m)
    when (c >= _boundedHashMapLimit m || t) $ do
        o <- swapTVar (_boundedHashMap0 m) mempty
        writeTVar (_boundedHashMap1 m) o
        writeTVar (_boundedHashMapCount m) 0
        writeTVar (_boundedHashMapGcTrigger m) False

-- -------------------------------------------------------------------------- --
-- Tests

prop_size :: NonNegative Int -> [(Int, Int)] -> Property
prop_size (NonNegative n) l = monadicIO $ do
    m <- run $ new @Int @Int (fromIntegral n) 600
    run $ traverse_ (atomically . uncurry (insert m)) l
    s <- run $ atomically $ size m
    assert $ s <= 2 * fromIntegral n
    assert $ s >= min (fromIntegral $ length $ L.nub $ fst <$> l) (fromIntegral n)

prop_gc :: Property
prop_gc = once $ monadicIO $ do
    m <- run $ new @Int @Int 2 1
    _ <- run $ atomically $ insert m 0 0
    do
        s <- run $ atomically $ size m
        assert $ s == 1
        run $ do
            threadDelay 1_100_000
            atomically (gc m)
    do
        s <- run $ atomically $ size m
        assert $ s == 1
        run $ do
            threadDelay 1_100_000
            atomically (gc m)
    do
        s <- run $ atomically $ size m
        assert $ s == 0

prop_gc2 :: Property
prop_gc2 = once $ monadicIO $ do
    m <- run $ new @Int @Int 2 1
    _ <- run $ atomically $ insert m 0 0
    do
        s <- run $ atomically $ size m
        assert $ s == 1
        run $ do
            threadDelay 100_000
            atomically (gc m)
    do
        s <- run $ atomically $ size m
        assert $ s == 1
        run $ do
            threadDelay 100_000
            atomically (gc m)
    do
        s <- run $ atomically $ size m
        assert $ s == 1

-- | This test is really, really slow
--
prop_size2 :: NonNegative Int -> [(Int, Int)] -> Property
prop_size2 (NonNegative n) l = monadicIO $ do
    m <- run $ new @Int @Int (fromIntegral n) 1
    run $ traverse_ (atomically . uncurry (insert m)) l
    do
        s <- run $ atomically $ size m
        assert $ s <= 2 * fromIntegral n
        assert $ s >= min (fromIntegral $ length $ L.nub $ fst <$> l) (fromIntegral n)
        run $ do
            threadDelay 1_000_000
            atomically (gc m)
    do
        s <- run $ atomically $ size m
        assert $ s <= fromIntegral n
        run $ do
            threadDelay 1_000_000
            atomically (gc m)
    do
        s <- run $ atomically $ size m
        assert $ s == 0

prop_content :: NonNegative Int -> [(Int, Int)] -> Property
prop_content (NonNegative n) l = monadicIO $ do
    m <- run $ new @Int @Int (fromIntegral n) 600
    run $ traverse_ (atomically . uncurry (insert m)) l
    s <- HM.toList <$> run (atomically $ toHashMap m)
    assert $ L.sort s == L.sort (recents (length s) l)

recents :: Eq a => Int -> [(a,b)] -> [(a,b)]
recents n l = take n $ L.nubBy ((==) `on` fst) $ reverse l

-- -------------------------------------------------------------------------- --
-- debug

t :: IO ()
t = do
    m <- run $ new @Int @Int 2 1
    _ <- run $ atomically $ insert m 0 0
    do
        s <- run $ atomically $ size m
        assert $ s == 1
        run $ do
            threadDelay 1_100_000
            atomically (gc m)
    do
        s <- run $ atomically $ size m
        assert $ s == 1
        run $ do
            threadDelay 1_100_000
            atomically (gc m)
    do
        s <- run $ atomically $ size m
        assert $ s == 0
  where
    run = id
    assert = print

t2 = do
    m <- new @Int @Int 2 1
    w <- mkWeakTVar (_boundedHashMapGcTrigger m) (print "finalize trigger")
    return w

data T3 k v = T3
    { _t0 :: {-# UNPACK #-} !Int
    , _t1 :: {-# UNPACK #-} !Int
    , _t2 :: {-# UNPACK #-} !(TVar Int)
    , _t3 :: {-# UNPACK #-} !(TVar (HM.HashMap k v))
    , _t4 :: {-# UNPACK #-} !(TVar Bool)
    }

newT3 :: forall k v . Eq k => Hashable k => Natural -> Natural -> IO (T3 k v)
newT3 a b = do
    tx <- T3 (fromIntegral a) (fromIntegral b)
        <$> newTVarIO 0
        <*> newTVarIO mempty
        <*> newTVarIO False
    w <- mkWeakTVar (_t4 tx) (return ())
    tid <- forkIO $ fix $ \run -> do
        threadDelay 1000000
        print "ahhhhh"
        deRefWeak w >>= \case
            Nothing -> do
                print "finalized t4"
                return ()
            Just var -> do
                atomically (writeTVar var True)
                run
    return tx

t3 = do
    t3 <- newT3 @Int @Int 1 1
    return ()

