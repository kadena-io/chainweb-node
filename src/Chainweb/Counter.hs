{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Counter
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Tools for counting events and logging the results.
--
module Chainweb.Counter
( Counter
, newCounter
, inc
, incBy

-- * Counter Map
, CounterMap
, newCounterMap
, incKey
, incKeyBy

-- * Rolling of Counters
, CounterValue
, roll

-- * Logging of Counters
, CounterLog
, LogFunctionCounter
, logFunctionCounter
) where

import Control.DeepSeq

import Data.Aeson
import qualified Data.Aeson.Key as A
import Data.Aeson.Encoding (pair)
import Data.Bifunctor
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics
import GHC.TypeLits

import System.LogLevel

-- internal modules

import Chainweb.Logger
import Chainweb.Utils

import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Typelevel Labeled Values

newtype Labeled (s :: Symbol) a = Labeled a
    deriving (Show, Eq, Ord, Generic)
    deriving (Functor, Foldable, Traversable)
    deriving newtype (Num, Enum, Bounded, Integral, Real, NFData)

instance (KnownSymbol s, ToJSON a) => ToJSON (Labeled s a) where
    toJSON = object . pure . kv
    toEncoding = pairs . kv
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

kv :: forall s a e x . KnownSymbol s => ToJSON a => KeyValue e x => Labeled s a -> x
kv (Labeled a) = symbolText @s .= a
{-# INLINE kv #-}

kv' :: forall s . KnownSymbol s => Labeled s Encoding -> Series
kv' (Labeled a) = pair (symbolText @s) a
{-# INLINE kv' #-}

-- -------------------------------------------------------------------------- --
-- Class of Counters

class IsCounter c where
    roll :: c -> IO CounterValue

instance KnownSymbol s => IsCounter (Counter s) where
    roll = rollCounter
    {-# INLINE roll #-}

instance KnownSymbol s => IsCounter (CounterMap s) where
    roll = rollCounterMap
    {-# INLINE roll #-}

-- -------------------------------------------------------------------------- --
-- Counter

newtype Counter (s :: Symbol) = Counter (IORef Int)
    deriving (Eq, Generic)
    deriving newtype (NFData)

-- | Roll the counter and return the value
--
rollCounter :: forall s . KnownSymbol s => Counter s -> IO CounterValue
rollCounter (Counter ref) = CounterValue @s . Labeled <$> atomicModifyIORef' ref (0,)

newCounter :: IO (Counter s)
newCounter = Counter <$> newIORef 0

inc :: Counter s -> IO ()
inc (Counter ref) = atomicModifyIORef' ref $ (,()) . succ

incBy :: Integral a => Counter s -> a -> IO ()
incBy (Counter ref) i = atomicModifyIORef' ref $ (,()) . (+) (int i)

-- -------------------------------------------------------------------------- --
-- CounterMap

newtype CounterMap (s :: Symbol) = CounterMap (IORef (HM.HashMap T.Text (IORef Int)))
    deriving (Eq, Generic)

newCounterMap :: IO (CounterMap s)
newCounterMap = CounterMap <$> newIORef mempty

-- | Roll the counters in the map and return the values.
--
-- This operation traverses the map twice. First for count the elements in the
-- map ('HM.size' is of /O(n)/) and then to copy the entries to the result
-- vector. Depending on the expected size of map this can take some time. When
-- this is a concern the log function should be called asynchronously.
--
rollCounterMap :: forall s . KnownSymbol s => CounterMap s -> IO CounterValue
rollCounterMap (CounterMap ref) = CounterMapValue @s . Labeled <$> do
    old <- atomicModifyIORef ref (mempty,)
    V.mapM (traverse readIORef) $ V.fromListN (HM.size old) $ HM.toList old
        -- We assume that 'V.mapM' and 'V.fromListN' get fused.

incKeyBy :: Integral a => CounterMap s -> T.Text -> a -> IO ()
incKeyBy cm@(CounterMap mref) k i = do
    (HM.lookup k <$> readIORef mref) >>= \case
        Just cref -> atomicModifyIORef' cref $ (, ()) . (+) (int i)
        Nothing -> do
            cref <- newIORef 0
            atomicModifyIORef mref $ \m -> (HM.insert k cref m, ())
                -- We don't for the result to reduce contention in the atomic
                -- swap. (It might be that the implementation of
                -- 'atomicModifyMutVar#' already takes care of this by adding
                -- adding the thunk for the update before doing any computation.
                -- But that's not obvious from the implementation of
                -- @atomicModifyIORef'@ and the documentation isn't clear.

            incKey cm k

incKey :: CounterMap s -> T.Text -> IO ()
incKey c k = incKeyBy c k (1 :: Int)
{-# INLINE incKey #-}

-- -------------------------------------------------------------------------- --
-- Counter Value

data CounterValue where
    CounterValue :: KnownSymbol s => {-# UNPACK #-} !(Labeled s Int) -> CounterValue
    CounterMapValue :: KnownSymbol s => {-# UNPACK #-} !(Labeled s (V.Vector (T.Text, Int))) -> CounterValue

instance NFData CounterValue where
    rnf (CounterValue v) = rnf v
    rnf (CounterMapValue v) = rnf v
    {-# INLINE rnf #-}

-- -------------------------------------------------------------------------- --
-- Logging of Counters

-- | The Counters are rolled at the time the message is logged (not when it is
-- processed by the backend). This adds a small amount of overhead, but ensures
-- that the timestamp of the log matches the time when the logs are rolled. This
-- means that logs are rolled even when the message ends up being discarded by
-- the backend, which means that counts are lost along with the discarded
-- messages. This is consistent with the semantics of discarding log messages.
--
newtype CounterLog = CounterLog (V.Vector CounterValue)
    deriving (Generic)
    deriving newtype (NFData)

instance LogMessage CounterLog where
    logText = encodeToText
    {-# INLINE logText #-}

instance ToJSON CounterLog where
    toJSON (CounterLog v) = object $ V.toList $ V.map f v
      where
        f (CounterValue i) = kv i
        f (CounterMapValue m) = kv $ fmap (object . V.toList . V.map (bimap A.fromText toJSON)) m
    {-# INLINE toJSON #-}

    toEncoding (CounterLog v) = pairs $ foldMap f v
      where
        f (CounterValue i) = kv i
        f (CounterMapValue m) = kv' $ fmap (pairs . foldMap (uncurry (.=) . first A.fromText)) m
    {-# INLINE toEncoding #-}

logFunctionCounter
    :: Logger l
    => l
    -> LogFunctionCounter
logFunctionCounter logger level = logFunctionJson logger level
    . CounterLog
    . V.fromList
    . toList
{-# INLINE logFunctionCounter #-}

type LogFunctionCounter = forall f . Foldable f => LogLevel -> f CounterValue -> IO ()
