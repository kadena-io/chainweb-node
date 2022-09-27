{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Storage.Table
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Edmund Noble <edmund@kadena.io>
-- Stability: experimental
-- Description: Key Value Store
--
-- API for Key-Value Stores
module Chainweb.Storage.Table
  ( IsCasValue (..),
    ReadableTable (..),
    ReadableCas,
    Table (..),
    Cas,
    casInsert,
    casInsertBatch,
    casDelete,
    casDeleteBatch,
    IterableTable (..),
    IterableCas,
    Entry (..),
    Iterator (..),
    CasIterator,
    tableLookupM,
    casLookupM,
    TableException (..),
  )
where

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (throwM)

import Data.Foldable
import Data.Maybe
import Data.Text (Text)

import GHC.Generics
import GHC.Stack

-- | The class of content-addressable values.
--
-- The casKey function must be morally injective:
--
-- prop> casKey a /= casKey b || a == b
--
-- Usually, 'casKey' is a cryptographic, i.e. collision resistant, hash
-- function.
class Eq (CasKeyType v) => IsCasValue v where
    type CasKeyType v
    casKey :: v -> CasKeyType v

-- | Read-Only View of a Content Addressed Key-Value Store
--
-- Since the key uniquely determines the content of the store a value for a key
-- is either available or not available. There is no dispute about the value
-- itself.
class ReadableTable t k v | t -> k v where
    tableLookup :: k -> t -> IO (Maybe v)
    tableLookupBatch :: [k] -> t -> IO [Maybe v]
    tableLookupBatch ks t = traverse (flip tableLookup t) ks
    tableMember :: k -> t -> IO Bool

type ReadableCas t v = ReadableTable t (CasKeyType v) v

class ReadableTable t k v => Table t k v | t -> k v where
    tableInsert :: k -> v -> t -> IO ()
    tableInsertBatch :: [(k, v)] -> t -> IO ()
    tableInsertBatch kvs t = traverse_ (flip (uncurry tableInsert) t) kvs
    tableDelete :: k -> t -> IO ()
    tableDeleteBatch :: [k] -> t -> IO ()
    tableDeleteBatch ks t = traverse_ (flip tableDelete t) ks

type Cas t v = Table t (CasKeyType v) v

casInsert :: (IsCasValue v, Cas t v) => v -> t -> IO ()
casInsert v = tableInsert (casKey v) v

casInsertBatch :: (IsCasValue v, Cas t v) => [v] -> t -> IO ()
casInsertBatch vs = tableInsertBatch [(casKey v, v) | v <- vs]

casDelete :: (IsCasValue v, Cas t v) => v -> t -> IO ()
casDelete = tableDelete . casKey

casDeleteBatch :: (IsCasValue v, Cas t v) => [v] -> t -> IO ()
casDeleteBatch = tableDeleteBatch . fmap casKey

class (Table t k v, Iterator i k v) => IterableTable t i k v | t -> k v, i -> k v, t -> i where
    -- the created iterator must be positioned at the start of the table.
    withTableIterator :: t -> (i -> IO a) -> IO a

type IterableCas t v = IterableTable t (CasKeyType v) v

data Entry k v = Entry !k !v
    deriving (Eq, Show, Ord)

class Iterator i k v | i -> k v where
    iterSeek :: i -> k -> IO ()
    iterLast :: i -> IO ()
    iterFirst :: i -> IO ()
    iterNext :: i -> IO ()
    iterPrev :: i -> IO ()
    iterEntry :: i -> IO (Maybe (Entry k v))
    iterKey :: i -> IO (Maybe k)
    iterKey i = (fmap . fmap) (\(Entry k _) -> k) $ iterEntry i
    iterValue :: i -> IO (Maybe v)
    iterValue i = (fmap . fmap) (\(Entry _ v) -> v) $ iterEntry i
    iterValid :: i -> IO Bool
    iterValid i = isJust <$> iterKey i

type CasIterator i v = Iterator i (CasKeyType v) v

-- | Lookup a value by its key in a content-addressable store and throw an
-- 'TableException' if the value doesn't exist in the store
tableLookupM :: (HasCallStack, ReadableTable t k v) => k -> t -> IO v
tableLookupM cas k =
    tableLookup cas k >>= \case
        Nothing ->
            throwM . TableException $
                "tableLookupM: lookup failed for table key"
        Just v -> return $! v

casLookupM :: (HasCallStack, Cas t v) => CasKeyType v -> t -> IO v
casLookupM = tableLookupM

-- | Exceptions that are thrown by instances of 'IsCas'.
data TableException
    = TableException !Text
    | TableImplementationException !SomeException
  deriving (Show, Generic)

instance Exception TableException

