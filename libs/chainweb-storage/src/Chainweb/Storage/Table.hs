{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
  ( IsCasValue (..)
  , ReadableTable (..)
  , tableLookupBatch
  , ReadableTable1
  , ReadableCas
  , Table (..)
  , Table1
  , Casify(..)
  , Cas
  , casInsert
  , casInsertBatch
  , casDelete
  , casDeleteBatch
  , IterableTable (..)
  , IterableTable1
  , IterableCas
  , Entry (..)
  , Iterator (..)
  , Iterator1
  , CasIterator
  , tableLookupM
  , casLookupM
  , TableException (..)
  )
where

import Control.Exception (Exception, SomeException)
import Control.Lens
import Control.Monad.Catch (throwM)

import Data.Coerce
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

-- | Read-Only View of a Key-Value Store
--
class ReadableTable t k v | t -> k v where
    tableLookup :: t -> k -> IO (Maybe v)
    tableLookupBatch' :: t -> Traversal s r k (Maybe v) -> s -> IO r
    tableLookupBatch' t l = l (tableLookup t)
    tableMember :: t -> k -> IO Bool
    tableMember t k = isJust <$> tableLookup t k

tableLookupBatch :: (ReadableTable t k v, Each s t' k (Maybe v)) => t -> s -> IO t'
tableLookupBatch t = tableLookupBatch' t each

type ReadableCas t v = ReadableTable t (CasKeyType v) v
type ReadableTable1 t = forall k v. ReadableTable (t k v) k v

class ReadableTable t k v => Table t k v | t -> k v where
    tableInsert :: t -> k -> v -> IO ()
    tableInsertBatch :: t -> [(k, v)] -> IO ()
    tableInsertBatch t kvs = traverse_ (uncurry (tableInsert t)) kvs
    tableDelete :: t -> k -> IO ()
    tableDeleteBatch :: t -> [k] -> IO ()
    tableDeleteBatch t ks = traverse_ (tableDelete t) ks
type Table1 t = forall k v. Table (t k v) k v

type Cas t v = Table t (CasKeyType v) v

casInsert :: (IsCasValue v, Cas t v) => t -> v -> IO ()
casInsert t v = tableInsert t (casKey v) v

casInsertBatch :: (IsCasValue v, Cas t v) => t -> [v] -> IO ()
casInsertBatch t vs = tableInsertBatch t [(casKey v, v) | v <- vs]

casDelete :: (IsCasValue v, Cas t v) => t -> v -> IO ()
casDelete t = tableDelete t . casKey

casDeleteBatch :: (IsCasValue v, Cas t v) => t -> [v] -> IO ()
casDeleteBatch t = tableDeleteBatch t . fmap casKey

class (Table t k v, Iterator i k v) => IterableTable t i k v | t -> i k v where
    -- the created iterator must be positioned at the start of the table.
    withTableIterator :: t -> (i -> IO a) -> IO a
type IterableTable1 t i = forall k v. IterableTable (t k v) (i k v) k v

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
type Iterator1 i = forall k v. Iterator (i k v) k v

type CasIterator i v = Iterator i (CasKeyType v) v

-- | A newtype wrapper that takes only a single type constructor. This useful in
-- situations where a Higher Order type constructor for a CAS is required. A
-- type synonym doesn't work in this situation because type synonyms must be
-- fully applied.
--
newtype Casify t v = Casify { unCasify :: t (CasKeyType v) v }
instance forall t k v. (CasKeyType v ~ k, ReadableTable (t k v) k v) => ReadableTable (Casify t v) k v where
    tableLookup = coerce @(t k v -> k -> IO (Maybe v)) tableLookup
    -- can't seem to write `coerce` without the resulting instantiation being impredicative
    tableLookupBatch' (Casify t) b = tableLookupBatch' t b
    tableMember = coerce @(t k v -> k -> IO Bool) tableMember
instance forall t k v. (CasKeyType v ~ k, Table (t k v) k v) => Table (Casify t v) k v where
    tableInsert = coerce @(t k v -> k -> v -> IO ()) tableInsert
    tableInsertBatch = coerce @(t k v -> [(k, v)] -> IO ()) tableInsertBatch
    tableDelete = coerce @(t k v -> k -> IO ()) tableDelete
    tableDeleteBatch = coerce @(t k v -> [k] -> IO ()) tableDeleteBatch
-- TODO: why is this Iterator superclass needed?
instance forall t i k v. (CasKeyType v ~ k, IterableTable (t k v) i k v, Iterator i k v) => IterableTable (Casify t v) i k v where
    withTableIterator :: forall a. Casify t v -> (i -> IO a) -> IO a
    withTableIterator = coerce @(t k v -> (i -> IO a) -> IO a) withTableIterator

-- | Lookup a value by its key in a key-value store and throw an
-- 'TableException' if the value doesn't exist in the store
tableLookupM :: (HasCallStack, ReadableTable t k v) => t -> k -> IO v
tableLookupM cas k =
    tableLookup cas k >>= \case
        Nothing ->
            throwM . TableException $
                "tableLookupM: lookup failed for table key"
        Just v -> return $! v

casLookupM :: (HasCallStack, ReadableCas t v) => t -> CasKeyType v -> IO v
casLookupM = tableLookupM

-- | Exceptions that are thrown by instances of 'IsCas'.
data TableException
    = TableException !Text
    | TableImplementationException !SomeException
  deriving (Show, Generic)

instance Exception TableException

