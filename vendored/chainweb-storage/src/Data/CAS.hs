{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module: Data.CAS
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
-- Description: Content Addressed Key Value Store (CAS)
--
-- API for Content-Addressable Stores (CAS)
--
module Data.CAS
( IsCasValue(..)
, HasCasLookup(..)
, IsCas(..)
, casLookupM
, HasCasLookupConstraint
, CasConstraint
) where

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (throwM)

import Data.Foldable
import Data.Kind
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as V

import GHC.Generics

-- | The class of content-addressable values.
--
-- The casKey function must be morally injective:
--
-- prop> casKey a /= casKey b || a == b
--
-- Usually, 'casKey' is a cryptographic, i.e. collision resistant, hash
-- function.
--
class Eq (CasKeyType v) => IsCasValue v where
    type CasKeyType v
    casKey :: v -> CasKeyType v

-- | Read-Only View of a Content Addressed Key-Value Store
--
-- Since the key uniquely determines the content of the store a value for a key
-- is either available or not available. There is no dispute about the value
-- itself.
--
class IsCasValue (CasValueType a) => HasCasLookup a where
    type CasValueType a :: Type

    -- | Lookup a value in a content-addressable store
    --
    casLookup :: a -> CasKeyType (CasValueType a) -> IO (Maybe (CasValueType a))

    -- | Lookup a batch of values in a content-addressable store
    --
    casLookupBatch :: a -> V.Vector (CasKeyType (CasValueType a)) -> IO (V.Vector (Maybe (CasValueType a)))
    casLookupBatch = traverse . casLookup
    {-# INLINE casLookupBatch #-}

    -- | Check for the existence of a value in a content addressable store
    --
    casMember :: a -> CasKeyType (CasValueType a) -> IO Bool
    casMember db = fmap isJust . casLookup db
    {-# INLINE casMember #-}


-- | Content Addressed Key-Value Stores
--
-- Since the key uniquely determines the content of the store a value for a key
-- is either available or not available. There is no dispute about the value
-- itself. Thus there are only 'casInsert' and 'casDelete' functions but there
-- is no @casUpdate@ function.
--
class HasCasLookup a => IsCas a where

    -- | Insert a value into a content-addressasble store
    --
    casInsert :: a -> CasValueType a -> IO ()

    -- | Delete a value from a content-addressable store
    --
    casDelete :: a -> CasKeyType (CasValueType a) -> IO ()

    -- | Insert a batch of values into a content-addressasble store
    --
    casInsertBatch :: a -> V.Vector (CasValueType a) -> IO ()
    casInsertBatch = traverse_ . casInsert
    {-# INLINE casInsertBatch #-}

    -- | Delete a batch of values from a content-addressable store
    --
    casDeleteBatch :: a -> V.Vector (CasKeyType (CasValueType a)) -> IO ()
    casDeleteBatch = traverse_ . casDelete
    {-# INLINE casDeleteBatch #-}

-- | Lookup a value by its key in a content-addressable store and throw an
-- 'CasException' if the value doesn't exist in the store
--
casLookupM
    :: HasCasLookup a
    => a -> CasKeyType (CasValueType a) -> IO (CasValueType a)
casLookupM cas k = casLookup cas k >>= \case
    Nothing -> throwM . CasException $
      "casLookupM: lookup failed for cas key"
    (Just !x) -> return x

-- | Exceptions that are thrown by instances of 'IsCas'.
--
data CasException = CasException Text |  CasImplementationException SomeException
    deriving (Show, Generic)

instance Exception CasException

-- | @HasCasLookupConstraint cas x@ asserts that @cas x@ is an instance if
-- 'HasCasLookup' with value type 'x'.
--
type HasCasLookupConstraint cas x = (HasCasLookup (cas x), CasValueType (cas x) ~ x)

-- | @CasConstraint cas x@ asserts that @cas x@ is an instance if 'IsCas' with
-- value type 'x'.
--
type CasConstraint cas x = (HasCasLookupConstraint cas x, IsCas (cas x))

