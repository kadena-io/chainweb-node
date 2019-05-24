{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
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
-- TODO
--
module Data.CAS
( IsCasValue(..)
, IsCas(..)
, casMember
, casLookupM
, CasConstraint
) where

import Control.DeepSeq
import Control.Exception (Exception)
import Control.Monad.Catch (throwM)

import Data.Kind
import Data.Maybe
import Data.Text (Text)

import GHC.Generics

-- | The casKey function must be morally injective:
--
-- prop> casKey a /= casKey b || a == b
--
-- Usually, 'casKey' is a cryptographic, i.e. collision resistant, hash
-- function.
--
class Eq (CasKeyType v) => IsCasValue v where
    type CasKeyType v
    casKey :: v -> CasKeyType v

-- | Content Addressed Key-Value Stores
--
-- Since the key uniquely determines the content of the store a value for a key
-- is either available or not available. There is no dispute about the value
-- itself. Thus there are only 'casInsert' and 'casDelete' functions but there
-- is no @casUpdate@ function.
--
class IsCasValue (CasValueType a) => IsCas a where
    type CasValueType a :: Type
    casLookup :: a -> CasKeyType (CasValueType a) -> IO (Maybe (CasValueType a))
    casInsert :: a -> CasValueType a -> IO ()
    casDelete :: a -> CasKeyType (CasValueType a) -> IO ()

casMember :: IsCas a => a -> CasKeyType (CasValueType a) -> IO Bool
casMember db = fmap isJust . casLookup db
{-# INLINE casMember #-}

casLookupM
    :: IsCas a
    => a -> CasKeyType (CasValueType a) -> IO (CasValueType a)
casLookupM cas k = casLookup cas k >>= \case
    Nothing -> throwM . CasException $
      "casLookupM: lookup failed for cas key"
    Just x -> return x

newtype CasException = CasException Text
    deriving (Eq, Show, Generic)
    deriving newtype (NFData)

instance Exception CasException

type CasConstraint cas x = (IsCas (cas x), CasValueType (cas x) ~ x)
