{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.CAS.HashMap
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
-- Description:
--
-- A thread-safe in-memory 'IsCas' implementation based on 'HM.HashMap'.
--
module Data.CAS.HashMap
( HashMapCas
, emptyCas
, toList
, size
) where

import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Data.Hashable
import qualified Data.HashMap.Strict as HM

-- internal modules

import Data.CAS

-- | An 'IsCas' implementation that is base on 'HM.HashMap'.
--
data HashMapCas v = IsCasValue v => HashMapCas !(TVar (HM.HashMap (CasKeyType v) v))

instance (Show (CasKeyType v), Hashable (CasKeyType v), IsCasValue v) => IsCas (HashMapCas v) where
    type CasValueType (HashMapCas v) = v
    casLookup (HashMapCas var) k = HM.lookup k <$> readTVarIO var
    casInsert cas@(HashMapCas var) a = casLookup cas (casKey a) >>= \case
        Just _ -> return ()
        Nothing -> atomically $ modifyTVar' var $ HM.insert (casKey a) a
    casDelete cas@(HashMapCas var) k = casLookup cas k >>= \case
        Nothing -> return ()
        Just _ -> atomically $ modifyTVar' var $ HM.delete k


-- | Create new empty CAS
--
emptyCas :: Hashable (CasKeyType v) => IsCasValue v => IO (HashMapCas v)
emptyCas = HashMapCas <$> newTVarIO mempty

-- | Return all entries of CAS as List
--
toList :: HashMapCas v -> IO [v]
toList (HashMapCas var) = HM.elems <$> readTVarIO var

-- | The number of items in the CAS
--
size :: HashMapCas v -> IO Int
size (HashMapCas var) = HM.size <$> readTVarIO var

