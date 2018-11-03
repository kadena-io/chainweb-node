{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.Singletons
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module contains a selection of the definitions from the module
-- "Data.Singletons.Internal" from the [singletons
-- package](https://hackage.haskell.org/package/singletons).
--
-- The copyright and license of the singletons package applies to the respective
-- definitions in this module.
--
module Data.Singletons
( Sing
, SingI(..)
, pattern Sing

-- * Sing Kind
, SingKind(..)
, SomeSing(..)
, pattern FromSing

-- * Sing Instance
, SingInstance(..)
, singInstance

-- * Utils
, KindOf
, demote
, withSomeSing
, withSing
, withSingI
, singThat
, singByProxy
) where

import Data.Kind

import Unsafe.Coerce

-- -------------------------------------------------------------------------- --
-- Sing

data family Sing :: k -> Type

class SingI (a :: k) where sing :: Sing a

pattern Sing :: forall k (a :: k) . () => SingI a => Sing a
pattern Sing <- (singInstance -> SingInstance)
  where Sing = sing
{-# COMPLETE Sing #-}

-- -------------------------------------------------------------------------- --
-- Sing Kind

class SingKind k where
    type Demote k = (r :: Type) | r -> k
    fromSing :: Sing (a :: k) -> Demote k
    toSing :: Demote k -> SomeSing k

data SomeSing k where
    SomeSing :: Sing (a :: k) -> SomeSing k

pattern FromSing :: SingKind k => forall (a :: k) . Sing a -> Demote k
pattern FromSing sng <- ((\demotedVal -> withSomeSing demotedVal SomeSing) -> SomeSing sng)
  where FromSing sng = fromSing sng
{-# COMPLETE FromSing #-}

-- -------------------------------------------------------------------------- --
-- Sing Instance

data SingInstance (a :: k) where
    SingInstance :: SingI a => SingInstance a

newtype DI a = Don'tInstantiate (SingI a => SingInstance a)

singInstance :: forall k (a :: k) . Sing a -> SingInstance a
singInstance s = with_sing_i SingInstance
  where
    with_sing_i :: (SingI a => SingInstance a) -> SingInstance a
    with_sing_i si = unsafeCoerce (Don'tInstantiate si) s

-- -------------------------------------------------------------------------- --
-- Utils

type KindOf (a :: k) = k

demote
    :: forall a
    . SingKind (KindOf a)
    => SingI a
    => Demote (KindOf a)
demote = fromSing (sing @(KindOf a) @a)

withSomeSing
    :: forall k r
    . SingKind k
    => Demote k
    -> (forall (a :: k) . Sing a -> r)
    -> r
withSomeSing x f = case toSing x of SomeSing x' -> f x'

withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing

withSingI :: Sing n -> (SingI n => r) -> r
withSingI x r = case singInstance x of SingInstance -> r

singThat
    :: forall k (a :: k)
    . SingKind k
    => SingI a
    => (Demote k -> Bool)
    -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing

singByProxy :: SingI a => proxy a -> Sing a
singByProxy _ = sing

