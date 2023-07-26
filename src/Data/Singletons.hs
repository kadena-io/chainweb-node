{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
( -- * Data family of singletons
  Sing(..)
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

-- * Constraint Dictionaries
, Dict(..)

-- * Typelevel Peano Numbers
, N(..)
, type SZ
, type SN
, natToN
, nToNat
, someN

-- * Tools for type level lists
, type AtIndex
, type Index
) where

import Data.Kind

import GHC.TypeLits

import Unsafe.Coerce

-- -------------------------------------------------------------------------- --
-- Sing

-- | Kind-indexed family of singleton types.
--
-- In order to define singletons for types of a data kind @A@, one defines an
-- instance that has a value constructor for each type constructor of @A@. The
-- following is an example how to define singletons for types of kind @Maybe a@:
--
-- @
-- data instance Sing :: forall a . Maybe a -> Type where
--    SNothing :: Sing 'Nothing
--    SJust :: Sing a -> Sing ('Just a)
-- @
--
data family Sing :: k -> Type

-- | The class of types that have singletons.
--
class SingI (a :: k) where sing :: Sing a

-- | A pattern for converting between a singlton and the corresponding
-- 'SingInstance'.
--
pattern Sing :: forall k (a :: k) . () => SingI a => Sing a
pattern Sing <- (singInstance -> SingInstance)
  where Sing = sing
{-# COMPLETE Sing #-}

-- -------------------------------------------------------------------------- --
-- Sing Kind

-- | The class of kinds for which singletons are defined.
--
class SingKind k where
    type Demote k = (r :: Type) | r -> k
    fromSing :: Sing (a :: k) -> Demote k
    toSing :: Demote k -> SomeSing k

-- | Existentially quantified singleton type. This allows to hide the type of
-- the singleton from the copmiler. The type can be brought into scope at
-- runtime by pattern matching on the 'SomeSing' constructor.
--
-- This can, for instance, be used for creating singletons from user provided
-- values are deserialized values.
--
data SomeSing k where
    SomeSing :: Sing (a :: k) -> SomeSing k

-- | A pattern for converting betwen a singleton and it's demoted value.
--
-- For version 8.10 GHC isn't able to match the COMPLETE pragma. Since orphan
-- COMPLETE pragmas aren't supported it's not possible to specialize the pargma
-- for individual types. Instead we are providing specialized patterns along
-- with the types for which singletons are defined.
--
pattern FromSing :: SingKind k => forall (a :: k) . Sing a -> Demote k
pattern FromSing sng <- ((\demotedVal -> withSomeSing demotedVal SomeSing) -> SomeSing sng)
  where FromSing sng = fromSing sng
{-# COMPLETE FromSing #-}

-- -------------------------------------------------------------------------- --
-- Sing Instance

-- | A data type to enclose and explicitly pass around an 'SingI' dictionary.
--
data SingInstance (a :: k) where
    SingInstance :: SingI a => SingInstance a

newtype DI a = Don'tInstantiate (SingI a => SingInstance a)

-- | Create a 'SingInstance' value from a singleton. Pattern matching on the
-- resulting value brings the respective 'SingI' constraint into scope.
--
singInstance :: forall k (a :: k) . Sing a -> SingInstance a
singInstance s = with_sing_i SingInstance
  where
    with_sing_i :: (SingI a => SingInstance a) -> SingInstance a
    with_sing_i si = unsafeCoerce (Don'tInstantiate si) s

-- -------------------------------------------------------------------------- --
-- Utils

-- | Obtain the kind of a type variable.
--
type KindOf (a :: k) = k

-- | Return the demoted value of a singleton.
--
demote
    :: forall a
    . SingKind (KindOf a)
    => SingI a
    => Demote (KindOf a)
demote = fromSing (sing @(KindOf a) @a)

-- | Provide a computation with the singlton for a value.
--
withSomeSing
    :: forall k r
    . SingKind k
    => Demote k
    -> (forall (a :: k) . Sing a -> r)
    -> r
withSomeSing x f = case toSing x of SomeSing x' -> f x'

-- | Given a 'SingI' instance provide an inner computation with an explicit
-- singlton value.
--
withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing

-- | Given an singleton value, provide an inner computation with a 'SingI'
-- insteance.
--
withSingI :: Sing n -> (SingI n => r) -> r
withSingI x r = case singInstance x of SingInstance -> r

-- | Provide 'Just' a singleton for a value that satisfies a predicate. If the
-- value doesn't satify the predicate 'Nothing' is returned.
--
singThat
    :: forall k (a :: k)
    . SingKind k
    => SingI a
    => (Demote k -> Bool)
    -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing

-- | Get a singleton value from a type proxy
--
singByProxy :: SingI a => proxy a -> Sing a
singByProxy _ = sing

-- -------------------------------------------------------------------------- --
-- Constraint Dictionaries

data Dict :: Constraint -> Type -> Type where
    Dict :: c => a -> Dict c a

-- -------------------------------------------------------------------------- --
-- Type Level Peano Numbers

data N = Z | S N
    deriving (Show, Eq)

data instance Sing (n :: N) where
    SZ :: Sing 'Z
    SS :: Sing i -> Sing ('S i)

deriving instance Show (Sing (n :: N))
deriving instance Eq (Sing (n :: N))

type SZ = Sing 'Z
type SN i = Sing ('S i)

instance SingI 'Z where sing = SZ
instance SingI i => SingI ('S i) where sing = SS sing

instance SingKind N where
    type Demote N = N
    fromSing SZ = Z
    fromSing (SS i) = S (fromSing i)

    toSing Z = SomeSing SZ
    toSing (S i) = case toSing i of
        SomeSing n -> SomeSing (SS n)

natToN :: Natural -> N
natToN 0 = Z
natToN i = S $ natToN (i - 1)

nToNat :: N -> Natural
nToNat Z = 0
nToNat (S i) = 1 + nToNat i

someN :: Natural -> SomeSing N
someN = toSing . natToN

-- -------------------------------------------------------------------------- --
-- HList tools

type family AtIndex (n :: N) (l :: [Type]) :: Type where
    AtIndex 'Z (h ': _) = h
    AtIndex ('S i) (_ ': t) = AtIndex i t
    AtIndex n '[] = TypeError ('Text "Data.Singletons.AtIndex: AtIndex for type level list out of bound")

type family Index (h :: Type) (l :: [Type]) :: N where
    Index h (h ': _) = 'Z
    Index h (_ ': t) = 'S (Index h t)
    Index h '[] = TypeError
        ( 'Text "Data.Singletons.Index: element of type "
        ':<>: 'ShowType h
        ':<>: 'Text " not found in list"
        )
