{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Numeric.AffineSpace
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Numeric.AffineSpace
(
-- * Torsor
  LeftTorsor(..)
, (.+^)
, (^+.)
, (.-.)
, (.-^)

-- * Vector Space
, FractionalVectorSpace(..)

-- * AfficeSpace
, AffineSpace
) where

import Numeric.Additive

-- -------------------------------------------------------------------------- --
-- Torsor

-- | A torsor is a generalization of affine spaces. It doesn't require the
-- underlying structure to be vector space, but an additive group suffices.
-- This means that it doesn't support scalar multiplication. In particular
-- it doesn't require an inverse operation to multiplication, which would
-- add unneeded complexity to the formal definition of the operational
-- semantics.
--
-- A Torsor is also called principal homogeous space.
--
-- prop> zero `add` a == a
-- prop> (a `plus` b) `add` t == a `add` (b `add` t)
-- prop> (s `diff` t) `add` t == s
--
-- The last property is states that `add` is a bijection.
--
class (AdditiveGroup (Diff t)) => LeftTorsor t where
    type Diff t
    add :: Diff t -> t -> t
    diff :: t -> t -> Diff t

instance LeftTorsor Integer where
    type Diff Integer = Integer
    add = (+)
    diff = (-)
    {-# INLINE add #-}
    {-# INLINE diff #-}

instance LeftTorsor Rational where
    type Diff Rational = Rational
    add = (+)
    diff = (-)
    {-# INLINE add #-}
    {-# INLINE diff #-}

infix 6 .-.
(.-.) :: AdditiveAbelianGroup (Diff t) => LeftTorsor t => t -> t -> Diff t
(.-.) = diff

infixl 6 ^+.
(^+.) :: AdditiveAbelianGroup (Diff t) => LeftTorsor t => Diff t -> t -> t
(^+.) = add

infixl 6 .+^
(.+^) :: AdditiveAbelianGroup (Diff t) => LeftTorsor t => t -> Diff t -> t
(.+^) = flip add

infixl 6 .-^
(.-^) :: AdditiveAbelianGroup (Diff t) => LeftTorsor t => t -> Diff t -> t
(.-^) t d = t .+^ invert d

-- -------------------------------------------------------------------------- --
-- | Vector Space over Fractional Numbers
--
-- A real vector space is an additive abelian group that forms an module
-- with the field of real numbers.
--
-- prop> a * (b `scale` c) == (a * b) `scale` c
-- prop> 1 `scale` a == a
-- prop> a `scale` (b `plus` c) == (a `scale` b) `plus` (a `scale` c)
-- prop> (a + b) `scale` c == (a `scale` c) `plus` (b `scale` c)
--
class (AdditiveAbelianGroup v, Fractional (Scalar v)) => FractionalVectorSpace v where
    type Scalar v
    scale :: Scalar v -> v -> v

instance FractionalVectorSpace Rational where
    type Scalar Rational = Rational
    scale = (*)

-- -------------------------------------------------------------------------- --
-- Affine Space

-- | An affine space is a torsor for the action of the additive group
-- of a vector space.
--
type AffineSpace t = (FractionalVectorSpace (Diff t), LeftTorsor t)
