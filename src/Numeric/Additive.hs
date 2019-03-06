{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Numeric.Additive
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Haskell's @Num@ class doesn't support fine grained control
-- over what arithmetic operations are defined for a type.
-- Sometimes only some operations have a well defined semantics
-- and @Num@ instances are notorious for including undefined/error
-- values or unlawful workarounds.
--
module Numeric.Additive
(
-- * Additive Semigroup
  AdditiveSemigroup(..)
, AdditiveAbelianSemigroup
, (^+^)

-- * Additive Monoid
, AdditiveMonoid(..)
, AdditiveAbelianMonoid

-- * Additive Group
, AdditiveGroup(..)

-- * Additive Abelian Group
, AdditiveAbelianGroup
, (^-^)
) where

import Data.DoubleWord
import Data.Int
import Data.Word

import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- | Additive Semigroup
--
-- prop> (a `plus` b) `plus` c == a `plus` (b `plus` c)
--
class AdditiveSemigroup g where
    plus :: g -> g -> g

instance AdditiveSemigroup Integer where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Rational where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Natural where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Int where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Word where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Word8 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Word16 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Word32 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Word64 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Word128 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Word256 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Int8 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Int16 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Int32 where
    plus = (+)
    {-# INLINE plus #-}

instance AdditiveSemigroup Int64 where
    plus = (+)
    {-# INLINE plus #-}

-- -------------------------------------------------------------------------- --
-- | Additive Abelian Semigroup
--
-- prop> a `plus` b == b `plus` a
--
class AdditiveSemigroup g => AdditiveAbelianSemigroup g

instance AdditiveAbelianSemigroup Integer
instance AdditiveAbelianSemigroup Rational
instance AdditiveAbelianSemigroup Natural
instance AdditiveAbelianSemigroup Int
instance AdditiveAbelianSemigroup Int8
instance AdditiveAbelianSemigroup Int16
instance AdditiveAbelianSemigroup Int32
instance AdditiveAbelianSemigroup Int64
instance AdditiveAbelianSemigroup Word
instance AdditiveAbelianSemigroup Word8
instance AdditiveAbelianSemigroup Word16
instance AdditiveAbelianSemigroup Word32
instance AdditiveAbelianSemigroup Word64
instance AdditiveAbelianSemigroup Word128
instance AdditiveAbelianSemigroup Word256

infixl 6 ^+^
(^+^) :: AdditiveAbelianSemigroup g => g -> g -> g
(^+^) = plus
{-# INLINE (^+^) #-}

-- -------------------------------------------------------------------------- --
-- | Additive Monoid
--
-- prop> a `plus` zero == a
-- prop> zero `plus` a == a
--
class AdditiveSemigroup g => AdditiveMonoid g where
    zero :: g

instance AdditiveMonoid Integer where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Rational where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Natural where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Int where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Word where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Word8 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Word16 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Word32 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Word64 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Word128 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Word256 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Int8 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Int16 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Int32 where
    zero = 0
    {-# INLINE zero #-}

instance AdditiveMonoid Int64 where
    zero = 0
    {-# INLINE zero #-}

type AdditiveAbelianMonoid g = (AdditiveMonoid g, AdditiveAbelianSemigroup g)

-- -------------------------------------------------------------------------- --
-- | Additive Group
--
-- prop> a `plus` inverse a == zero
-- prop> inverse a `plus` a == zero
--
class AdditiveMonoid g => AdditiveGroup g where
    invert :: g -> g
    invert a = zero `minus` a

    minus :: g -> g -> g
    minus a b = a `plus` invert b

    {-# MINIMAL invert | minus #-}

instance AdditiveGroup Integer where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Rational where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Int where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Word where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Word8 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Word16 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Word32 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Word64 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Word128 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Word256 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Int8 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Int16 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Int32 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

instance AdditiveGroup Int64 where
    invert a = -a
    minus = (-)
    {-# INLINE invert #-}
    {-# INLINE minus #-}

-- -------------------------------------------------------------------------- --
-- | Additive Abelian Group
--
type AdditiveAbelianGroup g = (AdditiveGroup g, AdditiveAbelianMonoid g)

infix 6 ^-^
(^-^) :: AdditiveAbelianGroup g => g -> g -> g
(^-^) = minus
{-# INLINE (^-^) #-}
