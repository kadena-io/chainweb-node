{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

#include "MachDeps.h"

-- |
-- Module: Numeric.Cast
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Lossless conversions between numeric types
--
module Numeric.Cast
( NumCast(..)
, MaybeNumCast(..)
) where

import Data.Int
import Data.Word
import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- Safe Casts

class NumCast a b where
    numCast :: a -> b

instance NumCast a a where
    numCast = id


instance Integral a => NumCast a Integer where
    numCast = toInteger


instance Real a => NumCast a Rational where
    numCast = toRational



-- signed integral types

instance NumCast Int8 Int16 where
    numCast = fromIntegral


instance NumCast Int8 Int32 where
    numCast = fromIntegral


instance NumCast Int8 Int64 where
    numCast = fromIntegral


instance NumCast Int16 Int32 where
    numCast = fromIntegral


instance NumCast Int16 Int64 where
    numCast = fromIntegral


instance NumCast Int32 Int64 where
    numCast = fromIntegral


#if (WORD_SIZE_IN_BITS <= 64)
instance NumCast Int Int64 where
    numCast = fromIntegral

#endif

#if (WORD_SIZE_IN_BITS >= 64)
instance NumCast Int64 Int where
    numCast = fromIntegral

#endif

-- unsigned integral types

instance NumCast Word8 Word16 where
    numCast = fromIntegral


instance NumCast Word8 Word32 where
    numCast = fromIntegral


instance NumCast Word8 Word64 where
    numCast = fromIntegral


instance NumCast Word8 Natural where
    numCast = fromIntegral


instance NumCast Word16 Word32 where
    numCast = fromIntegral


instance NumCast Word16 Word64 where
    numCast = fromIntegral


instance NumCast Word16 Natural where
    numCast = fromIntegral


instance NumCast Word32 Word64 where
    numCast = fromIntegral


instance NumCast Word32 Natural where
    numCast = fromIntegral


instance NumCast Word64 Natural where
    numCast = fromIntegral


#if (WORD_SIZE_IN_BITS <= 64)
instance NumCast Word Word64 where
    numCast = fromIntegral

#endif

#if (WORD_SIZE_IN_BITS >= 64)
instance NumCast Word64 Word where
    numCast = fromIntegral

#endif

-- Fractional Types

instance NumCast Float Double where
    numCast = realToFrac


-- TODO: bounded integral values to double

-- -------------------------------------------------------------------------- --
-- Checked Casts

class MaybeNumCast a b where maybeNumCast :: a -> Maybe b

-- instance NumCast a b => MaybeNumCast a b where maybeNumCast = Just . numCast

instance (Integral a) => MaybeNumCast a Integer where
    maybeNumCast = Just . toInteger


instance (Integral a) => MaybeNumCast a Natural where
    maybeNumCast a
        | a >= 0 = Just $ fromIntegral a
        | otherwise = Nothing


instance (Integral a, Integral b, Bounded b) => MaybeNumCast a b where
    maybeNumCast a
        | a <= fromIntegral (maxBound :: b) && a >= fromIntegral (minBound :: b) = Just $ fromIntegral a
        | otherwise = Nothing


-- TODO Floating numbers

