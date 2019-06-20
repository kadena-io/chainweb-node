{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.Time
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A time library with a focus on supporting different numeric types for
-- representing time and time spans. This is useful for defining time
-- operations and serialization down to the bit level, including potential
-- arithmetic overflows.
--
-- Precision is fixed to microseconds.
--
module Chainweb.Time
(
-- * TimeSpan
  TimeSpan(..)
, encodeTimeSpan
, encodeTimeSpanToWord64
, decodeTimeSpan
, castTimeSpan
, maybeCastTimeSpan
, ceilingTimeSpan
, floorTimeSpan
, scaleTimeSpan
, addTimeSpan

-- * Time
, Time(..)
, minTime
, maxTime
, encodeTime
, encodeTimeToWord64
, decodeTime
, castTime
, maybeCastTime
, ceilingTime
, floorTime
, getCurrentTimeIntegral
, epoche

-- * TimeSpan values
, microsecond
, millisecond
, second
, minute
, hour
, day

-- * Seconds
, Seconds(..)
, secondsToTimeSpan
, timeSpanToSeconds
, secondsToText
, secondsFromText
) where

import Control.DeepSeq
import Control.Monad ((<$!>))
import Control.Monad.Catch

import Data.Aeson (FromJSON, ToJSON)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Signed
import Data.Hashable (Hashable)
import Data.Int
import Data.Kind
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Word

import GHC.Generics

import Test.QuickCheck (Arbitrary(..), Gen)

-- internal imports

import Chainweb.Utils
import Numeric.Additive
import Numeric.AffineSpace
import Numeric.Cast

-- -------------------------------------------------------------------------- --
-- TimeSpan

-- | The internal unit is microseconds.
--
newtype TimeSpan :: Type -> Type where
    TimeSpan :: a -> TimeSpan a
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype
        ( AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , AdditiveGroup, FractionalVectorSpace
        , Enum, Bounded
        , ToJSON, FromJSON
        )

encodeTimeSpan :: MonadPut m => TimeSpan Int64 -> m ()
encodeTimeSpan (TimeSpan a) = putWord64le $ unsigned a
{-# INLINE encodeTimeSpan #-}

encodeTimeSpanToWord64 :: TimeSpan Int64 -> Word64
encodeTimeSpanToWord64 (TimeSpan a) = BA.unLE . BA.toLE $ unsigned a
{-# INLINE encodeTimeSpanToWord64 #-}

decodeTimeSpan :: MonadGet m => m (TimeSpan Int64)
decodeTimeSpan = TimeSpan . signed <$!> getWord64le
{-# INLINE decodeTimeSpan #-}

castTimeSpan :: NumCast a b => TimeSpan a -> TimeSpan b
castTimeSpan (TimeSpan a) = TimeSpan $ numCast a
{-# INLINE castTimeSpan #-}

maybeCastTimeSpan :: MaybeNumCast a b => TimeSpan a -> Maybe (TimeSpan b)
maybeCastTimeSpan (TimeSpan a) = TimeSpan <$!> maybeNumCast a
{-# INLINE maybeCastTimeSpan #-}

ceilingTimeSpan :: RealFrac a => Integral b => TimeSpan a -> TimeSpan b
ceilingTimeSpan (TimeSpan a) = TimeSpan (ceiling a)
{-# INLINE ceilingTimeSpan #-}

floorTimeSpan :: RealFrac a => Integral b => TimeSpan a -> TimeSpan b
floorTimeSpan (TimeSpan a) = TimeSpan (floor a)
{-# INLINE floorTimeSpan #-}

scaleTimeSpan :: Integral a => Num b => a -> TimeSpan b -> TimeSpan b
scaleTimeSpan scalar (TimeSpan t) = TimeSpan (fromIntegral scalar * t)
{-# INLINE scaleTimeSpan #-}

addTimeSpan :: Num a => TimeSpan a -> TimeSpan a -> TimeSpan a
addTimeSpan (TimeSpan a) (TimeSpan b) = TimeSpan (a + b)
{-# INLINE addTimeSpan #-}

-- -------------------------------------------------------------------------- --
-- Time

-- | Time is measured as microseconds relative to UNIX Epoche
--
newtype Time a = Time (TimeSpan a)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype (Enum, Bounded, ToJSON, FromJSON)

instance AdditiveGroup (TimeSpan a) => LeftTorsor (Time a) where
    type Diff (Time a) = TimeSpan a
    add s (Time t) = Time (s `plus` t)
    diff (Time t₁) (Time t₂) = t₁ `minus` t₂
    {-# INLINE add #-}
    {-# INLINE diff #-}

epoche :: Num a => Time a
epoche = Time (TimeSpan 0)
{-# INLINE epoche #-}

-- | Adhering to `Time`, this is the current number of microseconds since the
-- epoch.
--
getCurrentTimeIntegral :: Integral a => IO (Time a)
getCurrentTimeIntegral = do
    -- returns POSIX seconds with picosecond precision
    t <- getPOSIXTime
    return $! Time $! TimeSpan $! round $ t * 1000000

encodeTime :: MonadPut m => Time Int64 -> m ()
encodeTime (Time a) = encodeTimeSpan a
{-# INLINE encodeTime #-}

encodeTimeToWord64 :: Time Int64 -> Word64
encodeTimeToWord64 (Time a) = encodeTimeSpanToWord64 a
{-# INLINE encodeTimeToWord64 #-}

decodeTime :: MonadGet m => m (Time Int64)
decodeTime  = Time <$!> decodeTimeSpan
{-# INLINE decodeTime #-}

castTime :: NumCast a b => Time a -> Time b
castTime (Time a) = Time $ castTimeSpan a
{-# INLINE castTime #-}

maybeCastTime :: MaybeNumCast a b => Time a -> Maybe (Time b)
maybeCastTime (Time a) = Time <$!> maybeCastTimeSpan a
{-# INLINE maybeCastTime #-}

ceilingTime :: RealFrac a => Integral b => Time a -> Time b
ceilingTime (Time a) = Time (ceilingTimeSpan a)
{-# INLINE ceilingTime #-}

floorTime :: RealFrac a => Integral b => Time a -> Time b
floorTime (Time a) = Time (floorTimeSpan a)
{-# INLINE floorTime #-}

minTime :: Bounded a => Time a
minTime = minBound
{-# INLINE minTime #-}

maxTime :: Bounded a => Time a
maxTime = maxBound
{-# INLINE maxTime #-}

-- -------------------------------------------------------------------------- --
-- Time Span Values

microsecond :: Num a => TimeSpan a
microsecond = TimeSpan 1
{-# INLINE microsecond #-}

millisecond :: Num a => TimeSpan a
millisecond = TimeSpan kilo
{-# INLINE millisecond #-}

second :: Num a => TimeSpan a
second = TimeSpan mega
{-# INLINE second #-}

minute :: Num a => TimeSpan a
minute = TimeSpan $ mega * 60
{-# INLINE minute #-}

hour :: Num a => TimeSpan a
hour = TimeSpan $ mega * 3600
{-# INLINE hour #-}

day :: Num a => TimeSpan a
day = TimeSpan $ mega * 24 * 3600
{-# INLINE day #-}

-- -------------------------------------------------------------------------- --
-- Seconds

newtype Seconds = Seconds Integer
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype (FromJSON, ToJSON)
    deriving newtype (Num, Enum, Real)

secondsToTimeSpan :: Num a => Seconds -> TimeSpan a
secondsToTimeSpan (Seconds s) = scaleTimeSpan s second
{-# INLINE secondsToTimeSpan #-}

-- | Assumes that the `TimeSpan` contains milliseconds.
--
timeSpanToSeconds :: Integral a => TimeSpan a -> Seconds
timeSpanToSeconds (TimeSpan ms) = Seconds . int $ ms `div` 1000000
{-# INLINE timeSpanToSeconds #-}

secondsToText :: Seconds -> T.Text
secondsToText (Seconds s) = sshow s
{-# INLINE secondsToText #-}

secondsFromText :: MonadThrow m => T.Text -> m Seconds
secondsFromText = fmap Seconds . treadM
{-# INLINE secondsFromText #-}

instance HasTextRepresentation Seconds where
    toText = secondsToText
    {-# INLINE toText #-}
    fromText = secondsFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Arbitrary Instances

instance Arbitrary a => Arbitrary (Time a) where
    arbitrary = Time <$> arbitrary

instance Arbitrary a => Arbitrary (TimeSpan a) where
    arbitrary = TimeSpan <$> arbitrary

instance Arbitrary Seconds where
    arbitrary = int <$> (arbitrary :: Gen Integer)
