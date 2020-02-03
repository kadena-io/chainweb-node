{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
, floorTimeBy
, getCurrentTimeIntegral
, epoch
, timeMicrosQQ

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

-- * Micros
, Micros(..)
, microsToTimeSpan
, timeSpanToMicros
, microsToText
, microsFromText

-- * Math, constants
, add
, diff
, kilo
, mega
) where

import Control.DeepSeq
import Control.Monad ((<$!>))
import Control.Monad.Catch

import Data.Aeson (FromJSON, ToJSON)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Signed
import Data.Data
import Data.Hashable (Hashable)
import Data.Int
import qualified Data.Memory.Endian as BA
import Data.Ratio
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word

import GHC.Generics

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift)

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
newtype TimeSpan a = TimeSpan a
    deriving (Show, Eq, Ord, Generic, Data)
    deriving anyclass (Hashable, NFData, Lift)
    deriving newtype
        ( AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , AdditiveGroup, FractionalVectorSpace
        , Enum, Bounded
        , ToJSON, FromJSON
        )

encodeTimeSpan :: MonadPut m => TimeSpan Micros -> m ()
encodeTimeSpan (TimeSpan (Micros a)) = putWord64le $ unsigned a
{-# INLINE encodeTimeSpan #-}

encodeTimeSpanToWord64 :: TimeSpan Micros -> Word64
encodeTimeSpanToWord64 (TimeSpan (Micros a)) = BA.unLE . BA.toLE $ unsigned a
{-# INLINE encodeTimeSpanToWord64 #-}

decodeTimeSpan :: MonadGet m => m (TimeSpan Micros)
decodeTimeSpan = TimeSpan . Micros . signed <$!> getWord64le
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
    deriving (Show, Eq, Ord, Generic, Data)
    deriving anyclass (Hashable, NFData, Lift)
    deriving newtype (Enum, Bounded, ToJSON, FromJSON)

instance AdditiveGroup (TimeSpan a) => LeftTorsor (Time a) where
    type Diff (Time a) = TimeSpan a
    add s (Time t) = Time (s `plus` t)
    diff (Time t₁) (Time t₂) = t₁ `minus` t₂
    {-# INLINE add #-}
    {-# INLINE diff #-}

epoch :: Num a => Time a
epoch = Time (TimeSpan 0)
{-# INLINE epoch #-}

timeMicrosQQ :: QuasiQuoter
timeMicrosQQ = QuasiQuoter
    { quoteExp   = posixExp utcIso8601
    , quotePat   = const $ error "No quotePat defined for timeMicrosQQ"
    , quoteType  = const $ error "No quoteType defined for timeMicrosQQ"
    , quoteDec   = const $ error "No quoteDec defined for timeMicrosQQ"
    }
  where
    utcIso8601 = "%Y-%m-%dT%H:%M:%S%Q"
    posixExp :: String -> String -> ExpQ
    posixExp format input =
        let u = parseTimeOrError True defaultTimeLocale format input :: UTCTime
            p = utcTimeToPOSIXSeconds u
            t = Time $ TimeSpan $ round $ p * 1000000 :: Time Micros
        in t `seq` [| t |]

-- | Adhering to `Time`, this is the current number of microseconds since the
-- epoch.
--
getCurrentTimeIntegral :: Integral a => IO (Time a)
getCurrentTimeIntegral = do
    -- returns POSIX seconds with picosecond precision
    t <- getPOSIXTime
    return $! Time $! TimeSpan $! round $ t * 1000000

encodeTime :: MonadPut m => Time Micros -> m ()
encodeTime (Time a) = encodeTimeSpan a
{-# INLINE encodeTime #-}

encodeTimeToWord64 :: Time Micros -> Word64
encodeTimeToWord64 (Time a) = encodeTimeSpanToWord64 a
{-# INLINE encodeTimeToWord64 #-}

decodeTime :: MonadGet m => m (Time Micros)
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

floorTimeBy :: Integral a => TimeSpan a -> Time a -> Time a
floorTimeBy (TimeSpan a) (Time (TimeSpan b))
    = Time $ TimeSpan (floor (b % a) * a)
{-# INLINE floorTimeBy #-}

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
{-# INLINABLE secondsFromText #-}

instance HasTextRepresentation Seconds where
    toText = secondsToText
    {-# INLINE toText #-}
    fromText = secondsFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Microseconds

-- | Will last for around ~300,000 years after the Linux epoch.
--
newtype Micros = Micros Int64
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Data)
    deriving anyclass (Hashable, NFData)
    deriving newtype (Num, Integral, Real, AdditiveGroup, AdditiveMonoid, AdditiveSemigroup)
    deriving newtype (Arbitrary, ToJSON, FromJSON)

microsToTimeSpan :: Num a => Micros -> TimeSpan a
microsToTimeSpan (Micros us) = scaleTimeSpan us microsecond
{-# INLINE microsToTimeSpan #-}

-- | Assumes that the `TimeSpan` contains milliseconds.
--
timeSpanToMicros :: Integral a => TimeSpan a -> Micros
timeSpanToMicros (TimeSpan ms) = Micros . int $ ms * 1000
{-# INLINE timeSpanToMicros #-}

microsToText :: Micros -> T.Text
microsToText (Micros us) = sshow us
{-# INLINE microsToText #-}

microsFromText :: MonadThrow m => T.Text -> m Micros
microsFromText = fmap Micros . treadM
{-# INLINABLE microsFromText #-}

instance HasTextRepresentation Micros where
    toText = microsToText
    {-# INLINE toText #-}
    fromText = microsFromText
    {-# INLINABLE fromText #-}

-- -------------------------------------------------------------------------- --
-- Arbitrary Instances

instance Arbitrary a => Arbitrary (Time a) where
    arbitrary = Time <$> arbitrary

instance Arbitrary a => Arbitrary (TimeSpan a) where
    arbitrary = TimeSpan <$> arbitrary

instance Arbitrary Seconds where
    arbitrary = int <$> (arbitrary :: Gen Integer)
