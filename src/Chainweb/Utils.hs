{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Utils
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Utils
(
-- * SI unit prefixes
  milli
, micro
, nano
, pico
, femto
, kilo
, mega
, giga
, tera
, peta
, exa

-- * Misc
, int
, (==>)
, keySet
, minimumsOf
, minimumsByOf
, leadingZeros
, maxBy
, minBy

-- * Encoding and Serialization
, EncodingException(..)
, sshow
, runGet
, runGetEither
, encodeToText
, encodeB64Text
, decodeB64Text

-- * Error Handling
, Expected(..)
, Actual(..)
, (≡?)
, check
, fromMaybeM
, (???)
, fromEitherM
) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor
import Data.Bits
import Data.Bytes.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Monoid (Endo)
import Data.Serialize.Get (Get)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

import GHC.Generics

import Numeric.Natural


-- -------------------------------------------------------------------------- --
-- SI unit prefixes

milli, micro, nano, pico, femto :: Fractional a => a
milli = 10 ^^ (-3 :: Int)
micro = 10 ^^ (-6 :: Int)
nano = 10 ^^ (-9 :: Int)
pico = 10 ^^ (-12 :: Int)
femto = 10 ^^ (-15 :: Int)

kilo, mega, giga, tera, peta, exa :: Num a => a
kilo = 10 ^ (3 :: Int)
mega = 10 ^ (6 :: Int)
giga = 10 ^ (9 :: Int)
tera = 10 ^ (12 :: Int)
peta = 10 ^ (15 :: Int)
exa = 10 ^ (18 :: Int)

-- -------------------------------------------------------------------------- --
-- Misc

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

(==>) :: Bool -> Bool -> Bool
a ==> b = not a && b
infixr 1 ==>
{-# INLINE (==>) #-}

keySet :: HM.HashMap a b -> HS.HashSet a
keySet = HS.fromMap . set each ()
{-# INLINE keySet #-}

minimumsOf :: Ord a => Getting (Endo (Endo [a])) s a -> s -> [a]
minimumsOf l = foldlOf' l mf []
  where
    mf [] !y = [y]
    mf x@(h:_) !y = case compare h y of
        EQ -> y:x
        LT -> [y]
        GT -> x
{-# INLINE minimumsOf #-}

minimumsByOf :: Getting (Endo (Endo [a])) s a -> (a -> a -> Ordering) -> s -> [a]
minimumsByOf l cmp = foldlOf' l mf []
  where
    mf [] !y = [y]
    mf x@(h:_) !y = case cmp h y of
        EQ -> y:x
        LT -> [y]
        GT -> x
{-# INLINE minimumsByOf #-}

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp a b = case cmp a b of
    LT -> b
    _ -> a
{-# INLINE maxBy #-}

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp a b = case cmp a b of
    GT -> b
    _ -> a
{-# INLINE minBy #-}

-- -------------------------------------------------------------------------- --
-- Encodings and Serialization

data EncodingException where
    EncodeException :: T.Text -> EncodingException
    DecodeException :: T.Text -> EncodingException
    Base64DecodeException :: T.Text -> EncodingException
    ItemCountDecodeException :: Expected Natural -> Actual Natural -> EncodingException
    deriving (Show, Eq, Ord, Generic)

instance Exception EncodingException

runGet :: MonadThrow m => Get a -> B.ByteString -> m a
runGet g = fromEitherM . runGetEither g
{-# INLINE runGet #-}

runGetEither :: Get a -> B.ByteString -> Either EncodingException a
runGetEither g = first (DecodeException . T.pack) . runGetS g
{-# INLINE runGetEither #-}

sshow :: Show a => IsString b => a -> b
sshow = fromString . show
{-# INLINE sshow #-}

encodeToText :: ToJSON a => a -> T.Text
encodeToText = TL.toStrict . encodeToLazyText
{-# INLINE encodeToText #-}

decodeB64Text :: MonadThrow m => T.Text -> m B.ByteString
decodeB64Text = fromEitherM
    . first (Base64DecodeException . T.pack)
    . B64.decode
    . T.encodeUtf8
{-# INLINE decodeB64Text #-}

encodeB64Text :: B.ByteString -> T.Text
encodeB64Text = T.decodeUtf8 . B64.encode
{-# INLINE encodeB64Text #-}

-- -------------------------------------------------------------------------- --
-- Error Handling

newtype Expected a = Expected { getExpected :: a }
    deriving (Show, Eq, Ord, Generic, Functor)

newtype Actual a = Actual { getActual :: a }
    deriving (Show, Eq, Ord, Generic, Functor)

(≡?) :: Eq a => Expected a -> Actual a -> Bool
(≡?) (Expected a) (Actual b) = a == b

check
    :: MonadThrow m
    => Eq a
    => Exception e
    => (Expected a -> Actual a -> e)
    -> Expected a
    -> Actual a
    -> m a
check e a b = do
    unless (a ≡? b) $ throwM (e a b)
    return (getActual b)

fromMaybeM :: MonadThrow m => Exception e => e -> Maybe a -> m a
fromMaybeM e = maybe (throwM e) return
{-# INLINE fromMaybeM #-}

(???) :: MonadThrow m => Exception e => Maybe a -> e -> m a
(???) = flip fromMaybeM
infixl 0 ???
{-# INLINE (???) #-}

fromEitherM :: MonadThrow m => Exception e => Either e a -> m a
fromEitherM = either throwM return
{-# INLINE fromEitherM #-}

-- -------------------------------------------------------------------------- --
-- Count leading zeros of a bytestring

leadingZeros :: B.ByteString -> Natural
leadingZeros b = int (B.length x) * 8 + case B.uncons y of
    Just (h, _) -> int $ countLeadingZeros h
    Nothing -> 0
  where
    (x, y) = B.span (== 0x00) b

