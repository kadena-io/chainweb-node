{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
, len
, (==>)
, keySet
, minimumsOf
, minimumsByOf
, leadingZeros
, maxBy
, minBy
, (&)

-- * Encoding and Serialization
, EncodingException(..)

-- ** Binary
, runGet
, runGetEither

-- ** Text
, sshow
, tread
, treadM
, HasTextRepresentation(..)
, eitherFromText

-- ** Base64
, encodeB64Text
, decodeB64Text
, encodeB64UrlText
, decodeB64UrlText
, encodeB64UrlNoPaddingText
, decodeB64UrlNoPaddingText

-- ** JSON
, encodeToText
, decodeOrThrow
, decodeStrictOrThrow
, decodeFileStrictOrThrow
, decodeOrThrow'
, decodeStrictOrThrow'
, decodeFileStrictOrThrow'
, parseJsonFromText

-- * Error Handling
, Expected(..)
, Actual(..)
, unexpectedMsg
, (==?)
, check
, fromMaybeM
, (???)
, fromEitherM

-- * Command Line Options
, OptionParser
, prefixLong
, suffixHelp
, textOption
) where

import Configuration.Utils

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor
import Data.Bits
import Data.Bytes.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Monoid (Endo)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup hiding (option)
#endif
import Data.Serialize.Get (Get)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

import GHC.Generics

import Numeric.Natural

import qualified Options.Applicative as O

import Text.Read (readEither)

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

len :: Integral a => [b] -> a
len = int . length
{-# INLINE len #-}

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
-- * Encodings and Serialization

-- -------------------------------------------------------------------------- --
-- ** Binary

data EncodingException where
    EncodeException :: T.Text -> EncodingException
    DecodeException :: T.Text -> EncodingException
    Base64DecodeException :: T.Text -> EncodingException
    ItemCountDecodeException :: Expected Natural -> Actual Natural -> EncodingException
    TextFormatException :: T.Text -> EncodingException
    JsonDecodeException :: T.Text -> EncodingException
    deriving (Show, Eq, Ord, Generic)

instance Exception EncodingException

runGet :: MonadThrow m => Get a -> B.ByteString -> m a
runGet g = fromEitherM . runGetEither g
{-# INLINE runGet #-}

runGetEither :: Get a -> B.ByteString -> Either EncodingException a
runGetEither g = first (DecodeException . T.pack) . runGetS g
{-# INLINE runGetEither #-}

-- -------------------------------------------------------------------------- --
-- ** Text

sshow :: Show a => IsString b => a -> b
sshow = fromString . show
{-# INLINE sshow #-}

tread :: Read a => T.Text -> Either T.Text a
tread = first T.pack . readEither . T.unpack
{-# INLINE tread #-}

-- | Throws 'TextFormatException' on failure.
--
treadM :: MonadThrow m => Read a => T.Text -> m a
treadM = fromEitherM . first TextFormatException . tread
{-# INLINE treadM #-}

class HasTextRepresentation a where
    toText :: a -> T.Text
    fromText :: MonadThrow m => T.Text -> m a

instance HasTextRepresentation T.Text where
    toText = id
    {-# INLINE toText #-}
    fromText = return
    {-# INLINE fromText #-}

instance HasTextRepresentation [Char] where
    toText = T.pack
    {-# INLINE toText #-}
    fromText = return . T.unpack
    {-# INLINE fromText #-}

eitherFromText
    :: HasTextRepresentation a
    => T.Text
    -> Either String a
eitherFromText = either f return . fromText
  where
    f e = Left $ case fromException e of
        Just (TextFormatException err) -> T.unpack err
        _ -> show e
{-# INLINE eitherFromText #-}

-- -------------------------------------------------------------------------- --
-- ** Base64

decodeB64Text :: MonadThrow m => T.Text -> m B.ByteString
decodeB64Text = fromEitherM
    . first (Base64DecodeException . T.pack)
    . B64.decode
    . T.encodeUtf8
{-# INLINE decodeB64Text #-}

encodeB64Text :: B.ByteString -> T.Text
encodeB64Text = T.decodeUtf8 . B64.encode
{-# INLINE encodeB64Text #-}

decodeB64UrlText :: MonadThrow m => T.Text -> m B.ByteString
decodeB64UrlText = fromEitherM
    . first (Base64DecodeException . T.pack)
    . B64U.decode
    . T.encodeUtf8
{-# INLINE decodeB64UrlText #-}

encodeB64UrlText :: B.ByteString -> T.Text
encodeB64UrlText = T.decodeUtf8 . B64U.encode
{-# INLINE encodeB64UrlText #-}

decodeB64UrlNoPaddingText :: MonadThrow m => T.Text -> m B.ByteString
decodeB64UrlNoPaddingText = fromEitherM
    . first (Base64DecodeException . T.pack)
    . B64U.decode
    . T.encodeUtf8
    . pad
  where
    pad t = let s = T.length t `mod` 4 in t <> T.replicate ((4 - s) `mod` 4) "="
{-# INLINE decodeB64UrlNoPaddingText #-}

encodeB64UrlNoPaddingText :: B.ByteString -> T.Text
encodeB64UrlNoPaddingText = T.dropWhileEnd (== '=') . T.decodeUtf8 . B64U.encode
{-# INLINE encodeB64UrlNoPaddingText #-}

-- -------------------------------------------------------------------------- --
-- ** JSON

encodeToText :: ToJSON a => a -> T.Text
encodeToText = TL.toStrict . encodeToLazyText
{-# INLINE encodeToText #-}

decodeStrictOrThrow :: MonadThrow m => FromJSON a => B.ByteString -> m a
decodeStrictOrThrow = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecodeStrict
{-# INLINE decodeStrictOrThrow #-}

decodeOrThrow :: MonadThrow m => FromJSON a => BL.ByteString -> m a
decodeOrThrow = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecode
{-# INLINE decodeOrThrow #-}

decodeFileStrictOrThrow :: MonadIO m => MonadThrow m => FromJSON a => FilePath -> m a
decodeFileStrictOrThrow = fromEitherM
    <=< return . first (JsonDecodeException . T.pack)
    <=< liftIO . eitherDecodeFileStrict
{-# INLINE decodeFileStrictOrThrow #-}

decodeStrictOrThrow' :: MonadThrow m => FromJSON a => B.ByteString -> m a
decodeStrictOrThrow' = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecodeStrict
{-# INLINE decodeStrictOrThrow' #-}

decodeOrThrow' :: MonadThrow m => FromJSON a => BL.ByteString -> m a
decodeOrThrow' = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecode'
{-# INLINE decodeOrThrow' #-}

decodeFileStrictOrThrow'
    :: forall a m
    . MonadIO m
    => MonadThrow m
    => FromJSON a
    => FilePath
    -> m a
decodeFileStrictOrThrow' = fromEitherM
    <=< return . first (JsonDecodeException . T.pack)
    <=< liftIO . eitherDecodeFileStrict'
{-# INLINE decodeFileStrictOrThrow' #-}

parseJsonFromText
    :: HasTextRepresentation a
    => String
    -> Value
    -> Aeson.Parser a
parseJsonFromText l = withText l $ either fail return . eitherFromText
{-# INLINE parseJsonFromText #-}

-- -------------------------------------------------------------------------- --
-- Option Parsing

type OptionParser a = O.Parser a

prefixLong :: HasName f => Maybe String -> String -> Mod f a
prefixLong prefix l = long $ maybe "" ("-" <>) prefix <> l

suffixHelp :: Maybe String -> String -> Mod f a
suffixHelp suffix l = help $ l <> maybe "" (" for " <>) suffix

textOption :: HasTextRepresentation a => Mod OptionFields a -> O.Parser a
textOption = option (eitherReader $ first show . fromText . T.pack)

-- -------------------------------------------------------------------------- --
-- Error Handling

newtype Expected a = Expected { getExpected :: a }
    deriving (Show, Eq, Ord, Generic, Functor)

newtype Actual a = Actual { getActual :: a }
    deriving (Show, Eq, Ord, Generic, Functor)

unexpectedMsg :: Show a => T.Text -> Expected a -> Actual a -> T.Text
unexpectedMsg msg expected actual = msg
    <> ", expected: " <> sshow (getExpected expected)
    <> ", actual: " <> sshow (getActual actual)

(==?) :: Eq a => Expected a -> Actual a -> Bool
(==?) (Expected a) (Actual b) = a == b

check
    :: MonadThrow m
    => Eq a
    => Exception e
    => (Expected a -> Actual a -> e)
    -> Expected a
    -> Actual a
    -> m a
check e a b = do
    unless (a ==? b) $ throwM (e a b)
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

