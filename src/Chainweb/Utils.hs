{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Module: Chainweb.Utils
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Utilities used by various modules of the chainweb package
--
module Chainweb.Utils
(
-- * Unit Prefixes
  deci
, centi
, milli
, micro
, nano
, pico
, femto
, atto
, zepto
, yocto

, deka
, hecto
, kilo
, mega
, giga
, tera
, peta
, exa
, zetta
, yotta

, kibi
, mebi
, gibi
, tebi
, pebi
, exbi

-- * Misc
, int
, len
, (==>)
, keySet
, tabulateHashMap
, maxBy
, unlessM
, whenM
, ebool_
, alignWithV
, (&)
, IxedGet(..)
, ix'
, minusOrZero
, interleaveIO
, mutableVectorFromList

-- * Encoding and Serialization
, EncodingException(..)

-- ** Codecs
, Codec(..)

-- ** Text
, sshow
, tread
, treadM
, HasTextRepresentation(..)
, eitherFromText
, unsafeFromText
, parseM
, parseText

-- ** Base64
, encodeB64Text
, decodeB64Text
, encodeB64UrlText
, decodeB64UrlText
, encodeB64UrlNoPaddingText
, b64UrlNoPaddingTextEncoding
, decodeB64UrlNoPaddingText

-- ** JSON
, encodeToText
, encodeToByteString
, decodeOrThrow
, decodeStrictOrThrow
, decodeFileStrictOrThrow
, decodeOrThrow'
, decodeStrictOrThrow'
, decodeFileStrictOrThrow'
, parseJsonFromText

-- ** Cassava (CSV)
, CsvDecimal(..)

-- * Error Handling
, Expected(..)
, Actual(..)
, unexpectedMsg
, (==?)
, check
, fromMaybeM
, fromJuste
, (???)
, fromEitherM
, InternalInvariantViolation(..)

-- ** Synchronous Exceptions
, catchSynchronous
, catchAllSynchronous
, trySynchronous
, tryAllSynchronous
, runForever
, runForeverThrottled

-- * Command Line Options
, OptionParser
, prefixLong
, suffixHelp
, textReader
, textOption
, jsonOption -- rexport from Configuration.Utils

-- * Configuration to Enable/Disable Components

, EnableConfig(..)
, enableConfigConfig
, enableConfigEnabled
, defaultEnableConfig
, pEnableConfig
, enabledConfig
, validateEnableConfig

-- * Configuration Exception
, ConfigurationException(..)

-- * Streaming
, streamToHashSet
, streamToHashSet_
, nub
, timeoutStream
, reverseStream
, foldChunksM
, foldChunksM_
, progress

-- * Type Level
, symbolText

-- * Resource Management
, concurrentWith
, withLink
, concurrentlies
, concurrentlies_

-- * Tuples
, thd

-- * Strict Tuples
, T2(..)
, T3(..)
, sfst
, ssnd
, scurry
, suncurry
, suncurry3
, uncurry3
, _T2
, _T3

-- * Approximate thread delays
, approximately
, approximateThreadDelay

-- * TLS Manager with connection timeout settings
, manager
, unsafeManager
, unsafeManagerWithSettings
, setManagerRequestTimeout

-- * SockAddr from network package
, showIpv4
, showIpv6
, sockAddrJson

-- * Debugging Tools
, estimateBlockHeight
, parseUtcTime

) where

import Configuration.Utils hiding (Error, Lens)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.TokenBucket
import Control.DeepSeq
import Control.Exception (SomeAsyncException(..), evaluate)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch hiding (bracket)
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader as Reader

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV
import Data.Decimal
import Data.Functor.Of
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Proxy
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.These (These(..))
import Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word

import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Network.Connection as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.Socket

import Numeric.Natural

import qualified Options.Applicative as O

import qualified Streaming as S (concats, effect, inspect)
import qualified Streaming.Prelude as S

import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import System.LogLevel
import qualified System.Random.MWC as Prob
import qualified System.Random.MWC.Probability as Prob
import System.Timeout

import Text.Printf (printf)
import Text.Read (readEither)

-- -------------------------------------------------------------------------- --
-- SI unit prefixes

-- | cf. https://www.nist.gov/pml/owm/metric-si-prefixes
--
deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto :: Fractional a => a
deci = 10 ^^ (-1 :: Int)
centi = 10 ^^ (-2 :: Int)
milli = 10 ^^ (-3 :: Int)
micro = 10 ^^ (-6 :: Int)
nano = 10 ^^ (-9 :: Int)
pico = 10 ^^ (-12 :: Int)
femto = 10 ^^ (-15 :: Int)
atto = 10 ^^ (-18 :: Int)
zepto = 10 ^^ (-21 :: Int)
yocto = 10 ^^ (-24 :: Int)

-- | cf. https://www.nist.gov/pml/owm/metric-si-prefixes
--
deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta :: Num a => a
deka = 10 ^ (1 :: Int)
hecto = 10 ^ (2 :: Int)
kilo = 10 ^ (3 :: Int)
mega = 10 ^ (6 :: Int)
giga = 10 ^ (9 :: Int)
tera = 10 ^ (12 :: Int)
peta = 10 ^ (15 :: Int)
exa = 10 ^ (18 :: Int)
zetta = 10 ^ (21 :: Int)
yotta = 10 ^ (24 :: Int)

-- | IEC 60027-X unit prefixes for binary bases.
--
-- cf. https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-d-bibliography#05
--
kibi, mebi, gibi, tebi, pebi, exbi:: Num a => a
kibi = 1024 ^ (1 :: Int)
mebi = 1024 ^ (2 :: Int)
gibi = 1024 ^ (3 :: Int)
tebi = 1024 ^ (4 :: Int)
pebi = 1024 ^ (5 :: Int)
exbi = 1024 ^ (6 :: Int)

-- -------------------------------------------------------------------------- --
-- Misc

-- | A shorter alias for 'fromIntegral'
--
int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

-- | A generalization of 'length' that returns any type that is an instance of
-- 'Integral'.
--
len :: Integral a => [b] -> a
len = int . length
{-# INLINE len #-}

-- | Boolean implication operator.
--
(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b
infixr 1 ==>
{-# INLINE (==>) #-}

-- | The set of keys of a 'HM.HashMap'.
--
keySet :: HM.HashMap a b -> HS.HashSet a
keySet = HS.fromMap . set each ()
{-# INLINE keySet #-}

-- | A HashMap memoizing a function over a finite input type.
--
tabulateHashMap :: (Enum a, Bounded a, Hashable a, Eq a) => (a -> b) -> HM.HashMap a b
tabulateHashMap f = HM.fromList [ (a, f a) | a <- [minBound..maxBound] ]

-- | The maximum of two value by some comparision function.
--
maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp a b = case cmp a b of
    LT -> b
    _ -> a
{-# INLINE maxBy #-}

-- | A version of 'unless' with a monadic predicate.
--
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c a = c >>= flip unless a
{-# INLINE unlessM #-}

-- | A version of 'when' with a monadic predicate.
--
whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = c >>= flip when a
{-# INLINE whenM #-}

ebool_ :: e -> Bool -> Either e ()
ebool_ e = bool (Left e) (Right ())

-- | Elide 'semialign' import with this simple Vector-specialized version.
-- O(n)-ish -- O(min (m,n) + 2*max(m-n,n-m))
alignWithV :: (These a b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
alignWithV f a b = V.zipWith (\a' -> f . These a') a b <> case (V.length a,V.length b) of
  (la,lb) | la == lb -> mempty
          | la > lb -> V.map (f . This) $ V.drop lb a
          | otherwise -> V.map (f . That) $ V.drop la b

-- | Substraction that returns 0 when the second argument is larger than the
-- first. This can be in particular useful when substracting 'Natural' numbers.
-- The operator '-' would throw an 'Underflow' exception in this situation.
--
minusOrZero :: Ord a => Num a => a -> a -> a
minusOrZero a b = a - min a b
{-# INLINE minusOrZero #-}

-- | Analogous to `unsafeInterleaveIO` but doesn't hide the effect behind evaluation.
-- Careful; if the inner action throws an exception, it will never not throw that exception.
interleaveIO :: IO a -> IO (IO a)
interleaveIO act = evaluate <$> unsafeInterleaveIO act
{-# INLINE interleaveIO #-}

-- | Equivalent to V.thaw . V.fromList but by inspection probably faster.
mutableVectorFromList
    :: PrimMonad m
    => [a]
    -> m (MV.MVector (PrimState m) a)
mutableVectorFromList as = do
    vec <- MV.unsafeNew (length as)
    forM_ (zip [0..] as) $ uncurry (MV.unsafeWrite vec)
    return vec
{-# inline mutableVectorFromList #-}

-- -------------------------------------------------------------------------- --
-- * Read only Ixed

-- | Provides a simple Fold lets you fold the value at a given key in a Map or
-- element at an ordinal position in a list or Seq.
--
-- This is a restricted version of 'Ixed' from the lens package that prevents
-- the value at the key from being modified.
--
class IxedGet a where

    -- | Provide a 'Fold' for a value at a given key.
    --
    ixg :: Index a -> Fold a (IxValue a)

    default ixg :: Ixed a => Index a -> Fold a (IxValue a)
    ixg i = ix i
    {-# INLINE ixg #-}

-- | A strict version of 'ix'. It requires a 'Monad' constraint on the context.
--
ix'
    :: forall s f
    . Monad f
    => Ixed s
    => Index s
    -> (IxValue s -> f (IxValue s))
    -> s
    -> f s
ix' i f = ix i (f >=> \ !r -> return r)
{-# INLINE ix' #-}

-- -------------------------------------------------------------------------- --
-- * Encodings and Serialization

-- -------------------------------------------------------------------------- --
-- ** Binary

-- | Exceptions that are thrown when encoding or decoding values.
--
data EncodingException where
    EncodeException :: T.Text -> EncodingException
    DecodeException :: T.Text -> EncodingException
    Base64DecodeException :: T.Text -> EncodingException
    ItemCountDecodeException :: Expected Natural -> Actual Natural -> EncodingException
    TextFormatException :: T.Text -> EncodingException
    JsonDecodeException :: T.Text -> EncodingException
    X509CertificateDecodeException :: T.Text -> EncodingException
    X509KeyDecodeException :: T.Text -> EncodingException
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Exception EncodingException

-- -------------------------------------------------------------------------- --
-- ** Text

-- | Show a value as any type that is an instance of 'IsString'.
--
sshow :: Show a => IsString b => a -> b
sshow = fromString . show
{-# INLINE sshow #-}

-- | Read a value from a textual encoding using its 'Read' instance. Returns and
-- textual error message if the operation fails.
--
tread :: Read a => T.Text -> Either T.Text a
tread = first T.pack . readEither . T.unpack
{-# INLINE tread #-}

-- | Throws 'TextFormatException' on failure.
--
treadM :: MonadThrow m => Read a => T.Text -> m a
treadM = fromEitherM . first TextFormatException . tread
{-# INLINE treadM #-}

-- | Class of types that have an textual representation.
--
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

instance HasTextRepresentation Int where
    toText = sshow
    {-# INLINE toText #-}
    fromText = treadM
    {-# INLINE fromText #-}

instance HasTextRepresentation Integer where
    toText = sshow
    {-# INLINE toText #-}
    fromText = treadM
    {-# INLINE fromText #-}

instance HasTextRepresentation Word where
    toText = sshow
    {-# INLINE toText #-}
    fromText = treadM
    {-# INLINE fromText #-}

instance HasTextRepresentation Word64 where
    toText = sshow
    {-# INLINE toText #-}
    fromText = treadM
    {-# INLINE fromText #-}

instance HasTextRepresentation UTCTime where
    toText = T.pack . formatTime defaultTimeLocale iso8601DateTimeFormat
    {-# INLINE toText #-}

    fromText d = case parseTimeM False defaultTimeLocale fmt (T.unpack d) of
        Nothing -> throwM $ TextFormatException $ "failed to parse utc date " <> sshow d
        Just x -> return x
      where
        fmt = iso8601DateTimeFormat
    {-# INLINE fromText #-}

-- | Decode a value from its textual representation.
--
eitherFromText
    :: HasTextRepresentation a
    => T.Text
    -> Either String a
eitherFromText = either f return . fromText
  where
    f e = Left $ case fromException e of
        Just (TextFormatException err) -> T.unpack err
        _ -> displayException e
{-# INLINE eitherFromText #-}

-- | Unsafely decode a value rom its textual representation. It is an program
-- error if decoding fails.
--
unsafeFromText :: HasCallStack => HasTextRepresentation a => T.Text -> a
unsafeFromText = fromJuste . fromText
{-# INLINE unsafeFromText #-}

-- | Run a 'A.Parser' on a text input. All input must be consume by the parser.
-- A 'TextFormatException' is thrown if parsing fails.
--
parseM :: MonadThrow m => A.Parser a -> T.Text -> m a
parseM p = either (throwM . TextFormatException . T.pack) return
    . A.parseOnly (p <* A.endOfInput)
{-# INLINE parseM #-}

-- | A parser for types with an 'HasTextRepresentation' instance.
--
parseText :: HasTextRepresentation a => A.Parser T.Text -> A.Parser a
parseText p = either (fail . sshow) return . fromText =<< p
{-# INLINE parseText #-}

iso8601DateTimeFormat :: String
iso8601DateTimeFormat = iso8601DateFormat (Just "%H:%M:%SZ")
{-# INLINE iso8601DateTimeFormat #-}

-- -------------------------------------------------------------------------- --
-- ** Base64

-- | Decode a binary value from a textual base64 representation. A
-- 'Base64DecodeException' is thrown if the input is not a valid base64
-- encoding.
--
decodeB64Text :: MonadThrow m => T.Text -> m B.ByteString
decodeB64Text = fromEitherM
    . first (Base64DecodeException . T.pack)
    . B64.decode
    . T.encodeUtf8
{-# INLINE decodeB64Text #-}

-- | Encode a binary value to a textual base64 representation.
--
encodeB64Text :: B.ByteString -> T.Text
encodeB64Text = T.decodeUtf8 . B64.encode
{-# INLINE encodeB64Text #-}

-- | Decode a binary value from a textual base64-url representation. A
-- 'Base64DecodeException' is thrown if the input is not a valid base64-url
-- encoding.
--
decodeB64UrlText :: MonadThrow m => T.Text -> m B.ByteString
decodeB64UrlText = fromEitherM
    . first (Base64DecodeException . T.pack)
    . B64U.decode
    . T.encodeUtf8
{-# INLINE decodeB64UrlText #-}

-- | Encode a binary value to a textual base64-url representation.
--
encodeB64UrlText :: B.ByteString -> T.Text
encodeB64UrlText = T.decodeUtf8 . B64U.encode
{-# INLINE encodeB64UrlText #-}

-- | Decode a binary value from a textual base64-url without padding
-- representation. A 'Base64DecodeException' is thrown if the input is not a
-- valid base64-url without padding encoding.
--
decodeB64UrlNoPaddingText :: MonadThrow m => T.Text -> m B.ByteString
decodeB64UrlNoPaddingText = fromEitherM
    . first (Base64DecodeException . T.pack)
    . B64U.decode
    . T.encodeUtf8
    . pad
  where
    pad t = let s = T.length t `mod` 4 in t <> T.replicate ((4 - s) `mod` 4) "="
{-# INLINE decodeB64UrlNoPaddingText #-}

-- | Encode a binary value to a textual base64-url without padding
-- representation.
--
encodeB64UrlNoPaddingText :: B.ByteString -> T.Text
encodeB64UrlNoPaddingText = T.dropWhileEnd (== '=') . T.decodeUtf8 . B64U.encode
{-# INLINE encodeB64UrlNoPaddingText #-}

-- | Encode a binary value to a base64-url (without padding) JSON encoding.
--
b64UrlNoPaddingTextEncoding :: B.ByteString -> Encoding
b64UrlNoPaddingTextEncoding t =
    Aeson.unsafeToEncoding $ BB.char8 '\"' <> BB.byteString (B8.dropWhileEnd (== '=') $ B64U.encode t) <> BB.char8 '\"'

-- -------------------------------------------------------------------------- --
-- ** JSON

-- | Encode a value to a JSON text.
--
encodeToText :: ToJSON a => a -> T.Text
encodeToText = TL.toStrict . encodeToLazyText
{-# INLINE encodeToText #-}

-- | Encode a value to a strict 'B.ByteString'.
--
encodeToByteString :: ToJSON a => a -> B.ByteString
encodeToByteString = BL.toStrict . encode
{-# INLINE encodeToByteString #-}

-- | Decode a JSON value from a strict 'B.ByteString'. If decoding fails a
-- 'JsonDecodeException' is thrown.
--
decodeStrictOrThrow :: MonadThrow m => FromJSON a => B.ByteString -> m a
decodeStrictOrThrow = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecodeStrict
{-# INLINE decodeStrictOrThrow #-}

-- | Strictly decode a JSON value from a strict 'B.ByteString'. If decoding
-- fails a 'JsonDecodeException' is thrown.
--
decodeStrictOrThrow' :: MonadThrow m => FromJSON a => B.ByteString -> m a
decodeStrictOrThrow' = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecodeStrict'
{-# INLINE decodeStrictOrThrow' #-}

-- | Decode a JSON value from a lazy 'BL.ByteString'. If decoding fails a
-- 'JsonDecodeException' is thrown.
--
decodeOrThrow :: MonadThrow m => FromJSON a => BL.ByteString -> m a
decodeOrThrow = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecode
{-# INLINE decodeOrThrow #-}

-- | Strictly decode a JSON value from a lazy 'BL.ByteString'. If decoding fails
-- a 'JsonDecodeException' is thrown.
--
decodeOrThrow' :: MonadThrow m => FromJSON a => BL.ByteString -> m a
decodeOrThrow' = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecode'
{-# INLINE decodeOrThrow' #-}

-- | Decode a JSON value from the contents of a file. If decoding fails a
-- 'JsonDecodeException' is thrown.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
--
decodeFileStrictOrThrow :: MonadIO m => MonadThrow m => FromJSON a => FilePath -> m a
decodeFileStrictOrThrow = fromEitherM
    <=< return . first (JsonDecodeException . T.pack)
    <=< liftIO . eitherDecodeFileStrict
{-# INLINE decodeFileStrictOrThrow #-}

-- | Strictly decode a JSON value from the content of a file. If decoding fails
-- a 'JsonDecodeException' is thrown.
--
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

-- | A json parser for types with an instance of 'HasTextRepresentation'.
--
parseJsonFromText
    :: HasTextRepresentation a
    => String
    -> Value
    -> Aeson.Parser a
parseJsonFromText l = withText l $! either fail return . eitherFromText

-- -------------------------------------------------------------------------- --
-- ** Cassava (CSV)

newtype CsvDecimal = CsvDecimal { _csvDecimal :: Decimal }
    deriving newtype (Eq, Ord, Show, Read)

instance CSV.FromField CsvDecimal where
    parseField s = do
        cs <- either (fail . show) pure $ T.unpack <$> T.decodeUtf8' s
        either fail pure $ readEither cs
    {-# INLINE parseField #-}

-- -------------------------------------------------------------------------- --
-- Option Parsing

-- | Type of parsers for simple compandline options.
--
type OptionParser a = O.Parser a

-- | An option modifier that defines a long option name that may be prefixed.
--
-- @prefixLong (Just "component") "option"@ results in the command line option
-- @--component-option@.
--
prefixLong :: HasName f => Maybe String -> String -> Mod f a
prefixLong prefix l = long $ maybe "" (<> "-") prefix <> l

-- | An option modifier that defines an help message to which a suffix may be
-- appended.
--
-- @suffixHelp (Just "component") "Help message"@ result in the help text "Help
-- message for component"
--
suffixHelp :: Maybe String -> String -> Mod f a
suffixHelp suffix l = help $ l <> maybe "" (" for " <>) suffix

-- | A command line option reader for types with an instance of
-- 'HasTextRepresentation'.
--
textReader :: HasTextRepresentation a => ReadM a
textReader = eitherReader $ first show . fromText . T.pack

-- | A option parser for types with an instance of 'HasTextRepresentation'.
--
textOption :: HasTextRepresentation a => Mod OptionFields a -> O.Parser a
textOption = option textReader

-- -------------------------------------------------------------------------- --
-- Error Handling

-- | A newtype wrapper for tagger values as "expected" outcomes of some
-- computation.
--
newtype Expected a = Expected { getExpected :: a }
    deriving (Show, Eq, Ord, Generic, Functor)
    deriving newtype (NFData)

-- | A newtype wrapper for tagger values as "actual" outcomes of some
-- computation.
--
newtype Actual a = Actual { getActual :: a }
    deriving (Show, Eq, Ord, Generic, Functor)
    deriving newtype (NFData)

-- | A textual message that describes the 'Expected' and the 'Actual' outcome of
-- some computation.
--
unexpectedMsg :: Show a => T.Text -> Expected a -> Actual a -> T.Text
unexpectedMsg msg expected actual = msg
    <> ", expected: " <> sshow (getExpected expected)
    <> ", actual: " <> sshow (getActual actual)

-- | Compare an 'Expected' with an 'Actual' value.
--
(==?) :: Eq a => Expected a -> Actual a -> Bool
(==?) (Expected a) (Actual b) = a == b

-- | Compare an 'Expected' with an 'Actual' value and raise an exception if they
-- are different.
--
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
    return $! getActual b

-- | Throw an exception if a value is 'Nothing'.
--
fromMaybeM :: MonadThrow m => Exception e => e -> Maybe a -> m a
fromMaybeM e = maybe (throwM e) return
{-# INLINE fromMaybeM #-}

-- | Like `Data.Maybe.fromJust`, but carries forward the `HasCallStack`
-- constraint. "Juste" is French for "Just".
--
fromJuste :: HasCallStack => Maybe a -> a
fromJuste Nothing = error "Chainweb.Utils.fromJuste: Nothing"
fromJuste (Just a) = a

-- | Flipped infix version of 'fromMaybeM'
--
(???) :: MonadThrow m => Exception e => Maybe a -> e -> m a
(???) = flip fromMaybeM
infixl 0 ???
{-# INLINE (???) #-}

-- | Throw an exception if a value is a 'Left' result.
--
fromEitherM :: MonadThrow m => Exception e => Either e a -> m a
fromEitherM = either throwM return
{-# INLINE fromEitherM #-}

-- | An exeption to indicate an violation of an internal code invariants.
-- Throwing this type of exception means that there is a bug in the code.
--
newtype InternalInvariantViolation = InternalInvariantViolation T.Text
    deriving (Show)

instance Exception InternalInvariantViolation

-- | Catch and handle exception that are not contained in 'SomeAsyncException'.
--
catchSynchronous
    :: MonadCatch m
    => Exception e
    => NFData a
    => m a
    -> (e -> m a)
    -> m a
catchSynchronous a f = force <$> a `catches`
    [ Handler $ throwM @_ @SomeAsyncException
    , Handler f
    ]
{-# INLINE catchSynchronous #-}

-- | Catch all exceptions and return 'Left e' for all exceptions @e@ that are
-- not contained in 'SomeAsyncException'. Asynchronous exceptions are re-thrown.
--
trySynchronous
    :: MonadCatch m
    => Exception e
    => NFData a
    => m a
    -> m (Either e a)
trySynchronous a = (Right <$> a) `catches`
    [ Handler $ throwM @_ @SomeAsyncException
    , Handler $ pure . Left
    ]
{-# INLINE trySynchronous #-}

-- | A version of 'catchSynchronous' that doesn't discriminate on the type of
-- the exception but handles all exceptions in 'SomeExeption' that are not
-- contained in 'SomeAsyncException'.
--
catchAllSynchronous
    :: MonadCatch m
    => NFData a
    => m a
    -> (SomeException -> m a)
    -> m a
catchAllSynchronous = catchSynchronous
{-# INLINE catchAllSynchronous #-}

-- | A version of 'trySynchronous' that doesn't discriminate on the type of the
-- exception but handles all exceptions in 'SomeExeption' that are not contained
-- in 'SomeAsyncException'.
--
tryAllSynchronous
    :: MonadCatch m
    => NFData a
    => m a
    -> m (Either SomeException a)
tryAllSynchronous = trySynchronous
{-# INLINE tryAllSynchronous #-}

-- | Repeatedly run a computation 'forever' until it is stopped by receiving
-- 'SomeAsyncException'.
--
-- If the computation throws an exception that is not contained in
-- 'SomeAsyncException' an error message is logged and the function continues to
-- repeat the computation 'forever'.
--
-- An info-level message is logged when processing starts and stops.
--
runForever :: (LogLevel -> T.Text -> IO ()) -> T.Text -> IO () -> IO ()
runForever logfun name a = mask $ \umask -> do
    logfun Info $ "start " <> name
    let go = do
            forever (umask a) `catchAllSynchronous` \e ->
                logfun Error $ name <> " failed: " <> sshow e <> ". Restarting ..."
            go
    void go `finally` logfun Info (name <> " stopped")

-- | Repeatedly run a computation 'forever' at the given rate until it is
-- stopped by receiving 'SomeAsyncException'.
--
-- If the computation throws an exception that is not contained in
-- 'SomeAsyncException' an error message is logged and the function continues to
-- repeat the computation 'forever'.
--
-- An info-level message is logged when processing starts and stops.
--
runForeverThrottled
    :: (LogLevel -> T.Text -> IO ())
    -> T.Text
    -> Word64
        -- ^ burst size (number of calls)
    -> Word64
        -- ^ rate limit (usec / call)
    -> IO ()
    -> IO ()
runForeverThrottled logfun name burst rate a = mask $ \umask -> do
    tokenBucket <- newTokenBucket
    logfun Info $ "start " <> name
    let runThrottled = tokenBucketWait tokenBucket burst rate >> a
        go = do
            forever (umask runThrottled) `catchAllSynchronous` \e ->
                logfun Error $ name <> " failed: " <> sshow e <> ". Restarting ..."
            go
    void go `finally` logfun Info (name <> " stopped")

-- -------------------------------------------------------------------------- --
-- Configuration wrapper to enable and disable components

-- | Configuration wrapper to enable and disable components
--
data EnableConfig a = EnableConfig
    { _enableConfigEnabled :: !Bool
        -- ^ A flag that indicates whether the component is enabled.
    , _enableConfigConfig :: !a
        -- ^ The configuration value of the component.
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''EnableConfig

-- | The default is that the configured component is enabled.
--
defaultEnableConfig :: a -> EnableConfig a
defaultEnableConfig a = EnableConfig
    { _enableConfigEnabled = True
    , _enableConfigConfig = a
    }

enableConfigProperties :: ToJSON a => KeyValue e kv => EnableConfig a -> [kv]
enableConfigProperties o =
    [ "enabled" .= _enableConfigEnabled o
    , "configuration" .= _enableConfigConfig o
    ]
{-# INLINE enableConfigProperties #-}

instance ToJSON a => ToJSON (EnableConfig a) where
    toJSON = object . enableConfigProperties
    toEncoding = pairs . mconcat . enableConfigProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON (a -> a) => FromJSON (EnableConfig a -> EnableConfig a) where
    parseJSON = withObject "EnableConfig" $ \o -> id
        <$< enableConfigEnabled ..: "enabled" % o
        <*< enableConfigConfig %.: "configuration" % o
    {-# INLINE parseJSON #-}

validateEnableConfig :: ConfigValidation a l -> ConfigValidation (EnableConfig a) l
validateEnableConfig v c = when (_enableConfigEnabled c) $ v (_enableConfigConfig c)

-- | Command line parser for the configuration of a component that can be
-- enabled or disabled.
--
pEnableConfig :: String -> MParser a -> MParser (EnableConfig a)
pEnableConfig compName pConfig = id
    <$< enableConfigEnabled .:: enableDisableFlag
        % long compName
        <> help ("whether " <> compName <> " is enabled or disabled")
    <*< enableConfigConfig %:: pConfig

-- | Access wrapped config as `Maybe` if enabled.
enabledConfig :: EnableConfig a -> Maybe a
enabledConfig (EnableConfig e a) = if e then Just a else Nothing

-- -------------------------------------------------------------------------- --
-- Configuration Validation

-- | Exeption that is thrown when a configuration value is invalid.
--
newtype ConfigurationException = ConfigurationException T.Text
    deriving (Show, Eq, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)

instance Exception ConfigurationException

-- -------------------------------------------------------------------------- --
-- Streaming Utilities

-- | Add a timeout to each step of a stream.
--
-- If a step takes longer than the timeout processing of the stream is
-- terminated and 'Nothing' is returned. After processing all items of the
-- stream without a timeout 'Just' the result of the stream is returned.
--
timeoutStream
    :: Int
    -> S.Stream (Of a) IO r
    -> S.Stream (Of a) IO (Maybe r)
timeoutStream msecs = go
  where
    go s = lift (timeout msecs (S.next s)) >>= \case
        Nothing -> return Nothing
        Just (Left r) -> return (Just r)
        Just (Right (a, s')) -> S.yield a >> go s'

-- | Drop successive equal items from a stream.
--
-- @
-- S.toList_ (nub (S.each [1,1,2,2,3,4,4,4])) == [1,2,3,4]
-- @
--
nub :: Monad m => Eq a => S.Stream (Of a) m r -> S.Stream (Of a) m r
nub = S.concats . S.maps (S.drained . S.splitAt 1) . S.group

-- | Fold the items of a stream into a 'HS.HashSet'.
--
streamToHashSet
    :: Monad m
    => Eq a
    => Hashable a
    => S.Stream (Of a) m r
    -> m (Of (HS.HashSet a) r)
streamToHashSet = fmap (first HS.fromList) . S.toList

-- | Fold the items of a stream into a 'HS.HashSet' and discard the result of
-- the stream.
--
streamToHashSet_
    :: Monad m
    => Eq a
    => Hashable a
    => S.Stream (Of a) m r
    -> m (HS.HashSet a)
streamToHashSet_ = fmap HS.fromList . S.toList_

-- | This function reverses the order of the items in a stream. In order to due
-- so it store all items of the stream in a memory buffer before continuing to
-- stream starting at the former end of the stream.
--
-- /THIS FUNCTION BREAKS STREAMING! USE WITH CARE!/
--
-- The function 'getBranch' returns the keys of the branch in the order such
-- that an key of an entry is returned /before/ the keys of the dependencies of
-- the entry are returned.
--
-- Public 'ChainDb' API functions require that items are returned in an order
-- such that an item is returned /after/ all dependencies of the item are
-- returned.
--
-- Storing the complete result of a stream query in memory is problematic for
-- streams of unbounded length. It it is particularly problematic in a server
-- application. Beside of potential long latencies and GC issue overhead it can
-- also represent a DOS attack vulnerability. A server that uses this function
-- should use paging aggressively.
--
reverseStream :: Monad m => S.Stream (Of a) m () -> S.Stream (Of a) m ()
reverseStream = S.effect . S.fold_ (flip (:)) [] S.each

-- | Fold over a chunked stream
--
foldChunksM
    :: Monad m
    => (forall b . t -> S.Stream f m b -> m (Of t b))
    -> t
    -> S.Stream (S.Stream f m) m a
    -> m (Of t a)
foldChunksM f = go
  where
    go seed s = S.inspect s >>= \case
        Left r -> return (seed S.:> r)
        Right chunk -> do
            (!seed' S.:> s') <- f seed chunk
            go seed' s'
{-# INLINE foldChunksM #-}

-- | Fold over a chunked stream
--
foldChunksM_
    :: Monad m
    => (forall b . t -> S.Stream f m b -> m (Of t b))
    -> t
    -> S.Stream (S.Stream f m) m a
    -> m t
foldChunksM_ f seed = fmap (fst . S.lazily) . foldChunksM f seed
{-# INLINE foldChunksM_ #-}

-- | Progress reporting for long-running streams. I calls an action
-- every @n@ streams items.
--
progress
    :: Monad m
    => Int
        -- ^ How often to report progress
    -> (Int -> a -> m ())
        -- ^ progress callback
    -> S.Stream (S.Of a) m r
    -> S.Stream (S.Of a) m r
progress n act s = s
    & S.zip (S.enumFrom 1)
    & S.mapM (\(!i, !a) -> a <$ when (i `rem` n == 0) (act i a))
{-# INLINE progress #-}

-- -------------------------------------------------------------------------- --
-- Misc

-- | A binary codec.
--
-- TODO: maybe use Put/Get ?
--
data Codec t = Codec
    { codecEncode :: t -> ByteString
    , codecDecode :: ByteString -> Either String t
    }

-- -------------------------------------------------------------------------- --
-- Typelevel

-- | Return the value of a type level symbol as a value of a type that is an
-- instance of 'IsString'.
--
symbolText :: forall s a . KnownSymbol s => IsString a => a
symbolText = fromString $ symbolVal (Proxy @s)

-- -------------------------------------------------------------------------- --
-- Resource Management

-- | Bracket style resource managment uses CPS style which only supports
-- sequential componsition of different allocation functions.
--
-- This function is a hack that allows to allocate and deallocate several
-- resources concurrently and provide them to a single inner computation.
--
concurrentWith
    :: forall a b t d
    . Traversable t
    => (forall c . a -> (b -> IO c) -> IO c)
        -- concurrent resource allocation brackets. Given a value of type @a@,
        -- allocates a resource of type @b@, it provides the inner function with
        -- that value, and returns the result of the inner computation.
    -> (t b -> IO d)
        -- inner computation
    -> t a
        -- traversable that provides parameters to instantiate the resource
        -- allocation bracktes.
        --
        -- The value must be finite and is traversed twiced!
        --
    -> IO d
concurrentWith alloc inner params = do
    doneVar <- newEmptyMVar
    paramsWithVar <- traverse (\p -> (p,) <$> newEmptyMVar) params
    results <- concurrently (mapConcurrently (concAlloc doneVar) paramsWithVar) $ do
        resources <- traverse (takeMVar . snd) paramsWithVar
        result <- inner resources
        putMVar doneVar ()
        return result
    return $ snd results
  where
    concAlloc :: MVar () -> (a, MVar b) -> IO ()
    concAlloc doneVar (p, var) = alloc p $ \b -> do
        putMVar var b
        readMVar doneVar

-- | Run an async IO action, link it back to the main thread, and return
-- the async result
--
withLink :: forall a. IO a -> IO (Async a)
withLink act = do
  a <- async act
  link a
  return a

-- | Like `sequence` for IO but concurrent
concurrentlies :: forall a. [IO a] -> IO [a]
concurrentlies = runConcurrently . traverse Concurrently

-- | Like `sequence_` for IO but concurrent
concurrentlies_ :: forall a. [IO a] -> IO ()
concurrentlies_ = void . concurrentlies

-- -------------------------------------------------------------------------- --
-- Tuples

thd :: (a,b,c) -> c
thd (_,_,c) = c
{-# INLINE thd #-}

-- -------------------------------------------------------------------------- --
-- Strict Tuple

data T2 a b = T2 !a !b
    deriving (Show, Eq, Ord, Generic, NFData, Functor)

instance Bifunctor T2 where
    bimap f g (T2 a b) =  T2 (f a) (g b)
    {-# INLINE bimap #-}

data T3 a b c = T3 !a !b !c
    deriving (Show, Eq, Ord, Generic, NFData, Functor)

instance Bifunctor (T3 a) where
    bimap f g (T3 a b c) =  T3 a (f b) (g c)
    {-# INLINE bimap #-}

sfst :: T2 a b -> a
sfst (T2 a _) = a
{-# INLINE sfst #-}

ssnd :: T2 a b -> b
ssnd (T2 _ b) = b
{-# INLINE ssnd #-}

scurry :: (T2 a b -> c) -> a -> b -> c
scurry f a b = f (T2 a b)
{-# INLINE scurry #-}

suncurry :: (a -> b -> c) -> T2 a b -> c
suncurry k (T2 a b) = k a b
{-# INLINE suncurry #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 k (a, b, c) = k a b c
{-# INLINE uncurry3 #-}

suncurry3 :: (a -> b -> c -> d) -> T3 a b c -> d
suncurry3 k (T3 a b c) = k a b c
{-# INLINE suncurry3 #-}

_T2 :: Iso (T2 a b) (T2 s t) (a,b) (s,t)
_T2 = iso (\(T2 a b) -> (a,b)) (uncurry T2)
{-# INLINE _T2 #-}

_T3 :: Iso (T3 a b c) (T3 s t u) (a,b,c) (s,t,u)
_T3 = iso (\(T3 a b c) -> (a,b,c)) (\(a,b,c) -> T3 a b c)
{-# INLINE _T3 #-}

-- -------------------------------------------------------------------------- --
-- Approximate thread delays
approximately :: Integral a => a -> Prob.GenIO -> IO a
approximately k gen = max 0 <$!> sample
  where
    sample = (round . (/ 256.0) . head) <$!>
             Prob.samples 1 (Prob.normal mean sdev) gen
    mean   = fromIntegral $ k * 256
    sdev   = mean / 6


threadDelayRng :: MVar Prob.GenIO
threadDelayRng = unsafePerformIO (Prob.createSystemRandom >>= newMVar)
{-# NOINLINE threadDelayRng #-}

approximateThreadDelay :: Int -> IO ()
approximateThreadDelay d = withMVar threadDelayRng (approximately d)
                           >>= threadDelay

-- -------------------------------------------------------------------------- --
-- TLS Manager with connection timeout
--
-- TODO unify with other HTTP managers

manager :: Int -> IO HTTP.Manager
manager micros = HTTP.newManager
    $ setManagerRequestTimeout micros
    $ HTTP.tlsManagerSettings

unsafeManager :: Int -> IO HTTP.Manager
unsafeManager micros = HTTP.newTlsManagerWith
    $ setManagerRequestTimeout micros
    $ HTTP.mkManagerSettings (HTTP.TLSSettingsSimple True True True) Nothing

unsafeManagerWithSettings :: (HTTP.ManagerSettings -> HTTP.ManagerSettings) -> IO HTTP.Manager
unsafeManagerWithSettings settings = HTTP.newTlsManagerWith
    $ settings
    $ HTTP.mkManagerSettings (HTTP.TLSSettingsSimple True True True) Nothing

setManagerRequestTimeout :: Int -> HTTP.ManagerSettings -> HTTP.ManagerSettings
setManagerRequestTimeout micros settings = settings
    { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro micros
    }

-- -------------------------------------------------------------------------- --
-- SockAddr from network package

sockAddrJson :: KeyValue e kv => SockAddr -> [kv]
sockAddrJson (SockAddrInet p i) =
    [ "ipv4" .= showIpv4 i
    , "port" .= fromIntegral @PortNumber @Int p
    ]
sockAddrJson (SockAddrInet6 p f i s) =
    [ "ipv6" .= show i
    , "port" .= fromIntegral @PortNumber @Int p
    , "flowInfo" .= f
    , "scopeId" .= s
    ]
sockAddrJson (SockAddrUnix s) =
    [ "pipe" .= s
    ]

showIpv4 :: HostAddress -> T.Text
showIpv4 ha = T.intercalate "." $ sshow <$> [a0,a1,a2,a3]
  where
    (a0,a1,a2,a3) = hostAddressToTuple ha

showIpv6 :: HostAddress6 -> T.Text
showIpv6 ha = T.intercalate ":"
    $ T.pack . printf "%x" <$> [a0,a1,a2,a3,a4,a5,a6,a7]
  where
    (a0,a1,a2,a3,a4,a5,a6,a7) = hostAddress6ToTuple ha

-- -------------------------------------------------------------------------- --
-- Debugging Tools

-- | Given the current block height and the block rate, returns the estimate
-- block height for the given date.
--
estimateBlockHeight
    :: Double
        -- ^ Block rate as blocks per minute
    -> String
        -- ^ UTC date string
    -> Natural
        -- ^ Current Chain Height
    -> IO Natural
estimateBlockHeight rate dateStr curHeight = do
    dt <- diffUTCTime <$> parseUtcTime dateStr <*> getCurrentTime
    return $! curHeight + round (rate * realToFrac dt / 60)

-- | Parse UTC Time in the format "%y-%m-%dT%H:%M:%SZ"
--
parseUtcTime :: MonadThrow m => String -> m UTCTime
parseUtcTime d = case parseTimeM False defaultTimeLocale fmt d of
    Nothing -> throwM $ InternalInvariantViolation
        $ "parseUtcTime: failed to parse utc date " <> sshow d
    Just x -> return x
  where
    fmt = iso8601DateTimeFormat
