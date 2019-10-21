{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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
, allEqOn
, roundBy
, unlessM
, whenM
, ebool_
, partitionEithersNEL
, (&)
, IxedGet(..)

-- * Encoding and Serialization
, EncodingException(..)

-- ** Binary
, runGet
, runPut
, runGetEither

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
, eatIOExceptions

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
, jsonOption

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

-- * Filesystem
, withTempDir

-- * Type Level
, symbolText
-- * optics
, locally

-- * Resource Management
, concurrentWith
, withLink

-- * Strict Tuples
, sfst
, ssnd
, scurry
, suncurry
, suncurry3
, rwipe3

-- * Approximate thread delays
, approximateThreadDelay
) where

import Configuration.Utils hiding (Error, Lens)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.TokenBucket
import Control.DeepSeq
import Control.Exception
    (IOException, SomeAsyncException(..), bracket, evaluate)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch hiding (bracket)
import Control.Monad.IO.Class
import Control.Monad.Reader as Reader

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Bits
import Data.Bool (bool)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Unsafe as B
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Functor.Of
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Monoid (Endo)
import Data.Proxy
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.These (These(..))
import Data.Tuple.Strict
import Data.Word (Word64)

import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Numeric.Natural

import qualified Options.Applicative as O

import qualified Streaming as S (concats, effect)
import qualified Streaming.Prelude as S

import System.Directory (removeDirectoryRecursive)
import System.IO.Unsafe (unsafePerformIO)
import System.LogLevel
import System.Path (Absolute, Path, fragment, toAbsoluteFilePath, (</>))
import System.Path.IO (getTemporaryDirectory)
import System.Random (randomIO)
import qualified System.Random.MWC as Prob
import qualified System.Random.MWC.Probability as Prob
import System.Timeout

import Text.Printf (printf)
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

-- | The the minimum elements of a list.
--
minimumsOf :: Ord a => Getting (Endo (Endo [a])) s a -> s -> [a]
minimumsOf l = minimumsByOf l compare
{-# INLINE minimumsOf #-}

-- | The the minimum elements of a list by some comparision function.
--
minimumsByOf :: Getting (Endo (Endo [a])) s a -> (a -> a -> Ordering) -> s -> [a]
minimumsByOf l cmp = foldlOf' l mf []
  where
    mf [] !y = [y]
    mf x@(h:_) !y = case cmp h y of
        EQ -> y:x
        GT -> [y]
        LT -> x
{-# INLINE minimumsByOf #-}

-- | The maximum of two value by some comparision function.
--
maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp a b = case cmp a b of
    LT -> b
    _ -> a
{-# INLINE maxBy #-}

-- | The minimum of two value by some comparision function.
--
minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp a b = case cmp a b of
    GT -> b
    _ -> a
{-# INLINE minBy #-}

-- | Checks that all elements of foldable structure are equal under the given
-- mapping.
--
allEqOn :: Foldable f => Eq b => (a -> b) -> f a -> Bool
allEqOn p f = case toList f of
    [] -> True
    (h:t) -> all (== p h) $ p <$> t
{-# INLINEABLE allEqOn #-}

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

-- | Round an integral `n` up to the nearest multiple of
-- an integral `m`
--
roundBy :: Integral a => a -> a -> a
roundBy n m = ((n `div` m) + 1) * m
{-# INLINE roundBy #-}

partitionEithersNEL :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)
partitionEithersNEL (h :| es) = case bimap NEL.nonEmpty NEL.nonEmpty $ partitionEithers es of
    (Nothing, Nothing) -> either (This . pure) (That . pure) h
    (Just as, Nothing) -> This as
    (Nothing, Just bs) -> That bs
    (Just as, Just bs) -> These as bs

-- -------------------------------------------------------------------------- --
-- * Read only Ixed

-- | Provides a simple Fold lets you fold the value at a given key in a Map or
-- element at an ordinal position in a list or Seq.
--
-- This is a restrictec version of 'Ixed' from the lens package that prevents
-- the value at the key from being modified.
--
class IxedGet a where

    -- | Provide a 'Fold' for a value at a given key.
    --
    ixg :: Index a -> Fold a (IxValue a)

    default ixg :: Ixed a => Index a -> Fold a (IxValue a)
    ixg = ix
    {-# INLINE ixg #-}

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

instance Exception EncodingException

-- | Decode a value from a 'B.ByteString'. In case of a failure a
-- 'DecodeException' is thrown.
--
runGet :: MonadThrow m => Get a -> B.ByteString -> m a
runGet g = fromEitherM . runGetEither g
{-# INLINE runGet #-}

-- | Decode a value from a 'B.ByteString' and return either the result or a
-- 'DecodeException'.
--
runGetEither :: Get a -> B.ByteString -> Either EncodingException a
runGetEither g = first (DecodeException . T.pack) . runGetS g
{-# INLINE runGetEither #-}

-- | Encode a value into a 'B.ByteString'.
--
runPut :: Put -> B.ByteString
runPut = runPutS
{-# INLINE runPut #-}

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

jsonOption :: FromJSON a => Mod OptionFields a -> O.Parser a
jsonOption = option jsonReader

jsonReader :: FromJSON a => ReadM a
jsonReader = eitherReader $ eitherDecode' . BL8.pack

-- -------------------------------------------------------------------------- --
-- Error Handling

-- | A newtype wrapper for tagger values as "expected" outcomes of some
-- computation.
--
newtype Expected a = Expected { getExpected :: a }
    deriving (Show, Eq, Ord, Generic, Functor)

-- | A newtype wrapper for tagger values as "actual" outcomes of some
-- computation.
--
newtype Actual a = Actual { getActual :: a }
    deriving (Show, Eq, Ord, Generic, Functor)

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

-- | Catch and strictly evaluate any 'IOException's.
--
-- This function should be used with great care because operation may silently
-- fail without leaving a trace. This can hide issues in the code making them
-- very difficult to debug.
--
eatIOExceptions :: IO () -> IO ()
eatIOExceptions = handle $ \(e :: IOException) -> void $ evaluate e

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

-- | A version of 'tryhSynchronous' that doesn't discriminate on the type of the
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
-- Count leading zero bits of a bytestring

-- | Count leading zero bits of a bytestring
--
leadingZeros :: Integral int => B.ByteString -> int
leadingZeros b =
    let l = B.length b
        midx = B.findIndex (/= 0x00) b
        countInLastChar idx = countLeadingZeros $! B.unsafeIndex b (idx + 1)
        f idx = 8 * idx + countInLastChar idx
        !out = int $! maybe (8 * l) f midx
    in out
{-# INLINE leadingZeros #-}

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

instance ToJSON a => ToJSON (EnableConfig a) where
    toJSON o = object
        [ "enabled" .= _enableConfigEnabled o
        , "configuration" .= _enableConfigConfig o
        ]

instance FromJSON (a -> a) => FromJSON (EnableConfig a -> EnableConfig a) where
    parseJSON = withObject "EnableConfig" $ \o -> id
        <$< enableConfigEnabled ..: "enabled" % o
        <*< enableConfigConfig %.: "configuration" % o

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
        Just (Left r) -> return $! Just r
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

-- | A binary codec.
--
-- TODO: maybe use Put/Get ?
--
data Codec t = Codec
    { codecEncode :: t -> ByteString
    , codecDecode :: ByteString -> Either String t
    }

-- | Perform an action over a random path under @/tmp@. Example path:
--
-- @
-- Path "/tmp/chainweb-git-store-test-8086816238120523704"
-- @
--
withTempDir :: String -> (Path Absolute -> IO a) -> IO a
withTempDir tag f = bracket create delete f
  where
    create :: IO (Path Absolute)
    create = do
        tmp <- getTemporaryDirectory
        suff <- randomIO @Word64
        pure $! tmp </> fragment (printf "chainweb-%s-%d" tag suff)

    delete :: Path Absolute -> IO ()
    delete = toAbsoluteFilePath >=> removeDirectoryRecursive

-- -------------------------------------------------------------------------- --
-- Typelevel

-- | Return the value of a type level symbol as a value of a type that is an
-- instance of 'IsString'.
--
symbolText :: forall s a . KnownSymbol s => IsString a => a
symbolText = fromString $ symbolVal (Proxy @s)

-- -------------------------------------------------------------------------- --
-- Optics

#if ! MIN_VERSION_lens(4,17,1)
-- | Like 'local' for reader environments, but modifies the
-- target of a lens possibly deep in the environment
--
locally :: MonadReader s m => ASetter s s a b -> (a -> b) -> m r -> m r
locally l f = Reader.local (over l f)
#endif

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
        -- that value, and retursn the result of the inner computation.
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

-- -------------------------------------------------------------------------- --
-- Strict Tuple

suncurry3 :: (a -> b -> c -> d) -> T3 a b c -> d
suncurry3 k (T3 a b c) = k a b c
{-# INLINE suncurry3 #-}

rwipe3 :: T3 a b c -> T2 b c
rwipe3 (T3 _ b c) = T2 b c
{-# INLINE rwipe3 #-}

-- -------------------------------------------------------------------------- --
-- Approximate thread delays
approximately :: Int -> Prob.GenIO -> IO Int
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
