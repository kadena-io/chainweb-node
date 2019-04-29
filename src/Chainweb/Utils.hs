{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE TypeApplications #-}

-- ixg
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
, allEqOn
, unlessM
, whenM
, (&)
, IxedGet(..)

-- * Diffs
, DiffItem(..)
, resolve

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

-- * Command Line Options
, OptionParser
, prefixLong
, suffixHelp
, textReader
, textOption

-- * Configuration to Enable/Disable Components

, EnableConfig(..)
, enableConfigConfig
, enableConfigEnabled
, defaultEnableConfig
, pEnableConfig

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
) where

import Configuration.Utils hiding (Error)

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
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Functor.Of
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Monoid (Endo)
import Data.Proxy
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word64)

import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Numeric.Natural

import qualified Options.Applicative as O

import qualified Streaming as S (concats, effect, maps)
import qualified Streaming.Prelude as S

import System.Directory (removeDirectoryRecursive)
import System.LogLevel
import System.Path (Absolute, Path, fragment, toAbsoluteFilePath, (</>))
import System.Path.IO (getTemporaryDirectory)
import System.Random (randomIO)
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

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

len :: Integral a => [b] -> a
len = int . length
{-# INLINE len #-}

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b
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

-- -------------------------------------------------------------------------- --
-- * Read only Ixed

class IxedGet a where
    ixg :: Index a -> Fold a (IxValue a)

    default ixg :: Ixed a => Index a -> Fold a (IxValue a)
    ixg = ix
    {-# INLINE ixg #-}

-- -------------------------------------------------------------------------- --
-- * Diffs

data DiffItem a
    = LeftD a
    | RightD a
    | BothD a a
    deriving (Show, Eq, Ord, Generic, Hashable, Functor, Foldable, Traversable)

resolve :: (a -> b) -> (a -> b) -> (a -> a -> b) -> DiffItem a -> b
resolve l _ _ (LeftD a) = l a
resolve _ r _ (RightD a) = r a
resolve _ _ m (BothD a b) = m a b

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
    X509CertificateDecodeException :: T.Text -> EncodingException
    X509KeyDecodeException :: T.Text -> EncodingException
    deriving (Show, Eq, Ord, Generic)

instance Exception EncodingException

runGet :: MonadThrow m => Get a -> B.ByteString -> m a
runGet g = fromEitherM . runGetEither g
{-# INLINE runGet #-}

runGetEither :: Get a -> B.ByteString -> Either EncodingException a
runGetEither g = first (DecodeException . T.pack) . runGetS g
{-# INLINE runGetEither #-}

runPut :: Put -> B.ByteString
runPut = runPutS
{-# INLINE runPut #-}

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

instance HasTextRepresentation Int where
    toText = sshow
    {-# INLINE toText #-}
    fromText = treadM
    {-# INLINE fromText #-}

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

unsafeFromText :: HasCallStack => HasTextRepresentation a => T.Text -> a
unsafeFromText = fromJuste . fromText
{-# INLINE unsafeFromText #-}

parseM :: MonadThrow m => A.Parser a -> T.Text -> m a
parseM p = either (throwM . TextFormatException . T.pack) return
    . A.parseOnly (p <* A.endOfInput)
{-# INLINE parseM #-}

parseText :: HasTextRepresentation a => A.Parser T.Text -> A.Parser a
parseText p = either (fail . sshow) return . fromText =<< p
{-# INLINE parseText #-}

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

-- | Strict aeson encode.
encodeToByteString :: ToJSON a => a -> B.ByteString
encodeToByteString = BL.toStrict . encode

decodeStrictOrThrow :: MonadThrow m => FromJSON a => B.ByteString -> m a
decodeStrictOrThrow = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecodeStrict
{-# INLINE decodeStrictOrThrow #-}

decodeStrictOrThrow' :: MonadThrow m => FromJSON a => B.ByteString -> m a
decodeStrictOrThrow' = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecodeStrict'
{-# INLINE decodeStrictOrThrow' #-}

decodeOrThrow :: MonadThrow m => FromJSON a => BL.ByteString -> m a
decodeOrThrow = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecode
{-# INLINE decodeOrThrow #-}

decodeOrThrow' :: MonadThrow m => FromJSON a => BL.ByteString -> m a
decodeOrThrow' = fromEitherM
    . first (JsonDecodeException . T.pack)
    . eitherDecode'
{-# INLINE decodeOrThrow' #-}

decodeFileStrictOrThrow :: MonadIO m => MonadThrow m => FromJSON a => FilePath -> m a
decodeFileStrictOrThrow = fromEitherM
    <=< return . first (JsonDecodeException . T.pack)
    <=< liftIO . eitherDecodeFileStrict
{-# INLINE decodeFileStrictOrThrow #-}

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

-- -------------------------------------------------------------------------- --
-- Option Parsing

type OptionParser a = O.Parser a

prefixLong :: HasName f => Maybe String -> String -> Mod f a
prefixLong prefix l = long $ maybe "" (<> "-") prefix <> l

suffixHelp :: Maybe String -> String -> Mod f a
suffixHelp suffix l = help $ l <> maybe "" (" for " <>) suffix

textReader :: HasTextRepresentation a => ReadM a
textReader = eitherReader $ first show . fromText . T.pack

textOption :: HasTextRepresentation a => Mod OptionFields a -> O.Parser a
textOption = option textReader

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

-- | Like `Data.Maybe.fromJust`, but carries forward the `HasCallStack`
-- constraint. "Juste" is French for "Just".
--
fromJuste :: HasCallStack => Maybe a -> a
fromJuste Nothing = error "Chainweb.Utils.fromJuste: Nothing"
fromJuste (Just a) = a

(???) :: MonadThrow m => Exception e => Maybe a -> e -> m a
(???) = flip fromMaybeM
infixl 0 ???
{-# INLINE (???) #-}

fromEitherM :: MonadThrow m => Exception e => Either e a -> m a
fromEitherM = either throwM return
{-# INLINE fromEitherM #-}

newtype InternalInvariantViolation = InternalInvariantViolation T.Text
    deriving (Show)

instance Exception InternalInvariantViolation

eatIOExceptions :: IO () -> IO ()
eatIOExceptions = handle $ \(e :: IOException) -> void $ evaluate e

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

catchAllSynchronous
    :: MonadCatch m
    => NFData a
    => m a
    -> (SomeException -> m a)
    -> m a
catchAllSynchronous = catchSynchronous
{-# INLINE catchAllSynchronous #-}

tryAllSynchronous
    :: MonadCatch m
    => NFData a
    => m a
    -> m (Either SomeException a)
tryAllSynchronous = trySynchronous
{-# INLINE tryAllSynchronous #-}

runForever :: (LogLevel -> T.Text -> IO ()) -> T.Text -> IO () -> IO ()
runForever logfun name a = mask $ \umask -> do
    logfun Info $ "start " <> name
    let go = do
            forever (umask a) `catchAllSynchronous` \e ->
                logfun Error $ name <> " failed: " <> sshow e
            go
    void go
    logfun Info $ name <> " stopped"

-- -------------------------------------------------------------------------- --
-- Count leading zeros of a bytestring

leadingZeros :: B.ByteString -> Natural
leadingZeros b = int (B.length x) * 8 + case B.uncons y of
    Just (h, _) -> int $ countLeadingZeros h
    Nothing -> 0
  where
    (x, y) = B.span (== 0x00) b

-- -------------------------------------------------------------------------- --
-- Configuration wrapper to enable and disable components

data EnableConfig a = EnableConfig
    { _enableConfigEnabled :: !Bool
    , _enableConfigConfig :: !a
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''EnableConfig

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

pEnableConfig :: String -> MParser a -> MParser (EnableConfig a)
pEnableConfig compName pConfig = id
    <$< enableConfigEnabled .:: enableDisableFlag
        % long compName
        <> help ("whether " <> compName <> " is enabled or disabled")
    <*< enableConfigConfig %:: pConfig

-- -------------------------------------------------------------------------- --
-- Configuration Validation

newtype ConfigurationException = ConfigurationException T.Text
    deriving (Show, Eq, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)

instance Exception ConfigurationException

-- -------------------------------------------------------------------------- --
-- Streaming Utilities

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

nub :: Monad m => Eq a => S.Stream (Of a) m r -> S.Stream (Of a) m r
nub = S.concats . S.maps (S.drained . S.splitAt 1) . S.group

streamToHashSet
    :: Monad m
    => Eq a
    => Hashable a
    => S.Stream (Of a) m r
    -> m (Of (HS.HashSet a) r)
streamToHashSet = fmap (first HS.fromList) . S.toList

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

-- | TODO: maybe use Put/Get ?
data Codec t = Codec {
    codecEncode :: t -> ByteString
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
        pure $ tmp </> fragment (printf "chainweb-%s-%d" tag suff)

    delete :: Path Absolute -> IO ()
    delete = toAbsoluteFilePath >=> removeDirectoryRecursive

-- -------------------------------------------------------------------------- --
-- Typelevel

symbolText :: forall s a . KnownSymbol s => IsString a => a
symbolText = fromString $ symbolVal (Proxy @s)

-- -------------------------------------------------------------------------- --
-- Optics

-- | Like 'local' for reader environments, but modifies the
-- target of a lens possibly deep in the environment
--
locally :: MonadReader s m => ASetter s s a b -> (a -> b) -> m r -> m r
locally l f = Reader.local (over l f)
