{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Utils.Text
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Text Utils
--
module Chainweb.Utils.Text
( sshow
, tread
, treadM
, HasTextRepresentation(..)
, eitherFromText
, unsafeFromText
, parseM
, parseText
) where

import Control.DeepSeq
import Control.Monad.Catch

import qualified Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Hashable
import Data.String
import qualified Data.Text as T

import GHC.Generics
import GHC.Stack

import Network.HostAddress

import Text.Read (readEither)

-- -------------------------------------------------------------------------- --
-- Exceptions

newtype TextFormatException = TextFormatException T.Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)
    deriving newtype (NFData, IsString)

instance Exception TextFormatException

-- -------------------------------------------------------------------------- --
-- Misc Utils

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
treadM = either throwM return . first TextFormatException . tread
{-# INLINE treadM #-}

-- | Run a 'A.Parser' on a text input. All input must be consume by the parser.
-- A 'TextFormatException' is thrown if parsing fails.
--
parseM :: MonadThrow m => A.Parser a -> T.Text -> m a
parseM p = either (throwM . TextFormatException . T.pack) return
    . A.parseOnly (p <* A.endOfInput)
{-# INLINE parseM #-}

-- -------------------------------------------------------------------------- --
-- HasTextRepresentation

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

instance HasTextRepresentation Port where
    toText = portToText
    {-# INLINE toText #-}
    fromText = portFromText
    {-# INLINE fromText #-}

instance HasTextRepresentation Hostname where
    toText = hostnameToText
    {-# INLINE toText #-}
    fromText = hostnameFromText
    {-# INLINE fromText #-}

instance HasTextRepresentation HostAddress where
    toText = hostAddressToText
    {-# INLINE toText #-}
    fromText = hostAddressFromText
    {-# INLINE fromText #-}

-- | Unsafely decode a value rom its textual representation. It is an program
-- error if decoding fails.
--
unsafeFromText :: HasCallStack => HasTextRepresentation a => T.Text -> a
unsafeFromText a
    | Just x <- fromText a = x
    | otherwise = error "Chainweb.Utils.Text.unsafeFromText"
{-# INLINE unsafeFromText #-}

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

-- | A parser for types with an 'HasTextRepresentation' instance.
--
parseText :: HasTextRepresentation a => A.Parser T.Text -> A.Parser a
parseText p = either (fail . sshow) return . fromText =<< p
{-# INLINE parseText #-}

