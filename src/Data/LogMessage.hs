{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.LogMessage
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Data.LogMessage
( SomeLogMessage(..)
, LogMessage(..)

-- * Log Function
, LogFunction
, ALogFunction(..)
, alogFunction

-- * LogMessage types
, JsonLog(..)
, SomeJsonLog(..)
, TextLog(..)
, BinaryLog(..)
, SomeSymbolLog(..)
) where

import Control.DeepSeq

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as BL
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable, cast)

import GHC.Generics
import GHC.TypeLits

import System.LogLevel

-- -------------------------------------------------------------------------- --
-- SomeLogMessage

data SomeLogMessage = forall a . LogMessage a => SomeLogMessage a

instance NFData SomeLogMessage where
    rnf (SomeLogMessage a) = rnf a
    {-# INLINE rnf #-}

instance Show SomeLogMessage where
    show (SomeLogMessage a) = T.unpack $ logText a
    {-# INLINE show #-}

-- -------------------------------------------------------------------------- --
-- LogMessage

class (NFData a, Typeable a) => LogMessage a where
    logText :: a -> T.Text
    toLogMessage :: a -> SomeLogMessage
    fromLogMessage :: SomeLogMessage -> Maybe a

    toLogMessage = SomeLogMessage
    {-# INLINE toLogMessage #-}

    fromLogMessage (SomeLogMessage a) = cast a
    {-# INLINE fromLogMessage #-}

    default logText :: Show a => a -> T.Text
    logText = T.pack . show
    {-# INLINE logText #-}

instance LogMessage SomeLogMessage where
    logText (SomeLogMessage a) = logText a
    {-# INLINE logText #-}

    toLogMessage a = a
    {-# INLINE toLogMessage #-}

    fromLogMessage = Just
    {-# INLINE fromLogMessage #-}

-- | TODO: is this instance a good idea or should we use a
-- newtype wrapper?
--
instance LogMessage T.Text where
    logText t = t
    {-# INLINE logText #-}

-- -------------------------------------------------------------------------- --
-- LogFunction

type LogFunction = forall a . LogMessage a => LogLevel -> a -> IO ()

newtype ALogFunction = ALogFunction { _getLogFunction :: LogFunction }

alogFunction :: forall a . LogMessage a => ALogFunction -> LogLevel -> a -> IO ()
alogFunction (ALogFunction l) = l

-- -------------------------------------------------------------------------- --
-- LogMessage Types

newtype JsonLog a = JsonLog a
    deriving newtype (NFData)

instance (Typeable a, NFData a, ToJSON a) => LogMessage (JsonLog a) where
    logText (JsonLog a) = T.decodeUtf8 . BL.toStrict $ encode a
    {-# INLINE logText #-}

data SomeJsonLog = forall a . (NFData a, ToJSON a) => SomeJsonLog a

instance NFData SomeJsonLog where
    rnf (SomeJsonLog a) = rnf a
    {-# INLINE rnf #-}

instance LogMessage SomeJsonLog where
    logText (SomeJsonLog a) = T.decodeUtf8 . BL.toStrict $ encode a
    {-# INLINE logText #-}

newtype TextLog = TextLog T.Text
    deriving newtype (NFData, LogMessage)

data BinaryLog
    = BinaryLog B.ByteString
    | BinaryLogLazy BL.ByteString
    deriving (Generic)
    deriving anyclass (NFData)

instance LogMessage BinaryLog where
    logText (BinaryLog a) = T.decodeUtf8 $ B64.encode a
    logText (BinaryLogLazy a) = T.decodeUtf8 . B64.encode $ BL.toStrict a
    {-# INLINE logText #-}

data SomeSymbolLog = forall (a :: Symbol) . KnownSymbol a => SomeSymbolLog (Proxy a)

instance NFData SomeSymbolLog where
    rnf (SomeSymbolLog a) = rnf a

instance LogMessage SomeSymbolLog where
    logText (SomeSymbolLog (_ :: Proxy a)) = T.pack $ symbolVal (Proxy @a)
    {-# INLINE logText #-}

