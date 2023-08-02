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
-- This module defines log messages that are similar to Haskell exceptions from
-- 'Control.Exception'. Like exceptions, log messages in this module are typed
-- dynamically and classes of log messages types are extensible.
--
-- Log messages and exceptions are similar in that they can be emitted/thrown
-- anywhere in the code base (in the IO monad, for logs) and are propagated to
-- handlers that are defined upward in the call stack until they are eventually
-- picked up. The difference is that exceptions synchronously interrupt the
-- computation that throws them, while log messages are usually handled
-- asynchronously and the computation that emits them continues while the
-- message is handled.
--
-- Log messages are usually handled only at the top level by a global handler
-- (or stack of handlers), but that depends on the implementation of the logger
-- (usually a queue, but sometimes just an IO callback), which is orthorgonal to
-- the definitions in this module.
--
module Data.LogMessage
( SomeLogMessage(..)
, LogMessage(..)

-- * Log Function
, LogFunction
, LogFunctionText
, LogFunctionJson
, ALogFunction(..)
, alogFunction
, aNoLog

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
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable, cast)

import GHC.Generics
import GHC.TypeLits

import System.LogLevel

-- -------------------------------------------------------------------------- --
-- SomeLogMessage

-- | The 'SomeLogMessage' type is the root of the log message type hierarchy.
-- When a log message of type 'a' is emitted, behind the scenes it is
-- encapsulated in a 'SomeLogMessage'.
--
data SomeLogMessage = forall a . LogMessage a => SomeLogMessage a

instance NFData SomeLogMessage where
    rnf (SomeLogMessage a) = rnf a
    {-# INLINE rnf #-}

instance Show SomeLogMessage where
    show (SomeLogMessage a) = T.unpack $ logText a
    {-# INLINE show #-}

-- -------------------------------------------------------------------------- --
-- LogMessage

-- | The class of log messages.
--
-- Log messages must be instances of 'NFData' and 'Typeable' and must have a
-- textual representation. The default instance uses the 'Show' instance of a
-- type.
--
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

-- | Type of a log functions
--
type LogFunction = forall a . LogMessage a => LogLevel -> a -> IO ()

-- | 'LogFunction' type specialized to 'T.Text'
--
type LogFunctionText = LogLevel -> T.Text -> IO ()

-- | 'LogFunction' type specialized to JSON
--
type LogFunctionJson a =
  (Typeable a, NFData a, ToJSON a) => LogLevel -> a -> IO ()

-- | A newtype wrapper that allows to store a 'LogFunction' without running into
-- impredicative types.
--
newtype ALogFunction = ALogFunction { _getLogFunction :: LogFunction }

-- | Get a 'LogFunction' from 'ALogFunction'
--
alogFunction :: forall a . LogMessage a => ALogFunction -> LogLevel -> a -> IO ()
alogFunction (ALogFunction l) = l

-- | 'ALogFunction' that discards all log messages
--
aNoLog :: ALogFunction
aNoLog = ALogFunction $ \_ _ -> return ()

-- -------------------------------------------------------------------------- --
-- LogMessage Types

-- | A newtype wrapper for log messages types with a 'ToJSON' instance.
--
-- This type must not have a `ToJSON` instance.
--
newtype JsonLog a = JsonLog { unJsonLog :: a }
    deriving newtype (NFData)

instance (Typeable a, NFData a, ToJSON a) => LogMessage (JsonLog a) where
    logText (JsonLog a) = T.decodeUtf8 . BL.toStrict $ encode a
    {-# INLINE logText #-}

-- | A dynamically polymorphic wrapper for any log message type that has a
-- 'ToJSON' instance.
--
data SomeJsonLog = forall a . (NFData a, ToJSON a) => SomeJsonLog a

instance NFData SomeJsonLog where
    rnf (SomeJsonLog a) = rnf a
    {-# INLINE rnf #-}

instance LogMessage SomeJsonLog where
    logText (SomeJsonLog a) = T.decodeUtf8 . BL.toStrict $ encode a
    {-# INLINE logText #-}

-- | A newtype wrapper for textual log messages.
--
newtype TextLog = TextLog T.Text
    deriving newtype (NFData, LogMessage, IsString)

-- | Binary log messages.
--
data BinaryLog
    = BinaryLog B.ByteString
    | BinaryLogLazy BL.ByteString
    deriving (Generic)
    deriving anyclass (NFData)

instance LogMessage BinaryLog where
    logText (BinaryLog a) = T.decodeUtf8 $ B64.encode a
    logText (BinaryLogLazy a) = T.decodeUtf8 . B64.encode $ BL.toStrict a
    {-# INLINE logText #-}

-- | Static textual log messages using 'Symbol' literals from 'GHC.TypeLits'.
--
data SomeSymbolLog = forall (a :: Symbol) . KnownSymbol a => SomeSymbolLog (Proxy a)

instance NFData SomeSymbolLog where
    rnf (SomeSymbolLog a) = rnf a

instance LogMessage SomeSymbolLog where
    logText (SomeSymbolLog (_ :: Proxy a)) = T.pack $ symbolVal (Proxy @a)
    {-# INLINE logText #-}

