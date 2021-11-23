{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Utils.RequestLog
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A Wai middleware for logging HTTP requests
--
module Chainweb.Utils.RequestLog
(
  JsonSockAddr(..)

-- * Request Logging Middleware
, RequestLog(..)
, requestLogVersion
, requestLogMethod
, requestLogPath
, requestLogIsSecure
, requestLogRawRemoteHost
, requestLogRemoteHost
, requestLogQueryString
, requestLogBodyLength
, requestLogUserAgent
, requestLogHeaders
, requestLogger

-- * Request-Response Logging Middleware
, RequestResponseLog(..)
, requestResponseLogRequest
, requestResponseLogStatus
, requestResponseLogDurationMicro
, requestResponseLogger
) where

import Control.DeepSeq
import Control.Lens hiding ((.=))

import Data.Aeson
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics

import Network.HTTP.Types
import Network.Socket
import Network.Wai

import Numeric.Natural

import System.Clock
import System.LogLevel

-- internal modules

import Chainweb.Logger
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Request Logger

newtype JsonSockAddr = JsonSockAddr SockAddr
    deriving (Generic)
    deriving newtype (Show, Eq, Ord, NFData)

instance ToJSON JsonSockAddr where
    toJSON (JsonSockAddr s) = object $ sockAddrJson s
    toEncoding (JsonSockAddr s) = pairs . mconcat $ sockAddrJson s
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

data RequestLog = RequestLog
    { _requestLogVersion :: !T.Text
    , _requestLogMethod :: !T.Text
    , _requestLogPath :: ![T.Text]
    , _requestLogIsSecure :: !Bool
    , _requestLogRawRemoteHost :: !T.Text
    , _requestLogRemoteHost :: !JsonSockAddr
    , _requestLogQueryString :: !QueryText
    , _requestLogBodyLength :: !(Maybe Natural)
    , _requestLogUserAgent :: !(Maybe T.Text)
    , _requestLogHeaders :: !(HM.HashMap T.Text T.Text)
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

makeLenses ''RequestLog

requestLogProperties :: KeyValue kv => RequestLog -> [kv]
requestLogProperties o =
    [ "version" .= _requestLogVersion o
    , "method" .= _requestLogMethod o
    , "path" .= _requestLogPath o
    , "isSecure" .= _requestLogIsSecure o
    , "rawRemoteHost" .= _requestLogRawRemoteHost o
    , "remoteHost" .= _requestLogRemoteHost o
    , "queryString" .= _requestLogQueryString o
    , "bodyLength" .= _requestLogBodyLength o
    , "userAgent" .= _requestLogUserAgent o
    , "headers" .= _requestLogHeaders o
    ]
{-# INLINE requestLogProperties #-}

instance ToJSON RequestLog where
    toJSON = object . requestLogProperties
    toEncoding = pairs . mconcat . requestLogProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

-- | INVARIANT: this result of this function must not retain pointers to
-- the original request data that came over the wire.
--
logRequest :: Request -> RequestLog
logRequest req = RequestLog
    { _requestLogVersion = sshow $ httpVersion req
    , _requestLogMethod = T.decodeUtf8 $ requestMethod req
    , _requestLogPath = pathInfo req
    , _requestLogIsSecure = isSecure req
    , _requestLogRawRemoteHost = sshow $ remoteHost req
    , _requestLogRemoteHost = JsonSockAddr $ remoteHost req
    , _requestLogQueryString = queryToQueryText $ queryString req
    , _requestLogBodyLength = case requestBodyLength req of
        ChunkedBody -> Nothing
        KnownLength x -> Just $ int x
    , _requestLogUserAgent = T.decodeUtf8 <$> requestHeaderUserAgent req
    , _requestLogHeaders = HM.fromList $
        bimap (T.decodeUtf8 . CI.original) T.decodeUtf8 <$> (requestHeaders req)
    }

requestLogger :: Logger l => l -> Middleware
requestLogger logger app req respond = do
    logFunctionJson logger Info $ logRequest req
    app req respond

-- -------------------------------------------------------------------------- --
-- Request-Response Logger

data RequestResponseLog = RequestResponseLog
    { _requestResponseLogRequest :: !RequestLog
    , _requestResponseLogStatus :: !T.Text
    , _requestResponseLogDurationMicro :: !Int
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

makeLenses ''RequestResponseLog

requestResponseLogProperties :: KeyValue kv => RequestResponseLog -> [kv]
requestResponseLogProperties o =
    [ "request" .= _requestResponseLogRequest o
    , "status" .= _requestResponseLogStatus o
    , "durationMicro" .= _requestResponseLogDurationMicro o
    ]

instance ToJSON RequestResponseLog where
    toJSON = object . requestResponseLogProperties
    toEncoding = pairs . mconcat . requestResponseLogProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

logRequestResponse :: RequestLog -> Response -> Int -> RequestResponseLog
logRequestResponse reqLog res d = RequestResponseLog
    { _requestResponseLogRequest = reqLog
    , _requestResponseLogStatus = sshow $ responseStatus res
    , _requestResponseLogDurationMicro = d
    }

-- | NOTE: this middleware should only be used for APIs that don't stream. Otherwise
-- the logg may be delayed for indefinite time.
--
requestResponseLogger :: Logger l => l -> Middleware
requestResponseLogger logger app req respond = do
    let !reqLog = logRequest req
    reqTime <- getTime Monotonic
    app req $ \res -> do
        r <- respond res
        resTime <- getTime Monotonic
        logFunctionJson logger Info
            $ logRequestResponse reqLog res
            $ (int $ toNanoSecs $ diffTimeSpec resTime reqTime) `div` 1000
        return r
