{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Utils.RequestLog
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Utils.RequestLog
(
-- * RequestLog
  RequestLog(..)
, requestLogVersion
, requestLogMethod
, requestLogPath
, requestLogIsSecure
, requestLogRemoteHost
, requestLogQueryString
, requestLogBodyLength
, requestLogUserAgent

-- * Request Logging Middleware
, requestLogger
) where

import Control.DeepSeq
import Control.Lens

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics

import Network.HTTP.Types
import Network.Wai

import Numeric.Natural

import System.LogLevel

-- internal modules

import Chainweb.Logger
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Request Logger

data RequestLog = RequestLog
    { _requestLogVersion :: !T.Text
    , _requestLogMethod :: !T.Text
    , _requestLogPath :: ![T.Text]
    , _requestLogIsSecure :: !Bool
    , _requestLogRemoteHost :: !T.Text
    , _requestLogQueryString :: !QueryText
    , _requestLogBodyLength :: !(Maybe Natural)
    , _requestLogUserAgent :: !(Maybe T.Text)
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, ToJSON)

makeLenses ''RequestLog

logRequest :: Request -> RequestLog
logRequest req = RequestLog
    { _requestLogVersion = sshow $ httpVersion req
    , _requestLogMethod = T.decodeUtf8 $ requestMethod req
    , _requestLogPath = pathInfo req
    , _requestLogIsSecure = isSecure req
    , _requestLogRemoteHost = sshow $ remoteHost req
    , _requestLogQueryString = queryToQueryText $ queryString req
    , _requestLogBodyLength = case requestBodyLength req of
        ChunkedBody -> Nothing
        KnownLength x -> Just $ int x
    , _requestLogUserAgent = T.decodeUtf8 <$> requestHeaderUserAgent req
    }

requestLogger :: Logger l => l -> Middleware
requestLogger logger app req res = do
    logFunctionJson logger Info $ logRequest req
    app req res

