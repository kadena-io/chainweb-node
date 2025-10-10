{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: P2P.Utils
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module P2P.Utils
( displayClientError
, renderClientError
, clientErrorValue
, requestValue
) where

import Chainweb.Utils
import Control.Exception.Safe
import Data.Aeson
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Foldable (toList)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.URI (renderQuery)
import Servant.Client
import Servant.Client.Core

displayClientError :: SomeException -> T.Text
displayClientError e = case fromException e of
    Just (c :: ClientError) -> renderClientError c
    Nothing -> T.pack (displayException e)

renderClientError :: ClientError -> T.Text
renderClientError = encodeToText . clientErrorValue

clientErrorValue :: ClientError -> Value
clientErrorValue (FailureResponse req resp) = object
    [ "type" .= String "FailureResponse"
    , "request" .= requestValue req
    , "code" .= HTTP.statusCode (responseStatusCode resp)
    , "message" .= B8.unpack (HTTP.statusMessage (responseStatusCode resp))
    ]
clientErrorValue (DecodeFailure msg resp) = object
    [ "type" .= String "DecodeFailure"
    , "message" .= msg
    , "code" .= HTTP.statusCode (responseStatusCode resp)
    , "message" .= B8.unpack (HTTP.statusMessage (responseStatusCode resp))
    ]
clientErrorValue (UnsupportedContentType mt resp) = object
    [ "type" .= String "UnsupportedContentType"
    , "mediaType" .= show mt
    , "code" .= HTTP.statusCode (responseStatusCode resp)
    , "message" .= B8.unpack (HTTP.statusMessage (responseStatusCode resp))
    ]
clientErrorValue (InvalidContentTypeHeader resp) = object
    [ "type" .= String "InvalidContentTypeHeader"
    , "code" .= HTTP.statusCode (responseStatusCode resp)
    , "message" .= B8.unpack (HTTP.statusMessage (responseStatusCode resp))
    ]
clientErrorValue (ConnectionError e) = object
    [ "type" .= String "ConnectionError"
    , "exception" .= displayException e
    ]

requestValue :: RequestF () (BaseUrl, B.ByteString) -> Value
requestValue req = String
    $ T.decodeUtf8 (requestMethod req) -- this is OK, since the method is ASCII and produced locally
    <> " " <> (T.pack . showBaseUrl) base
    <> T.decodeUtf8 path -- this is OK, since the path is created locally
    <> T.decodeUtf8 (renderQuery True (toList $ requestQueryString req))
  where
    (base, path) = requestPath req

