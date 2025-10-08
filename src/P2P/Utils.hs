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
( renderFailure
, clientErrorValue
, requestValue
, renderClientError
) where

import Data.Aeson
import Chainweb.Utils
import Control.Exception.Safe
import Data.ByteString qualified as B
import Data.Foldable (toList)
import Data.Text.Encoding qualified as T
import Network.HTTP.Types.URI (renderQuery)
import Servant.Client
import Servant.Client.Core
import qualified Data.Text as T

renderFailure :: SomeException -> T.Text
renderFailure e = case fromException e of
    Just (c :: ClientError) -> renderClientError c
    Nothing -> T.pack (displayException e)

renderClientError :: ClientError -> T.Text
renderClientError = encodeToText . clientErrorValue

clientErrorValue :: ClientError -> Value
clientErrorValue (FailureResponse req resp) = object
    [ "type" .= String "FailureResponse"
    , "request" .= requestValue req
    , "response" .= show resp
    ]
clientErrorValue (DecodeFailure msg resp) = object
    [ "type" .= String "DecodeFailure"
    , "message" .= msg
    , "response" .= show resp
    ]
clientErrorValue (UnsupportedContentType mt resp) = object
    [ "type" .= String "UnsupportedContentType"
    , "mediaType" .= show mt
    , "response" .= show resp
    ]
clientErrorValue (InvalidContentTypeHeader resp) = object
    [ "type" .= String "InvalidContentTypeHeader"
    , "response" .= show resp
    ]
clientErrorValue (ConnectionError e) = object
    [ "type" .= String "ConnectionError"
    , "exception" .= displayException e
    ]

requestValue :: RequestF () (BaseUrl, B.ByteString) -> Value
requestValue req = String
    $ sshow (requestMethod req)
    <> " " <> sshow base
    <> "/" <> T.decodeUtf8 path
    <> T.decodeUtf8 (renderQuery True (toList $ requestQueryString req))
  where
    (base, path) = requestPath req

