{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.NodeVersion
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.NodeVersion
( NodeVersion(..)
, minAcceptedVersion
, isAcceptedVersion
, getNodeVersion
) where

import Control.DeepSeq
-- import Control.Retry

import Data.Aeson
import Data.Bifunctor
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics

import Network.HostAddress
import qualified Network.HTTP.Client as HTTP

-- internal modules

import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Utils.Text
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Chainweb Node Version

newtype NodeVersion = NodeVersion { _getNodeVersion :: [Int] }
    deriving (Show, Eq, Ord, Generic, NFData)

instance HasTextRepresentation NodeVersion where
    toText (NodeVersion l) = T.intercalate "." $ sshow <$> l
    fromText = fmap NodeVersion . traverse treadM . T.splitOn "."
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance ToJSON NodeVersion where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance FromJSON NodeVersion where
    parseJSON = parseJsonFromText "NodeVersion"
    {-# INLINE parseJSON #-}

minAcceptedVersion :: NodeVersion
minAcceptedVersion = NodeVersion [1,2]
{-# INLINE minAcceptedVersion #-}

isAcceptedVersion :: NodeVersion -> Bool
isAcceptedVersion = (<=) minAcceptedVersion
{-# INLINE isAcceptedVersion #-}

-- -------------------------------------------------------------------------- --
-- Query Remote Version

requestTimeoutMicros :: Int
requestTimeoutMicros = 4 * 1000000

getNodeVersion
    :: HTTP.Manager
    -> ChainwebVersion
    -> HostAddress
    -> Maybe T.Text
    -> IO (Either T.Text NodeVersion)
getNodeVersion mgr ver addr maybeReq = do
    hdrs <- tryAllSynchronous
        --  $ recoverAll policy $ const
        $ HTTP.responseHeaders <$> HTTP.httpNoBody url mgr
    return $ do
        r <- first sshow hdrs
        h <- case lookup chainwebNodeVersionHeaderName r of
            Nothing -> Left
                $ "missing " <> CI.original chainwebNodeVersionHeaderName <> " header"
            Just x ->
                Right x
        first sshow $ fromText $ T.decodeUtf8 $ h
  where
    -- policy = exponentialBackoff 100 <> limitRetries 2

    url = case maybeReq of
        Nothing -> cutReq ver addr
        Just e -> req ver addr e

req
    :: ChainwebVersion
    -> HostAddress
    -> T.Text
    -> HTTP.Request
req ver addr endpoint = HTTP.defaultRequest
    { HTTP.method = "GET"
    , HTTP.secure = True
    , HTTP.host = hostnameBytes (_hostAddressHost addr)
    , HTTP.port = int (_hostAddressPort addr)
    , HTTP.path = T.encodeUtf8 $ T.intercalate "/"
        [ "/chainweb"
        , prettyApiVersion
        , sshow ver
        , endpoint
        ]
    , HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros
    , HTTP.checkResponse = HTTP.throwErrorStatusCodes
    , HTTP.requestHeaders = [chainwebNodeVersionHeader]
    }

cutReq
    :: ChainwebVersion
    -> HostAddress
    -> HTTP.Request
cutReq ver addr = HTTP.defaultRequest
    { HTTP.method = "GET"
    , HTTP.secure = True
    , HTTP.host = hostnameBytes (_hostAddressHost addr)
    , HTTP.port = int (_hostAddressPort addr)
    , HTTP.path = "/chainweb/0.0/" <> sshow ver <> "/cut"
    , HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros
    , HTTP.checkResponse = HTTP.throwErrorStatusCodes
    , HTTP.requestHeaders = [chainwebNodeVersionHeader]
    }

