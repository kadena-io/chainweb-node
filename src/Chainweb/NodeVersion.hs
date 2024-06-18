{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.NodeVersion
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The Version of a chainweb node. This is also the version of the cabal
-- package.
--
-- The module also includes tools for querying the version of a remote node.
--
module Chainweb.NodeVersion
( NodeVersion(..)
, minAcceptedVersion
, isAcceptedVersion
, getNodeVersion

-- * Node Information
, RemoteNodeInfo(..)
, remoteNodeInfoAddr
, remoteNodeInfoHostname
, remoteNodeInfoTimestamp
, remoteNodeInfoVersion
, NodeInfoException(..)
, getRemoteNodeInfo
, requestRemoteNodeInfo
) where

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad.Catch

import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX

import GHC.Generics

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

-- internal modules

import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Chainweb Node Version

newtype NodeVersion = NodeVersion { _getNodeVersion :: [Int] }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (NFData)

instance HasTextRepresentation NodeVersion where
    toText (NodeVersion l) = T.intercalate "." $ sshow <$> l
    fromText = fmap NodeVersion . traverse treadM . T.splitOn "."
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance ToJSON NodeVersion where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

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
    -> ChainwebVersionName
    -> HostAddress
    -> Maybe T.Text
    -> IO (Either T.Text NodeVersion)
getNodeVersion mgr ver addr maybeReq = do
    hdrs <- tryAllSynchronous
        --  $ recoverAll policy $ const
        $ HTTP.responseHeaders <$> HTTP.httpNoBody url mgr
    return $ do
        r <- first
            (matchOrDisplayException @HTTP.HttpException showHTTPRequestException)
            hdrs
        h <- case lookup chainwebNodeVersionHeaderName r of
            Nothing -> Left
                $ "missing " <> CI.original chainwebNodeVersionHeaderName <> " header"
            Just x ->
                Right x
        first sshow $ fromText $ T.decodeUtf8 h
  where
    -- policy = exponentialBackoff 100 <> limitRetries 2

    url = case maybeReq of
        Nothing -> cutReq ver addr
        Just e -> req ver addr e

req
    :: ChainwebVersionName
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
        , getChainwebVersionName ver
        , endpoint
        ]
    , HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros
    , HTTP.checkResponse = HTTP.throwErrorStatusCodes
    , HTTP.requestHeaders = [chainwebNodeVersionHeader]
    }

cutReq
    :: ChainwebVersionName
    -> HostAddress
    -> HTTP.Request
cutReq ver addr = HTTP.defaultRequest
    { HTTP.method = "GET"
    , HTTP.secure = True
    , HTTP.host = hostnameBytes (_hostAddressHost addr)
    , HTTP.port = int (_hostAddressPort addr)
    , HTTP.path = T.encodeUtf8 $ T.intercalate "/"
        [ "/chainweb"
        , prettyApiVersion
        , getChainwebVersionName ver
        , "cut"
        ]
    , HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros
    , HTTP.checkResponse = HTTP.throwErrorStatusCodes
    , HTTP.requestHeaders = [chainwebNodeVersionHeader]
    }

-- -------------------------------------------------------------------------- --
-- Node Info

data NodeInfoException
    = VersionHeaderMissing !HostAddress
    | ServerTimestampHeaderMissing !HostAddress
    | PeerAddrHeaderMissing !HostAddress
    | HeaderFormatException !HostAddress !SomeException
    | NodeInfoConnectionFailure !HostAddress !SomeException
    | NodeInfoUnsupported !HostAddress !NodeVersion
        -- ^ this constructor can be removed once all nodes are running
        -- version 2.4 or larger
    deriving (Show, Generic)

instance Exception NodeInfoException

data RemoteNodeInfo = RemoteNodeInfo
    { _remoteNodeInfoVersion :: !NodeVersion
    , _remoteNodeInfoTimestamp :: !POSIXTime
    , _remoteNodeInfoAddr :: !HostAddress
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

makeLenses ''RemoteNodeInfo

remoteNodeInfoHostname :: Lens' RemoteNodeInfo Hostname
remoteNodeInfoHostname = remoteNodeInfoAddr . hostAddressHost
{-# INLINE remoteNodeInfoHostname #-}

remoteNodeInfoProperties :: KeyValue e kv => RemoteNodeInfo -> [kv]
remoteNodeInfoProperties x =
    [ "version" .= _remoteNodeInfoVersion x
    , "timestamp" .= _remoteNodeInfoTimestamp x
    , "hostaddress" .= _remoteNodeInfoAddr x
    ]
{-# INLINE remoteNodeInfoProperties #-}

instance ToJSON RemoteNodeInfo where
    toJSON = object . remoteNodeInfoProperties
    toEncoding = pairs . mconcat . remoteNodeInfoProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

-- | Request NodeInfos from a remote chainweb node.
--
-- This function throws 'NodeInfoUnsupported' for remote chainweb nodes
-- with a node version smaller or equal 2.5.
--
-- No retries are attempted in case of a failure.
--
requestRemoteNodeInfo
    :: HTTP.Manager
    -> ChainwebVersionName
    -> HostAddress
    -> Maybe T.Text
    -> IO RemoteNodeInfo
requestRemoteNodeInfo mgr ver addr maybeReq =
    tryAllSynchronous reqHdrs >>= \case
        Left e -> throwM $ NodeInfoConnectionFailure addr e
        Right x -> getRemoteNodeInfo addr x
  where
    reqHdrs = HTTP.responseHeaders <$> HTTP.httpNoBody url mgr
    url = case maybeReq of
        Nothing -> (cutReq ver addr) { HTTP.method = "HEAD" }
        Just e -> (req ver addr e) { HTTP.method = "HEAD" }

-- | Obtain 'NodeInfo' of a remote Chainweb node from response headers.
--
--   No retries are attempted in case of a failure.
--
getRemoteNodeInfo
    :: forall m
    . MonadThrow m
    => HostAddress
    -> HTTP.ResponseHeaders
    -> m RemoteNodeInfo
getRemoteNodeInfo addr hdrs = do
    vers <- case lookup chainwebNodeVersionHeaderName hdrs of
        Nothing -> throwM $ VersionHeaderMissing addr
        Just x -> hdrFromText x

    RemoteNodeInfo vers
        <$> case lookup serverTimestampHeaderName hdrs of
            Nothing -> throwM $ ServerTimestampHeaderMissing addr
            Just x -> fromIntegral @Int <$> hdrFromText x
        <*> case lookup peerAddrHeaderName hdrs of
            Nothing -> throwM $ PeerAddrHeaderMissing addr
            Just x -> hdrFromText x

  where
    hdrFromText :: HasTextRepresentation a => B.ByteString -> m a
    hdrFromText hdr = case fromText (T.decodeUtf8 hdr) of
        Left e -> throwM $ HeaderFormatException addr e
        Right x -> return x
