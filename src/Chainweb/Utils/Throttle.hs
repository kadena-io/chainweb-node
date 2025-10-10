{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Chainweb.Utils.Throttle
    ( ThrottleConfig(..)
    , ThrottledException(..)
    , throttleMiddleware
    , throttledResponse
    ) where

import Configuration.Utils
import Control.Exception.Safe
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Int
import Data.LogMessage
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Hashable
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Types
import Network.Socket (SockAddr(..))
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as Wai.Internal
import Numeric.Natural (Natural)
import qualified System.Clock as Clock
import System.IO.Unsafe (unsafeInterleaveIO)

import Chainweb.Utils.TokenLimiting
import Chainweb.Utils (int)
import Chainweb.Time

data ThrottleConfig = ThrottleConfig
    { _requestCost :: Int
    , _requestBody100ByteCost :: Int
    , _responseBody100ByteCost :: Int
    , _maxBudget :: Int
    -- TODO: charge for time, per second
    , _tokenBucketRefillPerSecond :: Int
    , _throttleExpiry :: Seconds
    } deriving stock (Show, Eq)

makeLenses ''ThrottleConfig

instance ToJSON ThrottleConfig where
    toJSON o = object
        [ "requestCost" .= _requestCost o
        , "requestBody100ByteCost" .= _requestBody100ByteCost o
        , "responseBody100ByteCost" .= _responseBody100ByteCost o
        , "maxBudget" .= _maxBudget o
        , "tokenBucketRefillPerSecond" .= _tokenBucketRefillPerSecond o
        , "throttleExpiry" .= int @Seconds @Int (_throttleExpiry o)
        ]

instance FromJSON (ThrottleConfig -> ThrottleConfig) where
    parseJSON = withObject "ThrottleConfig" $ \o -> id
        <$< requestCost ..: "requestCost" % o
        <*< requestBody100ByteCost ..: "requestBody100ByteCost" % o
        <*< responseBody100ByteCost ..: "responseBody100ByteCost" % o
        <*< maxBudget ..: "maxBudget" % o
        <*< tokenBucketRefillPerSecond ..: "tokenBucketRefillPerSecond" % o
        <*< throttleExpiry . (iso (int @Seconds @Int) (int @Int @Seconds)) ..: "throttleExpiry" % o

instance FromJSON ThrottleConfig where
    parseJSON = withObject "ThrottleConfig" $ \o -> do
        _requestCost <- o .: "requestCost"
        _requestBody100ByteCost <- o .: "requestBody100ByteCost"
        _responseBody100ByteCost <- o .: "responseBody100ByteCost"
        _maxBudget <- o .: "maxBudget"
        _tokenBucketRefillPerSecond <- o .: "tokenBucketRefillPerSecond"
        _throttleExpiry <- int @Natural @Seconds <$> o .: "throttleExpiry"
        return ThrottleConfig {..}

-- TODO: make an uncaught ThrottledException translate to a 429 in warp when possible
newtype ThrottledException = ThrottledException Text
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Exception)

hashWithSalt' :: Hashable a => a -> Int -> Int
hashWithSalt' = flip hashWithSalt

newtype HashableSockAddr = HashableSockAddr SockAddr
    deriving newtype (Show)

instance Eq HashableSockAddr where
    HashableSockAddr sockAddr1 == HashableSockAddr sockAddr2 = case (sockAddr1, sockAddr2) of
        (SockAddrInet _port1 hostAddr1, SockAddrInet _port2 hostAddr2) ->
            -- constructor port not used deliberately, requests can come from different ports
            hostAddr1 == hostAddr2
        (SockAddrInet6 _port1 flowInfo1 hostAddr1 scopeId1, SockAddrInet6 _port2 flowInfo2 hostAddr2 scopeId2) ->
            flowInfo1 == flowInfo2 && hostAddr1 == hostAddr2 && scopeId1 == scopeId2
        (SockAddrUnix sock1, SockAddrUnix sock2) ->
            sock1 == sock2
        _ -> False

instance Hashable HashableSockAddr where
    hashWithSalt salt (HashableSockAddr sockAddr) = case sockAddr of
        SockAddrInet _port hostAddr ->
            -- constructor tag
            hashWithSalt' (1 :: Word)
            -- port not used deliberately, requests can come from different ports
            . hashWithSalt' hostAddr
            $ salt
        SockAddrInet6 _port flowInfo hostAddr scopeId ->
            hashWithSalt' (2 :: Word)
            . hashWithSalt' flowInfo
            . hashWithSalt' hostAddr
            . hashWithSalt' scopeId
            $ salt
        SockAddrUnix sock ->
            hashWithSalt' (3 :: Word)
            . hashWithSalt' sock
            $ salt

debitOrDie :: (Hashable k) => TokenLimitMap k -> (Text, k) -> Int -> IO ()
debitOrDie tokenLimitMap (name, k) cost = do
    tryDebit cost k tokenLimitMap >>= \case
        True -> return ()
        False -> throwIO (ThrottledException name)

-- meant to be used with Warp's setOnExceptionResponse to translate thrown exceptions into responses
throttledResponse :: SomeException -> Maybe Wai.Response
throttledResponse (fromException -> Just (ThrottledException _)) =
    Just (Wai.responseLBS status429 [] "host throttled")
throttledResponse _ = Nothing

throttleMiddleware :: LogFunction -> Text -> ThrottleConfig -> (Wai.Middleware -> IO r) -> IO r
throttleMiddleware logfun name ThrottleConfig{..} k =
    withTokenLimitMap logfun ("request-throttler-" <> name) limitCachePolicy limitConfig $ \tokenLimitMap -> do
        k $ middleware tokenLimitMap
    where
    middleware tokenLimitMap app request respond = do
        debitOrDie' _requestCost
        meteredRequest <- meterRequest debitOrDie' request
        -- if response chunks are being sent back, it's too late to decide to return a 429.
        -- warp probably doesn't know what to do if the response streaming itself throws an error.
        -- so we use penalize instead, which can bring the token bucket negative and won't throw an error.
        app meteredRequest (meterResponse penalize' respond)
        where
        host = HashableSockAddr $ Wai.remoteHost request
        hostText = T.pack $ show (Wai.remoteHost request)
        debitOrDie' c = do
            debitOrDie tokenLimitMap (hostText, host) c
        penalize' tks = void $ penalize tks host tokenLimitMap

    limitCachePolicy = TokenLimitCachePolicy (Clock.TimeSpec { sec = int @Seconds @Int64 _throttleExpiry, nsec = 0 })
    limitConfig = defaultLimitConfig
        { maxBucketTokens = _maxBudget
        , initialBucketTokens = _maxBudget
        , bucketRefillTokensPerSecond = _tokenBucketRefillPerSecond
        }

    meterRequest debit request
        | _requestBody100ByteCost == 0 = return request
        | otherwise = case Wai.requestBodyLength request of
        Wai.KnownLength requestBodyLen -> do
            () <- debit $ (_requestBody100ByteCost * fromIntegral (max requestBodyLen 100)) `div` 100
            return request
        Wai.ChunkedBody ->
            return (Wai.setRequestBodyChunks (getMeteredRequestBodyChunk debit request) request)

    getMeteredRequestBodyChunk debit request = do
        chunk <- Wai.getRequestBodyChunk request
        -- charge *after* receiving a request body chunk
        () <- debit $ (_requestBody100ByteCost * max (BS.length chunk) 100) `div` 100
        return chunk

    -- the only way to match on responses without using internal API is via
    -- responseToStream, which converts any response into a streaming response.
    -- unfortunately:
    --    * all of the responses produced by servant are builder responses,
    --      not streaming responses
    --    * streaming responses are not supported by http2; we try to use http2
    --      (see https://hackage.haskell.org/package/http2-5.3.5/docs/src/Network.HTTP2.Server.Run.html#runIO)
    --    * a streaming response body may be less efficient than a builder
    --      response body, in particular because it needs to use a chunked
    --      encoding
    --
    meterResponse
        :: (Int -> IO ())
        -> (Wai.Response -> IO a) -> Wai.Response -> IO a
    meterResponse _ respond response
        | _responseBody100ByteCost == 0 = respond response
    meterResponse debit respond (Wai.Internal.ResponseStream status headers responseBody) = do
        respond
            $ Wai.responseStream status headers
            $ meterStreamingResponseBody debit responseBody
    meterResponse debit respond (Wai.Internal.ResponseBuilder status headers responseBody) = do
        respond
            <$> Wai.responseLBS status headers . LBS.fromChunks
            =<< meterBuilderResponseBody debit (LBS.toChunks $ BSB.toLazyByteString responseBody)
    meterResponse _ _ _ = error "unrecognized response type"

    meterStreamingResponseBody debit responseBody send flush = responseBody
        (\chunkBSBuilder -> do
            let chunkBS = BS.toStrict (BSB.toLazyByteString chunkBSBuilder)
            () <- debit $ (_responseBody100ByteCost * max (BS.length chunkBS) 100) `div` 100
            -- charger *before* sending a response body chunk
            send (BSB.byteString chunkBS)
        )
        flush
    meterBuilderResponseBody debit (chunk:chunks) = unsafeInterleaveIO $ do
        () <- debit $ (_responseBody100ByteCost * max (BS.length chunk) 100) `div` 100
        (chunk:) <$> meterBuilderResponseBody debit chunks
    meterBuilderResponseBody _ [] = return []
