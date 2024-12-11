{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chainweb.Utils.Throttling
    ( ThrottleEconomy(..)
    , ThrottledException(..)
    , throttleMiddleware
    ) where

import Data.LogMessage
import Data.Text (Text)
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai.Internal
import Chainweb.Utils.TokenLimiting
import Control.Exception.Safe
import Network.HTTP.Types.Status
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Hashable
import Network.Socket (SockAddr(..))
import qualified Data.ByteString.Builder as BSB
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.ByteString.Lazy as LBS

data ThrottleEconomy = ThrottleEconomy
    { requestCost :: Int
    , requestBody100ByteCost :: Int
    , responseBody100ByteCost :: Int
    , maxBudget :: Int
    , freeRate :: Int
    }

data ThrottledException = ThrottledException Text
    deriving (Show, Exception)

hashWithSalt' :: Hashable a => a -> Int -> Int
hashWithSalt' = flip hashWithSalt

newtype HashableSockAddr = HashableSockAddr SockAddr
    deriving newtype Eq
instance Hashable HashableSockAddr where
    hashWithSalt salt (HashableSockAddr sockAddr) = case sockAddr of
        SockAddrInet port hostAddr ->
            -- constructor tag
            hashWithSalt' (1 :: Word)
            . hashWithSalt' (fromIntegral port :: Word)
            . hashWithSalt' hostAddr
            $ salt
        SockAddrInet6 port flowInfo hostAddr scopeId ->
            hashWithSalt' (2 :: Word)
            . hashWithSalt' (fromIntegral port :: Word)
            . hashWithSalt' flowInfo
            . hashWithSalt' hostAddr
            . hashWithSalt' scopeId
            $ salt
        SockAddrUnix str ->
            hashWithSalt' (3 :: Word)
            . hashWithSalt' str
            $ salt

debitOrDie :: Hashable k => TokenLimitMap k -> (Text, k) -> Int -> IO ()
debitOrDie tokenLimitMap (name, k) cost = do
    tryDebit cost k tokenLimitMap >>= \case
        True -> return ()
        False -> throwIO (ThrottledException name)

throttleMiddleware :: LogFunction -> Text -> ThrottleEconomy -> (Wai.Middleware -> IO r) -> IO r
throttleMiddleware logfun name ThrottleEconomy{..} k =
    withTokenLimitMap logfun ("request-throttler-" <> name) limitCachePolicy limitConfig $ \tokenLimitMap -> do
        k $ middleware tokenLimitMap
    where
    middleware tokenLimitMap app request respond = do
        debitOrDie' requestCost
        meteredRequest <- meterRequest debitOrDie' request
        app meteredRequest (meterResponse debitOrDie' respond)
        where
        host = HashableSockAddr $ Wai.remoteHost request
        hostText = T.pack $ show (Wai.remoteHost request)
        debitOrDie' = debitOrDie tokenLimitMap (hostText, host)

    limitCachePolicy = TokenLimitCachePolicy 30
    limitConfig = defaultLimitConfig
        { maxBucketTokens = maxBudget
        , initialBucketTokens = maxBudget
        , bucketRefillTokensPerSecond = freeRate
        }

    meterRequest debit request
        | requestBody100ByteCost == 0 = return request
        | otherwise = case Wai.requestBodyLength request of
        Wai.KnownLength requestBodyLen -> do
            () <- debit $ (requestBody100ByteCost * fromIntegral requestBodyLen) `div` 100
            return request
        Wai.ChunkedBody ->
            return (Wai.setRequestBodyChunks (getMeteredRequestBodyChunk debit request) request)

    getMeteredRequestBodyChunk debit request = do
        chunk <- Wai.getRequestBodyChunk request
        -- charge *after* receiving a request body chunk
        () <- debit $ (requestBody100ByteCost * BS.length chunk) `div` 100
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
        | responseBody100ByteCost == 0 = respond response
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
            () <- debit $ (responseBody100ByteCost * BS.length chunkBS) `div` 100
            -- charger *before* sending a response body chunk
            send (BSB.byteString chunkBS)
        )
        flush
    meterBuilderResponseBody debit (chunk:chunks) = unsafeInterleaveIO $ do
        () <- debit $ (responseBody100ByteCost * BS.length chunk) `div` 100
        (chunk:) <$> meterBuilderResponseBody debit chunks
    meterBuilderResponseBody _ [] = return []
