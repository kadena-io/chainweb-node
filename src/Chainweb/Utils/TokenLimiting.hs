{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | A concurrent, expiring map from @k@ to RateLimiter.
module Chainweb.Utils.TokenLimiting
( TokenLimitMap
, TokenLimitCachePolicy(..)
, LimitConfig(..)
, withTokenLimitMap
, defaultLimitConfig
, makeLimitConfig
, getLimiter
, getLimitPolicy
, tryDebit
, waitDebit
, penalize
) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.TokenLimiter (LimitConfig(..), RateLimiter)
import qualified Control.Concurrent.TokenLimiter as TL
import Control.Exception
import Data.Cache (Cache)
import qualified Data.Cache as Cache
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics
import System.Clock (TimeSpec)

import Chainweb.Utils
import Data.LogMessage

data TokenLimitMap k = TokenLimitMap
    { _tlmMap :: !(Cache k RateLimiter)
    , _tlmLimitPolicy :: !LimitConfig
      -- ^ token bucket rate limiting policy (max num tokens, refill rate, etc)
    , _tlmCachePolicy :: !TokenLimitCachePolicy
      -- ^ inactivity period before your key's rate limiter is expired from the
      -- cache
    } deriving (Generic)

newtype TokenLimitCachePolicy = TokenLimitCachePolicy
    { policyExpirationTime :: TimeSpec
    } deriving (Generic, Eq, Ord, Num, Show)

makeLimitConfig :: Int -> Int -> Int -> LimitConfig
makeLimitConfig mx it ref =
    defaultLimitConfig
        { maxBucketTokens = mx
        , initialBucketTokens = it
        , bucketRefillTokensPerSecond = ref
        }

withTokenLimitMap
    :: (Eq k, Hashable k)
    => LogFunctionText
    -> Text
    -> TokenLimitCachePolicy
    -> LimitConfig
    -> (TokenLimitMap k -> IO a)
    -> IO a
withTokenLimitMap logfun mapName expPolicy@(TokenLimitCachePolicy expTSpec) lcfg act =
    mask $ \restore -> do
        cache <- restore (Cache.newCache (Just expTSpec))
        Async.withAsync (reaper logfun mapName cache) $ \_ -> do
            let m = TokenLimitMap cache lcfg expPolicy
            restore $ act m

reaper
    :: (Eq k, Hashable k)
    => LogFunctionText
    -> Text
    -> Cache k v -> IO ()
reaper logfun mapName cache = runForever logfun mapName $ do
    approximateThreadDelay (2 * 60 * 1000000) -- two minute cycle time
    Cache.purgeExpired cache

getLimiter :: (Eq k, Hashable k) => k -> TokenLimitMap k -> IO RateLimiter
getLimiter k (TokenLimitMap cache limitPolicy _) =
    Cache.fetchWithCache cache k (\_ -> TL.newRateLimiter limitPolicy)

penalize :: (Eq k, Hashable k) => Int -> k -> TokenLimitMap k -> IO Int
penalize ndebits k tlm = do
    rl <- getLimiter k tlm
    TL.penalize rl ndebits

tryDebit :: (Eq k, Hashable k) => Int -> k -> TokenLimitMap k -> IO Bool
tryDebit ndebits k tlm = do
    rl <- getLimiter k tlm
    TL.tryDebit (_tlmLimitPolicy tlm) rl ndebits

waitDebit :: (Eq k, Hashable k) => Int -> k -> TokenLimitMap k -> IO ()
waitDebit ndebits k tlm = do
    rl <- getLimiter k tlm
    TL.waitDebit (_tlmLimitPolicy tlm) rl ndebits

defaultLimitConfig :: LimitConfig
defaultLimitConfig = TL.defaultLimitConfig

getLimitPolicy :: TokenLimitMap k -> LimitConfig
getLimitPolicy = _tlmLimitPolicy
