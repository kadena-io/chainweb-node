{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A concurrent, expiring map from @k@ to RateLimiter.
module Chainweb.Utils.TokenLimiting
( TokenLimitMap
, TokenLimitCachePolicy(..)
, LimitConfig(..)
, withTokenLimitMap
, startTokenLimitMap
, stopTokenLimitMap
, defaultLimitConfig
, makeLimitConfig
, getLimiter
, withLimiter
, getLimitPolicy
, tryDebit
, waitDebit
, penalize
) where

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Concurrent.TokenLimiter (LimitConfig(..), RateLimiter)
import qualified Control.Concurrent.TokenLimiter as TL
import Control.Exception
import Control.Monad
import Data.Cache (Cache)
import qualified Data.Cache as Cache
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics
import System.Clock (TimeSpec)
import qualified System.Clock as Clock

import Chainweb.Utils
import Data.LogMessage

data TokenLimitMap k = TokenLimitMap
    { _tlmMap :: !(Cache k RateLimiter)
    , _tlmLimitPolicy :: !LimitConfig
      -- ^ token bucket rate limiting policy (max num tokens, refill rate, etc)
    , _tlmReaper :: !(Async ())
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
        Async.withAsync (reap cache) $ \rtid -> do
            let m = TokenLimitMap cache lcfg rtid expPolicy
            restore $ act m
  where
    reap = reaper logfun mapName

startTokenLimitMap
    :: (Eq k, Hashable k)
    => LogFunctionText
    -> Text
    -> TokenLimitCachePolicy
    -> LimitConfig
    -> IO (TokenLimitMap k)
startTokenLimitMap logfun mapName expPolicy@(TokenLimitCachePolicy expTSpec)
                   lcfg = do
    cache <- Cache.newCache (Just expTSpec)
    rtid <- Async.async (reap cache)
    return $! TokenLimitMap cache lcfg rtid expPolicy
  where
    reap = reaper logfun mapName

stopTokenLimitMap :: TokenLimitMap k -> IO ()
stopTokenLimitMap tlm = Async.cancel t `finally`
                        void (Async.waitCatch t)
  where
    t = _tlmReaper tlm

reaper
    :: (Eq k, Hashable k)
    => LogFunctionText
    -> Text
    -> Cache k v -> IO ()
reaper logfun mapName cache = runForever logfun mapName $ do
    approximateThreadDelay (2 * 60 * 1000000) -- two minute cycle time
    Cache.purgeExpired cache

getLimiter :: (Eq k, Hashable k) => k -> TokenLimitMap k -> IO RateLimiter
getLimiter k (TokenLimitMap cache limitPolicy _ cachePolicy) =
    Cache.lookup' cache k >>= maybe noKey return
  where
    expTSpec = policyExpirationTime cachePolicy
    addExpireTime = (+ expTSpec)
    noKey = do
        now <- Clock.getTime Clock.Monotonic
        rl <- TL.newRateLimiter limitPolicy
        atomically $ do
            mbV <- Cache.lookupSTM False k cache now
            maybe (do Cache.insertSTM k rl cache (Just $! addExpireTime now)
                      return rl)
                  (\rl' -> return rl')
                  mbV


withLimiter
    :: (Eq k, Hashable k)
    => k
    -> TokenLimitMap k
    -> (RateLimiter -> IO b)
    -> IO b
withLimiter k tlm act = getLimiter k tlm >>= act

penalize :: (Eq k, Hashable k) => Int -> k -> TokenLimitMap k -> IO Int
penalize ndebits k tlm = withLimiter k tlm $ \rl ->
                         TL.penalize rl ndebits

tryDebit :: (Eq k, Hashable k) => Int -> k -> TokenLimitMap k -> IO Bool
tryDebit ndebits k tlm = withLimiter k tlm $ \rl ->
                         TL.tryDebit (_tlmLimitPolicy tlm) rl ndebits

waitDebit :: (Eq k, Hashable k) => Int -> k -> TokenLimitMap k -> IO ()
waitDebit ndebits k tlm = withLimiter k tlm $ \rl ->
                          TL.waitDebit (_tlmLimitPolicy tlm) rl ndebits

defaultLimitConfig :: LimitConfig
defaultLimitConfig = TL.defaultLimitConfig

getLimitPolicy :: TokenLimitMap k -> LimitConfig
getLimitPolicy = _tlmLimitPolicy
