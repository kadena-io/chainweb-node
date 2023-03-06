{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Network.RateLimit
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module is based on the wai-middleware-throttle package and adds it
-- support for weights and response based rate limiting.
--
module Network.RateLimit
(
  -- * Wai request throttler
  ThrottleConfig(..)
, defaultThrottleConfig
, Throttle
, initIpThrottler
, initCustomThrottler
, throttle

-- * Low Level API

-- ** Rate Limiter
, Seconds(..)
, TokenRate(..)
, BurstFactor(..)
, TokenCount(..)
, create
, requestTokens
, penalizeTokens

-- ** Leaky Bucket
, Timestamp(..)
, Timespan(..)
, Level(..)
, Size(..)
, Disposal(..)
, Bucket
, createBucket
, request

-- ** BucketState
, BucketState
, updateBucketState


) where

import Control.Exception.Safe (onException)
import Control.Monad
import Control.Monad.STM (STM, atomically)

import Data.Cache (Cache, delete, insert, insertSTM, lookupSTM, newCache)
import Data.Hashable (Hashable, hashWithSalt)
import Data.IORef

import Network.HTTP.Types.Status (status429, status400)
import Network.Socket (SockAddr (SockAddrInet, SockAddrInet6, SockAddrUnix))
import Network.Wai (Application, Request, Response, remoteHost, responseLBS)

import System.Clock

-- Known Dependency lower bounds
-- cache >=0.1.1
-- network >=3.0.0

-- -------------------------------------------------------------------------- --
-- Rate Limiter
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Leaky Bucket Algorithm

-- | Current time measured in some unit from fixed epoch.
--
newtype Timestamp = Timestamp Double
    deriving (Show, Eq, Ord)

-- | Fill level of a bucket in time units.
--
-- On unit is the time that it takes the respective amount to drain from the
-- bucket.
--
newtype Level = Level Double
    deriving (Show, Eq, Ord)

-- | The size of the bucket in time units
--
newtype Size = Size Double
    deriving (Show, Eq, Ord)

-- | An quantity that is disposed into bucket. The quanity is measured in time
-- units as the time that it takes for the respective quantity to drain from the
-- bucket.
--
newtype Disposal = Disposal Double
    deriving (Show, Eq, Ord)

-- | Timespan, the difference of 'Timestamp's.
--
-- Used to notify the a user how long they would have to wait until there is
-- enough room in the bucket to support a requested disposal.
--
newtype Timespan = Timespan Double
    deriving (Show, Eq, Ord)

-- | Bucket State that tracks the current fill level of the bucket and a
-- specific time.
--
-- Everything related to a bucket is measured in abstract time units. On unit is
-- the time that it takes for the respective quanity to drain from the bucket.
--
-- In applications interactions with the bucket are usually performed in terms
-- of tokens, which respresent a certain amount of time units.
--
-- Typically, the time unit is based on some clock that measures time spans in
-- some unit, e.g. seconds. The buckets leaks at a rate that is measued in
-- tokens per time unit. A user requests a certain amount of tokens at the time.
-- The size of the bucket is modeles the number of tokens that a user can
-- request concurrently, or alternativly a burst factor, which describes how
-- many tokens a user can request at once or in fast succession before exhausing
-- the fill limit of the bucket. An example of such a use case is provided by
-- the rate limiter implementation in this module.
--
data BucketState = BucketState !Timestamp !Level

-- | Update the bucket state for the disposal of a given quantity.
--
updateBucketState
    :: Timestamp
        -- ^ The current time at which the disposal occurs
    -> Size
        -- ^ Size of the bucket, i.e. maximum allowed fill level for this
        -- disposal request
    -> Disposal
        -- ^ The number of units that are requested for disposal
    -> BucketState
        -- ^ The bucket state
    -> (BucketState, Maybe Timespan)
        -- ^ @Just@ the number missing units. @Just 0@ for success. @Nothing@ is
        -- returned in case the the request is larger than the available size,
        -- in which case the request will never succeed.
updateBucketState n@(Timestamp now) (Size size) (Disposal req) b@(BucketState (Timestamp t) (Level old))
    -- request is rejected and  bucket state is unchanged
    | req > size = (b, Nothing)
    -- request fails
    | new > size = (BucketState n (Level cur), Just (Timespan $ new - size))
    -- request succeeds
    | otherwise = (BucketState n (Level new), Just (Timespan 0))
  where
    cur = max 0 (old + t - now) -- current level
    new = cur + req -- requested new level

-- -------------------------------------------------------------------------- --
-- Mutable Bucket

-- | Leaky Bucket that can be used for rate limiting.
--
-- The content of the bucket leaks a a constant rate out of the bucket. The
-- content is measured in units is time that it takes the respective quantity of
-- the content of drain from the bucket.
--
-- Users can dispose some amount of content into the bucket up some maximal fill
-- level. If there is not enough room in the bucket left and the additional of
-- the diposed content would exeed the fill limit, the user has to wait unitl
-- enough content is drained from bucket in order to make enough room for the
-- new content.
--
-- It is not possible to dispose a larger quantity at once into the bucket than
-- the totoal capacity of the bucket up to the maximum fill level. Even if the
-- bucket drained completely and was empty, it would still not be able to hold
-- the quantity. Such a request is rejected right away.
--
-- For the bucket of in this implementation the fill level is adjustable for
-- each disposal request. In particular it is possible to specify an infinitly
-- large bucket size, in which case arbitrary large quantities can be disposed
-- in a single request. This means that subsequent requests with smaller fill
-- fill limits may have to wait a longer time until enough space becomes
-- available in the bucket.
--
newtype Bucket = Bucket (IORef BucketState)

-- | Create a new leaky bucket
--
-- If the bucket is initially empty a new client would be allowed start with a
-- burst, in which case the bucket should be cached sufficiently long. If the
-- bucket is intially full, it would mean that a new client would always be
-- throttled first and would have to wait before gaining access to the service.
-- That is similar to grey-listing.
--
createBucket :: Timestamp -> Level -> IO Bucket
createBucket now initialLevel = do
    Bucket <$> newIORef (BucketState now initialLevel)

request
    :: Bucket
        -- ^ The bucket
    -> Timestamp
        -- ^ The current time at which the requested disposal occurs
    -> Size
        -- ^ The size of the bucket, i.e. the maximum allowed fill level for this
        -- disposal request. This can be chosen on a per request base
    -> Disposal
        -- ^ The number of units that are requested for disposal
    -> IO (Maybe Timespan)
        -- ^ @Just@ the number of missing units. @Just 0@ for success. @Nothing@
        -- is returned in case the the request is larger than the requested
        -- size, in which case the request will never succeed.
request (Bucket ref) now size@(Size s) req@(Disposal r)
    | r > s = return Nothing -- short cicuit in case the request is rejected
    | otherwise = do
        atomicModifyIORef' ref $ updateBucketState now size req

-- -------------------------------------------------------------------------- --
-- High Level Rate Limiting Interface

-- | Represents a number of seconds
--
newtype Seconds = Seconds Double
    deriving (Show, Eq, Ord)

-- | Tokens per second
--
newtype TokenRate = TokenRate Double

-- | Count of tokens
--
newtype TokenCount = TokenCount Double

-- | Burst factor in numnber of tokens
--
newtype BurstFactor = BurstFactor Double

-- | The current time measured in seconds since some fixed epoch
--
getTimestamp :: IO Timestamp
getTimestamp = toSeconds <$> getTime Monotonic
  where
    toSeconds (TimeSpec s n) = Timestamp $! fromIntegral s + fromIntegral n / s2ns

-- | Create an empty bucket. This means that the maximum capacity is available
-- right from the start.
--
-- In the bucket one time unit corresponds to one second.
--
create :: IO Bucket
create = do
    now <- getTimestamp
    createBucket now (Level 0)

-- | Request the disposal of a number of tokens at a given rate. If the number
-- of requested tokens is not available at the given rate at the moment, the
-- result indicates the time to wait until the request can be fullfilled.
-- @Nothing@ is returned if the request is rejected because it exceeds the
-- available capacity.
--
-- Time is measured in seconds.
--
requestTokens
    :: Bucket
    -> BurstFactor
        -- ^ The burst factor for this request. This indicates the maximum level
        -- of concurrent requests that can be served on an empty bucket. This
        -- must be larger than the number of requested tokens. Otherwise the
        -- request is rejected.
    -> TokenRate
        -- The rate (in tokens per second) at which space becomes available for
        -- disposing tokens. Usually, this is a constant for a given bucket.
    -> TokenCount
        -- ^ The number of requested tokens. This can not be larger than the
        -- burst factor, otherwise the request is rejecthed.
    -> IO (Maybe Seconds)
        -- ^ @Just@ the time in seconds until retry. @Just 0@ in case of
        -- success. @Nothing@ is returned when the number of tokens for which
        -- disposal is requested is larger than the burst factor.
requestTokens bucket (BurstFactor burst) (TokenRate rate) (TokenCount count) = do
    now <- getTimestamp
    result <- request bucket now (Size $ burst / rate) (Disposal $ count / rate)
    return $! asSeconds <$!> result
  where
    asSeconds (Timespan s) = Seconds s

-- | Dispose an unlimted amount of tokens. The size of the bucket (burst factor)
-- is adjusted such that this request will always succeed. This represents a
-- penality for subsequent requests.
--
penalizeTokens
    :: Bucket
    -> TokenRate
        -- The rate (in tokens per second) at which space becomes available for
        -- disposing tokens. Usually, this is a constant for a given bucket.
    -> TokenCount
        -- ^ Number of requested tokens that are disposed into the bucket.
    -> IO ()
penalizeTokens bucket rate tokens = void $ requestTokens bucket infinity rate tokens
  where
    infinity = BurstFactor (1/0)
        -- FIXME round (1/0) results something very large, which should be fine.
        -- But is that guaranteed, or is it implementation specific?

-- -------------------------------------------------------------------------- --
-- Wai Request Throttler
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Address

newtype Address = Address SockAddr

instance Hashable Address where
    hashWithSalt s (Address (SockAddrInet _ a)) = hashWithSalt s a
    hashWithSalt s (Address (SockAddrInet6 _ _ a _)) = hashWithSalt s a
    hashWithSalt s (Address (SockAddrUnix a)) = hashWithSalt s a

instance Eq Address where
    Address (SockAddrInet _ a) == Address (SockAddrInet _ b) = a == b
    Address (SockAddrInet6 _ _ a _) == Address (SockAddrInet6 _ _ b _) = a == b
    Address (SockAddrUnix a) == Address (SockAddrUnix b) = a == b
    _ == _ = False -- not same constructor so cant be equal

instance Ord Address where
    Address (SockAddrInet _ a) <= Address (SockAddrInet _ b) = a <= b
    Address (SockAddrInet6 _ _ a _) <= Address (SockAddrInet6 _ _ b _) = a <= b
    Address (SockAddrUnix a) <= Address (SockAddrUnix b) = a <= b
    Address a <= Address b = a <= b -- not same constructor so use builtin ordering

extractAddress :: Request -> Either Response Address
extractAddress = Right . Address . remoteHost

-- -------------------------------------------------------------------------- --
-- Cache

data CacheState a
    = CacheStatePresent a
    | CacheStateInitializing

data CacheResult a
    = CacheResultExists a
    | CacheResultEmpty

-- -------------------------------------------------------------------------- --
-- ThrottleConfig

-- | Throttle config for controlling token bucket algorithm and cache
-- expiration.
--
data ThrottleConfig = ThrottleConfig
  { _throttleConfigRate :: !TokenRate

  , _throttleConfigBurstFactor :: !BurstFactor
    -- ^ Number of tokens can that can disposed in a single request. Or,
    -- alternatively, the maximum allowed number of concurrent requests.
    -- Defaults to 1. Should be larger than largest possible weight.

  , _throttleConfigCacheExpiration :: !TimeSpec
    -- ^ The amount of time before a stale token bucket is purged from the
    -- cache.
    --
    -- This number should be significantly larger than the burst factor.
    -- Otherwise a client can increase by pass the rate limit by making pause
    -- that cause its bucket to expire and be initialize new bucket with a fresh
    -- burst buffer.
    --

  , _throttleConfigGetRequestWeight :: !(Request -> Double)
    -- ^ Throttle weight of the request. Default is @(const 1)@
    --
    -- A value of 0 means that it isn't subject to throttling.

  , _throttleConfigGetResponseWeight :: !(Response -> Double)
    -- ^ The rate limiting weight of the response. Default is @(const 0)@.
    --
    -- This does not affect the current request, but penalizes subsequent
    -- requests by reducing the amout of available tokens.

  , _throttleConfigOnThrottled :: !(Seconds -> Response)
    -- ^ The response when a request is throttled - defaults to a vanilla 429

  , _throttleConfigOnReject :: !Response
    -- ^ Resposne when a request is permanently rejected, because it exceeds the
    -- capacity of the service. Defaults to 400.
  }

-- | Default throttle config.
--
defaultThrottleConfig :: TimeSpec -> ThrottleConfig
defaultThrottleConfig expirationInterval = ThrottleConfig
  { _throttleConfigRate = TokenRate 1 -- One request per second
  , _throttleConfigBurstFactor = BurstFactor 1
  , _throttleConfigCacheExpiration = expirationInterval
  , _throttleConfigGetRequestWeight = const 1 -- each request has weight 1
  , _throttleConfigGetResponseWeight = const 0 -- ignore response
  , _throttleConfigOnThrottled = const $
      responseLBS status429 [("Content-Type", "application/json")] "{\"message\":\"Too many requests.\"}"
      -- TODO: set retry-after header?
  , _throttleConfigOnReject =
      responseLBS status400 [("Content-Type", "application/json")] "{\"message\":\"Permanently rejected. Request exeeds limits.\"}"
  }

-- -------------------------------------------------------------------------- --
-- Throttle

-- | A throttle for a hashable key type. Initialize using 'initThrottler' with
-- 'defaultThrottleConfig'.
--
data Throttle a = Throttle
  { _throttleConfig :: !ThrottleConfig
  , _throttleCache :: !(Cache a (CacheState Bucket))
  , _throttleGetKey :: !(Request -> Either Response a)
  }

-- | Throttle based on IP Address
--
initIpThrottler :: ThrottleConfig -> IO (Throttle Address)
initIpThrottler = flip initCustomThrottler extractAddress

-- | Throttle for custom key types
--
initCustomThrottler
    :: ThrottleConfig
    -> (Request -> Either Response a)
    -> IO (Throttle a)
initCustomThrottler config throttleGetKey = do
    throttleCache <- newCache $ Just (_throttleConfigCacheExpiration config)
    return Throttle
        { _throttleConfig = config
        , _throttleCache = throttleCache
        , _throttleGetKey = throttleGetKey
        }

-- -------------------------------------------------------------------------- --
-- Internal

-- | Retrieve a token bucket from the cache.
--
retrieveCache
    :: Eq a
    => Hashable a
    => Throttle a
    -> TimeSpec
    -> a
    -> STM (CacheResult Bucket)
retrieveCache th time throttleKey = do
    lookupSTM True throttleKey cache time >>= \ case
      Just (CacheStatePresent oldBucket) -> pure $ CacheResultExists oldBucket
      Just CacheStateInitializing -> retrieveCache th time throttleKey
      Nothing -> do
          insertSTM throttleKey CacheStateInitializing cache Nothing
          pure CacheResultEmpty
  where
    cache = _throttleCache th

-- | Create a token bucket if it wasn't in the cache.
--
processCacheResult
    :: Eq a
    => Hashable a
    => Throttle a
    -> a
    -> CacheResult Bucket
    -> IO Bucket
processCacheResult _ _ (CacheResultExists b) = return b
processCacheResult t k CacheResultEmpty =
    initializeBucket `onException` delete cache k
  where
    cache = _throttleCache t
    initializeBucket = do
        bucket <- create
        insert cache k (CacheStatePresent bucket)
        return bucket

-- | Retrieve or initialize a token bucket depending on if it was found in the
-- cache.
--
retrieveOrInitializeBucket
    :: Eq a
    => Hashable a
    => Throttle a
    -> a
    -> IO Bucket
retrieveOrInitializeBucket t k = do
    now <- getTime Monotonic
    cacheResult <- atomically $ retrieveCache t now k
    processCacheResult t k cacheResult

-- | Throttle by the throttle key.
--
throttleRequest
    :: Eq a
    => Hashable a
    => Throttle a
    -> a
        -- ^ key
    -> Double
        -- ^ weight
    -> IO (Maybe Seconds)
        -- ^ Just the number of seconds to wait for the request to succeed; Just
        -- 0 in case of success. When the weight is  so large that the request
        -- would never succeed Nothin is returned.
throttleRequest t k weight = do
    bucket <- retrieveOrInitializeBucket t k
    requestTokens bucket burst rate (TokenCount weight)
  where
    conf = _throttleConfig t
    rate = _throttleConfigRate conf
    burst = _throttleConfigBurstFactor conf

-- | Penalize future requests by the throttle key.
--
penalizeResponse
    :: Eq a
    => Hashable a
    => Throttle a
    -> a
        -- ^ key
    -> Double
        -- ^ weight
    -> IO ()
penalizeResponse t k weight = do
    bucket <- retrieveOrInitializeBucket t k
    penalizeTokens bucket (_throttleConfigRate (_throttleConfig t)) (TokenCount weight)

-- -------------------------------------------------------------------------- --
-- Throttleling middleware

-- | Run the throttling middleware given a throttle that has been initialized.
--
throttle :: (Eq a, Hashable a) => Throttle a -> Application -> Application
throttle t app req respond = do
    case _throttleConfigGetRequestWeight conf req of
        0 -> app req respond
        w -> case _throttleGetKey t req of
            Left response -> respond response
            Right k -> throttleRequest t k w >>= \case
                Just (Seconds 0) -> do
                  app req $ \resp -> do
                      case _throttleConfigGetResponseWeight conf resp of
                          0 -> return ()
                          respWeight -> penalizeResponse t k respWeight
                      respond resp
                Just n -> respond $ _throttleConfigOnThrottled conf n
                Nothing -> respond $ _throttleConfigOnReject conf
  where
    conf = _throttleConfig t

