{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Network.Throttle
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Network.Throttle
( mkGenericThrottler
, checkPathPrefix
, mkThrottler

-- Chainweb Throttlers
, mkPutPeerThrottler
, mkLocalThrottler
, mkMiningThrottler
) where

import Control.Concurrent.TokenLimiter
import Control.Monad.Trans.Writer

import Data.Hashable
import Data.IORef
import Data.List (isPrefixOf)
import qualified Data.Text as T

import Network.Wai
import Network.Wai.Middleware.Throttle
import Network.Wai.Middleware.Throttle.Internal

import Numeric.Natural

import System.Clock

-- internal modules

import Data.BoundedHashMap

-- -------------------------------------------------------------------------- --
-- Throttling

mkGenericThrottler :: Double -> IO (Throttle Address)
mkGenericThrottler rate = mkThrottler 5 rate (const True)

checkPathPrefix
    :: [T.Text]
        -- ^ the base rate granted to users of the endpoing
    -> Request
    -> Bool
checkPathPrefix endpoint r = endpoint `isPrefixOf` drop 3 (pathInfo r)

-- | The period is 1 second. Burst is 2*rate.
--
mkThrottler
    :: Double
        -- ^ expiration of a stall bucket in seconds
    -> Double
        -- ^ the base rate granted to users of the endpoint (requests per second)
    -> (Request -> Bool)
        -- ^ Predicate to select requests that are throttled
    -> IO (Throttle Address)
mkThrottler e rate c = initThrottler (defaultThrottleSettings $ TimeSpec (ceiling e) 0) -- expiration
    { throttleSettingsRate = rate -- number of allowed requests per period
    , throttleSettingsPeriod = 1_000_000 -- 1 second
    , throttleSettingsBurst = 2 * ceiling rate
    , throttleSettingsIsThrottled = c
    }

mkRequestResponseThrottler
    :: Double
        -- ^ expiration of a stall bucket in seconds
    -> Double
        -- ^ the base rate granted to users of the endpoint (requests per second)
    -> (Request -> Response -> Bool)
        -- ^ Predicate to select requests that are throttled
    -> IO (Throttle Address)
mkRequestResponseThrottler e rate c = initThrottler (defaultThrottleSettings (TimeSpec (ceiling e) 0))
    { throttleSettingsRate = rate -- number of allowed requests per period
    , throttleSettingsPeriod = 1_000_000 -- 1 second
    , throttleSettingsBurst = 2 * ceiling rate
    , throttleSettingsIsThrottled = \req -> c req (error "TODO")
    }

-- |Run the throttling middleware given a throttle that has been initialized.
--
throttle' :: (Eq a, Hashable a) => Throttle a -> Application -> Application
throttle' th app req respond = do
  let settings = throttleSettings th
      getKey = throttleGetKey th
      isThrottled = throttleSettingsIsThrottled settings
      onThrottled = throttleSettingsOnThrottled settings
  if isThrottled req
    then app req respond
    else case getKey req of
      Left response -> respond response
      Right throttleKey -> do
        throttleRequest th throttleKey >>= \case
          0 -> runThrottled
          n -> respond $ onThrottled n
  where
    runThrottled = app req $ \r -> do

        respond r

-- -------------------------------------------------------------------------- --
--


-- |
--
-- 'RateLimiter' itself is thread safe. The 'IORef' is used to shield the
-- 'BoundedHashMap' from internal 'RateLimiter' changes. '_throttlerBuckets'
-- changes only when the set of 'RateLimiter's is updated.
--
-- Assuming that '_throttlerMaxEntries' is sufficiently large,
-- '_throttlerBuckets' should rarley change. When it does change clients race
-- and use 'atomicModifyIORef' to update it.
--
data Throttler a = Throttler
    { _throttlerSettings :: {-# UNPACK #-} !LimitConfig
    , _throttlerBuckets :: {-# UNPACK #-} !(IORef (BoundedHashMap a (IORef RateLimiter)))
    }

-- | j
--
-- When rates of less than one token per second is needed, one can increase the
-- price per token, by always debitting in quanities larger than 1.
--
newThrottler
    :: Eq a
    => Hashable a
    => Natural
        -- ^ Maximum number of recent keys (the actually allocated memory
        -- corresponds to up to twice this number)
    -> Int
        -- ^ initial credit. Using a negative value means that new keys
        -- have to wait for a while after knocking at the door.
    -> Natural
        -- ^ burst (capacity of bucket)
    -> Natural
        -- ^ rate (tokens per second)
    -> IO (Throttler a)
newThrottler maxKeys initialCredit burst rate = Throttler config <$> newIORef (new maxKeys)
  where
    config = defaultLimitConfig
        { maxBucketTokens = fromIntegral burst
        , initialBucketTokens = initialCredit
        , bucketRefillTokensPerSecond = fromIntegral rate
        }

debit :: Throttler k -> k -> Natural -> IO Bool
debit t k c = atomicModifyIORef (_throttlerBuckets k) $ \b -> do
    -- TODO make sure that "normally" the ptr to the buckets does not change
    n <- newIORef =<< newRateLimiter (_throttlerSettings t)
    runWriterT $ insertWithF f n k t
  where
    f _ ref = do
        x <- readIORef ref
        b <- tryDebit (_throttlerSettings t) x (Count c)
        tell b
        return ref

{-
-- -------------------------------------------------------------------------- --
-- Chainweb Throttlers

mkMiningThrottler :: Double -> IO (Throttle Address)
mkMiningThrottler rate = mkThrottler 5 rate (checkPathPrefix ["mining", "work"])

mkLocalThrottler :: Double -> IO (Throttle Address)
mkLocalThrottler rate = mkThrottler 5 rate (checkPathPrefix path)
  where
    path = ["pact", "api", "v1", "local"]

-- | At the start of each P2P sessions, PUTs itself into the peer db of the
-- node. The remote node validates the peer, which for unknown peers includes
-- checking connectivity.
--
-- A P2P session should last in the order of minutes. There are 21 P2P networks.
-- The default settings grant 21 requests per minute with a burst of 2x.
--
mkPutPeerThrottler :: Double -> IO (Throttle Address)
mkPutPeerThrottler rate =
    initThrottler (defaultThrottleSettings $ TimeSpec (ceiling @Double 120) 0) -- expiration
        { throttleSettingsRate = rate -- number of allowed requests per period
        , throttleSettingsPeriod = 60_000_000 -- 1 minute
        , throttleSettingsBurst = 2 * ceiling rate
        , throttleSettingsIsThrottled = \r ->
            elem "peer" (pathInfo r) && requestMethod r == "PUT"
        }
-}
