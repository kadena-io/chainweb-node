{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Pact.Backend.DbCache
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: See LICENSE file
--
-- LRU cache for avoiding expensive JSON deserialization.
--
module Chainweb.Pact.Backend.DbCache
( DbCacheLimitBytes(..)
, DbCache
, checkDbCache
, emptyDbCache
, cacheSize
, cacheCount
, isEmptyCache
, cacheStats
, updateCacheStats

  -- * Telemetry
, DbCacheStats(..)
) where

import Control.DeepSeq (NFData(..))
import Control.Lens hiding ((.=))
import Control.Monad (forM, unless)
import Control.Monad.State.Strict

import qualified Crypto.Hash as C (hash)
import Crypto.Hash.Algorithms

import Data.Aeson (FromJSON, ToJSON(..), (.=), object)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as BS
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import Data.Ord

import Database.SQLite3.Direct

import GHC.Compact
import GHC.Stack

import Numeric.Natural

import Pact.Types.Persistence

-- internal modules

import Chainweb.Utils (ix')


-- -------------------------------------------------------------------------- --
-- ModuleCacheLimitBytes

newtype DbCacheLimitBytes = DbCacheLimitBytes Natural
    deriving (Show, Read, Eq, Ord, ToJSON, FromJSON)

-- -------------------------------------------------------------------------- --
-- CacheAddress

newtype CacheAddress = CacheAddress BS.ShortByteString
    deriving (Show,Eq,Ord,Hashable)

-- -------------------------------------------------------------------------- --
-- CacheEntry

data CacheEntry a = CacheEntry
  { _ceTxId :: !TxId
    -- ^ Priority for cache eviction
  , _ceAddy :: !CacheAddress
    -- ^ Key. Uniquely identifies entries in the cache
  , _ceSize :: !Int
    -- ^ The size of the entry. Used to keep track of the cache limit
  , _ceData :: !a
    -- ^ Cached data. This is stored in a compact region.
  , _ceHits :: !Int
    -- ^ Count of cache hits for this entry
  }

ceTxId :: Lens' (CacheEntry a) TxId
ceTxId = lens _ceTxId (\a b -> a { _ceTxId = b })

ceHits :: Lens' (CacheEntry a) Int
ceHits = lens _ceHits (\a b -> a { _ceHits = b })

instance Eq (CacheEntry a) where
  a == b = _ceAddy a == _ceAddy b && _ceTxId a == _ceTxId b

-- | Sort by '_ceTxId' so that smaller tx ids are evicted first from the
-- cache. '_ceAddy' is just for stable sort.
--
instance Ord (CacheEntry a) where
  a `compare` b = comparing _ceTxId a b <> comparing _ceAddy a b

-- | Cache entries are stored within a compact regions. The whole region is GCed
-- only when no pointer to any data in the region is live any more.
--
compactCacheEntry :: MonadIO m => a -> m (a, Int)
compactCacheEntry d = do
    (c, s) <- liftIO $ do
        c <- compact d
        s <- compactSize c
        pure (c, s)
    return (getCompact c, fromIntegral s)

-- -------------------------------------------------------------------------- --
-- DbCache

data DbCache a = DbCache
    { _dcStore :: !(HM.HashMap CacheAddress (CacheEntry a))
    , _dcSize :: !Int
    , _dcLimit :: !Int

    -- Telemetry
    , _dcMisses :: !Int
    , _dcHits :: !Int
    }

makeLenses 'DbCache

emptyDbCache :: HasCallStack => DbCacheLimitBytes -> DbCache a
emptyDbCache (DbCacheLimitBytes limit)
    | fromIntegral limit > maxBound @Int = error "cache limit is too large"
emptyDbCache (DbCacheLimitBytes limit) = DbCache
    { _dcStore = mempty
    , _dcSize = 0
    , _dcLimit = fromIntegral limit
    , _dcMisses = 0
    , _dcHits = 0
    }

isEmptyCache :: DbCache a -> Bool
isEmptyCache = HM.null . _dcStore

-- | The total size of all data in the cache.
--
-- Complexity: \(O(1)\)
--
cacheSize :: DbCache a -> Int
cacheSize = _dcSize

-- | Number of entries in the cache.
--
-- Complexity: \(O(n)\), linear in the number of entries
--
cacheCount :: DbCache a -> Int
cacheCount = HM.size . _dcStore

-- | Provide read-time caching of deserialized JSON values.
-- If cache entry is found, mark it with the current txid and return value.
-- If not, deserialize row data and store in cache, which triggers
-- cache maintenance.
--
checkDbCache
    :: Utf8
        -- ^ Db key for data
    -> (ByteString -> Maybe a)
        -- ^ row data's decoding function
    -> ByteString
        -- ^ row data that contains the encoded value
    -> TxId
       -- ^ Current TxId from checkpointer, used as priority for cache eviction.
       -- Smaller values are evicted first.
    -> DbCache a
    -> IO (Maybe a, DbCache a)
checkDbCache key f rowdata txid = runStateT $ do
    readCache txid addy >>= \case

        -- Cache hit
        Just !x -> do
            modify' (dcHits +~ 1)
            return $ Just x

        -- Cache miss: decode module and insert into cache
        Nothing -> case f rowdata of
            Nothing -> return Nothing
            Just v -> do
                (m, s) <- compactCacheEntry v
                writeCache txid addy s m
                modify' (dcMisses +~ 1)
                return $ Just m
  where
    addy = mkAddress key rowdata

cacheStats :: DbCache a -> DbCacheStats
cacheStats mc = DbCacheStats
  { _dbCacheStatsSize = cacheSize mc
  , _dbCacheStatsCount = cacheCount mc
  , _dbCacheStatsHits = _dcHits mc
  , _dbCacheStatsMisses = _dcMisses mc
  }

-- | Return stats about the current cache, and return a cache
--   with Hits/Misses set to 0.
updateCacheStats :: DbCache a -> (DbCacheStats, DbCache a)
updateCacheStats mc = (cacheStats mc, set dcMisses 0 (set dcHits 0 mc))

-- -------------------------------------------------------------------------- --
-- Internal

mkAddress :: Utf8 -> ByteString -> CacheAddress
mkAddress (Utf8 key) rowdata = CacheAddress . BS.toShort $
    key <> ":" <> BA.convert (C.hash @_ @SHA512t_256 rowdata)

readCache
    :: TxId
        -- ^ Current TxId from checkpointer
    -> CacheAddress
    -> StateT (DbCache a) IO (Maybe a)
readCache txid ca = do
    mc <- use dcStore
    forM (HM.lookup ca mc) $ \e -> do
        modify'
            $ (dcStore . ix' ca . ceTxId .~ txid)
            . (dcStore . ix' ca . ceHits +~ 1)
        return $ _ceData e

-- | Add item to module cache
--
writeCache
    :: TxId
    -> CacheAddress
    -> Int
    -> a
    -> StateT (DbCache a) IO ()
writeCache txid ca sz v = do
    modify'
        $ (dcStore %~ HM.insert ca (CacheEntry txid ca sz v 0))
        . (dcSize +~ sz)
    maintainCache

-- | Prune Cache to meet size limit
--
-- Complexity: \(O(n \log(n))\) in the number of entries
--
maintainCache :: Monad m => StateT (DbCache a) m ()
maintainCache = do
    sz <- use dcSize
    lim <- use dcLimit
    unless (sz < lim) $ do
        vals <- sort . HM.elems <$> use dcStore
        evict sz lim vals
  where
    evict _ _ [] = return ()
    evict sz lim (e:es)
        | sz < lim = return ()
        | otherwise = do
            let sz' = sz - _ceSize e
            modify'
                $ (dcStore %~ HM.delete (_ceAddy e))
                . (dcSize .~ sz')
            evict sz' lim es

data DbCacheStats = DbCacheStats
  { _dbCacheStatsSize :: !Int
  , _dbCacheStatsCount :: !Int
  , _dbCacheStatsHits :: !Int
  , _dbCacheStatsMisses :: !Int
  }
  deriving stock (Eq, Show)

instance NFData DbCacheStats where
  rnf (DbCacheStats sz count hits misses)
    = rnf sz `seq` rnf count `seq` rnf hits `seq` rnf misses

instance ToJSON DbCacheStats where
  toJSON (DbCacheStats sz count hits misses) = object
    [ "size" .= sz
    , "count" .= count
    , "hits" .= hits
    , "misses" .= misses
    ]
