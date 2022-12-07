{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
( DbCache
, checkDbCache
, emptyDbCache
, cacheSize
, cacheCount
, isEmptyCache
) where


import Control.Lens
import Control.Monad.State.Strict
import Crypto.Hash.Algorithms
import qualified Crypto.Hash as C (hash)
import Data.Aeson (decodeStrict',FromJSON)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import Database.SQLite3.Direct

import Pact.Types.Persistence

-- -------------------------------------------------------------------------- --
-- CacheAddress

newtype CacheAddress = CacheAddress ByteString
    deriving (Show,Eq,Ord,Hashable)

-- -------------------------------------------------------------------------- --
-- CacheEntry

data CacheEntry a = CacheEntry
  { _ceTxId :: !TxId
    -- ^ Priority for cache eviction
  , _ceAddy :: !CacheAddress
    -- ^ Key. Uniquly identifieds entries in the cache
  , _ceSize :: !Int
    -- ^ The size of the entry. Use to keep track of the cache limit
  , _ceData :: !a
    -- ^ Cached data.
  }
  deriving (Show)

ceTxId :: Lens' (CacheEntry a) TxId
ceTxId = lens _ceTxId (\a b -> a { _ceTxId = b })

instance Eq (CacheEntry a) where
  a == b = _ceAddy a == _ceAddy b && _ceTxId a == _ceTxId b

-- | Sort first by '_ceTxId', so that smaller tx ids are evicted first from the
-- cache.
--
instance Ord (CacheEntry a) where
  a `compare` b = cmp _ceTxId (cmp _ceAddy EQ)
    where
      cmp acc next = case acc a `compare` acc b of
        EQ -> next
        c -> c

-- -------------------------------------------------------------------------- --
-- DbCache

data DbCache a = DbCache
  { _dcStore :: !(HM.HashMap CacheAddress (CacheEntry a))
  , _dcSize :: !Int
  , _dcLimit :: !Int
  }
  deriving (Show)

makeLenses 'DbCache

emptyDbCache :: Int -> DbCache a
emptyDbCache limit = DbCache
    { _dcStore = mempty
    , _dcSize = 0
    , _dcLimit = limit
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

-- | Decode Module from row data using the module cache.
--
checkDbCache
    :: FromJSON a
    => Utf8
        -- ^ Db key for data
    -> ByteString
        -- ^ row data that contains the encoded value
    -> TxId
       -- ^ Current TxId from checkpointer, used as priority for cache eviction.
       -- Smaller values are evicted first.
    -> DbCache a
    -> (Maybe a, DbCache a)
checkDbCache key rowdata txid = runState $ do
    readCache txid addy >>= \case

        -- Cache hit
        Just !x -> return $ Just x

        -- Cache miss: decode module and insert into cache
        Nothing -> case decodeStrict' rowdata of
            Nothing -> return Nothing
            Just !m -> do
                writeCache txid addy (BS.length rowdata) m
                return $ Just m
  where
    addy = mkAddress key rowdata

-- -------------------------------------------------------------------------- --
-- Internal

mkAddress :: Utf8 -> ByteString -> CacheAddress
mkAddress (Utf8 key) rowdata = CacheAddress $
    key <> ":" <> BA.convert (C.hash @_ @SHA512t_256 rowdata)

readCache
    :: TxId
        -- ^ Current TxId from checkpointer
    -> CacheAddress
    -> State (DbCache a) (Maybe a)
readCache txid ca = do
    dc <- use dcStore
    forM (HM.lookup ca dc) $ \e -> do
        modify' $ dcStore . ix ca . ceTxId .~ txid
        return $! _ceData e

-- | Add item to module cache
--
writeCache
    :: TxId
    -> CacheAddress
    -> Int
    -> a
    -> State (DbCache a) ()
writeCache txid ca sz v = do
    modify' $ dcStore %~ HM.insert ca (CacheEntry txid ca sz v)
    modify' $ dcSize +~ sz
    maintainCache

-- | Prune Cache to meet size limit
--
-- Complexity: \(O(n \log(n))\) in the number of entries
--
maintainCache :: State (DbCache a) ()
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
            modify' $ dcStore %~ HM.delete (_ceAddy e)
            let sz' = sz - _ceSize e
            modify' $ dcSize .~ sz'
            evict sz' lim es
