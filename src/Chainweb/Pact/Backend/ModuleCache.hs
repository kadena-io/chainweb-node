{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.Backend.ModuleCache
( ModuleCache
, checkModuleCache
, emptyCache
, size
, count
, isEmpty
, moduleCacheStats
) where

import Control.Lens hiding ((.=))
import Control.Monad.State.Strict

import qualified Crypto.Hash as C (hash)
import Crypto.Hash.Algorithms

import Data.Aeson (Value, object, decodeStrict', (.=))
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.List (sort)

import Database.SQLite3.Direct

import Pact.Types.Persistence
import Pact.Types.SizeOf

-- -------------------------------------------------------------------------- --
-- CacheAddress

newtype CacheAddress = CacheAddress ByteString
    deriving (Show,Eq,Ord,Hashable)

-- -------------------------------------------------------------------------- --
-- CacheEntry

data CacheEntry = CacheEntry
  { _ceTxId :: !TxId
    -- ^ Priority for cache eviction
  , _ceAddy :: !CacheAddress
    -- ^ Key. Uniquly identifieds entries in the cache
  , _ceSize :: !Int64
    -- ^ The size of the entry. Use to keep track of the cache limit
  , _ceData :: !PersistModuleData
    -- ^ Cached data.
  , _ceHits :: Int
    -- ^ Count of cache hits for this entry
  }
  deriving (Show)

ceTxId :: Lens' CacheEntry TxId
ceTxId = lens _ceTxId (\a b -> a { _ceTxId = b })

ceHits :: Lens' CacheEntry Int
ceHits = lens _ceHits (\a b -> a { _ceHits = b })

instance Eq CacheEntry where
  a == b = _ceAddy a == _ceAddy b && _ceTxId a == _ceTxId b

-- | Sort first by '_ceTxId', so that smaller tx ids are evicted first from the
-- cache.
--
instance Ord CacheEntry where
  a `compare` b = cmp _ceTxId (cmp _ceAddy EQ)
    where
      cmp acc next = case acc a `compare` acc b of
        EQ -> next
        c -> c

-- -------------------------------------------------------------------------- --
-- ModuleCache

data ModuleCache = ModuleCache
  { _mcStore :: !(HM.HashMap CacheAddress CacheEntry)
  , _mcSize :: !Int64
  , _mcLimit :: !Int64

  -- Telemetry
  , _mcMisses :: !Int
  , _mcHits :: !Int
  }
  deriving (Show)

makeLenses 'ModuleCache

emptyCache :: Int64 -> ModuleCache
emptyCache limit = ModuleCache
    { _mcStore = mempty
    , _mcSize = 0
    , _mcLimit = limit
    , _mcMisses = 0
    , _mcHits = 0
    }

isEmpty :: ModuleCache -> Bool
isEmpty = HM.null . _mcStore

-- | The totall size of all modules in the cache.
--
-- Complexity: \(O(1)\)
--
size :: ModuleCache -> Int
size = fromIntegral . _mcSize

-- | Number of modules in the cache.
--
-- Complexity: \(O(n)\), linear in the number of entries
--
count :: ModuleCache -> Int
count = HM.size . _mcStore

-- | Decode Module from row data using the module cache.
--
checkModuleCache
    :: Utf8
        -- ^ Row Key for the module
    -> ByteString
        -- ^ row data that contains the encoded module
    -> TxId
        -- ^ TxId. This is used as priority for cache eviction. Smaller values
        -- are evicted first.
    -> ModuleCache
    -> (Maybe PersistModuleData, ModuleCache)
checkModuleCache key rowdata txid = runState $ do
    readModuleCache txid addy >>= \case

        -- Cache hit
        Just !x -> do
            modify' (mcHits +~ 1)
            return $ Just x

        -- Cache miss: decode module and insert into cache
        Nothing -> case decodeStrict' rowdata of
            Nothing -> return Nothing
            Just !m -> do
                writeModuleCache txid addy m
                modify' (mcMisses +~ 1)
                return $ Just m
  where
    addy = mkAddress key rowdata

moduleCacheStats :: ModuleCache -> Value
moduleCacheStats mc = object
    [ "misses" .= _mcMisses mc
    , "hits" .= _mcHits mc
    , "size" .=  size mc
    , "count" .= count mc
    ]

-- -------------------------------------------------------------------------- --
-- Internal

mkAddress :: Utf8 -> ByteString -> CacheAddress
mkAddress (Utf8 key) rowdata = CacheAddress $
    key <> ":" <> BA.convert (C.hash @_ @SHA512t_256 rowdata)

readModuleCache
    :: TxId
        -- ^ TxId of
    -> CacheAddress
    -> State ModuleCache (Maybe PersistModuleData)
readModuleCache txid ca = do
    mc <- use mcStore
    forM (HM.lookup ca mc) $ \e -> do
        modify'
            $ (mcStore . ix ca . ceTxId .~ txid)
            . (mcStore . ix ca . ceHits +~ 1)
        return $! _ceData e

-- | Add item to module cache
--
writeModuleCache
    :: TxId
    -> CacheAddress
    -> PersistModuleData
    -> State ModuleCache ()
writeModuleCache txid ca v = do
    modify'
        $ (mcStore %~ HM.insert ca (CacheEntry txid ca sz v 0))
        . (mcSize +~ sz)
    maintainCache
  where
    sz = sizeOf SizeOfV1 $ void (_mdModule v)

-- | Prune Cache to meet size limit
--
-- Complexity: \(O(n \log(n))\) in the number of entries
--
maintainCache :: State ModuleCache ()
maintainCache = do
    sz <- use mcSize
    lim <- use mcLimit
    unless (sz < lim) $ do
        vals <- sort . HM.elems <$> use mcStore
        evict sz lim vals
  where
    evict _ _ [] = return ()
    evict sz lim (e:es)
        | sz < lim = return ()
        | otherwise = do
            let sz' = sz - _ceSize e
            modify'
                $ (mcStore %~ HM.delete (_ceAddy e))
                . (mcSize .~ sz')
            evict sz' lim es
