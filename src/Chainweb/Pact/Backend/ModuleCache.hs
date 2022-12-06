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
) where

import Control.Monad.State.Strict

import Database.SQLite3.Direct
import Data.ByteString (ByteString)
import Pact.Types.Persistence
import Control.Lens
import Data.Aeson (decodeStrict')
import qualified Crypto.Hash as C (hash)
import Crypto.Hash.Algorithms
import qualified Data.ByteArray as BA
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Int
import Pact.Types.SizeOf
import Data.List (sort)

-- -------------------------------------------------------------------------- --
-- CacheAddress

newtype CacheAddress = CacheAddress ByteString
    deriving (Show, Eq,Ord,Hashable)

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
  }
  deriving (Show)

ceTxId :: Lens' CacheEntry TxId
ceTxId = lens _ceTxId (\a b -> a { _ceTxId = b })

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
  }
  deriving (Show)

makeLenses 'ModuleCache

emptyCache :: Int64 -> ModuleCache
emptyCache limit = ModuleCache
    { _mcStore = mempty
    , _mcSize = 0
    , _mcLimit = limit
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
        Just !x -> return $ Just x

        -- Cache miss: decode module and insert into cache
        Nothing -> case decodeStrict' rowdata of
            Nothing -> return Nothing
            Just !m -> do
                writeModuleCache txid addy m
                return $ Just m
  where
    addy = mkAddress key rowdata

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
      mcStore . ix ca . ceTxId .= txid
      return $! _ceData e

-- | Add item to module cache
--
writeModuleCache
    :: TxId
    -> CacheAddress
    -> PersistModuleData
    -> State ModuleCache ()
writeModuleCache txid ca v = do
  let sz = sizeOf SizeOfV1 $ void (_mdModule v)
  mcStore %= HM.insert ca (CacheEntry txid ca sz v)
  mcSize %= (+ sz)
  maintainCache

-- | Prune Cache to meet size limit
--
-- Complexity: \(O(n \log(n))\) in the number of entries
--
maintainCache :: State ModuleCache ()
maintainCache = do
    sz <- use mcSize
    lim <- use mcLimit
    e <- gets isEmpty
    unless (e || sz < lim) $ do
        vals <- sort . HM.elems <$> use mcStore
        evict sz lim vals
  where
    evict _ _ [] = return ()
    evict sz lim (e:es)
      | sz < lim = return ()
      | otherwise = do
          mcStore %= HM.delete (_ceAddy e)
          let sz' = sz - _ceSize e
          mcSize .= sz'
          evict sz' lim es
