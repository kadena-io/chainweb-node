{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.Backend.ModuleCache
( checkModuleCache
, ModuleCache(..)
, emptyCache
, ceSize
, ceData
, ceAddy
, ceTxId
) where

import Control.Monad.State

import Database.SQLite3.Direct
import Data.ByteString (ByteString)
import Control.Monad.Trans.Maybe
import Pact.Types.Persistence
import Control.Lens
import Control.Applicative
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import qualified Crypto.Hash as C (hash)
import Crypto.Hash.Algorithms
import qualified Data.ByteArray as BA
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Int
import Pact.Types.SizeOf
import Data.List (sort)

newtype CacheAddress = CacheAddress ByteString
    deriving (Show, Eq,Ord,Hashable)

data CacheEntry = CacheEntry
  { _ceTxId :: TxId
  , _ceAddy :: CacheAddress
  , _ceSize :: Int64
  , _ceData :: PersistModuleData
  }
  deriving (Show)

instance Eq CacheEntry where
  a == b = _ceAddy a == _ceAddy b && _ceTxId a == _ceTxId b

instance Ord CacheEntry where
  a `compare` b = cmp _ceTxId (cmp _ceAddy EQ)
    where
      cmp acc next = case acc a `compare` acc b of
        EQ -> next
        c -> c


data ModuleCache = ModuleCache
  { _mcStore :: HM.HashMap CacheAddress CacheEntry
  , _mcSize :: Int64
  , _mcLimit :: Int64
  }
  deriving (Show)

makeLenses 'ModuleCache
makeLenses 'CacheEntry

emptyCache :: Int64 -> ModuleCache
emptyCache limit = ModuleCache
    { _mcStore = mempty
    , _mcSize = 0
    , _mcLimit = limit
    }

checkModuleCache
    :: Utf8
    -> ByteString
    -> TxId
    -> ModuleCache
    -> (Maybe PersistModuleData, ModuleCache)
checkModuleCache key rowdata txid = runState $ runMaybeT $ do
    readModuleCache txid addy <|>
        MaybeT (writeModuleCache txid addy $ decode (fromStrict rowdata))
  where
    addy = mkAddress key rowdata

mkAddress :: Utf8 -> ByteString -> CacheAddress
mkAddress (Utf8 key) rowdata = CacheAddress $
  key <> ":" <> BA.convert (C.hash @_ @SHA512t_256 rowdata)

readModuleCache :: TxId -> CacheAddress -> MaybeT (State ModuleCache) PersistModuleData
readModuleCache txid ca = do
  mc <- use mcStore
  MaybeT $ forM (HM.lookup ca mc) $ \e -> do
      mcStore . ix ca . ceTxId .= txid
      return $! _ceData e

writeModuleCache :: TxId -> CacheAddress -> Maybe PersistModuleData -> State ModuleCache (Maybe PersistModuleData)
writeModuleCache _ _ Nothing = pure Nothing
writeModuleCache txid ca (Just v) = do
  let sz = sizeOf SizeOfV1 $ void (_mdModule v)
  mcStore %= HM.insert ca (CacheEntry txid ca sz v)
  mcSize %= (+ sz)
  maintainCache
  return $ Just v

maintainCache :: State ModuleCache ()
maintainCache = do
    sz <- use mcSize
    lim <- use mcLimit
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
