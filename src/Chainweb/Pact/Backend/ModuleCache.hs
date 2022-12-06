{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Chainweb.Pact.Backend.ModuleCache

  where

import Database.SQLite3.Direct
import Data.ByteString (ByteString)
import Control.Monad.Trans.Maybe
import Chainweb.Pact.Backend.Types
import Pact.Types.Persistence
import Control.Lens
import Control.Applicative
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import qualified Crypto.Hash as C (hash)
import Crypto.Hash.Algorithms
import qualified Data.ByteArray as BA
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Data.Hashable
import Data.Int
import Pact.Types.SizeOf
import Data.List (sort)

newtype CacheAddress = CacheAddress ByteString
    deriving (Eq,Ord,Hashable)

data CacheEntry = CacheEntry
  { _ceTxId :: TxId
  , _ceAddy :: CacheAddress
  , _ceSize :: Int64
  , _ceData :: PersistModuleData
  }

instance Eq CacheEntry where
  a == b = _ceAddy a == _ceAddy b && _ceTxId a == _ceTxId b

instance Ord CacheEntry where
  a `compare` b = cmp _ceTxId (cmp _ceAddy EQ)
    where
      cmp acc next = case (acc a `compare` acc b) of
        EQ -> next
        c -> c


data ModuleCache = ModuleCache
  { _mcStore :: HM.HashMap CacheAddress CacheEntry
  , _mcSize :: Int64
  , _mcLimit :: Int64
  }
makeLenses 'ModuleCache
makeLenses 'CacheEntry

bsModuleCache :: Lens' BlockState ModuleCache
bsModuleCache = undefined

checkModuleCache
    :: Utf8
    -> ByteString
    -> MaybeT (BlockHandler SQLiteEnv) PersistModuleData
checkModuleCache key rowdata = do
    txid <- use bsTxId
    let addy = mkAddress key rowdata
    readModuleCache txid addy <|>
        MaybeT (writeModuleCache txid addy $ decode (fromStrict rowdata))


mkAddress :: Utf8 -> ByteString -> CacheAddress
mkAddress (Utf8 key) rowdata = CacheAddress $
  key <> ":" <> BA.convert (C.hash @_ @SHA512t_256 rowdata)

readModuleCache :: TxId -> CacheAddress -> MaybeT (BlockHandler SQLiteEnv) PersistModuleData
readModuleCache txid ca = do
  mc <- use (bsModuleCache . mcStore)
  MaybeT $ forM (HM.lookup ca mc) $ \e -> do
      bsModuleCache . mcStore . ix ca . ceTxId .= txid
      return $! _ceData e

writeModuleCache :: TxId -> CacheAddress -> Maybe PersistModuleData -> BlockHandler SQLiteEnv (Maybe PersistModuleData)
writeModuleCache _ _ Nothing = pure Nothing
writeModuleCache txid ca (Just v) = do
  let sz = sizeOf SizeOfV1 $ () <$ _mdModule v
  bsModuleCache . mcStore %= HM.insert ca (CacheEntry txid ca sz v)
  bsModuleCache . mcSize %= (+ sz)
  maintainCache
  return $ Just v

maintainCache :: BlockHandler SQLiteEnv ()
maintainCache = do
    sz <- use $ bsModuleCache . mcSize
    lim <- use $ bsModuleCache . mcLimit
    vals <- sort . HM.elems <$> use (bsModuleCache . mcStore)
    evict sz lim vals
  where
    evict _ _ [] = return ()
    evict sz lim (e:es)
      | sz < lim = return ()
      | otherwise = do
          bsModuleCache . mcStore %= HM.delete (_ceAddy e)
          let sz' = sz - _ceSize e
          bsModuleCache . mcSize .= sz'
          evict sz' lim es
