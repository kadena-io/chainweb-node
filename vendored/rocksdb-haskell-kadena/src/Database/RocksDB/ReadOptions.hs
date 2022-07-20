{-# language RankNTypes #-}

module Database.RocksDB.ReadOptions
    ( ReadOptions(..)
    , withReadOptions
    , withCReadOpts
    , setVerifyChecksums
    , setFillCache
    , setUseSnapshot
    , setUpperBound
    , setLowerBound
    , setAutoPrefixMode
    )
    where

import Control.Exception
import Data.ByteString(ByteString)
import qualified Data.ByteString.Unsafe as BU
import Foreign.Ptr

import Database.RocksDB.C
import Database.RocksDB.Types

newtype ReadOptions = ReadOptions
    { runReadOptions :: forall r. ReadOptionsPtr -> IO r -> IO r
    }
instance Monoid ReadOptions where
    mempty = ReadOptions (\_ k -> k)
instance Semigroup ReadOptions where
    ReadOptions r <> ReadOptions r' =
        ReadOptions $ \ptr k -> r ptr (r' ptr k)

withReadOptions :: ReadOptions -> (ReadOptionsPtr -> IO a) -> IO a
withReadOptions opts k = withCReadOpts $ \opts_ptr -> do
    runReadOptions opts opts_ptr (k opts_ptr)

withCReadOpts :: (ReadOptionsPtr -> IO a) -> IO a
withCReadOpts = bracket c_rocksdb_readoptions_create c_rocksdb_readoptions_destroy

-- | If true, all data read from underlying storage will be verified
-- against corresponding checksuyms.
--
-- Default: True
setVerifyChecksums :: Bool -> ReadOptions
setVerifyChecksums b = ReadOptions $ \opts_ptr k -> do
    c_rocksdb_readoptions_set_verify_checksums opts_ptr (boolToNum b)
    k

-- | Should the data read for this iteration be cached in memory? Callers
-- may wish to set this field to false for bulk scans.
--
-- Default: True
setFillCache :: Bool -> ReadOptions
setFillCache b = ReadOptions $ \opts_ptr k -> do
    c_rocksdb_readoptions_set_fill_cache opts_ptr (boolToNum b)
    k

-- | If 'Just', read as of the supplied snapshot (which must belong to the
-- DB that is being read and which must not have been released). If
-- 'Nothing', use an implicit snapshot of the state at the beginning of
-- this read operation.
--
-- Default: Nothing
setUseSnapshot :: Maybe Snapshot -> ReadOptions
setUseSnapshot snapshot = ReadOptions $ \opts_ptr k -> do
    c_rocksdb_readoptions_set_snapshot opts_ptr snap_ptr
    k
  where
    snap_ptr = case snapshot of
        Just (Snapshot p) -> p
        Nothing -> nullPtr

setUpperBound :: ByteString -> ReadOptions
setUpperBound upper = ReadOptions $ \opts_ptr k ->
    BU.unsafeUseAsCStringLen upper $ \(upperPtr, upperLen) -> do
        rocksdb_readoptions_set_iterate_upper_bound opts_ptr upperPtr (fromIntegral upperLen)
        k

setLowerBound :: ByteString -> ReadOptions
setLowerBound lower = ReadOptions $ \opts_ptr k ->
    BU.unsafeUseAsCStringLen lower $ \(lowerPtr, lowerLen) -> do
        rocksdb_readoptions_set_iterate_lower_bound opts_ptr lowerPtr (fromIntegral lowerLen)
        k

setAutoPrefixMode :: Bool -> ReadOptions
setAutoPrefixMode m = ReadOptions $ \opts_ptr k -> do
    rocksdb_readoptions_set_auto_prefix_mode opts_ptr (boolToNum m)
    k

boolToNum :: Num b => Bool -> b
boolToNum True  = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)
{-# INLINE boolToNum #-}
