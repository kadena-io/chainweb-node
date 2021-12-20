module Data.CAS.RocksDB.Checkpoint
  ( checkpointRocksDb
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Database.RocksDB.C(RocksDBPtr)
import Database.RocksDB.Internal(DB(..))

import Data.CAS.RocksDB(RocksDb(..))

data Checkpoint

foreign import ccall safe "rocksdb\\c.h rocksdb_checkpoint_object_create" 
  rocksdb_checkpoint_object_create :: RocksDBPtr -> Ptr CString -> IO (Ptr Checkpoint)

foreign import ccall safe "rocksdb\\c.h rocksdb_checkpoint_create"
  rocksdb_checkpoint_create :: Ptr Checkpoint -> CString -> CULong -> Ptr CString -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_checkpoint_object_destroy"
  rocksdb_checkpoint_object_destroy :: Ptr Checkpoint -> IO ()

checked :: String -> Ptr CString -> IO a -> IO a
checked whatWasIDoing errPtr act = do
  r <- act
  err <- peek errPtr
  unless (err == nullPtr) $ do
    errStr <- BS.packCString err
    let msg = unwords ["error while", whatWasIDoing <> ":", BSC.unpack errStr]
    free err
    fail msg
  return r

-- to unconditionally flush the WAL log, set logSizeFlushThreshold to zero. 
-- to *never* flush the WAL log, set logSizeFlushThreshold to maxBound :: CULong.
checkpointRocksDb :: RocksDb -> CULong -> FilePath -> IO ()
checkpointRocksDb RocksDb { _rocksDbHandle = DB dbPtr _ } logSizeFlushThreshold path = 
  alloca (\errPtr -> do
    poke errPtr (nullPtr :: CString)
    let 
      mkCheckpointObject = 
        checked "creating checkpoint object" errPtr $ 
          rocksdb_checkpoint_object_create dbPtr errPtr
      mkCheckpoint cp =
        withCString path (\path' -> 
          checked "creating checkpoint" errPtr $ 
            rocksdb_checkpoint_create cp path' logSizeFlushThreshold errPtr
          )
    bracket mkCheckpointObject rocksdb_checkpoint_object_destroy mkCheckpoint 
  )

