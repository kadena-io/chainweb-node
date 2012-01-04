{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Database.LevelDB.Base where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <leveldb/c.h>

data LevelDB = LevelDB
type LevelDBPtr = Ptr LevelDB

-- exported types are just opaque pointers
type CachePtr        = Ptr ()
type ComparatorPtr   = Ptr ()
type EnvPtr          = Ptr ()
type FileLockPtr     = Ptr ()
type IteratorPtr     = Ptr ()
type LoggerPtr       = Ptr ()
type OptionsPtr      = Ptr ()
type RandomFilePtr   = Ptr ()
type ReadOptionsPtr  = Ptr ()
type SeqfilePtr      = Ptr ()
type SnapshotPtr     = Ptr ()
type WritableFilePtr = Ptr ()
type WriteBatchPtr   = Ptr ()
type WriteOptionsPtr = Ptr ()

type DBName = CString
type ErrPtr = Ptr CString
type Key    = CString
type Val    = CString

newtype CompressionOpt = CompressionOpt { compressionOpt :: CInt }
  deriving (Eq, Show)
#{enum CompressionOpt, CompressionOpt
 , noCompression     = 0
 , snappyCompression = 1
 }
