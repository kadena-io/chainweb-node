-- |
-- Module      : Database.LevelDB.Types
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
-- License     : BSD3
-- Maintainer  : kim.altintop@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--

module Database.LevelDB.Types
    ( BatchOp (..)
    , BloomFilter (..)
    , Comparator (..)
    , Compression (..)
    , FilterPolicy (..)
    , Options (..)
    , Property (..)
    , ReadOptions (..)
    , Snapshot (..)
    , WriteBatch
    , WriteOptions (..)

    , defaultOptions
    , defaultReadOptions
    , defaultWriteOptions
    )
where

import           Data.ByteString    (ByteString)
import           Data.Default
import           Foreign

import           Database.LevelDB.C

-- | Snapshot handle
newtype Snapshot = Snapshot SnapshotPtr deriving (Eq)

-- | Compression setting
data Compression = NoCompression | Snappy deriving (Eq, Show)

-- | User-defined comparator
newtype Comparator = Comparator (ByteString -> ByteString -> Ordering)

-- | User-defined filter policy
data FilterPolicy = FilterPolicy
    { fpName       :: String
    , createFilter :: [ByteString] -> ByteString
    , keyMayMatch  :: ByteString -> ByteString -> Bool
    }

-- | Represents the built-in Bloom Filter
newtype BloomFilter = BloomFilter FilterPolicyPtr

-- | Options when opening a database
data Options = Options
    { blockRestartInterval :: !Int
      -- ^ Number of keys between restart points for delta encoding of keys.
      --
      -- This parameter can be changed dynamically. Most clients should leave
      -- this parameter alone.
      --
      -- Default: 16
    , blockSize            :: !Int
      -- ^ Approximate size of user data packed per block.
      --
      -- Note that the block size specified here corresponds to uncompressed
      -- data. The actual size of the unit read from disk may be smaller if
      -- compression is enabled.
      --
      -- This parameter can be changed dynamically.
      --
      -- Default: 4k
    , cacheSize            :: !Int
      -- ^ Control over blocks (user data is stored in a set of blocks, and a
      -- block is the unit of reading from disk).
      --
      -- If > 0, use the specified cache (in bytes) for blocks. If 0, leveldb
      -- will automatically create and use an 8MB internal cache.
      --
      -- Default: 0
    , comparator           :: !(Maybe Comparator)
      -- ^ Comparator used to defined the order of keys in the table.
      --
      -- If 'Nothing', the default comparator is used, which uses lexicographic
      -- bytes-wise ordering.
      --
      -- NOTE: the client must ensure that the comparator supplied here has the
      -- same name and orders keys /exactly/ the same as the comparator provided
      -- to previous open calls on the same DB.
      --
      -- Default: Nothing
    , compression          :: !Compression
      -- ^ Compress blocks using the specified compression algorithm.
      --
      -- This parameter can be changed dynamically.
      --
      -- Default: 'Snappy'
    , createIfMissing      :: !Bool
      -- ^ If true, the database will be created if it is missing.
      --
      -- Default: False
    , errorIfExists        :: !Bool
      -- ^ It true, an error is raised if the database already exists.
      --
      -- Default: False
    , maxOpenFiles         :: !Int
      -- ^ Number of open files that can be used by the DB.
      --
      -- You may need to increase this if your database has a large working set
      -- (budget one open file per 2MB of working set).
      --
      -- Default: 1000
    , paranoidChecks       :: !Bool
      -- ^ If true, the implementation will do aggressive checking of the data
      -- it is processing and will stop early if it detects any errors.
      --
      -- This may have unforeseen ramifications: for example, a corruption of
      -- one DB entry may cause a large number of entries to become unreadable
      -- or for the entire DB to become unopenable.
      --
      -- Default: False
    , writeBufferSize      :: !Int
      -- ^ Amount of data to build up in memory (backed by an unsorted log on
      -- disk) before converting to a sorted on-disk file.
      --
      -- Larger values increase performance, especially during bulk loads. Up to
      -- to write buffers may be held in memory at the same time, so you may
      -- with to adjust this parameter to control memory usage. Also, a larger
      -- write buffer will result in a longer recovery time the next time the
      -- database is opened.
      --
      -- Default: 4MB
    , filterPolicy         :: !(Maybe (Either BloomFilter FilterPolicy))
    }

defaultOptions :: Options
defaultOptions = Options
    { blockRestartInterval = 16
    , blockSize            = 4096
    , cacheSize            = 0
    , comparator           = Nothing
    , compression          = Snappy
    , createIfMissing      = False
    , errorIfExists        = False
    , maxOpenFiles         = 1000
    , paranoidChecks       = False
    , writeBufferSize      = 4 `shift` 20
    , filterPolicy         = Nothing
    }

instance Default Options where
    def = defaultOptions

-- | Options for write operations
data WriteOptions = WriteOptions
    { sync :: !Bool
      -- ^ If true, the write will be flushed from the operating system buffer
      -- cache (by calling WritableFile::Sync()) before the write is considered
      -- complete. If this flag is true, writes will be slower.
      --
      -- If this flag is false, and the machine crashes, some recent writes may
      -- be lost. Note that if it is just the process that crashes (i.e., the
      -- machine does not reboot), no writes will be lost even if sync==false.
      --
      -- In other words, a DB write with sync==false has similar crash semantics
      -- as the "write()" system call. A DB write with sync==true has similar
      -- crash semantics to a "write()" system call followed by "fsync()".
      --
      -- Default: False
    } deriving (Eq, Show)

defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions { sync = False }

instance Default WriteOptions where
    def = defaultWriteOptions

-- | Options for read operations
data ReadOptions = ReadOptions
    { verifyCheckSums :: !Bool
      -- ^ If true, all data read from underlying storage will be verified
      -- against corresponding checksuyms.
      --
      -- Default: False
    , fillCache       :: !Bool
      -- ^ Should the data read for this iteration be cached in memory? Callers
      -- may with to set this field to false for bulk scans.
      --
      -- Default: True
    , useSnapshot     :: !(Maybe Snapshot)
      -- ^ If 'Just', read as of the supplied snapshot (which must belong to the
      -- DB that is being read and which must not have been released). If
      -- 'Nothing', use an implicit snapshot of the state at the beginning of
      -- this read operation.
      --
      -- Default: Nothing
    } deriving (Eq)

defaultReadOptions :: ReadOptions
defaultReadOptions = ReadOptions
    { verifyCheckSums = False
    , fillCache       = True
    , useSnapshot     = Nothing
    }

instance Default ReadOptions where
    def = defaultReadOptions

type WriteBatch = [BatchOp]

-- | Batch operation
data BatchOp = Put ByteString ByteString | Del ByteString
    deriving (Eq, Show)

-- | Properties exposed by LevelDB
data Property = NumFilesAtLevel Int | Stats | SSTables
    deriving (Eq, Show)
