-- |
-- Module      : Database.LevelDB
-- Copyright   : (c) 2012 Kim Altintop
-- License     : BSD3
-- Maintainer  : kim.altintop@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
-- LevelDB Haskell binding.
--
-- The API closely follows the C-API of LevelDB.
-- For more information, see: <http://leveldb.googlecode.com>
--
--
-- Basic example:
--
-- > withLevelDB "/tmp/leveldbtest" [CreateIfMissing, CacheSize 1024] $ \db -> do
-- >     put db [] "foo" "bar"
-- >     get db [] "foo" >>= print
-- >     delete db [] "foo"
-- >     get db [] "foo" >>= print
--
-- Batch write and iterating:
--
-- > withLevelDB "/tmp/leveldbtest" [CreateIfMissing, CacheSize 1024] $ \db -> do
-- >     write db [Sync] [ Put "a" "one"
-- >                     , Put "b" "two"
-- >                     , Put "c" "three" ]
-- >     dumpEntries db []
-- >
-- >     where
-- >         dumpEntries db opts =
-- >             withIterator db opts $ \iter -> do
-- >                 iterFirst iter
-- >                 iterEntries iter print
-- >
-- >         iterEntries iter f = do
-- >             valid <- iterValid iter
-- >             when valid $ do
-- >                 key <- iterKey iter
-- >                 val <- iterValue iter
-- >                 _   <- f (key, val)
-- >                 _   <- iterNext iter
-- >                 iterEntries iter f

module Database.LevelDB (
  -- * Exported Types
    DB
  , BatchOp(..)
  , Compression(..)
  , Option(..)
  , Options
  , ReadOption(..)
  , ReadOptions
  , Snapshot
  , WriteBatch
  , WriteOption(..)
  , WriteOptions
  , Range

  -- * Basic Database Manipulation
  , withLevelDB
  , withSnapshot
  , put
  , delete
  , write
  , get

  -- * Administrative Functions
  , Property(..), getProperty
  , destroy
  , repair
  , approximateSize

  -- * Iteration
  , Iterator
  , withIterator
  , iterOpen
  , iterClose
  , iterValid
  , iterSeek
  , iterFirst
  , iterLast
  , iterNext
  , iterPrev
  , iterKey
  , iterValue
) where

import Control.Exception  (bracket)
import Control.Monad      (liftM, when)
import Data.ByteString    (ByteString)
import Data.List          (find)
import Data.Maybe
import Foreign
import Foreign.C.Error    (throwErrnoIfNull)
import Foreign.C.String   (withCString, peekCString)
import Foreign.C.Types    (CSize, CInt)

import Database.LevelDB.Base

import qualified Data.ByteString as SB
import qualified Data.ByteString.Unsafe as UB

-- import Debug.Trace

-- | Database handle
newtype DB = DB LevelDBPtr

-- | Iterator handle
newtype Iterator = Iterator IteratorPtr

-- | Snapshot handle
newtype Snapshot = Snapshot SnapshotPtr

-- | Compression setting
data Compression = NoCompression | Snappy deriving (Eq, Show)

type Options = [Option]
-- | Options when opening a database
data Option = CreateIfMissing
            | ErrorIfExists
            | ParanoidChecks
            | WriteBufferSize Int
            | MaxOpenFiles Int
            | BlockSize Int
            | BlockRestartInterval Int
            | UseCompression Compression
            | CacheSize Int
            deriving (Eq, Show)

type WriteOptions = [WriteOption]
-- | Options for write operations
data WriteOption  = Sync -- ^ fsync the rows written immediately
                  deriving (Show)

type ReadOptions = [ReadOption]
-- | Options for read operations
data ReadOption  = VerifyCheckSums
                 | FillCache
                 | UseSnapshot Snapshot

type WriteBatch = [BatchOp]
-- | Batch operation
data BatchOp = Put ByteString ByteString | Del ByteString deriving (Show)

-- | Properties exposed by LevelDB
data Property = NumFilesAtLevel Int | Stats | SSTables deriving (Eq, Show)


-- | Run an action on a database
withLevelDB :: FilePath -> Options -> (DB -> IO a) -> IO a
withLevelDB path opts act =
    withCString path    $ \path_ptr ->
    withCOptions opts   $ \opts_ptr ->
    alloca              $ \err_ptr  ->
        bracket (open path_ptr opts_ptr err_ptr) close act
    where
        open path_ptr opts_ptr err_ptr = do
            p <- throwIfErr "open" err_ptr $ c_leveldb_open opts_ptr path_ptr
            return . DB $ p

        close (DB db) = c_leveldb_close db

-- | Run an action with a snapshot of the database.
withSnapshot :: DB -> (Snapshot -> IO a) -> IO a
withSnapshot (DB db) = bracket (create db) (release db)
    where
        create  db_ptr = liftM Snapshot $ c_leveldb_create_snapshot db_ptr
        release db_ptr (Snapshot snap) = c_leveldb_release_snapshot db_ptr snap

-- | Get a DB property
getProperty :: DB -> Property -> IO (Maybe ByteString)
getProperty (DB db) p =
    withCString (prop p) $ \prop_ptr -> do
        val_ptr <- c_leveldb_property_value db prop_ptr
        if val_ptr == nullPtr
            then return Nothing
            else do res <- liftM Just $ SB.packCString val_ptr
                    free val_ptr
                    return res

    where
        prop (NumFilesAtLevel i) = "leveldb.num-files-at-level" ++ show i
        prop Stats    = "leveldb.stats"
        prop SSTables = "leveldb.sstables"

-- | Destroy the given leveldb database.
destroy :: FilePath -> Options -> IO ()
destroy path opts =
    withCString path    $ \path_ptr ->
    withCOptions opts   $ \opts_ptr ->
    alloca              $ \err_ptr  ->
        throwIfErr "destroy" err_ptr $ c_leveldb_destroy_db opts_ptr path_ptr

-- | Repair the given leveldb database.
repair :: FilePath -> Options -> IO ()
repair path opts =
    withCString path    $ \path_ptr ->
    withCOptions opts   $ \opts_ptr ->
    alloca              $ \err_ptr  ->
        throwIfErr "repair" err_ptr $ c_leveldb_repair_db opts_ptr path_ptr

-- TODO: support [Range], like C API does
type Range  = (ByteString, ByteString)

-- | Inspect the approximate sizes of the different levels
approximateSize :: DB -> Range -> IO Int64
approximateSize (DB db) (from, to) =
    UB.unsafeUseAsCStringLen from $ \(from_ptr, flen) ->
    UB.unsafeUseAsCStringLen to   $ \(to_ptr, tlen)   ->
    withArray [from_ptr]          $ \from_ptrs        ->
    withArray [i2s flen]          $ \flen_ptrs        ->
    withArray [to_ptr]            $ \to_ptrs          ->
    withArray [i2s tlen]          $ \tlen_ptrs        ->
    allocaArray 1                 $ \size_ptrs        -> do
        c_leveldb_approximate_sizes db 1 from_ptrs flen_ptrs to_ptrs tlen_ptrs size_ptrs
        liftM head $ peekArray 1 size_ptrs >>= mapM toInt64

    where
        toInt64 = return . fromIntegral

-- | Write a key/value pair
put :: DB -> WriteOptions -> ByteString -> ByteString -> IO ()
put (DB db) opts key value =
    UB.unsafeUseAsCStringLen key   $ \(key_ptr, klen) ->
    UB.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
    withCWriteOptions opts         $ \opts_ptr        ->
    alloca                         $ \err_ptr         ->
        throwIfErr "put" err_ptr
        $ c_leveldb_put db opts_ptr
                        key_ptr (i2s klen)
                        val_ptr (i2s vlen)

-- | Read a value by key
get :: DB -> ReadOptions -> ByteString -> IO (Maybe ByteString)
get (DB db) opts key =
    UB.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    withCReadOptions opts        $ \opts_ptr        ->
    alloca                       $ \err_ptr         ->
    alloca                       $ \vlen_ptr        -> do
        val_ptr <- throwIfErr "get" err_ptr
                   $ c_leveldb_get db opts_ptr key_ptr (i2s klen) vlen_ptr
        vlen <- peek vlen_ptr
        if val_ptr /= nullPtr
            then do
                res <- liftM Just $ SB.packCStringLen (val_ptr, s2i vlen)
                free val_ptr
                return res
            else return Nothing

-- | Delete a key/value pair
delete :: DB -> WriteOptions -> ByteString -> IO ()
delete (DB db) opts key =
    UB.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    withCWriteOptions opts       $ \opts_ptr        ->
    alloca                       $ \err_ptr         ->
        throwIfErr "delete" err_ptr
        $ c_leveldb_delete db opts_ptr key_ptr (i2s klen)

-- | Perform a batch mutation
write :: DB -> WriteOptions -> WriteBatch -> IO ()
write (DB db) opts batch =
    withCWriteOptions opts $ \opts_ptr  ->
    withCWriteBatch batch  $ \batch_ptr ->
    alloca                 $ \err_ptr   ->
        throwIfErr "write" err_ptr
        $ c_leveldb_write db opts_ptr batch_ptr

    where
        withCWriteBatch b f = do
            batch_ptr <- c_leveldb_writebatch_create
            mapM_ (batchAdd batch_ptr) b
            res <- f batch_ptr
            c_leveldb_writebatch_destroy batch_ptr
            return res

        batchAdd batch_ptr (Put key val) =
            UB.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
            UB.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
                c_leveldb_writebatch_put batch_ptr
                                         key_ptr (i2s klen)
                                         val_ptr (i2s vlen)

        batchAdd batch_ptr (Del key) =
            UB.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
                c_leveldb_writebatch_delete batch_ptr key_ptr (i2s klen)

-- | Run an action with an Iterator. The iterator will be closed after the
-- action returns or an error is thrown. Thus, the iterator will /not/ be valid
-- after this function terminates.
withIterator :: DB -> ReadOptions -> (Iterator -> IO a) -> IO a
withIterator db opts = bracket (iterOpen db opts) iterClose

-- | Create an Iterator. Consider using withIterator.
iterOpen :: DB -> ReadOptions -> IO Iterator
iterOpen (DB db) opts =
    withCReadOptions opts $ \opts_ptr ->
        liftM Iterator
        $ throwErrnoIfNull "create_iterator"
        $ c_leveldb_create_iterator db opts_ptr

-- | Release an Iterator. Consider using withIterator.
iterClose :: Iterator -> IO ()
iterClose (Iterator iter) = c_leveldb_iter_destroy iter

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: Iterator -> IO Bool
iterValid (Iterator iter) = do
    x <- c_leveldb_iter_valid iter
    return (x /= 0)

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: Iterator -> ByteString -> IO ()
iterSeek (Iterator iter) key =
    UB.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        c_leveldb_iter_seek iter key_ptr (i2s klen)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: Iterator -> IO ()
iterFirst (Iterator iter) = c_leveldb_iter_seek_to_first iter

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: Iterator -> IO ()
iterLast (Iterator iter) = c_leveldb_iter_seek_to_last iter

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned the last entry in the source.
iterNext :: Iterator -> IO ()
iterNext (Iterator iter) = c_leveldb_iter_next iter

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
iterPrev :: Iterator -> IO ()
iterPrev (Iterator iter) = c_leveldb_iter_prev iter

-- | Return the key for the current entry. The underlying storage for the
-- returned slice is valid only until the next modification of the iterator.
iterKey :: Iterator -> IO ByteString
iterKey (Iterator iter) =
    alloca $ \len_ptr -> do
        key_ptr <- c_leveldb_iter_key iter len_ptr
        klen <- peek len_ptr
        if key_ptr /= nullPtr
            then SB.packCStringLen (key_ptr, s2i klen)
            else ioError $ userError "null key"

-- | Return the value for the current entry. The underlying storage for the
-- returned slice is valid only until the next modification of the iterator.
iterValue :: Iterator -> IO ByteString
iterValue (Iterator iter) =
    alloca $ \len_ptr -> do
        val_ptr <- c_leveldb_iter_value iter len_ptr
        vlen <- peek len_ptr
        if val_ptr /= nullPtr
            then SB.packCStringLen (val_ptr, s2i vlen)
            else ioError $ userError "null value"

-- | Internal

withCOptions :: Options -> (OptionsPtr -> IO a) -> IO a
withCOptions opts f = do
    opts_ptr <- c_leveldb_options_create
    mapM_ (setopt opts_ptr) opts
    if isJust $ maybeCacheSize opts
        then withCache (fromJust $ maybeCacheSize opts) $ \cache_ptr -> do
                 c_leveldb_options_set_cache opts_ptr cache_ptr
                 run opts_ptr
        else run opts_ptr

    where
        run opts_ptr = do
            res <- f opts_ptr
            c_leveldb_options_destroy opts_ptr
            return res

        maybeCacheSize os = find isCs os >>= \(CacheSize s) -> return s

        isCs (CacheSize _) = True
        isCs _             = False

        withCache s = bracket (c_leveldb_cache_create_lru $ i2s s)
                              c_leveldb_cache_destroy

        setopt opts_ptr CreateIfMissing =
            c_leveldb_options_set_create_if_missing opts_ptr 1
        setopt opts_ptr ErrorIfExists =
            c_leveldb_options_set_error_if_exists opts_ptr 1
        setopt opts_ptr ParanoidChecks =
            c_leveldb_options_set_paranoid_checks opts_ptr 1
        setopt opts_ptr (WriteBufferSize s) =
            c_leveldb_options_set_write_buffer_size opts_ptr $ i2s s
        setopt opts_ptr (MaxOpenFiles n) =
            c_leveldb_options_set_max_open_files opts_ptr $ i2ci n
        setopt opts_ptr (BlockSize s) =
            c_leveldb_options_set_block_size opts_ptr $ i2s s
        setopt opts_ptr (BlockRestartInterval i) =
            c_leveldb_options_set_block_restart_interval opts_ptr $ i2ci i
        setopt opts_ptr (UseCompression NoCompression) =
            c_leveldb_options_set_compression opts_ptr noCompression
        setopt opts_ptr (UseCompression Snappy) =
            c_leveldb_options_set_compression opts_ptr snappyCompression
        setopt _ (CacheSize _) = return ()

withCWriteOptions :: WriteOptions -> (WriteOptionsPtr -> IO a) -> IO a
withCWriteOptions opts f = do
    opts_ptr <- c_leveldb_writeoptions_create
    mapM_ (setopt opts_ptr) opts
    res <- f opts_ptr
    c_leveldb_writeoptions_destroy opts_ptr
    return res

    where
        setopt opts_ptr Sync = c_leveldb_writeoptions_set_sync opts_ptr 1

withCReadOptions :: ReadOptions -> (ReadOptionsPtr -> IO a) -> IO a
withCReadOptions opts f = do
    opts_ptr <- c_leveldb_readoptions_create
    mapM_ (setopt opts_ptr) opts
    res <- f opts_ptr
    c_leveldb_readoptions_destroy opts_ptr
    return res

    where
        setopt opts_ptr VerifyCheckSums =
            c_leveldb_readoptions_set_verify_checksums opts_ptr 1
        setopt opts_ptr FillCache =
            c_leveldb_readoptions_set_fill_cache opts_ptr 1
        setopt opts_ptr (UseSnapshot (Snapshot snap)) =
            c_leveldb_readoptions_set_snapshot opts_ptr snap

throwIfErr :: String -> ErrPtr -> (ErrPtr -> IO a) -> IO a
throwIfErr s err_ptr f = do
    res  <- f err_ptr
    erra <- peek err_ptr
    when (erra /= nullPtr) $ do
        err <- peekCString erra
        ioError $ userError $ s ++ ": " ++ err
    return res

s2i :: CSize -> Int
s2i = fromIntegral

i2s :: Int -> CSize
i2s = fromIntegral

i2ci :: Int -> CInt
i2ci = fromIntegral
