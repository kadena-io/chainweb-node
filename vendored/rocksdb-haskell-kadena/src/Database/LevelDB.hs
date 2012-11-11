{-# LANGUAGE RecordWildCards #-}
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

module Database.LevelDB (
  -- * Exported Types
    DB
  , BatchOp(..)
  , Comparator(..)
  , Compression(..)
  , Options(..)
  , ReadOptions(..)
  , Snapshot
  , WriteBatch
  , WriteOptions(..)
  , Range

  -- * Defaults
  , defaultOptions
  , defaultWriteOptions
  , defaultReadOptions

  -- * Basic Database Manipulation
  , withSnapshot
  , open
  , put
  , delete
  , write
  , get
  , createSnapshot
  , createSnapshot'

  -- * Filter Policy / Bloom Filter
  , FilterPolicy(..)
  , bloomFilter

  -- * Administrative Functions
  , Property(..), getProperty
  , destroy
  , repair
  , approximateSize
  , version

  -- * Iteration
  , Iterator
  , withIterator
  , iterOpen
  , iterOpen'
  , iterValid
  , iterSeek
  , iterFirst
  , iterLast
  , iterNext
  , iterPrev
  , iterKey
  , iterValue
  , iterGetError
  , mapIter
  , iterItems
  , iterKeys
  , iterValues

  -- * Re-exports
  , MonadResource(..)
  , runResourceT
  , resourceForkIO
) where

import Control.Applicative                ((<$>), (<*>))
import Control.Concurrent                 (MVar, withMVar, newMVar)
import Control.Exception                  (throwIO)
import Control.Monad                      (liftM, when)
import Control.Monad.IO.Class             (liftIO)
import Control.Monad.Trans.Resource
import Data.ByteString                    (ByteString)
import Data.ByteString.Internal           (ByteString(..))
import Data.Default
import Data.Maybe                         (catMaybes)
import Foreign
import Foreign.C.Error                    (throwErrnoIfNull)
import Foreign.C.String                   (withCString, peekCString)
import Foreign.C.Types                    (CSize, CInt)

import Database.LevelDB.Base

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Unsafe   as BU

-- import Debug.Trace

-- | Database handle
newtype DB = DB LevelDBPtr deriving (Eq)

-- | Iterator handle
data Iterator = Iterator
    { _iterPtr  :: !IteratorPtr
    , _iterLock :: !(MVar ())
    } deriving (Eq)

-- | Snapshot handle
newtype Snapshot = Snapshot SnapshotPtr deriving (Eq)

-- | Compression setting
data Compression = NoCompression | Snappy deriving (Eq, Show)

-- | User-defined comparator
newtype Comparator = Comparator (ByteString -> ByteString -> Ordering)

data Comparator' = Comparator' (FunPtr CompareFun)
                               (FunPtr Destructor)
                               (FunPtr NameFun)
                               ComparatorPtr

-- | User-defined filter policy
data FilterPolicy = FilterPolicy
    { fpName       :: String
    , createFilter :: [ByteString] -> ByteString
    , keyMayMatch  :: ByteString -> ByteString -> Bool
    }

data FilterPolicy' = FilterPolicy' (FunPtr CreateFilterFun)
                                   (FunPtr KeyMayMatchFun)
                                   (FunPtr Destructor)
                                   (FunPtr NameFun)
                                   FilterPolicyPtr

-- | Represents the built-in Bloom Filter
newtype BloomFilter = BloomFilter FilterPolicyPtr

bloomFilter :: MonadResource m => Int -> m BloomFilter
bloomFilter i = do
    let i' = fromInteger . toInteger $ i
    fp_ptr <- snd <$> allocate (c_leveldb_filterpolicy_create_bloom i')
                               (c_leveldb_filterpolicy_destroy)
    return . BloomFilter $ fp_ptr


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

data Options' = Options'
    { _optsPtr  :: !OptionsPtr
    , _cachePtr :: !(Maybe CachePtr)
    , _comp     :: !(Maybe Comparator')
    , _fpPtr    :: !(Maybe (Either FilterPolicyPtr FilterPolicy'))
    }

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


-- | Open a database
--
-- The returned handle will automatically be released when the enclosing
-- 'runResourceT' terminates.
open :: MonadResource m => FilePath -> Options -> m DB
open path opts = snd <$> open' path opts

open' :: MonadResource m => FilePath -> Options -> m (ReleaseKey, DB)
open' path opts = do
    opts' <- snd <$> allocate (mkOpts opts) freeOpts
    allocate (mkDB opts') freeDB

    where
        mkDB (Options' opts_ptr _ _ _) =
            withCString path $ \path_ptr ->
                liftM DB
                $ throwIfErr "open"
                $ c_leveldb_open opts_ptr path_ptr

        freeDB (DB db_ptr) = c_leveldb_close db_ptr
{-# INLINE open' #-}

-- | Run an action with a snapshot of the database.
--
-- The snapshot will be released when the action terminates or throws an
-- exception. Note that this function is provided for convenience and does not
-- prevent the 'Snapshot' handle to escape. It will, however, be invalid after
-- this function returns and should not be used anymore.
withSnapshot :: MonadResource m => DB -> (Snapshot -> m a) -> m a
withSnapshot db f = do
    (rk, snap) <- createSnapshot' db
    res <- f snap
    release rk
    return res

-- | Create a snapshot of the database.
--
-- The returned 'Snapshot' will be released automatically when the enclosing
-- 'runResourceT' terminates. It is recommended to use 'createSnapshot'' instead
-- and release the resource manually as soon as possible.
createSnapshot :: MonadResource m => DB -> m Snapshot
createSnapshot db = snd <$> createSnapshot' db

-- | Create a snapshot of the database which can (and should) be released early.
createSnapshot' :: MonadResource m => DB -> m (ReleaseKey, Snapshot)
createSnapshot' db = allocate (mkSnap db) (freeSnap db)
    where
        mkSnap (DB db_ptr) =
            Snapshot <$> c_leveldb_create_snapshot db_ptr

        freeSnap (DB db_ptr) (Snapshot snap) =
            c_leveldb_release_snapshot db_ptr snap

-- | Get a DB property
getProperty :: MonadResource m => DB -> Property -> m (Maybe ByteString)
getProperty (DB db_ptr) p = liftIO $
    withCString (prop p) $ \prop_ptr -> do
        val_ptr <- c_leveldb_property_value db_ptr prop_ptr
        if val_ptr == nullPtr
            then return Nothing
            else do res <- Just <$> BS.packCString val_ptr
                    free val_ptr
                    return res

    where
        prop (NumFilesAtLevel i) = "leveldb.num-files-at-level" ++ show i
        prop Stats    = "leveldb.stats"
        prop SSTables = "leveldb.sstables"

-- | Destroy the given leveldb database.
destroy :: MonadResource m => FilePath -> Options -> m ()
destroy path opts = do
    (rk, opts') <- allocate (mkOpts opts) freeOpts
    liftIO $ destroy' opts'
    release rk

    where
        destroy' (Options' opts_ptr _ _ _) =
            withCString path $ \path_ptr ->
                throwIfErr "destroy" $ c_leveldb_destroy_db opts_ptr path_ptr

-- | Repair the given leveldb database.
repair :: MonadResource m => FilePath -> Options -> m ()
repair path opts = do
    (rk, opts') <- allocate (mkOpts opts) freeOpts
    liftIO $ repair' opts'
    release rk

    where
        repair' (Options' opts_ptr _ _ _) =
            withCString path $ \path_ptr ->
                throwIfErr "repair" $ c_leveldb_repair_db opts_ptr path_ptr

-- TODO: support [Range], like C API does
type Range  = (ByteString, ByteString)

-- | Inspect the approximate sizes of the different levels
approximateSize :: MonadResource m => DB -> Range -> m Int64
approximateSize (DB db_ptr) (from, to) = liftIO $
    BU.unsafeUseAsCStringLen from $ \(from_ptr, flen) ->
    BU.unsafeUseAsCStringLen to   $ \(to_ptr, tlen)   ->
    withArray [from_ptr]          $ \from_ptrs        ->
    withArray [intToCSize flen]   $ \flen_ptrs        ->
    withArray [to_ptr]            $ \to_ptrs          ->
    withArray [intToCSize tlen]   $ \tlen_ptrs        ->
    allocaArray 1                 $ \size_ptrs        -> do
        c_leveldb_approximate_sizes db_ptr 1
                                    from_ptrs flen_ptrs
                                    to_ptrs tlen_ptrs
                                    size_ptrs
        liftM head $ peekArray 1 size_ptrs >>= mapM toInt64

    where
        toInt64 = return . fromIntegral

-- | Write a key/value pair
put :: MonadResource m => DB -> WriteOptions -> ByteString -> ByteString -> m ()
put (DB db_ptr) opts key value =  do
    (rk, opts_ptr) <- mkCWriteOpts opts

    liftIO $
        BU.unsafeUseAsCStringLen key   $ \(key_ptr, klen) ->
        BU.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
            throwIfErr "put"
                $ c_leveldb_put db_ptr opts_ptr
                                key_ptr (intToCSize klen)
                                val_ptr (intToCSize vlen)

    release rk

-- | Read a value by key
get :: MonadResource m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
get (DB db_ptr) opts key = do
    (rk, opts_ptr) <- mkCReadOptions opts

    res <- liftIO $
        BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        alloca                       $ \vlen_ptr        -> do
            val_ptr <- throwIfErr "get" $
                c_leveldb_get db_ptr opts_ptr key_ptr (intToCSize klen) vlen_ptr
            vlen <- peek vlen_ptr
            if val_ptr == nullPtr
                then return Nothing
                else do
                    res' <- Just <$> BS.packCStringLen (val_ptr, cSizeToInt vlen)
                    free val_ptr
                    return res'

    release rk
    return res

-- | Delete a key/value pair
delete :: MonadResource m => DB -> WriteOptions -> ByteString -> m ()
delete (DB db_ptr) opts key = do
    (rk, opts_ptr) <- mkCWriteOpts opts

    liftIO $ BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        throwIfErr "delete"
            $ c_leveldb_delete db_ptr opts_ptr key_ptr (intToCSize klen)

    release rk

-- | Perform a batch mutation
write :: MonadResource m => DB -> WriteOptions -> WriteBatch -> m ()
write (DB db_ptr) opts batch = do
    (rk_opts, opts_ptr)   <- mkCWriteOpts opts
    (rk_batch, batch_ptr) <- allocate c_leveldb_writebatch_create
                                      c_leveldb_writebatch_destroy

    mapM_ (liftIO . batchAdd batch_ptr) batch

    liftIO
        $ throwIfErr "write"
        $ c_leveldb_write db_ptr opts_ptr batch_ptr

    -- ensure @ByteString@s (and respective shared @CStringLen@s) aren't GC'ed
    -- until here
    mapM_ (liftIO . touch) batch

    release rk_opts
    release rk_batch

    where
        batchAdd batch_ptr (Put key val) =
            BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
            BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
                c_leveldb_writebatch_put batch_ptr
                                         key_ptr (intToCSize klen)
                                         val_ptr (intToCSize vlen)

        batchAdd batch_ptr (Del key) =
            BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
                c_leveldb_writebatch_delete batch_ptr key_ptr (intToCSize klen)

        touch (Put (PS p _ _) (PS p' _ _)) = do
            touchForeignPtr p
            touchForeignPtr p'

        touch (Del (PS p _ _)) = touchForeignPtr p

-- | Run an action with an Iterator. The iterator will be closed after the
-- action returns or an error is thrown. Thus, the iterator will /not/ be valid
-- after this function terminates.
withIterator :: MonadResource m => DB -> ReadOptions -> (Iterator -> m a) -> m a
withIterator db opts f = do
    (rk, iter) <- iterOpen' db opts
    res <- f iter
    release rk
    return res

-- | Create an 'Iterator'.
--
-- The iterator will be released when the enclosing 'runResourceT' terminates.
-- You may consider to use 'iterOpen'' instead and manually release the iterator
-- as soon as it is no longer needed (alternatively, use 'withIterator').
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. You may,
-- however, specify an older 'Snapshot' in the 'ReadOptions'.
iterOpen :: MonadResource m => DB -> ReadOptions -> m Iterator
iterOpen db opts = snd <$> iterOpen' db opts

-- | Create an 'Iterator' which can be released early.
iterOpen' :: MonadResource m => DB -> ReadOptions -> m (ReleaseKey, Iterator)
iterOpen' db opts = do
    (rk, opts_ptr) <- mkCReadOptions opts
    iter <- allocate (mkIter db opts_ptr) freeIter

    release rk
    return iter

    where
        mkIter (DB db_ptr) opts_ptr = do
            lock   <- liftIO $ newMVar ()
            it_ptr <- liftIO
                          $ throwErrnoIfNull "create_iterator"
                          $ c_leveldb_create_iterator db_ptr opts_ptr
            return $ Iterator it_ptr lock

        freeIter (Iterator iter lck) =
            withMVar lck (\_ -> c_leveldb_iter_destroy iter)

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: MonadResource m => Iterator -> m Bool
iterValid (Iterator iter _) = do
    x <- liftIO $ c_leveldb_iter_valid iter
    return (x /= 0)

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: MonadResource m => Iterator -> ByteString -> m ()
iterSeek (Iterator iter lck) key = liftIO $ withMVar lck go
    where
        go _ = BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
                   c_leveldb_iter_seek iter key_ptr (intToCSize klen)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: MonadResource m => Iterator -> m ()
iterFirst (Iterator iter lck) = liftIO $ withMVar lck go
    where
        go _ = c_leveldb_iter_seek_to_first iter

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: MonadResource m => Iterator -> m ()
iterLast (Iterator iter lck) = liftIO $ withMVar lck go
    where
        go _ = c_leveldb_iter_seek_to_last iter

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the last entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterPrev' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterNext :: MonadResource m => Iterator -> m ()
iterNext iter@(Iterator iter_ptr lck) = do
    valid <- iterValid iter
    when valid $ liftIO $ withMVar lck go

    where
        go _ = c_leveldb_iter_next iter_ptr

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterNext' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterPrev :: MonadResource m => Iterator -> m ()
iterPrev iter@(Iterator iter_ptr lck) = do
    valid <- iterValid iter
    when valid $ liftIO $ withMVar lck go

    where
        go _ = c_leveldb_iter_prev iter_ptr

-- | Return the key for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterKey :: MonadResource m => Iterator -> m (Maybe ByteString)
iterKey iter = do
    valid <- iterValid iter
    if not valid
        then return Nothing
        else iterKey' iter

    where
        iterKey' (Iterator iter_ptr _) = liftIO $
            alloca $ \len_ptr -> do
                key_ptr <- c_leveldb_iter_key iter_ptr len_ptr
                if key_ptr == nullPtr
                    then return Nothing
                    else do
                        klen <- peek len_ptr
                        Just <$> BS.packCStringLen (key_ptr, cSizeToInt klen)

-- | Return the value for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterValue :: MonadResource m => Iterator -> m (Maybe ByteString)
iterValue iter = do
    valid <- iterValid iter
    if not valid
        then return Nothing
        else iterValue' iter

    where
        iterValue' (Iterator iter_ptr _) = liftIO $
            alloca $ \len_ptr -> do
                val_ptr <- c_leveldb_iter_value iter_ptr len_ptr
                if val_ptr == nullPtr
                    then return Nothing
                    else do
                        vlen <- peek len_ptr
                        Just <$> BS.packCStringLen (val_ptr, cSizeToInt vlen)

-- | Check for errors
--
-- Note that this captures somewhat severe errors such as a corrupted database.
iterGetError :: MonadResource m => Iterator -> m (Maybe ByteString)
iterGetError (Iterator iter_ptr _) = liftIO $
    alloca $ \err_ptr -> do
        poke err_ptr nullPtr
        c_leveldb_iter_get_error iter_ptr err_ptr
        erra <- peek err_ptr
        if erra == nullPtr
            then return Nothing
            else do
                err <- peekCString erra
                return . Just . BC.pack $ err

-- | Map a function over an iterator, advancing the iterator forward and
-- returning the value. The iterator should be put in the right position prior
-- to calling the function.
--
-- Note that this function accumulates the result strictly, ie. it reads all
-- values into memory until the iterator is exhausted. This is most likely not
-- what you want for large ranges. You may consider using conduits instead, for
-- an example see: <https://gist.github.com/adc8ec348f03483446a5>
mapIter :: MonadResource m => (Iterator -> m a) -> Iterator -> m [a]
mapIter = go []

    where
        go acc f iter = do
            valid <- iterValid iter
            if not valid
                then return acc
                else do
                    val <- f iter
                    _   <- iterNext iter
                    go (val : acc) f iter

-- | Return a list of key and value tuples from an iterator. The iterator
-- should be put in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'.
iterItems :: MonadResource m => Iterator -> m [(ByteString, ByteString)]
iterItems iter = catMaybes <$> mapIter iterItems' iter

    where
        iterItems' iter' = do
            mkey <- iterKey iter'
            mval <- iterValue iter'
            return $ (,) <$> mkey <*> mval

-- | Return a list of key from an iterator. The iterator should be put
-- in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'
iterKeys :: MonadResource m => Iterator -> m [ByteString]
iterKeys iter = catMaybes <$> mapIter iterKey iter

-- | Return a list of values from an iterator. The iterator should be put
-- in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'
iterValues :: MonadResource m => Iterator -> m [ByteString]
iterValues iter = catMaybes <$> mapIter iterValue iter


-- | Return the runtime version of the underlying LevelDB library as a (major,
-- minor) pair.
version :: MonadResource m => m (Int, Int)
version = do
    major <- liftIO c_leveldb_major_version
    minor <- liftIO c_leveldb_minor_version

    return (cIntToInt major, cIntToInt minor)

--
-- Internal
--

mkOpts :: Options -> IO Options'
mkOpts Options{..} = do
    opts_ptr <- c_leveldb_options_create

    c_leveldb_options_set_block_restart_interval opts_ptr
        $ intToCInt blockRestartInterval
    c_leveldb_options_set_block_size opts_ptr
        $ intToCSize blockSize
    c_leveldb_options_set_compression opts_ptr
        $ ccompression compression
    c_leveldb_options_set_create_if_missing opts_ptr
        $ boolToNum createIfMissing
    c_leveldb_options_set_error_if_exists opts_ptr
        $ boolToNum errorIfExists
    c_leveldb_options_set_max_open_files opts_ptr
        $ intToCInt maxOpenFiles
    c_leveldb_options_set_paranoid_checks opts_ptr
        $ boolToNum paranoidChecks
    c_leveldb_options_set_write_buffer_size opts_ptr
        $ intToCSize writeBufferSize

    cache <- maybeSetCache opts_ptr cacheSize
    cmp   <- maybeSetCmp opts_ptr comparator
    fp    <- maybeSetFilterPolicy opts_ptr filterPolicy

    return (Options' opts_ptr cache cmp fp)

    where
        ccompression NoCompression = noCompression
        ccompression Snappy        = snappyCompression

        maybeSetCache :: OptionsPtr -> Int -> IO (Maybe CachePtr)
        maybeSetCache opts_ptr size =
            if size <= 0
                then return Nothing
                else do
                    cache_ptr <- c_leveldb_cache_create_lru $ intToCSize size
                    c_leveldb_options_set_cache opts_ptr cache_ptr
                    return . Just $ cache_ptr

        maybeSetCmp :: OptionsPtr -> Maybe Comparator -> IO (Maybe Comparator')
        maybeSetCmp opts_ptr (Just mcmp) = Just <$> setcmp opts_ptr mcmp
        maybeSetCmp _ Nothing = return Nothing

        setcmp :: OptionsPtr -> Comparator -> IO Comparator'
        setcmp opts_ptr (Comparator cmp) = do
            cmp'@(Comparator' _ _ _ cmp_ptr) <- mkComparator "user-defined" cmp
            c_leveldb_options_set_comparator opts_ptr cmp_ptr
            return cmp'

        maybeSetFilterPolicy :: OptionsPtr
                             -> Maybe (Either BloomFilter FilterPolicy)
                             -> IO (Maybe (Either FilterPolicyPtr FilterPolicy'))
        maybeSetFilterPolicy _ Nothing = return Nothing
        maybeSetFilterPolicy opts_ptr (Just (Left (BloomFilter bloom_ptr))) = do
            c_leveldb_options_set_filter_policy opts_ptr bloom_ptr
            return Nothing -- bloom filter is freed automatically
        maybeSetFilterPolicy opts_ptr (Just (Right fp)) = do
            fp'@(FilterPolicy' _ _ _ _ fp_ptr) <- mkFilterPolicy fp
            c_leveldb_options_set_filter_policy opts_ptr fp_ptr
            return . Just . Right $ fp'

freeOpts :: Options' -> IO ()
freeOpts (Options' opts_ptr mcache_ptr mcmp_ptr mfp) = do
    c_leveldb_options_destroy opts_ptr
    maybe (return ()) c_leveldb_cache_destroy mcache_ptr
    maybe (return ()) freeComparator mcmp_ptr
    maybe (return ())
          (either c_leveldb_filterpolicy_destroy freeFilterPolicy)
          mfp

    return ()

mkCWriteOpts :: MonadResource m => WriteOptions -> m (ReleaseKey, WriteOptionsPtr)
mkCWriteOpts WriteOptions{..} = do
    (rk, opts_ptr) <- allocate c_leveldb_writeoptions_create
                               c_leveldb_writeoptions_destroy

    liftIO
        $ c_leveldb_writeoptions_set_sync opts_ptr
        $ boolToNum sync

    return (rk, opts_ptr)

mkCReadOptions:: MonadResource m => ReadOptions -> m (ReleaseKey, ReadOptionsPtr)
mkCReadOptions ReadOptions{..} = do
    (rk, opts_ptr) <- allocate c_leveldb_readoptions_create
                               c_leveldb_readoptions_destroy

    liftIO
        $ c_leveldb_readoptions_set_verify_checksums opts_ptr
        $ boolToNum verifyCheckSums

    liftIO
        $ c_leveldb_readoptions_set_verify_checksums opts_ptr
        $ boolToNum fillCache

    maybeSetSnapshot opts_ptr useSnapshot

    return (rk, opts_ptr)

    where
        maybeSetSnapshot opts_ptr (Just (Snapshot snap_ptr)) =
            liftIO $ c_leveldb_readoptions_set_snapshot opts_ptr snap_ptr

        maybeSetSnapshot _ Nothing = return ()

throwIfErr :: String -> (ErrPtr -> IO a) -> IO a
throwIfErr s f = alloca $ \err_ptr -> do
    poke err_ptr nullPtr
    res  <- f err_ptr
    erra <- peek err_ptr
    when (erra /= nullPtr) $ do
        err <- peekCString erra
        throwIO $ userError $ s ++ ": " ++ err
    return res

cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral
{-# INLINE cSizeToInt #-}

intToCSize :: Int -> CSize
intToCSize = fromIntegral
{-# INLINE intToCSize #-}

intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral
{-# INLINE cIntToInt #-}

boolToNum :: Num b => Bool -> b
boolToNum True  = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)
{-# INLINE boolToNum #-}

mkCompareFun :: (ByteString -> ByteString -> Ordering) -> CompareFun
mkCompareFun cmp = cmp'

    where
        cmp' _ a alen b blen = do
            a' <- BS.packCStringLen (a, fromInteger . toInteger $ alen)
            b' <- BS.packCStringLen (b, fromInteger . toInteger $ blen)
            return $ case cmp a' b' of
                         EQ ->  0
                         GT ->  1
                         LT -> -1

mkComparator :: String -> (ByteString -> ByteString -> Ordering) -> IO Comparator'
mkComparator name f =
    withCString name $ \cs -> do
        ccmpfun <- mkCmp . mkCompareFun $ f
        cdest   <- mkDest $ \_ -> ()
        cname   <- mkName $ \_ -> cs
        ccmp    <- c_leveldb_comparator_create nullPtr cdest ccmpfun cname
        return $ Comparator' ccmpfun cdest cname ccmp


freeComparator :: Comparator' -> IO ()
freeComparator (Comparator' ccmpfun cdest cname ccmp) = do
    c_leveldb_comparator_destroy ccmp
    freeHaskellFunPtr ccmpfun
    freeHaskellFunPtr cdest
    freeHaskellFunPtr cname

mkCreateFilterFun :: ([ByteString] -> ByteString) -> CreateFilterFun
mkCreateFilterFun f = f'

    where
        f' _ ks ks_lens n_ks flen = do
            let n_ks' = fromInteger . toInteger $ n_ks
            ks'      <- peekArray n_ks' ks
            ks_lens' <- peekArray n_ks' ks_lens
            keys     <- mapM bstr (zip ks' ks_lens')
            let res = f keys
            poke flen (fromIntegral . BS.length $ res)
            BS.useAsCString res $ \cstr -> return cstr

        bstr (x,len) = BS.packCStringLen (x, fromInteger . toInteger $ len)

mkKeyMayMatchFun :: (ByteString -> ByteString -> Bool) -> KeyMayMatchFun
mkKeyMayMatchFun g = g'

    where
        g' _ k klen f flen = do
            k' <- BS.packCStringLen (k, fromInteger . toInteger $ klen)
            f' <- BS.packCStringLen (f, fromInteger . toInteger $ flen)
            return . boolToNum $ g k' f'


mkFilterPolicy :: FilterPolicy -> IO FilterPolicy'
mkFilterPolicy FilterPolicy{..} =
    withCString fpName $ \cs -> do
        cname  <- mkName $ \_ -> cs
        cdest  <- mkDest $ \_ -> ()
        ccffun <- mkCF . mkCreateFilterFun $ createFilter
        ckmfun <- mkKMM . mkKeyMayMatchFun $ keyMayMatch
        cfp    <- c_leveldb_filterpolicy_create nullPtr cdest ccffun ckmfun cname

        return $ FilterPolicy' ccffun ckmfun cdest cname cfp

freeFilterPolicy :: FilterPolicy' -> IO ()
freeFilterPolicy (FilterPolicy' ccffun ckmfun cdest cname cfp) = do
    c_leveldb_filterpolicy_destroy cfp
    freeHaskellFunPtr ccffun
    freeHaskellFunPtr ckmfun
    freeHaskellFunPtr cdest
    freeHaskellFunPtr cname
