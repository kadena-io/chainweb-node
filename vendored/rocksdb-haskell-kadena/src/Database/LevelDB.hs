{-# LANGUAGE FlexibleContexts #-}
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
-- >                 iterItems iter >>= print

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
  --, withLevelDB
  --, withSnapshot
  , open
  , put
  , delete
  , write
  , get
  , createSnapshot
  , createSnapshot'

  -- * Administrative Functions
  , Property(..), getProperty
  , destroy
  , repair
  , approximateSize

  -- * Iteration
  , Iterator
  --, withIterator
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
  , mapIter
  , iterItems
  , iterKeys
  , iterValues

  -- * Re-exports / aliases
  , runLevelDB
  , rel -- FIXME: need better name
) where

import Control.Applicative          ((<$>), (<*>))
import Control.Concurrent           (MVar, withMVar, newMVar)
import Control.Exception            (bracket, bracketOnError, throwIO)
import Control.Monad                (liftM, when)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.ByteString              (ByteString)
import Data.List                    (find)
import Foreign
import Foreign.C.Error              (throwErrnoIfNull)
import Foreign.C.String             (withCString, peekCString)
import Foreign.C.Types              (CSize, CInt)

import Database.LevelDB.Base

import qualified Data.ByteString as SB
import qualified Data.ByteString.Unsafe as UB

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

data Options' = Options'
    { _optsPtr  :: !OptionsPtr
    , _cachePtr :: !(Maybe CachePtr)
    } deriving (Eq)

type WriteOptions = [WriteOption]
-- | Options for write operations
data WriteOption  = Sync -- ^ fsync the rows written immediately
                  deriving (Eq, Show)

type ReadOptions = [ReadOption]
-- | Options for read operations
data ReadOption  = VerifyCheckSums
                 | FillCache
                 | UseSnapshot Snapshot
                 deriving (Eq)

type WriteBatch = [BatchOp]
-- | Batch operation
data BatchOp = Put ByteString ByteString | Del ByteString
             deriving (Eq, Show)

-- | Properties exposed by LevelDB
data Property = NumFilesAtLevel Int | Stats | SSTables
              deriving (Eq, Show)


-- | Run an action on a database
--withLevelDB :: FilePath -> Options -> (DB -> IO a) -> IO a
--withLevelDB path opts = bracket (open path opts) close

runLevelDB :: MonadBaseControl IO m => ResourceT m a -> m a
runLevelDB = runResourceT

rel :: MonadResource m => ReleaseKey -> m ()
rel = release

-- | Open a database
open :: MonadResource m => FilePath -> Options -> m DB
open path opts = liftM snd $ open' path opts

open' :: MonadResource m => FilePath -> Options -> m (ReleaseKey, DB)
open' path opts = do
    opts' <- liftM snd $ allocate (mkOpts opts) freeOpts
    allocate (mkDB opts') freeDB

    where
        mkDB (Options' opts_ptr _) =
            withCString path $ \path_ptr ->
                liftM DB $ throwIfErr "open" $ c_leveldb_open opts_ptr path_ptr

        freeDB (DB db_ptr) = c_leveldb_close db_ptr

-- | Run an action with a snapshot of the database.
--withSnapshot :: DB -> (Snapshot -> IO a) -> IO a
--withSnapshot (DB db _ _) = bracket (create db) (release db)

createSnapshot :: MonadResource m => DB -> m Snapshot
createSnapshot db = liftM snd (createSnapshot' db)

createSnapshot' :: MonadResource m => DB -> m (ReleaseKey, Snapshot)
createSnapshot' db = allocate (mkSnap db) (freeSnap db)
    where
        mkSnap (DB db_ptr) =
            liftM Snapshot $ c_leveldb_create_snapshot db_ptr

        freeSnap (DB db_ptr) (Snapshot snap) =
            c_leveldb_release_snapshot db_ptr snap

-- | Get a DB property
getProperty :: MonadResource m => DB -> Property -> m (Maybe ByteString)
getProperty (DB db_ptr) p = liftIO $
    withCString (prop p) $ \prop_ptr -> do
        val_ptr <- c_leveldb_property_value db_ptr prop_ptr
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
destroy :: MonadResource m => FilePath -> Options -> m ()
destroy path opts = do
    (rk, opts') <- allocate (mkOpts opts) freeOpts
    liftIO $ destroy' opts'
    release rk

    where
        destroy' (Options' opts_ptr _) =
            withCString path $ \path_ptr ->
                throwIfErr "destroy" $ c_leveldb_destroy_db opts_ptr path_ptr

-- | Repair the given leveldb database.
repair :: MonadResource m => FilePath -> Options -> m ()
repair path opts = do
    (rk, opts') <- allocate (mkOpts opts) freeOpts
    liftIO $ repair' opts'
    release rk

    where
        repair' (Options' opts_ptr _) =
            withCString path $ \path_ptr ->
                throwIfErr "repair" $ c_leveldb_repair_db opts_ptr path_ptr

-- TODO: support [Range], like C API does
type Range  = (ByteString, ByteString)

-- | Inspect the approximate sizes of the different levels
approximateSize :: MonadResource m => DB -> Range -> m Int64
approximateSize (DB db_ptr) (from, to) = liftIO $
    UB.unsafeUseAsCStringLen from $ \(from_ptr, flen) ->
    UB.unsafeUseAsCStringLen to   $ \(to_ptr, tlen)   ->
    withArray [from_ptr]          $ \from_ptrs        ->
    withArray [i2s flen]          $ \flen_ptrs        ->
    withArray [to_ptr]            $ \to_ptrs          ->
    withArray [i2s tlen]          $ \tlen_ptrs        ->
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
put (DB db_ptr) opts key value = liftIO $
    UB.unsafeUseAsCStringLen key   $ \(key_ptr, klen) ->
    UB.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
    withCWriteOptions opts         $ \opts_ptr        ->
        throwIfErr "put" $ c_leveldb_put db_ptr opts_ptr
                                         key_ptr (i2s klen)
                                         val_ptr (i2s vlen)

-- | Read a value by key
get :: MonadResource m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
get (DB db_ptr) opts key = liftIO $
    UB.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    withCReadOptions opts        $ \opts_ptr        ->
    alloca                       $ \vlen_ptr        -> do
        val_ptr <- throwIfErr "get" $
            c_leveldb_get db_ptr opts_ptr key_ptr (i2s klen) vlen_ptr
        vlen <- peek vlen_ptr
        if val_ptr == nullPtr
            then return Nothing
            else do
                res <- liftM Just $ SB.packCStringLen (val_ptr, s2i vlen)
                free val_ptr
                return res

-- | Delete a key/value pair
delete :: MonadResource m => DB -> WriteOptions -> ByteString -> m ()
delete (DB db_ptr) opts key = liftIO $
    UB.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    withCWriteOptions opts       $ \opts_ptr        ->
        throwIfErr "delete" $ c_leveldb_delete db_ptr opts_ptr key_ptr (i2s klen)

-- | Perform a batch mutation
write :: MonadResource m => DB -> WriteOptions -> WriteBatch -> m ()
write (DB db_ptr) opts batch = liftIO $
    withCWriteOptions opts $ \opts_ptr  ->
    withCWriteBatch batch  $ \batch_ptr ->
        throwIfErr "write"
        $ c_leveldb_write db_ptr opts_ptr batch_ptr

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
-- withIterator :: DB -> ReadOptions -> (Iterator -> IO a) -> IO a
-- withIterator db opts = bracket (iterOpen db opts) iterClose

-- | Create an Iterator. Consider using withIterator.
iterOpen :: MonadResource m => DB -> ReadOptions -> m Iterator
iterOpen db opts = liftM snd (iterOpen' db opts)

iterOpen' :: MonadResource m => DB -> ReadOptions -> m (ReleaseKey, Iterator)
iterOpen' db opts = allocate (mkIter db opts) freeIter
    where
        mkIter (DB db_ptr) opts' = do
            lock   <- liftIO $ newMVar ()
            it_ptr <- liftIO $ withCReadOptions opts' $ \opts_ptr ->
                          throwErrnoIfNull "create_iterator"
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
        go _ = UB.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
                   c_leveldb_iter_seek iter key_ptr (i2s klen)

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
-- /true/ iff the iterator was not positioned the last entry in the source.
iterNext :: MonadResource m => Iterator -> m ()
iterNext (Iterator iter lck) = liftIO $ withMVar lck go
    where
        go _ = c_leveldb_iter_next iter

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
iterPrev :: MonadResource m => Iterator -> m ()
iterPrev (Iterator iter lck) = liftIO $ withMVar lck go
    where
        go _ = c_leveldb_iter_prev iter

-- | Return the key for the current entry. The underlying storage for the
-- returned slice is valid only until the next modification of the iterator.
iterKey :: MonadResource m => Iterator -> m ByteString
iterKey (Iterator iter _) = liftIO $
    alloca $ \len_ptr -> do
        key_ptr <- c_leveldb_iter_key iter len_ptr
        klen <- peek len_ptr
        if key_ptr /= nullPtr
            then SB.packCStringLen (key_ptr, s2i klen)
            else throwIO $ userError "null key"

-- | Return the value for the current entry. The underlying storage for the
-- returned slice is valid only until the next modification of the iterator.
iterValue :: MonadResource m => Iterator -> m ByteString
iterValue (Iterator iter _) = liftIO $
    alloca $ \len_ptr -> do
        val_ptr <- c_leveldb_iter_value iter len_ptr
        vlen <- peek len_ptr
        if val_ptr /= nullPtr
            then SB.packCStringLen (val_ptr, s2i vlen)
            else throwIO $ userError "null value"

-- | Map a function over an iterator, returning the value. The iterator
-- should be put in the right position prior to calling this with the iterator.
mapIter :: MonadResource m => (Iterator -> m a) -> Iterator -> m [a]
mapIter f iter = do
    valid <- iterValid iter
    if not valid
        then return []
        else do
            val <- f iter
            _   <- iterNext iter
            fmap (val :) $ mapIter f iter

-- | Return a list of key and value tuples from an iterator. The iterator
-- should be put in the right position prior to calling this with the iterator.
iterItems :: MonadResource m => Iterator -> m [(ByteString, ByteString)]
iterItems = mapIter $ \iter -> (,) <$> iterKey iter <*> iterValue iter

-- | Return a list of key from an iterator. The iterator should be put
-- in the right position prior to calling this with the iterator.
iterKeys :: MonadResource m => Iterator -> m [ByteString]
iterKeys = mapIter iterKey

-- | Return a list of values from an iterator. The iterator should be put
-- in the right position prior to calling this with the iterator.
iterValues :: MonadResource m => Iterator -> m [ByteString]
iterValues = mapIter iterValue

--
-- Internal
--

mkOpts :: Options -> IO Options'
mkOpts opts = do
    opts_ptr <- c_leveldb_options_create
    cache    <- maybe (return Nothing)
                      (liftM Just . setcache opts_ptr)
                      (maybeCacheSize opts)
    mapM_ (setopt opts_ptr) opts

    return (Options' opts_ptr cache)

    where
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

        maybeCacheSize os = find isCs os >>= \(CacheSize s) -> return s

        isCs (CacheSize _) = True
        isCs _             = False

        setcache :: OptionsPtr -> Int -> IO CachePtr
        setcache opts_ptr s = do
            cache_ptr <- c_leveldb_cache_create_lru $ i2s s
            c_leveldb_options_set_cache opts_ptr cache_ptr
            return cache_ptr

freeOpts :: Options' -> IO ()
freeOpts (Options' opts_ptr mcache_ptr) = do
    c_leveldb_options_destroy opts_ptr
    maybe (return ()) c_leveldb_cache_destroy mcache_ptr
    return ()

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

throwIfErr :: String -> (ErrPtr -> IO a) -> IO a
throwIfErr s f = alloca $ \err_ptr -> do
    poke err_ptr nullPtr
    res  <- f err_ptr
    erra <- peek err_ptr
    when (erra /= nullPtr) $ do
        err <- peekCString erra
        throwIO $ userError $ s ++ ": " ++ err
    return res

s2i :: CSize -> Int
s2i = fromIntegral
{-# INLINE s2i #-}

i2s :: Int -> CSize
i2s = fromIntegral
{-# INLINE i2s #-}

i2ci :: Int -> CInt
i2ci = fromIntegral
{-# INLINE i2ci #-}
