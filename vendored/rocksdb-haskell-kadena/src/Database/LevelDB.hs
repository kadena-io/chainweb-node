-- |
-- Module      : Database.LevelDB
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
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

import           Control.Applicative          ((<$>))
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Int                     (Int64)

import           Database.LevelDB.Base        (BatchOp, BloomFilter, Comparator,
                                               Compression, DB, FilterPolicy,
                                               Iterator, Options, Property,
                                               Range, ReadOptions, Snapshot,
                                               WriteBatch, WriteOptions,
                                               defaultOptions,
                                               defaultReadOptions,
                                               defaultWriteOptions)
import qualified Database.LevelDB.Base        as Base

bloomFilter :: MonadResource m => Int -> m BloomFilter
bloomFilter i =
    snd <$> allocate (Base.createBloomFilter i)
                      Base.releaseBloomFilter


-- | Open a database
--
-- The returned handle will automatically be released when the enclosing
-- 'runResourceT' terminates.
open :: MonadResource m => FilePath -> Options -> m DB
open path opts = snd <$> open' path opts

open' :: MonadResource m => FilePath -> Options -> m (ReleaseKey, DB)
open' path opts = allocate (Base.open path opts) Base.close
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
createSnapshot' db = allocate (Base.createSnapshot db) (Base.releaseSnapshot db)

-- | Get a DB property
getProperty :: MonadResource m => DB -> Property -> m (Maybe ByteString)
getProperty = Base.getProperty

-- | Destroy the given leveldb database.
destroy :: MonadResource m => FilePath -> Options -> m ()
destroy = Base.destroy

-- | Repair the given leveldb database.
repair :: MonadResource m => FilePath -> Options -> m ()
repair = Base.repair


-- | Inspect the approximate sizes of the different levels
approximateSize :: MonadResource m => DB -> Range -> m Int64
approximateSize = Base.approximateSize

-- | Write a key/value pair
put :: MonadResource m => DB -> WriteOptions -> ByteString -> ByteString -> m ()
put = Base.put

-- | Read a value by key
get :: MonadResource m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
get = Base.get

-- | Delete a key/value pair
delete :: MonadResource m => DB -> WriteOptions -> ByteString -> m ()
delete = Base.delete

-- | Perform a batch mutation
write :: MonadResource m => DB -> WriteOptions -> WriteBatch -> m ()
write = Base.write


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
iterOpen' db opts = allocate (Base.createIter db opts) Base.releaseIter

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: MonadResource m => Iterator -> m Bool
iterValid = Base.iterValid

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: MonadResource m => Iterator -> ByteString -> m ()
iterSeek = Base.iterSeek

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: MonadResource m => Iterator -> m ()
iterFirst = Base.iterFirst

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: MonadResource m => Iterator -> m ()
iterLast = Base.iterLast

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the last entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterPrev' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterNext :: MonadResource m => Iterator -> m ()
iterNext = Base.iterNext

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterNext' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterPrev :: MonadResource m => Iterator -> m ()
iterPrev = Base.iterPrev

-- | Return the key for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterKey :: MonadResource m => Iterator -> m (Maybe ByteString)
iterKey = Base.iterKey

-- | Return the value for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterValue :: MonadResource m => Iterator -> m (Maybe ByteString)
iterValue = Base.iterValue

-- | Check for errors
--
-- Note that this captures somewhat severe errors such as a corrupted database.
iterGetError :: MonadResource m => Iterator -> m (Maybe ByteString)
iterGetError = Base.iterGetError


-- | Map a function over an iterator, advancing the iterator forward and
-- returning the value. The iterator should be put in the right position prior
-- to calling the function.
--
-- Note that this function accumulates the result strictly, ie. it reads all
-- values into memory until the iterator is exhausted. This is most likely not
-- what you want for large ranges. You may consider using conduits instead, for
-- an example see: <https://gist.github.com/adc8ec348f03483446a5>
mapIter :: MonadResource m => (Iterator -> IO a) -> Iterator -> m [a]
mapIter = Base.mapIter

-- | Return a list of key and value tuples from an iterator. The iterator
-- should be put in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'.
iterItems :: MonadResource m => Iterator -> m [(ByteString, ByteString)]
iterItems = Base.iterItems

-- | Return a list of key from an iterator. The iterator should be put
-- in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'
iterKeys :: MonadResource m => Iterator -> m [ByteString]
iterKeys = Base.iterKeys

-- | Return a list of values from an iterator. The iterator should be put
-- in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'
iterValues :: MonadResource m => Iterator -> m [ByteString]
iterValues = Base.iterValues


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
        $ c_leveldb_readoptions_set_fill_cache opts_ptr
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