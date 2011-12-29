module Database.LevelDB (
  -- * Basic Types
    DB
  , Options
  , Option(..)
  , WriteOptions
  , WriteOption(..)
  , ReadOptions
  , ReadOption(..)
  , Compression(..)
  , WriteBatch
  , BatchOp(..)

  -- * Basic Database Manipulation
  , withLevelDB
  , open
  , close
  , destroy
  , put
  , delete
  , write
  , get

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

  -- * Utilities
  , defaultOptions
  , defaultReadOptions
  , defaultWriteOptions
) where

import Control.Exception (bracket)
import Control.Monad     (liftM, when)
import Data.ByteString   (ByteString)
import Foreign           (alloca, peek, nullPtr)
import Foreign.C.Error   (throwErrnoIfNull)
import Foreign.C.String  (withCString, peekCString)

import Database.LevelDB.Base

import qualified Data.ByteString as SB
import qualified Data.ByteString.Unsafe as UB

-- import Debug.Trace

-- | Database Handle
newtype DB = DB { _ldb :: LevelDBPtr }

-- | Iterator Handle
newtype Iterator = Iterator { _iter :: IteratorPtr }

data Compression = NoCompression
                 | Snappy
                 deriving (Eq, Show)

type Options = [Option]
data Option = CreateIfMissing
            | ErrorIfExists
            | ParanoidChecks
            | WriteBufferSize Int
            | MaxOpenFiles Int
            | BlockSize Int
            | BlockRestartInterval Int
            | UseCompression Compression
            -- logger
            -- comparator
            deriving (Eq, Show)

type WriteOptions = [WriteOption]
data WriteOption  = Sync
                  deriving (Show)

type ReadOptions = [ReadOption]
data ReadOption  = VerifyCheckSums
                 | FillCache
                 -- snapshot
                 deriving (Eq, Show)

type WriteBatch = [BatchOp]
data BatchOp = Put ByteString ByteString
             | Del ByteString
             deriving (Show)

-- | Default options acc. to util/options.cc
defaultOptions :: Options
defaultOptions = [ CreateIfMissing ]

defaultWriteOptions :: WriteOptions
defaultWriteOptions = []

defaultReadOptions :: ReadOptions
defaultReadOptions = []

withLevelDB :: String -> Options -> (DB -> IO a) -> IO a
withLevelDB name opts = bracket (open name opts) close

-- | Open a leveldb database with the given name and options
open :: String -> Options -> IO DB
open name opts =
    withCString name  $ \cname ->
    withCOptions opts $ \copts ->
    alloca            $ \cerr  ->
    liftM DB $
        throwIfErr "open" cerr $ c_leveldb_open copts cname

-- | Close the given leveldb database
close :: DB -> IO ()
close (DB db) = c_leveldb_close db

-- | Destroy the given leveldb database.
destroy :: String -> Options -> IO ()
destroy name opts =
    withCString name  $ \cname ->
    withCOptions opts $ \copts ->
    alloca            $ \cerr ->
    throwIfErr "destroy" cerr $ c_leveldb_destroy_db copts cname

-- | Write a key/value pair
put :: DB -> WriteOptions -> ByteString -> ByteString -> IO ()
put (DB db) opts key value =
    UB.unsafeUseAsCStringLen key   $ \(k,kl) ->
    UB.unsafeUseAsCStringLen value $ \(v,vl) ->
    withCWriteOptions opts         $ \copts  ->
    alloca                         $ \cerr   -> do
    let klen = fromIntegral kl
        vlen = fromIntegral vl
    throwIfErr "put" cerr $
        c_leveldb_put db copts k klen v vlen

-- | Read a value by key
get :: DB -> ReadOptions -> ByteString -> IO (Maybe ByteString)
get (DB db) opts key =
    UB.unsafeUseAsCStringLen key $ \(k,kl) ->
    withCReadOptions opts        $ \copts  ->
    alloca                       $ \cerr   ->
    alloca                       $ \vl     -> do
    v    <- throwIfErr "get" cerr $
                c_leveldb_get db copts k (fromIntegral kl) vl
    vlen <- peek vl
    if v /= nullPtr
        then liftM Just $
                 SB.packCStringLen (v, fromInteger . toInteger $ vlen)
        else return Nothing

-- | Delete a key/value pair
delete :: DB -> WriteOptions -> ByteString -> IO ()
delete (DB db) opts key =
    UB.unsafeUseAsCStringLen key $ \(k,kl) ->
    withCWriteOptions opts       $ \copts  ->
    alloca                       $ \cerr   ->
    throwIfErr "delete" cerr $
        c_leveldb_delete db copts k (fromIntegral kl)

-- | Perform a batch mutation
write :: DB -> WriteOptions -> WriteBatch -> IO ()
write (DB db) opts batch =
    withCWriteOptions opts $ \copts  ->
    withCWriteBatch batch  $ \cbatch ->
    alloca                 $ \cerr   ->
    throwIfErr "write" cerr $
        c_leveldb_write db copts cbatch

    where
        withCWriteBatch b f = do
            cbatch <- c_leveldb_writebatch_create
            mapM_ (batchAdd cbatch) b
            res <- f cbatch
            c_leveldb_writebatch_destroy cbatch
            return res

        batchAdd cbatch (Put key val) =
            UB.unsafeUseAsCStringLen key $ \(k,kl) ->
            UB.unsafeUseAsCStringLen val $ \(v,vl) ->
            c_leveldb_writebatch_put cbatch
                                     k (fromIntegral kl)
                                     v (fromIntegral vl)

        batchAdd cbatch (Del key) =
            UB.unsafeUseAsCStringLen key $ \(k,kl) ->
            c_leveldb_writebatch_delete cbatch k (fromIntegral kl)

-- | Run an action with an Iterator. The iterator will be closed after the
-- action returns or an error is thrown. Thus, the iterator will /not/ be valid
-- after this function terminates.
withIterator :: DB -> ReadOptions -> (Iterator -> IO a) -> IO a
withIterator db opts = bracket (iterOpen db opts) iterClose

-- | Create an Iterator. Consider using withIterator.
iterOpen :: DB -> ReadOptions -> IO Iterator
iterOpen (DB db) opts =
    withCReadOptions opts $ \copts ->
    liftM Iterator
        $ throwErrnoIfNull "create_iterator"
        $ c_leveldb_create_iterator db copts

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
    UB.unsafeUseAsCStringLen key $ \(k,kl) ->
    c_leveldb_iter_seek iter k (fromIntegral kl)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: Iterator -> IO ()
iterFirst (Iterator iter) = c_leveldb_iter_seek_to_first iter

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: Iterator -> IO ()
iterLast (Iterator iter) = c_leveldb_iter_seek_to_last iter

-- | Moves to the next entry in the source. After this call, /iterValid/ is
-- /true/ iff the iterator was not positioned the last entry in the source.
iterNext :: Iterator -> IO ()
iterNext (Iterator iter) = c_leveldb_iter_next iter

-- | Moves to the previous entry in the source. After this call, /iterValid/ is
-- /true/ iff the iterator was not positioned at the first entry in the source.
iterPrev :: Iterator -> IO ()
iterPrev (Iterator iter) = c_leveldb_iter_prev iter

-- | Return the key for the current entry. The underlying storage for the
-- returned slice is valid only until the next modification of the iterator.
iterKey :: Iterator -> IO ByteString
iterKey (Iterator iter) =
    alloca $ \clen -> do
    key  <- c_leveldb_iter_key iter clen
    klen <- peek clen
    if key /= nullPtr
        then SB.packCStringLen (key, fromInteger . toInteger $ klen)
        else ioError $ userError "null key"

-- | Return the value for the current entry. The underlying storage for the
-- returned slice is valid only until the next modification of the iterator.
iterValue :: Iterator -> IO ByteString
iterValue (Iterator iter) =
    alloca $ \clen -> do
    val  <- c_leveldb_iter_value iter clen
    vlen <- peek clen
    SB.packCStringLen (val, fromInteger . toInteger $ vlen)


-- | Internal

withCOptions :: Options -> (OptionsPtr -> IO a) -> IO a
withCOptions opts f = do
    copts <- c_leveldb_options_create
    mapM_ (setopt copts) opts
    c_leveldb_options_set_create_if_missing copts 1
    res <- f copts
    c_leveldb_options_destroy copts
    return res

    where
        setopt copts CreateIfMissing =
            c_leveldb_options_set_create_if_missing copts 1
        setopt copts ErrorIfExists =
            c_leveldb_options_set_error_if_exists copts 1
        setopt copts ParanoidChecks =
            c_leveldb_options_set_paranoid_checks copts 1
        setopt copts (WriteBufferSize s) =
            c_leveldb_options_set_write_buffer_size copts $ fromIntegral s
        setopt copts (MaxOpenFiles n) =
            c_leveldb_options_set_max_open_files copts $ fromIntegral n
        setopt copts (BlockSize s) =
            c_leveldb_options_set_block_size copts $ fromIntegral s
        setopt copts (BlockRestartInterval i) =
            c_leveldb_options_set_block_restart_interval copts $ fromIntegral i
        setopt copts (UseCompression NoCompression) =
            c_leveldb_options_set_compression copts noCompression
        setopt copts (UseCompression Snappy) =
            c_leveldb_options_set_compression copts snappyCompression

withCWriteOptions :: WriteOptions -> (WriteOptionsPtr -> IO a) -> IO a
withCWriteOptions opts f = do
    copts <- c_leveldb_writeoptions_create
    mapM_ (setopt copts) opts
    res <- f copts
    c_leveldb_writeoptions_destroy copts
    return res

    where
        setopt copts Sync = c_leveldb_writeoptions_set_sync copts 1

withCReadOptions :: ReadOptions -> (ReadOptionsPtr -> IO a) -> IO a
withCReadOptions opts f = do
    copts <- c_leveldb_readoptions_create
    mapM_ (setopt copts) opts
    res <- f copts
    c_leveldb_readoptions_destroy copts
    return res

    where
        setopt copts VerifyCheckSums =
            c_leveldb_readoptions_set_verify_checksums copts 1
        setopt copts FillCache =
            c_leveldb_readoptions_set_fill_cache copts 1

throwIfErr :: String -> ErrPtr -> (ErrPtr -> IO a) -> IO a
throwIfErr s cerr f = do
    res  <- f cerr
    erra <- peek cerr
    when (erra /= nullPtr) $ do
        err <- peekCString erra
        ioError $ userError $ s ++ ": " ++ err
    return res
