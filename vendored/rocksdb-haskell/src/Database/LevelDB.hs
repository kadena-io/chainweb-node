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

import Control.Applicative                ((<$>))
import Control.Monad.Trans.Resource
import Data.Int                           (Int64)
import Data.ByteString                    (ByteString)

import Database.LevelDB.Base              (BatchOp, BloomFilter, Comparator, Compression,
                                           DB, FilterPolicy, Iterator, Options, Property, Range,
                                           ReadOptions, Snapshot, WriteBatch, WriteOptions,
                                           defaultOptions, defaultReadOptions, defaultWriteOptions)
import qualified Database.LevelDB.Base    as Base

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
mapIter :: MonadResource m => (Iterator -> m a) -> Iterator -> m [a]
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
version = Base.version
