{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.CAS.RocksDB
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Persisted iey-value and and content-addressable-key-value stores with a
-- RocksDB backend.
--
-- A 'RocksDbTable' provides a typed key-value store with an additional iterator
-- interace.
--
-- 'RocksDbCas' adds an 'IsCas' instance for a 'RocksDbTable' where the value
-- type is content-addressable.
--
-- TODO: Abstract the 'RocksDbTable' API into a typeclass so that one can
-- provide alterantive implementations for it.
--
module Data.CAS.RocksDB
( RocksDb(..)
, rocksDbHandle
, rocksDbNamespace
, openRocksDb
, closeRocksDb
, withRocksDb
, withTempRocksDb
, destroyRocksDb
, resetOpenRocksDb

-- * Rocks DB Table
, Codec(..)
, RocksDbTable
, newTable
, tableLookup
, tableInsert
, tableDelete

-- * Rocks DB Table Iterator
, RocksDbTableIter
, createTableIter
, releaseTableIter
, withTableIter
, tableIterValid

-- ** Seeking
, tableIterSeek
, tableIterFirst
, tableIterLast

-- ** Advance Iterator
, tableIterNext
, tableIterPrev

-- ** Query Iterator
, tableIterEntry
, tableIterValue
, tableIterKey

-- ** Streams
, iterToEntryStream
, iterToValueStream
, iterToKeyStream

-- ** Extremal Table Entries
, tableMaxKey
, tableMaxValue
, tableMaxEntry
, tableMinKey
, tableMinValue
, tableMinEntry

-- * RocksDbCas
, RocksDbCas(..)
, newCas
) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.CAS
import Data.String
import qualified Data.Text as T

import qualified Database.RocksDB.Base as R
import qualified Database.RocksDB.Iterator as I

import GHC.Generics (Generic)
import GHC.Stack

import qualified Streaming.Prelude as S

import System.IO.Temp

-- -------------------------------------------------------------------------- --
-- Utils

-- | A newtype wrapper for tagger values as "expected" outcomes of some
-- computation.
--
newtype Expected a = Expected { getExpected :: a }
    deriving (Show, Eq, Ord, Generic, Functor)

-- | A newtype wrapper for tagger values as "actual" outcomes of some
-- computation.
--
newtype Actual a = Actual { getActual :: a }
    deriving (Show, Eq, Ord, Generic, Functor)

-- | A textual message that describes the 'Expected' and the 'Actual' outcome of
-- some computation.
--
unexpectedMsg :: Show a => T.Text -> Expected a -> Actual a -> T.Text
unexpectedMsg msg expected actual = msg
    <> ", expected: " <> sshow (getExpected expected)
    <> ", actual: " <> sshow (getActual actual)

-- | Show a value as any type that is an instance of 'IsString'.
--
sshow :: Show a => IsString b => a -> b
sshow = fromString . show
{-# INLINE sshow #-}

-- -------------------------------------------------------------------------- --
-- RocksDb

-- | This wrapper allows to create key namespaces. This is, for instance, useful
-- for testing when running several concurrent tests on the same rocks db
-- instances.
--
data RocksDb = RocksDb
    { _rocksDbHandle :: !R.DB
    , _rocksDbNamespace :: !B.ByteString
    }

makeLenses ''RocksDb

-- | Open a 'RocksDb' instance with the default namespace. If no rocks db exists
-- at the provided directory path, a new database is created.
--
openRocksDb :: FilePath -> IO RocksDb
openRocksDb path = RocksDb <$> R.open path opts <*> mempty
  where
    opts = R.defaultOptions { R.createIfMissing = True }

-- | Open a 'RocksDb' and reset it if it already exists.
--
resetOpenRocksDb :: FilePath -> IO RocksDb
resetOpenRocksDb path = do
    destroyRocksDb path
    RocksDb <$> R.open path opts <*> mempty
  where
    opts = R.defaultOptions { R.createIfMissing = True, R.errorIfExists = True }

-- | Close a 'RocksDb' instance.
--
closeRocksDb :: RocksDb -> IO ()
closeRocksDb = R.close . _rocksDbHandle

-- | Provide a computation with a 'RocksDb' instance. If no rocks db exists at
-- the provided directory path, a new database is created.
--
withRocksDb :: FilePath -> (RocksDb -> IO a) -> IO a
withRocksDb path = bracket (openRocksDb path) closeRocksDb

-- | Provide a computation with a temporary 'RocksDb'. The database is deleted
-- when the computation exits.
--
withTempRocksDb :: String -> (RocksDb -> IO a) -> IO a
withTempRocksDb template f = withSystemTempDirectory template $ \dir ->
    withRocksDb dir f

-- | Delete the RocksDb instance.
--
-- This is the prefered method of deleting an rocks db instance. A rocks db
-- instance may store files in different locations. This function guarantees
-- that all files are deleted.
--
destroyRocksDb :: FilePath -> IO ()
destroyRocksDb path = R.destroy path opts
  where
    opts = R.defaultOptions { R.createIfMissing = False }

-- -------------------------------------------------------------------------- --
-- RocksDb Table

-- NOTE: the implementation currently doesnt' support iteration over nested
-- namespaces. This could easily be added via an iterator that iterates over
-- all respective prefixes ending with '/' or '$' and using a codec that
-- could handle the types in all sub-namespaces. This could for instance
-- be useful for iterating over the complete database.

-- | A binary codec for encoding and decoding values that are stored in a
-- 'RocksDb' Table.
--
data Codec a = Codec
    { _codecEncode :: !(a -> B.ByteString)
        -- ^ encode a value.
    , _codecDecode :: !(forall m . MonadThrow m => B.ByteString -> m a)
        -- ^ decode a value. Throws an exception of decoding fails.
    }

-- | A logical table in a 'RocksDb'. Tables in a rocks db are have isolated key
-- namespaces.
--
data RocksDbTable k v = RocksDbTable
    { _rocksDbTableValueCodec :: !(Codec v)
    , _rocksDbTableKeyCodec :: !(Codec k)
    , _rocksDbTableName :: !B.ByteString
    , _rocksDbTableDb :: !R.DB
    }

-- | Create a new 'RocksDbTable' in the given 'RocksDb'.
--
-- Table name components must NOT contain any of '$', '%', and '/'. A user
-- error is raised if the namespace contains any of these characters.
--
newTable
    :: HasCallStack
    => RocksDb
    -> Codec v
    -> Codec k
    -> [B.ByteString]
    -> RocksDbTable k v
newTable db valCodec keyCodec namespace
    | any (B8.any (\x -> x `elem` ['$', '%', '/'])) namespace
        = error $ "Data.CAS.RocksDb.newTable: invalid character in table namespace: " <> sshow namespace
    | otherwise
        = RocksDbTable valCodec keyCodec ns (_rocksDbHandle db)
  where
    ns = _rocksDbNamespace db <> "-" <> B.intercalate "/" namespace <> "$"
{-# INLINE newTable #-}

-- | @tableInsert db k v@ inserts the value @v@ at key @k@ in the rocks db table
-- @db@.
--
tableInsert :: RocksDbTable k v -> k -> v -> IO ()
tableInsert db k v = R.put
    (_rocksDbTableDb db)
    R.defaultWriteOptions
    (encKey db k)
    (encVal db v)
{-# INLINE tableInsert #-}

-- | @tableLookup db k@ returns 'Just' the value at key @k@ in the
-- 'RocksDbTable' @db@ if it exists, or 'Nothing' if the @k@ doesn't exist in
-- the table.
--
tableLookup :: RocksDbTable k v -> k -> IO (Maybe v)
tableLookup db k = do
    maybeBytes <- R.get (_rocksDbTableDb db) R.defaultReadOptions (encKey db k)
    traverse (decVal db) maybeBytes
{-# INLINE tableLookup #-}

-- | @tableDelete db k@ deletes the value at the key @k@ from the 'RocksDbTable'
-- db. If the @k@ doesn't exist in @db@ this function does nothing.
--
tableDelete :: RocksDbTable k v -> k -> IO ()
tableDelete db k = R.delete
    (_rocksDbTableDb db)
    R.defaultWriteOptions
    (encKey db k)
{-# INLINE tableDelete #-}

-- -------------------------------------------------------------------------- --
-- Table Iterator

-- | An iterator for a 'RocksDbTable'. An interator is a stateful object that
-- represents an enumeration of a subset of the entries of an immutable snapshop
-- of the table.
--
-- An iterator should be used single threaded. Since the iterator retains the
-- snapshot of the database, one should avoid storing iterators over longer
-- periods of time. After usage it should be released in a timely manner.
--
-- The recommended way to created a 'RocksDbTableIter' is to use the function
-- `withTableIter`.
--
data RocksDbTableIter k v = RocksDbTableIter
    { _rocksDbTableIterValueCodec :: !(Codec v)
    , _rocksDbTableIterKeyCodec :: !(Codec k)
    , _rocksDbTableIterNamespace :: !B.ByteString
    , _rocksDbTableIter :: !I.Iterator
    }

-- | Creates a 'RocksDbTableIterator'. If the 'RocksDbTable' is not empty, the
-- iterator is pointing to the first key in the 'RocksDbTable' and is valid.
--
-- The returnd iterator must be released with 'releaseTableIter' when it is not
-- needed any more. Not doing so release in a data leak that retains database
-- snapshots.
--
createTableIter :: RocksDbTable k v -> IO (RocksDbTableIter k v)
createTableIter db = do
    !tit <- RocksDbTableIter
        (_rocksDbTableValueCodec db)
        (_rocksDbTableKeyCodec db)
        (_rocksDbTableName db)
        <$> I.createIter (_rocksDbTableDb db) R.defaultReadOptions
    tableIterFirst tit
    return tit
{-# INLINE createTableIter #-}

-- | Releases an 'RocksDbTableIteror', freeing up it's resources.
--
releaseTableIter :: RocksDbTableIter k v -> IO ()
releaseTableIter = I.releaseIter . _rocksDbTableIter
{-# INLINE releaseTableIter #-}

-- | Provide an computation with a 'RocksDbTableIteror' and release the iterator
-- after after the computation has finished either by returning a result or
-- throwing an exception.
--
-- This is function provides the prefered way of creating and using a
-- 'RocksDbTableIter'.
--
withTableIter :: RocksDbTable k v -> (RocksDbTableIter k v -> IO a) -> IO a
withTableIter db = bracket (createTableIter db) releaseTableIter
{-# INLINE withTableIter #-}

-- | Checks if an 'RocksDbTableIterator' is valid.
--
-- A valid iterator returns a value when 'tableIterEntry', 'tableIterValue', or
-- 'tableIterKey' is called on it.
--
tableIterValid :: MonadIO m => RocksDbTableIter k v -> m Bool
tableIterValid it = I.iterKey (_rocksDbTableIter it) >>= \case
    Nothing -> return False
    (Just !x) -> return $! checkIterKey it x
{-# INLINE tableIterValid #-}

-- | Efficiently seek to a key in a 'RocksDbTableIterator' iteration.
--
tableIterSeek :: MonadIO m => RocksDbTableIter k v -> k -> m ()
tableIterSeek it = I.iterSeek (_rocksDbTableIter it) . encIterKey it
{-# INLINE tableIterSeek #-}

-- | Seek to the first key in a 'RocksDbTable'.
--
tableIterFirst :: MonadIO m => RocksDbTableIter k v -> m ()
tableIterFirst it
    = I.iterSeek (_rocksDbTableIter it) (_rocksDbTableIterNamespace it)
{-# INLINE tableIterFirst #-}

-- | Seek to the last value in a 'RocksDbTable'
--
tableIterLast :: MonadIO m => RocksDbTableIter k v -> m ()
tableIterLast it = do
    I.iterSeek (_rocksDbTableIter it) (namespaceLast it)
    I.iterPrev (_rocksDbTableIter it)
{-# INLINE tableIterLast #-}

-- | Move a 'RocksDbTableIter' to the next key in a 'RocksDbTable'.
--
tableIterNext :: MonadIO m => RocksDbTableIter k v -> m ()
tableIterNext = I.iterNext . _rocksDbTableIter
{-# INLINE tableIterNext #-}

-- | Move a 'RocksDbTableIter' to the previous key in a 'RocksDbTable'.
--
tableIterPrev :: MonadIO m => RocksDbTableIter k v -> m ()
tableIterPrev = I.iterPrev . _rocksDbTableIter
{-# INLINE tableIterPrev #-}

-- | Returns the key and the value at the current position of a
-- 'RocksDbTableIter'. Returns 'Nothing' if the iterator is invalid.
--
tableIterEntry
    :: MonadIO m
    => MonadThrow m
    => RocksDbTableIter k v
    -> m (Maybe (k, v))
tableIterEntry it = I.iterEntry (_rocksDbTableIter it) >>= \case
    Nothing -> return Nothing
    Just (k, v) -> do
        tryDecIterKey it k >>= \case
            Nothing -> return Nothing
            (Just !k') -> do
                !v' <- decIterVal it v
                return $! Just $! (k', v')
{-# INLINE tableIterEntry #-}

-- | Returns the value at the current position of a 'RocksDbTableIter'. Returns
-- 'Nothing' if the iterator is invalid.
--
tableIterValue
    :: MonadIO m
    => MonadThrow m
    => RocksDbTableIter k v
    -> m (Maybe v)
tableIterValue it = fmap snd <$> tableIterEntry it
{-# INLINE tableIterValue #-}

-- | Returns the key at the current position of a 'RocksDbTableIter'. Returns
-- 'Nothing' if the iterator is invalid.
--
tableIterKey
    :: MonadIO m
    => MonadThrow m
    => RocksDbTableIter k v
    -> m (Maybe k)
tableIterKey it = I.iterKey (_rocksDbTableIter it) >>= \case
    Nothing -> return Nothing
    Just k -> tryDecIterKey it k
{-# INLINE tableIterKey #-}

-- | Returns the stream of key-value pairs of an 'RocksDbTableIter'.
--
-- The iterator must be released after the stream is consumed. Releasing the
-- iterator to early while the stream is still in use results in a runtime
-- error. Not releasing the iterator after the processing of the stream has
-- finished results in a memory leak.
--
iterToEntryStream :: MonadIO m => RocksDbTableIter k v -> S.Stream (S.Of (k,v)) m ()
iterToEntryStream it = liftIO (tableIterEntry it) >>= \case
    Nothing -> return ()
    Just x -> S.yield x >> tableIterNext it >> iterToEntryStream it
{-# INLINE iterToEntryStream #-}

-- | Returns the stream of values of an 'RocksDbTableIter'.
--
-- The iterator must be released after the stream is consumed. Releasing the
-- iterator to early while the stream is still in use results in a runtime
-- error. Not releasing the iterator after the processing of the stream has
-- finished results in a memory leak.
--
iterToValueStream :: MonadIO m => RocksDbTableIter k v -> S.Stream (S.Of v) m ()
iterToValueStream it = liftIO (tableIterValue it) >>= \case
    Nothing -> return ()
    Just x -> S.yield x >> tableIterNext it >> iterToValueStream it
{-# INLINE iterToValueStream #-}

-- | Returns the stream of keys of an 'RocksDbTableIter'.
--
-- The iterator must be released after the stream is consumed. Releasing the
-- iterator to early while the stream is still in use results in a runtime
-- error. Not releasing the iterator after the processing of the stream has
-- finished results in a memory leak.
--
iterToKeyStream :: MonadIO m => RocksDbTableIter k v -> S.Stream (S.Of k) m ()
iterToKeyStream it = liftIO (tableIterKey it) >>= \case
    Nothing -> return ()
    Just x -> S.yield x >> tableIterNext it >> iterToKeyStream it
{-# INLINE iterToKeyStream #-}

-- Extremal Table Entries

-- | The maximum key in a 'RocksDbTable'.
--
tableMaxKey :: RocksDbTable k v -> IO (Maybe k)
tableMaxKey = flip withTableIter $ \i -> tableIterLast i *> tableIterKey i
{-# INLINE tableMaxKey #-}

-- | The maximum value in a 'RocksDbTable'.
--
tableMaxValue :: RocksDbTable k v -> IO (Maybe v)
tableMaxValue = flip withTableIter $ \i -> tableIterLast i *> tableIterValue i
{-# INLINE tableMaxValue #-}

-- | The maximum key-value in a 'RocksDbTable'.
--
tableMaxEntry :: RocksDbTable k v -> IO (Maybe (k, v))
tableMaxEntry = flip withTableIter $ \i -> tableIterLast i *> tableIterEntry i
{-# INLINE tableMaxEntry #-}

-- | The minimum key in a 'RocksDbTable'.
--
tableMinKey :: RocksDbTable k v -> IO (Maybe k)
tableMinKey = flip withTableIter $ \i -> tableIterFirst i *> tableIterKey i
{-# INLINE tableMinKey #-}

-- | The minimum value in a 'RocksDbTable'.
--
tableMinValue :: RocksDbTable k v -> IO (Maybe v)
tableMinValue = flip withTableIter $ \i -> tableIterFirst i *> tableIterValue i
{-# INLINE tableMinValue #-}

-- | The minimum key-value in a 'RocksDbTable'.
--
tableMinEntry :: RocksDbTable k v -> IO (Maybe (k, v))
tableMinEntry = flip withTableIter $ \i -> tableIterFirst i *> tableIterEntry i
{-# INLINE tableMinEntry #-}

-- -------------------------------------------------------------------------- --
-- CAS

-- | For a 'IsCasValue' @v@ with 'CasKeyType v ~ k@,  a 'RocksDbTable k v' is an
-- instance of 'IsCas'.
--
instance (IsCasValue v, CasKeyType v ~ k) => IsCas (RocksDbTable k v) where
    type CasValueType (RocksDbTable k v) = v

    casLookup = tableLookup
    casInsert db a = tableInsert db (casKey a) a
    casDelete = tableDelete

    {-# INLINE casLookup #-}
    {-# INLINE casInsert #-}
    {-# INLINE casDelete #-}

-- | A newtype wrapper that takes only a single type constructor. This useful in
-- situations where a Higher Order type constructor for a CAS is required. A
-- type synonym doesn't work in this situation because type synonyms must be
-- fully applied.
--
newtype RocksDbCas v = RocksDbCas { _getRocksDbCas :: RocksDbTable (CasKeyType v) v }

instance IsCasValue v => IsCas (RocksDbCas v) where
    type instance CasValueType (RocksDbCas v) = v

    casLookup (RocksDbCas x) = casLookup x
    casInsert (RocksDbCas x) = casInsert x
    casDelete (RocksDbCas x) = casDelete x

    {-# INLINE casLookup #-}
    {-# INLINE casInsert #-}
    {-# INLINE casDelete #-}

-- | Create a new 'RocksDbCas'.
--
newCas
    :: CasKeyType v ~ k
    => RocksDb
    -> Codec v
    -> Codec k
    -> [B.ByteString]
    -> RocksDbCas v
newCas db vc kc n = RocksDbCas $ newTable db vc kc n
{-# INLINE newCas #-}

-- -------------------------------------------------------------------------- --
-- Exceptions

-- | Excpeptions that can be thrown by functions in this module.
--
data RocksDbException
    = RocksDbTableIterInvalidKeyNamespace (Expected B.ByteString) (Actual B.ByteString)
    deriving (Eq, Ord, Generic)

instance Exception RocksDbException where
    displayException (RocksDbTableIterInvalidKeyNamespace e a)
        = T.unpack $ unexpectedMsg "Data.CAS.RocksDB: invalid table key" e a
    {-# INLINE displayException #-}

instance Show RocksDbException where
    show = displayException
    {-# INLINE show #-}

-- -------------------------------------------------------------------------- --
-- Table Utils

encVal :: RocksDbTable k v -> v -> B.ByteString
encVal = _codecEncode . _rocksDbTableValueCodec
{-# INLINE encVal #-}

encKey :: RocksDbTable k v -> k -> B.ByteString
encKey it k = prefix <> _codecEncode (_rocksDbTableKeyCodec it) k
  where
    prefix = _rocksDbTableName it
{-# INLINE encKey #-}

decVal :: MonadThrow m => RocksDbTable k v -> B.ByteString -> m v
decVal = _codecDecode . _rocksDbTableValueCodec
{-# INLINE decVal #-}

-- -------------------------------------------------------------------------- --
-- Iter Utils

namespaceLast :: RocksDbTableIter k v -> B.ByteString
namespaceLast it = B.init (_rocksDbTableIterNamespace it) <> "%"
{-# INLINE namespaceLast #-}

encIterKey :: RocksDbTableIter k v -> k -> B.ByteString
encIterKey it k = prefix <> _codecEncode (_rocksDbTableIterKeyCodec it) k
  where
    prefix = _rocksDbTableIterNamespace it
{-# INLINE encIterKey #-}

decIterVal :: MonadThrow m => RocksDbTableIter k v -> B.ByteString -> m v
decIterVal = _codecDecode . _rocksDbTableIterValueCodec
{-# INLINE decIterVal #-}

checkIterKey :: RocksDbTableIter k v -> B.ByteString -> Bool
checkIterKey it k = maybe False (const True) $ decIterKey it k
{-# INLINE checkIterKey #-}

-- | Return 'Nothing' if the namespace doesn't match, and throws if the
-- key can't be decoded.
--
-- This function is useful because invalid iterators are represented as
-- iterators that point outside their respective namespace key range.
--
tryDecIterKey :: MonadThrow m => RocksDbTableIter k v -> B.ByteString -> m (Maybe k)
tryDecIterKey it k = case B.splitAt (B.length namespace) k of
    (a, b)
        | a /= namespace -> return Nothing
        | otherwise -> Just <$> _codecDecode (_rocksDbTableIterKeyCodec it) b
  where
    namespace = _rocksDbTableIterNamespace it
{-# INLINE tryDecIterKey #-}

decIterKey :: MonadThrow m => RocksDbTableIter k v -> B.ByteString -> m k
decIterKey it k = case B.splitAt (B.length namespace) k of
    (a, b)
        | a == namespace -> _codecDecode (_rocksDbTableIterKeyCodec it) b
        | otherwise -> throwM
            $ RocksDbTableIterInvalidKeyNamespace (Expected namespace) (Actual a)
  where
    namespace = _rocksDbTableIterNamespace it
{-# INLINE decIterKey #-}

