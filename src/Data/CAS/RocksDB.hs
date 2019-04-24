{-# LANGUAGE DeriveAnyClass #-}
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
module Data.CAS.RocksDB
( RocksDb(..)
, rocksDbHandle
, rocksDbNamespace
, openRocksDb
, closeRocksDb
, withRocksDb
, withTempRocksDb

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
import qualified Data.Text as T

import qualified Database.RocksDB.Base as R
import qualified Database.RocksDB.Iterator as I

import GHC.Generics (Generic)
import GHC.Stack

import qualified Streaming.Prelude as S

import System.IO.Temp

-- internal modules
import Chainweb.Utils hiding (Codec)

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

openRocksDb :: FilePath -> IO RocksDb
openRocksDb path = RocksDb <$> R.open path opts <*> mempty
  where
    opts = R.defaultOptions { R.createIfMissing = True }

closeRocksDb :: RocksDb -> IO ()
closeRocksDb = R.close . _rocksDbHandle

withRocksDb :: FilePath -> (RocksDb -> IO a) -> IO a
withRocksDb path = bracket (openRocksDb path) closeRocksDb

withTempRocksDb :: String -> (RocksDb -> IO a) -> IO a
withTempRocksDb template f = withSystemTempDirectory template $ \dir ->
    withRocksDb dir f

-- -------------------------------------------------------------------------- --
-- RocksDb Table

-- NOTE: the implementation currently doesnt' support iteration over nested
-- namespaces. This could easily be added via an iterator that iterates over
-- all respective prefixes ending with '/' or '$' and using a codec that
-- could handle the types in all sub-namespaces. This could for instance
-- be useful for iterating over the complete database.

data Codec a = Codec
    { _codecEncode :: !(a -> B.ByteString)
    , _codecDecode :: !(forall m . MonadThrow m => B.ByteString -> m a)
    }

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

tableInsert :: RocksDbTable k v -> k -> v -> IO ()
tableInsert db k v = R.put
    (_rocksDbTableDb db)
    R.defaultWriteOptions
    (encKey db k)
    (encVal db v)
{-# INLINE tableInsert #-}

tableLookup :: RocksDbTable k v -> k -> IO (Maybe v)
tableLookup db k = do
    maybeBytes <- R.get (_rocksDbTableDb db) R.defaultReadOptions (encKey db k)
    traverse (decVal db) maybeBytes
{-# INLINE tableLookup #-}

tableDelete :: RocksDbTable k v -> k -> IO ()
tableDelete db k = R.delete
    (_rocksDbTableDb db)
    R.defaultWriteOptions
    (encKey db k)
{-# INLINE tableDelete #-}

-- -------------------------------------------------------------------------- --
-- Table Iterator

data RocksDbTableIter k v = RocksDbTableIter
    { _rocksDbTableIterValueCodec :: !(Codec v)
    , _rocksDbTableIterKeyCodec :: !(Codec k)
    , _rocksDbTableIterNamespace :: !B.ByteString
    , _rocksDbTableIter :: !I.Iterator
    }

createTableIter :: RocksDbTable k v -> IO (RocksDbTableIter k v)
createTableIter db = do
    tit <- RocksDbTableIter
        (_rocksDbTableValueCodec db)
        (_rocksDbTableKeyCodec db)
        (_rocksDbTableName db)
        <$> I.createIter (_rocksDbTableDb db) R.defaultReadOptions
    tableIterFirst tit
    return tit
{-# INLINE createTableIter #-}

releaseTableIter :: RocksDbTableIter k v -> IO ()
releaseTableIter = I.releaseIter . _rocksDbTableIter
{-# INLINE releaseTableIter #-}

withTableIter :: RocksDbTable k v -> (RocksDbTableIter k v -> IO a) -> IO a
withTableIter db = bracket (createTableIter db) releaseTableIter
{-# INLINE withTableIter #-}

tableIterValid :: MonadIO m => RocksDbTableIter k v -> m Bool
tableIterValid it = I.iterKey (_rocksDbTableIter it) >>= \case
    Nothing -> return False
    Just x -> return (checkIterKey it x)
{-# INLINE tableIterValid #-}

tableIterSeek :: MonadIO m => RocksDbTableIter k v -> k -> m ()
tableIterSeek it = I.iterSeek (_rocksDbTableIter it) . encIterKey it
{-# INLINE tableIterSeek #-}

tableIterFirst :: MonadIO m => RocksDbTableIter k v -> m ()
tableIterFirst it
    = I.iterSeek (_rocksDbTableIter it) (_rocksDbTableIterNamespace it)
{-# INLINE tableIterFirst #-}

tableIterLast :: MonadIO m => RocksDbTableIter k v -> m ()
tableIterLast it = do
    I.iterSeek (_rocksDbTableIter it) (namespaceLast it)
    I.iterPrev (_rocksDbTableIter it)
{-# INLINE tableIterLast #-}

tableIterNext :: MonadIO m => RocksDbTableIter k v -> m ()
tableIterNext = I.iterNext . _rocksDbTableIter
{-# INLINE tableIterNext #-}

tableIterPrev :: MonadIO m => RocksDbTableIter k v -> m ()
tableIterPrev = I.iterPrev . _rocksDbTableIter
{-# INLINE tableIterPrev #-}

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
            Just k' -> do
                v' <- decIterVal it v
                return $ Just (k', v')
{-# INLINE tableIterEntry #-}

tableIterValue
    :: MonadIO m
    => MonadThrow m
    => RocksDbTableIter k v
    -> m (Maybe v)
tableIterValue it = fmap snd <$> tableIterEntry it
{-# INLINE tableIterValue #-}

tableIterKey
    :: MonadIO m
    => MonadThrow m
    => RocksDbTableIter k v
    -> m (Maybe k)
tableIterKey it = I.iterKey (_rocksDbTableIter it) >>= \case
    Nothing -> return Nothing
    Just k -> tryDecIterKey it k
{-# INLINE tableIterKey #-}

iterToEntryStream :: MonadIO m => RocksDbTableIter k v -> S.Stream (S.Of (k,v)) m ()
iterToEntryStream it = liftIO (tableIterEntry it) >>= \case
    Nothing -> return ()
    Just x -> S.yield x >> tableIterNext it >> iterToEntryStream it
{-# INLINE iterToEntryStream #-}

iterToValueStream :: MonadIO m => RocksDbTableIter k v -> S.Stream (S.Of v) m ()
iterToValueStream it = liftIO (tableIterValue it) >>= \case
    Nothing -> return ()
    Just x -> S.yield x >> tableIterNext it >> iterToValueStream it
{-# INLINE iterToValueStream #-}

iterToKeyStream :: MonadIO m => RocksDbTableIter k v -> S.Stream (S.Of k) m ()
iterToKeyStream it = liftIO (tableIterKey it) >>= \case
    Nothing -> return ()
    Just x -> S.yield x >> tableIterNext it >> iterToKeyStream it
{-# INLINE iterToKeyStream #-}

-- -------------------------------------------------------------------------- --
-- CAS

instance (IsCasValue v, CasKeyType v ~ k) => IsCas (RocksDbTable k v) where
    type CasValueType (RocksDbTable k v) = v

    casLookup = tableLookup
    casInsert db a = tableInsert db (casKey a) a
    casDelete = tableDelete

    {-# INLINE casLookup #-}
    {-# INLINE casInsert #-}
    {-# INLINE casDelete #-}

-- | A newtype wrapper that takes only a single type constructor. This
-- useful in situations where a Higher Order type constructor for a CAS
-- is required. A type synonym isn't doesn't work in this situation because
-- type synonyms must be fully applied.
--
newtype RocksDbCas v = RocksDbCas (RocksDbTable (CasKeyType v) v)

instance IsCasValue v => IsCas (RocksDbCas v) where
    type instance CasValueType (RocksDbCas v) = v

    casLookup (RocksDbCas x) = casLookup x
    casInsert (RocksDbCas x) = casInsert x
    casDelete (RocksDbCas x) = casDelete x

    {-# INLINE casLookup #-}
    {-# INLINE casInsert #-}
    {-# INLINE casDelete #-}

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

