{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Storage.Table.RocksDB
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Persisted key-value and and content-addressable-key-value stores with a
-- RocksDB backend.
--
-- A 'RocksDbTable' provides a typed key-value store with an additional iterator
-- interace.
--
-- 'RocksDbCas' adds an 'IsCas' instance for a 'RocksDbTable' where the value
-- type is content-addressable.
--
-- TODO: Abstract the 'RocksDbTable' API into a typeclass so that one can
-- provide alternative implementations for it.
--
module Chainweb.Storage.Table.RocksDB
  ( RocksDb(..)
  -- , rocksDbHandle
  -- , rocksDbNamespace
  , openRocksDb
  , closeRocksDb
  , withRocksDb
  , withTempRocksDb
  , destroyRocksDb
  , resetOpenRocksDb
  , modernDefaultOptions

  -- * Rocks DB Table
  , Codec(..)
  , RocksDbTable
  , newTable
  , tableLookup
  , tableLookupBatch
  , tableInsert
  , tableDelete

  -- * Batch Updates
  , RocksDbUpdate(..)
  , updateBatch

  -- * Rocks DB Table Iterator
  , RocksDbTableIter

  -- ** Streams
  , iterToEntryStream
  , iterToValueStream
  , iterToReverseValueStream
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

  -- * RocksDB-specific tools
  , checkpointRocksDb
  , deleteRangeRocksDb
  , compactRangeRocksDb
  ) where

import Control.Exception(evaluate)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as BU
import Data.Coerce
import Data.Foldable
import Data.Maybe

import Chainweb.Storage.Table
import Data.String
import qualified Data.Text as T
import qualified Data.Vector as V

import Foreign
import Foreign.C

import qualified Database.RocksDB.Base as R
import qualified Database.RocksDB.ReadOptions as R
import qualified Database.RocksDB.C as C
import qualified Database.RocksDB.Internal as R
import qualified Database.RocksDB.Iterator as I

import GHC.Generics (Generic)
import qualified GHC.Foreign as GHC
import qualified GHC.IO.Encoding as GHC
import GHC.Stack

import NoThunks.Class

import qualified Streaming.Prelude as S

import System.Directory
import System.IO.Temp

-- -------------------------------------------------------------------------- --
-- Utils

-- | A newtype wrapper for tagger values as "expected" outcomes of some
-- computation.
--
newtype Expected a = Expected { getExpected :: a }
    deriving (Show, Eq, Ord, Generic, Functor)
    deriving newtype (NoThunks)

-- | A newtype wrapper for tagger values as "actual" outcomes of some
-- computation.
--
newtype Actual a = Actual { getActual :: a }
    deriving (Show, Eq, Ord, Generic, Functor)
    deriving newtype (NoThunks)

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

-- -------------------------------------------------------------------------- --
-- RocksDb

-- | This wrapper allows to create key namespaces. This is, for instance, useful
-- for testing when running several concurrent tests on the same rocks db
-- instances. Key namespaces must not contain the character '-'.
--
data RocksDb = RocksDb
    { _rocksDbHandle :: !R.DB
    , _rocksDbNamespace :: !B.ByteString
    }

instance NoThunks RocksDb where
    wNoThunks ctx (RocksDb a b) = allNoThunks
        [ noThunks ctx (InspectHeapNamed @"Data.RocksDB.Base.DB" a)
        , noThunks ctx b
        ]
    showTypeOf _ = "Chainweb.Storage.Table.RocksDB.RocksDb"


-- makeLenses ''RocksDb

modernDefaultOptions :: R.Options
modernDefaultOptions = R.defaultOptions
    { R.maxOpenFiles = -1
    , R.writeBufferSize = 64 `shift` 20
    , R.createIfMissing = True
    }

-- | Open a 'RocksDb' instance with the default namespace. If no rocks db exists
-- at the provided directory path, a new database is created.
--
openRocksDb :: FilePath -> C.OptionsPtr -> IO RocksDb
openRocksDb path opts_ptr = do
    GHC.setFileSystemEncoding GHC.utf8
    createDirectoryIfMissing True path
    db <- withFilePath path $ \path_ptr ->
        fmap R.DB
        $ R.throwIfErr "open"
        $ C.c_rocksdb_open opts_ptr path_ptr
    let rdb = RocksDb db mempty
    initializeRocksDb rdb
    return rdb

withOpts :: R.Options -> (R.Options' -> IO a) -> IO a
withOpts opts =
    bracket (R.mkOpts opts) R.freeOpts

-- | Each table key starts with @_rocksDbNamespace db <> "-"@. Here we insert a
-- dummy key that is guaranteed to be appear after any other key in the
-- database. We rely on its existence that in the implementation of
-- 'tableIteratorLast'.
--
initializeRocksDb :: RocksDb -> IO ()
initializeRocksDb db = R.put
    (_rocksDbHandle db)
    R.defaultWriteOptions
    (_rocksDbNamespace db <> ".")
    ""

-- | Open a 'RocksDb' and reset it if it already exists.
--
resetOpenRocksDb :: FilePath -> IO RocksDb
resetOpenRocksDb path = do
    destroyRocksDb path
    db <- RocksDb <$> R.open path opts <*> mempty
    initializeRocksDb db
    return db
  where
    opts = modernDefaultOptions { R.errorIfExists = True }

-- | Close a 'RocksDb' instance.
--
closeRocksDb :: RocksDb -> IO ()
closeRocksDb (RocksDb (R.DB db_ptr) _) = C.c_rocksdb_close db_ptr

-- | Provide a computation with a 'RocksDb' instance. If no rocks db exists at
-- the provided directory path, a new database is created.
--
withRocksDb :: FilePath -> R.Options -> (RocksDb -> IO a) -> IO a
withRocksDb path opts act =
    withOpts opts $ \(R.Options' opts_ptr _ _) ->
        bracket (openRocksDb path opts_ptr) closeRocksDb act

-- | Provide a computation with a temporary 'RocksDb'. The database is deleted
-- when the computation exits.
--
withTempRocksDb :: String -> (RocksDb -> IO a) -> IO a
withTempRocksDb template f = withSystemTempDirectory template $ \dir ->
    withRocksDb dir modernDefaultOptions f

-- | Delete the RocksDb instance.
--
-- This is the prefered method of deleting an rocks db instance. A rocks db
-- instance may store files in different locations. This function guarantees
-- that all files are deleted.
--
destroyRocksDb :: FilePath -> IO ()
destroyRocksDb path = R.destroy path opts
  where
    opts = modernDefaultOptions { R.createIfMissing = False }

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
    , _codecDecode :: !(forall m. MonadThrow m => B.ByteString -> m a)
        -- ^ decode a value. Throws an exception of decoding fails.
    }

instance NoThunks (Codec a) where
    -- NoThunks does not look inside of closures for captured thunks
    wNoThunks _ _ = return Nothing
    showTypeOf _ = "Chainweb.Storage.Table.RocksDB.Codec"

-- | A logical table in a 'RocksDb'. Tables in a rocks db are have isolated key
-- namespaces.
--
data RocksDbTable k v = RocksDbTable
    { _rocksDbTableValueCodec :: !(Codec v)
    , _rocksDbTableKeyCodec :: !(Codec k)
    , _rocksDbTableNamespace :: !B.ByteString
    , _rocksDbTableDb :: !R.DB
    }

instance NoThunks (RocksDbTable k v) where
    wNoThunks ctx (RocksDbTable a b c d) = allNoThunks
        [ noThunks ctx a
        , noThunks ctx b
        , noThunks ctx c
        , noThunks ctx (InspectHeapNamed @"Data.RocksDB.Base.DB" d)
        ]
    showTypeOf _ = "Chainweb.Storage.Table.RocksDB.RocksDbTable"

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
        = error $ "Chainweb.Storage.Table.RocksDb.newTable: invalid character in table namespace: " <> sshow namespace
    | otherwise
        = RocksDbTable valCodec keyCodec ns (_rocksDbHandle db)
  where
    ns = _rocksDbNamespace db <> "-" <> B.intercalate "/" namespace


-- -------------------------------------------------------------------------- --
-- Batches

-- | A single update operation for a 'RocksDbTable'
--
data RocksDbUpdate
    = forall k v . RocksDbDelete
        { _rocksDbUpdateTable :: !(RocksDbTable k v)
        , _rocksDbUpdateKey :: !k
        }
    | forall k v . RocksDbInsert
        { _rocksDbUpdateTable :: !(RocksDbTable k v)
        , _rocksDbUpdateKey :: !k
        , _rocksDbUpdateValue :: !v
        }

rocksDbUpdateDb :: RocksDbUpdate -> R.DB
rocksDbUpdateDb (RocksDbDelete t _) = _rocksDbTableDb t
rocksDbUpdateDb (RocksDbInsert t _ _) = _rocksDbTableDb t

-- | Atomically execute a batch of rocks db updates.
--
-- All tables in the batch operations belong to the same rocks db instance.
-- Otherwise an error is raised.
--
updateBatch :: HasCallStack => [RocksDbUpdate] -> IO ()
updateBatch [] = return ()
updateBatch batch = R.write rdb R.defaultWriteOptions $ checkMkOp <$> batch
  where
    rdb = rocksDbUpdateDb $ head batch

    checkMkOp o
        | rdb == rocksDbUpdateDb o = mkOp o
        | otherwise = error "Chainweb.Storage.Table.RocksDB.updateBatch: all operations in a batch must be for the same RocksDB instance."

    mkOp (RocksDbDelete t k) = R.Del (encKey t k)
    mkOp (RocksDbInsert t k v) = R.Put (encKey t k) (encVal t v)

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
-- `withTableIterator`.
--
data RocksDbTableIter k v = RocksDbTableIter
    { _rocksDbTableIterValueCodec :: !(Codec v)
    , _rocksDbTableIterKeyCodec :: !(Codec k)
    , _rocksDbTableIterNamespace :: !B.ByteString
    , _rocksDbTableIter :: !I.Iterator
    }

instance NoThunks (RocksDbTableIter k v) where
    wNoThunks ctx (RocksDbTableIter a b c d) = allNoThunks
        [ noThunks ctx a
        , noThunks ctx b
        , noThunks ctx c
        , noThunks ctx (InspectHeapNamed @"Data.RocksDB.Iterator.Iterator" d)
        ]
    showTypeOf _ = "Chainweb.Storage.Table.RocksDB.RocksDbTableIterator"

instance Iterator (RocksDbTableIter k v) k v where
  iterValid it =
      I.iterValid (_rocksDbTableIter it)

  iterSeek it = I.iterSeek (_rocksDbTableIter it) . encIterKey it

  iterFirst it =
      I.iterFirst (_rocksDbTableIter it)

  iterLast it =
      I.iterLast (_rocksDbTableIter it)

  iterNext = I.iterNext . _rocksDbTableIter

  iterPrev = I.iterPrev . _rocksDbTableIter

  iterEntry it =
      I.iterEntry (_rocksDbTableIter it) >>= \case
          Nothing -> return Nothing
          Just (k, v) -> do
              k' <- decIterKey it k
              v' <- decIterVal it v
              return $! Just $! Entry k' v'

  iterValue it = I.iterValue (_rocksDbTableIter it) >>= \case
      Nothing -> return Nothing
      Just v -> Just <$> (evaluate =<< decIterVal it v)

  iterKey it = I.iterKey (_rocksDbTableIter it) >>= \case
      Nothing -> return Nothing
      Just k -> Just <$> (evaluate =<< decIterKey it k)

instance IterableTable (RocksDbTable k v) (RocksDbTableIter k v) k v where
    withTableIterator db k = R.withReadOptions readOptions $ \opts_ptr ->
        I.withIter (_rocksDbTableDb db) opts_ptr $ \iter -> do
            let tableIter = makeTableIter iter
            iterFirst tableIter
            k tableIter
      where
        readOptions = fold
            [ R.setLowerBound (namespaceFirst $ _rocksDbTableNamespace db)
            , R.setUpperBound (namespaceLast $ _rocksDbTableNamespace db)
            -- TODO: this setting tells rocksdb to use prefix seek *when it can*.
            -- the question remains: is it actually being used?
            , R.setAutoPrefixMode True
            ]
        makeTableIter =
            RocksDbTableIter
                (_rocksDbTableValueCodec db)
                (_rocksDbTableKeyCodec db)
                (_rocksDbTableNamespace db)

-- | Returns the stream of key-value pairs of an 'RocksDbTableIter'.
--
-- The iterator must be released after the stream is consumed. Releasing the
-- iterator to early while the stream is still in use results in a runtime
-- error. Not releasing the iterator after the processing of the stream has
-- finished results in a memory leak.
--
iterToEntryStream :: RocksDbTableIter k v -> S.Stream (S.Of (Entry k v)) IO ()
iterToEntryStream it =
    liftIO (iterEntry it) >>= \case
        Nothing -> return ()
        Just x -> S.yield x >> liftIO (iterNext it) >> iterToEntryStream it

-- | Returns the stream of values of an 'RocksDbTableIter'.
--
-- The iterator must be released after the stream is consumed. Releasing the
-- iterator to early while the stream is still in use results in a runtime
-- error. Not releasing the iterator after the processing of the stream has
-- finished results in a memory leak.
--
iterToValueStream :: Show k => RocksDbTableIter k v -> S.Stream (S.Of v) IO ()
iterToValueStream it = do
    liftIO (iterValue it) >>= \case
        Nothing -> return ()
        Just x -> S.yield x >> liftIO (iterNext it) >> iterToValueStream it

-- Returns the stream of key-value pairs of an 'RocksDbTableIter' in reverse
-- order.
--
-- The iterator must be released after the stream is consumed. Releasing the
-- iterator to early while the stream is still in use results in a runtime
-- error. Not releasing the iterator after the processing of the stream has
-- finished results in a memory leak.
--
iterToReverseValueStream :: RocksDbTableIter k v -> S.Stream (S.Of v) IO ()
iterToReverseValueStream it =
    liftIO (iterValue it) >>= \case
        Nothing -> return ()
        Just x -> S.yield x >> liftIO (iterPrev it) >> iterToReverseValueStream it

-- | Returns the stream of keys of an 'RocksDbTableIter'.
--
-- The iterator must be released after the stream is consumed. Releasing the
-- iterator to early while the stream is still in use results in a runtime
-- error. Not releasing the iterator after the processing of the stream has
-- finished results in a memory leak.
--
iterToKeyStream :: RocksDbTableIter k v -> S.Stream (S.Of k) IO ()
iterToKeyStream it =
    liftIO (iterKey it) >>= \case
        Nothing -> return ()
        Just x -> S.yield x >> liftIO (iterNext it) >> iterToKeyStream it

-- Extremal Table Entries

-- | The maximum key in a 'RocksDbTable'.
--
tableMaxKey :: RocksDbTable k v -> IO (Maybe k)
tableMaxKey = flip withTableIterator $ \i -> iterLast i *> iterKey i

-- | The maximum value in a 'RocksDbTable'.
--
tableMaxValue :: RocksDbTable k v -> IO (Maybe v)
tableMaxValue = flip withTableIterator $ \i -> iterLast i *> iterValue i

-- | The maximum key-value in a 'RocksDbTable'.
--
tableMaxEntry :: RocksDbTable k v -> IO (Maybe (Entry k v))
tableMaxEntry = flip withTableIterator $ \i -> iterLast i *> iterEntry i

-- | The minimum key in a 'RocksDbTable'.
--
tableMinKey :: RocksDbTable k v -> IO (Maybe k)
tableMinKey = flip withTableIterator $ \i -> iterFirst i *> iterKey i

-- | The minimum value in a 'RocksDbTable'.
--
tableMinValue :: RocksDbTable k v -> IO (Maybe v)
tableMinValue = flip withTableIterator $ \i -> iterFirst i *> iterValue i

-- | The minimum key-value in a 'RocksDbTable'.
--
tableMinEntry :: RocksDbTable k v -> IO (Maybe (Entry k v))
tableMinEntry = flip withTableIterator $ \i -> iterFirst i *> iterEntry i

-- -------------------------------------------------------------------------- --
-- CAS

-- | For a 'IsCasValue' @v@ with 'CasKeyType v ~ k@,  a 'RocksDbTable k v' is an
-- instance of 'HasCasLookup'.
--
instance ReadableTable (RocksDbTable k v) k v where
    tableLookup k db = do
        maybeBytes <- get (_rocksDbTableDb db) mempty (encKey db k)
        traverse (decVal db) maybeBytes

    tableMember k db = 
        isJust <$> get (_rocksDbTableDb db) mempty (encKey db k)

    -- | @tableLookupBatch db ks@ returns for each @k@ in @ks@ 'Just' the value at
    -- key @k@ in the 'RocksDbTable' @db@ if it exists, or 'Nothing' if the @k@
    -- doesn't exist in the table.
    --
    tableLookupBatch ks db = do
        results <- V.toList <$> multiGet (_rocksDbTableDb db) mempty (V.fromList $ map (encKey db) ks)
        forM results $ \case
            Left e -> error $ "Chainweb.Storage.Table.RocksDB.tableLookupBatch: " <> e
            Right x -> traverse (decVal db) x

-- | For a 'IsCasValue' @v@ with 'CasKeyType v ~ k@,  a 'RocksDbTable k v' is an
-- instance of 'IsCas'.
--
instance Table (RocksDbTable k v) k v where
    tableInsert k v db = R.put
        (_rocksDbTableDb db)
        R.defaultWriteOptions
        (encKey db k)
        (encVal db v)

    tableDelete k db = R.delete
        (_rocksDbTableDb db)
        R.defaultWriteOptions
        (encKey db k)

    tableInsertBatch kvs db = 
        updateBatch (uncurry (RocksDbInsert db) <$> kvs)

    tableDeleteBatch ks db = 
        updateBatch (RocksDbDelete db <$> ks)

-- | A newtype wrapper that takes only a single type constructor. This useful in
-- situations where a Higher Order type constructor for a CAS is required. A
-- type synonym doesn't work in this situation because type synonyms must be
-- fully applied.
--
newtype RocksDbCas v = RocksDbCas { _getRocksDbCas :: RocksDbTable (CasKeyType v) v }
    deriving newtype (NoThunks)

instance (k ~ CasKeyType v, IsCasValue v) => ReadableTable (RocksDbCas v) k v where
    tableLookup k (RocksDbCas x) = tableLookup k x
    tableLookupBatch ks (RocksDbCas x) = tableLookupBatch ks x
    tableMember k (RocksDbCas x) = tableMember k x

instance (k ~ CasKeyType v, IsCasValue v) => Table (RocksDbCas v) k v where
    tableInsert k v (RocksDbCas x) = tableInsert k v x
    tableDelete k (RocksDbCas x) = tableDelete k x
    tableInsertBatch kvs (RocksDbCas x) = tableInsertBatch kvs x
    tableDeleteBatch ks (RocksDbCas x) = tableDeleteBatch ks x

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

-- -------------------------------------------------------------------------- --
-- Exceptions

-- | Excpeptions that can be thrown by functions in this module.
--
data RocksDbException
    = RocksDbTableIterInvalidKeyNamespace (Expected B.ByteString) (Actual B.ByteString)
    deriving (Eq, Ord, Generic, NoThunks)

instance Exception RocksDbException where
    displayException (RocksDbTableIterInvalidKeyNamespace e a)
        = T.unpack $ unexpectedMsg "Chainweb.Storage.Table.RocksDB: invalid table key" e a

instance Show RocksDbException where
    show = displayException

-- -------------------------------------------------------------------------- --
-- Table Utils

encVal :: RocksDbTable k v -> v -> B.ByteString
encVal = _codecEncode . _rocksDbTableValueCodec

encKey :: RocksDbTable k v -> k -> B.ByteString
encKey it k = namespaceFirst ns <> _codecEncode (_rocksDbTableKeyCodec it) k
  where
    ns = _rocksDbTableNamespace it

decVal :: MonadThrow m => RocksDbTable k v -> B.ByteString -> m v
decVal tbl = _codecDecode $ _rocksDbTableValueCodec tbl

-- -------------------------------------------------------------------------- --
-- Iter Utils

namespaceFirst :: B.ByteString -> B.ByteString
namespaceFirst ns = ns <> "$"

namespaceLast :: B.ByteString -> B.ByteString
namespaceLast ns = ns <> "%"

encIterKey :: RocksDbTableIter k v -> k -> B.ByteString
encIterKey it k = namespaceFirst ns <> _codecEncode (_rocksDbTableIterKeyCodec it) k
  where
    ns = _rocksDbTableIterNamespace it

decIterVal :: MonadThrow m => RocksDbTableIter k v -> B.ByteString -> m v
decIterVal i = _codecDecode (_rocksDbTableIterValueCodec i) 

decIterKey :: MonadThrow m => RocksDbTableIter k v -> B.ByteString -> m k
decIterKey it k =
    _codecDecode (_rocksDbTableIterKeyCodec it) (B.drop (B.length prefix) k)
  where
    prefix = namespaceFirst $ _rocksDbTableIterNamespace it

checked :: HasCallStack => String -> (Ptr CString -> IO a) -> IO a
checked whatWasIDoing act = alloca $ \errPtr -> do
    poke errPtr (nullPtr :: CString)
    r <- act errPtr
    err <- peek errPtr
    unless (err == nullPtr) $ do
        errStr <- B.packCString err
        let msg = unwords ["Chainweb.Storage.Table.RocksDB.checked: error while", whatWasIDoing <> ":", B8.unpack errStr]
        C.c_rocksdb_free err
        error msg
    return r

-- to unconditionally flush the WAL log before making the checkpoint, set logSizeFlushThreshold to zero.
-- to *never* flush the WAL log, set logSizeFlushThreshold to maxBound :: CULong.
checkpointRocksDb :: RocksDb -> CULong -> FilePath -> IO ()
checkpointRocksDb RocksDb { _rocksDbHandle = R.DB dbPtr } logSizeFlushThreshold path =
    bracket mkCheckpointObject C.rocksdb_checkpoint_object_destroy mkCheckpoint
  where
    mkCheckpointObject =
        checked "creating checkpoint object" $
            C.rocksdb_checkpoint_object_create dbPtr
    mkCheckpoint cp =
        withCString path $ \path' ->
            checked "creating checkpoint" $
                C.rocksdb_checkpoint_create cp path' logSizeFlushThreshold

validateRangeOrdered :: HasCallStack => RocksDbTable k v -> (Maybe k, Maybe k) -> (B.ByteString, B.ByteString)
validateRangeOrdered table (Just (encKey table -> l), Just (encKey table -> u))
    | l >= u =
        error "Chainweb.Storage.Table.RocksDB.validateRangeOrdered: range bounds not ordered according to codec"
    | otherwise = (l, u)
validateRangeOrdered table (l, u) =
    ( maybe (namespaceFirst (_rocksDbTableNamespace table)) (encKey table) l
    , maybe (namespaceLast (_rocksDbTableNamespace table)) (encKey table) u
    )

-- | Batch delete a range of keys in a table.
-- Throws if the range of the *encoded keys* is not ordered (lower, upper).
deleteRangeRocksDb :: HasCallStack => RocksDbTable k v -> (Maybe k, Maybe k) -> IO ()
deleteRangeRocksDb table range = do
    let !range' = validateRangeOrdered table range
    let R.DB dbPtr = _rocksDbTableDb table
    R.withCWriteOpts R.defaultWriteOptions $ \optsPtr ->
        BU.unsafeUseAsCStringLen (fst range') $ \(minKeyPtr, minKeyLen) ->
        BU.unsafeUseAsCStringLen (snd range') $ \(maxKeyPtr, maxKeyLen) ->
        checked "Chainweb.Storage.Table.RocksDB.deleteRangeRocksDb" $
            C.rocksdb_delete_range dbPtr optsPtr
                minKeyPtr (fromIntegral minKeyLen :: CSize)
                maxKeyPtr (fromIntegral maxKeyLen :: CSize)

compactRangeRocksDb :: HasCallStack => RocksDbTable k v -> (Maybe k, Maybe k) -> IO ()
compactRangeRocksDb table range =
    BU.unsafeUseAsCStringLen (fst range') $ \(minKeyPtr, minKeyLen) ->
        BU.unsafeUseAsCStringLen (snd range') $ \(maxKeyPtr, maxKeyLen) ->
        C.rocksdb_compact_range dbPtr
            minKeyPtr (fromIntegral minKeyLen :: CSize)
            maxKeyPtr (fromIntegral maxKeyLen :: CSize)
  where
    !range' = validateRangeOrdered table range
    R.DB dbPtr = _rocksDbTableDb table

-- | Read a value by key.
-- One less copy than the version in rocksdb-haskell by using unsafePackCStringFinalizer.
get :: MonadIO m => R.DB -> R.ReadOptions -> ByteString -> m (Maybe ByteString)
get (R.DB db_ptr) opts key = liftIO $ R.withReadOptions opts $ \opts_ptr ->
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    alloca $ \vlen_ptr -> do
        val_ptr <- checked "Chainweb.Storage.Table.RocksDB.get" $
            C.c_rocksdb_get db_ptr opts_ptr key_ptr (R.intToCSize klen) vlen_ptr
        vlen <- peek vlen_ptr
        if val_ptr == nullPtr
            then return Nothing
            else do
                Just <$> BU.unsafePackCStringFinalizer
                    ((coerce :: Ptr CChar -> Ptr Word8) val_ptr)
                    (R.cSizeToInt vlen)
                    (C.c_rocksdb_free val_ptr)

-- | Marshal a 'FilePath' (Haskell string) into a `NUL` terminated C string using
-- temporary storage.
-- On Linux, UTF-8 is almost always the encoding used.
-- When on Windows, UTF-8 can also be used, although the default for those devices is
-- UTF-16. For a more detailed explanation, please refer to
-- https://msdn.microsoft.com/en-us/library/windows/desktop/dd374081(v=vs.85).aspx.
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = GHC.withCString GHC.utf8

-- -------------------------------------------------------------------------- --
-- Multi Get

multiGet
    :: MonadIO m
    => R.DB
    -> R.ReadOptions
    -> V.Vector ByteString
    -> m (V.Vector (Either String (Maybe ByteString)))
multiGet (R.DB db_ptr) opts keys = liftIO $ R.withReadOptions opts $ \opts_ptr ->
    allocaArray len $ \keysArray ->
    allocaArray len $ \keySizesArray ->
    allocaArray len $ \valuesArray ->
    allocaArray len $ \valueSizesArray ->
    allocaArray len $ \errsArray ->
        let go i (key : ks) =
                BU.unsafeUseAsCStringLen key $ \(keyPtr, keyLen) -> do
                    pokeElemOff keysArray i keyPtr
                    pokeElemOff keySizesArray i (R.intToCSize keyLen)
                    go (i + 1) ks

            go _ [] = do
                C.rocksdb_multi_get db_ptr opts_ptr (R.intToCSize len)
                    keysArray keySizesArray
                    valuesArray valueSizesArray
                    errsArray
                V.generateM len $ \i -> do
                  valuePtr <- peekElemOff valuesArray i
                  if valuePtr /= nullPtr
                    then do
                      valueLen <- R.cSizeToInt <$> peekElemOff valueSizesArray i
                      r <- BU.unsafePackMallocCStringLen (valuePtr, valueLen)
                      return $ Right $ Just r
                    else do
                      errPtr <- peekElemOff errsArray i
                      if errPtr /= nullPtr
                        then do
                          err <- B8.unpack <$> BU.unsafePackMallocCString errPtr
                          return $ Left err
                        else
                          return $ Right Nothing
        in go 0 $ V.toList keys
  where
    len = V.length keys
