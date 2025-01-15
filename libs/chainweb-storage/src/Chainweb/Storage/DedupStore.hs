{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Chainweb.Storage.DedupStore
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A Deduplicated Key-Value Store
--
module Chainweb.Storage.DedupStore
(
-- * Deduplicated Key-Value Store
  DedupStore(..)
, newDedupStore
, dedupInsert
, dedupLookup
, DedupStoreException(..)

-- * Low level Deduplicated Chunk Store
, dedupStore
, dedupStore'
, dedupRestore
) where

import Control.DeepSeq
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Catch (throwM)

import qualified Crypto.Hash as C

import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import Data.Hashable
import Data.Int
import qualified Data.List as L
import Data.String
import qualified Data.Text as T
import Data.Word

import Foreign.Ptr
import Foreign.Storable

import GHC.Generics
import GHC.Stack

import System.IO.Unsafe

-- internal modules

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

-- -------------------------------------------------------------------------- --
-- Utils

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
{-# INLINE fst3 #-}

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a
{-# INLINE snd3 #-}

third3 :: (a, b, c) -> c
third3 (_, _, a) = a
{-# INLINE third3 #-}

-- -------------------------------------------------------------------------- --
-- Dedup Hash

dedupHashAlg :: C.Blake2b_256
dedupHashAlg = C.Blake2b_256

type DedupHashAlg = C.Blake2b_256

dedupHashSize :: Int
dedupHashSize = C.hashDigestSize dedupHashAlg

newtype DedupHash = DedupHash { _dedupHashBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (NFData, Hashable, BA.ByteArrayAccess)

-- -------------------------------------------------------------------------- --
-- DedupStore

-- | Exception thrown by the DedupStore
--
newtype DedupStoreException = DedupStoreCorruptedData T.Text
    deriving (Eq, Show, Ord, Generic)
    deriving newtype (NFData, Hashable, IsString)

instance Exception DedupStoreException

-- | A deduplicating key value store
--
data DedupStore k v = DedupStore
    { _dedupRoots :: !(RocksDbTable k DedupHash)
    , _dedupChunks :: !(Casify RocksDbTable Chunk)
    , _dedupValueCodec :: !(Codec v)
    }

-- | Create a new 'DedupStore'
--
newDedupStore
    :: HasCallStack
    => RocksDb
    -> Codec v
    -> Codec k
    -> [B.ByteString]
    -> DedupStore k v
newDedupStore rdb vc kc n = DedupStore
    { _dedupRoots = newTable rdb dedupHashCodec kc (n <> ["roots"])
    , _dedupChunks = Casify $ newTable rdb dedupChunkCodec dedupHashCodec (n <> ["chunks"])
    , _dedupValueCodec = vc
    }
  where
    dedupHashCodec = Codec
        { _codecEncode = _dedupHashBytes
        , _codecDecode = \l -> do
            unless (B.length l == dedupHashSize)
                $ throwM $ DedupStoreCorruptedData "Internal key has wrong size"
            return (DedupHash l)
        }

    dedupChunkCodec = Codec
        { _codecEncode = \(Chunk t h b) -> B.singleton t <> _dedupHashBytes h <> b
        , _codecDecode = \bs -> case B.uncons bs of
            Nothing -> throwM $ DedupStoreCorruptedData "Got empty chunk"
            Just (a, b) -> case B.splitAt dedupHashSize b of
                (c, d)
                    | B.length c < dedupHashSize
                        -> throwM $ DedupStoreCorruptedData "Got too short chunk"
                    | otherwise -> return $ Chunk a (DedupHash c) d
        }

-- | Insert a new items into a 'DedupStore'.
--
dedupInsert :: DedupStore k v -> k -> v -> IO ()
dedupInsert store k v = do
    dedupKey <- dedupStore (_dedupChunks store)
        $ BL.fromStrict $ _codecEncode (_dedupValueCodec store) v
    tableInsert (_dedupRoots store) k dedupKey
{-# INLINE dedupInsert #-}

-- | @dedupLookup db k@ returns 'Just' the value at key @k@ in the
-- 'DedupStore' @db@ if it exists, or 'Nothing' if the @k@ doesn't exist in
-- the table.
--
dedupLookup :: DedupStore k v -> k -> IO (Maybe v)
dedupLookup store k = do
    tableLookup (_dedupRoots store) k >>= \case
        Nothing -> return Nothing
        Just dedupKey -> do
            dedupRestore (_dedupChunks store) dedupKey >>= \case
                Nothing -> return Nothing
                Just bytes -> Just <$>
                    _codecDecode (_dedupValueCodec store) (BL.toStrict bytes)
{-# INLINE dedupLookup #-}

-- -------------------------------------------------------------------------- --
-- Low-Level Deduplication CAS

-- | A chunk of data
--
data Chunk = Chunk Word8 DedupHash B.ByteString
    deriving (Show, Eq, Ord, Generic)

instance IsCasValue Chunk where
    type CasKeyType Chunk = DedupHash
    casKey (Chunk _ h _) = h

type DedupCas cas = Cas cas Chunk

-- | Store a sequence of bytes in a deduplicated content addressed store.
-- Returns the hash of the bytes, that can be used to query the data from the
-- store.
--
dedupStore
    :: HasCallStack
    => DedupCas cas
    => cas
    -> BL.ByteString
    -> IO DedupHash
dedupStore store bytes = third3 <$> dedupStore' store bytes
{-# INLINE dedupStore #-}

-- | Store a sequence of bytes in a deduplicated content addressed store.
-- Returns the hash of the bytes, that can be used to query the data from the
-- store.
--
-- It also returns the number of chunk hits and chunk misses that are a measure
-- for the deduplication rate.
--
dedupStore'
    :: HasCallStack
    => DedupCas cas
    => cas
    -> BL.ByteString
    -> IO (Int, Int, DedupHash)
dedupStore' store = go0
  where
    go0 :: HasCallStack => BL.ByteString -> IO (Int, Int, DedupHash)
    go0 bytes = mapM (hashAndStore 0x0) (toChunks $ roll bytes) >>= \case
        [] -> error "Chainweb.Storage.DedupStore.dedupStore.go0: Data.ByteString.Lazy.toChunks must not return an empty list"
        [x] -> return x
        l -> do
            (h, m, r) <- go1 (third3 <$> l)
            return (sum (fst3 <$> l) + h, sum (snd3 <$> l) + m, r)

    go1 :: HasCallStack => [DedupHash] -> IO (Int, Int, DedupHash)
    go1 hashes = mapM (hashAndStore 0x1) (rollHashes hashes) >>= \case
        [] -> error "Chainweb.Storage.DedupStore.dedupStore.go1: Data.ByteString.Lazy.toChunks must not return an empty list"
        [x] -> return x
        l -> do
            (h, m, r) <- go1 (third3 <$> l)
            return (sum (fst3 <$> l) + h, sum (snd3 <$> l) + m, r)

    toChunks x = case BL.toChunks x of
        [] -> [""]
        l -> l
    {-# INLINE toChunks #-}

    -- This is called as the dedup hashes are produced, in a bottom up way. We
    -- could optimize for data base queries (at the cost of more memory
    -- overhead) by first computing the keys, checking for keys top-down, and
    -- inserting the data in a second pass only for those keys that aren't in
    -- the database already.
    --
    hashAndStore :: Word8 -> B.ByteString -> IO (Int, Int, DedupHash)
    hashAndStore tag c = do
        let h = DedupHash $ BA.convert $ C.hash @_ @DedupHashAlg (B.cons tag c)
        (hit, miss) <- tableMember store h >>= \x -> if x
            then return (1, 0)
            else do
                casInsert store (Chunk tag h c)
                return (0, 1)
        return (hit, miss, h)
    {-# INLINE hashAndStore #-}
    {-# SCC hashAndStore #-}

    rollHashes :: [DedupHash] -> [B.ByteString]
    rollHashes hs = fmap (B.concat . fmap _dedupHashBytes) $ rollLazy pickInt64 8 2 11 hs
    {-# INLINE rollHashes #-}
    {-# SCC rollHashes #-}

    pickInt64 :: DedupHash -> Int64
    pickInt64 s = unsafePerformIO $ BU.unsafeUseAsCString
        (_dedupHashBytes s)
        (peek . castPtr)
    {-# INLINE pickInt64 #-}
    {-# SCC pickInt64 #-}
{-# SCC dedupStore' #-}

-- | Retrieve a sequence of bytes from a deduplicated content addressable store.
--
dedupRestore
    :: DedupCas cas
    => cas
    -> DedupHash
    -> IO (Maybe BL.ByteString)
dedupRestore store key = fmap BB.toLazyByteString <$> do
    tableLookup store key >>= \case
        Nothing -> return Nothing
        Just (Chunk 0x0 _ b) -> return $ Just $ BB.byteString b
        Just (Chunk 0x1 _ b) -> Just <$> splitHashes b
        _ -> throwM $ DedupStoreCorruptedData  "Unknown chunk tag"

  where
    go h = do
        tableLookup store h >>= \case
            Nothing -> throwM $ DedupStoreCorruptedData "Missing chunk from store"
            Just (Chunk 0x0 _ b) -> return (BB.byteString b)
            Just (Chunk 0x1 _ b) -> splitHashes b
            _ -> throwM $ DedupStoreCorruptedData "Unknown chunk tag"

    splitHashes b
        | B.null b = return mempty
        | otherwise = case B.splitAt (C.hashDigestSize C.Blake2b_256) b of
            (h, r) -> (<>) <$> go (DedupHash h) <*> splitHashes r
{-# SCC dedupRestore #-}

-- -------------------------------------------------------------------------- --
-- Rabin-Karp Rolling Hash
--

-- | Re-chunks a lazy 'BL.ByteString' using the Rabin-Karp rolling hash
-- function. The expected chunk size of the resuling 'BL.ByteString' is 8k, with
-- a minimal chunk size of 128 bytes and a maximal chunk size of 64k.
--
-- The implementation is adapted from https://github.com/ekmett/hash, Copyright
-- 2012-2013 Edward Kmett.
--
roll :: BL.ByteString -> BL.ByteString
roll z = BL.fromChunks $ go seed 0 z za ze
  where
    z' = BL.unpack z
    za = BL.unpack (BL.replicate window 0) <> z'
    {-# SCC za #-}
    ze = z'
    {-# SCC ze #-}

    go
        :: Int64
            -- ^ current hash
        -> Int64
            -- ^ current byte count in input string
        -> BL.ByteString
            -- ^ input bytestring
        -> [Word8]
            -- ^ bytes from start of window (starts all 0 before input string)
        -> [Word8]
            -- ^ bytes after end window
        -> [B.ByteString]
            -- ^ Chunk
    go !h !c !bs (x:xs) (y:ys)
        | ((h' .&. mask == mask) && c >= minSize) || c >= maxSize = case BL.splitAt (c + 1) bs of
            (l,r) -> {-# SCC concat #-} B.concat (BL.toChunks l) : go seed 0 r xs ys
        | otherwise = go h' (c + 1) bs xs ys
      where
        !x' = if c < window then 0 else fromIntegral x
        !h' = rem (rem (h - magic_d * x') magic_q * magic_r + fromIntegral y) magic_q
        {-# SCC h' #-}
    go _ _ bs _ _ = [B.concat $ BL.toChunks bs]
    {-# SCC go #-}

    magic_d = powqp magic_r (window-1) magic_q
    mask    = 8191
    minSize = 128
    maxSize = 65536
    magic_r = 256
    magic_q = 32749
    window  = 128
    seed = 0
{-# INLINE roll #-}
{-# SCC roll #-}

-- | Breaks an input list into chunks using Rabin-Karp rolling hash with the
-- provided expected, minimal, and maximal chunks size.
--
-- The implementation is adapted from https://github.com/ekmett/hash, Copyright
-- 2012-2013 Edward Kmett.
--
rollLazy
    :: forall a
    . (a -> Int64)
        -- ^ hash function (64 bit should be sufficient for must use cases)
    -> Int
        -- ^ expected size (as power of 2)
    -> Int
        -- ^ min size (as power of 2). Must be equal or smaller than expected.
    -> Int
        -- ^ max size (as power of 2). Must be equal or larger than expected.
    -> [a]
    -> [[a]]
rollLazy f expectedPow2 minSizePow2 maxSizePow2 z = case z of
    [] -> []
    (h : _) -> go seed 0 z (L.replicate window h <> z) z
  where
    go
        :: Int64
            -- current hash
        -> Int
            -- current item count in input string
        -> [a]
            -- input items.
        -> [a]
            -- items from start of window (starts all 0 before input string). We
            -- drop the first item of this on each rehash
        -> [a]
            -- items after end window. We add the first item from this on each
            -- rehash.
        -> [[a]]
            -- Chunks
    go !h !c !bs (x:xs) (y:ys)

        -- Found new chunk
        | ((h' .&. mask == mask) && c >= minSize) || c >= maxSize
            = case L.splitAt (c + 1) bs of
                (l,r) -> l : go seed 0 r xs ys

        -- Continue
        | otherwise
            = go h' (c + 1) bs xs ys
      where
        !x' = if c < window then 0 else f x
        !h' = rem (rem (h - magic_d * x') magic_q * magic_r + f y) magic_q
    go _ _ bs _ _ = [bs]

    mask = (2^expectedPow2) - 1
    minSize = 2^minSizePow2
    maxSize = 2^maxSizePow2

    magic_d = powqp magic_r (fromIntegral window - 1) magic_q
    magic_r = 256
    magic_q = 32749

    window :: Int
    window  = minSize

    seed :: Int64
    seed = 0
{-# INLINE rollLazy #-}
{-# SCC rollLazy #-}

-- | Calculates @powqp m n p = 'rem' (m^n) p@ under the assumption that @m * p *
-- p@ can fit in an @Int64@.
--
-- This function is copied from https://github.com/ekmett/hash, Copyright
-- 2012-2013 Edward Kmett.
--
powqp :: Int64 -> Int64 -> Int64 -> Int64
powqp _ 0 _ = 1
powqp m 1 p = rem m p
powqp m n p = case quotRem n 2 of
  (q, 1) | k <- powqp m q p -> rem (k * k * m) p
  (q, _) | k <- powqp m q p -> rem (k * k) p
{-# INLINE powqp #-}
{-# SCC powqp #-}

