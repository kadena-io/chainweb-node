{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.Backend.PactState.GrandHash.Algorithm
  ( ChainGrandHash(..)
  , TableHash(..)
  , computeGrandHash
  , hashTable
  , rowToHashInput
  , tableNameToHashInput
  , hashStream
  , hashAlgorithm
  )
  where

import Chainweb.Pact.Backend.PactState (PactRow(..), PactRowContents(..))
import Crypto.Hash (hashInitWith, hashUpdate, hashFinalize)
import Crypto.Hash.Algorithms (SHA3_256(..))
import Data.Bifunctor (first)
import Data.ByteArray qualified as Memory
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S

-- | The GrandHash of a chain. This is the hash of all 'TableHash'es,
--   ordered and concatenated (or computed via incremental hash).
newtype ChainGrandHash = ChainGrandHash { getChainGrandHash :: ByteString }
  deriving newtype (Eq, Ord)
  deriving (Show) via HexBytes

-- | The hash of a table. This is the output of 'hashTable'.
newtype TableHash = TableHash { getTableHash :: ByteString }
  deriving newtype (Eq, Ord)
  deriving (Show) via HexBytes

newtype HexBytes = HexBytes ByteString

instance Show HexBytes where
  show (HexBytes b) = Text.unpack (Text.decodeUtf8 (Base16.encode b))

computeGrandHash :: forall m r. (Monad m)
  => Stream (Of (Text, Map ByteString PactRowContents)) m r
  -> m (ChainGrandHash, r)
computeGrandHash pactState = do
  -- We map over the state of the chain (tables) at the given blockheight.
  -- For each table, we sort the rows by rowKey, lexicographically.
  -- Then we feed the sorted rows into the incremental 'hashAggregate' function.
  let tableHashes :: Stream (Of TableHash) m r
      tableHashes = flip S.mapMaybeM pactState $ \(tblName, state) -> do
        let rows =
              Vector.fromList
              $ List.sortOn (\pr -> pr.rowKey)
              $ List.map (\(rowKey, PactRowContents{..}) -> PactRow{..})
              $ Map.toList state
        let tableHash = hashTable tblName rows
        pure tableHash

  -- This is a simple incremental hash over all of the table hashes.
  -- This is well-defined by its order because 'getLatestPactStateAt'
  -- guarantees that the stream of tables is ordered lexicographically by table
  -- name.
  fmap (first ChainGrandHash) $ hashStream $ S.map getTableHash tableHashes

hashStream :: (Monad m) => Stream (Of ByteString) m r -> m (ByteString, r)
hashStream s = do
  fmap S.lazily $ S.fold
    hashUpdate
    (hashInitWith hashAlgorithm)
    (Memory.convert . hashFinalize)
    s

-- | This is the grand hash of a table
--
--   Precondition: The PactRows must be in ordered by rowkey ASC
hashTable :: Text -> Vector PactRow -> Maybe TableHash
hashTable tblName rows
  | Vector.length rows == 0 = Nothing
  | otherwise = Just
      $ TableHash
      $ Memory.convert
      $ hashFinalize
      $ Vector.foldl'
          (\ctx row -> hashUpdate ctx (rowToHashInput row))
          (hashUpdate (hashInitWith hashAlgorithm) (tableNameToHashInput tblName))
          rows

-- | The hash algorithm used by the code in GrandHash-ing.
hashAlgorithm :: SHA3_256
hashAlgorithm = SHA3_256

-- | Pack a table name into bytes for into into a hash function.
--
--   The format is:
--
--   <char 'T'> (1 byte)
--   <UTF-8 length of lower-cased tablename : unsigned 64-bit little endian> (8 bytes)
--   <lowertable name in lowercase> (variable # of bytes, maximum size TBD)
--
--   And thus the size of the output will always be 9 bytes + the UTF-8 length
--   of the lower-cased table name. The tablename cannot be empty.
tableNameToHashInput :: Text -> ByteString
tableNameToHashInput tblName =
  let
    tbl = Text.encodeUtf8 (Text.toLower tblName)
  in
  BL.toStrict
  $ BB.toLazyByteString
  $ BB.charUtf8 'T'
    <> BB.word64LE (fromIntegral @Int @Word64 (BS.length tbl))
    <> BB.byteString tbl

-- | Pack a 'PactRow' into bytes for input into a hash function.
--
--   The format is:
--
--   <char 'K'> (1 byte)
--   <UTF-8 length of rowkey : unsigned 64-bit little endian> (8 bytes)
--   <rowkey> (variable # of bytes, maximum size TBD)
--
--   <char 'I'> (1 byte)
--   <txId : unsigned 64-bit little endian> (8 bytes)
--
--   <char 'D'> (1 byte)
--   <UTF-8 length of rowdata : unsigned 64-bit little endian> (8 bytes)
--   <rowdata> (variable # of bytes, maximum size TBD)
--
--   And thus the size of this will always be 27 bytes + the combined UTF-8
--   lengths of all the variable-sized inputs. None of the variable-sized
--   inputs can be empty.
rowToHashInput :: PactRow -> ByteString
rowToHashInput pr =
  BL.toStrict
  $ BB.toLazyByteString
  $
       BB.charUtf8 'K'
    <> BB.word64LE (fromIntegral @Int @Word64 (BS.length pr.rowKey))
    <> BB.byteString pr.rowKey

    <> BB.charUtf8 'I'
    <> BB.word64LE (fromIntegral @Int64 @Word64 pr.txId)

    <> BB.charUtf8 'D'
    <> BB.word64LE (fromIntegral @Int @Word64 (BS.length pr.rowData))
    <> BB.byteString pr.rowData
