{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.ChainDB.Entry
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Implementation of "Chainweb.ChainDB.Entry" based on "Chainweb.BlockHeader".
--
module Chainweb.ChainDB.Entry
( Key
, Entry
, key
, parent
, rank
, encodeKey
, decodeKey
, encodeEntry
, decodeEntry
) where

import Control.Monad.Catch

import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.Text as T

import Numeric.Natural


-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader

type Key = BlockHash

-- | Type of a database entry
--
type Entry = BlockHeader

-- | Compute the 'Key' from an 'Entry'. A key is a globally unique hash of an
-- entry. Two entries have the same key if and only if they are the same.
--
key :: Entry -> Key
key = _blockHash

-- | Each but exaclty one entry has a parent. The unique entry without a parent
-- is called the root entry.
--
-- The parent relation induces a tree on the set of all entries.
--
parent :: Entry -> Maybe Key
parent e
    | isGenesisBlockHeader e = Nothing
    | otherwise = Just $ _blockParent e

-- | The rank of an entry is the depth of the entry in the tree from the root.
--
rank :: Entry -> Natural
rank = fromIntegral . _blockHeight

-- | Serialize a `Key`.
--
encodeKey :: Key -> B.ByteString
encodeKey = runPutS . encodeBlockHash

-- | Deserialize a `Key`.
--
-- Raises some exception if the decoding fails.
--
decodeKey :: MonadThrow m => B.ByteString -> m Key
decodeKey = either (throwM . DecodeFailure . T.pack) return
    . runGetS decodeBlockHash

-- | Serialize an `Entry`.
--
encodeEntry :: Entry -> B.ByteString
encodeEntry = runPutS . encodeBlockHeader

-- | Deserialize an `Entry`.
--
-- Raises some exception if the decoding fails.
--
decodeEntry :: MonadThrow m => B.ByteString -> m Entry
decodeEntry = either (throwM . DecodeFailure . T.pack) return
    . runGetS decodeBlockHeader

-- -------------------------------------------------------------------------- --
-- Exceptions

newtype BlockHeaderEntryException = DecodeFailure T.Text
    deriving (Show)

instance Exception BlockHeaderEntryException
