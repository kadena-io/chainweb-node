{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.ChainDB.Entry.BlockHeader
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Implementation of "Chainweb.ChainDB.Entry" based on "Chainweb.BlockHeader".
--
module Chainweb.ChainDB.Entry.BlockHeader
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
type Entry = BlockHeader

key :: Entry -> Key
key = _blockHash

parent :: Entry -> Maybe Key
parent e
    | isGenesisBlockHeader e = Nothing
    | otherwise = Just $ _blockParent e

rank :: Entry -> Natural
rank = fromIntegral . _blockHeight

encodeKey :: Key -> B.ByteString
encodeKey = runPutS . encodeBlockHash

decodeKey :: MonadThrow m => B.ByteString -> m Key
decodeKey = either (throwM . DecodeFailure . T.pack) return
    . runGetS decodeBlockHash

encodeEntry :: Entry -> B.ByteString
encodeEntry = runPutS . encodeBlockHeader

decodeEntry :: MonadThrow m => B.ByteString -> m Entry
decodeEntry = either (throwM . DecodeFailure . T.pack) return
    . runGetS decodeBlockHeader

-- -------------------------------------------------------------------------- --
-- Exceptions

newtype BlockHeaderEntryException = DecodeFailure T.Text
    deriving (Show)

instance Exception BlockHeaderEntryException

