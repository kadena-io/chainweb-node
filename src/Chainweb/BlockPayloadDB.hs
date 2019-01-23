module Chainweb.BlockPayloadDB
  ( DB(..)
  , PayloadConfig(..)
  ) where

import Control.DeepSeq
import Data.Vector (Vector)

import Chainweb.BlockHeader (BlockPayloadHash(..))
import Chainweb.Utils (Codec(..))

data PayloadConfig t = PayloadConfig {
    payloadCodec :: {-# UNPACK #-} !(Codec t)
  , payloadHash :: t -> BlockPayloadHash
}

data NFData t => DB t = DB {
    payloadLookup :: Vector BlockPayloadHash -> IO (Vector (Maybe t))
  , payloadInsert :: Vector t -> IO (Vector (Either String ()))
  , payloadDelete :: Vector BlockPayloadHash -> IO ()
}
