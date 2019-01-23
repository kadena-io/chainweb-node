-- | Block Payload DB API.
module Chainweb.BlockPayloadDB
  ( DB(..)
  , PayloadConfig(..)
  ) where

------------------------------------------------------------------------------
import Data.Vector (Vector)
------------------------------------------------------------------------------
import Chainweb.BlockHeader (BlockPayloadHash(..))
import Chainweb.Utils (Codec(..))
------------------------------------------------------------------------------

-- | 'PayloadConfig' tells the block payload db how to interpret block payloads.
data PayloadConfig t = PayloadConfig {
    payloadCodec :: {-# UNPACK #-} !(Codec t)    -- ^ codec for the payload.
  , payloadHash :: t -> BlockPayloadHash         -- ^ hash function for the payload.
}

-- | The block payload DB api.
data DB t = DB {
    payloadLookup :: Vector BlockPayloadHash -> IO (Vector (Maybe t))
  , payloadInsert :: Vector t -> IO (Vector (Either String ()))
  , payloadDelete :: Vector BlockPayloadHash -> IO ()
}
