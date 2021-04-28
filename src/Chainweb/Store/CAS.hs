-- | Content-addressable stores.
module Chainweb.Store.CAS
  ( DB(..)
  , PayloadConfig(..)
  ) where

------------------------------------------------------------------------------
import Data.Vector (Vector)
------------------------------------------------------------------------------
import Chainweb.BlockHeader
import Chainweb.Utils (Codec(..))
------------------------------------------------------------------------------

-- | 'PayloadConfig' tells the CAS store how to interpret payloads.
data PayloadConfig t = PayloadConfig {
    payloadCodec :: {-# UNPACK #-} !(Codec t)    -- ^ codec for the payload.
  , payloadHash :: t -> BlockPayloadHash         -- ^ hash function for the payload.
}

-- | The content-addressable store DB api.
data DB t = DB {
    casDbConfig :: PayloadConfig t
  , casLookup :: Vector BlockPayloadHash -> IO (Vector (Maybe t))
  , casInsert :: Vector t -> IO ()
  , casDelete :: Vector BlockPayloadHash -> IO ()
}
