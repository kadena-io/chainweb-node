{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Chainweb.Pact.Backend.PactState.EmbeddedSnapshot
  ( Snapshot(..)
  , unsafeDecodeBlockHeader
  , unsafeBase16Decode
  )
  where

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.Pact.Backend.PactState.GrandHash.Algorithm (ChainGrandHash)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

data Snapshot = Snapshot
  { pactHash :: ChainGrandHash
  , blockHeader :: BlockHeader
  }
  deriving stock (Eq, Show)

unsafeDecodeBlockHeader :: Text -> BlockHeader
unsafeDecodeBlockHeader t = case A.decodeStrict (Text.encodeUtf8 t) of
  Just a -> a
  Nothing -> error "unsafeDecodeBlockHeader: invalid BlockHeader JSON"

unsafeBase16Decode :: Text -> ByteString
unsafeBase16Decode t = case Base16.decode (Text.encodeUtf8 t) of
  Right a -> a
  Left err -> error ("unsafeBase16Decode failed: " ++ show err)
