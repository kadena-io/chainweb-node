{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Pact.SPV.Hyperlane where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.ByteString.Builder

import Data.Int
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Binary
import Data.Decimal
import Numeric.Natural

data HyperlaneMessage = HyperlaneMessage
  { fmcVersion :: Word8            -- uint8
  , fmcNonce :: Word32             -- uint32
  , fmcOriginDomain :: Word32      -- uint32
  , fmcSender :: Text              -- string
  , fmcDestinationDomain :: Word32 -- uint32
  , fmcRecipient :: Text           -- string
  , fmcMessageBody :: Text         -- string
  }

instance Binary HyperlaneMessage where
  put (HyperlaneMessage {..}) = do
    put $ padLeft $ encode fmcVersion                                 -- 32 bytes
    put $ padLeft $ encode fmcNonce                                   -- 32 bytes
    put $ padLeft $ encode fmcOriginDomain                            -- 32 bytes
    -- 96 bytes
    put $ padLeft $ encode (96 + 32 * 4 :: Int)                              -- 32 bytes
    put $ padLeft $ encode fmcDestinationDomain                       -- 32 bytes
    put $ padLeft $ encode (96 + 32 * 5 + senderSize)                 -- 32 bytes
    put $ padLeft $ encode (96 + 32 * 6 + senderSize + recipientSize) -- 32 bytes
    -- 96 + 32 * 4
    put $ padLeft $ encode senderSize                                 -- 32 bytes
    put sender                                                        -- senderSize
    -- 96 + 32 * 5 + senderSize
    put $ padLeft $ encode recipientSize                              -- 32 bytes
    put recipient                                                     -- recipientSize
    -- 96 + 32 * 6 + senderSize + recipientSize
    put $ padLeft $ encode messageBodySize                            -- 32 bytes
    put messageBody                                                   -- messageBodySize
    where
      (sender, senderSize) = padRight $ BL.fromStrict $ Text.encodeUtf8 fmcSender
      (recipient, recipientSize) = padRight $ BL.fromStrict $ Text.encodeUtf8 fmcRecipient
      (messageBody, messageBodySize) = padRight $ BL.fromStrict $ Text.encodeUtf8 fmcMessageBody

  get = do
    fmcVersion <- get :: Get Word8
    fmcNonce <- get :: Get Word32
    fmcOriginDomain <- get :: Get Word32
    fmcSender <- get :: Get Text
    fmcDestinationDomain <- get :: Get Word32
    fmcRecipient <- get :: Get Text
    fmcMessageBody <- get :: Get Text
    return $ HyperlaneMessage {..}

data TokenMessageERC20 = TokenMessageERC20
  { tmRecipient :: Text -- string
  , tmAmount :: Integer -- uint256
  , tmMetadata :: Text  -- string
  }

instance Binary TokenMessageERC20 where
  put (TokenMessageERC20 {..}) = do
    -- the first offset is constant
    put $ padLeft $ encode (96 :: Int)               -- 32 bytes
    put $ padLeft $ encode tmAmount                  -- 32 bytes
    put $ padLeft $ encode (96 + 32 + recipientSize) -- 32 bytes
    -- 96 bytes
    put $ padLeft $ encode recipientSize             -- 32 bytes
    put recipient                                    -- recipientSize
    -- 96 bytes + 32 bytes + recipientSize
    put $ padLeft $ encode metadataSize              -- 32 bytes
    put metadata                                     -- metadataSize
    where
      (recipient, recipientSize) = padRight $ BL.fromStrict $ Text.encodeUtf8 tmRecipient
      (metadata, metadataSize) = padRight $ BL.fromStrict $ Text.encodeUtf8 tmMetadata

  get = do
    -- tmRecipient <- get :: Get ByteString
    -- tmAmount <- get :: Get Word32
    -- tmMetadata <- get :: Get ByteString
    return $ TokenMessageERC20 {..}

data TokenMessageERC721 = TokenMessageERC721
  { tmRecipient :: Text  -- bytes
  , tmTokenId :: Integer -- uint256
  , tmMetadata :: Text   -- bytes
  }

instance Binary TokenMessageERC721 where
  put (TokenMessageERC721 {..}) = do
    -- the first offset is constant
    put $ padLeft $ encode (96 :: Int)               -- 32 bytes
    put $ padLeft $ encode tmTokenId                 -- 32 bytes
    put $ padLeft $ encode (96 + 32 + recipientSize) -- 32 bytes
    -- 96 bytes
    put $ padLeft $ encode recipientSize             -- 32 bytes
    put recipient                                    -- recipientSize
    -- 96 bytes + 32 bytes + recipientSize
    put $ padLeft $ encode metadataSize              -- 32 bytes
    put metadata                                     -- metadataSize
    where
      (recipient, recipientSize) = padRight $ BL.fromStrict $ Text.encodeUtf8 tmRecipient
      (metadata, metadataSize) = padRight $ BL.fromStrict $ Text.encodeUtf8 tmMetadata

  get = do
    -- tmRecipient <- get :: Get ByteString
    -- tmTokenId <- get :: Get Word32
    -- tmMetadata <- get :: Get ByteString
    return $ TokenMessageERC721 {..}

parseHyperlaneMessage :: BS.ByteString -> Either String HyperlaneMessage
parseHyperlaneMessage s =
  case decodeOrFail (fromStrict s) of
    Right (_, _, m) -> Right m
    Left _ -> Left "Failed to parse hyperlane message"

-- | Pad with zeroes on the left to 32 bytes
--
-- > padLeft "hello world"
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULhello world"
padLeft :: ByteString -> ByteString
padLeft s = BL.replicate (32 - BL.length s) 0 <> s

-- | Pad with zeroes on the right, such that the resulting size is a multiple of 32.
--
-- > padRight "hello world"
-- ("hello world\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL",32)
padRight :: ByteString -> (ByteString, Int64)
padRight s =
  let
    size = BL.length s
    missingZeroes = fromIntegral $ (32 - size) `mod` 32
  in (s <> BL.replicate missingZeroes 0, size + missingZeroes)