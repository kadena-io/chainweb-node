{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Pact.SPV.Hyperlane where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy
import Data.ByteString.Builder

import Data.Text (Text)
import Data.Binary

import Numeric.Natural


newtype Argument = Argument ByteString

data HyperlaneMessage = HyperlaneMessage
  { fmcVersion :: Word8            -- uint8
  , fmcNonce :: Word32             -- uint32
  , fmcOriginDomain :: Word32      -- uint32
  , fmcSender :: ByteString        -- bytes32
  , fmcDestinationDomain :: Word32 -- uint32
  , fmcRecipient :: ByteString     -- bytes32
  , fmcMessageBody :: ByteString   -- bytes
  }

instance Binary HyperlaneMessage where
  put (HyperlaneMessage {..}) = do
    put fmcVersion
    put fmcNonce
    put fmcOriginDomain
    put fmcSender
    put fmcDestinationDomain
    put fmcRecipient
    put fmcMessageBody

  get = do
    fmcVersion <- get :: Get Word8
    fmcNonce <- get :: Get Word32
    fmcOriginDomain <- get :: Get Word32
    fmcSender <- get :: Get ByteString
    fmcDestinationDomain <- get :: Get Word32
    fmcRecipient <- get :: Get ByteString
    fmcMessageBody <- get :: Get ByteString
    return $ HyperlaneMessage {..}

data TokenMessageERC20 = TokenMessageERC20
  { tmRecipient :: Text -- string
  , tmAmount :: Natural       -- uint256
  , tmMetadata :: Text  -- string
  }

instance Binary TokenMessageERC20 where
  put (TokenMessageERC20 {..}) = do
    put tmRecipient
    put tmAmount
    put tmMetadata

  get = do
    -- tmRecipient <- get :: Get ByteString
    -- tmAmount <- get :: Get Word32
    -- tmMetadata <- get :: Get ByteString
    return $ TokenMessageERC20 {..}

data TokenMessageERC721 = TokenMessageERC721
  { tmRecipient :: ByteString -- bytes
  , tmTokenId :: Natural      -- uint256
  , tmMetadata :: ByteString  -- bytes
  }

instance Binary TokenMessageERC721 where
  put (TokenMessageERC721 {..}) = do
    put tmRecipient
    put tmTokenId
    put tmMetadata

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