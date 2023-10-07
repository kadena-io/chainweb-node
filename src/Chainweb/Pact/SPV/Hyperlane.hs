{-# LANGUAGE RecordWildCards #-}

module Chainweb.Pact.SPV.Hyperlane where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy
import Data.ByteString.Builder

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

data TokenMessage = TokenMessage
  { tmRecipient :: ByteString -- bytes
  , tmAmount :: Natural       -- uint256
  , tmMetadata :: ByteString  -- bytes
  }

{-

function formatMessage(
        uint8 _version,
        uint32 _nonce,
        uint32 _originDomain,
        bytes32 _sender,
        uint32 _destinationDomain,
        bytes32 _recipient,
        bytes calldata _messageBody
    ) internal pure returns (bytes memory) {
        return
            abi.encodePacked(
                _version,
                _nonce,
                _originDomain,
                _sender,
                _destinationDomain,
                _recipient,
                _messageBody
            );
    }
-}

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

parseHyperlaneMessage :: BS.ByteString -> Either String HyperlaneMessage
parseHyperlaneMessage s =
  case decodeOrFail (fromStrict s) of
    Right (_, _, m) -> Right m
    Left _ -> Left "Failed to parse hyperlane message"