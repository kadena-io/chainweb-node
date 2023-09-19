{-# LANGUAGE RecordWildCards #-}

module Chainweb.Pact.SPV.Hyperlane where

import Data.ByteString.Lazy
import Data.ByteString.Builder

import Data.Binary

newtype Argument = Argument ByteString

data FormatMessageCall = FormatMessageCall
  { fmcVersion :: Word8
  , fmcNonce :: Word32
  , fmcOriginDomain :: Word32
  , fmcSender :: ByteString
  , fmcDestinationDomain :: Word32
  , fmcRecipient :: ByteString
  , fmcMessageBody :: ByteString
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

signature: "formatMessage(uint8,uint32,uint32,bytes32,uint32,bytes32,bytes)"
Keccak hash of the signature "0xd57f8952f34e582e3760214a5ed542cb2b0fe2940dade6d615c363192405938e"
method id: "0xd57f8952"
-}

instance Binary FormatMessageCall where
  put (FormatMessageCall {..}) = do
    put fmcVersion
    put fmcNonce
    put fmcOriginDomain
    put fmcSender
    put fmcDestinationDomain
    put fmcRecipient
    put fmcSender

  get = do
    -- parse method id
    b1 <- get :: Get Word8
    b2 <- get :: Get Word8
    b3 <- get :: Get Word8
    b4 <- get :: Get Word8

    case (b1, b2, b3, b4) of
      (0xd5, 0x7f, 0x89, 0x52) -> do
        fmcVersion <- get :: Get Word8
        fmcNonce <- get :: Get Word32
        fmcOriginDomain <- get :: Get Word32
        fmcSender <- get :: Get ByteString
        fmcDestinationDomain <- get :: Get Word32
        fmcRecipient <- get :: Get ByteString
        fmcSender <- get :: Get ByteString
        return $ FormatMessageCall {..}
      _ -> fail "Unexpected method id: " ++ show (b1, b2, b3, b4)


--    "0xcdcd77c0992ec5bbfc459984220f8c45084cc24d9b6efed1fae540db8de801d2"