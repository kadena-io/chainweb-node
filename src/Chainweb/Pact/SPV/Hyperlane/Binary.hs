{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Pact.SPV.Hyperlane.Binary where

import Data.ByteString as BS
import Data.DoubleWord
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word

import Chainweb.Utils.Serialization

-- | Ethereum address takes 20 bytes
ethereumAddressSize :: Int
ethereumAddressSize = 20

data HyperlaneMessage = HyperlaneMessage
  { hmVersion :: Word8            -- uint8
  , hmNonce :: Word32             -- uint32
  , hmOriginDomain :: Word32      -- uint32
  , hmSender :: BS.ByteString     -- bytes32
  , hmDestinationDomain :: Word32 -- uint32
  , hmRecipient :: BS.ByteString  -- bytes32
  , hmTokenMessage :: TokenMessageERC20
  }

-- Corresponds to abi.encodePacked behaviour
putHyperlaneMessage :: HyperlaneMessage -> Put
putHyperlaneMessage (HyperlaneMessage {..}) = do
  putWord8 hmVersion
  putWord32be hmNonce
  putWord32be hmOriginDomain
  putRawByteString (padLeft hmSender)
  putWord32be hmDestinationDomain
  putRawByteString (padLeft hmRecipient)

  putTokenMessageERC20 hmTokenMessage

getHyperlaneMessage :: Get HyperlaneMessage
getHyperlaneMessage = do
  hmVersion <- getWord8
  hmNonce <- getWord32be
  hmOriginDomain <- getWord32be
  hmSender <- BS.takeEnd ethereumAddressSize <$> getBS 32
  hmDestinationDomain <- getWord32be
  hmRecipient <- BS.dropWhile (== 0) <$> getBS 32
  hmTokenMessage <- getTokenMessageERC20

  return $ HyperlaneMessage {..}

data TokenMessageERC20 = TokenMessageERC20
  { tmRecipient :: Text -- string
  , tmAmount :: Word256 -- uint256
  } deriving (Show, Eq)

-- example
-- 0000000000000000000000000000000000000000000000000000000000000040 # 64
-- 0000000000000000000000000000000000000000000000008ac7230489e80000 # 10000000000000000000
-- 000000000000000000000000000000000000000000000000000000000000002a # 42
-- 3078373143373635364543376162383862303938646566423735314237343031 # "0x71C7656EC7ab88b098defB751B7401B5f6d8976F"
-- 4235663664383937364600000000000000000000000000000000000000000000

-- Corresponds to abi.encode behaviour
putTokenMessageERC20 :: TokenMessageERC20 -> Put
putTokenMessageERC20 (TokenMessageERC20 {..}) = do
  -- the first offset is constant
  putWord256be (64 :: Word256) -- 32 bytes
  putWord256be tmAmount        -- 32 bytes
  -- 64 bytes
  putWord256be recipientSize   -- 32 bytes
  putRawByteString recipient   -- recipientSize
  where
    (recipient, recipientSize) = padRight $ Text.encodeUtf8 tmRecipient

getTokenMessageERC20 :: Get TokenMessageERC20
getTokenMessageERC20 = do
    _firstOffset <- getWord256be
    tmAmount <- getWord256be

    recipientSize <- getWord256be
    tmRecipient <- Text.decodeUtf8 <$> getBS recipientSize
    return $ TokenMessageERC20 {..}

data MessageIdMultisigIsmMetadata = MessageIdMultisigIsmMetadata
  { mmimOriginMerkleTreeAddress :: ByteString
  , mmimSignedCheckpointRoot :: ByteString
  , mmimSignedCheckpointIndex :: Word256
  , mmimSignatures :: [ByteString]
  }

-- example
-- 6f726967696e4d65726b6c655472656541646472657373000000000000000000 32 originMerkleTreeAddress
-- 6d65726b6c65526f6f7400000000000000000000000000000000000000000000 64 merkleRoot
-- 00000000000000000000000000000000000000000000000000000000ffffffff 96 4294967295
-- 0000000000000000000000000000000000000000000000000000000000000080 128 128
-- 0000000000000000000000000000000000000000000000000000000000000041 160 65
-- 4e45a1dc8d84b3a63db8c9c6cbe4e0a780ecb55ff157c038b9f5d1a2be7a0a02
-- 77c3c8d8e6029f65f7f7b0ad8b80fae2b178d14c9a7b228a539349aad0c7b58b
-- 1b00000000000000000000000000000000000000000000000000000000000000

getMessageIdMultisigIsmMetadata :: Get MessageIdMultisigIsmMetadata
getMessageIdMultisigIsmMetadata = do
  mmimOriginMerkleTreeAddress <- getBS 32
  mmimSignedCheckpointRoot <- getBS 32
  mmimSignedCheckpointIndex <- getWord256be
  _firstOffset <- getWord256be

  -- we don't care about the size, we know that each signature is 65 bytes long
  signaturesSize <- getWord256be

  signaturesBytes <- getByteString (fromIntegral signaturesSize)
  let mmimSignatures = sliceSignatures signaturesBytes

  return $ MessageIdMultisigIsmMetadata{..}

-- | Pad with zeroes on the left to 32 bytes
--
-- > padLeft "hello world"
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULhello world"
padLeft :: ByteString -> ByteString
padLeft s = BS.replicate (32 - BS.length s) 0 <> s

-- | Pad with zeroes on the right, such that the resulting size is a multiple of 32.
--
-- > padRight "hello world"
-- ("hello world\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL",11)
padRight :: ByteString -> (ByteString, Word256)
padRight s =
  let
    size = BS.length s
    missingZeroes = restSize size
  in (s <> BS.replicate missingZeroes 0, fromIntegral size)

-- | Returns the modular of 32 bytes.
restSize :: Integral a => a -> a
restSize size = (32 - size) `mod` 32

-- | Reads a given number of bytes and the rest because binary data padded up to 32 bytes.
getBS :: Word256 -> Get BS.ByteString
getBS size = (BS.take (fromIntegral size)) <$> getByteString (fromIntegral $ size + restSize size)

-- | Signatures are 65 bytes sized, we split the bytestring by 65 symbols segments.
sliceSignatures :: ByteString -> [ByteString]
sliceSignatures sig' = go sig' []
  where
    go s sigs = if BS.length s >= 65
      then let (sig, rest) = BS.splitAt 65 s in go rest (sig:sigs)
      else Prelude.reverse sigs