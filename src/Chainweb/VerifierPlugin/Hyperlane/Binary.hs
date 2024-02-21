{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.VerifierPlugin.Hyperlane.Binary
  ( HyperlaneMessage(..)
  , putHyperlaneMessage
  , getHyperlaneMessage

  , MessageIdMultisigIsmMetadata(..)
  , putMessageIdMultisigIsmMetadata
  , getMessageIdMultisigIsmMetadata

  , ethereumAddressSize
  ) where

import Data.Foldable
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
  , hmMessageBody :: BS.ByteString
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

  putRawByteString hmMessageBody

getHyperlaneMessage :: Get HyperlaneMessage
getHyperlaneMessage = do
  hmVersion <- getWord8
  hmNonce <- getWord32be
  hmOriginDomain <- getWord32be
  hmSender <- BS.takeEnd ethereumAddressSize <$> getByteString 32
  hmDestinationDomain <- getWord32be
  hmRecipient <- BS.dropWhile (== 0) <$> getByteString 32
  hmMessageBody <- BL.toStrict <$> getRemainingLazyByteString

  return $ HyperlaneMessage {..}

data MessageIdMultisigIsmMetadata = MessageIdMultisigIsmMetadata
  { mmimOriginMerkleTreeAddress :: ByteString
  , mmimSignedCheckpointRoot :: ByteString
  , mmimSignedCheckpointIndex :: Word32
  , mmimSignatures :: [ByteString]
  }

putMessageIdMultisigIsmMetadata :: MessageIdMultisigIsmMetadata -> Put
putMessageIdMultisigIsmMetadata (MessageIdMultisigIsmMetadata{..}) = do
  putRawByteString (padLeft mmimOriginMerkleTreeAddress)
  putRawByteString (padLeft mmimSignedCheckpointRoot)
  putWord32be mmimSignedCheckpointIndex

  forM_ mmimSignatures $ \s -> do
    putRawByteString s

getMessageIdMultisigIsmMetadata :: Get MessageIdMultisigIsmMetadata
getMessageIdMultisigIsmMetadata = do
  mmimOriginMerkleTreeAddress <- getByteString 32
  mmimSignedCheckpointRoot <- getByteString 32
  mmimSignedCheckpointIndex <- getWord32be

  signaturesBytes <- getRemainingLazyByteString
  let mmimSignatures = sliceSignatures signaturesBytes

  return $ MessageIdMultisigIsmMetadata{..}

-- | Pad with zeroes on the left to 32 bytes
--
-- > padLeft "hello world"
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULhello world"
padLeft :: ByteString -> ByteString
padLeft s = BS.replicate (32 - BS.length s) 0 <> s

-- | Signatures are 65 bytes sized, we split the bytestring by 65 symbols segments.
sliceSignatures :: BL.ByteString -> [ByteString]
sliceSignatures s
  | BL.length s < 65 = []
  | otherwise = let (sig, rest) = BL.splitAt 65 s in BL.toStrict sig : sliceSignatures rest
