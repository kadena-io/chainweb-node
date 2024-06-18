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

  , MerkleRootMultisigIsmMetadata(..)
  , putMerkleRootMultisigIsmMetadata
  , getMerkleRootMultisigIsmMetadata

  , padLeft
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
  putRawByteString hmSender
  putWord32be hmDestinationDomain
  putRawByteString hmRecipient

  putRawByteString hmMessageBody

getHyperlaneMessage :: Get HyperlaneMessage
getHyperlaneMessage = do
  hmVersion <- getWord8
  hmNonce <- getWord32be
  hmOriginDomain <- getWord32be
  hmSender <- getByteString 32
  hmDestinationDomain <- getWord32be
  hmRecipient <- getByteString 32
  hmMessageBody <- BL.toStrict <$> getRemainingLazyByteString

  return $ HyperlaneMessage {..}

{-
  Format of metadata:
  [   0:  32] Origin merkle tree address
  [  32:  64] Signed checkpoint root
  [  64:  68] Signed checkpoint index
  [  68:????] Validator signatures (length := threshold * 65)

  Copied from hyperlane code base
  https://github.com/hyperlane-xyz/hyperlane-monorepo/blob/b14f997810ebd7dbdff2ac6622a149ae77010ae3/solidity/contracts/isms/libs/MessageIdMultisigIsmMetadata.sol#L5
-}
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

{-
  Format of metadata:
  [   0:  32] Origin merkle tree address
  [  32:  36] Index of message ID in merkle tree
  [  36:  68] Signed checkpoint message ID
  [  68:1092] Merkle proof
  [1092:1096] Signed checkpoint index (computed from proof and index)
  [1096:????] Validator signatures (length := threshold * 65)

  Copied from hyperlane code base
  https://github.com/hyperlane-xyz/hyperlane-monorepo/blob/b14f997810ebd7dbdff2ac6622a149ae77010ae3/solidity/contracts/isms/libs/MerkleRootMultisigIsmMetadata.sol#L5
-}
data MerkleRootMultisigIsmMetadata = MerkleRootMultisigIsmMetadata
  { mrmimOriginMerkleTreeAddress :: ByteString
  , mrmimMessageIdIndex :: Word32
  , mrmimSignedCheckpointMessageId :: ByteString
  , mrmimMerkleProof :: ByteString
  , mrmimSignedCheckpointIndex :: Word32
  , mrmimSignatures :: [ByteString]
  }

putMerkleRootMultisigIsmMetadata :: MerkleRootMultisigIsmMetadata -> Put
putMerkleRootMultisigIsmMetadata (MerkleRootMultisigIsmMetadata{..}) = do
  putRawByteString (padLeft mrmimOriginMerkleTreeAddress)
  putWord32be mrmimMessageIdIndex
  putRawByteString (padLeft mrmimSignedCheckpointMessageId)
  putRawByteString mrmimMerkleProof
  putWord32be mrmimSignedCheckpointIndex

  forM_ mrmimSignatures $ \s -> do
    putRawByteString s

getMerkleRootMultisigIsmMetadata :: Get MerkleRootMultisigIsmMetadata
getMerkleRootMultisigIsmMetadata = do
  mrmimOriginMerkleTreeAddress <- getByteString 32
  mrmimMessageIdIndex <- getWord32be
  mrmimSignedCheckpointMessageId <- getByteString 32
  mrmimMerkleProof <- getByteString 1024
  mrmimSignedCheckpointIndex <- getWord32be

  signaturesBytes <- getRemainingLazyByteString
  let mrmimSignatures = sliceSignatures signaturesBytes

  return $ MerkleRootMultisigIsmMetadata{..}

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
