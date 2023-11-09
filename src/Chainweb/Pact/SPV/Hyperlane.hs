{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Pact.SPV.Hyperlane where

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.DoubleWord
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Binary
import qualified Data.Binary.Builder as Builder
import Data.Binary.Get
import Data.Binary.Put

data HyperlaneMessage = HyperlaneMessage
  { hmVersion :: Word8            -- uint8
  , hmNonce :: Word32             -- uint32
  , hmOriginDomain :: Word32      -- uint32
  , hmSender :: BS.ByteString     -- bytes32
  , hmDestinationDomain :: Word32 -- uint32
  , hmRecipient :: BS.ByteString  -- bytes32
  , hmTokenMessage :: TokenMessageERC20
  }

instance Binary HyperlaneMessage where
  put (HyperlaneMessage {..}) = do
    put hmVersion
    put hmNonce
    put hmOriginDomain
    putBS (padLeft hmSender)
    put hmDestinationDomain
    putBS (padLeft hmRecipient)

    put hmTokenMessage

  get = do
    hmVersion <- getWord8
    hmNonce <- getWord32be
    hmOriginDomain <- getWord32be
    hmSender <- BS.drop 12 <$> getBS 32
    hmDestinationDomain <- getWord32be
    hmRecipient <- (BS.dropWhile (==0)) <$> getBS 32
    rest <- getRemainingLazyByteString
    let hmTokenMessage = decode rest

    return $ HyperlaneMessage {..}

data TokenMessageERC20 = TokenMessageERC20
  { tmRecipient :: Text -- string
  , tmAmount :: Word256 -- uint256
  } deriving (Show, Eq)

instance Binary TokenMessageERC20 where
  put (TokenMessageERC20 {..}) = do
    -- the first offset is constant
    put (64 :: Word256) -- 32 bytes
    put tmAmount        -- 32 bytes
    -- 64 bytes
    put recipientSize   -- 32 bytes
    putBS recipient     -- recipientSize
    where
      (recipient, recipientSize) = padRight $ Text.encodeUtf8 tmRecipient

  get = do
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

instance Binary MessageIdMultisigIsmMetadata where
  put = error "put instance is not implemented for MessageIdMultisigIsmMetadata"

  get = do
    mmimOriginMerkleTreeAddress <- getBS 32
    mmimSignedCheckpointRoot <- getBS 32
    mmimSignedCheckpointIndex <- getWord256be
    _firstOffset <- getWord256be

    -- we don't care about the size, we know that each signature is 65 bytes long
    _signaturesSize <- getWord256be

    theRest <- BL.toStrict <$> getRemainingLazyByteString
    let mmimSignatures = sliceSignatures theRest

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

restSize :: Integral a => a -> a
restSize size = (32 - size) `mod` 32

-- | Puts bytestring without size using 'Builder'.
putBS :: ByteString -> Put
putBS s = putBuilder $ Builder.fromByteString s

getBS :: Word256 -> Get BS.ByteString
getBS size = (BS.take (fromIntegral size)) <$> getByteString (fromIntegral $ size + restSize size)

instance Binary Word128 where
  put (Word128 w1 w2) = do
    putWord64be w1
    putWord64be w2

  get = do
    w1 <- getWord64be
    w2 <- getWord64be
    pure $ Word128 w1 w2

putWord128be :: Word128 -> Put
putWord128be = put

getWord128be :: Get Word128
getWord128be = get

instance Binary Word256 where
  put (Word256 w1 w2) = do
    putWord128be w1
    putWord128be w2

  get = do
    w1 <- getWord128be
    w2 <- getWord128be
    pure $ Word256 w1 w2

putWord256be :: Word256 -> Put
putWord256be = put

getWord256be :: Get Word256
getWord256be = get

sliceSignatures :: ByteString -> [ByteString]
sliceSignatures sig' = go sig' []
  where
    go s sigs = if BS.length s >= 65
      then let (sig, rest) = BS.splitAt 65 s in go rest (sig:sigs)
      else Prelude.reverse sigs