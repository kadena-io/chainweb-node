{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}


module Chainweb.Pact.SPV.Hyperlane where

import Control.Monad (replicateM)

import Data.ByteString as BS
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder

import Data.DoubleWord
import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Binary
import qualified Data.Binary.Builder as Builder
import Data.Binary.Get
import Data.Binary.Put
import Data.Decimal
import Numeric.Natural

data HyperlaneMessage = HyperlaneMessage
  { hmVersion :: Word256           -- uint8
  , hmNonce :: Word256             -- uint32
  , hmOriginDomain :: Word256      -- uint32
  , hmSender :: BS.ByteString      -- bytes32
  , hmDestinationDomain :: Word256 -- uint32
  , hmRecipient :: BS.ByteString   -- bytes32
  , hmMessageBody :: Text          -- string
  }

instance Binary HyperlaneMessage where
  put (HyperlaneMessage {..}) = do
    put hmVersion                                  -- 32 bytes
    put hmNonce                                    -- 32 bytes
    put hmOriginDomain                             -- 32 bytes
    -- 96 bytes
    put (96 + 32 * 4 :: Word256)                   -- 32 bytes
    put hmDestinationDomain                        -- 32 bytes
    put (96 + 32 * 5 + senderSize)                 -- 32 bytes
    put (96 + 32 * 6 + senderSize + recipientSize) -- 32 bytes
    -- 96 + 32 * 4
    put senderSize                                 -- 32 bytes
    putBS sender                                   -- senderSize
    -- 96 + 32 * 5 + senderSize
    put recipientSize                              -- 32 bytes
    putBS recipient                                -- recipientSize
    -- 96 + 32 * 6 + senderSize + recipientSize
    put messageBodySize                            -- 32 bytes
    putBS messageBody                              -- messageBodySize
    where
      (sender, senderSize) = padRight hmSender
      (recipient, recipientSize) = padRight hmRecipient
      (messageBody, messageBodySize) = padRight $ Text.encodeUtf8 hmMessageBody

  get = do
    hmVersion <- getWord256be
    hmNonce <- getWord256be
    hmOriginDomain <- getWord256be

    _firstOffset <- getWord256be
    hmDestinationDomain <- getWord256be
    _secondOffset <- getWord256be
    _thirdOffset <- getWord256be

    senderSize <- getWord256be
    hmSender <- getBS senderSize

    recipientSize <- getWord256be
    hmRecipient <- getBS recipientSize

    messageBodySize <- getWord256be
    hmMessageBody <- Text.decodeUtf8 <$> getBS messageBodySize

    return $ HyperlaneMessage {..}

data TokenMessageERC20 = TokenMessageERC20
  { tmRecipient :: Text -- string
  , tmAmount :: Word256 -- uint256
  }

instance Binary TokenMessageERC20 where
  put (TokenMessageERC20 {..}) = do
    -- the first offset is constant
    put (96 :: Word256)           -- 32 bytes
    put tmAmount                  -- 32 bytes
    -- 96 bytes
    put recipientSize             -- 32 bytes
    putBS recipient               -- recipientSize
    where
      (recipient, recipientSize) = padRight $ Text.encodeUtf8 tmRecipient

  get = do
    _firstOffset <- getWord256be
    tmAmount <- getWord256be

    recipientSize <- getWord256be
    tmRecipient <- Text.decodeUtf8 <$> getBS recipientSize
    return $ TokenMessageERC20 {..}

data TokenMessageERC721 = TokenMessageERC721
  { tmRecipient :: Text  -- bytes
  , tmTokenId :: Word256 -- uint256
  , tmMetadata :: Text   -- bytes
  }

instance Binary TokenMessageERC721 where
  put (TokenMessageERC721 {..}) = do
    -- the first offset is constant
    put (96 :: Word256)           -- 32 bytes
    put tmTokenId                 -- 32 bytes
    put (96 + 32 + recipientSize) -- 32 bytes
    -- 96 bytes
    put recipientSize             -- 32 bytes
    putBS recipient               -- recipientSize
    -- 96 bytes + 32 bytes + recipientSize
    put metadataSize              -- 32 bytes
    putBS metadata                -- metadataSize
    where
      (recipient, recipientSize) = padRight $ Text.encodeUtf8 tmRecipient
      (metadata, metadataSize) = padRight $ Text.encodeUtf8 tmMetadata

  get = do
    _firstOffset <- getWord256be
    tmTokenId <- getWord256be
    _secondOffset <- getWord256be

    recipientSize <- getWord256be
    tmRecipient <- Text.decodeUtf8 <$> getBS recipientSize

    metadataSize <- getWord256be
    tmMetadata <- Text.decodeUtf8 <$> getBS metadataSize
    return $ TokenMessageERC721 {..}

data MessageMultisigIsmMetadata = MessageMultisigIsmMetadata
  { mmimOriginMerkleTreeAddress :: Text
  , mmimSignedCheckpointRoot :: Text
  , mmimSignedCheckpointIndex :: Word256
  , mmimSignatures :: [Text]
  }

instance Binary MessageMultisigIsmMetadata where
  put = error "put instance is not implemented for MessageMultisigIsmMetadata"

  get = do
    _firstOffset <- getWord256be
    _secondOffset <- getWord256be
    mmimSignedCheckpointIndex <- getWord256be
    _thirdOffset <- getWord256be

    addressSize <- getWord256be
    mmimOriginMerkleTreeAddress <- Text.decodeUtf8 <$> getBS addressSize

    rootSize <- getWord256be
    mmimSignedCheckpointRoot <- Text.decodeUtf8 <$> getBS rootSize

    rootSize <- getWord256be
    mmimSignedCheckpointRoot <- Text.decodeUtf8 <$> getBS rootSize

    listSize <- getWord256be

    _offsets <- replicateM (fromIntegral listSize) getWord256be

    mmimSignatures <- replicateM (fromIntegral listSize) $ do
      signatureSize <- getWord256be
      signatureBody <- Text.decodeUtf8 <$> getBS signatureSize
      return signatureBody

    return $ MessageMultisigIsmMetadata{..}

-- | Pad with zeroes on the left to 32 bytes
--
-- > padLeft "hello world"
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULhello world"
padLeft :: ByteString -> ByteString
padLeft s = BS.replicate (32 - BS.length s) 0 <> s

-- | Pad with zeroes on the right, such that the resulting size is a multiple of 32.
--
-- > padRight "hello world"
-- ("hello world\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL",32)
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
