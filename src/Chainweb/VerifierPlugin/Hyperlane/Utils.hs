{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.VerifierPlugin.Hyperlane.Utils
  ( recoverAddress

  , keccak256ByteString

  , wordToDecimal
  , decimalToWord

  , encodeHex
  , decodeHex

  , ethereumHeader
  ) where

import Control.Lens hiding (index)
import Control.Monad.Catch

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Builder as Builder
import Data.Text (Text)
import Data.DoubleWord
import Data.Decimal
import Data.Ratio
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Crypto.Secp256k1 as ECDSA

import Ethereum.Misc hiding (Word256)

import Chainweb.VerifierPlugin.Hyperlane.Binary

-- | Recovers the address from keccak256 encoded digest and signature.
recoverAddress :: MonadThrow m => Keccak256Hash -> B.ByteString -> m (Maybe B.ByteString)
recoverAddress digest sig' = do
  fnDigest <- ECDSA.ecdsaMessageDigest $ _getBytesN $ _getKeccak256Hash digest
  let
    mkR s = ECDSA.ecdsaR $ BS.toShort s
    mkS s = ECDSA.ecdsaS $ BS.toShort s
    mkV s = ECDSA.ecdsaV $ BS.toShort s
    ecrecover sig = do
      -- signature is a 65 bytes long sequence (r, s, v), where r and s are both 32 bytes and v is 1 byte
      let (binR, sAndV) = B.splitAt 32 sig
      r <- mkR binR
      s <- mkS (B.take 32 sAndV)
      v <- mkV (B.drop 32 sAndV)
      pure $ ECDSA.ecdsaRecoverPublicKey fnDigest r s v <&> getAddress

  ecrecover sig'

-- | Returns an address, a rightmost 160 bits (20 bytes) of the keccak hash of the public key.
getAddress :: ECDSA.EcdsaPublicKey -> B.ByteString
getAddress pubkey = B.takeEnd ethereumAddressSize
    $ keccak256ByteString
    $ BS.fromShort
    $ BS.drop 1 -- drop the first 0x04 byte the indicates that the key is encoded in compressed format
    $ ECDSA.ecdsaPublicKeyBytes pubkey

-- | Header of the 32 bytes ethereum binary message.
ethereumHeader :: B.ByteString
ethereumHeader = "\x19" <> "Ethereum Signed Message:\n" <> "32"

encodeHex :: B.ByteString -> Text
encodeHex = ((<>) "0x") . Text.decodeUtf8 . B.toStrict . Builder.toLazyByteString . Builder.byteStringHex

decodeHex :: Text -> Either String B.ByteString
decodeHex s
  | Just h <- Text.stripPrefix "0x" s = B16.decode $ Text.encodeUtf8 h
  | otherwise = Left "decodeHex: does not start with 0x"

decimalToWord :: Decimal -> Word256
decimalToWord d =
  let ethInWei = 1000000000000000000 -- 1e18
  in round $ d * ethInWei

wordToDecimal :: Word256 -> Decimal
wordToDecimal w =
  let ethInWei = 1000000000000000000 -- 1e18
  in fromRational (toInteger w % ethInWei)

keccak256ByteString :: B.ByteString -> B.ByteString
keccak256ByteString = BS.fromShort . _getBytesN . _getKeccak256Hash . keccak256
