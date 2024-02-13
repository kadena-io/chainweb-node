{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.VerifierPlugin.Hyperlane.Announcement (plugin) where

import Control.Lens hiding (index)
import Control.Monad (unless)
import Control.Monad.Catch
import Control.Monad.Except

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Builder as Builder
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Crypto.Secp256k1 as ECDSA

import Ethereum.Misc hiding (Word256)

import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Capability

import Chainweb.Pact.SPV.Hyperlane.Binary
import Chainweb.Utils.Serialization (putRawByteString, runPutS)

import Chainweb.VerifierPlugin
import Chainweb.Utils (decodeB64UrlNoPaddingText)

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \vals caps gasRef -> do
  -- extract capability values
  let SigCapability{..} = Set.elemAt 0 caps
  capSigner <- case _scArgs of
      [sig] -> return sig
      _ -> throwError $ VerifierError $ "Not enough capability arguments. Expected: signer."

  -- extract proof object values
  (storageLocation, encodedSignature) <- case vals !! 0 of
    (PList values)
      | (PLiteral (LString loc)) : (PLiteral (LString sig)) : _ <- V.toList values ->
        pure (loc, sig)
    _ -> throwError $ VerifierError "Expected a proof data as a list"

  signatureBinary <- do
    chargeGas gasRef 5
    sig <- decodeB64UrlNoPaddingText encodedSignature
    return sig

  let
    -- This is a kadena's domain hash calculated in Solidity as
    -- keccak256(abi.encodePacked(626, "kb-mailbox", "HYPERLANE_ANNOUNCEMENT"))
    domainHashHex :: Text
    domainHashHex = "0xa69e6ef1a8e62aa6b513bd7d694c6d237164fb04df4e5fb4106e47bf5b5a0428"

  domainHash <- case decodeHex domainHashHex of
          Right s -> pure s
          Left e -> throwError $ VerifierError $ Text.pack $ "Decoding of domainHashHex failed: " ++ e

  -- validate signer
  let
    hash' = getKeccak256Hash $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString domainHash
      putRawByteString $ Text.encodeUtf8 storageLocation

  let
    announcementDigest = keccak256 $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString ethereumHeader
      putRawByteString hash'

  address <- chargeGas gasRef 16250 >> recoverAddress announcementDigest signatureBinary
  let addressVal = PLiteral . LString . encodeHex <$> address

  case addressVal of
    Just addressVal' ->
      unless (addressVal' == capSigner) $
        throwError $ VerifierError $
          "Incorrect signer. Expected: " <> (Text.pack $ show addressVal) <> " but got " <> (Text.pack $ show capSigner)
    Nothing ->
        throwError $ VerifierError $ "Failed to recover the address from the signature"

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

  addr <- ecrecover sig'
  pure addr

-- | Returns an address, a rightmost 160 bits (20 bytes) of the keccak hash of the public key.
getAddress :: ECDSA.EcdsaPublicKey -> B.ByteString
getAddress pubkey = B.takeEnd ethereumAddressSize
    $ getKeccak256Hash
    $ BS.fromShort
    $ BS.drop 1 -- drop the first 0x04 byte the indicates that the key is encoded in compressed format
    $ ECDSA.ecdsaPublicKeyBytes pubkey

encodeHex :: B.ByteString -> Text
encodeHex = ((<>) "0x") . Text.decodeUtf8 . B.toStrict . Builder.toLazyByteString . Builder.byteStringHex

decodeHex :: Text -> Either String B.ByteString
decodeHex s
  | Just h <- Text.stripPrefix "0x" s = B16.decode $ Text.encodeUtf8 h
  | otherwise = Left "decodeHex: does not start with 0x"

-- | Header of the 32 bytes ethereum binary message.
ethereumHeader :: B.ByteString
ethereumHeader = "\x19" <> "Ethereum Signed Message:\n" <> "32"

getKeccak256Hash :: B.ByteString -> B.ByteString
getKeccak256Hash = BS.fromShort . _getBytesN . _getKeccak256Hash . keccak256
