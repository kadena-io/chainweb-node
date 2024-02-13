{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.VerifierPlugin.Hyperlane.Message (plugin) where

import Control.Error
import Control.Lens hiding (index)
import Control.Monad (unless)
import Control.Monad.Catch
import Control.Monad.Except

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Builder as Builder
import Data.Text (Text)
import Data.DoubleWord
import Data.Decimal
import Data.Ratio
import qualified Data.Map.Strict as M
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
import Chainweb.Utils.Serialization (putRawByteString, runPutS, runGetS, putWord32be)

import Chainweb.VerifierPlugin
import Chainweb.Utils (decodeB64UrlNoPaddingText)

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \vals caps gasRef -> do
  -- extract capability values
  let SigCapability{..} = Set.elemAt 0 caps
  (capTokenMessage, capRecipient, capSigners) <- case _scArgs of
      [o, r, sigs] -> return (o, r, sigs)
      _ -> throwError $ VerifierError $ "Not enough capability arguments. Expected: HyperlaneMessage object, recipient and signers."

  -- extract proof object values
  (encodedHyperlaneMessage, encodedMetadata) <- case vals !! 0 of
    (PList values)
      | (PLiteral (LString msg)) : (PLiteral (LString mtdt)) : _ <- V.toList values ->
        pure (msg, mtdt)
    _ -> throwError $ VerifierError "Expected a proof data as a list"

  (HyperlaneMessage{..}, hyperlaneMessageBinary) <- do
    chargeGas gasRef 5
    msg <- decodeB64UrlNoPaddingText encodedHyperlaneMessage
    decoded <- runGetS getHyperlaneMessage msg
    return (decoded, msg)

  MessageIdMultisigIsmMetadata{..} <- do
    chargeGas gasRef 5
    metadata <- decodeB64UrlNoPaddingText encodedMetadata
    runGetS getMessageIdMultisigIsmMetadata metadata

  -- validate recipient
  let recipientVal = PLiteral $ LString $ Text.decodeUtf8 hmRecipient
  unless (recipientVal == capRecipient) $
    throwError $ VerifierError $
      "Recipients don't match. Expected: " <> (Text.pack $ show recipientVal) <> " but got " <> (Text.pack $ show capRecipient)

  -- validate token message
  unless (_qnName _scName == "MRC20") $
    throwError $ VerifierError $ "Only TokenMessageERC20 is supported at the moment. Expected capability name: MRC20 but got " <> (_qnName _scName)

  let
    TokenMessageERC20{..} = hmTokenMessage
    tokenVal = PObject $ ObjectMap $ M.fromList
        [ ("recipient", PLiteral $ LString tmRecipient), ("amount", PLiteral $ LDecimal $ wordToDecimal tmAmount) ]

  unless (tokenVal == capTokenMessage) $
    throwError $ VerifierError $
      "Invalid TokenMessage. Expected: " <> (Text.pack $ show tokenVal) <> " but got " <> (Text.pack $ show capTokenMessage)

  -- validate signers
  let
    domainHash = getKeccak256Hash $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putWord32be hmOriginDomain
      putRawByteString mmimOriginMerkleTreeAddress
      putRawByteString "HYPERLANE"

  let messageId = getKeccak256Hash hyperlaneMessageBinary

  let
    hash' = getKeccak256Hash $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString domainHash
      putRawByteString mmimSignedCheckpointRoot
      putWord32be mmimSignedCheckpointIndex
      putRawByteString messageId
  let
    digest = keccak256 $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString ethereumHeader
      putRawByteString hash'
  addresses <- catMaybes <$> mapM (\sig -> chargeGas gasRef 16250 >> recoverAddress digest sig) mmimSignatures
  let addressesVals = PList $ V.fromList $ map (PLiteral . LString . encodeHex) addresses

  unless (addressesVals == capSigners) $
    throwError $ VerifierError $
      "Signers don't match. Expected: " <> (Text.pack $ show addressesVals) <> " but got " <> (Text.pack $ show capSigners)

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

-- | Header of the 32 bytes ethereum binary message.
ethereumHeader :: B.ByteString
ethereumHeader = "\x19" <> "Ethereum Signed Message:\n" <> "32"

encodeHex :: B.ByteString -> Text
encodeHex = ((<>) "0x") . Text.decodeUtf8 . B.toStrict . Builder.toLazyByteString . Builder.byteStringHex

wordToDecimal :: Word256 -> Decimal
wordToDecimal w =
  let ethInWei = 1000000000000000000 -- 1e18
  in fromRational (toInteger w % ethInWei)

getKeccak256Hash :: B.ByteString -> B.ByteString
getKeccak256Hash = BS.fromShort . _getBytesN . _getKeccak256Hash . keccak256
