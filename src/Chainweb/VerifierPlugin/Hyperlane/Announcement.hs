{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Chainweb.VerifierPlugin.Hyperlane.Announcement
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
--
-- Verifer plugin for Hyperlane Validator Announcement message
-- to verify that the validator is the one who signed the data.
--
module Chainweb.VerifierPlugin.Hyperlane.Announcement (plugin) where

import Control.Monad (unless)
import Control.Monad.Except

import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Set as Set

import Ethereum.Misc hiding (Word256)

import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Capability

import Chainweb.VerifierPlugin.Hyperlane.Utils
import Chainweb.Utils.Serialization (putRawByteString, runPutS, putWord32be)

import Chainweb.VerifierPlugin
import Chainweb.Utils (decodeB64UrlNoPaddingText, sshow)
import Pact.Core.Errors (VerifierError(..))

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \_ proof caps gasRef -> do
  -- extract capability values
  (capLocation, capSigner, capMailboxAddress) <- case Set.toList caps of
    [cap] -> case _scArgs cap of
      [location, sig, mailbox] -> return (location, sig, mailbox)
      _ -> throwError $ VerifierError "Incorrect number of capability arguments. Expected: storageLocation, signer."
    _ -> throwError $ VerifierError "Expected one capability."

  -- extract proof object values
  (storageLocationText, signatureBase64, mailboxAddressText) <- case proof of
    (PList values)
      | [PLiteral (LString loc), PLiteral (LString sig), PLiteral (LString mailbox)] <- V.toList values
      -> pure (loc, sig, mailbox)
    _ -> throwError $ VerifierError "Expected a proof data as a list"

  -- validate storage location
  let storageLocationPactValue = PLiteral $ LString storageLocationText
  unless (storageLocationPactValue == capLocation) $
    throwError $ VerifierError $
      "Incorrect storageLocation. Expected: " <> sshow storageLocationPactValue <> " but got " <> sshow capLocation

  -- validate mailbox address
  let mailboxAddressPactValue = PLiteral $ LString mailboxAddressText
  unless (mailboxAddressPactValue == capMailboxAddress) $
    throwError $ VerifierError $
      "Incorrect mailbox address. Expected: " <> sshow mailboxAddressPactValue <> " but got " <> sshow capMailboxAddress

  -- validate signer
  let
    -- Hyperlane's domain id for Kadena bridge,
    -- for more details see https://docs.hyperlane.xyz/docs/reference/domains
    kadenaDomainId = 626
    domainHash = keccak256ByteString $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putWord32be kadenaDomainId
      putRawByteString $ Text.encodeUtf8 mailboxAddressText
      putRawByteString "HYPERLANE_ANNOUNCEMENT"

  let
    announcementDigest = keccak256 $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString ethereumHeader
      putRawByteString $
        keccak256ByteString $ runPutS $ do
          putRawByteString domainHash
          putRawByteString $ Text.encodeUtf8 storageLocationText

  signatureBinary <- do
    chargeGas gasRef 5
    sig <- decodeB64UrlNoPaddingText signatureBase64
    return sig

  address <- chargeGas gasRef 16250 >> recoverAddress announcementDigest signatureBinary
  let addressPactValue = PLiteral . LString . encodeHex <$> address

  case addressPactValue of
    Just addressPactValue' ->
      unless (addressPactValue' == capSigner) $
        throwError $ VerifierError $
          "Incorrect signer. Expected: " <> sshow addressPactValue' <> " but got " <> sshow capSigner
    Nothing ->
        throwError $ VerifierError $ "Failed to recover the address from the signature"
