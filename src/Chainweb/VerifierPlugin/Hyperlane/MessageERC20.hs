{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Chainweb.VerifierPlugin.Hyperlane.MessageERC20
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
--
-- Verifer plugin for Hyperlane Message with ERC20 token message.
-- Verifies the message using the provided metadata.
--
module Chainweb.VerifierPlugin.Hyperlane.MessageERC20 (plugin) where

import Control.Error
import Control.Monad (unless)
import Control.Monad.Except

import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Set as Set

import Ethereum.Misc hiding (Word256)

import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Capability

import Chainweb.Utils.Serialization (putRawByteString, runPutS, runGetS, putWord32be)

import Chainweb.VerifierPlugin
import Chainweb.VerifierPlugin.Hyperlane.Binary
import Chainweb.VerifierPlugin.Hyperlane.Utils
import Chainweb.Utils (decodeB64UrlNoPaddingText, sshow)

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \proof caps gasRef -> do
  -- extract capability values
  SigCapability{..} <- case Set.toList caps of
    [cap] -> return cap
    _ -> throwError $ VerifierError "Expected one capability."

  (capTokenMessage, capRecipient, capSigners) <- case _scArgs of
      [o, r, sigs] -> return (o, r, sigs)
      _ -> throwError $ VerifierError $ "Incorrect number of capability arguments. Expected: HyperlaneMessage object, recipient, signers."

  -- extract proof object values
  (hyperlaneMessageBase64, metadataBase64) <- case proof of
    PList values
      | [PLiteral (LString msg), PLiteral (LString mtdt)] <- V.toList values ->
        pure (msg, mtdt)
    _ -> throwError $ VerifierError "Expected a proof data as a list"

  (HyperlaneMessage{..}, hyperlaneMessageBinary) <- do
    chargeGas gasRef 5
    msg <- decodeB64UrlNoPaddingText hyperlaneMessageBase64
    decoded <- runGetS getHyperlaneMessage msg
    return (decoded, msg)

  MessageIdMultisigIsmMetadata{..} <- do
    chargeGas gasRef 5
    metadata <- decodeB64UrlNoPaddingText metadataBase64
    runGetS getMessageIdMultisigIsmMetadata metadata

  -- validate recipient
  let hmRecipientPactValue = PLiteral $ LString $ Text.decodeUtf8 hmRecipient
  unless (hmRecipientPactValue == capRecipient) $
    throwError $ VerifierError $
      "Recipients don't match. Expected: " <> sshow hmRecipientPactValue <> " but got " <> sshow capRecipient

  let
    TokenMessageERC20{..} = hmTokenMessage
    hmTokenMessagePactValue = PObject $ ObjectMap $ M.fromList
        [ ("recipient", PLiteral $ LString tmRecipient), ("amount", PLiteral $ LDecimal $ wordToDecimal tmAmount) ]

  unless (hmTokenMessagePactValue == capTokenMessage) $
    throwError $ VerifierError $
      "Invalid TokenMessage. Expected: " <> sshow hmTokenMessagePactValue <> " but got " <> sshow capTokenMessage

  -- validate signers
  let
    domainHash = keccak256ByteString $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putWord32be hmOriginDomain
      putRawByteString mmimOriginMerkleTreeAddress
      putRawByteString "HYPERLANE"

  let messageId = keccak256ByteString hyperlaneMessageBinary

  let
    digest = keccak256 $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString ethereumHeader
      putRawByteString $
        keccak256ByteString $ runPutS $ do
          putRawByteString domainHash
          putRawByteString mmimSignedCheckpointRoot
          putWord32be mmimSignedCheckpointIndex
          putRawByteString messageId

  addresses <- catMaybes <$> mapM (\sig -> chargeGas gasRef 16250 >> recoverAddress digest sig) mmimSignatures
  let addressesVals = PList $ V.fromList $ map (PLiteral . LString . encodeHex) addresses

  -- Note, that we check the signers for the full equality including their order and amount.
  -- Hyperlane's verifier uses a threshold and inclusion check.
  unless (addressesVals == capSigners) $
    throwError $ VerifierError $
      "Signers don't match. Expected: " <> sshow addressesVals <> " but got " <> sshow capSigners
