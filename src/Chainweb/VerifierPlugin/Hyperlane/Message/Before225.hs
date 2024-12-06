{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Chainweb.VerifierPlugin.Hyperlane.Message
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
--
-- Deprecated verifier plugin behaviour for Hyperlane Message.
-- Verifies the message using the provided Message Id metadata.
--
module Chainweb.VerifierPlugin.Hyperlane.Message.Before225 (runPlugin) where

import Control.Error
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.ST

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.STRef

import Ethereum.Misc hiding (Word256)

import Pact.Types.Runtime hiding (ChainId)
import Pact.Types.PactValue
import Pact.Types.Capability

import Chainweb.Utils.Serialization (putRawByteString, runPutS, runGetS, putWord32be)

import Chainweb.VerifierPlugin
import Chainweb.VerifierPlugin.Hyperlane.Binary
import Chainweb.VerifierPlugin.Hyperlane.Utils
import Chainweb.Utils (encodeB64UrlNoPaddingText, decodeB64UrlNoPaddingText, sshow)
import Pact.Core.Errors (VerifierError(..))

base64DecodeGasCost :: Gas
base64DecodeGasCost = 5

runPlugin :: forall s
        . PactValue
        -> Set.Set SigCapability
        -> STRef s Gas
        -> ExceptT VerifierError (ST s) ()
runPlugin proof caps gasRef = do
  -- extract capability values
  SigCapability{..} <- case Set.toList caps of
    [cap] -> return cap
    _ -> throwError $ VerifierError "Expected one capability."

  (capMessageBody, capRecipient, capSigners) <- case _scArgs of
      [mb, r, sigs] -> return (mb, r, sigs)
      _ -> throwError $ VerifierError $ "Incorrect number of capability arguments. Expected: messageBody, recipient, signers."

  -- extract proof object values
  (hyperlaneMessageBase64, metadataBase64) <- case proof of
    PList values
      | [PLiteral (LString msg), PLiteral (LString mtdt)] <- V.toList values ->
        pure (msg, mtdt)
    _ -> throwError $ VerifierError "Expected a proof data as a list"

  (HyperlaneMessage{..}, hyperlaneMessageBinary) <- do
    chargeGas gasRef base64DecodeGasCost
    msg <- decodeB64UrlNoPaddingText hyperlaneMessageBase64
    decoded <- runGetS getHyperlaneMessage msg
    return (decoded, msg)

  MessageIdMultisigIsmMetadata{..} <- do
    chargeGas gasRef base64DecodeGasCost
    metadata <- decodeB64UrlNoPaddingText metadataBase64
    runGetS getMessageIdMultisigIsmMetadata metadata

  -- validate recipient
  let hmRecipientPactValue = PLiteral $ LString $ Text.decodeUtf8 $ BS.dropWhile (== 0) hmRecipient
  unless (hmRecipientPactValue == capRecipient) $
    throwError $ VerifierError $
      "Recipients don't match. Expected: " <> sshow hmRecipientPactValue <> " but got " <> sshow capRecipient

  let
    hmMessageBodyPactValue = PLiteral $ LString $ encodeB64UrlNoPaddingText hmMessageBody

  unless (hmMessageBodyPactValue == capMessageBody) $
    throwError $ VerifierError $
      "Invalid TokenMessage. Expected: " <> sshow hmMessageBodyPactValue <> " but got " <> sshow capMessageBody

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

  -- 16250 is a gas cost of the address recovery
  addresses <- catMaybes <$> mapM (\sig -> chargeGas gasRef 16250 >> recoverAddress digest sig) mmimSignatures
  let addressesVals = PList $ V.fromList $ map (PLiteral . LString . encodeHex) addresses

  -- Note, that we check the signers for the full equality including their order and amount.
  -- Hyperlane's ISM uses a threshold and inclusion check.
  unless (addressesVals == capSigners) $
    throwError $ VerifierError $
      "Signers don't match. Expected: " <> sshow addressesVals <> " but got " <> sshow capSigners
