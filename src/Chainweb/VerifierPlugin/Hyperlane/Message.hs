{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.VerifierPlugin.Hyperlane.Message (plugin) where

import Control.Error
import Control.Monad (unless)
import Control.Monad.Except

import qualified Data.Map.Strict as M
import qualified Data.Text as Text
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
import Chainweb.Utils (decodeB64UrlNoPaddingText)

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \proof caps gasRef -> do
  -- extract capability values
  let SigCapability{..} = Set.elemAt 0 caps
  (capTokenMessage, capRecipient, capSigners) <- case _scArgs of
      [o, r, sigs] -> return (o, r, sigs)
      _ -> throwError $ VerifierError $ "Not enough capability arguments. Expected: HyperlaneMessage object, recipient and signers."

  -- extract proof object values
  (encodedHyperlaneMessage, encodedMetadata) <- case proof of
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
