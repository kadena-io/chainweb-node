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
-- Verifier plugin for Hyperlane Message.
-- Verifies the message using the provided Merkle Tree metadata.
--
module Chainweb.VerifierPlugin.Hyperlane.Message.After224 (runPlugin) where

import Control.Error
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.ST

import Data.Traversable (forM)
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
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

  (capMessageId, capMessage, capSigners, capThreshold) <- case _scArgs of
      [mid, mb, PList sigs, PLiteral (LInteger threshold)] -> do
        parsedSigners <- forM sigs $ \case
          (PLiteral (LString v)) -> pure v
          _ -> throwError $ VerifierError "Only string signers are supported"
        return (mid, mb, parsedSigners, fromIntegral threshold)
      _ -> throwError $ VerifierError $ "Incorrect number of capability arguments. Expected: messageId, message, signers, threshold."

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

  MerkleRootMultisigIsmMetadata{..} <- do
    chargeGas gasRef 5
    metadata <- decodeB64UrlNoPaddingText metadataBase64
    runGetS getMerkleRootMultisigIsmMetadata metadata

  -- validate messageId
  let
    messageId = keccak256ByteString hyperlaneMessageBinary
    messageIdPactValue = PLiteral $ LString $ encodeHex messageId

  unless (messageIdPactValue == capMessageId) $
    throwError $ VerifierError $
      "Invalid message id. Expected: " <> sshow messageIdPactValue <> " but got " <> sshow capMessageId

  -- validate message
  let
    hmVersionPactValue = PLiteral $ LInteger $ fromIntegral hmVersion
    hmNoncePactValue = PLiteral $ LInteger $ fromIntegral hmNonce
    hmOriginDomainPactValue = PLiteral $ LInteger $ fromIntegral hmOriginDomain
    hmDestinationDomainPactValue = PLiteral $ LInteger $ fromIntegral hmDestinationDomain
    hmSenderPactValue = PLiteral $ LString $ encodeB64UrlNoPaddingText hmSender
    hmRecipientPactValue = PLiteral $ LString $ encodeB64UrlNoPaddingText hmRecipient

    hmMessageBodyPactValue = PLiteral $ LString $ encodeB64UrlNoPaddingText hmMessageBody
    hmMessagePactValue = PObject . ObjectMap . M.fromList $
      [ ("version", hmVersionPactValue)
      , ("nonce", hmNoncePactValue)
      , ("originDomain", hmOriginDomainPactValue)
      , ("destinationDomain", hmDestinationDomainPactValue)
      , ("sender", hmSenderPactValue)
      , ("recipient", hmRecipientPactValue)
      , ("messageBody", hmMessageBodyPactValue)
      ]

  unless (hmMessagePactValue == capMessage) $
    throwError $ VerifierError $
      "Invalid message. Expected: " <> sshow hmMessagePactValue <> " but got " <> sshow capMessage

  -- validate signers
  let
    domainHash = keccak256ByteString $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putWord32be hmOriginDomain
      putRawByteString mrmimOriginMerkleTreeAddress
      putRawByteString "HYPERLANE"

  root <- do
    chargeGas gasRef 18 -- gas cost of the `branchRoot`
    return $ branchRoot messageId mrmimMerkleProof mrmimMessageIdIndex

  let
    digest = keccak256 $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString ethereumHeader
      putRawByteString $
        keccak256ByteString $ runPutS $ do
          putRawByteString domainHash
          putRawByteString root
          putWord32be mrmimSignedCheckpointIndex
          putRawByteString mrmimSignedCheckpointMessageId

  -- Requires that m-of-n validators verify a merkle root, and verifies a merkle proof of message against that root.
  --
  -- The signature addresses and validator addresses should be in the same order.
  --
  -- The original algorithm in hyperlane.
  -- https://github.com/hyperlane-xyz/hyperlane-monorepo/blob/v3/solidity/contracts/isms/multisig/AbstractMultisigIsm.sol#L67
  unless (capThreshold > 0) $ throwError $ VerifierError $ "Threshold should be greater than 0"

  unless (length mrmimSignatures >= capThreshold) $ throwError $ VerifierError $ "The number of signatures can't be less than threshold"

  let
    verify [] _ = pure ()
    verify (sig:sigs) validators =
      chargeGas gasRef 16250 >> recoverAddress digest sig >>= \case
        Just addr -> do
          case V.elemIndex (encodeHex addr) validators of
            Just i -> verify sigs (V.drop (i + 1) validators)
            Nothing -> throwError $ VerifierError "Verification failed"
        Nothing -> throwError $ VerifierError $ "Failed to recover an address: incorrect signature"

  verify (take capThreshold mrmimSignatures) capSigners
