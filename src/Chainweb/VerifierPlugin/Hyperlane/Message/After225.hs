{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Chainweb.VerifierPlugin.Hyperlane.Message
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
--
-- Verifier plugin for Hyperlane Message.
-- Verifies the message using the provided Merkle Tree metadata.
--
module Chainweb.VerifierPlugin.Hyperlane.Message.After225 (runPlugin) where

import Control.Lens
import Control.Error
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.Trans.Class

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
import Pact.Core.Errors (VerifierError(..))

evaluateST :: a -> ST s a
evaluateST a = unsafeIOToST (evaluate a)

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

  let
    parseInt k l = case l of
        LInteger i -> return i
        LDecimal d
          | (i, 0) <- properFraction d
          -> return i
        _ -> throwError $ VerifierError $ k <> " is not an integer"

  (capMessageId, capMessage, capSigners, capThreshold) <- case _scArgs of
    [mid, mb, PList sigs, PLiteral literalThreshold] -> do
      threshold <- parseInt "Threshold" literalThreshold
      parsedSigners <- forM sigs $ \case
        (PLiteral (LString v)) -> pure v
        _ -> throwError $ VerifierError "Only string signers are supported"

      parsedObject <-
        case mb of
          PObject (ObjectMap m) -> do
            let
              parseField k = case (m ^? at (FieldKey k) . _Just . _PLiteral) of
                  Just l -> PLiteral . LInteger <$> parseInt k l
                  _ -> throwError $ VerifierError $ k <> " is missing"

            version <- parseField "version"
            nonce <- parseField "nonce"
            origin <- parseField "originDomain"
            destination <- parseField "destinationDomain"

            return $ PObject $ ObjectMap $ m
              & at "version" .~ Just version
              & at "nonce" .~ Just nonce
              & at "originDomain" .~ Just origin
              & at "destinationDomain" .~ Just destination

          _ -> throwError $ VerifierError "Message should be an object"


      return (mid, parsedObject, parsedSigners, fromIntegral threshold)
    _ -> throwError $ VerifierError $ "Incorrect number of capability arguments. Expected: messageId, message, signers, threshold."

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

  -- validate messageId
  let
    messageId = keccak256ByteString hyperlaneMessageBinary
    messageIdPactValue = PLiteral $ LString $ encodeB64UrlNoPaddingText messageId

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

  chargeGas gasRef base64DecodeGasCost
  binMetadata <- maybe (throwError $ VerifierError "error decoding metadata from base64") return $
    decodeB64UrlNoPaddingText metadataBase64
  let
    useMessageIdMetadata = do
      MessageIdMultisigIsmMetadata{..} <- maybe
        (throwError $ VerifierError "error decoding metadata from bytes") return $
        runGetS getMessageIdMultisigIsmMetadata binMetadata
      let digestEnd = do
            putRawByteString mmimSignedCheckpointRoot
            putWord32be mmimSignedCheckpointIndex
            putRawByteString messageId
      return (mmimOriginMerkleTreeAddress, digestEnd, mmimSignatures)
  let
    useMerkleTreeMetadata = do
      MerkleRootMultisigIsmMetadata{..} <- maybe
        (throwError $ VerifierError "error decoding metadata from bytes") return $
        runGetS getMerkleRootMultisigIsmMetadata binMetadata
      chargeGas gasRef 18 -- gas cost of the `branchRoot`
      root <- lift $ evaluateST $ branchRoot messageId mrmimMerkleProof mrmimMessageIdIndex
      let digestEnd = do
            putRawByteString root
            putWord32be mrmimSignedCheckpointIndex
            putRawByteString mrmimSignedCheckpointMessageId
      return (mrmimOriginMerkleTreeAddress, digestEnd, mrmimSignatures)

  (originAddress, metadataDigestEnd, metadataSignatures) <-
    useMerkleTreeMetadata `catchError` \_ -> useMessageIdMetadata

  -- validate signers
  let
    domainHash = keccak256ByteString $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putWord32be hmOriginDomain
      putRawByteString originAddress
      putRawByteString "HYPERLANE"

  let
    digest = keccak256 $ runPutS $ do
      -- Corresponds to abi.encodePacked behaviour
      putRawByteString ethereumHeader
      putRawByteString $
        keccak256ByteString $ runPutS $ do
          putRawByteString domainHash
          metadataDigestEnd

  -- Requires that m-of-n validators verify a merkle root, and verifies a merkle proof of message against that root.
  --
  -- The signature addresses and validator addresses should be in the same order.
  --
  -- The original algorithm in hyperlane.
  -- https://github.com/hyperlane-xyz/hyperlane-monorepo/blob/v3/solidity/contracts/isms/multisig/AbstractMultisigIsm.sol#L67
  unless (capThreshold > 0) $ throwError $ VerifierError $ "Threshold should be greater than 0"

  unless (length metadataSignatures >= capThreshold) $ throwError $ VerifierError $ "The number of signatures can't be less than threshold"

  let
    verify [] _ = pure ()
    verify (sig:sigs) validators =
      -- gas cost of the address recovery
      chargeGas gasRef 16250 >> recoverAddress digest sig >>= \case
        Just addr -> do
          case V.elemIndex (encodeHex addr) validators of
            Just i -> verify sigs (V.drop (i + 1) validators)
            Nothing -> throwError $ VerifierError "Verification failed"
        Nothing -> throwError $ VerifierError $ "Failed to recover an address: incorrect signature"

  verify (take capThreshold metadataSignatures) capSigners
