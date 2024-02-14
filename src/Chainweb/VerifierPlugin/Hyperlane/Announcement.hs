{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.VerifierPlugin.Hyperlane.Announcement (plugin) where

import Control.Monad (unless)
import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Set as Set

import Ethereum.Misc hiding (Word256)

import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Capability

import Chainweb.VerifierPlugin.Hyperlane.Utils
import Chainweb.Utils.Serialization (putRawByteString, runPutS)

import Chainweb.VerifierPlugin
import Chainweb.Utils (decodeB64UrlNoPaddingText)

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \proof caps gasRef -> do
  -- extract capability values
  let SigCapability{..} = Set.elemAt 0 caps
  capSigner <- case _scArgs of
      [sig] -> return sig
      _ -> throwError $ VerifierError $ "Not enough capability arguments. Expected: signer."

  -- extract proof object values
  (storageLocation, encodedSignature) <- case proof of
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
