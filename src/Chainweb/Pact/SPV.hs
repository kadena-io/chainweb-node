{-# language
    ImportQualifiedPost
  , LambdaCase
  , OverloadedStrings
  , OverloadedRecordDot
  , ScopedTypeVariables
  , TypeApplications
#-}
{-# LANGUAGE FlexibleContexts #-}

module Chainweb.Pact.SPV (pactSPV) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Pact.Core.Command.Types (CommandResult(..), PactResult(..))
import Pact.Core.DefPacts.Types (DefPactExec(..))
import Pact.Core.Hash (Hash(..))
import Pact.Core.PactValue (ObjectData(..), PactValue(..))
import Pact.Core.SPV (SPVSupport(..), ContProof (..))
import Pact.Core.StableEncoding (encodeStable)

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Pact.Backend.Types
import Chainweb.Parent
import Chainweb.Pact.Payload(TransactionOutput(..))
import Chainweb.SPV (TransactionOutputProof(..), outputProofChainId)
import Chainweb.SPV.VerifyProof (runTransactionOutputProof, checkProofAndExtractOutput)
import Chainweb.Utils (decodeB64UrlNoPaddingText, unlessM)
import Chainweb.Version qualified as CW

pactSPV :: HeaderOracle -> SPVSupport
pactSPV oracle = SPVSupport
    { _spvSupport = \proofType proof -> verifySPV oracle proofType proof
    , _spvVerifyContinuation = \contProof -> verifyCont oracle contProof
    }

-- | Attempt to verify an SPV proof of a continuation given
--   a continuation payload object bytestring. On success, returns
--   the 'DefPactExec' associated with the proof.
verifyCont :: ()
    => HeaderOracle
    -> ContProof
    -> IO (Either Text DefPactExec)
verifyCont bdb (ContProof base64Proof) = runExceptT $ do
    proofBytes <- case decodeB64UrlNoPaddingText (Text.decodeUtf8 base64Proof) of
        Left _ -> throwError "verifyCont: Invalid base64-encoded transaction output proof"
        Right bs -> return bs

    outputProof <- case Aeson.decodeStrict' @(TransactionOutputProof ChainwebMerkleHashAlgorithm) proofBytes of
        Nothing -> throwError "verifyCont: Cannot decode transaction output proof"
        Just u -> return u

    let cid = CW._chainId bdb

    when (view outputProofChainId outputProof /= cid) $
        throwError "verifyCont: cannot redeem continuation proof on wrong targget chain"

    -- ContProof verification is a 3-step process:
    --   1. Verify SPV TransactionOutput proof via Chainweb SPV API
    --   2. Decode tx outputs to 'CommandResult' 'Hash' _
    --   3. Extract continuation 'DefPactExec' from decoded result and return the cont exec object
    TransactionOutput proof <- checkProofAndExtractOutput bdb outputProof

    -- TODO: Do we care about the error type here?
    commandResult <- case Aeson.decodeStrict' @(CommandResult Hash Aeson.Value) proof of
        Nothing -> throwError "verifyCont: Unable to decode SPV transaction output"
        Just cr -> return cr

    case _crContinuation commandResult of
        Nothing -> throwError "verifyCont: No pact exec found in command result"
        Just defpactExec -> return defpactExec

verifySPV :: ()
    => HeaderOracle
    -> Text -- ^ ETH or TXOUT - defines the type of proof used in validation
    -> ObjectData PactValue
    -> IO (Either Text (ObjectData PactValue))
verifySPV oracle proofType proof = runExceptT $ do
    case proofType of
        "ETH" -> do
            throwError "verifySPV: ETH proof type is not yet supported in Pact."
        "TXOUT" -> do
            outputProof <- case pactObjectOutputProof proof of
                Left err -> throwError err
                Right u -> return u

            when (view outputProofChainId outputProof /= oracle.chain) $
                throwError "verifySPV: cannot redeem spv proof on wrong target chain"

            -- SPV proof verification is a 3-step process:
            --   1. Verify SPV TransactionOutput proof via Chainweb SPV API
            --   2. Decode tx outputs to 'CommandResult' 'Hash' _
            --   3. Extract tx outputs as a pact object and return the object

            TransactionOutput rawCommandResult <-
              checkProofAndExtractOutput oracle outputProof

            commandResult <- case Aeson.decodeStrict' @(CommandResult Hash Aeson.Value) rawCommandResult of
                Nothing -> throwError "verifySPV: Unable to decode SPV transaction output"
                Just cr -> return cr

            case _crResult commandResult of
                PactResultErr _ -> do
                    throwError "verifySPV: Failed command result in tx output proof"
                PactResultOk pv -> do
                    case pv of
                        PObject o -> do
                            return (ObjectData o)
                        _ -> do
                            throwError "verifySPV: command result not an object"
        _ -> do
            throwError $ "Unsupported SPV type: " <> proofType

pactObjectOutputProof :: ObjectData PactValue -> Either Text (TransactionOutputProof ChainwebMerkleHashAlgorithm)
pactObjectOutputProof (ObjectData o) = do
    case Aeson.decodeStrict' @(TransactionOutputProof ChainwebMerkleHashAlgorithm) $ encodeStable o of
        Nothing -> Left "pactObjectOutputProof: Failed to decode proof object"
        Just outputProof -> Right outputProof
