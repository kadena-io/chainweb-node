{-# language
    ImportQualifiedPost
  , LambdaCase
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
#-}

module Chainweb.Pact5.SPV (pactSPV) where

import Chainweb.BlockHeader (BlockHeader, blockHeight)
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.BlockHeaderDB.HeaderOracle qualified as Oracle
import Chainweb.Payload (TransactionOutput(..))
import Chainweb.SPV (SpvException(..), TransactionOutputProof(..), outputProofChainId)
import Chainweb.SPV.VerifyProof (verifyTransactionOutputProof)
import Chainweb.Utils (decodeB64UrlNoPaddingText)
import Chainweb.Version qualified as CW
import Chainweb.Version.Guards qualified as CW
import Control.Lens
import Control.Monad (when)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash.Algorithms (SHA512t_256)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Pact.Core.Command.Types (CommandResult(..), PactResult(..))
import Pact.Core.DefPacts.Types (DefPactExec(..))
import Pact.Core.Hash (Hash(..))
import Pact.Core.PactValue (ObjectData(..), PactValue(..))
import Pact.Core.SPV (ContProof(..), SPVSupport(..))
import Pact.Core.StableEncoding (encodeStable)

pactSPV :: BlockHeaderDb -> BlockHeader -> SPVSupport
pactSPV bdb bh = SPVSupport
    { _spvSupport = \proofType proof -> verifySPV bdb bh proofType proof
    , _spvVerifyContinuation = \contProof -> verifyCont bdb bh contProof
    }

-- | Attempt to verify an SPV proof of a continuation given
--   a continuation payload object bytestring. On success, returns
--   the 'DefPactExec' associated with the proof.
verifyCont :: ()
    => BlockHeaderDb
    -> BlockHeader
    -> ContProof
    -> IO (Either Text DefPactExec)
verifyCont bdb bh (ContProof base64Proof) = runExceptT $ do
    oracle <- liftIO $ Oracle.createSpv bdb bh

    proofBytes <- case decodeB64UrlNoPaddingText (Text.decodeUtf8 base64Proof) of
        Left _ -> throwError "verifyCont: Invalid base64-encoded transaction output proof"
        Right bs -> return bs

    outputProof <- case Aeson.decodeStrict' @(TransactionOutputProof SHA512t_256) proofBytes of
        Nothing -> throwError "verifyCont: Cannot decode transaction output proof"
        Just u -> return u

    let cid = CW._chainId bdb

    when (view outputProofChainId outputProof /= cid) $
        throwError "verifyCont: cannot redeem continuation proof on wrong targget chain"

    -- ContProof verification is a 3-step process:
    --   1. Verify SPV TransactionOutput proof via Chainweb SPV API
    --   2. Decode tx outputs to 'CommandResult' 'Hash' _
    --   3. Extract continuation 'DefPactExec' from decoded result and return the cont exec object
    TransactionOutput proof <- catchAndDisplaySPVError bh $ liftIO $ verifyTransactionOutputProof oracle outputProof

    -- TODO: Do we care about the error type here?
    commandResult <- case Aeson.decodeStrict' @(CommandResult Hash Aeson.Value) proof of
        Nothing -> throwError "verifyCont: Unable to decode SPV transaction output"
        Just cr -> return cr

    case _crContinuation commandResult of
        Nothing -> throwError "verifyCont: No pact exec found in command result"
        Just defpactExec -> return defpactExec

verifySPV :: ()
    => BlockHeaderDb
    -> BlockHeader
    -> Text -- ^ ETH or TXOUT - defines the type of proof used in validation
    -> ObjectData PactValue
    -> IO (Either Text (ObjectData PactValue))
verifySPV bdb bh proofType proof = runExceptT $ do
    let cid = CW._chainId bdb

    case proofType of
        "ETH" -> do
            throwError "verifySPV: ETH proof type is not yet supported in Pact5."
        "TXOUT" -> do
            oracle <- liftIO $ Oracle.createSpv bdb bh

            outputProof <- case pactObjectOutputProof proof of
                Left err -> throwError err
                Right u -> return u

            when (view outputProofChainId outputProof /= cid) $
                throwError "verifySPV: cannot redeem spv proof on wrong target chain"

            -- SPV proof verification is a 3-step process:
            --   1. Verify SPV TransactionOutput proof via Chainweb SPV API
            --   2. Decode tx outputs to 'CommandResult' 'Hash' _
            --   3. Extract tx outputs as a pact object and return the object

            TransactionOutput rawCommandResult <- catchAndDisplaySPVError bh $ liftIO $ verifyTransactionOutputProof oracle outputProof

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

pactObjectOutputProof :: ObjectData PactValue -> Either Text (TransactionOutputProof SHA512t_256)
pactObjectOutputProof (ObjectData o) = do
    case Aeson.decodeStrict' @(TransactionOutputProof SHA512t_256) $ encodeStable o of
        Nothing -> Left "pactObjectOutputProof: Failed to decode proof object"
        Just outputProof -> Right outputProof

catchAndDisplaySPVError :: BlockHeader -> ExceptT Text IO a -> ExceptT Text IO a
catchAndDisplaySPVError bh eio =
  if CW.chainweb219Pact (CW._chainwebVersion bh) (CW._chainId bh) (view blockHeight bh)
  then catch eio $ \case
    SpvExceptionVerificationFailed m -> throwError ("spv verification failed: " <> m)
    spvErr -> throwM spvErr
  else eio
