{-# language
    ImportQualifiedPost
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
#-}

module Chainweb.Pact5.SPV (pactSPV) where

import Pact.Core.StableEncoding (StableEncoding(..), encodeStable)
import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.Bifunctor (first)
import Pact.Core.Names (Field(..))
import Pact.JSON.Encode qualified as J
import Pact.Core.PactValue (ObjectData(..), PactValue(..))
import Chainweb.BlockHeader (BlockHeader, _blockHash)
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.Payload (TransactionOutput(..))
import Chainweb.SPV (TransactionOutputProof(..), outputProofChainId)
import Chainweb.SPV.VerifyProof (verifyTransactionOutputProofAt_)
import Chainweb.Utils (decodeB64UrlNoPaddingText)
import Chainweb.Version qualified as CWVersion
import Control.Lens
import Control.Monad (when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash.Algorithms (SHA512t_256)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Pact.Core.Command.Types (CommandResult(..), PactResult(..))
import Pact.Core.DefPacts.Types (DefPactExec(..))
import Pact.Core.Hash (Hash(..))
import Pact.Core.SPV (ContProof(..), SPVSupport(..))

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
    proofBytes <- case decodeB64UrlNoPaddingText (Text.decodeUtf8 base64Proof) of
        Left _ -> throwError "verifyCont: Invalid base64-encoded transaction output proof"
        Right bs -> return bs

    outputProof <- case Aeson.decodeStrict' @(TransactionOutputProof SHA512t_256) proofBytes of
        Nothing -> throwError "verifyCont: Cannot decode transaction output proof"
        Just u -> return u

    let cid = CWVersion._chainId bdb

    when (view outputProofChainId outputProof /= cid) $
        throwError "verifyCont: cannot redeem continuation proof on wrong targget chain"

    -- ContProof verification is a 3-step process:
    --   1. Verify SPV TransactionOutput proof via Chainweb SPV API
    --   2. Decode tx outputs to 'CommandResult' 'Hash' _
    --   3. Extract continuation 'DefPactExec' from decoded result and return the cont exec object
    TransactionOutput proof <- liftIO $ verifyTransactionOutputProofAt_ bdb outputProof (_blockHash bh)

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
    let cid = CWVersion._chainId bdb

    case proofType of
        "ETH" -> do
            throwError "verifySPV: ETH proof type is not yet supported in Pact5."
        "TXOUT" -> do
            outputProof <- case pactObjectOutputProof proof of
                Left err -> throwError err
                Right u -> return u

            when (view outputProofChainId outputProof /= cid) $
                throwError "verifySPV: cannot redeem spv proof on wrong target chain"

            -- SPV proof verification is a 3-step process:
            --   1. Verify SPV TransactionOutput proof via Chainweb SPV API
            --   2. Decode tx outputs to 'CommandResult' 'Hash' _
            --   3. Extract tx outputs as a pact object and return the object

            TransactionOutput rawCommandResult <- liftIO $ verifyTransactionOutputProofAt_ bdb outputProof (_blockHash bh)

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
