{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact Service SPV support
--
module Chainweb.Pact.SPV
( -- * spv support
  pactSPV
, verifySPV
, verifyCont
  -- * spv api utilities
, getTxIdx
) where


import GHC.Stack

import Control.Error
import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Except

import Data.Aeson hiding (Object, (.=))
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64U
import Data.Default (def)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.Read (readMaybe)

import Crypto.Hash.Algorithms

import Ethereum.Header as EthHeader
import Ethereum.Misc
import Ethereum.Receipt
import Ethereum.Receipt.ReceiptProof
import Ethereum.RLP

import Numeric.Natural

import qualified Streaming.Prelude as S

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.Pact.Service.Types(internalError)
import Chainweb.Pact.Utils (aeson)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.VerifyProof
import Chainweb.TreeDB
import Chainweb.Utils
import qualified Chainweb.Version as CW
import qualified Chainweb.Version.Guards as CW

import Chainweb.Storage.Table


-- internal pact modules

import qualified Pact.JSON.Encode as J
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Runtime
import Pact.Types.SPV

catchAndDisplaySPVError :: BlockHeader -> ExceptT Text IO a -> ExceptT Text IO a
catchAndDisplaySPVError bh =
  if CW.chainweb219Pact (CW._chainwebVersion bh) (_blockChainId bh) (_blockHeight bh)
  then flip catch $ \case
    SpvExceptionVerificationFailed m -> throwError ("spv verification failed: " <> m)
    spvErr -> throwM spvErr
  else id

forkedThrower :: BlockHeader -> Text -> ExceptT Text IO a
forkedThrower bh =
  if CW.chainweb219Pact (CW._chainwebVersion bh) (_blockChainId bh) (_blockHeight bh)
  then throwError
  else internalError

-- | Spv support for pact
--
pactSPV
    :: BlockHeaderDb
      -- ^ handle into the cutdb
    -> BlockHeader
      -- ^ the context for verifying the proof
    -> SPVSupport
pactSPV bdb bh = SPVSupport (verifySPV bdb bh) (verifyCont bdb bh)

-- | SPV transaction verification support. Calls to 'verify-spv' in Pact
-- will thread through this function and verify an SPV receipt, making the
-- requisite calls to the SPV api and verifying the output proof.
--
verifySPV
    :: BlockHeaderDb
      -- ^ handle into the cut db
    -> BlockHeader
        -- ^ the context for verifying the proof
    -> Text
      -- ^ TXOUT or TXIN - defines the type of proof
      -- used in validation
    -> Object Name
      -- ^ the proof object to validate
    -> IO (Either Text (Object Name))
verifySPV bdb bh typ proof = runExceptT $ go typ proof
  where
    cid = CW._chainId bdb
    enableBridge = CW.enableSPVBridge (CW._chainwebVersion bh) cid (_blockHeight bh)

    mkSPVResult' cr j
        | enableBridge =
          return $ mkSPVResult cr j
        | otherwise = case fromPactValue j of
            TObject o _ -> return o
            _ -> throwError "spv-verified tx output has invalid type"

    go s o = case s of

      -- Ethereum Receipt Proof
      "ETH" | enableBridge -> except (extractEthProof o) >>=
        \parsedProof -> case validateReceiptProof parsedProof of
          Left e -> throwError $ "Validation of Eth proof failed: " <> sshow e
          Right result -> return $ ethResultToPactValue result

      "HYPERLANE_V3" | enableBridge -> except (extractEthProof o) >>=
        \parsedProof -> case validateReceiptProof parsedProof of
          Left e -> throwError $ "Validation of Hyperlane proof failed: " <> sshow e
          Right result -> return $ ethResultToPactValue result

      -- Chainweb tx output proof
      "TXOUT" -> do
        u <- except $ extractProof enableBridge o
        unless (view outputProofChainId u == cid) $
          forkedThrower bh "cannot redeem spv proof on wrong target chain"

        -- SPV proof verification is a 3 step process:
        --
        --  1. verify spv tx output proof via chainweb spv api
        --
        --  2. Decode tx outputs to 'HashCommandResult'
        --
        --  3. Extract tx outputs as a pact object and return the
        --  object.

        TransactionOutput p <- catchAndDisplaySPVError bh $ liftIO $ verifyTransactionOutputProofAt_ bdb u (_blockHash bh)

        q <- case decodeStrict' p :: Maybe (CommandResult Hash) of
          Nothing -> forkedThrower bh "unable to decode spv transaction output"
          Just cr -> return cr

        case _crResult q of
          PactResult Left{} ->
            throwError "Failed command result in tx output proof"
          PactResult (Right v) ->
            mkSPVResult' q v

      t -> throwError $! "unsupported SPV types: " <> t

-- | A tag for specifying the format of base64 error messages on chain.
--
--   `Legacy` errors match those produced by our legacy version of
--   base64-bytestring, and are produced by parsing the error messages
--   we receive from our current version of base64-bytestring (1.2),
--   and formatting them in the older style. Legacy behavior also implies
--   that non-canonical encodings be allowed, because that was the behavior
--   of the legacy bytestring parser; and improperly padded messages
--   will get extra padding, because that is the legacy chainweb behavior.
--
--   `Simplified` errors are a static string, which may not describe
--   the issue with the base64-encoded string as well, but will be
--   more stable as we upgrade base64 decoding libraries in the future.
--   In the `Simplified` errors setting, messages rejected for using
--   non-canonical encodings will remain as errors, because we want to
--   begin enforcing the expected constraints on base64-encoded messages.
--   Similarly, Simplified parsing will not add extra padding to improperly
--   padded messages.
data GenerateBase64ErrorMessage
  = Legacy
  | Simplified
  deriving (Eq, Show)


-- | A modified version of `decodeBase64UrlNoPaddingText` that emits
--   base64 decoding errors in a configurable way.
decodeB64UrlNoPaddingTextWithFixedErrorMessage :: MonadThrow m => GenerateBase64ErrorMessage -> Text.Text -> m B.ByteString
decodeB64UrlNoPaddingTextWithFixedErrorMessage errorMessageType msg =
  fromEitherM
  . first (\e -> Base64DecodeException $ base64ErrorMessage e)
  . (if errorMessageType == Legacy then patchNonCanonical else id)
  . B64U.decode
  . Text.encodeUtf8
  . (if errorMessageType == Legacy then pad else id)
  $ msg
  where
    pad t = let s = Text.length t `mod` 4 in t <> Text.replicate ((4 - s) `mod` 4) "="
    base64ErrorMessage m = case errorMessageType of
      Legacy -> base64DowngradeErrorMessage (Text.pack m)
      Simplified -> "could not base64-decode message"
    patchNonCanonical decodeResult = case decodeResult of
      Right bs -> Right bs
      Left e | "non-canonical" `Text.isInfixOf` Text.pack e ->
               (B64U.decodeNonCanonical (Text.encodeUtf8 msg))
      Left e -> Left e
{-# INLINE decodeB64UrlNoPaddingTextWithFixedErrorMessage #-}



-- | Converts the error message format of base64-bytestring-1.2
--   into that of base64-bytestring-0.1, for the error messages
--   that have made it onto the chain.
--   This allows us to upgrade to base64-bytestring-1.2 without
--   breaking compatibility.
base64DowngradeErrorMessage :: Text -> Text
base64DowngradeErrorMessage msg = case msg of
  "Base64-encoded bytestring has invalid size" ->
       "invalid base64 encoding near offset 0"
  (Text.stripPrefix "invalid character at offset: " -> Just suffix) ->
    Text.pack $ "invalid base64 encoding near offset " ++ show (adjustedOffset suffix)
  (Text.stripPrefix "invalid padding at offset: " -> Just suffix) ->
    Text.pack $ "invalid padding near offset " ++ show (adjustedOffset suffix)
  e -> e
  where
    adjustedOffset :: Text -> Int
    adjustedOffset suffix = case readMaybe (Text.unpack suffix) of
      Nothing -> 0
      Just offset -> let
        endsWithThreeEquals = Text.drop (Text.length msg - 3) msg == "==="
        adjustment = if endsWithThreeEquals then -1 else 0
        in
        offset - (offset `rem` 4) + adjustment

-- | SPV defpact transaction verification support. This call validates a pact 'endorsement'
-- in Pact, providing a validation that the yield data of a cross-chain pact is valid.
--
verifyCont
    :: BlockHeaderDb
      -- ^ handle into the cut db
    -> BlockHeader
        -- ^ the context for verifying the proof
    -> ContProof
      -- ^ bytestring of 'TransactionOutputP roof' object to validate
    -> IO (Either Text PactExec)
verifyCont bdb bh (ContProof cp) = runExceptT $ do
    let errorMessageType =
          if CW.chainweb221Pact
             (CW._chainwebVersion bh)
             (_blockChainId bh)
             (_blockHeight bh)
          then Simplified
          else Legacy
    t <- decodeB64UrlNoPaddingTextWithFixedErrorMessage errorMessageType $ Text.decodeUtf8 cp
    case decodeStrict' t of
      Nothing -> forkedThrower bh "unable to decode continuation proof"
      Just u
        | view outputProofChainId u /= cid ->
          forkedThrower bh "cannot redeem continuation proof on wrong target chain"
        | otherwise -> do

          -- Cont proof verification is a 3 step process:
          --
          --  1. verify spv tx output proof via chainweb spv api
          --
          --  2. Decode tx outputs to 'HashCommandResult'
          --
          --  3. Extract continuation 'PactExec' from decoded result
          --  and return the cont exec object

          TransactionOutput p <- catchAndDisplaySPVError bh $ liftIO $ verifyTransactionOutputProofAt_ bdb u (_blockHash bh)

          q <- case decodeStrict' p :: Maybe (CommandResult Hash) of
            Nothing -> forkedThrower bh "unable to decode spv transaction output"
            Just cr -> return cr

          case _crContinuation q of
            Nothing -> throwError "no pact exec found in command result"
            Just pe -> return pe
  where
    cid = CW._chainId bdb

-- | Extract a 'TransactionOutputProof' from a generic pact object
--
extractProof :: Bool -> Object Name -> Either Text (TransactionOutputProof SHA512t_256)
extractProof False o = toPactValue (TObject o def) >>= k
  where
    k = aeson (Left . pack) Right
      . fromJSON
      . J.toJsonViaEncode
extractProof True (Object (ObjectMap o) _ _ _) = case M.lookup "proof" o of
  Just (TLitString proof) -> do
    j <- first (const "Base64 decode failed") (decodeB64UrlNoPaddingText proof)
    first (const "Decode of TransactionOutputProof failed") (decodeStrictOrThrow j)
  _ -> Left "Invalid input, expected 'proof' field with base64url unpadded text"

-- | Extract an Eth 'ReceiptProof' from a generic pact object
--
-- The proof object has a sinle property "proof". The value is the
-- base64UrlWithoutPadding encoded proof blob.
--
-- NOTE: If this fails the failure message is included on the chain. We
-- therefore replace failure and exception messages from external libraries with
-- stable internal messages.
--
-- For details of the returned value see 'Ethereum.Receipt'
--
extractEthProof :: Object Name -> Either Text ReceiptProof
extractEthProof o = case M.lookup "proof" $ _objectMap $ _oObject o of
  Nothing -> Left "Decoding of Eth proof object failed: missing 'proof' property"
  Just (TLitString p) -> do
    bytes' <- errMsg "Decoding of Eth proof object failed: invalid base64URLWithoutPadding encoding"
        $ decodeB64UrlNoPaddingText p
    errMsg "Decoding of Eth proof object failed: invalid binary proof data"
        $ get getRlp bytes'
  Just _ -> Left "Decoding of Eth proof object failed: invalid 'proof' property"
  where
    errMsg t = first (const t)

ethResultToPactValue :: ReceiptProofValidation -> Object Name
ethResultToPactValue ReceiptProofValidation{..} = mkObject
    [ ("depth", tInt _receiptProofValidationDepth)
    , ("header", header _receiptProofValidationHeader)
    , ("index", tix _receiptProofValidationIndex)
    , ("root",jsonStr _receiptProofValidationRoot)
    , ("weight",tInt _receiptProofValidationWeight)
    , ("receipt",receipt _receiptProofValidationReceipt)
    ]
  where
    receipt Receipt{..} = obj
      [ ("cumulative-gas-used", tInt _receiptGasUsed)
      , ("status",toTerm $ _receiptStatus == TxStatus 1)
      , ("logs",toTList TyAny def $ map rlog _receiptLogs)]
    rlog LogEntry{..} = obj
      [ ("address",jsonStr _logEntryAddress)
      , ("topics",toTList TyAny def $ map topic _logEntryTopics)
      , ("data",jsonStr _logEntryData)]
    topic t = jsonStr t
    header ch@ConsensusHeader{..} = obj
      [ ("difficulty", jsonStr _hdrDifficulty)
      , ("extra-data", jsonStr _hdrExtraData)
      , ("gas-limit", tInt _hdrGasLimit)
      , ("gas-used", tInt _hdrGasUsed)
      , ("hash", jsonStr $ EthHeader.blockHash ch)
      , ("miner", jsonStr _hdrBeneficiary)
      , ("mix-hash", jsonStr _hdrMixHash)
      , ("nonce", jsonStr _hdrNonce)
      , ("number", tInt _hdrNumber)
      , ("parent-hash", jsonStr _hdrParentHash)
      , ("receipts-root", jsonStr _hdrReceiptsRoot)
      , ("sha3-uncles", jsonStr _hdrOmmersHash)
      , ("state-root", jsonStr _hdrStateRoot)
      , ("timestamp", ts _hdrTimestamp)
      , ("transactions-root", jsonStr _hdrTransactionsRoot)
      ]
    jsonStr v = case toJSON v of
      String s -> tStr s
      _ -> tStr $ sshow v
    ts (Timestamp t) = tInt t
    tix (TransactionIndex i) = tInt i
{-# INLINE ethResultToPactValue #-}

-- | Look up pact tx hash at some block height in the
-- payload db, and return the tx index for proof creation.
--
-- Note: runs in O(n) - this should be revisited if possible
--
getTxIdx
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> BlockHeight
    -> PactHash
    -> IO (Either Text Int)
getTxIdx bdb pdb bh th = do
    -- get BlockPayloadHash
    m <- maxEntry bdb
    ph <- seekAncestor bdb m (int bh) >>= \case
        Just x -> return $ Right $! _blockPayloadHash x
        Nothing -> return $ Left "unable to find payload associated with transaction hash"

    case ph of
      (Left !s) -> return $ Left s
      (Right !a) -> do
        -- get payload
        payload <- _payloadWithOutputsTransactions <$> casLookupM pdb a

        -- Find transaction index
        r <- S.each payload
          & S.map fst
          & S.mapM toTxHash
          & sindex (== th)

        r & note "unable to find transaction at the given block height"
          & fmap int
          & return
  where
    toPactTx :: MonadThrow m => Transaction -> m (Command Text)
    toPactTx (Transaction b) = decodeStrictOrThrow' b

    toTxHash :: MonadThrow m => Transaction -> m PactHash
    toTxHash = fmap _cmdHash . toPactTx

    sfind :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe a)
    sfind p = S.head_ . S.dropWhile (not . p)

    sindex :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe Natural)
    sindex p s = S.zip (S.each [0..]) s & sfind (p . snd) & fmap (fmap fst)

mkObject :: [(FieldKey, Term n)] -> Object n
mkObject ps = Object (ObjectMap (M.fromList ps)) TyAny Nothing def

obj :: [(FieldKey, Term n)] -> Term n
obj = toTObject TyAny def

tInt :: Integral i => i -> Term Name
tInt = toTerm . fromIntegral @_ @Integer

-- | Encode a "successful" CommandResult into a Pact object.
mkSPVResult
    :: CommandResult Hash
       -- ^ Full CR
    -> PactValue
       -- ^ Success result
    -> Object Name
mkSPVResult CommandResult{..} j =
    mkObject
    [ ("result", fromPactValue j)
    , ("req-key", tStr $ asString $ unRequestKey _crReqKey)
    , ("txid", tStr $ maybe "" asString _crTxId)
    , ("gas", toTerm $ (fromIntegral _crGas :: Integer))
    , ("meta", maybe empty metaField _crMetaData)
    , ("logs", tStr $ asString _crLogs)
    , ("continuation", maybe empty contField _crContinuation)
    , ("events", toTList TyAny def $ map eventField _crEvents)
    ]
  where
    metaField v = case fromJSON v of
      Error _ -> obj []
      Success p -> fromPactValue p

    contField (PactExec stepCount yield executed step pactId pactCont rollback _nested) = obj
        [ ("step", toTerm step)
        , ("step-count", toTerm stepCount)
        , ("yield", maybe empty yieldField yield)
        , ("pact-id", toTerm pactId)
        , ("cont",contField1 pactCont)
        , ("step-has-rollback",toTerm rollback)
        , ("executed",tStr $ maybe "" sshow executed)
        ]

    contField1 PactContinuation {..} = obj
        [ ("name",tStr $ asString _pcDef)
        , ("args",toTList TyAny def $ map fromPactValue _pcArgs)
        ]

    yieldField Yield {..} = obj
        [ ("data",fromPactValue (PObject _yData))
        , ("provenance", maybe empty provField _yProvenance)
        ]

    provField Provenance {..} = obj
        [ ("target-chain", toTerm $ _chainId _pTargetChainId)
        , ("module-hash", tStr $ asString $ _mhHash $ _pModuleHash)
        ]

    eventField PactEvent {..} = obj
        [ ("name", toTerm _eventName)
        , ("params", toTList TyAny def (map fromPactValue _eventParams))
        , ("module", tStr $ asString _eventModule)
        , ("module-hash", tStr $ asString _eventModuleHash)
        ]

    empty = obj []
