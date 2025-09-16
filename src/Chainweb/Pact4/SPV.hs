{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Pact4.SPV
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact Service SPV support
--
module Chainweb.Pact4.SPV
( -- * spv support
  pactSPV
, verifySPV
, verifyCont
  -- * spv api utilities
, getTxIdx
) where

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.HeaderOracle qualified as Oracle
import Chainweb.BlockHeight
import Chainweb.Pact.Types(internalError)
import Chainweb.Pact.Utils (aeson)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.VerifyProof
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version qualified as CW
import Chainweb.Version.Guards qualified as CW
import Control.Error
import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Crypto.Hash.Algorithms
import Data.Aeson hiding (Object, (.=))
import Data.Bifunctor
import Data.ByteString qualified as B
import Data.ByteString.Base64.URL qualified as B64U
import Data.Map.Strict qualified as M
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Ethereum.Header qualified as EthHeader
import Ethereum.Misc
import Ethereum.RLP
import Ethereum.Receipt
import Ethereum.Receipt.ReceiptProof
import GHC.Stack
import Numeric.Natural
import Pact.JSON.Encode qualified as J
import Pact.Types.Command qualified as Pact4
import Pact.Types.Hash qualified as Pact4
import Pact.Types.Info qualified as Pact4
import Pact.Types.PactValue qualified as Pact4
import Pact.Types.Runtime qualified as Pact4
import Pact.Types.SPV qualified as Pact4
import Streaming.Prelude qualified as S
import Text.Read (readMaybe)

catchAndDisplaySPVError :: BlockHeader -> ExceptT Text IO a -> ExceptT Text IO a
catchAndDisplaySPVError bh =
  if CW.chainweb219Pact (CW._chainwebVersion bh) (CW._chainId bh) (view blockHeight bh)
  then flip catch $ \case
    SpvExceptionVerificationFailed m -> throwError ("spv verification failed: " <> m)
    spvErr -> throwM spvErr
  else id

forkedThrower :: BlockHeader -> Text -> ExceptT Text IO a
forkedThrower bh =
  if CW.chainweb219Pact (CW._chainwebVersion bh) (CW._chainId bh) (view blockHeight bh)
  then throwError
  else internalError

-- | Spv support for pact
--
pactSPV
    :: BlockHeaderDb
      -- ^ handle into the cutdb
    -> BlockHeader
      -- ^ the context for verifying the proof
    -> Pact4.SPVSupport
pactSPV bdb bh = Pact4.SPVSupport (verifySPV bdb bh) (verifyCont bdb bh)

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
    -> Pact4.Object Pact4.Name
      -- ^ the proof object to validate
    -> IO (Either Text (Pact4.Object Pact4.Name))
verifySPV bdb bh typ proof = do
  oracle <- Oracle.createSpv bdb bh
  runExceptT $ go oracle typ proof
  where
    cid = CW._chainId bdb
    enableBridge = CW.enableSPVBridge (CW._chainwebVersion bh) cid (view blockHeight bh)

    mkSPVResult' cr j
        | enableBridge =
          return $ mkSPVResult cr j
        | otherwise = case Pact4.fromPactValue j of
            Pact4.TObject o _ -> return o
            _ -> throwError "spv-verified tx output has invalid type"

    go oracle s o = case s of

      -- Ethereum Receipt Proof
      "ETH" | enableBridge -> except (extractEthProof o) >>=
        \parsedProof -> case validateReceiptProof parsedProof of
          Left e -> throwError $ "Validation of Eth proof failed: " <> sshow e
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

        TransactionOutput p <- catchAndDisplaySPVError bh $ Pact4.liftIO $ verifyTransactionOutputProof oracle u

        q <- case decodeStrict' p :: Maybe (Pact4.CommandResult Pact4.Hash) of
          Nothing -> forkedThrower bh "unable to decode spv transaction output"
          Just cr -> return cr

        case Pact4._crResult q of
          Pact4.PactResult Left{} ->
            throwError "Failed command result in tx output proof"
          Pact4.PactResult (Right v) ->
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
    -> Pact4.ContProof
      -- ^ bytestring of 'TransactionOutputP roof' object to validate
    -> IO (Either Text Pact4.PactExec)
verifyCont bdb bh (Pact4.ContProof cp) = runExceptT $ do
    oracle <- liftIO $ Oracle.createSpv bdb bh
    let errorMessageType =
          if CW.chainweb221Pact
             (CW._chainwebVersion bh)
             (CW._chainId bh)
             (view blockHeight bh)
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
          --  2. Decode tx outputs to 'Pact4.CommandResult' 'Pact4.Hash'
          --
          --  3. Extract continuation 'PactExec' from decoded result
          --  and return the cont exec object

          TransactionOutput p <- catchAndDisplaySPVError bh $ Pact4.liftIO $ verifyTransactionOutputProof oracle u

          q <- case decodeStrict' p :: Maybe (Pact4.CommandResult Pact4.Hash) of
            Nothing -> forkedThrower bh "unable to decode spv transaction output"
            Just cr -> return cr

          case Pact4._crContinuation q of
            Nothing -> throwError "no pact exec found in command result"
            Just pe -> return pe
  where
    cid = CW._chainId bdb

-- | Extract a 'TransactionOutputProof' from a generic pact object
--
extractProof :: Bool -> Pact4.Object Pact4.Name -> Either Text (TransactionOutputProof SHA512t_256)
extractProof False o = Pact4.toPactValue (Pact4.TObject o Pact4.noInfo) >>= k
  where
    k = aeson (Left . pack) Right
      . fromJSON
      . J.toJsonViaEncode
extractProof True (Pact4.Object (Pact4.ObjectMap o) _ _ _) = case M.lookup "proof" o of
  Just (Pact4.TLitString proof) -> do
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
extractEthProof :: Pact4.Object Pact4.Name -> Either Text ReceiptProof
extractEthProof o = case M.lookup "proof" $ Pact4._objectMap $ Pact4._oObject o of
  Nothing -> Left "Decoding of Eth proof object failed: missing 'proof' property"
  Just (Pact4.TLitString p) -> do
    bytes' <- errMsg "Decoding of Eth proof object failed: invalid base64URLWithoutPadding encoding"
        $ decodeB64UrlNoPaddingText p
    errMsg "Decoding of Eth proof object failed: invalid binary proof data"
        $ get getRlp bytes'
  Just _ -> Left "Decoding of Eth proof object failed: invalid 'proof' property"
  where
    errMsg t = first (const t)

ethResultToPactValue :: ReceiptProofValidation -> Pact4.Object Pact4.Name
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
      , ("status",Pact4.toTerm $ _receiptStatus == TxStatus 1)
      , ("logs",Pact4.toTList Pact4.TyAny Pact4.noInfo $ map rlog _receiptLogs)]
    rlog LogEntry{..} = obj
      [ ("address",jsonStr _logEntryAddress)
      , ("topics",Pact4.toTList Pact4.TyAny Pact4.noInfo $ map topic _logEntryTopics)
      , ("data",jsonStr _logEntryData)]
    topic t = jsonStr t
    header ch@EthHeader.ConsensusHeader{..} = obj
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
      String s -> Pact4.tStr s
      _ -> Pact4.tStr $ sshow v
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
    -> Pact4.PactHash
    -> IO (Either Text Int)
getTxIdx bdb pdb bh th = do
    -- get BlockPayloadHash
    m <- maxEntry bdb
    ph <- seekAncestor bdb m (int bh) >>= \case
        Just x -> return $ Right $! view blockPayloadHash x
        Nothing -> return $ Left "unable to find payload associated with transaction hash"

    case ph of
      (Left !s) -> return $ Left s
      (Right !a) -> do
        -- get payload
        Just payload <- lookupPayloadWithHeight pdb (Just bh) a

        -- Find transaction index
        r <- S.each (_payloadWithOutputsTransactions payload)
          & S.map fst
          & S.mapM toTxHash
          & sindex (== th)

        r & note "unable to find transaction at the given block height"
          & fmap int
          & return
  where
    toPactTx :: MonadThrow m => Transaction -> m (Pact4.Command Text)
    toPactTx (Transaction b) = decodeStrictOrThrow' b

    toTxHash :: MonadThrow m => Transaction -> m Pact4.PactHash
    toTxHash = fmap Pact4._cmdHash . toPactTx

    sfind :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe a)
    sfind p = S.head_ . S.dropWhile (not . p)

    sindex :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe Natural)
    sindex p s = S.zip (S.each [0..]) s & sfind (p . snd) & fmap (fmap fst)

mkObject :: [(Pact4.FieldKey, Pact4.Term n)] -> Pact4.Object n
mkObject ps = Pact4.Object (Pact4.ObjectMap (M.fromList ps)) Pact4.TyAny Nothing Pact4.noInfo

obj :: [(Pact4.FieldKey, Pact4.Term n)] -> Pact4.Term n
obj = Pact4.toTObject Pact4.TyAny Pact4.noInfo

tInt :: Integral i => i -> Pact4.Term Pact4.Name
tInt = Pact4.toTerm . fromIntegral @_ @Integer

-- | Encode a "successful" CommandResult into a Pact object.
mkSPVResult
    :: Pact4.CommandResult Pact4.Hash
       -- ^ Full CR
    -> Pact4.PactValue
       -- ^ Success result
    -> Pact4.Object Pact4.Name
mkSPVResult Pact4.CommandResult{..} j =
    mkObject
    [ ("result", Pact4.fromPactValue j)
    , ("req-key", Pact4.tStr $ Pact4.asString $ Pact4.unRequestKey _crReqKey)
    , ("txid", Pact4.tStr $ maybe "" Pact4.asString _crTxId)
    , ("gas", Pact4.toTerm $ (fromIntegral _crGas :: Integer))
    , ("meta", maybe empty metaField _crMetaData)
    , ("logs", Pact4.tStr $ Pact4.asString _crLogs)
    , ("continuation", maybe empty contField _crContinuation)
    , ("events", Pact4.toTList Pact4.TyAny Pact4.noInfo $ map eventField _crEvents)
    ]
  where
    metaField v = case fromJSON v of
      Error _ -> obj []
      Success p -> Pact4.fromPactValue p

    contField (Pact4.PactExec stepCount yield executed step pactId pactCont rollback _nested) = obj
        [ ("step", Pact4.toTerm step)
        , ("step-count", Pact4.toTerm stepCount)
        , ("yield", maybe empty yieldField yield)
        , ("pact-id", Pact4.toTerm pactId)
        , ("cont",contField1 pactCont)
        , ("step-has-rollback",Pact4.toTerm rollback)
        , ("executed",Pact4.tStr $ maybe "" sshow executed)
        ]

    contField1 Pact4.PactContinuation {..} = obj
        [ ("name",Pact4.tStr $ Pact4.asString _pcDef)
        , ("args",Pact4.toTList Pact4.TyAny Pact4.noInfo $ map Pact4.fromPactValue _pcArgs)
        ]

    yieldField Pact4.Yield {..} = obj
        [ ("data",Pact4.fromPactValue (Pact4.PObject _yData))
        , ("provenance", maybe empty provField _yProvenance)
        ]

    provField Pact4.Provenance {..} = obj
        [ ("target-chain", Pact4.toTerm $ Pact4._chainId _pTargetChainId)
        , ("module-hash", Pact4.tStr $ Pact4.asString $ Pact4._mhHash $ _pModuleHash)
        ]

    eventField Pact4.PactEvent {..} = obj
        [ ("name", Pact4.toTerm _eventName)
        , ("params", Pact4.toTList Pact4.TyAny Pact4.noInfo (map Pact4.fromPactValue _eventParams))
        , ("module", Pact4.tStr $ Pact4.asString _eventModule)
        , ("module-hash", Pact4.tStr $ Pact4.asString _eventModuleHash)
        ]

    empty = obj []
