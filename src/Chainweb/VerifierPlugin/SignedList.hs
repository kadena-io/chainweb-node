{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

--
-- The plugin expects the proof value to be structured as follows:
--
--   1. A Pact list of message parts (strings or nested structures)
--   2. A hex-encoded, compressed secp256k1 public key (33 bytes)
--   3. A hex-encoded signature (64 bytes)
--
-- The corresponding capability must contain exactly two arguments:
--
--   1. The same message parts as provided in the proof (after stripping any hashing wrappers)
--   2. The public key (matching exactly the proof's public key)
--
-- Verification process:
--   • Computes SHA3-256 over the UTF-8 concatenation of the message parts
--     (nested lists are recursively hashed and concatenated as needed).
--   • Verifies the signature using the provided public key and computed hash.
--
-- If any check fails, the transaction aborts with a VerifierError.

module Chainweb.VerifierPlugin.SignedList
   (plugin,parseHashList) where

import Control.Monad (unless, guard)
import Control.Monad.Except
import Data.Function ((&))

import qualified Data.Set as Set
import Data.Decimal (Decimal)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SBS

import Data.ByteArray (convert)
import Crypto.Hash (hashWith, SHA3_256(..))
import qualified Crypto.Hash as Hash
import Crypto.Number.Serialize (os2ip)
import Crypto.PubKey.ECC.ECDSA (PublicKey(..), Signature(..), verifyDigest)
import Crypto.PubKey.ECC.Types (CurveName(SEC_p256k1), getCurveByName, Point(..))
import Crypto.Secp256k1 (ecdsaPublicKeyFromCompressed, ecdsaPublicKeyBytes)

import Pact.Core.Errors (VerifierError(..))
import Pact.Types.PactValue
import Pact.Types.Capability (SigCapability(..))
import Pact.Types.Exp (Literal(..))
import Pact.Types.Term (objectMapToListWith,Gas(..))

import Chainweb.VerifierPlugin (VerifierPlugin(..), chargeGas)

import Data.STRef
import Control.Monad.ST

--------------------------------------------------------------------------------
-- Gas Charging Parameters
--------------------------------------------------------------------------------

-- Fixed cost for signature verification
newtype GasParams = GasParams { sigVerificationGas :: Gas }

gasParams :: GasParams
gasParams = GasParams { sigVerificationGas = Gas 650 }

--------------------------------------------------------------------------------
-- Message structure
--------------------------------------------------------------------------------
-- NOTE: We avoid decoding at parse time. Hex blobs are kept as Text

data HashListNode
  = HLNString T.Text
  | HLNDecimal Decimal
  | HLNHashHex T.Text          -- ^ hex-encoded byte chunk, decoded in fold
  | HLNList [HashListNode]
  deriving (Eq, Show)

data HashList
  = HLList [HashListNode]
  | HLHashHex T.Text           -- ^ top-level precomputed digest (hex), decoded in fold
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parsing (no decoding here)
--------------------------------------------------------------------------------
parseHashList :: PactValue -> Either T.Text HashList
parseHashList (PList vec)   = HLList <$> traverse parseHashListNode (V.toList vec)
parseHashList (PObject om)  =
  case objectMapToListWith (,) om of
    [("0x", PLiteral (LString hexStr))] -> Right $ HLHashHex hexStr
    _ -> Left $ "Malformed binary object at top-level, expected exactly one key '0x'"
parseHashList _ = Left $ "Expected a list or binary object at the top-level"

parseHashListNode :: PactValue -> Either T.Text HashListNode
parseHashListNode = \case
  PLiteral (LString t)   -> Right $ HLNString t
  PLiteral (LDecimal d)  -> Right $ HLNDecimal d
  PObject om -> case objectMapToListWith (,) om of
    [("0x", PLiteral (LString hexStr))] -> Right $ HLNHashHex hexStr  -- decoding is in foldHashList
    _ -> Left $ "Malformed binary object, expected exactly one key '0x'"
  PList lst              -> HLNList <$> traverse parseHashListNode (V.toList lst)
  _                      -> Left $ "Invalid PactValue type for hashing"

--------------------------------------------------------------------------------
-- Folding (all decoding + hashing happens here, with gas)
--------------------------------------------------------------------------------
foldHashList
  :: forall s. GasParams
  -> STRef s Gas
  -> HashList
  -> ExceptT VerifierError (ST s) (BS.ByteString, PactValue)
foldHashList gp gasRef = \case
  -- Top-level precomputed digest provided as hex.
  
  HLHashHex hexTxt -> do
    bs <- decodeHex hexTxt
    pure (bs, PList V.empty)

  -- Structural message that needs concatenation + hashing
  HLList nodes -> do
    (chunks, strippedNodes) <- unzip <$> traverse foldNode nodes
    let concatenated = BS.concat chunks
    pure (hashSHA3 concatenated, PList (V.catMaybes $ V.fromList strippedNodes))

  where
    foldNode :: HashListNode -> ExceptT VerifierError (ST s) (BS.ByteString, Maybe PactValue)
    foldNode (HLNString t) = do
      let bs = T.encodeUtf8 t
      pure (bs, Just (PLiteral (LString t)))

    foldNode (HLNDecimal d) = do
      let bs = T.encodeUtf8 (T.pack (show d))
      pure (bs, Just (PLiteral (LDecimal d)))

    foldNode (HLNHashHex hexTxt) = do
      bs <- decodeHex hexTxt
      pure (bs, Nothing)

    foldNode (HLNList ns) = do
      (bs, pv) <- foldHashList gp gasRef (HLList ns)
      pure (bs, Just pv)

    hashSHA3 :: BS.ByteString -> BS.ByteString
    hashSHA3 = convert . hashWith SHA3_256


decodeHex
  :: T.Text
  -> ExceptT VerifierError (ST s) BS.ByteString
decodeHex hexTxt = do
  case hexToBS (T.encodeUtf8 hexTxt) of
    Just bs -> pure bs
    Nothing -> throwError $ VerifierError $ "Malformed hex encoding: " <> hexTxt

--------------------------------------------------------------------------------
-- Plugin Entry Point
--------------------------------------------------------------------------------
plugin :: VerifierPlugin
plugin = VerifierPlugin $ \_ proof caps gasRef -> do
  let gp = gasParams

  -- Extract and validate capability arguments
  (capArgs :: [PactValue]) <- case Set.toList caps of
    [SigCapability{_scArgs = as}] -> pure as
    _ -> throwError $ VerifierError "Expected exactly one capability"

  (capMsgParts, capPubKeyTxt) <- case capArgs of
    [l@(PList _), PLiteral (LString pkTxt)] -> pure (l, pkTxt)
    _ -> throwError $ VerifierError "Capability args must be: (list, pubKey)"

  -- Parse proof structure (no decoding here)
  (msgParts, pubKeyTxt, sigTxt) <- case proof of
    PList vec
      | [PList lst, PLiteral (LString pk), PLiteral (LString sig)] <- V.toList vec -> pure (lst, pk, sig)
    _ -> throwError $ VerifierError "Proof must be: [message parts, pubKey, sig]"

  parsedMsgParts <- case parseHashList (PList msgParts) of
    Right hl -> pure hl
    Left _ -> throwError $ VerifierError $ "Parsing hash list failed"

  (msgHashBS, strippedMsgParts) <- foldHashList gp gasRef parsedMsgParts

  unless (strippedMsgParts == capMsgParts && pubKeyTxt == capPubKeyTxt) $
     throwError $ VerifierError $ "Capability arguments do not match proof data"
   -- Signature verification
  chargeGas gasRef (sigVerificationGas gp)

  let sigHexBS = T.encodeUtf8 sigTxt
      pkHexBS  = T.encodeUtf8 pubKeyTxt

  isValid <- case verifySecp256k1Signature msgHashBS sigHexBS pkHexBS of
    Just v  -> pure v
    Nothing -> throwError $ VerifierError "Malformed signature verification inputs"

  unless isValid $
     throwError $ VerifierError "Signature verification failed"

  pure ()

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
verifySecp256k1Signature :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe Bool
verifySecp256k1Signature msgHash sigHex pubKeyHexComp =
  case (parseSignature sigHex, parsePubKey pubKeyHexComp) of
    (Just sig, Just pk) -> do
      digest <- Hash.digestFromByteString msgHash :: Maybe (Hash.Digest SHA3_256)
      pure (verifyDigest pk sig digest)
    _ -> Nothing

parseSignature :: BS.ByteString -> Maybe Signature
parseSignature hex = do
  bytes <- hexToBS hex
  guard (BS.length bytes == 64)
  let (rBytes, sBytes) = BS.splitAt 32 bytes
  pure $ Signature (os2ip rBytes) (os2ip sBytes)

parsePubKey :: BS.ByteString -> Maybe PublicKey
parsePubKey hex = do
  compressed <- hexToBS hex >>= decompressPublicKey
  case BS.uncons compressed of
    Just (0x04, rest) | BS.length rest == 64 ->
      let (x, y) = BS.splitAt 32 rest
      in pure $ PublicKey (getCurveByName SEC_p256k1) (Point (os2ip x) (os2ip y))
    _ -> Nothing

decompressPublicKey :: BS.ByteString -> Maybe BS.ByteString
decompressPublicKey cKey =
    SBS.toShort cKey
  & ecdsaPublicKeyFromCompressed
  & either (const Nothing) (Just . SBS.fromShort . ecdsaPublicKeyBytes)

hexToBS :: BS.ByteString -> Maybe BS.ByteString
hexToBS = either (const Nothing) Just . B16.decode
