{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language ImportQualifiedPost #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Chainweb.Pact5.Transaction
  ( Transaction
  , PayloadWithText
  , payloadBytes
  , payloadObj
  , payloadCodec
  , parseCommand
  , parsePact4Command
  ) where

import "aeson" Data.Aeson qualified as Aeson
import "base" Data.Coerce (coerce)
import "base" Data.Function
import "base" Data.Word (Word64)
import "base" GHC.Generics (Generic)
import "bytestring" Data.ByteString.Char8 (ByteString)
import "bytestring" Data.ByteString.Short qualified as SB
import "deepseq" Control.DeepSeq
import "lens" Control.Lens
import "pact" Pact.Parse qualified as Pact4
import "pact" Pact.Types.Capability qualified as Pact4
import "pact" Pact.Types.ChainId qualified as Pact4
import "pact" Pact.Types.ChainMeta qualified as Pact4
import "pact" Pact.Types.Command qualified as Pact4
import "pact" Pact.Types.Crypto qualified as Pact4
import "pact" Pact.Types.Gas qualified as Pact4
import "pact" Pact.Types.Hash qualified as Pact4
import "pact" Pact.Types.PactValue qualified as Pact4
import "pact" Pact.Types.RPC qualified as Pact4
import "pact" Pact.Types.SPV qualified as Pact4
import "pact" Pact.Types.Term.Internal (PactId(..))
import "pact" Pact.Types.Verifier (VerifierName(..))
import "pact" Pact.Types.Verifier qualified as Pact4
import "pact-json" Pact.JSON.Encode qualified as J
import "pact-json" Pact.JSON.Encode (Encode(..))
import "pact-json" Pact.JSON.Legacy.Value qualified as J
import "pact-tng" Pact.Core.Capabilities
import "pact-tng" Pact.Core.ChainData
import "pact-tng" Pact.Core.Command.Crypto
import "pact-tng" Pact.Core.Command.RPC
import "pact-tng" Pact.Core.Command.Types
import "pact-tng" Pact.Core.Gas.Types
import "pact-tng" Pact.Core.Hash
import "pact-tng" Pact.Core.Names
import "pact-tng" Pact.Core.PactValue
import "pact-tng" Pact.Core.SPV
import "pact-tng" Pact.Core.StableEncoding
import "pact-tng" Pact.Core.Verifiers
import "pact-tng" Pact.Core.Verifiers qualified as Pact5
import "pact-tng" Pact.Core.Signer
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Chainweb.Pact.Conversion
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Utils

type Transaction = Command (PayloadWithText PublicMeta ParsedCode)

data PayloadWithText meta code = UnsafePayloadWithText
    { _payloadBytes :: !SB.ShortByteString
    , _payloadObj :: !(Payload meta code)
    }
    deriving stock (Show, Generic)
    deriving stock (Functor)
    deriving anyclass (NFData)

instance Eq (PayloadWithText meta code) where
    (==) = (==) `on` _payloadBytes

instance (J.Encode meta, J.Encode code) => J.Encode (PayloadWithText meta code) where
    build p = J.object
      [ "payloadBytes" J..= J.encodeText (decodeUtf8 $ SB.fromShort $ _payloadBytes p)
      , "payloadObject" J..= _payloadObj p
      ]

payloadBytes :: Getter (PayloadWithText meta code) SB.ShortByteString
payloadBytes = to _payloadBytes
{-# inline conlike payloadBytes #-}

payloadObj :: Getter (PayloadWithText meta code) (Payload meta code)
payloadObj = to _payloadObj
{-# inline conlike payloadObj #-}

-- | A codec for Pact5's (Command PayloadWithText) transactions.
--
payloadCodec
    :: Codec (Command (PayloadWithText PublicMeta ParsedCode))
payloadCodec = Codec enc dec
    where
    enc c = J.encodeStrict $ fmap (decodeUtf8 . encodePayload) c
    dec bs = case Aeson.decodeStrict' bs of
        Just (cmd :: Command Text) -> parseCommand cmd
        Nothing -> Left "decode PayloadWithText failed"

parseCommand :: Command Text -> Either String (Command (PayloadWithText PublicMeta ParsedCode))
parseCommand cmd = do
    let cmd' = fmap encodeUtf8 cmd
    let code = SB.toShort (_cmdPayload cmd')
    parsedCmd <- over (_Right . cmdPayload . pMeta) _stableEncoding $ unsafeParseCommand cmd'
    return (parsedCmd & cmdPayload %~ \obj -> UnsafePayloadWithText { _payloadBytes = code, _payloadObj = obj })

encodePayload :: PayloadWithText meta code -> ByteString
encodePayload = SB.fromShort . _payloadBytes

parsePact4Command :: Pact4.UnparsedTransaction -> Either String Transaction
parsePact4Command cmd4 = do
  let cmd = fromPact4Command cmd4
  payloadWithParsedCode :: Payload PublicMeta ParsedCode <-
    (pPayload . _Exec . pmCode) parsePact (cmd ^. cmdPayload . payloadObj)
  let payloadWithTextWithParsedCode =
        UnsafePayloadWithText (cmd ^. cmdPayload . payloadBytes) payloadWithParsedCode
  return $ cmd & cmdPayload .~ payloadWithTextWithParsedCode

fromPact4Command :: Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Text) -> Command (PayloadWithText PublicMeta Text)
fromPact4Command cmd4 = Command
  { _cmdPayload = fromPact4PayloadWithText (Pact4._cmdPayload cmd4)
  , _cmdSigs = map fromPact4UserSig (Pact4._cmdSigs cmd4)
  , _cmdHash = fromPact4Hash (Pact4._cmdHash cmd4)
  }
  where
    fromPact4PayloadWithText :: Pact4.PayloadWithText Pact4.PublicMeta Text -> PayloadWithText PublicMeta Text
    fromPact4PayloadWithText payload4 = UnsafePayloadWithText
      { _payloadBytes = Pact4.payloadBytes payload4
      , _payloadObj = fromPact4Payload (Pact4.payloadObj payload4)
      }

    fromPact4Payload :: Pact4.Payload Pact4.PublicMeta Text -> Payload PublicMeta Text
    fromPact4Payload payload4 = Payload
      { _pPayload = fromPact4RPC (Pact4._pPayload payload4)
      , _pNonce = Pact4._pNonce payload4
      , _pMeta = fromPact4PublicMeta (Pact4._pMeta payload4)
      , _pSigners = map fromPact4Signer (Pact4._pSigners payload4)
      , _pVerifiers = map fromPact4Verifier <$> Pact4._pVerifiers payload4
      , _pNetworkId = fromPact4NetworkId <$> Pact4._pNetworkId payload4
      }

    fromPact4RPC :: Pact4.PactRPC c -> PactRPC c
    fromPact4RPC = \case
      Pact4.Exec execMsg -> Exec $ ExecMsg
        { _pmCode = Pact4._pmCode execMsg
        , _pmData = legacyJsonToPactValue (Pact4._pmData execMsg)
        }
      Pact4.Continuation contMsg -> Continuation $ ContMsg
        { _cmPactId = coerce Pact4._cmPactId contMsg
        , _cmStep = Pact4._cmStep contMsg
        , _cmRollback = Pact4._cmRollback contMsg
        , _cmData = legacyJsonToPactValue (Pact4._cmData contMsg)
        , _cmProof = ContProof . Pact4._contProof <$> Pact4._cmProof contMsg
        }

    fromPact4PublicMeta :: Pact4.PublicMeta -> PublicMeta
    fromPact4PublicMeta pm4 = PublicMeta
      { _pmChainId = coerce (Pact4._pmChainId pm4)
      , _pmSender = Pact4._pmSender pm4
      , _pmGasLimit = fromPact4GasLimit (Pact4._pmGasLimit pm4)
      , _pmGasPrice = fromPact4GasPrice (Pact4._pmGasPrice pm4)
      , _pmTTL = fromPact4TTLSeconds (Pact4._pmTTL pm4)
      , _pmCreationTime = fromPact4TxCreationTime (Pact4._pmCreationTime pm4)
      }

    fromPact4Signer :: Pact4.Signer -> Signer
    fromPact4Signer signer4 = Signer
      { _siScheme = Pact4._siScheme signer4 <&> \case { Pact4.ED25519 -> ED25519; Pact4.WebAuthn -> WebAuthn; }
      , _siPubKey = Pact4._siPubKey signer4
      , _siAddress = Pact4._siAddress signer4
      , _siCapList = map fromPact4SigCapability (Pact4._siCapList signer4)
      }

    fromPact4SigCapability :: Pact4.SigCapability -> SigCapability
    fromPact4SigCapability cap4 = SigCapability $ CapToken
      { _ctName = fromLegacyQualifiedName (Pact4._scName cap4)
      , _ctArgs = fromPact4PactValue <$> Pact4._scArgs cap4
      }

    fromPact4Verifier :: Pact4.Verifier Pact4.ParsedVerifierProof -> Verifier Pact5.ParsedVerifierProof
    fromPact4Verifier verifier4 = Verifier
      { _verifierName = coerce (Pact4._verifierName verifier4)
      , _verifierProof = Pact5.ParsedVerifierProof
          $ fromPact4PactValue
          $ case Pact4._verifierProof verifier4 of { Pact4.ParsedVerifierProof pv -> pv; }
      , _verifierCaps = fromPact4SigCapability <$> Pact4._verifierCaps verifier4
      }

    fromPact4NetworkId :: Pact4.NetworkId -> NetworkId
    fromPact4NetworkId = NetworkId . Pact4._networkId

    legacyJsonToPactValue :: J.LegacyValue -> PactValue
    legacyJsonToPactValue lv = case decodeStable @PactValue (J.encodeStrict lv) of
      Just pv -> pv
      Nothing -> error "TODO: don't throw an error here, use Either"

    fromPact4PactValue :: Pact4.PactValue -> PactValue
    fromPact4PactValue pv4 = case fromLegacyPactValue pv4 of
      Right pv -> pv
      Left err -> error $ "TODO: don't throw an error here: " ++ show err

    fromPact4UserSig :: Pact4.UserSig -> UserSig
    fromPact4UserSig = \case
      Pact4.ED25519Sig txt -> ED25519Sig txt
      Pact4.WebAuthnSig webAuthnSig4 -> WebAuthnSig $ WebAuthnSignature
        { clientDataJSON = Pact4.clientDataJSON webAuthnSig4
        , authenticatorData = Pact4.authenticatorData webAuthnSig4
        , signature = Pact4.signature webAuthnSig4
        }

    fromPact4Hash :: Pact4.PactHash -> Hash
    fromPact4Hash (Pact4.TypedHash sbs) = Hash sbs

    fromPact4ParsedInteger :: Pact4.ParsedInteger -> Word64
    fromPact4ParsedInteger (Pact4.ParsedInteger i)
      | i < 0 = error "fromPact4ParsedInteger: negative argument"
      | otherwise = fromIntegral i

    fromPact4GasLimit :: Pact4.GasLimit -> GasLimit
    fromPact4GasLimit (Pact4.GasLimit i) = GasLimit (Gas (fromPact4ParsedInteger i))

    fromPact4GasPrice :: Pact4.GasPrice -> GasPrice
    fromPact4GasPrice (Pact4.GasPrice (Pact4.ParsedDecimal d)) = GasPrice d

    fromPact4TTLSeconds :: Pact4.TTLSeconds -> TTLSeconds
    fromPact4TTLSeconds (Pact4.TTLSeconds (Pact4.ParsedInteger i)) = TTLSeconds i

    fromPact4TxCreationTime :: Pact4.TxCreationTime -> TxCreationTime
    fromPact4TxCreationTime (Pact4.TxCreationTime (Pact4.ParsedInteger i)) = TxCreationTime i

-- decodePayload
--     :: ByteString
--     -> Either String PayloadWithText
-- decodePayload bs = case Aeson.decodeStrict' bs of
--     Just (payload :: Payload (StableEncoding PublicMeta) Text) -> do
--         p <- traverse parseCode $
--             over pMeta _stableEncoding payload
--         return $! PayloadWithText (SB.toShort bs) p
--     Nothing -> Left "decoding Payload failed"
