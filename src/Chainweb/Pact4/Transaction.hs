{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}

module Chainweb.Pact4.Transaction
  ( Transaction
  , UnparsedTransaction
  , HashableTrans(..)
  , PayloadWithText
  , PactParserVersion(..)
  , IsWebAuthnPrefixLegal(..)
  , payloadCodec
  , rawCommandCodec
  , encodePayload
  , decodePayload
  , cmdGasLimit
  , cmdGasPrice
  , cmdTimeToLive
  , cmdCreationTime
  , mkPayloadWithText
  , mkPayloadWithTextOld
  , payloadBytes
  , payloadObj
  , parsePact
  ) where

import Control.DeepSeq
import Control.Lens

import qualified Data.Aeson as Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SB
import Data.Hashable
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import GHC.Generics (Generic)

import qualified Pact.Parse as P (parsePact, legacyParsePact)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas (GasLimit(..), GasPrice(..))
import Pact.Types.Hash
import qualified Pact.JSON.Encode as J
import Pact.JSON.Legacy.Value

import Chainweb.Utils
import Chainweb.Utils.Serialization

-- | A product type representing a `Payload PublicMeta ParsedCode` coupled with
-- the Text that generated it, to make gossiping easier.
--
data PayloadWithText meta code = PayloadWithText
    { _payloadBytes :: !SB.ShortByteString
    , _payloadObj :: !(Payload meta code)
    }
    deriving stock (Functor, Foldable, Traversable, Show, Eq, Generic)
    deriving anyclass (NFData)

payloadBytes :: PayloadWithText meta code -> SB.ShortByteString
payloadBytes = _payloadBytes

payloadObj :: PayloadWithText meta code -> Payload meta code
payloadObj = _payloadObj

mkPayloadWithText :: Command (ByteString, Payload meta code) -> Command (PayloadWithText meta code)
mkPayloadWithText = over cmdPayload $ \(bs, p) -> PayloadWithText
    { _payloadBytes = SB.toShort bs
    , _payloadObj = p
    }

mkPayloadWithTextOld :: Payload PublicMeta ParsedCode -> PayloadWithText PublicMeta ParsedCode
mkPayloadWithTextOld p = PayloadWithText
    { _payloadBytes = SB.toShort $ J.encodeStrict $ toLegacyJsonViaEncode $ fmap _pcCode p
    , _payloadObj = p
    }

type Transaction = Command (PayloadWithText PublicMeta ParsedCode)
type UnparsedTransaction = Command (PayloadWithText PublicMeta Text)

data PactParserVersion
    = PactParserGenesis
    | PactParserChainweb213
    deriving (Eq, Ord, Bounded, Show, Enum)

data IsWebAuthnPrefixLegal
    = WebAuthnPrefixIllegal
    | WebAuthnPrefixLegal
    deriving (Eq, Ord, Bounded, Show, Enum)

-- | Hashable newtype of Transaction
newtype HashableTrans a = HashableTrans { unHashable :: Command a }
    deriving (Eq, Functor, Ord)

instance (Eq code, Eq meta) => Hashable (HashableTrans (PayloadWithText meta code)) where
    hashWithSalt s (HashableTrans t) = hashWithSalt s hashCode
      where
        (TypedHash hc) = _cmdHash t
        decHC = runGetEitherS getWord64le
        !hashCode = either error id $ decHC (B.take 8 $ SB.fromShort hc)
    {-# INLINE hashWithSalt #-}

rawCommandCodec :: Codec (Command (PayloadWithText PublicMeta Text))
rawCommandCodec = Codec enc dec
    where
    enc cmd = J.encodeStrict $ J.text . decodeUtf8 . SB.fromShort . _payloadBytes <$> cmd
    dec bs = do
        cmd <- Aeson.eitherDecodeStrict' bs
        let p = encodeUtf8 $ _cmdPayload cmd
        payloadObject <- Aeson.eitherDecodeStrict' p
        let payloadWithText = PayloadWithText { _payloadBytes = (SB.toShort p), _payloadObj = payloadObject }
        return $ payloadWithText <$ cmd

-- | A codec for Pact4's (Command PayloadWithText) transactions.
--
payloadCodec
    :: PactParserVersion
    -> Codec Transaction
payloadCodec ppv = Codec enc dec
    where
    enc c = J.encodeStrict $ fmap (decodeUtf8 . encodePayload) c
    dec bs = case Aeson.decodeStrict' bs of
        Just cmd -> traverse (decodePayload ppv . encodeUtf8) cmd
        Nothing -> Left "decode PayloadWithText failed"

encodePayload :: PayloadWithText meta code -> ByteString
encodePayload = SB.fromShort . _payloadBytes

decodePayload
    :: Aeson.FromJSON meta
    => PactParserVersion
    -> ByteString
    -> Either String (PayloadWithText meta ParsedCode)
decodePayload ppv bs = case Aeson.decodeStrict' bs of
    Just payload -> do
        p <- traverse (parsePact ppv) payload
        return $! PayloadWithText (SB.toShort bs) p
    Nothing -> Left "decoding Payload failed"

parsePact
    :: PactParserVersion
        -- ^ If the chain context is @Nothing@, latest parser version is used.
    -> Text
    -> Either String ParsedCode
parsePact PactParserChainweb213 = P.parsePact
parsePact PactParserGenesis = P.legacyParsePact

-- | Access the gas limit/supply of a public chain command payload
cmdGasLimit :: Lens' (Command (Payload PublicMeta c)) GasLimit
cmdGasLimit = cmdPayload . pMeta . pmGasLimit
{-# INLINE cmdGasLimit #-}

-- | Get the gas price of a public chain command payload
cmdGasPrice :: Lens' (Command (Payload PublicMeta c)) GasPrice
cmdGasPrice = cmdPayload . pMeta . pmGasPrice
{-# INLINE cmdGasPrice #-}

cmdTimeToLive :: Lens' (Command (Payload PublicMeta c)) TTLSeconds
cmdTimeToLive = cmdPayload . pMeta . pmTTL
{-# INLINE cmdTimeToLive #-}

cmdCreationTime :: Lens' (Command (Payload PublicMeta c)) TxCreationTime
cmdCreationTime = cmdPayload . pMeta . pmCreationTime
{-# INLINE cmdCreationTime #-}
