{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Transaction
  ( ChainwebTransaction
  , HashableTrans(..)
  , PayloadWithText
  , PactParserVersion(..)
  , chainwebPayloadCodec
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
import qualified Data.ByteString.Lazy as BL
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

import Chainweb.Utils
import Chainweb.Utils.Serialization

-- | A product type representing a `Payload PublicMeta ParsedCode` coupled with
-- the Text that generated it, to make gossiping easier.
--
data PayloadWithText = PayloadWithText
    { _payloadBytes :: !SB.ShortByteString
    , _payloadObj :: !(Payload PublicMeta ParsedCode)
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

payloadBytes :: PayloadWithText -> SB.ShortByteString
payloadBytes = _payloadBytes

payloadObj :: PayloadWithText -> Payload PublicMeta ParsedCode
payloadObj = _payloadObj

mkPayloadWithText :: Command ByteString -> Payload PublicMeta ParsedCode -> PayloadWithText
mkPayloadWithText cmd p = PayloadWithText
    { _payloadBytes = SB.toShort $ _cmdPayload cmd
    , _payloadObj = p
    }

mkPayloadWithTextOld :: Payload PublicMeta ParsedCode -> PayloadWithText
mkPayloadWithTextOld p = PayloadWithText
    { _payloadBytes = SB.toShort $ BL.toStrict $ Aeson.encode $ fmap _pcCode p
    , _payloadObj = p
    }

type ChainwebTransaction = Command PayloadWithText

data PactParserVersion
    = PactParserGenesis
    | PactParserChainweb213
    deriving (Eq, Ord, Bounded, Show, Enum)

-- | Hashable newtype of ChainwebTransaction
newtype HashableTrans a = HashableTrans { unHashable :: Command a }
    deriving (Eq, Functor, Ord)

instance Hashable (HashableTrans PayloadWithText) where
    hashWithSalt s (HashableTrans t) = hashWithSalt s hashCode
      where
        (TypedHash hc) = _cmdHash t
        decHC = runGetEitherS getWord64le
        !hashCode = either error id $ decHC (B.take 8 $ SB.fromShort hc)
    {-# INLINE hashWithSalt #-}

-- | A codec for (Command PayloadWithText) transactions.
chainwebPayloadCodec
    :: PactParserVersion
    -> Codec (Command PayloadWithText)
chainwebPayloadCodec ppv = Codec enc dec
  where
    enc c = encodeToByteString $ fmap (decodeUtf8 . encodePayload) c
    dec bs = case Aeson.decodeStrict' bs of
               Just cmd -> traverse (decodePayload ppv . encodeUtf8) cmd
               Nothing -> Left "decode PayloadWithText failed"

encodePayload :: PayloadWithText -> ByteString
encodePayload = SB.fromShort . _payloadBytes

decodePayload
    :: PactParserVersion
    -> ByteString
    -> Either String PayloadWithText
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
