{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Transaction
  ( ChainwebTransaction
  , HashableTrans(..)
  , PayloadWithText
  , chainwebPayloadCodec
  , encodePayload
  , decodePayload
  , gasLimitOf
  , gasPriceOf
  , timeToLiveOf
  , creationTimeOf
  , mkPayloadWithText
  , mkPayloadWithTextOld
  , payloadBytes
  , payloadObj
  , parsePact
  ) where

import Control.DeepSeq

import qualified Data.Aeson as Aeson
import Data.Bytes.Get
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
import Chainweb.Version
import Chainweb.BlockHeight

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

-- | Hashable newtype of ChainwebTransaction
newtype HashableTrans a = HashableTrans { unHashable :: Command a }
    deriving (Eq, Functor, Ord)

instance Hashable (HashableTrans PayloadWithText) where
    hashWithSalt s (HashableTrans t) = hashWithSalt s hashCode
      where
        (TypedHash hc) = _cmdHash t
        decHC = runGetS getWord64host
        !hashCode = either error id $ decHC (B.take 8 hc)
    {-# INLINE hashWithSalt #-}

-- | A codec for (Command PayloadWithText) transactions.
chainwebPayloadCodec
    :: Maybe (ChainwebVersion, BlockHeight)
    -> Codec (Command PayloadWithText)
chainwebPayloadCodec chainCtx = Codec enc dec
  where
    enc c = encodeToByteString $ fmap (decodeUtf8 . encodePayload) c
    dec bs = case Aeson.decodeStrict' bs of
               Just cmd -> traverse (decodePayload chainCtx . encodeUtf8) cmd
               Nothing -> Left "decode PayloadWithText failed"

encodePayload :: PayloadWithText -> ByteString
encodePayload = SB.fromShort . _payloadBytes

decodePayload
    :: Maybe (ChainwebVersion, BlockHeight)
    -> ByteString
    -> Either String PayloadWithText
decodePayload chainCtx bs = case Aeson.decodeStrict' bs of
    Just payload -> do
        p <- traverse (parsePact chainCtx) payload
        return $! PayloadWithText (SB.toShort bs) p
    Nothing -> Left "decoding Payload failed"

parsePact
    :: Maybe (ChainwebVersion, BlockHeight)
        -- ^ If the chain context is @Nothing@, latest parser version is used.
    -> Text
    -> Either String ParsedCode
parsePact Nothing code = P.parsePact code
parsePact (Just (v, h)) code
    | chainweb213Pact v h = P.parsePact code
    | otherwise = P.legacyParsePact code

-- | Get the gas limit/supply of a public chain command payload
gasLimitOf :: forall c. Command (Payload PublicMeta c) -> GasLimit
gasLimitOf = _pmGasLimit . _pMeta . _cmdPayload
{-# INLINE gasLimitOf #-}

-- | Get the gas price of a public chain command payload
gasPriceOf :: forall c. Command (Payload PublicMeta c) -> GasPrice
gasPriceOf = _pmGasPrice . _pMeta . _cmdPayload
{-# INLINE gasPriceOf #-}

timeToLiveOf :: forall c . Command (Payload PublicMeta c) -> TTLSeconds
timeToLiveOf = _pmTTL . _pMeta . _cmdPayload
{-# INLINE timeToLiveOf #-}

creationTimeOf :: forall c . Command (Payload PublicMeta c) -> TxCreationTime
creationTimeOf = _pmCreationTime . _pMeta . _cmdPayload
{-# INLINE creationTimeOf #-}
