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
  , PayloadWithText(..)
  , payloadBytes
  , payloadObj
  , chainwebPayloadCodec
  , chainwebPayloadDecode
  , gasLimitOf
  , gasPriceOf
  , timeToLiveOf
  , creationTimeOf
  ) where

import Control.DeepSeq
import Control.Lens

import qualified Data.ByteString.Char8 as B
import Data.Hashable

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Bytes.Get
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Short as SB
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import GHC.Generics (Generic)

import Pact.Parse (parseExprs)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas (GasLimit(..), GasPrice(..))
import Pact.Types.Hash

import Chainweb.Utils (Codec(..))

-- | A product type representing a `Payload PublicMeta ParsedCode` coupled with
-- the Text that generated it, to make gossiping easier.
--
data PayloadWithText = PayloadWithText
    { _payloadBytes :: !SB.ShortByteString
    , _payloadObj :: !(Payload PublicMeta ParsedCode)
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

instance ToJSON PayloadWithText where
    toJSON (PayloadWithText bs _) = toJSON (T.decodeUtf8 $ SB.fromShort bs)
    toEncoding (PayloadWithText bs _) = toEncoding (T.decodeUtf8 $ SB.fromShort bs)

instance FromJSON PayloadWithText where
    parseJSON = Aeson.withText "PayloadWithText" $ \text -> let bs = T.encodeUtf8 text in
        case traverse parsePact =<< Aeson.eitherDecodeStrict' bs of
          Left err -> fail err
          (Right !payload) -> pure $! PayloadWithText (SB.toShort bs) (force payload)
      where
        parsePact :: Text -> Either String ParsedCode
        parsePact code = ParsedCode code <$> parseExprs code

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
chainwebPayloadCodec :: Codec (Command PayloadWithText)
chainwebPayloadCodec = Codec
    (force . SB.fromShort . _payloadBytes . _cmdPayload)
    (force . chainwebPayloadDecode)

chainwebPayloadDecode :: ByteString -> Either String (Command PayloadWithText)
chainwebPayloadDecode bs = case Aeson.decodeStrict' bs of
    Just cmd -> Right $ force cmd
    Nothing -> Left "decoding PayloadWithText failed"

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

makeLenses ''PayloadWithText
