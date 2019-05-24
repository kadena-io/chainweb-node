{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Transaction
  ( ChainwebTransaction
  , PayloadWithText(..)
  , chainwebPayloadCodec
  , chainwebPayloadDecode
  , gasLimitOf
  , gasPriceOf
  ) where

import Chainweb.Utils (Codec(..))

import Control.DeepSeq

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import GHC.Generics (Generic)

import Pact.Parse (ParsedDecimal(..), ParsedInteger(..), parseExprs)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas (GasLimit(..), GasPrice(..))

-- | A product type representing a `Payload PublicMeta ParsedCode` coupled with
-- the Text that generated it, to make gossiping easier.
--
data PayloadWithText = PayloadWithText
    { payloadBytes :: ByteString
    , payloadObj :: Payload PublicMeta ParsedCode
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

instance Ord PayloadWithText where
    compare x y = compare (payloadBytes x) (payloadBytes y)

instance ToJSON PayloadWithText where
    toJSON (PayloadWithText bs _) = toJSON (T.decodeUtf8 bs)
    toEncoding (PayloadWithText bs _) = toEncoding (T.decodeUtf8 bs)

instance FromJSON PayloadWithText where
    parseJSON = Aeson.withText "PayloadWithText" $ \text -> let bs = T.encodeUtf8 text in
        case traverse parsePact =<< Aeson.eitherDecodeStrict' bs of
          Left err -> fail err
          Right payload -> pure $ PayloadWithText bs payload
      where
        parsePact :: Text -> Either String ParsedCode
        parsePact code = ParsedCode code <$> parseExprs code

type ChainwebTransaction = Command PayloadWithText

-- | A codec for (Command PayloadWithText) transactions.
chainwebPayloadCodec :: Codec (Command PayloadWithText)
chainwebPayloadCodec = Codec (payloadBytes . _cmdPayload) chainwebPayloadDecode

chainwebPayloadDecode :: ByteString -> Either String (Command PayloadWithText)
chainwebPayloadDecode bs = case Aeson.decodeStrict' bs of
    Just cmd -> Right cmd
    Nothing -> Left "decoding PayloadWithText failed"

-- | Get the gas limit/supply of a public chain command payload
gasLimitOf :: forall c. Command (Payload PublicMeta c) -> GasLimit
gasLimitOf cmd = case _pmGasLimit . _pMeta . _cmdPayload $ cmd of
    ParsedInteger limit -> GasLimit . fromIntegral $ limit
{-# INLINE gasLimitOf #-}

-- | Get the gas price of a public chain command payload
gasPriceOf :: forall c. Command (Payload PublicMeta c) -> GasPrice
gasPriceOf cmd = case _pmGasPrice . _pMeta . _cmdPayload $ cmd of
    ParsedDecimal price -> GasPrice price
{-# INLINE gasPriceOf #-}
