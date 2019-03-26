{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Transaction
  ( ChainwebTransaction
  , PayloadWithText(..)
  , payloadBytes
  , payloadObj
  , chainwebTransactionConfig
  , chainwebPayloadCodec
  , gasLimitOf
  , gasPriceOf
  ) where


import Control.Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Pact.Parse (ParsedDecimal(..), ParsedInteger(..), parseExprs)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas (GasLimit(..), GasPrice(..))
import Pact.Types.Util (Hash(..))

import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import qualified Chainweb.Time as Time
import Chainweb.Utils

-- | A product type representing a `Payload PublicMeta ParsedCode` coupled with
-- the Text that generated it, to make gossiping easier
data PayloadWithText = PayloadWithText
    { _payloadBytes :: ByteString
    , _payloadObj :: Payload PublicMeta ParsedCode
    }
    deriving (Show, Eq)



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

makeLenses 'PayloadWithText

type ChainwebTransaction = Command PayloadWithText

chainwebTransactionConfig :: TransactionConfig ChainwebTransaction
chainwebTransactionConfig = TransactionConfig
  { txCodec = chainwebPayloadCodec
  , txHasher = commandHash
  , txHashMeta = chainwebTestHashMeta
  , txGasPrice = getGasPrice
  , txGasLimit = getGasLimit
  , txMetadata = const txmeta
  , txValidate = validate
  }
  where

    getGasPrice = gasPriceOf . fmap _payloadObj

    getGasLimit = fromIntegral . gasLimitOf . fmap _payloadObj

    commandHash c = let (Hash h) = _cmdHash c
                    in TransactionHash h

    -- TODO: plumb through origination + expiry time from pact once it makes it
    -- into PublicMeta
    txmeta = TransactionMetadata Time.minTime Time.maxTime

    getChainId = chainIdFromText' . view (cmdPayload . payloadObj . pMeta . pmChainId)

    validate cid cmd = return $ getChainId cmd >>= \c ->
      if c == cid then return ()
        else Left $ "Invalid chain id: " <> sshow c




-- | A codec for (Command PayloadWithText) transactions.
chainwebPayloadCodec :: Codec (Command PayloadWithText)
chainwebPayloadCodec = Codec (_payloadBytes . _cmdPayload) chainwebPayloadDecode

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
