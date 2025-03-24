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

module Chainweb.Pact.Transaction
    ( Transaction
    , PayloadWithText
    , unsafeMkPayloadWithText
    , payloadBytes
    , payloadObj
    , commandCodec
    , parseCommand
    , HashableTransaction(..)
    ) where

import "aeson" Data.Aeson qualified as Aeson
import "base" Data.Function
import "base" GHC.Generics (Generic)
import "bytestring" Data.ByteString.Char8 (ByteString)
import "bytestring" Data.ByteString.Short qualified as SB
import "deepseq" Control.DeepSeq
import "lens" Control.Lens
import "pact-json" Pact.JSON.Encode qualified as J
import "pact-json" Pact.JSON.Encode (Encode(..))
import "pact-tng" Pact.Core.ChainData
import "pact-tng" Pact.Core.Command.Types
import "pact-tng" Pact.Core.StableEncoding
import "pact-tng" Pact.Core.Errors
import "pact-tng" Pact.Core.Info
import "pact-tng" Pact.Core.Pretty qualified as Pact
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Chainweb.Utils
import Data.Hashable

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

unsafeMkPayloadWithText :: Payload meta code -> ByteString -> PayloadWithText meta code
unsafeMkPayloadWithText payload bytes = UnsafePayloadWithText
    { _payloadBytes = SB.toShort bytes
    , _payloadObj = payload
    }

payloadBytes :: Getter (PayloadWithText meta code) SB.ShortByteString
payloadBytes = to _payloadBytes
{-# inline conlike payloadBytes #-}

payloadObj :: Getter (PayloadWithText meta code) (Payload meta code)
payloadObj = to _payloadObj
{-# inline conlike payloadObj #-}

-- | A codec for Pact5's (Command PayloadWithText) transactions.
--
commandCodec
    :: Codec (Command (PayloadWithText PublicMeta ParsedCode))
commandCodec = Codec enc dec
    where
    enc c = J.encodeStrict $ fmap (decodeUtf8 . encodePayload) c
    dec bs = case Aeson.decodeStrict' bs of
        -- Note: this can only ever emit a `ParseError`, which by default are quite small.
        -- Still, `pretty` instances are scary, but this cannot make it into block outputs so this should
        -- be okay
        Just (cmd :: Command Text) -> over _Left Pact.renderCompactString $ parseCommand cmd
        Nothing -> Left "decode PayloadWithText failed"

parseCommand :: Command Text -> Either (PactError SpanInfo) (Command (PayloadWithText PublicMeta ParsedCode))
parseCommand cmd = do
    let cmd' = fmap encodeUtf8 cmd
    let code = SB.toShort (_cmdPayload cmd')
    parsedCmd <- over (_Right . cmdPayload . pMeta) _stableEncoding $ unsafeParseCommand cmd'
    return
        (parsedCmd & cmdPayload %~ \obj -> UnsafePayloadWithText { _payloadBytes = code, _payloadObj = obj })

encodePayload :: PayloadWithText meta code -> ByteString
encodePayload = SB.fromShort . _payloadBytes

newtype HashableTransaction = HashableTransaction Transaction
    deriving Eq

instance Hashable HashableTransaction where
    hashWithSalt salt (HashableTransaction tx) = hashWithSalt salt (_cmdHash tx)

-- decodePayload
--     :: ByteString
--     -> Either String PayloadWithText
-- decodePayload bs = case Aeson.decodeStrict' bs of
--     Just (payload :: Payload (StableEncoding PublicMeta) Text) -> do
--         p <- traverse parseCode $
--             over pMeta _stableEncoding payload
--         return $! PayloadWithText (SB.toShort bs) p
--     Nothing -> Left "decoding Payload failed"
