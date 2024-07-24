{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}

module Chainweb.Pact5.Transaction
  ( Transaction
  , PayloadWithText
  , payloadBytes
  , payloadObj
  , payloadCodec
  , parseCommand
  , parsePact4Command
  ) where

import Control.DeepSeq
import Control.Lens

import qualified Data.Aeson as Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SB
import Data.Function
import Data.Hashable
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import GHC.Generics (Generic)

import Pact.Core.Command.Types
import Pact.Core.ChainData
import Pact.Core.Evaluate
import Pact.Core.StableEncoding
import Pact.Core.Syntax.ParseTree
import qualified Pact.JSON.Encode as J

import Chainweb.Utils
import Chainweb.Utils.Serialization
import qualified Chainweb.Pact4.Transaction as Pact4

type Transaction = Command (PayloadWithText PublicMeta ParsedCode)

data PayloadWithText meta code = UnsafePayloadWithText
    { _payloadBytes :: !SB.ShortByteString
    , _payloadObj :: !(Payload meta code)
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

instance Eq (PayloadWithText meta code) where
    (==) = (==) `on` _payloadBytes

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
parsePact4Command tx = undefined

-- decodePayload
--     :: ByteString
--     -> Either String PayloadWithText
-- decodePayload bs = case Aeson.decodeStrict' bs of
--     Just (payload :: Payload (StableEncoding PublicMeta) Text) -> do
--         p <- traverse parseCode $
--             over pMeta _stableEncoding payload
--         return $! PayloadWithText (SB.toShort bs) p
--     Nothing -> Left "decoding Payload failed"
