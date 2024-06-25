{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}

module Chainweb.Pact5.Transaction
  ( Transaction
  , PayloadWithText(..)
  , payloadCodec
  , parseCommand
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


type Transaction = Command PayloadWithText

data PayloadWithText = PayloadWithText
    { _payloadBytes :: !SB.ShortByteString
    , _payloadObj :: !(Payload PublicMeta ParsedCode)
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

instance Eq PayloadWithText where
    (==) = (==) `on` _payloadBytes

-- | A codec for Pact5's (Command PayloadWithText) transactions.
--
payloadCodec
    :: Codec (Command PayloadWithText)
payloadCodec = Codec enc dec
  where
    enc c = J.encodeStrict $ fmap (decodeUtf8 . encodePayload) c
    dec bs = case Aeson.decodeStrict' bs of
        Just (cmd :: Command Text) -> parseCommand cmd
        Nothing -> Left "decode PayloadWithText failed"

parseCommand :: Command Text -> Either String (Command PayloadWithText)
parseCommand cmd = do
    let cmd' = fmap encodeUtf8 cmd
    let code = SB.toShort (_cmdPayload cmd')
    parsedCmd <- over (_Right . cmdPayload . pMeta) _stableEncoding $ unsafeParseCommand cmd'
    return (parsedCmd & cmdPayload %~ \obj -> PayloadWithText { _payloadBytes = code, _payloadObj = obj })

encodePayload :: PayloadWithText -> ByteString
encodePayload = SB.fromShort . _payloadBytes

-- decodePayload
--     :: ByteString
--     -> Either String PayloadWithText
-- decodePayload bs = case Aeson.decodeStrict' bs of
--     Just (payload :: Payload (StableEncoding PublicMeta) Text) -> do
--         p <- traverse parseCode $
--             over pMeta _stableEncoding payload
--         return $! PayloadWithText (SB.toShort bs) p
--     Nothing -> Left "decoding Payload failed"
