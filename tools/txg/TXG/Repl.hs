-- Porcelain for the transaction generator in the REPL

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TXG.Repl
  ( rk
  , chain
  , chain0
  , host
  , mkKey
  , stockKey
  , signedCode
  , transfer
  , module Chainweb.ChainId
  , module Chainweb.Version
  , module Pact.Types.ChainMeta
  , module TXG.Simulate.Contracts.CoinContract
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
--import qualified Data.Vector as V
import qualified Data.Yaml as Y
import Text.Printf
import Pact.Types.API
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Scheme
import Pact.Types.Util
import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Version

import TXG.ReplInternals
import TXG.Simulate.Contracts.CoinContract
import TXG.Simulate.Contracts.Common

-- Helper for simplifying construction of RequestKeys
rk :: String -> RequestKeys
rk s = RequestKeys $ fromString s :| []

host :: String -> HostAddress
host h = HostAddress hn 443
  where
    Just hn = hostnameFromText $ T.pack h

instance IsString RequestKey where
  fromString s = RequestKey h
    where
      Right h = fromText' (T.pack s)

chain :: Int -> ChainId
chain n = fromJust $ chainIdFromText $ T.pack $ show n

chain0 :: ChainId
chain0 = fromJust $ chainIdFromText "0"

mkKey :: ByteString -> ByteString -> Either String SomeKeyPair
mkKey pub priv = importKeyPair defaultScheme (Just $ PubBS pub) (PrivBS priv)

stockKey :: Text -> IO SomeKeyPair
stockKey s = do
  Right (Object o) <- Y.decodeFileEither "pact/genesis/testnet/keys.yaml"
  let Just (Object kp) = HM.lookup s o
      Just (String pub) = HM.lookup "public" kp
      Just (String priv) = HM.lookup "secret" kp
      Right k = mkKey (encodeUtf8 pub) (encodeUtf8 priv)
  return k

signedCode :: SomeKeyPair -> String -> IO [Command Text]
signedCode k c =
  fmap (:[]) (txToCommand defPubMeta (k :| []) (PactCode c))

transfer :: Text -> Text -> Double -> IO [Command Text]
transfer from to amt = do
  k <- stockKey from
  let meta = defPubMeta { _pmSender = from }
  fmap (:[]) $ txToCommand meta (k :| []) $
    CallBuiltin $ CC $ CoinTransfer
      (SenderName $ Account $ T.unpack from)
      (ReceiverName $ Account $ T.unpack to)
      (Amount $ realToFrac amt)
