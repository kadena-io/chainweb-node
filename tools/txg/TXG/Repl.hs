-- | Porcelain for the transaction generator in the REPL. Many of these
-- functions are very unsafe because they are designed for maximum convenience
-- in ghci.  Do not depend on this module from important code!

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TXG.Repl
  (
  -- * Core functions
    send
  , poll
  , local
  , cmd

  -- * Specific kinds of transactions
  , transfer
  , transferCreate

  -- * Various convenience functions
  , rk
  , chain
  , chain0
  , host
  , mkKey
  , mkKeyCombined
  , k2g
  , mkGuard
  , mkGuardCombined
  , stockKey
  , signedCode

  , module Chainweb.ChainId
  , module Chainweb.Version
  , module Pact.Types.ChainMeta
  , module TXG.Simulate.Contracts.CoinContract
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Yaml as Y
import Text.Printf
import Pact.ApiReq
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
import TXG.Simulate.Utils

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

-- | Easy way to construct a ChainId
chain :: Int -> ChainId
chain n = fromJust $ chainIdFromText $ T.pack $ show n

-- | ChainId for the most commonly used chain
chain0 :: ChainId
chain0 = fromJust $ chainIdFromText "0"

-- | Decodes a base16-encoded key into a ByteString
mkKeyBS :: Text -> ByteString
mkKeyBS = decodeKey . encodeUtf8

mkKey :: Text -> Text -> SomeKeyPair
mkKey pub priv = skp
  where
    Right skp = importKeyPair defaultScheme (Just $ PubBS $ mkKeyBS pub) (PrivBS $ mkKeyBS priv)

-- | Pact-web's private key copy/paste feature copies a string that contains the
-- private and public keys concatenated together. This function makes it easy to
-- make key pairs from those strings.
mkKeyCombined :: Text -> SomeKeyPair
mkKeyCombined pactWebPriv = mkKey pub priv
  where
    (priv,pub) = T.splitAt (T.length pactWebPriv `div` 2) pactWebPriv

k2g :: SomeKeyPair -> Guard
k2g skp = Guard (skp :| [])

mkGuard pub priv = k2g $ mkKey pub priv
mkGuardCombined pactWebPriv = k2g $ mkKeyCombined pactWebPriv

-- | Convenient access to predefined testnet sender accounts
stockKey :: Text -> IO ApiKeyPair
stockKey s = do
  Right (Object o) <- Y.decodeFileEither "pact/genesis/testnet/keys.yaml"
  let Just (Object kp) = HM.lookup s o
      Just (String pub) = HM.lookup "public" kp
      Just (String priv) = HM.lookup "secret" kp
  return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519)

signedCode
  :: SomeKeyPair
  -- ^ Key pair to sign with
  -> String
  -- ^ Pact code
  -> IO [Command Text]
signedCode k c =
  fmap (:[]) (txToCommand defPubMeta (k :| []) (PactCode c))

-- | Convenience function for constructing a coin transfer transaction
transfer :: Text -> Text -> Double -> IO [Command Text]
transfer from to amt = do
  k <- stockKey from
  let meta = defPubMeta { _pmSender = from }
  kps <- mkKeyPairs [k]
  fmap (:[]) $ txToCommand meta (NEL.fromList kps) $
    CallBuiltin $ CC $ CoinTransfer
      (SenderName $ Account $ T.unpack from)
      (ReceiverName $ Account $ T.unpack to)
      (Amount amt)

-- | Convenience function for constructing a transfer-and-create transaction
transferCreate :: Text -> Text -> Guard -> Double -> IO [Command Text]
transferCreate from to guard amt = do
  k <- stockKey from
  let meta = defPubMeta { _pmSender = from }
  kps <- mkKeyPairs [k]
  fmap (:[]) $ txToCommand meta (NEL.fromList kps) $
    CallBuiltin $ CC $ CoinTransferAndCreate
      (SenderName $ Account $ T.unpack from)
      (ReceiverName $ Account $ T.unpack to)
      guard
      (Amount amt)
