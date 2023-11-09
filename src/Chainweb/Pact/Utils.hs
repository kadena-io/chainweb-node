{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Pact.Utils
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb

module Chainweb.Pact.Utils
    ( -- * combinators
      aeson
    , fromPactChainId
    , toTxCreationTime

    -- * k:account helper functions
    , validateKAccount
    , extractPubKeyFromKAccount
    , generateKAccountFromPubKey
    , pubKeyToKAccountKeySet
    , generateKeySetFromKAccount
    , validateKAccountKeySet

    -- * empty payload
    , emptyPayload
    ) where

import Data.Aeson
import qualified Data.Text as T

import Control.Monad.Catch

import Pact.Parse
import qualified Pact.Types.ChainId as P
import qualified Pact.Types.Term as P
import Pact.Types.ChainMeta
import Pact.Types.KeySet (KeysetPublicKey(KeysetPublicKey), ed25519HexFormat)
import Pact.Types.Crypto (PPKScheme(ED25519))

import qualified Pact.JSON.Encode as J

-- Internal modules

import Chainweb.ChainId
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Time

fromPactChainId :: MonadThrow m => P.ChainId -> m ChainId
fromPactChainId (P.ChainId t) = chainIdFromText t

-- | This is the recursion principle of an 'Aeson' 'Result' of type 'a'.
-- Similar to 'either', 'maybe', or 'bool' combinators
--
aeson :: (String -> b) -> (a -> b) -> Result a -> b
aeson f _ (Error a) = f a
aeson _ g (Success a) = g a

toTxCreationTime :: Time Micros -> TxCreationTime
toTxCreationTime (Time timespan) =
  TxCreationTime $ ParsedInteger $ fromIntegral $ timeSpanToSeconds timespan



-- TODO: This will fail for k: accounts that correspond to WebAuthn public
-- keys. Consider extending it when we integrate WebAuthn with k: accounts.
-- Note: This function is only used in Rosetta as a validation step.
validateKAccount :: T.Text -> Bool
validateKAccount acctName =
  case T.take 2 acctName of
    "k:" ->
      let pubKey = P.PublicKeyText $ T.drop 2 acctName
      in ed25519HexFormat $ KeysetPublicKey pubKey ED25519
    _ -> False

extractPubKeyFromKAccount :: T.Text -> Maybe P.PublicKeyText
extractPubKeyFromKAccount kacct
  | validateKAccount kacct =
    Just $ P.PublicKeyText $ T.drop 2 kacct
  | otherwise = Nothing

generateKAccountFromPubKey :: P.PublicKeyText -> Maybe T.Text
generateKAccountFromPubKey pubKey
  | validPubKey =
    let pubKeyText = P._pubKey pubKey
    in Just $ "k:" <> pubKeyText
  | otherwise = Nothing
  where
    validPubKey :: Bool
    validPubKey = ed25519HexFormat $ KeysetPublicKey pubKey ED25519


-- Warning: Only use if already certain that PublicKeyText
-- is valid.
-- Note: We are assuming the k: account is ED25519.
pubKeyToKAccountKeySet :: P.PublicKeyText -> P.KeySet
pubKeyToKAccountKeySet pubKey = P.mkKeySetText [pubKey] "keys-all"

generateKeySetFromKAccount :: T.Text -> Maybe P.KeySet
generateKeySetFromKAccount kacct = do
  pubKey <- extractPubKeyFromKAccount kacct
  pure $ pubKeyToKAccountKeySet pubKey

validateKAccountKeySet :: T.Text -> P.KeySet -> Bool
validateKAccountKeySet kacct actualKeySet =
  case generateKeySetFromKAccount kacct of
    Nothing -> False
    Just expectedKeySet
      | expectedKeySet == actualKeySet -> True
      | otherwise -> False

-- | Empty payload marking no-op transaction payloads.
--
emptyPayload :: PayloadWithOutputs
emptyPayload = newPayloadWithOutputs miner coinbase mempty
  where
    miner = MinerData $ J.encodeStrict noMiner
    coinbase = noCoinbaseOutput
