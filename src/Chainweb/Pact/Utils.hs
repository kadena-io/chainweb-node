{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.Text qualified as T
import Control.Monad.Catch
import Pact.JSON.Encode qualified as J
import Chainweb.ChainId
import Chainweb.Miner.Pact
import Chainweb.Pact.Payload
import Chainweb.Time
import Pact.Core.ChainData qualified as P
import Pact.Core.Guards qualified as P
import Pact.Core.Guards (ed25519HexFormat)
import Data.Set qualified as Set
import Chainweb.Utils (fromTextM)

fromPactChainId :: MonadThrow m => P.ChainId -> m ChainId
fromPactChainId (P.ChainId t) = fromTextM t

-- | This is the recursion principle of an 'Aeson' 'Result' of type 'a'.
-- Similar to 'either', 'maybe', or 'bool' combinators
--
aeson :: (String -> b) -> (a -> b) -> Result a -> b
aeson f _ (Error a) = f a
aeson _ g (Success a) = g a

toTxCreationTime :: Time Micros -> P.TxCreationTime
toTxCreationTime (Time timespan) =
  P.TxCreationTime $ fromIntegral $ timeSpanToSeconds timespan



validateKAccount :: T.Text -> Bool
validateKAccount acctName =
  case T.take 2 acctName of
    "k:" ->
      let pubKey = P.PublicKeyText $ T.drop 2 acctName
      in ed25519HexFormat pubKey
    _ -> False

extractPubKeyFromKAccount :: T.Text -> Maybe P.PublicKeyText
extractPubKeyFromKAccount kacct
  | validateKAccount kacct =
    Just $ P.PublicKeyText $ T.drop 2 kacct
  | otherwise = Nothing

generateKAccountFromPubKey :: P.PublicKeyText -> Maybe T.Text
generateKAccountFromPubKey pubKey
  | ed25519HexFormat pubKey =
    let pubKeyText = P._pubKey pubKey
    in Just $ "k:" <> pubKeyText
  | otherwise = Nothing


-- Warning: Only use if already certain that PublicKeyText
-- is valid.
-- Note: We are assuming the k: account is ED25519.
pubKeyToKAccountKeySet :: P.PublicKeyText -> P.KeySet
pubKeyToKAccountKeySet pubKey = P.KeySet (Set.singleton pubKey) P.KeysAll

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
