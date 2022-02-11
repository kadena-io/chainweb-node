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
    ( -- * persistence
      toEnv'
    , toEnvPersist'
      -- * combinators
    , aeson
    -- * time-to-live related items
    , maxTTL
    , timingsCheck
    , fromPactChainId
    , lenientTimeSlop
    , toTxCreationTime

    -- * k:account helper functions
    , validatePubKey
    , validateKAccount
    , extractPubKeyFromKAccount
    , generateKAccountFromPubKey
    , pubKeyToKAccountKeySet
    , generateKeySetFromKAccount
    , validateKAccountKeySet
    ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import Data.Char ( isDigit, ord )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Concurrent.MVar
import Control.Monad.Catch

import Pact.Interpreter as P
import Pact.Parse
import qualified Pact.Types.ChainId as P
import qualified Pact.Types.Term as P
import Pact.Types.ChainMeta
import Pact.Types.Command ( Payload, Command, ParsedCode )

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Pact.Backend.Types
import Chainweb.Time
import Chainweb.Transaction

fromPactChainId :: MonadThrow m => P.ChainId -> m ChainId
fromPactChainId (P.ChainId t) = chainIdFromText t

toEnv' :: EnvPersist' -> IO Env'
toEnv' (EnvPersist' ep') = do
    let thePactDb = _pdepPactDb $! ep'
    let theDbEnv = _pdepEnv $! ep'
    env <- mkPactDbEnv thePactDb theDbEnv
    return $! Env' env

toEnvPersist' :: Env' -> IO EnvPersist'
toEnvPersist' (Env' pactDbEnv) = do
    let mVar = pdPactDbVar $! pactDbEnv -- :: MVar (P.DbEnv a)
    !dbEnv <- readMVar $! mVar           -- :: P.DbEnv a
    let pDbEnvPersist = PactDbEnvPersist
          { _pdepPactDb = pdPactDb pactDbEnv -- :: P.PactDb (P.DbEnv a)
          , _pdepEnv = dbEnv
          }
    return $! EnvPersist' pDbEnvPersist

-- | This is the recursion principle of an 'Aeson' 'Result' of type 'a'.
-- Similar to 'either', 'maybe', or 'bool' combinators
--
aeson :: (String -> b) -> (a -> b) -> Result a -> b
aeson f _ (Error a) = f a
aeson _ g (Success a) = g a

-- | The maximum time-to-live (expressed in seconds)
maxTTL :: ParsedInteger
maxTTL = ParsedInteger $ 2 * 24 * 60 * 60
-- This is probably going to be changed. Let us make it 2 days for now.

-- prop_tx_ttl_newBlock/validateBlock
--
-- Timing checks used to be based on the creation time of the validated
-- block. That changed on mainnet at block height 449940. Tx creation time
-- and TTL don't affect the tx outputs and pact state and can thus be
-- skipped when replaying old blocks.
--
timingsCheck
    :: ParentCreationTime
    -> Command (Payload PublicMeta ParsedCode)
    -> Bool
timingsCheck (ParentCreationTime (BlockCreationTime txValidationTime)) tx =
    ttl > 0
    && txValidationTime >= timeFromSeconds 0
    && txOriginationTime >= 0
    && timeFromSeconds txOriginationTime <= lenientTxValidationTime
    && timeFromSeconds (txOriginationTime + ttl) > txValidationTime
    && ttl <= maxTTL
  where
    (TTLSeconds ttl) = timeToLiveOf tx
    timeFromSeconds = Time . secondsToTimeSpan . Seconds . fromIntegral
    (TxCreationTime txOriginationTime) = creationTimeOf tx
    lenientTxValidationTime = add (scaleTimeSpan lenientTimeSlop second) txValidationTime

-- | Validation "slop" to allow for a more lenient creation time check after
-- @useLegacyCreationTimeForTxValidation@ is no longer true.
-- Without this, transactions showing up in the interim between
-- parent block issuance and new block creation can get rejected; the tradeoff reduces
-- the accuracy of the tx creation time vs "blockchain time", but is better than e.g.
-- incurring artificial latency to wait for a parent block that is acceptable for a tx.
-- 95 seconds represents the 99th percentile of block arrival times.
lenientTimeSlop :: Seconds
lenientTimeSlop = 95


toTxCreationTime :: Time Micros -> TxCreationTime
toTxCreationTime (Time timespan) =
  TxCreationTime $ ParsedInteger $ fromIntegral $ timeSpanToSeconds timespan

-- Note (linda):
-- `ed25519Hex` and i`sHexDigitLower` are copied from:
--  https://github.com/kadena-io/pact/blob/0ea3e6f63a2f677520fa425fa5f01984a7345160/src/Pact/Types/KeySet.hs
-- (Feb 10, 2022) Once the latest version of Pact is merged in, these duplicate
-- functions don't need to be used.

-- | Current "Kadena" ED-25519 key format: 64-length hex.
ed25519Hex :: P.PublicKey -> Bool
ed25519Hex (P.PublicKey k) = BSC.length k == 64 && BSC.all isHexDigitLower k

-- | Lower-case hex numbers.
isHexDigitLower :: Char -> Bool
isHexDigitLower c =
  -- adapted from GHC.Unicode#isHexDigit
  isDigit c || (fromIntegral (ord c - ord 'a')::Word) <= 5

validatePubKey :: P.PublicKey -> Bool
validatePubKey = ed25519Hex

validateKAccount :: T.Text -> Bool
validateKAccount acctName =
  case T.take 2 acctName of
    "k:" ->
      let pubKey = P.PublicKey $ T.encodeUtf8 $ T.drop 2 acctName
      in validatePubKey pubKey
    _ -> False

extractPubKeyFromKAccount :: T.Text -> Maybe P.PublicKey
extractPubKeyFromKAccount kacct
  | validateKAccount kacct =
    Just $ P.PublicKey $ T.encodeUtf8 $ T.drop 2 kacct
  | otherwise = Nothing

generateKAccountFromPubKey :: P.PublicKey -> Maybe T.Text
generateKAccountFromPubKey pubKey
  | validatePubKey pubKey =
    let pubKeyText = T.decodeUtf8 $ P._pubKey pubKey
    in Just $ "k:" <> pubKeyText
  | otherwise = Nothing

-- Warning: Only use if already certain that PublicKey
-- is valid.
pubKeyToKAccountKeySet :: P.PublicKey -> P.KeySet
pubKeyToKAccountKeySet pubKey = P.mkKeySet [pubKey] "keys-all"

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