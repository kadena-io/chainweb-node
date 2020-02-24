{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
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
    ) where

import Data.Aeson

import Control.Concurrent.MVar
import Control.Monad.Catch

import Pact.Interpreter as P
import Pact.Parse
import qualified Pact.Types.ChainId as P
import Pact.Types.ChainMeta
import Pact.Types.Command

import Chainweb.BlockCreationTime
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
timingsCheck
    :: BlockCreationTime
        -- ^ reference time for tx validation.
        --  If @useCurrentHeaderCreationTimeForTxValidation blockHeight@
        --  this is the the creation time of the current block. Otherwise it
        --  is the creation time of the parent block header.
    -> Command (Payload PublicMeta ParsedCode)
    -> Bool
timingsCheck (BlockCreationTime txValidationTime) tx =
    ttl > 0
    && txValidationTime >= toMicrosFromSeconds 0
    && txOriginationTime >= 0
    && toMicrosFromSeconds txOriginationTime < txValidationTime
    && toMicrosFromSeconds (txOriginationTime + ttl) >= txValidationTime
    && ttl <= maxTTL
  where
    (TTLSeconds ttl) = timeToLiveOf tx
    toMicrosFromSeconds = Time . TimeSpan . Micros . fromIntegral . (1000000 *)
    (TxCreationTime txOriginationTime) = creationTimeOf tx

