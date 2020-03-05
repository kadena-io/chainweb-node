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
    , newBlockTimingsCheck
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

import Chainweb.BlockHeader
import Chainweb.BlockCreationTime
import Chainweb.ChainId
import Chainweb.Pact.Backend.Types
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Version

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
    && txValidationTime >= timeFromSeconds 0
    && txOriginationTime >= 0
    && timeFromSeconds txOriginationTime <= txValidationTime
    && timeFromSeconds (txOriginationTime + ttl) > txValidationTime
    && ttl <= maxTTL
  where
    (TTLSeconds ttl) = timeToLiveOf tx
    timeFromSeconds = Time . secondsToTimeSpan . Seconds . fromIntegral
    (TxCreationTime txOriginationTime) = creationTimeOf tx

-- -------------------------------------------------------------------------- --
-- New Block timings check

-- | Only used during new block. MUST NOT be used during block validation.
--
-- prop_tx_ttl_newBlock/validateBlock
--
newBlockTimingsCheck
    :: ParentHeader
    -> BlockCreationTime
        -- ^ validation of the parent header
    -> Command (Payload PublicMeta ParsedCode)
    -> Bool
newBlockTimingsCheck parentHeader (BlockCreationTime txValidationTime) tx =
    ttl > 0
    && txValidationTime >= timeFromSeconds 0
    && txOriginationTime >= 0
    && timeFromSeconds txOriginationTime <= txValidationTime
    && timeFromSeconds (txOriginationTime + ttl - compatPeriod) > txValidationTime
    && ttl <= maxTTL
  where
    (TTLSeconds ttl) = timeToLiveOf tx
    timeFromSeconds = Time . secondsToTimeSpan . Seconds . fromIntegral
    (TxCreationTime txOriginationTime) = creationTimeOf tx

    -- ensure that every block that validates with
    -- @txValidationTime == _blockCreatinTime parentHeader@ (new behavior) also validates with
    -- @txValidationTime == _blockCreationTime currentHeader@ (old behavior).
    --
    -- The compat period puts an effective lower limit on the TTL value. During
    -- the transition period any transactions that is submitted with a lower TTL
    -- value is considered expired and rejected immediately. After the
    -- transition period, which will probably last a few days, the compat period
    -- is disabled again.
    --
    -- The time between two blocks is distributed exponentially with a rate \(r\) of 2
    -- blocks per minutes. The quantiles of the exponential distribution with
    -- parameter \(r\) is \(quantile(x) = \frac{- ln(1 - x)}{r}\).
    --
    -- Thus the 99.99% will be solved in less than @(- log (1 - 0.9999)) / 2@
    -- minutes, which is less than 5 minutes.
    --
    -- In practice, due to the effects of difficulty adjustement, the
    -- distribution is skewed such that the 99.99 percentile is actually a
    -- little less than 3 minutes.
    --
    compatPeriod
        | useCurrentHeaderCreationTimeForTxValidation v bh =  60 * 3
        | otherwise = 0
    v = _chainwebVersion parentHeader
    bh = succ $ _blockHeight $ _parentHeader parentHeader

