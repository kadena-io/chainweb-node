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

-- 2 | The maximum time-to-live (expressed in seconds)
maxTTL :: ParsedInteger
maxTTL = ParsedInteger $ 2 * 24 * 60 * 60
-- This is probably going to be changed. Let us make it 2 days for now.

-- timingsCheck :: BlockCreationTime -> Command (Payload PublicMeta ParsedCode) -> Bool
-- timingsCheck (BlockCreationTime blockOriginationTime) tx =
--     ttl > 0
--     && blockOriginationTime >= (toMicrosFromSeconds 0)
--     && txOriginationTime >=
--     && toMicrosFromSeconds txOriginationTime < blockOriginationTime
--     && toMicrosFromSeconds (txOriginationTime + ttl) >= blockOriginationTime
--     && ttl <= maxTTL
--   where
--     (TTLSeconds ttl) = timeToLiveOf tx
--     toMicrosFromSeconds = Time . TimeSpan . Micros . fromIntegral . (1000000 *)
--     (TxCreationTime txOriginationTime) = creationTimeOf tx
timingsCheck :: BlockCreationTime -> Command (Payload PublicMeta ParsedCode) -> Bool
timingsCheck (BlockCreationTime blockOriginationTime) tx =
  let b =
        ttl > 0
        && blockOriginationTime >= (toMicrosFromSeconds 0)
        && txOriginationTime >= 0
        && toMicrosFromSeconds txOriginationTime < blockOriginationTime
        && toMicrosFromSeconds (txOriginationTime + ttl) >= blockOriginationTime
        && ttl <= maxTTL
  in if not b
       then
         let timingStr =
                "\nblockOriginationTime: " ++ show blockOriginationTime
                ++ "\ntoMicrosFromSeconds 0: " ++ show (toMicrosFromSeconds 0)
                ++ "\ntxOriginationTime: " ++ show txOriginationTime
                ++ "\ntoMicrosFromSeconds txOriginationTime: " ++ show (toMicrosFromSeconds txOriginationTime)
                ++ "\ntoMicrosFromSeconds txOriginationTime + ttl: "
                   ++ show (toMicrosFromSeconds (txOriginationTime + ttl))
                ++ "\n: ttl: " ++ show ttl
                ++ "\n: maxTTL: " ++ show maxTTL
                ++ "\nttl > 0: " ++ show (ttl >0)
                ++ "\nblockOriginationTime >= 0: "
                    ++ show (txOriginationTime >= 0)
                ++ "\ntoMicrosFromSeconds txOriginationTime < blockOriginationTime: "
                    ++ show (toMicrosFromSeconds txOriginationTime < blockOriginationTime)
                ++ "\ntoMicrosFromSeconds (txOriginationTime + ttl) >= blockOriginationTime: "
                    ++ show (toMicrosFromSeconds (txOriginationTime + ttl) >= blockOriginationTime)
                ++ "\nttl <= maxTTL: " ++ show (ttl <= maxTTL)
         in error $ "failed timings check: " ++ timingStr
       else b
  where
    (TTLSeconds ttl) = timeToLiveOf tx
    toMicrosFromSeconds = Time . TimeSpan . Micros . fromIntegral . (1000000 *)
    (TxCreationTime txOriginationTime) = creationTimeOf tx
