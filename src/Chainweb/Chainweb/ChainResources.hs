{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.ChainResources
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Allocate chainweb resources for individual chains
--
module Chainweb.Chainweb.ChainResources
( ChainResources(..)
, chainResBlockHeaderDb
, chainResMempool
, chainResLogger
, chainResPact
, withChainResources
) where

import Control.Concurrent.MVar
import Control.Lens hiding ((.=), (<.>))

import Data.Maybe


import Prelude hiding (log)


-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Logger
import qualified Chainweb.Mempool.Consensus as MPCon
import qualified Chainweb.Mempool.InMem as Mempool
import qualified Chainweb.Mempool.InMemTypes as Mempool
import Chainweb.Mempool.Mempool (MempoolBackend)
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.Types
import Chainweb.Payload.PayloadStore
import Chainweb.Transaction
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.CAS.RocksDB

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

data ChainResources logger = ChainResources
    { _chainResBlockHeaderDb :: !BlockHeaderDb
    , _chainResLogger :: !logger
    , _chainResMempool :: !(MempoolBackend ChainwebTransaction)
    , _chainResPact :: PactExecutionService
    }

makeLenses ''ChainResources

instance HasChainwebVersion (ChainResources logger) where
    _chainwebVersion = _chainwebVersion . _chainResBlockHeaderDb
    {-# INLINE _chainwebVersion #-}

instance HasChainId (ChainResources logger) where
    _chainId = _chainId . _chainResBlockHeaderDb
    {-# INLINE _chainId #-}

-- | Intializes all local Chain resources, but doesn't start any networking.
--
withChainResources
    :: Logger logger
    => PayloadCasLookup cas
    => ChainwebVersion
    -> ChainId
    -> RocksDb
    -> logger
    -> (MVar PactExecutionService -> Mempool.InMemConfig ChainwebTransaction)
    -> PayloadDb cas
    -> FilePath
        -- ^ database directory for checkpointer
    -> PactServiceConfig
    -> (ChainResources logger -> IO a)
    -> IO a
withChainResources
  v cid rdb logger mempoolCfg0 payloadDb pactDbDir pactConfig inner =
    withBlockHeaderDb rdb v cid $ \cdb -> do
      pexMv <- newEmptyMVar
      let mempoolCfg = mempoolCfg0 pexMv
      Mempool.withInMemoryMempool_ (setComponent "mempool" logger) mempoolCfg v $ \mempool -> do
        mpc <- MPCon.mkMempoolConsensus mempool cdb $ Just payloadDb
        withPactService v cid (setComponent "pact" logger) mpc cdb
                        payloadDb pactDbDir pactConfig $ \requestQ -> do
            let pex = pes requestQ
            putMVar pexMv pex

            -- run inner
            inner $ ChainResources
                { _chainResBlockHeaderDb = cdb
                , _chainResLogger = logger
                , _chainResMempool = mempool
                , _chainResPact = pex
                }
  where
    pes requestQ = case v of
        Test{} -> emptyPactExecutionService
        TimedConsensus{} -> emptyPactExecutionService
        PowConsensus{} -> emptyPactExecutionService
        TimedCPM{} -> mkPactExecutionService requestQ
        FastTimedCPM{} -> mkPactExecutionService requestQ
        Development -> mkPactExecutionService requestQ
        Testnet04 -> mkPactExecutionService requestQ
        Mainnet01 -> mkPactExecutionService requestQ

