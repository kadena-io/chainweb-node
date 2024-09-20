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
{-# LANGUAGE DataKinds #-}

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
import Chainweb.Pact.Types
import Chainweb.Payload.PayloadStore
import qualified Chainweb.Pact4.Transaction as Pact4
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table.RocksDB
import Chainweb.Counter

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

data ChainResources logger = ChainResources
    { _chainResBlockHeaderDb :: !BlockHeaderDb
    , _chainResLogger :: !logger
    , _chainResMempool :: !(MempoolBackend Pact4.UnparsedTransaction)
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
    => CanReadablePayloadCas tbl
    => ChainwebVersion
    -> ChainId
    -> RocksDb
    -> logger
    -> (MVar PactExecutionService -> Mempool.InMemConfig Pact4.UnparsedTransaction)
    -> PayloadDb tbl
    -> FilePath
        -- ^ database directory for checkpointer
    -> PactServiceConfig
    -> Counter "txFailures"
    -> (ChainResources logger -> IO a)
    -> IO a
withChainResources
  v cid rdb logger mempoolCfg0 payloadDb pactDbDir pactConfig txFailuresCounter inner =
    withBlockHeaderDb rdb v cid $ \cdb -> do
      pexMv <- newEmptyMVar
      let mempoolCfg = mempoolCfg0 pexMv
      Mempool.withInMemoryMempool (setComponent "mempool" logger) mempoolCfg v $ \mempool -> do
        mpc <- MPCon.mkMempoolConsensus mempool cdb $ Just payloadDb
        withPactService v cid logger (Just txFailuresCounter) mpc cdb
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
    pes requestQ
        | v ^. versionCheats . disablePact = emptyPactExecutionService
        | otherwise = mkPactExecutionService requestQ
