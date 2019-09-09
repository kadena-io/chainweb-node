{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

#ifndef MIN_VERSION_servant_client
#define MIN_VERSION_servant_client(a,b,c) 1
#endif

-- |
-- Module: Chainweb.Miner.Miners
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Miner.Miners
  ( -- * Local Mining
    localPOW
  , localTest
    -- * Remote Mining
  , remoteMining
  , transferableBytes
  ) where

import Data.Bytes.Put (runPutS)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.These (these)
import Data.Tuple.Strict (T2(..), T3(..))

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar)
import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Control.Scheduler

import Network.HTTP.Client (Manager)

import Numeric.Natural (Natural)

import Servant.Client

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.CutDB (CutDb, cutDbPayloadStore, _cut)
import Chainweb.Difficulty (encodeHashTarget)
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Core
import Chainweb.Miner.Kato (MiningState(..), awaitNewCut, newWork, publish)
import Chainweb.Miner.Pact (Miner)
import Chainweb.Miner.RestAPI.Client (submitClient)
import Chainweb.NodeId (NodeId)
import Chainweb.RestAPI.Orphans ()
import Chainweb.Sync.WebBlockHeaderStore
#if !MIN_VERSION_servant_client(0,16,0)
import Chainweb.RestAPI.Utils
#endif
import Chainweb.Time (Seconds(..))
import Chainweb.Utils (int, partitionEithersNEL, runForever, runGet, suncurry)
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.LogMessage (LogFunction)

---

--------------------------------------------------------------------------------
-- Local Mining

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
localTest
    :: LogFunction
    -> ChainwebVersion
    -> Miner
    -> NodeId
    -> CutDb cas
    -> MWC.GenIO
    -> MinerCount
    -> IO ()
localTest lf v m nid cdb gen miners =
    runForever lf "Chainweb.Miner.Miners.localTest" $ loop mempty
  where
    loop :: MiningState -> IO a
    loop (MiningState old) = do
        c <- _cut cdb
        T3 p bh pl <- newWork m nid pact c
        let ms = MiningState $ HM.insert (_blockPayloadHash bh) (T2 p pl) old
        work bh >>= publish lf ms cdb >>= \ms' -> awaitNewCut cdb c >> loop ms'

    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

    t :: Double
    t = int graphOrder / (int (_minerCount miners) * meanBlockTime * 1000000)

    graphOrder :: Natural
    graphOrder = order $ _chainGraph v

    meanBlockTime :: Double
    meanBlockTime = case blockRate v of
        Just (BlockRate (Seconds n)) -> int n
        Nothing -> error $ "No BlockRate available for given ChainwebVersion: " <> show v

    work :: BlockHeader -> IO BlockHeader
    work bh = MWC.geometric1 t gen >>= threadDelay >> pure bh

-- TODO `MiningState` is reset when an exception occurs and `runForever`
-- restarts the work.
-- | A single-threaded in-process Proof-of-Work mining loop.
--
localPOW :: LogFunction -> ChainwebVersion -> Miner -> NodeId -> CutDb cas -> IO ()
localPOW lf v m nid cdb = runForever lf "Chainweb.Miner.Miners.localPOW" $ loop mempty
  where
    loop :: MiningState -> IO a
    loop (MiningState old) = do
        c <- _cut cdb
        T3 p bh pl <- newWork m nid pact c
        let ms = MiningState $ HM.insert (_blockPayloadHash bh) (T2 p pl) old
        work bh >>= publish lf ms cdb >>= \ms' -> awaitNewCut cdb c >> loop ms'

    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

    work :: BlockHeader -> IO BlockHeader
    work bh = do
        let T2 tbytes hbytes = transferableBytes bh
        HeaderBytes newBytes <- usePowHash v (\p -> mine p (_blockNonce bh) tbytes) hbytes
        runGet decodeBlockHeaderWithoutHash newBytes

-- | Can be piped to `workBytes` for a form suitable to use with `MiningAPI`.
--
transferableBytes :: BlockHeader -> T2 TargetBytes HeaderBytes
transferableBytes bh = T2 t h
  where
    t = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh
    h = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh

--------------------------------------------------------------------------------
-- Remote Mining

-- | Some remote process which is performing the low-level mining for us. May be
-- on a different machine, may be on multiple machines, may be arbitrarily
-- multithreaded.
--
-- ASSUMPTION: The contents of the given @NonEmpty BaseUrl@ are unique.
--
remoteMining :: Manager -> NonEmpty BaseUrl -> BlockHeader -> IO ()
remoteMining m urls bh = do
    -- TODO Use different `Comp`?
    rs <- partitionEithersNEL <$> traverseConcurrently Par' f urls
    these (throwM . NEL.head) (\_ -> pure ()) (\_ _ -> pure ()) rs
  where
    bs :: WorkBytes
    bs = suncurry workBytes $ transferableBytes bh

    f :: BaseUrl -> IO (Either ClientError ())
    f url = runClientM (submitClient bs) $ ClientEnv m url Nothing
