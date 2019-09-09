{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Miner.RestAPI.Server
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Miner.RestAPI.Server where

import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import Control.Lens (over, view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..), T3(..))

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), decodeBlockHeaderWithoutHash)
import Chainweb.Chainweb.MinerResources (MinerResources(..))
import Chainweb.CutDB (CutDb, cutDbPayloadStore, _cut)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Coordinator (MiningState(..), newWork, publish)
import Chainweb.Miner.Core (HeaderBytes(..), WorkBytes, workBytes)
import Chainweb.Miner.Miners (transferableBytes)
import Chainweb.Miner.Pact (Miner)
import Chainweb.Miner.RestAPI (MiningApi)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Utils (runGet, suncurry)
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.LogMessage (LogFunction)
import Data.Singletons

---

workHandler :: forall l cas. Logger l => MinerResources l cas -> Miner -> IO WorkBytes
workHandler mr m = do
    c <- _cut cdb
    T3 p bh pl <- newWork m pact c
    let !phash = _blockPayloadHash bh
    atomically . modifyTVar' (_minerResState mr) . over _Unwrapped . HM.insert phash $ T2 p pl
    pure . suncurry workBytes $ transferableBytes bh
  where
    cdb :: CutDb cas
    cdb = _minerResCutDb mr

    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

-- TODO Occasionally prune the `MiningState`?
solvedHandler :: forall l cas. Logger l => MinerResources l cas -> HeaderBytes -> IO ()
solvedHandler mr (HeaderBytes hbytes) = do
    ms <- readTVarIO tms
    bh <- runGet decodeBlockHeaderWithoutHash hbytes
    publish lf ms (_minerResCutDb mr) bh
    atomically . modifyTVar' tms . over _Unwrapped . HM.delete $ _blockPayloadHash bh
  where
    tms :: TVar MiningState
    tms = _minerResState mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

miningServer
    :: forall l cas (v :: ChainwebVersionT)
    .  Logger l
    => MinerResources l cas
    -> Server (MiningApi v)
miningServer mr = liftIO . workHandler mr :<|> liftIO . solvedHandler mr

someMiningServer :: Logger l => ChainwebVersion -> MinerResources l cas -> SomeServer
someMiningServer (FromSing (SChainwebVersion :: Sing v)) mr =
    SomeServer (Proxy @(MiningApi v)) $ miningServer mr
