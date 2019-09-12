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

import Control.Monad (void)
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import Control.Lens (over, view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..), T3(..))

import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)

import Servant.API
import Servant.Server

import qualified Streaming.Prelude as P

-- internal modules

import Chainweb.Cut (Cut)
import Chainweb.BlockHeader (BlockHeader(..), decodeBlockHeaderWithoutHash)
import Chainweb.Chainweb.MinerResources (MiningCoordination(..))
import Chainweb.CutDB (CutDb, cutDbPayloadStore, _cut, cutStream)
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

workHandler :: forall l cas. Logger l => MiningCoordination l cas -> Miner -> IO WorkBytes
workHandler mr m = do
    c <- _cut cdb
    T3 p bh pl <- newWork m pact c
    let !phash = _blockPayloadHash bh
    atomically . modifyTVar' (_coordState mr) . over _Unwrapped . HM.insert phash $ T2 p pl
    pure . suncurry workBytes $ transferableBytes bh
  where
    cdb :: CutDb cas
    cdb = _coordCutDb mr

    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

-- TODO Occasionally prune the `MiningState`?
solvedHandler
    :: forall l cas. Logger l => MiningCoordination l cas -> HeaderBytes -> IO NoContent
solvedHandler mr (HeaderBytes hbytes) = do
    ms <- readTVarIO tms
    bh <- runGet decodeBlockHeaderWithoutHash hbytes
    publish lf ms (_coordCutDb mr) bh
    atomically . modifyTVar' tms . over _Unwrapped . HM.delete $ _blockPayloadHash bh
    pure NoContent
  where
    tms :: TVar MiningState
    tms = _coordState mr

    lf :: LogFunction
    lf = logFunction $ _coordLogger mr

updatesHandler :: CutDb cas -> Tagged Handler Application
updatesHandler cdb = Tagged $ \req respond -> do
    chan <- newChan
    void $ P.mapM_ (writeChan chan . f) $ cutStream cdb
    eventSourceAppChan chan req respond
  where
    -- | A completely empty `ServerEvent` that signals the discovery of a new
    -- `Cut`. Currently there is no need to actually send any information over
    -- to the caller.
    --
    f :: Cut -> ServerEvent
    f _ = ServerEvent Nothing Nothing []

miningServer
    :: forall l cas (v :: ChainwebVersionT)
    .  Logger l
    => MiningCoordination l cas
    -> Server (MiningApi v)
miningServer mr =
    liftIO . workHandler mr
    :<|> liftIO . solvedHandler mr
    :<|> updatesHandler (_coordCutDb mr)

someMiningServer :: Logger l => ChainwebVersion -> MiningCoordination l cas -> SomeServer
someMiningServer (FromSing (SChainwebVersion :: Sing v)) mr =
    SomeServer (Proxy @(MiningApi v)) $ miningServer mr
