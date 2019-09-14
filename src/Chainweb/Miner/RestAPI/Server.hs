{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Binary.Builder (fromByteString)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..), T3(..))

import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.Cut (Cut)
import Chainweb.BlockHeader (BlockHeader(..), decodeBlockHeaderWithoutHash)
import Chainweb.Chainweb.MinerResources (MiningCoordination(..))
import Chainweb.CutDB (CutDb, cutDbPayloadStore, _cut, awaitNewCutByChainId)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Coordinator (MiningState(..), newWork, publish)
import Chainweb.Miner.Core (HeaderBytes(..), WorkBytes, workBytes, ChainBytes(..))
import Chainweb.Miner.Miners (transferableBytes)
import Chainweb.Miner.Pact (Miner)
import Chainweb.Miner.RestAPI (MiningApi)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Utils (runGet, suncurry3)
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.LogMessage (LogFunction)
import Data.Singletons

---

workHandler
    :: forall l cas
    .  Logger l
    => MiningCoordination l cas
    -> Maybe ChainId
    -> Miner
    -> IO WorkBytes
workHandler mr mcid m = do
    c <- _cut cdb
    T3 p bh pl <- newWork mcid m pact c
    let !phash = _blockPayloadHash bh
    atomically . modifyTVar' (_coordState mr) . over _Unwrapped . HM.insert phash $ T2 p pl
    pure . suncurry3 workBytes $ transferableBytes bh
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

updatesHandler :: CutDb cas -> ChainBytes -> Tagged Handler Application
updatesHandler cdb (ChainBytes cbytes) = Tagged $ \req respond -> do
    cid <- runGet decodeChainId cbytes
    cv  <- _cut cdb >>= newIORef
    eventSourceAppIO (go cid cv) req respond
  where
    -- | A nearly empty `ServerEvent` that signals the discovery of a new
    -- `Cut`. Currently there is no need to actually send any information over
    -- to the caller.
    --
    f :: ServerEvent
    f = ServerEvent (Just $ fromByteString "New Cut") Nothing []

    go :: ChainId -> IORef Cut -> IO ServerEvent
    go cid cv = readIORef cv >>= awaitNewCutByChainId cdb cid >>= writeIORef cv >> pure f

miningServer
    :: forall l cas (v :: ChainwebVersionT)
    .  Logger l
    => MiningCoordination l cas
    -> Server (MiningApi v)
miningServer mr =
    (\mcid m -> liftIO $ workHandler mr mcid m)
    :<|> liftIO . solvedHandler mr
    :<|> updatesHandler (_coordCutDb mr)

someMiningServer :: Logger l => ChainwebVersion -> MiningCoordination l cas -> SomeServer
someMiningServer (FromSing (SChainwebVersion :: Sing v)) mr =
    SomeServer (Proxy @(MiningApi v)) $ miningServer mr
