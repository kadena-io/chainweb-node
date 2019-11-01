{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Data.Binary.Builder (fromByteString)
import Data.Generics.Wrapped (_Unwrapped)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..), T3(..))

import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), decodeBlockHeaderWithoutHash)
import Chainweb.Chainweb.MinerResources (MiningCoordination(..))
import Chainweb.Cut (Cut)
import Chainweb.CutDB (CutDb, awaitNewCutByChainId, cutDbPayloadStore, _cut)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Coordinator
    (ChainChoice(..), MiningState(..), newWork, publish)
import Chainweb.Miner.Core
    (ChainBytes(..), HeaderBytes(..), WorkBytes, workBytes)
import Chainweb.Miner.Miners (transferableBytes)
import Chainweb.Miner.Pact (Miner)
import Chainweb.Miner.RestAPI (MiningApi)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.Utils (runGet, suncurry3)
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.LogMessage (LogFunction)
import Data.Singletons

---

-- | KILLSWITCH: The logic here involving `txSilenceEndDate` is to be removed in a
-- future version of Chainweb. This logic errors on remote requests for new
-- mining work, such that this Node can no longer meaningfully participate in
-- mining.
--
workHandler
    :: Logger l
    => MiningCoordination l cas
    -> ChainwebVersion
    -> Maybe ChainId
    -> Miner
    -> Handler WorkBytes
workHandler mr v mcid m = do
    now <- liftIO getCurrentTimeIntegral
    case txSilenceEndDate v of
        Just end | now > end ->
            throwM err400 { errBody = "Node is out of date - please upgrade."}
        _ -> do
            MiningState ms <- liftIO . readTVarIO $ _coordState mr
            when (M.size ms > _coordLimit mr) $ do
                liftIO $ atomicModifyIORef' (_coord503s mr) (\c -> (c + 1, ()))
                throwM err503 { errBody = "Too many work requests" }
            liftIO $ workHandler' mr mcid m

workHandler'
    :: forall l cas
    .  Logger l
    => MiningCoordination l cas
    -> Maybe ChainId
    -> Miner
    -> IO WorkBytes
workHandler' mr mcid m = do
    c <- _cut cdb
    T3 p bh pl <- newWork (maybe Anything Suggestion mcid) m pact c
    let !phash = _blockPayloadHash bh
        !bct = _blockCreationTime bh
    atomically . modifyTVar' (_coordState mr) . over _Unwrapped . M.insert (T2 bct phash) $ T3 m p pl
    pure . suncurry3 workBytes $ transferableBytes bh
  where
    cdb :: CutDb cas
    cdb = _coordCutDb mr

    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

solvedHandler
    :: forall l cas. Logger l => MiningCoordination l cas -> HeaderBytes -> IO NoContent
solvedHandler mr (HeaderBytes hbytes) = do
    ms <- readTVarIO tms
    bh <- runGet decodeBlockHeaderWithoutHash hbytes
    publish lf ms (_coordCutDb mr) bh
    let !phash = _blockPayloadHash bh
        !bct = _blockCreationTime bh
    atomically . modifyTVar' tms . over _Unwrapped $ M.delete (T2 bct phash)
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
    -> ChainwebVersion
    -> Server (MiningApi v)
miningServer mr v =
    workHandler mr v
    :<|> liftIO . solvedHandler mr
    :<|> updatesHandler (_coordCutDb mr)

someMiningServer :: Logger l => ChainwebVersion -> MiningCoordination l cas -> SomeServer
someMiningServer v@(FromSing (SChainwebVersion :: Sing vT)) mr =
    SomeServer (Proxy @(MiningApi vT)) $ miningServer mr v
