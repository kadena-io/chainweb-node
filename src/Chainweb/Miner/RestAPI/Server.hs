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
import Control.Lens (over)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(..))

import Servant.Server

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), decodeBlockHeaderWithoutHash)
import Chainweb.Chainweb.MinerResources (MinerResources(..))
import Chainweb.CutDB (CutDb)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Coordinator (MiningState(..), publish)
import Chainweb.Miner.Core (HeaderBytes(..))
import Chainweb.Miner.RestAPI (MiningResultApi)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Utils (runGet)
import Chainweb.Version

import Data.LogMessage (LogFunction)
import Data.Singletons

---

-- TODO Occasionally prune the `MiningState`?
solvedHandler :: forall l cas. Logger l => MinerResources l cas -> HeaderBytes -> Handler ()
solvedHandler mr (HeaderBytes hbytes) = liftIO $ do
    ms <- readTVarIO tms
    bh <- runGet decodeBlockHeaderWithoutHash hbytes
    publish lf ms cdb bh
    atomically . modifyTVar' tms . over _Unwrapped . HM.delete $ _blockPayloadHash bh
  where
    tms :: TVar MiningState
    tms = _minerResState mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    cdb :: CutDb cas
    cdb = _minerResCutDb mr

miningServer
    :: forall l cas (v :: ChainwebVersionT)
    .  Logger l
    => MinerResources l cas
    -> Server (MiningResultApi v)
miningServer mr = solvedHandler mr

someMiningServer :: Logger l => ChainwebVersion -> MinerResources l cas -> SomeServer
someMiningServer (FromSing (SChainwebVersion :: Sing v)) mr =
    SomeServer (Proxy @(MiningResultApi v)) $ miningServer mr
