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

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (liftIO)

import Data.Proxy (Proxy(..))

import Servant.Server

-- internal modules

import Chainweb.BlockHeader (decodeBlockHeaderWithoutHash)
import Chainweb.Chainweb.MinerResources (MinerResources(..))
import Chainweb.CutDB (CutDb)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Coordinator (MiningState(..), publishing)
import Chainweb.Miner.Core (HeaderBytes(..))
import Chainweb.Miner.RestAPI (MiningResultApi)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Utils (runGet)
import Chainweb.Version

import Data.LogMessage (LogFunction)
import Data.Singletons

---

solvedHandler :: forall l cas. Logger l => MinerResources l cas -> HeaderBytes -> Handler ()
solvedHandler mr (HeaderBytes hbytes) = liftIO $
    runGet decodeBlockHeaderWithoutHash hbytes >>= publishing lf tms cdb
  where
    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    tms :: TVar (Maybe MiningState)
    tms = _minerResState mr

    cdb :: CutDb cas
    cdb = _minerResCutDb mr

-- TODO Use type-level `CutDbT`?
miningServer
    :: forall l cas (v :: ChainwebVersionT)
    .  Logger l
    => MinerResources l cas
    -> Server (MiningResultApi v)
miningServer mr = solvedHandler mr

someMiningServer :: Logger l => ChainwebVersion -> MinerResources l cas -> SomeServer
someMiningServer (FromSing (SChainwebVersion :: Sing v)) mr =
    SomeServer (Proxy @(MiningResultApi v)) $ miningServer mr
