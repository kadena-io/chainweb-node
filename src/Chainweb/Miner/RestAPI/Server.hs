{-# LANGUAGE DataKinds #-}
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
import Chainweb.CutDB (CutDb)
import Chainweb.Miner.Coordinator (MiningState(..), publishing)
import Chainweb.Miner.Core (HeaderBytes(..))
import Chainweb.Miner.RestAPI (MiningResultApi)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Utils (runGet)
import Chainweb.Version

import Data.LogMessage (LogFunction)
import Data.Singletons

---

solvedHandler
    :: LogFunction
    -> TVar (Maybe MiningState)
    -> CutDb cas
    -> HeaderBytes
    -> Handler ()
solvedHandler lf tms cdb (HeaderBytes hbytes) = liftIO $
    runGet decodeBlockHeaderWithoutHash hbytes >>= publishing lf tms cdb

-- TODO Use type-level `CutDbT`?
miningServer
    :: forall cas (v :: ChainwebVersionT)
    .  LogFunction
    -> TVar (Maybe MiningState)
    -> CutDb cas
    -> Server (MiningResultApi v)
miningServer lf tms cdb = solvedHandler lf tms cdb

someMiningServer
    :: ChainwebVersion
    -> LogFunction
    -> TVar (Maybe MiningState)
    -> CutDb cas
    -> SomeServer
someMiningServer (FromSing (SChainwebVersion :: Sing v)) lf tms cdb =
    SomeServer (Proxy @(MiningResultApi v)) (miningServer lf tms cdb)
