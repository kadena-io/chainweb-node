{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
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

-- import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..))

import Servant.Server

-- internal modules

import Chainweb.BlockHeader (decodeBlockHeaderWithoutHash)
import Chainweb.CutDB (CutDb)
import Chainweb.Miner.Core (HeaderBytes(..))
import Chainweb.Miner.Ministo (PrevBlock(..), publishing)
import Chainweb.Miner.RestAPI (MiningResultApi)
import Chainweb.Payload (PayloadWithOutputs)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Utils (runGet)
import Chainweb.Version (ChainwebVersion, ChainwebVersionT)

import Data.LogMessage (LogFunction)

---

solvedHandler
    :: LogFunction
    -> TVar (Maybe (T2 PayloadWithOutputs PrevBlock))
    -> CutDb cas
    -> HeaderBytes
    -> Handler ()
solvedHandler lf tp cdb (HeaderBytes hbytes) = liftIO $
    runGet decodeBlockHeaderWithoutHash hbytes >>= publishing lf tp cdb

-- TODO Use type-level `CutDbT`?
miningServer
    :: forall cas (v :: ChainwebVersionT)
    .  LogFunction
    -> TVar (Maybe (T2 PayloadWithOutputs PrevBlock))
    -> CutDb cas
    -> Server (MiningResultApi v)
miningServer lf tp cdb = solvedHandler lf tp cdb

-- TODO Get help from Lars
someMiningServer
    :: ChainwebVersion
    -> LogFunction
    -> TVar (Maybe (T2 PayloadWithOutputs PrevBlock))
    -> CutDb cas
    -> SomeServer
someMiningServer _ _ _ _ = undefined
-- someMiningServer v lf tp cdb = undefined
    -- SomeServer (Proxy @(MiningResultApi _)) (miningServer lf tp cdb)
