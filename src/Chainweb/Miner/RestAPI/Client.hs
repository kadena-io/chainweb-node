{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Miner.RestAPI.Client
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Miner.RestAPI.Client where

import Servant.Client (ClientM, client)

-- internal modules

import Chainweb.Miner.Core (HeaderBytes(..))
import Chainweb.Miner.RestAPI (miningResultApi)
import Chainweb.Version

import Data.Singletons

---

solvedClient :: ChainwebVersion -> HeaderBytes -> ClientM ()
solvedClient (FromSing (SChainwebVersion :: Sing v)) = client (miningResultApi @v)
