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
module Chainweb.Miner.RestAPI.Client
  ( -- * Mining Work Submission
    submitClient
    -- * Mining Results
  , solvedClient
  ) where

import Data.Proxy (Proxy(..))

import Servant.Client (ClientM, client)

-- internal modules

import Chainweb.Miner.Core (HeaderBytes(..), WorkBytes(..))
import Chainweb.Miner.RestAPI (MiningSubmissionApi, miningResultApi)
import Chainweb.Version

import Data.Singletons

---

-- -----------------------------------------------------------------------------
-- Mining Work Submission

submitClient :: WorkBytes -> ClientM ()
submitClient = client (Proxy @MiningSubmissionApi)

-- -----------------------------------------------------------------------------
-- Mining Results

solvedClient :: ChainwebVersion -> HeaderBytes -> ClientM ()
solvedClient (FromSing (SChainwebVersion :: Sing v)) = client (miningResultApi @v)
