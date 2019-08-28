{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Miner.RestAPI
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Miner.RestAPI
  ( MiningResultApi_
  , MiningResultApi
  , miningResultApi
  , someMiningResultApi
  ) where

import Data.Proxy (Proxy(..))

import Servant.API

-- internal modules

import Chainweb.Miner.Core (HeaderBytes)
import Chainweb.RestAPI.Utils (ChainwebEndpoint(..), Reassoc, SomeApi(..))
import Chainweb.Version

import Data.Singletons

---

-- | To yield a solved `Chainweb.BlockHeader.BlockHeader` back to a Chainweb
-- Node for it to be reassociated with its `Chainweb.Cut.Cut` and Payload, then
-- published to the rest of the network.
type MiningResultApi_ = ReqBody '[OctetStream] HeaderBytes :> Post '[JSON] ()

type MiningResultApi (v :: ChainwebVersionT)
    = 'ChainwebEndpoint v :> Reassoc MiningResultApi_

miningResultApi :: forall (v :: ChainwebVersionT). Proxy (MiningResultApi v)
miningResultApi = Proxy

someMiningResultApi :: ChainwebVersion -> SomeApi
someMiningResultApi (FromSing (SChainwebVersion :: Sing v)) = SomeApi $ miningResultApi @v
