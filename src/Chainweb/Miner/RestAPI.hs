{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Miner.RestAPI
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Miner.RestAPI
  ( -- * Mining API
    MiningApi_
  , MiningApi
  , miningApi
  , someMiningApi
  ) where

import Data.Proxy (Proxy(..))

import Pact.Utils.Servant

import Servant.API

-- internal modules

import Chainweb.Miner.Core (ChainBytes, HeaderBytes, WorkBytes)
import Chainweb.Miner.Pact (Miner)
import Chainweb.RestAPI.Utils (ChainwebEndpoint(..), Reassoc, SomeApi(..))
import Chainweb.Version

-- -----------------------------------------------------------------------------
-- Mining API

-- | /work/: To have a new `BlockHeader` assembled to mine upon. `Miner`
-- information is required in order to properly distribute mining rewards.
--
-- /solved/: To yield a solved `Chainweb.BlockHeader.BlockHeader` back to a
-- Chainweb Node for it to be reassociated with its `Chainweb.Cut.Cut` and
-- Payload, then published to the rest of the network.
--
type MiningApi_ =
    "mining" :> "work"
             :> QueryParam "chain" ChainId
             :> ReqBody '[PactJson] Miner
             :> Get '[OctetStream] WorkBytes
    :<|> "mining" :> "solved"
                  :> ReqBody '[OctetStream] HeaderBytes
                  :> Post '[JSON] NoContent
    :<|> "mining" :> "updates"
                  :> ReqBody '[OctetStream] ChainBytes
                  :> Raw

type MiningApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> Reassoc MiningApi_

miningApi :: forall (v :: ChainwebVersionT). Proxy (MiningApi v)
miningApi = Proxy

someMiningApi :: ChainwebVersion -> SomeApi
someMiningApi (FromSingChainwebVersion (SChainwebVersion :: Sing v)) = SomeApi $ miningApi @v
