{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Miner.RestAPI.Client
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Miner.RestAPI.Client
  ( workClient
  , solvedClient
  ) where

import Network.HTTP.Types (Method)

import Servant.API
import Servant.Client (ClientM, Response, client)

-- internal modules

import Chainweb.Miner.Core (HeaderBytes(..), WorkBytes(..))
import Chainweb.Miner.Pact (Miner)
import Chainweb.Miner.RestAPI (miningApi)
import Chainweb.Version

import Data.Singletons

-- -----------------------------------------------------------------------------
-- Mining Results

workClient :: ChainwebVersion -> Miner -> ClientM WorkBytes
workClient v m = case clients v of
  f :<|> _ -> f m

solvedClient :: ChainwebVersion -> HeaderBytes -> ClientM NoContent
solvedClient v hbytes = case clients v of
  _ :<|> f :<|> _ -> f hbytes

clients
    :: ChainwebVersion
    -> (Miner -> ClientM WorkBytes)
    :<|> (HeaderBytes -> ClientM NoContent)
    :<|> (ChainId -> Method -> ClientM Response)
clients (FromSing (SChainwebVersion :: Sing v)) = client (miningApi @v)
