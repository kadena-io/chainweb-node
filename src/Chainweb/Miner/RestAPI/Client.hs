{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Miner.RestAPI.Client
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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

import Chainweb.Miner.Core (ChainBytes, HeaderBytes, WorkBytes)
import Chainweb.Miner.Pact (Miner)
import Chainweb.Miner.RestAPI (miningApi)
import Chainweb.Version

-- -----------------------------------------------------------------------------
-- Mining Results

workClient :: ChainwebVersion -> Maybe ChainId -> Miner -> ClientM WorkBytes
workClient v mcid m = case clients v of
  f :<|> _ -> f mcid m

solvedClient :: ChainwebVersion -> HeaderBytes -> ClientM NoContent
solvedClient v hbytes = case clients v of
  _ :<|> f :<|> _ -> f hbytes

clients
    :: ChainwebVersion
    -> (Maybe ChainId -> Miner -> ClientM WorkBytes)
    :<|> (HeaderBytes -> ClientM NoContent)
    :<|> (ChainBytes -> Method -> ClientM Response)
clients (FromSingChainwebVersion (SChainwebVersion :: Sing v)) = client (miningApi @v)
