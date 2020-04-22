{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Rosetta.RestAPI
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.RestAPI where

import Servant.API

---

type RosettaApi_ =
  -- Accounts --
    "rosetta" :> "account" :> "balance" :> ReqBody '[JSON] () :> Post '[JSON] ()
  -- Blocks --
  :<|> "rosetta" :> "block" :> "transaction" :> ReqBody '[JSON] () :> Post '[JSON] ()
  :<|> "rosetta" :> "block" :> ReqBody '[JSON] () :> Post '[JSON] ()
  -- Construction --
  :<|> "rosetta" :> "construction" :> "metadata" :> ReqBody '[JSON] () :> Post '[JSON] ()
  :<|> "rosetta" :> "construction" :> "submit" :> ReqBody '[JSON] () :> Post '[JSON] ()
  -- Mempool --
  :<|> "rosetta" :> "mempool" :> "transaction" :> ReqBody '[JSON] () :> Post '[JSON] ()
  :<|> "rosetta" :> "mempool" :> ReqBody '[JSON] () :> Post '[JSON] ()
  -- Network --
  :<|> "rosetta" :> "network" :> "list" :> ReqBody '[JSON] () :> Post '[JSON] ()
  :<|> "rosetta" :> "network" :> "options" :> ReqBody '[JSON] () :> Post '[JSON] ()
  :<|> "rosetta" :> "network" :> "status" :> ReqBody '[JSON] () :> Post '[JSON] ()
