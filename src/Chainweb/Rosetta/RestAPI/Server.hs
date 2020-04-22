{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Rosetta.RestAPI.Server
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.RestAPI.Server where

import Data.Proxy (Proxy(..))

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Rosetta.RestAPI
import Chainweb.Version

---

rosettaServer :: forall (v :: ChainwebVersionT). Server (RosettaApi v)
rosettaServer = (\_ -> pure ())
  :<|> (\_ -> pure ())
  :<|> (\_ -> pure ())
  :<|> (\_ -> pure ())
  :<|> (\_ -> pure ())
  :<|> (\_ -> pure ())
  :<|> (\_ -> pure ())
  :<|> (\_ -> pure ())
  :<|> (\_ -> pure ())
  :<|> (\_ -> pure ())

someRosettaServer :: ChainwebVersion -> SomeServer
someRosettaServer (FromSingChainwebVersion (SChainwebVersion :: Sing vT)) =
    SomeServer (Proxy @(RosettaApi vT)) rosettaServer
