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

import Control.Monad.Except (throwError)

import Data.Aeson (encode)
import Data.Proxy (Proxy(..))

import Rosetta

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Rosetta.RestAPI
import Chainweb.Version

---

rosettaServer :: forall (v :: ChainwebVersionT). Server (RosettaApi v)
rosettaServer = (\_ -> undefined)
    -- Blocks --
    :<|> (\_ -> undefined)
    :<|> (\_ -> undefined)
    -- Construction --
    :<|> (\_ -> undefined)
    :<|> (\_ -> undefined)
    -- Mempool --
    :<|> mempoolTransactionH
    :<|> mempoolH
    -- Network --
    :<|> (\_ -> undefined)
    :<|> (\_ -> undefined)
    :<|> (\_ -> undefined)

someRosettaServer :: ChainwebVersion -> SomeServer
someRosettaServer (FromSingChainwebVersion (SChainwebVersion :: Sing vT)) =
    SomeServer (Proxy @(RosettaApi vT)) rosettaServer

--------------------------------------------------------------------------------
-- Mempool Handlers

mempoolH :: MempoolRequest -> Handler MempoolResponse
mempoolH (MempoolRequest (NetworkIdentifier _ _ msni)) = case msni of
    Nothing -> throwError err500 { errBody = encode $ rosettaError RosettaChainUnspecified }
    Just sni -> do
        undefined

mempoolTransactionH :: MempoolTransactionRequest -> Handler MempoolTransactionResponse
mempoolTransactionH = undefined
