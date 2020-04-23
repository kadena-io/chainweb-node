{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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

import Rosetta

import Servant.API

import Chainweb.RestAPI.Utils (ChainwebEndpoint(..), Reassoc)
import Chainweb.Version

---

type RosettaApi_ =
    -- Accounts --
    "rosetta" :> "account" :> "balance"
        :> ReqBody '[JSON] AccountBalanceRequest
        :> Post '[JSON] AccountBalanceResponse
    -- Blocks --
    :<|> "rosetta" :> "block" :> "transaction"
        :> ReqBody '[JSON] BlockTransactionRequest
        :> Post '[JSON] BlockTransactionResponse
    :<|> "rosetta" :> "block"
        :> ReqBody '[JSON] BlockRequest
        :> Post '[JSON] BlockResponse
    -- Construction --
    :<|> "rosetta" :> "construction" :> "metadata"
        :> ReqBody '[JSON] ConstructionMetadataRequest
        :> Post '[JSON] ConstructionMetadataResponse
    :<|> "rosetta" :> "construction" :> "submit"
        :> ReqBody '[JSON] ConstructionSubmitRequest
        :> Post '[JSON] ConstructionSubmitResponse
    -- Mempool --
    :<|> "rosetta" :> "mempool" :> "transaction"
        :> ReqBody '[JSON] MempoolTransactionRequest
        :> Post '[JSON] MempoolTransactionResponse
    :<|> "rosetta" :> "mempool"
        :> ReqBody '[JSON] MempoolRequest
        :> Post '[JSON] MempoolResponse
    -- Network --
    :<|> "rosetta" :> "network" :> "list"
        :> ReqBody '[JSON] ()  -- TODO Need clarification.
        :> Post '[JSON] NetworkListResponse
    :<|> "rosetta" :> "network" :> "options"
        :> ReqBody '[JSON] ()
        :> Post '[JSON] NetworkOptionsResponse
    :<|> "rosetta" :> "network" :> "status"
        :> ReqBody '[JSON] ()
        :> Post '[JSON] NetworkStatusResponse

type RosettaApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> Reassoc RosettaApi_
